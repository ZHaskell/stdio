#include <text.h>
#include <stdint.h>
#include <utf8rewind.h>
#include <codepoint.h>

#ifdef __SSE2__
#include <simdasciicheck.h>
#include <simdutf8check.h>
#endif

HsInt ascii_validate(const char* p, HsInt off, HsInt len){
    const char* q = p + off;
#ifdef __AVX2__
    return (HsInt)validate_ascii_fast_avx(q, (size_t)len);
#else
#ifdef __SSE2__
    return (HsInt)validate_ascii_fast(q, (size_t)len);
#else
    return (HsInt)ascii_u64(q, (size_t)len);
#endif
#endif
}

HsInt utf8_validate(const char* p, HsInt off, HsInt len){
    const char* q = p + off;
#ifdef __AVX2__
    return (HsInt)validate_utf8_fast_avx(q, (size_t)len);
#else
#ifdef __SSE2__
    return (HsInt)validate_utf8_fast(q, (size_t)len);
#else
    return utf8_validate_slow(q, (size_t)len);
#endif
#endif
}
// for some reason unknown, on windows we have to supply a seperated version of utf8_validate
// otherwise we got segfault if we import the same FFI with different type (Addr# vs ByteArray#)
HsInt utf8_validate_addr(const char* p, HsInt len){
#ifdef __AVX2__
    return (HsInt)validate_utf8_fast_avx(p, (size_t)len);
#else
#ifdef __SSE2__
    return (HsInt)validate_utf8_fast(p, (size_t)len);
#else
    return utf8_validate_slow(p, (size_t)len);
#endif
#endif
}

////////////////////////////////////////////////////////////////////////////////

static inline int ascii_u64(const uint8_t *data, size_t len)
{
    uint8_t orall = 0;

    if (len >= 16) {

        uint64_t or1 = 0, or2 = 0;
        const uint8_t *data2 = data+8;

        do {
            or1 |= *(const uint64_t *)data;
            or2 |= *(const uint64_t *)data2;
            data += 16;
            data2 += 16;
            len -= 16;
        } while (len >= 16);

        /*
         * Idea from Benny Halevy <bhalevy@scylladb.com>
         * - 7-th bit set   ==> orall = !(non-zero) - 1 = 0 - 1 = 0xFF
         * - 7-th bit clear ==> orall = !0 - 1          = 1 - 1 = 0x00
         */
        orall = !((or1 | or2) & 0x8080808080808080ULL) - 1;
    }

    while (len--)
        orall |= *data++;

    return orall < 0x80;
}

////////////////////////////////////////////////////////////////////////////////

#define UTF8_ACCEPT 0
#define UTF8_REJECT 1

static const uint8_t utf8d[] = {
    0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
    0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
    0,   0,   0,   0,   0,   0,   0,   0,   0,   0, // 00..1f
    0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
    0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
    0,   0,   0,   0,   0,   0,   0,   0,   0,   0, // 20..3f
    0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
    0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
    0,   0,   0,   0,   0,   0,   0,   0,   0,   0, // 40..5f
    0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
    0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
    0,   0,   0,   0,   0,   0,   0,   0,   0,   0, // 60..7f
    1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,
    1,   1,   1,   1,   1,   9,   9,   9,   9,   9,   9,
    9,   9,   9,   9,   9,   9,   9,   9,   9,   9, // 80..9f
    7,   7,   7,   7,   7,   7,   7,   7,   7,   7,   7,
    7,   7,   7,   7,   7,   7,   7,   7,   7,   7,   7,
    7,   7,   7,   7,   7,   7,   7,   7,   7,   7, // a0..bf
    8,   8,   2,   2,   2,   2,   2,   2,   2,   2,   2,
    2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,
    2,   2,   2,   2,   2,   2,   2,   2,   2,   2, // c0..df
    0xa, 0x3, 0x3, 0x3, 0x3, 0x3, 0x3, 0x3, 0x3, 0x3, 0x3,
    0x3, 0x3, 0x4, 0x3, 0x3, // e0..ef
    0xb, 0x6, 0x6, 0x6, 0x5, 0x8, 0x8, 0x8, 0x8, 0x8, 0x8,
    0x8, 0x8, 0x8, 0x8, 0x8 // f0..ff
};

static const uint8_t utf8d_transition[] = {
    0x0, 0x1, 0x2, 0x3, 0x5, 0x8, 0x7, 0x1, 0x1, 0x1, 0x4,
    0x6, 0x1, 0x1, 0x1, 0x1, // s0..s0
    1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,
    1,   1,   1,   1,   1,   1,   0,   1,   1,   1,   1,
    1,   0,   1,   0,   1,   1,   1,   1,   1,   1, // s1..s2
    1,   2,   1,   1,   1,   1,   1,   2,   1,   2,   1,
    1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,
    1,   2,   1,   1,   1,   1,   1,   1,   1,   1, // s3..s4
    1,   2,   1,   1,   1,   1,   1,   1,   1,   2,   1,
    1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,
    1,   3,   1,   3,   1,   1,   1,   1,   1,   1, // s5..s6
    1,   3,   1,   1,   1,   1,   1,   3,   1,   3,   1,
    1,   1,   1,   1,   1,   1,   3,   1,   1,   1,   1,
    1,   1,   1,   1,   1,   1,   1,   1,   1,   1, // s7..s8
};

static uint32_t inline updatestate(uint32_t *state, uint32_t byte) {
    uint32_t type = utf8d[byte];
    *state = utf8d_transition[16 * *state + type];
    return *state;
}

HsInt utf8_validate_slow(const char* c, size_t len){
    const unsigned char *cu = (const unsigned char *)c;
    uint32_t state = 0;
    for (size_t i = 0; i < len; i++) {
        uint32_t byteval = (uint32_t)cu[i];
        if (updatestate(&state, byteval) == UTF8_REJECT)
            return 0;
    }
    return 1;
}

HsInt utf8_isnormalized(const char* p, HsInt off, HsInt len, size_t flag){
    size_t offset;
    return (HsInt)utf8isnormalized(p+off, len, flag, &offset);
}

HsInt utf8_normalize(const char* p, HsInt off, HsInt len, const char* q, HsInt len2, size_t flag){
    size_t converted_size;
    int32_t errors;
    if ((converted_size = utf8normalize(p+off, len, q, len2, flag, &errors)) == 0 ||
        errors != UTF8_ERR_NONE)
    {
        return -1;
    } else {
        return converted_size;
    }
}

HsInt utf8_normalize_length(const char* p, HsInt off, HsInt len, size_t flag){
    size_t converted_size;
    int32_t errors;
    if ((converted_size = utf8normalize(p+off, len, NULL, 0, flag, &errors)) == 0 ||
        errors != UTF8_ERR_NONE)
    {
        return -1;
    } else {
        return converted_size;
    }
}

HsInt utf8_casefold(const char* p, HsInt off, HsInt len, const char* q, HsInt len2, size_t locale){
    size_t converted_size;
    int32_t errors;
    if ((converted_size = utf8casefold(p+off, len, q, len2, locale, &errors)) == 0 ||
        errors != UTF8_ERR_NONE)
    {
        return -1;
    } else {
        return converted_size;
    }
}

HsInt utf8_casefold_length(const char* p, HsInt off, HsInt len, size_t locale){
    size_t converted_size;
    int32_t errors;
    if ((converted_size = utf8casefold(p+off, len, NULL, 0, locale, &errors)) == 0 ||
        errors != UTF8_ERR_NONE)
    {
        return -1;
    } else {
        return converted_size;
    }
}

HsInt utf8_tolower(const char* p, HsInt off, HsInt len, const char* q, HsInt len2, size_t locale){
    size_t converted_size;
    int32_t errors;
    if ((converted_size = utf8tolower(p+off, len, q, len2, locale, &errors)) == 0 ||
        errors != UTF8_ERR_NONE)
    {
        return -1;
    } else {
        return converted_size;
    }
}

HsInt utf8_tolower_length(const char* p, HsInt off, HsInt len, size_t locale){
    size_t converted_size;
    int32_t errors;
    if ((converted_size = utf8tolower(p+off, len, NULL, 0, locale, &errors)) == 0 ||
        errors != UTF8_ERR_NONE)
    {
        return -1;
    } else {
        return converted_size;
    }
}

HsInt utf8_toupper(const char* p, HsInt off, HsInt len, const char* q, HsInt len2, size_t locale){
    size_t converted_size;
    int32_t errors;
    if ((converted_size = utf8toupper(p+off, len, q, len2, locale, &errors)) == 0 ||
        errors != UTF8_ERR_NONE)
    {
        return -1;
    } else {
        return converted_size;
    }
}

HsInt utf8_toupper_length(const char* p, HsInt off, HsInt len, size_t locale){
    size_t converted_size;
    int32_t errors;
    if ((converted_size = utf8toupper(p+off, len, NULL, 0, locale, &errors)) == 0 ||
        errors != UTF8_ERR_NONE)
    {
        return -1;
    } else {
        return converted_size;
    }
}

HsInt utf8_totitle(const char* p, HsInt off, HsInt len, const char* q, HsInt len2, size_t locale){
    size_t converted_size;
    int32_t errors;
    if ((converted_size = utf8totitle(p+off, len, q, len2, locale, &errors)) == 0 ||
        errors != UTF8_ERR_NONE)
    {
        return -1;
    } else {
        return converted_size;
    }
}

HsInt utf8_totitle_length(const char* p, HsInt off, HsInt len, size_t locale){
    size_t converted_size;
    int32_t errors;
    if ((converted_size = utf8totitle(p+off, len, NULL, 0, locale, &errors)) == 0 ||
        errors != UTF8_ERR_NONE)
    {
        return -1;
    } else {
        return converted_size;
    }
}

HsInt utf8_iscategory(const char* p, HsInt off, HsInt len, size_t flags){
    return (HsInt)utf8iscategory(p+off, len, flags);
}
