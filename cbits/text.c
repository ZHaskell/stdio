#include <text.h>
#include <stdint.h>
#include <utf8rewind.h>
#include <codepoint.h>

#ifdef __SSE2__
#include <simdutf8check.h>
#endif

HsInt utf8_validate(const char* p, HsInt off, HsInt len){
    const char* q = p + off;
#ifdef __AVX2__
    return (HsInt)validate_utf8_fast_avx(q, (size_t)len);
#else
#ifdef __SSE2__
    return (HsInt)validate_utf8_fast(q, (size_t)len);
#else
    return utf8_validate_slow(q, len);
#endif
#endif
}

////////////////////////////////////////////////////////////////////////////////

static inline int ascii_u64(const uint8_t *data, int len)
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

#if defined(__x86_64__)
#include <x86intrin.h>

static inline int ascii_simd(const uint8_t *data, int len)
{
    if (len >= 32) {
        const uint8_t *data2 = data+16;

        __m128i or1 = _mm_set1_epi8(0), or2 = or1;

        while (len >= 32) {
            __m128i input1 = _mm_lddqu_si128((const __m128i *)data);
            __m128i input2 = _mm_lddqu_si128((const __m128i *)data2);

            or1 = _mm_or_si128(or1, input1);
            or2 = _mm_or_si128(or2, input2);

            data += 32;
            data2 += 32;
            len -= 32;
        }

        or1 = _mm_or_si128(or1, or2);
        if (_mm_movemask_epi8(_mm_cmplt_epi8(or1, _mm_set1_epi8(0))))
            return 0;
    }

    return ascii_u64(data, len);
}

#elif defined(__aarch64__)
#include <arm_neon.h>

static inline int ascii_simd(const uint8_t *data, int len)
{
    if (len >= 32) {
        const uint8_t *data2 = data+16;

        uint8x16_t or1 = vdupq_n_u8(0), or2 = or1;

        while (len >= 32) {
            const uint8x16_t input1 = vld1q_u8(data);
            const uint8x16_t input2 = vld1q_u8(data2);

            or1 = vorrq_u8(or1, input1);
            or2 = vorrq_u8(or2, input2);

            data += 32;
            data2 += 32;
            len -= 32;
        }

        or1 = vorrq_u8(or1, or2);
        if (vmaxvq_u8(or1) >= 0x80)
            return 0;
    }

    return ascii_u64(data, len);
}

#endif
////////////////////////////////////////////////////////////////////////////////

HsInt utf8_validate_slow(const char* src, HsInt len){
    const char* end = src + len;
    unicode_t* decoded;
    for (; src < end; ) {

        if (*src <= MAX_BASIC_LATIN)
        {
            /* Basic Latin */
            src++;
        }
        else
        {
            /* Multi-byte sequence */

            static const uint8_t SequenceMask[7] = {
                0x00, 0x7F, 0x1F, 0x0F,
                0x07, 0x03, 0x01
            };
            static const unicode_t SequenceMinimum[7] = {
                0x0000, 0x0000, 0x0080, 0x0800,
                0x10000, MAX_LEGAL_UNICODE, MAX_LEGAL_UNICODE
            };

            size_t src_size = end-src;
            uint8_t src_index;

            /* Length of sequence is determined by first byte */

            uint8_t decoded_length = codepoint_decoded_length[*src];
            if (decoded_length < 1 || decoded_length > 4)
            {
                /* Not a multi-byte sequence starter */
                return 0;
            } else {
                /* Use mask to strip value from first byte */

                decoded = (unicode_t)(*src & SequenceMask[decoded_length]);

                /* All bytes in the sequence must be processed */

                for (src_index = 1; src_index < decoded_length; ++src_index)
                {
                    src++;

                    /* Check if next byte is valid */

                    if (src_size == 0 ||               /* Not enough data */
                        (*src < 0x80 || *src > 0xBF))  /* Not a continuation byte */
                    {
                        return 0;
                    }

                    src_size--;

                    /* Add value of continuation byte to codepoint */

                    decoded = (*decoded << 6) | (*src & 0x3F);
                }

                /* Check for overlong sequences and surrogate pairs */

                if (decoded < SequenceMinimum[decoded_length] || decoded > MAX_LEGAL_UNICODE ||
                    (decoded >= SURROGATE_HIGH_START && decoded <= SURROGATE_LOW_END))
                {
                    return 0;
                }
            }

            src += decoded_length;
        }
    }
    /* loop over all bytes */
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
