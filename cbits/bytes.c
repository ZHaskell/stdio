#include <bytes.h>

HsInt hs_memchr(uint8_t *a, HsInt aoff, uint8_t b, HsInt n) {
    a += aoff;
    uint8_t *p = memchr(a, b, (size_t)n);
    if (p == NULL) return -1;
    else return (p - a);
}
