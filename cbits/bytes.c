/*
Copyright (c) 2017-2019 Dong Han
Copyright Johan Tibell 2011, Dong Han 2019
All rights reserved.
Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:
    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.
    * Neither the name of Johan Tibell nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.
THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#include <bytes.h>

HsInt hs_memchr(uint8_t *a, HsInt aoff, uint8_t b, HsInt n) {
    a += aoff;
    uint8_t *p = memchr(a, b, (size_t)n);
    if (p == NULL) return -1;
    else return (p - a);
}


/* FNV-1 hash
 *
 * The FNV-1 hash description: http://isthe.com/chongo/tech/comp/fnv/
 * The FNV-1 hash is public domain: http://isthe.com/chongo/tech/comp/fnv/#public_domain
 *
 * The original version from hashable use long type which doesn't match 'Int' in haskell and
 * cause problems on window, here we use HsInt.
 */
HsInt hs_fnv_hash_addr(const unsigned char* str, HsInt len, HsInt salt) {

    HsWord hash = salt;
    while (len--) {
      hash = (hash * 16777619) ^ *str++;
    }

    return hash;
}

HsInt hs_fnv_hash(const unsigned char* str, HsInt offset, HsInt len, HsInt salt) {
    return hs_fnv_hash_addr(str + offset, len, salt);
}
