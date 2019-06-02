/*
 * Copyright (c) 2017-2018 Dong Han
 *
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. Neither the names of the authors or the names of any contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#include <Rts.h>

HsInt ascii_validate(const char* p, HsInt off, HsInt len);
HsInt ascii_validate_addr(const char* p, HsInt len);
HsInt utf8_validate(const char* p, HsInt off, HsInt len);
HsInt utf8_validate_addr(const char* p, HsInt len);

HsInt find_json_string_end(uint32_t* state, const unsigned char* ba, HsInt offset, HsInt len);
HsInt decode_json_string(char *dest, const char *src, HsInt srcoff, HsInt srclen);
HsInt escape_json_string_length(const unsigned char *src, HsInt srcoff, HsInt srclen);
HsInt escape_json_string(const unsigned char *src, HsInt srcoff, HsInt srclen, unsigned char *dest, HsInt desoff);

HsInt utf8_isnormalized(const char* p, HsInt off, HsInt len, size_t flag);
HsInt utf8_normalize(const char* p, HsInt off, HsInt len, char* q, HsInt len2, size_t flag);
HsInt utf8_normalize_length(const char* p, HsInt off, HsInt len, size_t flag);


HsInt utf8_casefold(const char* p, HsInt off, HsInt len, char* q, HsInt len2, size_t locale);
HsInt utf8_casefold_length(const char* p, HsInt off, HsInt len, size_t locale);

HsInt utf8_tolower(const char* p, HsInt off, HsInt len, char* q, HsInt len2, size_t locale);
HsInt utf8_tolower_length(const char* p, HsInt off, HsInt len, size_t locale);

HsInt utf8_toupper(const char* p, HsInt off, HsInt len, char* q, HsInt len2, size_t locale);
HsInt utf8_toupper_length(const char* p, HsInt off, HsInt len, size_t locale);

HsInt utf8_totitle(const char* p, HsInt off, HsInt len, char* q, HsInt len2, size_t locale);
HsInt utf8_totitle_length(const char* p, HsInt off, HsInt len, size_t locale);

HsInt utf8_iscategory(const char* p, HsInt off, HsInt len, size_t flags);
