/* (depends smoothlang_anc2020_iocons) */

/*
 * Copyright (c) 2009, Ali Clark <emailaliclark@gmail.com>
 *
 * Permission to use, copy, modify, and/or distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */

#include "smoothlang/anc2020/smooth.h"

#define iocons smoothlang_anc2020_iocons__iocons
smooth_t iocons (smooth_t x, smooth_t y);

smooth_t smoothlang_anc2020_bytepointer__setbyte (smooth_t addr, smooth_t byte, smooth_t z) {
  *((unsigned char*) addr) = byte;
  return iocons(byte, z);
}

smooth_t smoothlang_anc2020_bytepointer__getbyte (smooth_t addr, smooth_t z) {
  return iocons(*((unsigned char*) addr), z);
}
