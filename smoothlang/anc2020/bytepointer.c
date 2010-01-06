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

#define NULL 0

#define iocons smoothlang_anc2020_iocons__iocons
smooth_t iocons (smooth_t x, smooth_t y);

typedef unsigned long int size_t;
void  free   (void* p);
void* malloc (size_t s);

smooth_t smoothlang_anc2020_bytepointer__malloc_opt (smooth_t size) {
  return malloc(size);
}

smooth_t smoothlang_anc2020_bytepointer__free_opt (smooth_t addr) {
  free(addr);
  return NULL;
}

smooth_t smoothlang_anc2020_bytepointer__setbyte_opt (smooth_t addr, smooth_t byte) {
  *((unsigned char*) addr) = byte;
  return byte;
}

smooth_t smoothlang_anc2020_bytepointer__getbyte_opt (smooth_t addr) {
  return *((unsigned char*) addr);
}


smooth_t smoothlang_anc2020_bytepointer__malloc (smooth_t size, smooth_t z) {
  return iocons(malloc(size), z);
}

smooth_t smoothlang_anc2020_bytepointer__free (smooth_t addr, smooth_t z) {
  free(addr);
  return iocons(NULL, z);
}

smooth_t smoothlang_anc2020_bytepointer__setbyte (smooth_t addr, smooth_t byte, smooth_t z) {
  *((unsigned char*) addr) = byte;
  return iocons(byte, z);
}

smooth_t smoothlang_anc2020_bytepointer__getbyte (smooth_t addr, smooth_t z) {
  return iocons(*((unsigned char*) addr), z);
}
