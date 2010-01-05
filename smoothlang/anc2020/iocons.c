
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

typedef unsigned long int size_t;

void  free   (void* p);
void* malloc (size_t s);

typedef struct iocons_s {
  smooth_t x; /* The IO value */
  smooth_t y; /* The RealWorld value */
} iocons_t;

static void iocons_free (smooth_t a) {
  iocons_t* c = (iocons_t*) a;
  smooth_gc_decref(c->x);
  smooth_gc_decref(c->y);
  free(c);
}

/* WARNING unless this is added to a gc somehow, you will find it leaks! */
smooth_t smoothlang_anc2020_iocons__iocons (smooth_t x, smooth_t y) {
  iocons_t* c = malloc(sizeof(iocons_t));
  c->x = x;
  c->y = y;
  smooth_gc_incref(x); /* This effectively pins the objects until we get deleted. */
  smooth_gc_incref(y);
  smooth_gc_register((smooth_t) c, iocons_free);
  return (smooth_t) c;
}

smooth_t smoothlang_anc2020_iocons__iocons_car (smooth_t x) {
  return ((iocons_t*) x)->x;
}

smooth_t smoothlang_anc2020_iocons__iocons_cdr (smooth_t x) {
  return ((iocons_t*) x)->y;
}

