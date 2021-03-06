
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

/*
 * IOCONS is not a normal data structure - it aims to optimise itself away.
 *
 * For any module function you write which uses IOCONS, please follow both these rules:
 *
 * a) The RealWorld argument to your function should always be passed as the last argument.
 * b) You should create another version of your function without IOCONS or the last argument.
 *    This version should be named similarly, but with _opt appended.
 *
 * eg.
 *
 * smooth_t smoothlang_anc2020_iochar__putchar_opt (smooth_t x) {
 *   return putchar(x);
 * }
 *
 * smooth_t smoothlang_anc2020_iochar__putchar (smooth_t x, smooth_t rw) {
 *   return iocons(smoothlang_anc2020_iochar__putchar_opt(x), rw);
 * }
 *
 */


#include <stdlib.h>

typedef struct iocons_s {
  smooth x; /* The IO value */
  smooth y; /* The RealWorld value */
} iocons_t;

static void iocons_free (smooth a) {
  iocons_t* c = (iocons_t*) a;
  smooth_gc_decref(c->x);
  smooth_gc_decref(c->y);
  free(c);
}

/* WARNING unless this is added to a gc somehow, you will find it leaks! */
smooth smoothlang_anc2020_iocons__iocons (smooth x, smooth y) {
  iocons_t* c = malloc(sizeof(iocons_t));
  c->x = x;
  c->y = y;
  smooth_gc_incref(x); /* This effectively pins the objects until we get deleted. */
  smooth_gc_incref(y);
  smooth_gc_register((smooth) c, iocons_free);
  return (smooth) c;
}

smooth smoothlang_anc2020_iocons__iocons_car (smooth x) {
  return ((iocons_t*) x)->x;
}

smooth smoothlang_anc2020_iocons__iocons_cdr (smooth x) {
  return ((iocons_t*) x)->y;
}

