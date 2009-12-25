
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

static smooth_t numeral_addone (smooth_t x) {
  return x + 1;
}

static smooth_t numeral_numeral_x (smooth_closure_t* self, smooth_t local) {
  smooth_t depth = SMOOTH_C_LOCAL(SMOOTH_C_PARENT(self));
  smooth_t f     = SMOOTH_C_LOCAL(self);

  /* Since we defined `addone` ourselves, we know it just adds 1, `depth` times */
  if (f == ((smooth_t) numeral_addone)) {
    return depth + local;
  }

  SMOOTH_PUSH(local);
  for (; depth > 0; --depth) {
    SMOOTH_CALL(f);
  }

  return SMOOTH_POP();
}

static smooth_t numeral_numeral_f (smooth_closure_t* self, smooth_t local) {
  return SMOOTH_C_CREATE(numeral_numeral_x, local, self);
}

/* This is our optimisation function (lambda -> C-code | null) */
/*SMOOTH

(: (smoothlang_anc2020_numeral__numeral_to_ulint x) (cquote 0))

*/
smooth_t smoothlang_anc2020_numeral__numeral_to_ulint (smooth_t x) {
  SMOOTH_PUSH(0);
  SMOOTH_PUSH(numeral_addone);
  SMOOTH_CALL(x);
  SMOOTH_CALL(SMOOTH_POP());
  return SMOOTH_POP();
}

smooth_t smoothlang_anc2020_numeral__ulint_to_numeral (smooth_t a) {
  return SMOOTH_C_CREATE(numeral_numeral_f, a, NULL);
}
