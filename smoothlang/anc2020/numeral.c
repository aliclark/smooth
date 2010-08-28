
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

static smooth numeral_addone (smooth x) {
  return x + 1;
}

static smooth numeral_inner (smooth self, smooth local) {
  smooth f     = smooth_closure_lookup(self, 0);
  smooth depth = smooth_closure_lookup(self, 1);

  /* Since we defined `addone` ourselves, we know it just adds 1, `depth` times */
  if (f == ((smooth) numeral_addone)) {
    return depth + local;
  }

  /* This is here as a formal requirement, but there is no need to ever reach this section. */
  for (; depth > 0; --depth) {
    local = smooth_apply(f, &local, 1);
  }
  return local;
}

/* This is our optimisation function (lambda -> C-code | null) */
/*SMOOTH

(: (smoothlang_anc2020_numeral__numeral_to_ulint x) (cquote 0))

*/
smooth smoothlang_anc2020_numeral__numeral_to_ulint (smooth x) {
  smooth args[] = { numeral_addone, 0 };
  return smooth_apply(x, args, 2);
}

smooth smoothlang_anc2020_numeral__ulint_to_numeral (smooth a) {
  return smooth_closure_create((smooth) numeral_inner, 2, &a, 1, NULL);
}

