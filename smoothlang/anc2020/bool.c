
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

#define FALSE 0
#define TRUE  1

static smooth bool_inner (smooth_closure self, smooth local) {
  return SMOOTH_C_LOOKUP(self, 1) ? SMOOTH_C_LOOKUP(self, 0) : local;
}

smooth smoothlang_anc2020_bool__bool_to_cbool (smooth x) {
  return SMOOTH_APPLY(SMOOTH_APPLY(x, TRUE), FALSE);
}

smooth smoothlang_anc2020_bool__cbool_to_bool (smooth a) {
  return SMOOTH_C_CREATE(bool_inner, 2, a);
}

smooth smoothlang_anc2020_bool__cbool_if (smooth test, smooth_lazy* a,
                                                       smooth_lazy* b) {
  return SMOOTH_FORCE(test ? a : b);
}

