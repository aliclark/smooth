/*SMOOTH (depends smoothlang/anc2020/iocons smoothlang/anc2020/numeral ) */

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

#define  iocons smoothlang_anc2020_iocons__iocons
smooth_t iocons (smooth_t, smooth_t);

#include <stdio.h> /* Needed for definition of typedef FILE or stdin,stdout */

smooth_t smoothlang_anc2020_iochar_stdin;
smooth_t smoothlang_anc2020_iochar_stdout;

void smoothlang_anc2020_iochar (void) {
  smoothlang_anc2020_iochar_stdin  = (smooth_t) stdin;
  smoothlang_anc2020_iochar_stdout = (smooth_t) stdout;
}

smooth_t smoothlang_anc2020_iochar__cputc (smooth_t s, smooth_t c, smooth_t i) {
  return iocons(putc(c, s), i);
}

smooth_t smoothlang_anc2020_iochar__cgetc (smooth_t s, smooth_t i) {
  return iocons(getc(s), i);
}
