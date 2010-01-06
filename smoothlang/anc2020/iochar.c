/*SMOOTH (init-module) (exposes putc getc puts) */

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

#include <stdio.h> /* Needed for definition of typedef FILE or stdin,stdout */

#define iocons smoothlang_anc2020_iocons__iocons
#define cfputc(c, s) putc((int) c, (FILE*) s)
#define cfgetc(s)    getc((FILE*) s)

smooth_t iocons (smooth_t, smooth_t);

smooth_t smoothlang_anc2020_iochar__stdin;
smooth_t smoothlang_anc2020_iochar__stdout;
smooth_t smoothlang_anc2020_iochar__stderr;

smooth_t smoothlang_anc2020_iochar__iochar_init (smooth_t z) {
  smoothlang_anc2020_iochar__stdin  = (smooth_t) stdin;
  smoothlang_anc2020_iochar__stdout = (smooth_t) stdout;
  smoothlang_anc2020_iochar__stderr = (smooth_t) stderr;
  return iocons(0, z);
}

smooth_t smoothlang_anc2020_iochar__cfputc_opt (smooth_t c, smooth_t s) {
  return cfputc(c, s);
}

smooth_t smoothlang_anc2020_iochar__cfgetc_opt (smooth_t s) {
  return cfgetc(s);
}

smooth_t smoothlang_anc2020_iochar__cfputc (smooth_t c, smooth_t s, smooth_t i) {
  return iocons(cfputc(c, s), i);
}

smooth_t smoothlang_anc2020_iochar__cfgetc (smooth_t s, smooth_t i) {
  return iocons(cfgetc(s), i);
}

/* If we get *really* good at optimisation,
 * we might be able to replace consequetive cfputc_opt calls with this. */
smooth_t smoothlang_anc2020_iochar__cfputs_opt (smooth_t x, smooth_t s) {
  return fputs((char*) x, (FILE*) s);
}
