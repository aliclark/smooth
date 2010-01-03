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

#define  iocons smoothlang_anc2020_iocons__iocons
smooth_t iocons (smooth_t, smooth_t);

#include <stdio.h> /* Needed for definition of typedef FILE or stdin,stdout */

smooth_t smoothlang_anc2020_iochar__stdin;
smooth_t smoothlang_anc2020_iochar__stdout;
smooth_t smoothlang_anc2020_iochar__stderr;

void smoothlang_anc2020_iochar (void) {
  smoothlang_anc2020_iochar__stdin  = (smooth_t) stdin;
  smoothlang_anc2020_iochar__stdout = (smooth_t) stdout;
  smoothlang_anc2020_iochar__stderr = (smooth_t) stderr;
}

smooth_t smoothlang_anc2020_iochar__cfputc (smooth_t c, smooth_t s, smooth_t i) {
  return iocons(putc(c, (FILE*) s), i);
}

smooth_t smoothlang_anc2020_iochar__cfgetc (smooth_t s, smooth_t i) {
  return iocons(getc((FILE*) s), i);
}


/* These can be used instead if our optimiser recognises opportunities to use them. */

smooth_t smoothlang_anc2020_iochar__fputc_opt (smooth_t c, smooth_t s) {
  return putc(c, (FILE*) s);
}

smooth_t smoothlang_anc2020_iochar__fgetc_opt (smooth_t s) {
  return getc((FILE*) s);
}

/* If we get *really* good at optimisation,
 * we might be able to replace consequetive fputc calls with this. */
smooth_t smoothlang_anc2020_iochar__fputs_opt (smooth_t x, smooth_t s) {
  return fputs((char*) x, (FILE*) s);
}
