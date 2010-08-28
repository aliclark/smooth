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

smooth iocons (smooth, smooth);

/*TYPE:(c_FILE a)*/ smooth smoothlang_anc2020_iochar__stdin;
/*TYPE:(c_FILE a)*/ smooth smoothlang_anc2020_iochar__stdout;
/*TYPE:(c_FILE a)*/ smooth smoothlang_anc2020_iochar__stderr;

/*TYPE:(numeral z) -> (iocons b z)*/
smooth smoothlang_anc2020_iochar__iochar_init (smooth z) {
  smoothlang_anc2020_iochar__stdin  = (smooth) stdin;
  smoothlang_anc2020_iochar__stdout = (smooth) stdout;
  smoothlang_anc2020_iochar__stderr = (smooth) stderr;
  return iocons(0, z);
}

smooth smoothlang_anc2020_iochar__cfputc_opt (smooth c, smooth s) {
  return cfputc(c, s);
}

smooth smoothlang_anc2020_iochar__cfgetc_opt (smooth s) {
  return cfgetc(s);
}

/*TYPE:(c_Char a) -> (c_FILE s) -> (numeral i) -> (iocons c_Char i)*/
smooth smoothlang_anc2020_iochar__cfputc (smooth c, smooth s, smooth z) {
  return iocons(cfputc(c, s), z);
}

/*TYPE:(c_FILE s) -> (numeral i) -> (iocons c_Char i)*/
smooth smoothlang_anc2020_iochar__cfgetc (smooth s, smooth z) {
  return iocons(cfgetc(s), z);
}

/* If we get *really* good at optimisation,
 * we might be able to replace consequetive cfputc_opt calls with this. */
smooth smoothlang_anc2020_iochar__cfputs_opt (smooth x, smooth s) {
  return fputs((char*) x, (FILE*) s);
}

