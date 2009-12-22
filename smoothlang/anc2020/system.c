
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

int   system (const char* s);
char* getenv (const char* s);

smooth_t smoothlang_anc2020_system__argc;
smooth_t smoothlang_anc2020_system__argv;

void smoothlang_anc2020_system (int argc, char** argv) {
  smoothlang_anc2020_system__argc = (smooth_t) argc;
  smoothlang_anc2020_system__argv = (smooth_t) argv;
}

smooth_t smoothlang_anc2020_system__system (smooth_t command, smooth_t z) {
  return iocons(system(command), z);
}

smooth_t smoothlang_anc2020_system__getenv (smooth_t name, smooth_t z) {
  return iocons(getenv(name), z);
}

smooth_t smoothlang_anc2020_system__exit (smooth_t status, smooth_t z) {
  return iocons(exit(status), z);
}
