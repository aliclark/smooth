
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

#ifndef SMOOTH__H
#define SMOOTH__H


#ifdef NO_LIBS

typedef long int smooth;

#else

#include <stdint.h> /* intptr_t */

/*
 * intptr_t is pretty much the only valid option here, and if not available,
 * would recommend typedef'ing intptr_t for your particular platform.
 */
typedef intptr_t smooth;

#endif

/*
 * You should assume that the "smooth" type is large enough to hold a pointer,
 * but could also hold some signed integer type numeric value of reasonable size (in similar usage to "int")
 *
 * Likewise for smooth_size, except in this case do not assume whether smooth_size is unsigned or signed
 * (only use unsigned values for it).
 *
 * Additionally, do not assume the size of smooth_size - it will be reasonably sized as with "int",
 * but if in doubt please split the operation over multiple calls with smaller smooth_size value.
 * (the interface is such that it is always possible to do this for calls taking smooth_size.)
 *
 * A smooth_size is not necessarily the same underlying type as the "smooth" type,
 * and thus casting between the two is not recommended unless certain it is with reasonably sized
 * unsigned values.
 *
 * An implementation may or may not define some way to access the program arguments.
 * It may or may not provide this information only to a certain module(s) through which
 * access to the arguments can be made.
 *
 * At current, the main implementation of Smooth provides these as smooth_argc and smooth_argv,
 * but if likely to be reusing code for example running an executable on the metal
 * or a different runtime, it is recommended to implement a more abstracted method of access.
 */


/*
 * This could be anything of many different types, with "unsigned int"
 * being a likely candidate, but for now reusing smooth type keeps everything nicely contained.
 */
typedef smooth smooth_size;


#ifdef __cplusplus
extern "C" {
#endif


  /*
   * As stated before, these are non standard but there's not much reason
   * that a C implementation couldn't provide these
   */
  extern int    smooth_argc;
  extern char** smooth_argv;


  /*
   * Conservate - freeptr only gets called when there is no reference to that _value_ remaining,
   * even if the remaining references are using the same value for something entirely different.
   */
  void smooth_gc_register (smooth ptr,  void (*freeptr)(smooth));
  void smooth_gc_incref   (smooth ptr);
  void smooth_gc_decref   (smooth ptr);


  /*
   * Since closures are an integral part of the language it is important they provide very fast access,
   * hence the large number of parameters for this call.
   *
   * The call will create memory for storage of 'n' values, and will fill in 'args_count' of these
   * from the 'args' array right off the bat. (use &local, and 1) for these value if you just have
   * the one smooth value you want to pass - no need to create a new array for it.
   *
   * Any remaining values up to 'n' will be filled up on consecutive calls to the closure,
   * and finally when all values are filled, a call to smooth_apply will execute the 'code' function,
   * with the closure itself passed as first argument, and the fateful last argument as the second.
   *
   * Although that interface would be complete on its own allowing you to store any values,
   * it is useful in some cases to pass a 'parent' argument,
   * so that if you do a lookup on the closure higher than the 'n'th value,
   * it will look inside the parent for the value, and so on.
   *
   * This itself is not perfect, because there is now little control over what you can supply for 'parent'
   * but since the code will need to be aware of the parent's structure anyway, we can easily ignore
   * useless values.
   */
  smooth smooth_closure_create (smooth code, smooth_size n, smooth* args, smooth_size args_count, smooth parent);

  /* Standard O(1) value lookup */
  smooth smooth_closure_lookup (smooth closure, smooth_size i);


  /*
   * Keep applying 'n' arguments from the array 'args' to the function 'fn' and those returned on application.
   * If 'fn' is a closure with unfilled arguments, this will be faster than simply calling with one argument at a time,
   * because we can just keep filling up the values copying them from 'args'.
   *
   * For cases where 'fn' is a lambda or primitive however, the arguments are applied one at a time,
   * since the lambda or primitive will likely use this to decide what it returns.
   */
  smooth smooth_apply (smooth fn, smooth* args, smooth_size n);


#ifdef __cplusplus
}
#endif

#endif /* SMOOTH__H */

