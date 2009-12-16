/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */

/*
    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

/*
 *  ali_memory.h
 *
 *  Copyright (C) 2009  Ali Clark
 */

#ifndef _ALI_MEMORY_H
#define _ALI_MEMORY_H

void  free (void* ptr);
void* malloc (unsigned long int size);

/* _ attribute _ */
#ifndef __attribute__
#  define __attribute__(x)  /*0*/
#endif /* __attribute__ */

#define NIL 0

#define MALLOCATE(num)              check_not_null(malloc(ceil_base2(num)))

#define TALLOCATE( type, num)      ((type *) MALLOCATE( (num) * sizeof( type)))
#define TALLOCATED( type, name, num) type *name = TALLOCATE( type, num)
#define STALLOCATED( type, name)     TALLOCATED( type, name, 1)

#define FREE( ptr) free( ptr); ptr = NIL

#ifdef __cplusplus
extern "C" {
#endif

         void* check_not_null (void* pointer);

	/* Round up to nearest base2 number. */
	unsigned long int ceil_base2 (const unsigned long int number) __attribute__((const));

#ifdef __cplusplus
}
#endif

#endif /* _ALI_MEMORY_H */

