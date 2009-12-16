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
 *  ali_memory.c
 *
 *  Copyright (C) 2009  Ali Clark
 */

#include "smoothlang/anc2020/linkedarray/ali_memory.h"

#define EXIT_FAILURE 1

void exit (int s);
void perror (const char* s);

/*
 * Returns the most significant bit position of n from 0 to 31.
 * -1 returned if no bit is set.
 */
static long int msb (unsigned long int number) __attribute__((const));

/*---------------------------------------------------------------------------*/

static long int msb (unsigned long int n)
{
	long int pos = 0;
	long int tmp = 0;
	tmp = n >> 16;

	if (tmp != 0) {
		n = tmp;
		pos = pos + 16;
	}
	tmp = n >> 8;

	if (tmp != 0) {
		n = tmp;
		pos = pos + 8;
	}
	tmp = n >> 4;

	if (tmp != 0) {
		n = tmp;
		pos = pos + 4;
	}
	tmp = n >> 2;

	if (tmp != 0) {
		n = tmp;
		pos = pos + 2;
	}
	tmp = n >> 1;

	if (tmp != 0) {
		n = tmp;
		pos = pos + 1;
	}
	return pos + n - 1;
}

/*---------------------------------------------------------------------------*/

unsigned long int ceil_base2 (const unsigned long int x)
{
	return (x & (x - 1)) ? ((unsigned long int) 1) << (msb( x) + 1) : x;
}

/*---------------------------------------------------------------------------*/

void *check_not_null (void *ptr)
{
	if (!ptr) {
		perror( "Could not allocate memory.");
		exit( EXIT_FAILURE);
	}
	return ptr;
}

/*---------------------------------------------------------------------------*/

