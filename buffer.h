/******************************************************************************************************************************************
File name:			buffer.h
Compiler:			Microsoft Visual Studio 2017 Enterprise edition
Author:				Daniel Dias / 040893258
Course:				CST8152 - Compilers / Lab section 11
Assignment:			1
Date:				4 February 2019
Professor:			Svillen Ranev
Purpose:			Header file - contains all constants, function declarations and data types declrations needed for the buffer.c file
Function list:		b_allocate(), b_addc(), b_clear(), b_free(), b_isfull(), b_limit(), b_capacity(), b_mark()
					b_mode(), b_incfactor(), b_load(), b_isempty(), b_getc(), b_eob(), b_print(), b_compact()
					b_rflag(), b_retract(), b_reset(), b_getcoffset(), b_rewind(), b_location()
******************************************************************************************************************************************/
#ifndef BUFFER_H_
#define BUFFER_H_

/*#pragma warning(1:4001) *//*to enforce C89 type comments  - to make //comments an warning */

/*#pragma warning(error:4001)*//* to enforce C89 comments - to make // comments an error */

/* standard header files */
#include <stdio.h>  /* standard input/output */
#include <malloc.h> /* for dynamic memory allocation*/
#include <limits.h> /* implementation-defined data type ranges and limits */

/* constant definitions */
#define RT_FAIL_1 -1         /* operation failure return value 1 */
#define RT_FAIL_2 -2         /* operation failure return value 2 */
#define LOAD_FAIL -2         /* load fail return value */

#define DEFAULT_INIT_CAPACITY 200   /* default initial buffer capacity */
#define DEFAULT_INC_FACTOR 15       /* default increment factor */

#define RT_PASS_1 0
#define RT_PASS_2 1
#define MAXIMUM_ALLOWED_POSITIVE_VALUE (SHRT_MAX-1)
#define FIXED 'f'
#define ADDITIVE 'a'
#define MULTIPLICATIVE 'm'
#define FIXED_MODE 0
#define ADDITIVE_MODE 1
#define MULTIPLICATIVE_MODE -1
#define RESET_OFFSET 0
#define ZERO 0
#define HUNDRED 100
#define HEX100 0x100

#define DEFAULT_FLAGS 0xFFFC
#define SET_EOB 0x0002
#define RESET_EOB 0xFFFD
#define CHECK_EOB 0x0002
#define SET_R_FLAG 0x0001
#define RESET_R_FLAG 0xFFFE
#define CHECK_R_FLAG 0x0001

/* user data type declarations */
typedef struct BufferDescriptor {
	char *cb_head;   /* pointer to the beginning of character array (character buffer) */
	short capacity;    /* current dynamic memory size (in bytes) allocated to character buffer */
	short addc_offset;  /* the offset (in chars) to the add-character location */
	short getc_offset;  /* the offset (in chars) to the get-character location */
	short markc_offset; /* the offset (in chars) to the mark location */
    char inc_factor; /* character array increment factor */
	char mode;       /* operational mode indicator*/
	unsigned short flags;     /* contains character array reallocation flag and end-of-buffer flag */
} Buffer, *pBuffer;
/*typedef Buffer *pBuffer;*/

/* function declarations */
Buffer * b_allocate(short, char, char);
pBuffer b_addc(pBuffer const, char);
int b_clear(Buffer * const);
void b_free(Buffer * const);
int b_isfull(Buffer * const);
short b_limit(Buffer * const);
short b_capacity(Buffer * const);
short b_mark(pBuffer const, short);
int b_mode(Buffer * const);
size_t b_incfactor(Buffer * const);
int b_load(FILE * const, Buffer * const);
int b_isempty(Buffer * const);
char b_getc(Buffer * const);
int b_eob(Buffer * const);
int b_print(Buffer * const);
Buffer * b_compact(Buffer * const, char);
char b_rflag(Buffer * const);
short b_retract(Buffer * const);
short b_reset(Buffer * const);
short b_getcoffset(Buffer * const);
int b_rewind(Buffer * const);
char * b_location(Buffer * const);
#endif