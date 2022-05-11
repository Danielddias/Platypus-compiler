/******************************************************************************************************************************************
File name:			table.h
Compiler:			Microsoft Visual Studio 2017 Enterprise edition
Author:				Daniel Dias / 040893258
Course:				CST8152 - Compilers / Lab section 11
Assignment:			2
Date:				18 March 2019
Professor:			Svillen Ranev
Purpose:			header file, contains the regular expression and transition table
Function list:		scanner_init(), malar_next_token(), char_class(), get_next_state(), iskeyword(),
					aa_func02(), aa_func03(), aa_func05(), aa_func08(), aa_func10(), aa_func11(), aa_func12()
******************************************************************************************************************************************/

#ifndef  TABLE_H_
#define  TABLE_H_ 

#ifndef BUFFER_H_
#include "buffer.h"
#endif

#ifndef NULL
#include <_null.h> /* NULL pointer constant is defined there */
#endif

#define SEOF 255
/*  Special case tokens processed separately one by one
 *  in the token-driven part of the scanner
 *  '=' , ' ' , '(' , ')' , '{' , '}' , == , <> , '>' , '<' , ';',
 *  white space
 *  !!comment , ',' , ';' , '-' , '+' , '*' , '/', << ,
 *  .AND., .OR. , SEOF, 'illegal symbol',
 */
 
#define ES  11 /* Error state  with no retract */
#define ER  12 /* Error state  with retract */
#define IS -1    /* Inavalid state */

#define TABLE_COLUMNS 8
/*transition table - type of states defined in separate table */
int  st_table[][TABLE_COLUMNS] = {
	/* State 0 */  { 1,   6,   4,   ES,   ES,    ES,     9,    IS},
	/* State 1 */  { 1,   1,   1,    2,    3,     2,    ES,    2},
	/* State 2 */  { IS, IS,  IS,   IS,   IS,    IS,    IS,    IS},
	/* State 3 */  { IS, IS,  IS,   IS,   IS,    IS,    IS,    IS},
	/* State 4 */  { ES,  4,   4,    7,    5,     5,    ES,    ES},
	/* State 5 */  { IS, IS,  IS,   IS,   IS,    IS,    IS,    IS},
	/* State 6 */  { ES,  6,  ES,    7,   ES,     5,    ES,    5},
	/* State 7 */  { 8,   7,   7,    8,    8,     8,    ES,    8},
	/* State 8 */  { IS, IS,  IS,   IS,   IS,    IS,    IS,    IS},
	/* State 9 */  { 9,   9,   9,    9,    9,     9,    10,    12},
	/* State 10 */ { IS, IS,  IS,   IS,   IS,    IS,    IS,    IS},
	/* State 11 */ { IS, IS,  IS,   IS,   IS,    IS,    IS,    IS},
	/* State 12 */ { IS, IS,  IS,   IS,   IS,    IS,    IS,    IS}

};

	#define ASWR     0  /* accepting state with retract */
	#define ASNR     1  /* accepting state with no retract */
	#define NOAS     2  /* not accepting state */

	int as_table[] = {
	/* State 0 */	NOAS,
	/* State 1 */	NOAS,
	/* State 2 */	ASWR,
	/* State 3 */	ASNR,
	/* State 4 */	NOAS,
	/* State 5 */	ASWR,
	/* State 6 */	NOAS,
	/* State 7 */	NOAS,
	/* State 8 */	ASWR,
	/* State 9 */	NOAS,
	/* State 10 */	ASNR,
	/* State 11 */	ASNR,
	/* State 12 */	ASWR
	};

/* Accepting action function declarations */

Token aa_func02(char lexeme[]); 
Token aa_func03(char lexeme[]); 
Token aa_func05(char lexeme[]); 
Token aa_func08(char lexeme[]); 
Token aa_func10(char lexeme[]);
Token aa_func11(char lexeme[]); 
Token aa_func12(char lexeme[]); 

/* defining a new type: pointer to function (of one char * argument) 
   returning Token
*/  

typedef Token (*PTR_AAF)(char *lexeme);

PTR_AAF aa_table[] = {
	/*state 0*/  NULL,
	/*state 1*/  NULL,
	/*state 2*/  aa_func02,
	/*state 3*/  aa_func03,
	/*state 4*/  NULL,
	/*state 5*/  aa_func05,
	/*state 6*/  NULL,
	/*state 7*/  NULL,
	/*state 8*/  aa_func08,
	/*state 9*/  NULL,
	/*state 10*/ aa_func10,
	/*state 11*/ aa_func11,
	/*state 12*/ aa_func12

};

#define KWT_SIZE  10

char * kw_table []=
	{
	"ELSE",
	"FALSE",
	"IF",
	"PLATYPUS",
	"READ",
	"REPEAT",
	"THEN",
	"TRUE",
	"WHILE",
	"WRITE"   
	};

#endif
