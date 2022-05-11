/******************************************************************************************************************************************
File name:			scanner.c
Compiler:			Microsoft Visual Studio 2017 Enterprise edition
Author:				Daniel Dias / 040893258
Course:				CST8152 - Compilers / Lab section 11
Assignment:			2
Date:				18 March 2019
Professor:			Svillen Ranev
Purpose:			Implements a lexical scanner
Function list:		scanner_init(), malar_next_token(), char_class(), get_next_state(), iskeyword(),
					aa_func02(), aa_func03(), aa_func05(), aa_func08(), aa_func10(), aa_func11(), aa_func12()
******************************************************************************************************************************************/

/* The #define _CRT_SECURE_NO_WARNINGS should be used in MS Visual Studio projects
 * to suppress the warnings about using "unsafe" functions like fopen()
 * and standard sting library functions defined in string.h.
 * The define does not have any effect in Borland compiler projects.
 */
#define _CRT_SECURE_NO_WARNINGS

#include <stdio.h>   /* standard input / output */
#include <ctype.h>   /* conversion functions */
#include <stdlib.h>  /* standard library functions and constants */
#include <string.h>  /* string functions */
#include <limits.h>  /* integer types constants */
#include <float.h>   /* floating-point types constants */

/*#define NDEBUG        to suppress assert() call */
#include <assert.h>  /* assert() prototype */

/* project header files */
#include "buffer.h"
#include "token.h"
#include "table.h"

#define DEBUG  /* for conditional processing */
#undef  DEBUG

/* Global objects - variables */
/* This buffer is used as a repository for string literals.
   It is defined in platy_st.c */
extern Buffer * str_LTBL; /*String literal table */
int line; /* current line number of the source code */
extern int scerrnum;     /* defined in platy_st.c - run-time error number */

/* Local(file) global objects - variables */
static Buffer *lex_buf;/*pointer to temporary lexeme buffer*/
static pBuffer sc_buf; /*pointer to input source buffer*/
/* No other global variable declarations/definitiond are allowed */

/* scanner.c static(local) function  prototypes */ 
static int char_class(char c); /* character class function */
static int get_next_state(int, char, int *); /* state machine function */
static int iskeyword(char * kw_lexeme); /*keywords lookup functuion */

/*Initializes scanner */
int scanner_init(Buffer * psc_buf) {
  	if(b_isempty(psc_buf)) return EXIT_FAILURE;/*1*/
	/* in case the buffer has been read previously  */
	b_rewind(psc_buf);
	b_clear(str_LTBL);
	line = 1;
	sc_buf = psc_buf;
	return EXIT_SUCCESS;/*0*/
/*   scerrnum = 0;  *//*no need - global ANSI C */
}
/*****************************************************************************************************************************************
Purpose:				Gets and processes each token
Author:					Daniel Dias / 040893258
History/Versions:		1.1
Called functions:		b_getc()
						b_retract()
						b_reset()
						b_location()
						b_mark()
						b_getoffset()
						b_addc()
						b_allocate()
						b_compact()
						strcpy()
Parameters:				-
Return value:			t - returns token
Algorithm:				Reads one character at a time from sc_buf
						process each individual token
						breaks out of loop if SEOF is found
						returns a token structure if it matches
						returns an error token incase of any errors
******************************************************************************************************************************************/
Token malar_next_token() {
	
		Token t = { 0 }; /* token to return after pattern recognition. Set all structure members to 0 */
		unsigned char c; /* input symbol */
		int state = 0; /* initial state of the FSM */
		short lexstart;  /*start offset of a lexeme in the input char buffer (array) */
		short lexend;    /*end   offset of a lexeme in the input char buffer (array)*/
		int accept = NOAS; /* type of state - initially not accepting */
		char tempC;
		int i = 0;
	
		while (1) { /* endless loop broken by token returns it will generate a warning */ 
			/* Part 1: Implementation of token driven scanner */

			//GET THE NEXT SYMBOL FROM THE INPUT BUFFER 
			c = b_getc(sc_buf);
			/*if (c == SEOF || c == '\0') {
				t.code = SEOF_EOF;
				t.attribute.seof = SEOF;
				return t;
			}*/
			if (c == SEOF || c == '\0') {
				t.code = SEOF_T;
				t.attribute.seof = SEOF_EOF;
				return t;
			}
			if (isspace(c)) {  /* isspace() is used to detect all empty spaces */
				if (c == '\n') line++;
				continue;
			}
			switch (c){

			case '!':
				tempC = c = b_getc(sc_buf);
				/*If ! is not followed by another !, it is an error*/
				 if (tempC != '!')
				{
					t.code = ERR_T;
					t.attribute.err_lex[0] = '!';
					
					if (c != '\n') {
						t.attribute.err_lex[1] = c;
					}
					t.attribute.err_lex[2] = '\0';
				}

				while (c != '\n')
				{
					c = b_getc(sc_buf);
					if (c == SEOF || c == '\0')
					{
						t.code = SEOF_T;
						t.attribute.seof = SEOF;
						return t;
					}
				}				
				line++;
				if (tempC != '!')
					return t;				
				continue;

			case '=':
				tempC = b_getc(sc_buf);
				/*If = is followed by another = , set relational operator token*/
				if (tempC == '=')
				{
					t.code = REL_OP_T;
					t.attribute.rel_op = EQ;
					return t;
				}
				/*If = is not followed by = , retract and set assignment operator token*/
				b_retract(sc_buf);
				t.code = ASS_OP_T;
				return t;

			case '-': t.code = ART_OP_T; t.attribute.arr_op = MINUS; return t;
			case '+': t.code = ART_OP_T; t.attribute.arr_op = PLUS; return t;
			case '/': t.code = ART_OP_T; t.attribute.arr_op = DIV; return t;
			case '*': t.code = ART_OP_T; t.attribute.arr_op = MULT; return t;
			case '>': t.code = REL_OP_T; t.attribute.rel_op = GT; return t;
			case '<':
				/*Get next character token*/
				tempC = b_getc(sc_buf);
				/*Check for relation operator*/
				if (tempC == '>')
				{	
					t.code = REL_OP_T;
					t.attribute.rel_op = NE;
					return t;
				}
				/*Check for string concatenation*/
				else if (tempC == '<') {
					t.code = SCC_OP_T;
					return t;
				}
				/*If neither match, set t to relation op and set attribute to less than (>)*/
				b_retract(sc_buf);
				t.code = REL_OP_T;
				t.attribute.rel_op = LT;
				return t;
			case '.':

				b_mark(sc_buf, b_getcoffset(sc_buf));
				/*Get next character*/
				tempC = b_getc(sc_buf);
				/*Check for .AND. operator*/
				if (tempC == 'A' && b_getc(sc_buf) == 'N' && b_getc(sc_buf) == 'D' && b_getc(sc_buf) == '.')
				{
					t.code = LOG_OP_T;
					t.attribute.log_op = AND;
					return t;
				}
				/*Check for .OR. operator*/
				else if (tempC == 'O' && b_getc(sc_buf) == 'R' && b_getc(sc_buf) == '.')
				{
					t.code = LOG_OP_T;
					t.attribute.log_op = OR;
					return t;
				}
				/*If neither match, return error token*/
				else
				{
					b_reset(sc_buf);
					t.code = ERR_T;
					t.attribute.err_lex[0] = '.';
					t.attribute.err_lex[1] = '\0';
					return t;
				}
			case '(': t.code = LPR_T; return t;
			case ')': t.code = RPR_T; return t;
			case '{': t.code = LBR_T; return t;
			case '}': t.code = RBR_T; return t;
			case ',': t.code = COM_T; return t;
			case ';': t.code = EOS_T; return t;
			
			default:
					/*Finite state machine*/
					//FSM0.Begin with state = 0 and the input character c
					
					lexstart = b_mark(sc_buf, b_getcoffset(sc_buf) - 1);
					state = 0;

					//FSM1.Get the next state from the transition table
					state = get_next_state(state, c, &accept);
				
					//	FSM3.If the state is not accepting(accept == NOAS), go to step FSM1
					//	If the step is accepting, token is found, leave the machine and
					//	call an accepting function as described below.
					while (accept == NOAS)
					{

						//FSM2.Get the next character;
						c = b_getc(sc_buf);
						state = get_next_state(state, c, &accept);
						
					}
					/*If accept reaches an accepting state with retract, call b_retract*/
					if (accept == ASWR) {

						b_retract(sc_buf);
					}
					
					/*Set lexend*/
					lexend = b_getcoffset(sc_buf);
					/*Create a buffer*/
					(lex_buf = b_allocate(lexend - lexstart, 0, 'f'));

					/*Retract getc_offset to the mark set at the beginning*/
					b_reset(sc_buf);

				/*Copy lexeme between lexstart and lexend into the lex_bif using add_c */
				for (i = lexstart; i < lexend; i++) 
				{
					c = b_getc(sc_buf);
					b_addc(lex_buf, c); 
				}
				b_compact(lex_buf, '\0');
				/*If VID, FPL or IL is recognized, call the respective accepting fucntion using the aa_table*/
				t = aa_table[state](b_location(lex_buf));

					b_free(lex_buf);
					return t;
				}

			} //end switch
		} /*end while loop*/
	
/*****************************************************************************************************************************************
Purpose:				Gets the next state from the transition table
Author:					Daniel Dias / 040893258
History/Versions:		1.0
Called functions:		char_class()
Parameters:				state
						c
						accept
Return value:			next
******************************************************************************************************************************************/
int get_next_state(int state, char c, int *accept)
   {
	  
	   int col;
	   int next;
	   col = char_class(c);
	   next = st_table[state][col];
#ifdef DEBUG
	   printf("Input symbol: %c Row: %d Column: %d Next: %d \n", c, state, col, next);
#endif

	   assert(next != IS);

#ifdef DEBUG
	   if (next == IS) {
		   printf("Scanner Error: Illegal state:\n");
		   printf("Input symbol: %c Row: %d Column: %d\n", c, state, col);
		   exit(1);
	   }
#endif
	   *accept = as_table[next];
	   return next;
   }
/*****************************************************************************************************************************************
Purpose:				Gets an index for the column in the transition table
Author:					Daniel Dias / 040893258
History/Versions:		1.0
Called functions:		isdigit()
Parameters:				c
Return value:			val
Algorithm:				Compares the parameter (c) to find which column in the transition table it refers to
******************************************************************************************************************************************/
int char_class (char c){
        int val = 0;
		/*Column 1*/
		if (isalpha(c)) {
			val = 0;
		}
		/*Column 2*/
		else if (c == '0') {
			val = 1;
		}
		/*Column 3*/
		else if (c != '0' && isdigit(c)) {
			val = 2;
		}
		/*Column 4*/
		else if (c == '.') {
			val = 3;
		}
		/*Column 5*/
		else if (c == '@') {
			val = 4;
		}
		/*Column 7*/
		else if (c == '"') {
			val = 6;
		}
		/*Column 8*/
		else if ( c == SEOF || c == '\0') {
			val = 7;
		}
		/*Column 6*/
		else {
			val = 5;
		}      
        return val;
}

/*****************************************************************************************************************************************
Purpose:				Accepting function for the arithmentic variable identifier AND keywords
Author:					Daniel Dias / 040893258
History/Versions:		1.0
Called functions:		iskeyword()
Parameters:				lexeme
Return value:			t - token
Algorithm:				Call the iskeyword() function, giving it the lexeme
						if the function returns -1, a keyword was not found
							if a keyword is found, set a token and return
						otherwise, set AVID token
						if the lexeme is longer than VID_LEN, assign the attribute and add a \0 at the end	
******************************************************************************************************************************************/
Token aa_func02(char lexeme[]) {
	Token t = { 0 }; /*Token*/
	int i; 
	int tokenNum = iskeyword(lexeme); //gets the keyword index

#ifdef DEBUG
	printf("lexeme: |%s|\u", lexeme);
#endif
	/*If keywork is found, set token*/
	if (tokenNum != RT_FAIL_1) {
		t.code = KW_T;
		t.attribute.kwt_idx = tokenNum;
		return t;
		}

	/*Set AVID token*/
	t.code = AVID_T;
	/*If lexeme is longer than VID_LEN assign the attribute and add \0 at the end*/
	if (strlen(lexeme) > VID_LEN) {
		for (i = 0; i < VID_LEN; ++i) {
			t.attribute.vid_lex[i] = lexeme[i];
		}
		t.attribute.vid_lex[VID_LEN] = '\0';
	}
	/*Incase lexeme is not longer than VID_LEN*/
	else {
		strcpy(t.attribute.vid_lex, lexeme);
	}
	return t; 
}

/*****************************************************************************************************************************************
Purpose:				Accepting function for the string variable identifier
Author:					Daniel Dias / 040893258
History/Versions:		1.0
Called functions:		strlen()
						aa_table[ES]()
Parameters:				lexeme[]
Return value:			t - a token
Algorithm:				If the lexeme is longer than VID_LEN
							Store the first VID_LEN-1 characters into vid_lex
							Add @ and \0 at the end
						if the lexeme is not longer than VID_LEN
							add \0 (but not @)
******************************************************************************************************************************************/
Token aa_func03(char lexeme[]){
	Token t = { 0 }; /*Token*/
	int i;
	/*Set SVID token*/
	t.code = SVID_T;
#ifdef DEBUG
	printf("lexeme: |%s|\u", lexeme);
#endif

	/*Check if the lexeme is longer than VID_LEN*/
	if (strlen(lexeme) > VID_LEN) {
		/*Store the first VID_LEN-1 characters into vid_lex*/
		for (i = 0; i <= VID_LEN-1; ++i) {
			t.attribute.vid_lex[i] = lexeme[i];
		}
		/*Add @ to the name and add \0 at the end to make a string*/
		t.attribute.vid_lex[VID_LEN - 1] = '@';
		t.attribute.vid_lex[VID_LEN] = '\0';
	}
	/*Incase lexeme is not longer than VID_LEN*/
	else {
		strncpy(t.attribute.vid_lex, lexeme, strlen(lexeme));
		t.attribute.vid_lex[strlen(lexeme)] = '\0';
	}
  return t;
}

/*****************************************************************************************************************************************
Purpose:				Accepting function for the integer literal
Author:					Daniel Dias / 040893258
History/Versions:		1.0
Called functions:		atol()
						aa_table[ES]()
Parameters:				lexeme[]
Return value:			t - a token
Algorithm:				If the lexeme is within range of 2-byte integer
							Call error state function to get the error token
						If num is within range, set integer literal token
******************************************************************************************************************************************/
Token aa_func05(char lexeme[]) {
	Token t = { 0 }; /*Token*/
	long num = atol(lexeme);
	
#ifdef DEBUG
	printf("lexeme: |%s|\u", lexeme);
#endif
	/*Check if num is within range of 2-byte integer*/
	if (num > SHRT_MAX || num < SHRT_MIN) {
		/*Call error state function to get the error token*/
		t = aa_table[ES](lexeme);
	}
	/*If num is within range, set integer literal token*/
	else {
		t.code = INL_T;
		t.attribute.int_value = num;
	}
	return t;
}
/*****************************************************************************************************************************************
Purpose:				Accepting function for the floating point literal
Author:					Daniel Dias / 040893258
History/Versions:		1.0
Called functions:		atof()
						aa_table[ES]()
Parameters:				lexeme[]
Return value:			t - a token
Algorithm:				If the lexeme is in the range as a 4-byte float
							call an error state to get the error token
						otherwise, set the token
******************************************************************************************************************************************/
Token aa_func08(char lexeme[]){
	Token t = { 0 }; /*Token*/
	double num = atof(lexeme);
	
#ifdef DEBUG
	printf("lexeme: |%s|\u", lexeme);
#endif
	/*Check if num is in the same range as the value of 4-byte float*/
	if (num > FLT_MAX || (num < FLT_MIN && num > 0)) {
		/*Call error state function to get the error token*/
		t = aa_table[ES](lexeme);
	}
	else {
		/*Set token*/
		t.code = FPL_T;
		t.attribute.flt_value = (float)num;
	}
  return t;
}

/*****************************************************************************************************************************************
Purpose:				Accepting function for string literals
Author:					Daniel Dias / 040893258
History/Versions:		1.0
Called functions:		b_limit()
						b_addc()
Parameters:				lexeme[]
Return value:			t - a token
Algorithm:				if the lexeme doesnt contain "
						copies the first character of the lexeme into the buffer
						if the lexeme contains \n, increment the line
						add \0 to make c-type string
******************************************************************************************************************************************/
Token aa_func10(char lexeme[]){
	Token t = { 0 }; /*Token*/
	int i;

#ifdef DEBUG
	printf("lexeme: |%s|\u", lexeme);
#endif

	t.attribute.str_offset = b_limit(str_LTBL);
	/*Copy the first char of the lexeme into the buffer*/
	for (i = 0; i < strlen(lexeme); i++) {
		
		if (lexeme[i] != '"') {
			b_addc(str_LTBL, lexeme[i]);
			/*If line terminator found, increment line*/
			if (lexeme[i] == '\n') {
				line++;
			}
		}
	}
	/*Add \0 at the end to make c-type string*/
	b_addc(str_LTBL, '\0');
	/*Set token*/
	t.code = STR_T;
  return t;
}

/*****************************************************************************************************************************************
Purpose:				Accepting for the error token with no retract
Author:					Daniel Dias / 040893258
History/Versions:		1.0
Called functions:		strlen()
Parameters:				lexeme[]
Return value:			t - a token
Algorithm:				If the error lexeme is longer than ERR_LEN
							only the first 3 characters are stored along with 3 dots
							if the error lexeme contains a line terminator, increment the line
******************************************************************************************************************************************/
Token aa_func11(char lexeme[]){
	Token t = { 0 }; /*Token*/
	int i;
	t.code = ERR_T;
	/*If the error lexeme is longer than ERR_LEN on the first 3 characters are stored and 3 dots are added*/
	if (strlen(lexeme) > ERR_LEN) {
		strncpy(t.attribute.err_lex, lexeme, ERR_LEN - 3);
		t.attribute.err_lex[ERR_LEN - 3] = '.';
		t.attribute.err_lex[ERR_LEN - 2] = '.';
		t.attribute.err_lex[ERR_LEN - 1] = '.';
		t.attribute.err_lex[ERR_LEN] = '\0';
	}
	else {
		strncpy(t.attribute.err_lex, lexeme, ERR_LEN - 3);
		t.attribute.err_lex[ERR_LEN] = '\0';
	}
	/*Check if the error lexeme contains line terminators*/
	for (i = 0; i < strlen(lexeme); i++)
	{
		if (lexeme[i] == '\n')
		{
			line++;
		}
	}
	return t;
}
/*****************************************************************************************************************************************
Purpose:				Accepting function for error token with retract
Author:					Daniel Dias / 040893258
History/Versions:		1.0
Called functions:		aa_table[ES]()
Parameters:				lexeme[]
Return value:			t - a token	
******************************************************************************************************************************************/
Token aa_func12(char lexeme[])
{
	Token t = { 0 }; /*Token*/
	t = aa_table[ES](lexeme);
	return t;

}
/*****************************************************************************************************************************************
Purpose:				Checks if the provided lexeme is a keyword
Author:					Daniel Dias / 040893258
History/Versions:		1.0
Called functions:		strncmp(), strlen()
Parameters:				kw_lexeme - lexeme to be evaluated to see if it's a keyword
Return value:			RT_FAIL_1 - Incase a keyword is not found
						i - index for keyword found in the kw_table array
Algorithm:				Iterates through the table, checking if the parameter variable matches
******************************************************************************************************************************************/
int iskeyword(char * kw_lexeme)
{
	int i; 

	for (i = 0; i < KWT_SIZE; ++i)
		if (strcmp(kw_table[i], kw_lexeme) == 0) return i;
	
	return RT_FAIL_1;
}
