/******************************************************************************************************************************************
File name:			buffer.c
Compiler:			Microsoft Visual Studio 2017 Enterprise edition
Author:				Daniel Dias / 040893258
Course:				CST8152 - Compilers / Lab section 11
Assignment:			1
Date:				4 February 2019
Professor:			Svillen Ranev
Purpose:			Create a buffer structure to take input from a source file and output it to an output file

Function list:		b_allocate(), b_addc(), b_clear(), b_free(), b_isfull(), b_limit(), b_capacity(), b_mark()
					b_mode(), b_incfactor(), b_load(), b_isempty(), b_getc(), b_eob(), b_print(), b_compact()
					b_rflag(), b_retract(), b_reset(), b_getcoffset(), b_rewind(), b_location()
******************************************************************************************************************************************/

#include <stdlib.h>
#include <limits.h>
#include "buffer.h"

/******************************************************************************************************************************************
Purpose: Allocate memory for a buffer pointer and assign the capacity, inc factor and the mode of the buffer
Author: Daniel Dias / 040893258 
History/Versions: 1.0
Called functions: calloc(), malloc(), free()
Parameters: short init_capacity, char inc_factor, char o_mode
Return value: NULL, pBD
Algorithm:  -Allocates a buffer pointer
			-sets the flags to their default value
			-check if the init_capacity is 0, if it is; give it the default value(200)
				-Allocate memory for the cb_head using the init_capacity parameter
				-Also assign the default inc_factor to mode 1 or -1 aswell as assign 0 to mode f
			-check the o_mode and assign the buffer structure accordingly
******************************************************************************************************************************************/
Buffer * b_allocate(short init_capacity, char inc_factor, char o_mode) {

	/*Allocate buffer pointer*/
	pBuffer pBD = (Buffer *)calloc(1, sizeof(Buffer));
	/*Set default value to flags*/
	pBD->flags = DEFAULT_FLAGS;

	/*If init_capacity is 0, give default value (200) and ignore inc_factor parameter and assign default inc_factor value (15 or 0) to buffer structure*/
	 if (init_capacity == ZERO ) {

		init_capacity = DEFAULT_INIT_CAPACITY;
		/*Allocate memory using the default init value(200)*/
		pBD->cb_head = malloc(sizeof(unsigned char) * DEFAULT_INIT_CAPACITY);

		if (o_mode == ADDITIVE) {

			pBD->inc_factor = DEFAULT_INC_FACTOR;
			pBD->mode = ADDITIVE_MODE;
		}
		else if (o_mode == MULTIPLICATIVE) {

			pBD->inc_factor = DEFAULT_INC_FACTOR;
			pBD->mode = MULTIPLICATIVE_MODE;
		}
		else if (o_mode == FIXED){

			pBD->inc_factor = FIXED_MODE;
			pBD->mode = FIXED_MODE;
		}
	}
	else if(init_capacity <= MAXIMUM_ALLOWED_POSITIVE_VALUE && init_capacity >= ZERO){
		/*if the init_capacity is not 0, malloc with the init_capacity*/
		pBD->cb_head = malloc(sizeof(unsigned char) * init_capacity);
	}
	/*If memory allocation failed, free pointer and return NULL*/
	if (pBD->cb_head == NULL) {

		free(pBD->cb_head);
		free(pBD);
		return NULL;
	}

		if (o_mode == FIXED) {

			pBD->mode = FIXED_MODE;
			pBD->inc_factor = 0;
		}
		else if (o_mode == ADDITIVE && (unsigned char)inc_factor > 0 && (unsigned char)inc_factor <= 255) {

			pBD->mode = ADDITIVE_MODE;
			pBD->inc_factor = (unsigned char)inc_factor;

		}
		else if (o_mode == MULTIPLICATIVE && (unsigned char)inc_factor > 0 && (unsigned char)inc_factor <= 128) {

			pBD->mode = MULTIPLICATIVE_MODE;
			pBD->inc_factor = (unsigned char)inc_factor; //assign inc_factor to the buffer inc_factor
		}
		/*if the o_mode is invalid, return NULL;*/
		else if (o_mode != FIXED && o_mode != ADDITIVE && o_mode != MULTIPLICATIVE) {
			return NULL;
		}
	pBD->capacity = init_capacity;
	
	return pBD;
}
/******************************************************************************************************************************************
Purpose:			Adds the symbol to the buffer structure, resizes the structure to make space for more symbols
Author:				Daniel Dias / 040893258
History/Versions:	 1.0
Called functions:	sizeof(), realloc()
Parameters:			Buffer * const pBD, char symbol
Return value:		NULL, pBD
Algorithm:			-Checks if addc_offset is smaller then capacity, if it is; add symbol otherwise, resize
					-if the mode is 0, return null
					-if the mode is 1, calculate the new capacity
					-if the mode is -1, calculate the new capacity, increment and available space
					-Allocate a new character buffer with the new capacity
					-If the memory location changed, set the r_flag
					-Assign the new variables
					-Try to add the symbol to the newly reszied buffer
******************************************************************************************************************************************/
pBuffer b_addc(pBuffer const pBD, char symbol) {

	if (!pBD) {
		return NULL;
	}
	/*Variables used in multiplicative mode, used to calculates the new capacity, increment and available space*/
	short new_cap = ZERO, new_inc = ZERO, av_space = ZERO;
	char *temp = NULL;
	/*reset the r_flag*/
	pBD->flags &= RESET_R_FLAG;
	/*If there is space availables, add the symbol and increment addc_offset*/
	if (pBD->addc_offset < pBD->capacity) {
		pBD->cb_head[pBD->addc_offset++] = symbol;
		return pBD;
	}
	/*if the addc offset is equal to the capacity, resize the buffer*/
	if (pBD->addc_offset == pBD->capacity) {

		/*Fixed mode, return NULL*/
		if (pBD->mode == FIXED_MODE) {
			return NULL;
		}
		/*Additive mode, calculate the new capacity by adding inc_factor(in bytes) to capacity*/
		else if (pBD->mode == 1) {

			if (pBD->capacity >= MAXIMUM_ALLOWED_POSITIVE_VALUE) {
				return NULL;
			}
			/*Calculate the new capacity*/
			new_cap = pBD->capacity + sizeof(char) * (unsigned char)pBD->inc_factor;

			/*If the result is below 0, return null; or if it exceeds the Maximum allowed positive value-1, assing that value to the new capacity*/
			if (new_cap < ZERO) {
				return NULL;
			}
			else if (new_cap > MAXIMUM_ALLOWED_POSITIVE_VALUE) {
				new_cap = MAXIMUM_ALLOWED_POSITIVE_VALUE;

			}
		}
		/*Multiplicative mode, calculates the new capacity and a new increment value*/
		else if (pBD->mode == MULTIPLICATIVE_MODE) {

			if (pBD->capacity == MAXIMUM_ALLOWED_POSITIVE_VALUE) {
				return NULL;
			}
			/*Calculate the available space and the new increment*/
			av_space = MAXIMUM_ALLOWED_POSITIVE_VALUE - pBD->capacity;
			new_inc = av_space * (unsigned long)pBD->inc_factor / HUNDRED;
			
			/*Make sure the new capacity is valid*/
			if ((pBD->capacity + new_inc < MAXIMUM_ALLOWED_POSITIVE_VALUE) && new_inc != 0) {
				new_cap = pBD->capacity + new_inc;
			}
			/*if not valid, assign the maximum allowed positive value*/
			else if (pBD->capacity < MAXIMUM_ALLOWED_POSITIVE_VALUE) {
				new_cap = MAXIMUM_ALLOWED_POSITIVE_VALUE;
			}

		}
		else {
			return NULL;
		}
	}
		/*Allocate new character buffer into temp variable*/
		temp = realloc(pBD->cb_head, new_cap);
		/*if allocation fails return NULL*/
		if (temp == NULL) {
			return NULL;
		}

		/*if the location of the char buffer has been moved, set r_flag to 1*/
		 if (temp != (pBD->cb_head)) {
			pBD->cb_head = temp;
			pBD->flags |= SET_R_FLAG;
		}

		/*Assign new capacity*/
		pBD->capacity = new_cap;

		/*If there is available space in the buffer, add the symbol to the buffer structure*/
	if (pBD->addc_offset < pBD->capacity) {
		pBD->cb_head[pBD->addc_offset++] = symbol;
	}
		return pBD;	
}
/******************************************************************************************************************************************
Purpose:			Sets the offsets and flags of the buffer structure to 0
Author:				Daniel Dias / 040893258
History/Versions:	1.0
Called functions:	None
Parameters:			Buffer * const pBD	
Return value:		RT_FAIL_1, RT_PASS_1
Algorithm:-
******************************************************************************************************************************************/
int b_clear(Buffer * const pBD) {
	if (!pBD) {
		return RT_FAIL_1;
	}
	pBD->addc_offset = RESET_OFFSET;
	pBD->getc_offset = RESET_OFFSET;
	pBD->flags &= RESET_EOB;
	pBD->flags &= RESET_R_FLAG;
	pBD->markc_offset = RESET_OFFSET;

	return RT_PASS_1;
}
/******************************************************************************************************************************************
Purpose:			Frees the allocated memory of the buffer
Author:				Daniel Dias / 040893258
History/Versions:	1.0
Called functions:	free()
Parameters:			Buffer * const pBD
Return value: -
Algorithm: -
******************************************************************************************************************************************/
void b_free(Buffer * const pBD) {

	if (pBD) {

		if (pBD->cb_head) {
		free(pBD->cb_head);
		}

		free(pBD);
	}
}
/******************************************************************************************************************************************
Purpose:			Checks if the buffer is full
Author:				Daniel Dias / 040893258
History/Versions:	1.0
Called functions:	None
Parameters:			Buffer * const pBD	
Return value:		RT_FAIL_1, RT_FAIL_1, RT_FAIL_2
Algorithm:			Compares the addc_offset to the capacity, if they are equal, the buffer is full
******************************************************************************************************************************************/
int b_isfull(Buffer * const pBD) {
	if (!pBD) {
		return RT_FAIL_1;
	}
	if (pBD->addc_offset == pBD->capacity) {
		return RT_PASS_2;
	}
	else {
		return RT_PASS_1;
	}
}
/******************************************************************************************************************************************
Purpose:			Returns the addc_offset
Author:				Daniel Dias / 040893258
History/Versions:	1.0
Called functions:	None
Parameters:			Buffer * const pBD	
Return value:		RT_FAIL_1, addc_offset
Algorithm: -
******************************************************************************************************************************************/
short b_limit(Buffer * const pBD) {
	if (!pBD) {
		return RT_FAIL_1;
	}
	else {
		return pBD->addc_offset;
	}
}
/******************************************************************************************************************************************
Purpose:			Returns the buffer capacity
Author:				Daniel Dias / 040893258
History/Versions:	1.0
Called functions:	None
Parameters:			Buffer * const pBD	
Return value:		RT_FAIL_1, capacity
Algorithm: - 
******************************************************************************************************************************************/
short b_capacity(Buffer * const pBD) {
	if (!pBD) {
		return RT_FAIL_1;
	}
	return pBD->capacity;
}
/******************************************************************************************************************************************
Purpose:			Assings the mark parameter to markc_offset if it is valid
Author:				Daniel Dias / 040893258
History/Versions:	1.0
Called functions:	None
Parameters:			Buffer * const pBD, short mark
Return value:		RT_FAIL_1, markc_offset
Algorithm:			Checks if mark is smaller than 0 or if it's bigger than addc_offset
******************************************************************************************************************************************/
short b_mark(pBuffer const pBD, short mark) {
	if (!pBD) {
		return RT_FAIL_1;
	}
	/*Validates the mark parameter, returns -1 if invalid*/
	if (mark < ZERO || mark > pBD->addc_offset) {
		return RT_FAIL_1;
	}
	/*Assigns mark to the markc_offset if its valid*/
	pBD->markc_offset = mark;
	return pBD->markc_offset;
}
/******************************************************************************************************************************************
Purpose:			Returns the mode of the buffer
Author:				Daniel Dias / 040893258
History/Versions:	1.0
Called functions:	None
Parameters:			Buffer * const pBD	
Return value:		RT_FAIL_2, mode
Algorithm: - 
******************************************************************************************************************************************/
int b_mode(Buffer * const pBD) {
	if (!pBD) {
		return RT_FAIL_2;
	}
	return pBD->mode;
}
/******************************************************************************************************************************************
Purpose:			Returns the inc_factor of the buffer
Author:				 Daniel Dias / 040893258
History/Versions:	 1.0
Called functions:	None
Parameters:			Buffer * const pBD	
Return value:		HEX100, inc_factor
Algorithm: - 
******************************************************************************************************************************************/
size_t b_incfactor(Buffer * const pBD) {
	if (!pBD) {
		return HEX100;
	}
	return (unsigned char)pBD->inc_factor;

}
/******************************************************************************************************************************************
Purpose:			Loads the provided input file 
Author:				Daniel Dias / 040893258
History/Versions:	1.0
Called functions:	feof(), fgetc(), ungetc(), b_addc()
Parameters:			FILE * const fi, Buffer * const pBD	
Return value:		RT_FAIL_1, ZERO, LOAD_FAIL, count
Algorithm:			-In a while loop, load each char into a temp variable and pass it as the symbol parameter in b_addc
					-increment counter by 1 
					-use feof() to detect the end of the file
					-if b_addc returns an error, ungetc and print the last char read from the file
******************************************************************************************************************************************/
int b_load(FILE * const fi, Buffer * const pBD) {
	if (!pBD || !fi) {
		return RT_FAIL_1;
	}
	/*Use a variable to get the content and then pass it as a parameter for the b_addc function*/
	char file_cont;
	/*Counter for the amount of characters printed*/
	int counter = ZERO;

	while (!feof(fi)) {
		/*Load file content into a variable*/
		file_cont = (char)fgetc(fi);
		/*if the file is empty, break*/
		if (feof(fi)) {
			break;
		}
		/*if addc returns an error, ungetc and return load_fail*/
		if (!b_addc(pBD, file_cont)) {
			ungetc(file_cont, fi);
			printf("Last character read from input file is: %c %d\n", file_cont, file_cont);
			return LOAD_FAIL;

		}
		counter++;
	}
	return counter;
}
/******************************************************************************************************************************************
Purpose:			Check if addc_offset is 0, returns 1 if it is; otherwise, return 0
Author:				Daniel Dias / 040893258
History/Versions:	1.0
Called functions:	None
Parameters:			Buffer * const pBD	
Return value:		RT_FAIL_1, RT_PASS_2, RT_PASS_1
Algorithm: - 
******************************************************************************************************************************************/
int b_isempty(Buffer * const pBD) {
	if (!pBD) {
		return RT_FAIL_1;
	}
	if (pBD->addc_offset == ZERO) {
		return RT_PASS_2;
	}
	else {
		return RT_PASS_1;
	}
}
/******************************************************************************************************************************************
Purpose:			returns the buffer at the getc_offset+1 location
Author:				Daniel Dias / 040893258
History/Versions:	1.0
Called functions:	None
Parameters:			Buffer * const pBD	
Return value:		RT_FAIL_2, RT_PASS_1, cb_head[pBD->getc_offset]
Algorithm:			-Compares the addc and getc offsets, if they are equal; set eobflag and return 0; 
					-otherwise, return the cb_head at the getc offset+1
******************************************************************************************************************************************/
char b_getc(Buffer * const pBD) {

	if (!pBD) {
		return RT_FAIL_2;
	}
	/*Reset eob flag*/
	pBD->flags &= RESET_EOB;

	/*If the addc_offset and getc_offset are equal, set the EOB flag and return 0*/
	if (pBD->addc_offset == pBD->getc_offset) {
		/*set eob flag*/
		pBD->flags |= SET_EOB;
		return RT_PASS_1; 
	}
	
	return pBD->cb_head[pBD->getc_offset++];
	
}
/******************************************************************************************************************************************
Purpose:			Returns the value of the EOB flag
Author:				Daniel Dias / 040893258
History/Versions:	1.0
Called functions:	None
Parameters:			Buffer * const pBD	
Return value:		RT_FAIL_1, (pBD->flags & CHECK_EOB)
Algorithm: -
******************************************************************************************************************************************/
int b_eob(Buffer * const pBD) {
	if (!pBD) {
		return RT_FAIL_1;
	}
	return (pBD->flags & CHECK_EOB);
}
/******************************************************************************************************************************************
Purpose:			Prints the buffer content 
Author:				Daniel Dias / 040893258
History/Versions:	1.0
Called functions:	b_getc(), b_eob()
Parameters:			Buffer * const pBD	
Return value:		RT_FAIL_1, RT_PASS_1, counter
Algorithm:			-do-while loop to iterate through the buffer
					-if eob is 1, exit loop and print a new line
******************************************************************************************************************************************/
int b_print(Buffer * const pBD) {
	if (!pBD) {
		return RT_FAIL_1;
	}
	if (!pBD->addc_offset) {
		
		return RT_PASS_1;
	}

	/*Counter for the amount of char printed*/
	int counter = ZERO;
	/*Char variable to get the next symbol in the buffer structure*/
	char c;
	do {
		/*Gets next char*/
		c = b_getc(pBD);
		/*if eob is 1, break;*/
		if (b_eob(pBD)) {
			break;
		}
		printf("%c",c);
		counter++;

	} while (!b_eob(pBD));

	printf("\n");
	
	return counter;
}
/******************************************************************************************************************************************
Purpose:			Reallocates memory for a new capacity
Author:				Daniel Dias / 040893258
History/Versions:	1.0
Called functions:	realloc()
Parameters:			Buffer * const pBD, char symbol
Return value:		NULL, pBD
Algorithm:			-resets the r_flag
					-Reallocate memory and give it to a temporary variable
					-if the memory location changed, set the r_flag
					-add the symbol to the head at addc_offset + 1
******************************************************************************************************************************************/
Buffer * b_compact(Buffer * const pBD, char symbol) {
	char * tempArray = NULL; /*the new char array to be the buffer*/
	short tempCap = 0;	/*the new capacity*/
	if (pBD == NULL) {/*checks for null pointer*/
		return NULL;
	}
	if (pBD->addc_offset + 1 > SHRT_MAX || pBD->addc_offset < 0) { /*If the addc_offset will be out of bounds */
		return NULL;
	}
	tempCap = ((pBD->addc_offset + 1) * (sizeof(*pBD->cb_head))); /*Get the new capacity*/
	if (tempCap > SHRT_MAX || tempCap < 0) {/*make sure the new capacity is in bounds*/
		return NULL;
	}
	tempArray = (char *)realloc(pBD->cb_head, tempCap); /*realloc memory for the new buffer*/
	if (tempArray == NULL) {/*checks for null pointer*/
		return NULL;
	}
	if (pBD->cb_head != tempArray) {/*if the memory location of the buffer char array has changed*/
		pBD->flags |= SET_R_FLAG; /*set the r flag*/
	}
	pBD->capacity = tempCap;
	pBD->cb_head = tempArray;
	pBD->cb_head[pBD->addc_offset] = symbol;
	pBD->addc_offset++;
	return pBD;
}
/******************************************************************************************************************************************
Purpose:			Returns the value of r_flag
Author:				Daniel Dias / 040893258
History/Versions:	1.0
Called functions:	None
Parameters:			Buffer * const pBD	
Return value:		RT_FAIL_1, pBD->flags & CHECK_R_FLAG
Algorithm: -
******************************************************************************************************************************************/
char b_rflag(Buffer * const pBD) {
	if (!pBD) {
		return RT_FAIL_1;
	}
	return (pBD->flags & CHECK_R_FLAG);
}
/******************************************************************************************************************************************
Purpose:			Decrements the getc_offset and returns it
Author:				Daniel Dias / 040893258
History/Versions:	1.1
Called functions:	None
Parameters:			Buffer * const pBD	
Return value:		RT_FAIL_1, getc_offset
Algorithm: -
******************************************************************************************************************************************/
short b_retract(Buffer * const pBD) {

	return pBD ? pBD->getc_offset > ZERO ? --(pBD->getc_offset) : RT_FAIL_1 : RT_FAIL_1;
}
/******************************************************************************************************************************************
Purpose:			Assigns the value of markc_offset to getc_offset and returns it
Author:				Daniel Dias / 040893258
History/Versions:	1.0
Called functions:	None
Parameters:			Buffer * const pBD	
Return value:		RT_FAIL_1, getc_offset
Algorithm: -
******************************************************************************************************************************************/
short b_reset(Buffer * const pBD) {
	if (!pBD) {
		return RT_FAIL_1;
	}
	/*Set the value getc_offset to markc_offset*/
	pBD->getc_offset = pBD->markc_offset;

	return pBD->getc_offset;
}
/******************************************************************************************************************************************
Purpose:			Returns getc_offset
Author:				Daniel Dias / 040893258
History/Versions:	1.0
Called functions:	None
Parameters:			Buffer * const pBD	
Return value:		RT_FAIL_1, getc_offset
Algorithm: - 
******************************************************************************************************************************************/
short b_getcoffset(Buffer * const pBD) {
	if (!pBD) {
		return RT_FAIL_1;
	}

	return pBD->getc_offset;
}
/******************************************************************************************************************************************
Purpose:			Sets the getc and markc offsets to 0
Author:				Daniel Dias / 040893258
History/Versions:	1.0
Called functions:	None
Parameters:			Buffer * const pBD	
Return value:		RT_FAIL_1, RT_PASS_1
Algorithm: - 
******************************************************************************************************************************************/
int b_rewind(Buffer * const pBD) {
	if (!pBD) {
		return RT_FAIL_1;
	}

	pBD->getc_offset = RESET_OFFSET;
	pBD->markc_offset = RESET_OFFSET;

	return RT_PASS_1;
}
/*****************************************************************************************************************************************
Purpose:				
Author:					Daniel Dias / 040893258
History/Versions:		1.0
Called functions:		
Parameters:							
Return value:			
Algorithm:				
******************************************************************************************************************************************/
char * b_location(Buffer * const pBD) {
	if (!pBD) {
		return NULL;
	}

	return pBD->cb_head + pBD->markc_offset;
}