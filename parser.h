/******************************************************************************************************************************************
File name:			parser.h
Compiler:			Microsoft Visual Studio 2017 Enterprise edition
Author:				Daniel Dias / 040893258
Course:				CST8152 - Compilers / Lab section 11
Assignment:			3
Date:				19 April 2019
Professor:			Svillen Ranev
Purpose:			Implements a parser header
******************************************************************************************************************************************/
#include "buffer.h"
#include "token.h"

extern Token malar_next_token(void); 
extern int line; 
extern Buffer *str_LTBL; 
extern char *kw_table[];
static Token lookahead;
int synerrno;

#define NO_ATTR -1
#define ELSE 0
#define FALSE 1
#define IF 2
#define PLATYPUS 3
#define READ 4
#define REPEAT 5
#define THEN 6
#define TRUE 7
#define WHILE 8
#define WRITE 9


void parser(void);
void match(int pr_token_code, int pr_token_attribute);
void syn_printe();
void syn_eh(int sync_token_code);
void gen_incode(char* argu);
void program(void);
void opt_statements(void);
void statements(void);
void statement(void);
void statements_prime(void);
void assignmentStatement(void);
void assignmentExpression(void);
void stringExpression(void);
void arithmeticExpression(void);
void iterationStatement(void);
void selectionStatement(void);
void iterationStatement(void);
void arithmeticExpression(void);
void stringExpression(void);
void pre_condition(void);
void inputStatement(void);
void variableList(void);
void variableList_prime(void);
void variableIdentifier(void);
void outputStatement(void);
void outputList(void);
void unaryArithmeticExpression(void);
void additiveArithmeticExpression(void);
void additiveArithmeticExpression_prime(void);
void multiplicativeArithmeticExpression(void);
void multiplicativeArithmeticExpression_prime(void);
void primaryArithmeticExpression(void);
void stringExpression(void);
void stringExpression_prime(void);
void primaryStringExpression();
void conditionalExpression(void);
void logical_ORExpression(void);
void logical_ORExpression_prime(void);
void logical_ANDExpression(void);
void logicalANDExpression_prime(void);
void relationalExpression(void);
void primary_a_relationalExpression_prime(void);
void primary_a_relationalExpression(void);
void primary_s_relationalExpression(void);
void primary_s_relationalExpression_prime(void);


#pragma once
