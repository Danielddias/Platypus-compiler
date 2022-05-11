/******************************************************************************************************************************************
File name:			parser.c
Compiler:			Microsoft Visual Studio 2017 Enterprise edition
Author:				Daniel Dias / 040893258
Course:				CST8152 - Compilers / Lab section 11
Assignment:			3
Date:				19 April 2019
Professor:			Svillen Ranev
Purpose:			Implements a parser

Function list:		 parser();match(int pr_token_code, int pr_token_attribute);syn_printe(); syn_eh(int sync_token_code);gen_incode(char* argu) program();
					 opt_statements(); statements(); statement(); statements_prime();assignmentStatement(); 
					 assignmentExpression(); stringExpression(); arithmeticExpression(); iterationStatement();selectionStatement() 
					 iterationStatement(); arithmeticExpression(); stringExpression(); pre_condition(); inputStatement(); variableList(); 
					 variableList_prime(); variableIdentifier(); outputStatement(); outputList(); unaryArithmeticExpression();
					 additiveArithmeticExpression(); additiveArithmeticExpression_prime(); multiplicativeArithmeticExpression(); 
					 multiplicativeArithmeticExpression_prime();primaryArithmeticExpression(); stringExpression(); stringExpression_prime(); 
					 primaryStringExpression(); conditionalExpression(); logical_ORExpression(); logical_ORExpression_prime(); logical_ANDExpression();
					 logicalANDExpression_prime(); relationalExpression(); primary_a_relationalExpression_prime(); primary_a_relationalExpression();
					 primary_s_relationalExpression(); primary_s_relationalExpression_prime();

******************************************************************************************************************************************/
#define _CRT_SECURE_NO_WARNINGS

#include "token.h"
#include "parser.h"
/*****************************************************************************************************************************************
Purpose:				First function to be started to begin the parsing
Author:					Daniel Dias / 040893258
History/Versions:		1.0
Called functions:		match()		
Parameters:				-
Return value:			-
 				
******************************************************************************************************************************************/
void parser(void) {

	lookahead = malar_next_token();
	program(); match(SEOF_T, NO_ATTR);
	gen_incode("PLATY: Source file parsed");

}
/*****************************************************************************************************************************************
Purpose:				Matches
Author:					Daniel Dias / 040893258
History/Versions:		1.0
Called functions:		syn_eh() malar_next_token()
Parameters:				- 
Return value:			-
 
******************************************************************************************************************************************/
void match(int pr_token_code, int pr_token_attribute) {
	if (lookahead.code != pr_token_code) {
		syn_eh(pr_token_code); return;

	} else if (lookahead.code == SEOF_T) 
		return;

	switch (pr_token_code) {
	case KW_T:
	case LOG_OP_T:
	case ART_OP_T:
	case REL_OP_T:
		if (pr_token_attribute != lookahead.attribute.get_int) {
			syn_eh(pr_token_code); 
			return;
		}
		break;
	}

	lookahead = malar_next_token();

	if (lookahead.code == ERR_T) {
		syn_printe();
		lookahead = malar_next_token();
		synerrno++;
		return;
	}
}
/*****************************************************************************************************************************************
Purpose:				Prints the syntax error
Author:					Svillen Ranev
History/Versions:		1.0
Called functions:		b_mark() b_location()
Parameters:				-
Return value:			-
 
******************************************************************************************************************************************/
void syn_printe() {
	Token t = lookahead;

	printf("PLATY: Syntax error:  Line:%3d\n", line);
	printf("*****  Token code:%3d Attribute: ", t.code);
	switch (t.code) {
	case  ERR_T: /* ERR_T     0   Error token */
		printf("%s\n", t.attribute.err_lex);
		break;
	case  SEOF_T: /*SEOF_T    1   Source end-of-file token */
		printf("SEOF_T\t\t%d\t\n", t.attribute.seof);
		break;
	case  AVID_T: /* AVID_T    2   Arithmetic Variable identifier token */
	case  SVID_T:/* SVID_T    3  String Variable identifier token */
		printf("%s\n", t.attribute.vid_lex);
		break;
	case  FPL_T: /* FPL_T     4  Floating point literal token */
		printf("%5.1f\n", t.attribute.flt_value);
		break;
	case INL_T: /* INL_T      5   Integer literal token */
		printf("%d\n", t.attribute.get_int);
		break;
	case STR_T:/* STR_T     6   String literal token */
		b_mark(str_LTBL, t.attribute.str_offset);
		printf("%s\n", b_location(str_LTBL));
		break;

	case SCC_OP_T: /* 7   String concatenation operator token */
		printf("NA\n");
		break;

	case  ASS_OP_T:/* ASS_OP_T  8   Assignment operator token */
		printf("NA\n");
		break;
	case  ART_OP_T:/* ART_OP_T  9   Arithmetic operator token */
		printf("%d\n", t.attribute.get_int);
		break;
	case  REL_OP_T: /*REL_OP_T  10   Relational operator token */
		printf("%d\n", t.attribute.get_int);
		break;
	case  LOG_OP_T:/*LOG_OP_T 11  Logical operator token */
		printf("%d\n", t.attribute.get_int);
		break;

	case  LPR_T: /*LPR_T    12  Left parenthesis token */
		printf("NA\n");
		break;
	case  RPR_T: /*RPR_T    13  Right parenthesis token */
		printf("NA\n");
		break;
	case LBR_T: /*    14   Left brace token */
		printf("NA\n");
		break;
	case RBR_T: /*    15  Right brace token */
		printf("NA\n");
		break;

	case KW_T: /*     16   Keyword token */
		printf("%s\n", kw_table[t.attribute.get_int]);
		break;

	case COM_T: /* 17   Comma token */
		printf("NA\n");
		break;
	case EOS_T: /*    18  End of statement *(semi - colon) */
		printf("NA\n");
		break;
	default:
		printf("PLATY: Scanner error: invalid token code: %d\n", t.code);
	}/*end switch*/
}/* end syn_printe()*/
/*****************************************************************************************************************************************
Purpose:				Implements panic mode recovery
Author:					Daniel Dias / 040893258
History/Versions:		1.0
Called functions:		syn_printe();
Parameters:				int sync_token_code
Return value:			-
 
******************************************************************************************************************************************/
void syn_eh(int sync_token_code) {
	syn_printe();
	synerrno++;

	while (sync_token_code != lookahead.code) {
		lookahead = malar_next_token();

		if (lookahead.code == SEOF_T) {
			if (sync_token_code != SEOF_T)
				exit(synerrno); 
			else 
				return;
		}
	}

	lookahead = malar_next_token();
	return;
}
/*****************************************************************************************************************************************
Purpose:				Prints the string parameter
Author:					Daniel Dias / 040893258
History/Versions:		1.0
Called functions:		-
Parameters:				char* argu
Return value:			-
 
******************************************************************************************************************************************/
void gen_incode(char* argu) {
	printf("%s\n", argu);
}
/*****************************************************************************************************************************************
Purpose:				<program> -> PLATYPUS {<opt_statements>}
Author:					Daniel Dias / 040893258
History/Versions:		1.0
Called functions:		match()
Parameters:				-
Return value:			-
 
******************************************************************************************************************************************/
void program(void) {
	match(KW_T, PLATYPUS); 
	match(LBR_T, NO_ATTR); 
	opt_statements();
	match(RBR_T, NO_ATTR);
	gen_incode("PLATY: Program parsed");
}/*****************************************************************************************************************************************
Purpose:				<opt_statements> - > <statements> | E
Author:					Daniel Dias / 040893258
History/Versions:		1.0
Called functions:
Parameters:				-
Return value:			-
 
******************************************************************************************************************************************/void opt_statements(void) {
	switch (lookahead.code) {
	case AVID_T:
	case SVID_T:statements(); break;
	case KW_T:
		if (lookahead.attribute.get_int != PLATYPUS
			&& lookahead.attribute.get_int != ELSE			
			&& lookahead.attribute.get_int != THEN
			&& lookahead.attribute.get_int != TRUE
			&& lookahead.attribute.get_int != REPEAT
			&& lookahead.attribute.get_int != FALSE) {
			statements(); 
			break;
		}
	default: 
		gen_incode("PLATY: Opt_statements parsed");
	}
}
/*****************************************************************************************************************************************
Purpose:				<statements> -> <statement><statements'>
Author:					Daniel Dias / 040893258
History/Versions:		1.0
Called functions:
Parameters:				-
Return value:			-
 
******************************************************************************************************************************************/
void statements(void) {
	statement();
	statements_prime();
}
/*****************************************************************************************************************************************
Purpose:			<statement> -> <assignment statement>|<selection statement>|<iteration statement>|<input statement>|<output statement>
					  FIRST (<statement>) = { AVID_T, SVID_T, KW_T(IF), KW_T(WHILE), KW_T(READ), KW_T(WRITE) }

Author:					Daniel Dias / 040893258
History/Versions:		1.0
Called functions:
Parameters:				-
Return value:			-
 
******************************************************************************************************************************************/
void statement(void) {
	switch (lookahead.code) {
	case AVID_T:
	case SVID_T:assignmentStatement();
		break;
	case KW_T:
		switch (lookahead.attribute.get_int) {
		case IF: selectionStatement(); break;
		case WHILE: iterationStatement(); break;
		case WRITE: outputStatement(); break;
		case READ: inputStatement(); break;
		}
		break;
	default: syn_printe(); 
		break;
	}
}
/*****************************************************************************************************************************************
Purpose:				<statements'> -> <statement><statements'> | E
Author:					Daniel Dias / 040893258
History/Versions:		1.0
Called functions:
Parameters:				-
Return value:			-
 
******************************************************************************************************************************************/
void statements_prime(void) {
	switch (lookahead.code) {
	case AVID_T:
	case SVID_T:statement(); statements_prime();
		break;
	case KW_T: if (lookahead.attribute.get_int != PLATYPUS && lookahead.attribute.get_int != ELSE && lookahead.attribute.get_int != REPEAT && lookahead.attribute.get_int != THEN && lookahead.attribute.get_int != TRUE && lookahead.attribute.get_int != FALSE) {
		statement(); 
		statements_prime();
		break;
	}
	break;
	}
}
/*****************************************************************************************************************************************
Purpose:				<assignment statement> -> <assignment expression>
Author:					Daniel Dias / 040893258
History/Versions:		1.0
Called functions:		match()
Parameters:				-
Return value:			-
 
******************************************************************************************************************************************/
void assignmentStatement(void) {
	assignmentExpression();
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Assignment statement parsed");
}
/*****************************************************************************************************************************************
Purpose:				<assignment expression> -> AVID = <arithmetic expression> | SVID = <string expression>
Author:					Daniel Dias / 040893258
History/Versions:		1.0
Called functions:		match()
Parameters:				-
Return value:			-
 
******************************************************************************************************************************************/
void assignmentExpression(void) {
	switch (lookahead.code) {
	case AVID_T:
		match(AVID_T, NO_ATTR);
		match(ASS_OP_T, EQ);
		arithmeticExpression();
		gen_incode("PLATY: Assignment expression (arithmetic) parsed");
		break;
	case SVID_T:
		match(SVID_T, NO_ATTR);
		match(ASS_OP_T, EQ);
		stringExpression();
		gen_incode("PLATY: Assignment expression (string) parsed");
		break;
	default: syn_printe(); 
		break;
	}
}
/*****************************************************************************************************************************************
Purpose:				<iteration statement> ->
					  WHILE <pre-condition> (<conditional expression>)
					  REPEAT { <statements>};
					  FIRST (<iteration statement>) = { KW_T(WHILE) }
Author:					Daniel Dias / 040893258
History/Versions:		1.0
Called functions:		match()
Parameters:				-
Return value:			-
 
******************************************************************************************************************************************/
void iterationStatement(void) {
	match(KW_T, WHILE);
	pre_condition();
	match(LPR_T, NO_ATTR);
	conditionalExpression();
	match(RPR_T, NO_ATTR);
	match(KW_T, REPEAT);
	match(LBR_T, NO_ATTR);
	statements();
	match(RBR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Iteration statement parsed");
}
/*****************************************************************************************************************************************
Purpose:				<input statement> ->
					  READ (<variable list>);
					  FIRST (<input statement>) = { KW_T(READ) }
Author:					Daniel Dias / 040893258
History/Versions:		1.0
Called functions:		match() variableList()
Parameters:				-
Return value:			-
 
******************************************************************************************************************************************/
void inputStatement(void) {
	match(KW_T, READ);
	match(LPR_T, NO_ATTR);
	variableList();
	match(RPR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Input statement parsed");
}
/*****************************************************************************************************************************************
Purpose:				<selection statement> ->
						 IF <pre-condition>  (<conditional expression>) THEN { <opt_statements> }
						 ELSE { <opt_statements> } ;
						 FIRST (<selection statement>) = { KW_T(IF) }
Author:					Daniel Dias / 040893258
History/Versions:		1.0
Called functions:		pre_condition(); match(); opt_statements();
Parameters:				-
Return value:			-
 
******************************************************************************************************************************************/
void selectionStatement(void) {
	match(KW_T, IF);
	pre_condition();
	match(LPR_T, NO_ATTR);
	conditionalExpression();
	match(RPR_T, NO_ATTR);
	match(KW_T, THEN);
	match(LBR_T, NO_ATTR);
	opt_statements();
	match(RBR_T, NO_ATTR);
	match(KW_T, ELSE);
	match(LBR_T, NO_ATTR);
	opt_statements();
	match(RBR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Selection statement parsed");
}
/*****************************************************************************************************************************************
Purpose:			<pre-condition> -> TRUE | FALSE
					  FIRST(<pre-condition>) = { KW_T(TRUE), KW_T(FALSE) }
Author:					Daniel Dias / 040893258
History/Versions:		1.0
Called functions:		syn_printe(); match()
Parameters:				-
Return value:			-
 
******************************************************************************************************************************************/
void pre_condition(void) {
	switch (lookahead.code) {
	case KW_T:
		switch (lookahead.attribute.get_int) {
		case TRUE: match(KW_T, TRUE);
			break;
		case FALSE: match(KW_T, FALSE);
			break;
		default: syn_printe(); 
			break;
		}
		break;
	default: syn_printe(); 
		break;
	}
}

/*****************************************************************************************************************************************
Purpose:				<variable list> -> <variable identifier> <variable list>
					  FIRST (<variable list>) = { AVID_T, SVID_T }
Author:					Daniel Dias / 040893258
History/Versions:		1.0
Called functions:			variableIdentifier(); variableList_prime();
Parameters:				-
Return value:			-
 
******************************************************************************************************************************************/
void variableList(void) {
	variableIdentifier();
	variableList_prime();
	gen_incode("PLATY: Variable list parsed");
}
/*****************************************************************************************************************************************
Purpose:				<variable list> -> ,<variable identifier> <variable list> |  E
					  FIRST (<variable list>) = { COM_T, E }
Author:					Daniel Dias / 040893258
History/Versions:		1.0
Called functions:		match(), variableIdentifier(), variableList_prime();
Parameters:				-
Return value:			-
 
******************************************************************************************************************************************/
void variableList_prime(void) {
	switch (lookahead.code) {
	case COM_T: match(COM_T, NO_ATTR);
		variableIdentifier(); variableList_prime();
		break;
	}
}
/*****************************************************************************************************************************************
Purpose:				<variable identifier> -> AVID | SVID
					  FIRST (<variable identifier>) = { AVID_T, SVID_T }
Author:					Daniel Dias / 040893258
History/Versions:		1.0
Called functions:		match(), syn_printe()
Parameters:				-
Return value:			-
 
******************************************************************************************************************************************/
void variableIdentifier(void) {
	switch (lookahead.code) {
	case AVID_T:
	case SVID_T: match(lookahead.code, NO_ATTR);
		break;
	default: syn_printe(); 
		break;
	}
}
/*****************************************************************************************************************************************
Purpose:				<output statement> -> WRITE (<output_list>);
					  FIRST(<output statement>) = { KW_T(WRITE) }
Author:					Daniel Dias / 040893258
History/Versions:		1.0
Called functions:		match(), outputList()
Parameters:				-
Return value:			-
 
******************************************************************************************************************************************/
void outputStatement(void) {
	match(KW_T, WRITE);
	match(LPR_T, NO_ATTR);
	outputList();
	match(RPR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Output statement parsed");
}
/*****************************************************************************************************************************************
Purpose:				<output_list> -> <opt_variable list> | STR_T;
					  FIRST (<output_list>) = {AVID_T, SVID_T, STR_T, E }
Author:					Daniel Dias / 040893258
History/Versions:		1.0
Called functions:
Parameters:				-
Return value:			-
 
******************************************************************************************************************************************/
void outputList(void) {
	switch (lookahead.code) {
	case AVID_T:
	case SVID_T: variableList();
		break;
	case STR_T: match(STR_T, NO_ATTR);
		gen_incode("PLATY: Output list (string literal) parsed");
		break;
	default:
		gen_incode("PLATY: Output list (empty) parsed");
		break;
	}
}
/*****************************************************************************************************************************************
Purpose:				<arithmetic expression> - > <unary arithmetic expression>|<additive arithmetic expression>
					  FIRST (<arithmetic expression>) = { -, +, AVID_T, FPL_T, INL_T, ( }
Author:					Daniel Dias / 040893258
History/Versions:		1.0
Called functions:
Parameters:				-
Return value:			-
 
******************************************************************************************************************************************/
void arithmeticExpression(void) {
	switch (lookahead.code) {
	case ART_OP_T:
		switch (lookahead.attribute.arr_op) {
		case PLUS:
		case MINUS:
			unaryArithmeticExpression();
			break;
		default: syn_printe(); 
				 break;
		}
		break;
	case AVID_T:
	case FPL_T:
	case INL_T:
	case LPR_T: 
		additiveArithmeticExpression();
		break;
	default: syn_printe(); 
			 break;
	}
	gen_incode("PLATY: Arithmetic expression parsed");
}
/*****************************************************************************************************************************************
Purpose:				<unary arithmetic expression> -> - <primary arithmetic expression> | + <primary arithmetic expression>
					  FIRST (<unary arithmetic expression>) = { -, +}
Author:					Daniel Dias / 040893258
History/Versions:		1.0
Called functions:		match()
Parameters:				-
Return value:			-
 
******************************************************************************************************************************************/
void unaryArithmeticExpression(void) {
	switch (lookahead.code) {
	case ART_OP_T:
		switch (lookahead.attribute.arr_op) {
		case MINUS:
			match(ART_OP_T, MINUS);
			primaryArithmeticExpression();
			break;
		case PLUS:
			match(ART_OP_T, PLUS);
			primaryArithmeticExpression();
			break;
		default: syn_printe(); 
			break;
		}
	}
	gen_incode("PLATY: Unary arithmetic expression parsed");
}
/*****************************************************************************************************************************************
Purpose:				<additive arithmetic expression> -> <multiplicative arithmetic expression><additive arithmetic expression'>
					    FIRST (<additive arithmetic expression>) = { AVID_T, FPL_T, INL_T, ( }
Author:					Daniel Dias / 040893258
History/Versions:		1.0
Called functions:		multiplicativeArithmeticExpression();
						additiveArithmeticExpression_prime();
Parameters:				-
Return value:			-
 
******************************************************************************************************************************************/
void additiveArithmeticExpression(void) {
	multiplicativeArithmeticExpression();
	additiveArithmeticExpression_prime();
}
/*****************************************************************************************************************************************
Purpose:				<additive arithmetic expression> ->
						+ <multiplicative arithmetic expression><additive arithmetic expression'>
					  | -  <multiplicative arithmetic expression><additive arithmetic expression'>
					  | E
					  FIRST (<additive arithmetic expression>) = { +, -, E}
Author:					Daniel Dias / 040893258
History/Versions:		1.0
Called functions:		multiplicativeArithmeticExpression();
						additiveArithmeticExpression_prime();
						match()
						gen_incode()
Parameters:				-
Return value:			-
 
******************************************************************************************************************************************/
void additiveArithmeticExpression_prime(void) {
	switch (lookahead.code) {
	case ART_OP_T:
		switch (lookahead.attribute.arr_op) {
		case PLUS:
		case MINUS:
			match(ART_OP_T, lookahead.attribute.arr_op);
			multiplicativeArithmeticExpression();
			additiveArithmeticExpression_prime();
			gen_incode("PLATY: Additive arithmetic expression parsed");
			break;
		default:
			break;
		}
	}
}
/*****************************************************************************************************************************************
Purpose:				<multiplicative arithmetic expression> -> <primary arithmetic expression><multiplicative arithmetic expression'>
					  FIRST (<multiplicative arithmetic expression>) = { AVID_T, FPL_T, INL_T, ( }
Author:					Daniel Dias / 040893258
History/Versions:		1.0
Called functions:	primaryArithmeticExpression();
					multiplicativeArithmeticExpression_prime();
Parameters:				-
Return value:			-
 
******************************************************************************************************************************************/
void multiplicativeArithmeticExpression(void) {
	primaryArithmeticExpression();
	multiplicativeArithmeticExpression_prime();
}
/*****************************************************************************************************************************************
Purpose:				<multiplicative arithmetic expression'> ->
						* <primary arithmetic expression><multiplicative arithmetic expression'>
					  | / <primary arithmetic expression><multiplicative arithmetic expression'>
					  | E
					  FIRST (<multiplicative arithmetic expression>) = { *, / , e}
Author:					Daniel Dias / 040893258
History/Versions:		1.0
Called functions:		match() primaryArithmeticExpression(); multiplicativeArithmeticExpression_prime(); gen_incode()
Parameters:				-
Return value:			-
 
******************************************************************************************************************************************/
void multiplicativeArithmeticExpression_prime(void) {
	switch (lookahead.code) {
	case ART_OP_T:
		switch (lookahead.attribute.arr_op) {
		case MULT:
		case DIV:
			match(ART_OP_T, lookahead.attribute.arr_op);
			primaryArithmeticExpression();
			multiplicativeArithmeticExpression_prime();
			gen_incode("PLATY: Multiplicative arithmetic expression parsed");
			break;
		default:
			break;
		}
	}
}
/*****************************************************************************************************************************************
Purpose:				<primary arithmetic expression> ->
						AVID_T
					  | FPL_T
					  | INL_T
					  | (<arithmetic expression>)
					  FIRST (<primary arithmetic expression > )= { AVID_T, FPL_T, INL_T, ( }
Author:					Daniel Dias / 040893258
History/Versions:		1.0
Called functions:		match() arithmeticExpression(); syn_printe();
Parameters:				-
Return value:			-
 
******************************************************************************************************************************************/
void primaryArithmeticExpression(void) {
	switch (lookahead.code) {
	case AVID_T:
	case FPL_T:
	case INL_T:
		match(lookahead.code, NO_ATTR);
		break;
	case LPR_T:
		match(LPR_T, NO_ATTR);
		arithmeticExpression();
		match(RPR_T, NO_ATTR);
		break;
	default: syn_printe(); 
		break;
	}
	gen_incode("PLATY: Primary arithmetic expression parsed");
}
/*****************************************************************************************************************************************
Purpose:				<string expression> -> <primary string expression> <string expression'>
					  FIRST ( <string expression> ) = { SVID_T, STR_T }
Author:					Daniel Dias / 040893258
History/Versions:		1.0
Called functions:			primaryStringExpression();
						stringExpression_prime();
Parameters:				-
Return value:			-
 
******************************************************************************************************************************************/
void stringExpression(void) {
	primaryStringExpression();
	stringExpression_prime();
	gen_incode("PLATY: String expression parsed");
}
/*****************************************************************************************************************************************
Purpose:				<string expression'> -> <<  <primary string expression> <string expression'> | e
					  FIRST ( <string expression'> ) = { <<, e}
Author:					Daniel Dias / 040893258
History/Versions:		1.0
Called functions:		match()
						primaryStringExpression();
						stringExpression_prime();
Parameters:				-
Return value:			-
 
******************************************************************************************************************************************/
void stringExpression_prime(void) {
	switch (lookahead.code) {
	case SCC_OP_T:
		match(SCC_OP_T, NO_ATTR);
		primaryStringExpression();
		stringExpression_prime();
		break;
	default:
		break;
	}
}
/*****************************************************************************************************************************************
Purpose:				<primary string expression> ->
								   SVID_T
								 | STR_T
					  FIRST ( <primary string expression>) = { SVID_T, STR_T }
Author:					Daniel Dias / 040893258
History/Versions:		1.0
Called functions:		match() syn_printe();
Parameters:				-
Return value:			-
 
******************************************************************************************************************************************/
void primaryStringExpression(void) {
	switch (lookahead.code) {
	case SVID_T:
	case STR_T: match(lookahead.code, NO_ATTR);
		break;
	default: syn_printe(); 
		break;
	}
	gen_incode("PLATY: Primary string expression parsed");
}
/*****************************************************************************************************************************************
Purpose:				<conditional expression> -> <logical OR  expression>
					  FIRST (<conditional expression>) = { AVID_T, FPL_T, INL_T, SVID_T, STR_T }
Author:					Daniel Dias / 040893258
History/Versions:		1.0
Called functions:	logical_ORExpression();
					gen_incode()
Parameters:				-
Return value:			-
 
******************************************************************************************************************************************/
void conditionalExpression(void) {
	logical_ORExpression();
	gen_incode("PLATY: Conditional expression parsed");
}
/*****************************************************************************************************************************************
Purpose:				<logical OR expression> -> <logical AND expression> <logical OR expression'>
Author:					Daniel Dias / 040893258
History/Versions:		1.0
Called functions:		logical_ANDExpression();
						logical_ORExpression_prime();
Parameters:				-
Return value:			-
 
******************************************************************************************************************************************/
void logical_ORExpression(void) {
	logical_ANDExpression();
	logical_ORExpression_prime();
}
/*****************************************************************************************************************************************
Purpose:				<logical OR expression'> -> .OR.  <logical AND expression><logical OR expression'> | e
					  FIRST (<logical OR expression>) = { .OR. , e }
Author:					Daniel Dias / 040893258
History/Versions:		1.0
Called functions:		match()			logical_ANDExpression();
			logical_ORExpression_prime();
Parameters:				-
Return value:			-
 
******************************************************************************************************************************************/
void logical_ORExpression_prime(void) {
	switch (lookahead.code) {
	case LOG_OP_T:
		switch (lookahead.attribute.log_op) {
		case OR:
			match(LOG_OP_T, OR);
			logical_ANDExpression();
			logical_ORExpression_prime();
			gen_incode("PLATY: Logical OR expression parsed");
			break;
		}
		break;
	}
}
/*****************************************************************************************************************************************
Purpose:				<logical AND expression> -> <relational expression> <logical AND expression>
					  FIRST (<logical AND expression>) = { AVID_T, FPL_T, INL_T, SVID_T, STR_T }
Author:					Daniel Dias / 040893258
History/Versions:		1.0
Called functions:			relationalExpression();
	logicalANDExpression_prime();
Parameters:				-
Return value:			-
 
******************************************************************************************************************************************/
void logical_ANDExpression(void) {
	relationalExpression();
	logicalANDExpression_prime();
}
/*****************************************************************************************************************************************
Purpose:				<logical AND expression> -> .AND.  <relational expression><logical AND expression> | e
					  FIRST (<logical AND expression>) = {.AND. , e }
Author:					Daniel Dias / 040893258
History/Versions:		1.0
Called functions:		match()			relationalExpression();
			logicalANDExpression_prime();
Parameters:				-
Return value:			-
 
******************************************************************************************************************************************/
void logicalANDExpression_prime(void) {
	switch (lookahead.code) {
	case LOG_OP_T:
		switch (lookahead.attribute.log_op) {
		case AND:
			match(LOG_OP_T, AND);
			relationalExpression();
			logicalANDExpression_prime();
			gen_incode("PLATY: Logical AND expression parsed");
			break;
		}
		break;
	}
}
/*****************************************************************************************************************************************
Purpose:				<relational expression> ->
							  <primary a_relational expression> <primary  a_relational expression>
							| <primary s_relational  expression> <primary s_relational expression>
					  FIRST(<relational expression>) = { AVID_T, FPL_T, INL_T, SVID_T, STR_T }
Author:					Daniel Dias / 040893258
History/Versions:		1.0
Called functions:		primary_a_relationalExpression();
						primary_a_relationalExpression_prime();
						primary_s_relationalExpression();
						primary_s_relationalExpression_prime();
Parameters:				-
Return value:			-
 
******************************************************************************************************************************************/
void relationalExpression(void) {
	switch (lookahead.code) {
	case AVID_T:
	case FPL_T:
	case INL_T:
		primary_a_relationalExpression();
		primary_a_relationalExpression_prime();
		break;
	case SVID_T:
	case STR_T:
		primary_s_relationalExpression();
		primary_s_relationalExpression_prime();
		break;
	default: syn_printe();
		break;
	}
	gen_incode("PLATY: Relational expression parsed");
}
/*****************************************************************************************************************************************
Purpose:				<primary s_relational expression> -> <primary string expression>
					  FIRST(<primary s_relational expression >)= { SVID_T, STR_T }
Author:					Daniel Dias / 040893258
History/Versions:		1.0
Called functions:		primaryStringExpression();
						gen_incode()
Parameters:				-
Return value:			-
 
******************************************************************************************************************************************/
void primary_s_relationalExpression(void) {
	primaryStringExpression();
	gen_incode("PLATY: Primary s_relational expression parsed");
}
/*****************************************************************************************************************************************
Purpose:				<primary s_relational expression'> ->
									  == <primary s_relational expression>
									| <> <primary s_relational expression>
									| > <primary s_relational expression>
									| < <primary s_relational expression>
					  FIRST(<primary s_relational expression'>) = { ==, <>, >, <  }
Author:					Daniel Dias / 040893258
History/Versions:		1.0
Called functions:		match() primary_s_relationalExpression();  syn_printe();
Parameters:				-
Return value:			-
 
******************************************************************************************************************************************/
void primary_s_relationalExpression_prime(void)
{
	switch (lookahead.code)
	{
	case REL_OP_T:
		switch (lookahead.attribute.rel_op)
		{
		case NE:
		case EQ:
		case GT:
		case LT:
			match(REL_OP_T, lookahead.attribute.rel_op);
			primary_s_relationalExpression();
			break;
		default:
			syn_printe();
			break;
		}
		break;
	default:
		syn_printe(); break;
	}
}
/*****************************************************************************************************************************************
Purpose:				<primary a_relational expression> ->
							 == <primary a_relational expression>
						   | <> <primary a_relational expression>
						   | > <primary a_relational expression>
						   | < <primary a_relational expression>
					  FIRST(<primary a_relational expression>) = { ==, <>, >, < }
Author:					Daniel Dias / 040893258
History/Versions:		1.0
Called functions:		match() primary_a_relationalExpression(); syn_printe();
Parameters:				-
Return value:			-
 
******************************************************************************************************************************************/
void primary_a_relationalExpression_prime(void)
{
	switch (lookahead.code)
	{
	case REL_OP_T:
		switch (lookahead.attribute.rel_op)
		{
		case NE:
		case EQ:
		case GT:
		case LT:
			match(REL_OP_T, lookahead.attribute.rel_op);
			primary_a_relationalExpression();
			break;
		default:
			syn_printe();
			break;
		}
		break;
	default:
		syn_printe(); break;
	}
}
/*****************************************************************************************************************************************
Purpose:				<primary a_relational expression> ->
									AVID_T
								  | FPL_T
								  | INL_T
					  FIRST(<primary a_relational expression>)= {  AVID_T, FPL_T, INL_T}
Author:					Daniel Dias / 040893258
History/Versions:		1.0
Called functions:		match() syn_printe(); gen_incode()
Parameters:				-
Return value:			-
 
******************************************************************************************************************************************/
void primary_a_relationalExpression(void) {
	switch (lookahead.code) {
	case AVID_T:
	case FPL_T:
	case INL_T: match(lookahead.code, NO_ATTR);
		break;
	default:
		syn_printe();
		break;
	}
	gen_incode("PLATY: Primary a_relational expression parsed");
}