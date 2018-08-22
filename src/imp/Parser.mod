(*!m2pim*)

(* *************************************************************************
 * Grammar Tool for LL(1) Grammars
 *
 * Copyright (C) 2018 Benjamin Kowarsch
 *
 * Synopsis
 *
 * GLL is a tool to verify and visualise LL(1) grammars.  It parses EBNF
 * specifications of grammars, calculates FIRST and FOLLOW sets, reports
 * any LL(1) violations and generates (railroad) syntax diagrams.
 *
 * File
 *
 * Parser.mod
 *
 * Implementation of EBNF parser module.
 *
 * License
 *
 * GLL is free software: you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License Version 2 (LGPL2)
 * as published by the Free Software Foundation.
 *
 * GLL is distributed in the hope that it will be useful,  but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  Refer to the license for details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with GLL.  If not, please visit <http://www.gnu.org/licenses/>.
 * ************************************************************************* *)

IMPLEMENTATION MODULE Parser;

(* Parser for EBNF grammar specifications *)

IMPORT
  AST, AstNodeType, AstQueue, NonTerminals,
  Lexer, LexQueue, Symbol, Token, TokenSet, Filename, String;

FROM AST IMPORT AstT; (* alias for AST.AST *)
FROM AstNodeType IMPORT AstNodeTypeT; (* alias for AstNodeType.AstNodeType *)
FROM AstQueue IMPORT AstQueueT; (* alias for AstQueue.AstQueue *)
FROM Lexer IMPORT LexerT; (* alias for Lexer.Lexer *)
FROM LexQueue IMPORT LexQueueT; (* alias for LexQueue.LexQueue *)
FROM Symbol IMPORT SymbolT; (* alias for Symbol.Symbol *)
FROM Token IMPORT TokenT; (* alias for Token.Token *)
FROM TokenSet IMPORT TokenSetT; (* alias for TokenSet.TokenSet *)
FROM String IMPORT StringT; (* alias for String.String *)

FROM NonTerminals IMPORT FIRST, FOLLOW, inFIRST;


(* Parse Procedure Type *)

TYPE ParseProc = PROCEDURE ( VAR AstT ) : SymbolT;


(* Parser context *)

VAR
  lexer : LexerT;
  statistics : Statistics;


(* Operations *)

(* ---------------------------------------------------------------------------
 * Public procedure ParseEBNF (source, ast, status)
 * ---------------------------------------------------------------------------
 * Parses rule specification and builds its AST.  Passes back AST on success,
 * or NIL on failure.  Sets statistics and passes back status.
 * ---------------------------------------------------------------------------
 *)
PROCEDURE ParseEBNF ( source : StringT; VAR ast : AstT; VAR status : Status );

VAR
  lexerStatus : Lexer.Status;
  lookahead : SymbolT;
  ast : AstT;

BEGIN
  
  (* init statistics *)
  
  statistics.lexicalWarnings := 0; statistics.lexicalErrors := 0;
  statistics.syntaxWarnings := 0; statistics.syntaxErrors := 0;
  
  (* TO DO : verify filename in source *)
  
  lexer := Lexer.new(source, lexerStatus);
  
  (* TO DO : verify lexer status *)
  
  (* pase EBNF specification *)
  
  lookahead := specification(ast);
  
  (* TO DO: verify lookahead *)
  (* TO DO: get lexical stats from lexer *)
  
  Lexer.Release(lexer)

END ParseEBNF;


(* ---------------------------------------------------------------------------
 * Procedure Parser.GetStats (stats, status)
 * ---------------------------------------------------------------------------
 * Passes back  statistics of a  prior call  to procedure ParseEBNF.  If there
 * was no prior call to ParseEBNF,  Failure is passed in status  and  the data
 * passed in stats is invalid and meaningless.
 * ---------------------------------------------------------------------------
 *)
PROCEDURE GetStats ( stats : Statistics; VAR status : Status );

BEGIN
  (* TO DO *)
END GetStats;


(* Private Operations *)

(* ************************************************************************ *
 * Syntax Analysis                                                          *
 * ************************************************************************ *)

(* --------------------------------------------------------------------------
 * Specific Parsing Functions
 * ------------------------------------------------------------------------ *)

(* ---------------------------------------------------------------------------
 * Private function specification(lookahead)
 * ---------------------------------------------------------------------------
 * Parses rule  specification,  constructs its AST node,  passes the new look-
 * ahead symbol back in out-parameter lookahead and returns the AST node.
 * Returns NIL on failure.
 *
 * specification :=
 *   GRAMMAR ident ';' ( definition ';' )+ ENDG ident '.'
 *   ;
 * ---------------------------------------------------------------------------
 *)
PROCEDURE specification ( VAR lookahead : SymbolT ) : AstT;

VAR
  ident1, ident2 : StringT;
  
BEGIN
  
  (* GRAMMAR *)
  lookahead := Lexer.consumeSym(lexer);
  
  (* ident *)
  IF matchToken(Token.Ident) THEN
    ident1 = lookahead.lexeme;
    lookahead := Lexer.consumeSym(lexer)
    
  ELSE (* resync *)
    lookahead := skipToMatchTokenOrSet(Token.Semicolon, FIRST(Definition))
  END; (* IF *)
  
  (* ';' *)
  IF matchToken(Token.Semicolon) THEN
    lookahead := Lexer.consumeSym(lexer)
  ELSE (* resync *)
    lookahead := skipToMatchSetOrSet(First(Definition), FOLLOW(Definition))
  END; (* IF *)
  
  (* (definition ';')+ *)
  REPEAT
    (* definition *)
    defNode := definition(lookahead);
    
    (* ';' *)
    IF matchToken(Token.Semicolon) THEN
      lookahead := Lexer.consumeSym(lexer)
    ELSE (* resync *)
      lookahead := skipToMatchSetOrSet(First(Definition), FOLLOW(Definition))
    END (* IF *)
    
  UNTIL NOT inFIRST(Definition, lookahead.token);
  
  (* ENDG *)
  IF lookahead.token = Token.Endg THEN
    lookahead := Lexer.consumeSym(lexer)
  ELSE (* resync *)
    lookahead := skipToMatchTokenOrToken(Token.Ident, Token.Period)
  END; (* IF *)
  
  (* ident *)
  IF matchToken(Token.Ident) THEN
    ident2 = lookahead.lexeme;
    lookahead := Lexer.consumeSym(lexer)
    
    (* TO DO : verify ident1 = ident2 *)
    
  ELSE (* resync *)
    lookahead := skipToMatchTokenOrToken(Token.Period, Token.EOF)
  END; (* IF *)
  
  (* TO DO : build AST node *)
  
  RETURN ast
END specification;


(* ---------------------------------------------------------------------------
 * Private function definition(lookahead)
 * ---------------------------------------------------------------------------
 * Parses  rule  definition,  constructs its AST node,  passes  the  new look-
 * ahead symbol back in out-parameter lookahead and returns the AST node.
 * Returns NIL on failure.
 *
 * definition :=
 *   RESERVED reswordList |
 *   ALIAS aliasDef |
 *   nonTerminalDef |
 *   terminalDef |
 *   fragmentDef
 *   ;
 * ---------------------------------------------------------------------------
 *)
PROCEDURE definition ( VAR lookahead : SymbolT ) : AstT;

BEGIN
  
  (* TO DO *)
  
  RETURN ast
END definition;


(* ---------------------------------------------------------------------------
 * Private function reswordList(lookahead)
 * ---------------------------------------------------------------------------
 * Parses rule  reswordList,  constructs its AST node,  passes  the  new look-
 * ahead symbol back in out-parameter lookahead and returns the AST node.
 * Returns NIL on failure.
 *
 * reswordList :=
 *   reswordDef ( ',' reswordDef )*
 *   ;
 * ---------------------------------------------------------------------------
 *)
PROCEDURE reswordList ( VAR lookahead : SymbolT ) : AstT;

BEGIN
  
  (* TO DO *)
  
  RETURN ast
END reswordList;


(* ---------------------------------------------------------------------------
 * Private function reswordDef(lookahead)
 * ---------------------------------------------------------------------------
 * Parses  rule  reswordDef,  constructs its AST node,  passes  the  new look-
 * ahead symbol back in out-parameter lookahead and returns the AST node.
 * Returns NIL on failure.
 *
 * reswordDef :=
 *   ReswordIdent ( '=' String )?
 *   ;
 * ---------------------------------------------------------------------------
 *)
PROCEDURE reswordDef ( VAR lookahead : SymbolT ) : AstT;

BEGIN
  
  (* TO DO *)
  
  RETURN ast
END reswordDef;


(* ---------------------------------------------------------------------------
 * Private function aliasDef(lookahead)
 * ---------------------------------------------------------------------------
 * Parses  rule  aliasDef,  constructs its AST node,  passes the new lookahead
 * symbol back in out-parameter lookahead and returns the AST node.
 * Returns NIL on failure.
 *
 * aliasDef :=
 *   nonTermAliasDef | termAliasDef
 *   ;
 * ---------------------------------------------------------------------------
 *)
PROCEDURE aliasDef ( VAR lookahead : SymbolT ) : AstT;

BEGIN
  
  (* TO DO *)
  
  RETURN ast
END aliasDef;


(* ---------------------------------------------------------------------------
 * Private function nonTermAliasDef(lookahead)
 * ---------------------------------------------------------------------------
 * Parses  rule  nonTermAliasDef,  constructs  its AST node,  passes  the  new
 * lookahead symbol back in out-parameter lookahead and returns the AST node.
 * Returns NIL on failure.
 *
 * nonTermAliasDef :=
 *   nonTermAliasList '=' ident
 *   ;
 * ---------------------------------------------------------------------------
 *)
PROCEDURE nonTermAliasDef ( VAR lookahead : SymbolT ) : AstT;

BEGIN
  
  (* TO DO *)
  
  RETURN ast
END nonTermAliasDef;


(* ---------------------------------------------------------------------------
 * Private function nonTermAliasList(lookahead)
 * ---------------------------------------------------------------------------
 * Parses rule  nonTermAliasList,  constructs  its AST node,  passes  the  new
 * lookahead symbol back in out-parameter lookahead and returns the AST node.
 * Returns NIL on failure.
 *
 * nonTermAliasList :=
 *   nonTermAliasList '=' ident
 *   ;
 * ---------------------------------------------------------------------------
 *)
PROCEDURE nonTermAliasList ( VAR lookahead : SymbolT ) : AstT;

BEGIN
  
  (* TO DO *)
  
  RETURN ast
END nonTermAliasList;


(* ---------------------------------------------------------------------------
 * Private function termAliasDef(lookahead)
 * ---------------------------------------------------------------------------
 * Parses  rule  termAliasDef,  constructs its AST node,  passes the new look-
 * ahead symbol back in out-parameter lookahead and returns the AST node.
 * Returns NIL on failure.
 *
 * termAliasDef :=
 *   terminalAliasList '=' ( TerminalIdent | literal )
 *   ;
 * ---------------------------------------------------------------------------
 *)
PROCEDURE termAliasDef ( VAR lookahead : SymbolT ) : AstT;

BEGIN
  
  (* TO DO *)
  
  RETURN ast
END termAliasDef;


(* ---------------------------------------------------------------------------
 * Private function terminalAliasList(lookahead)
 * ---------------------------------------------------------------------------
 * Parses  rule  terminalAliasList,  constructs its AST node,  passes  the new
 * lookahead symbol back in out-parameter lookahead and returns the AST node.
 * Returns NIL on failure.
 *
 * terminalAliasList :=
 *   TerminalIdent ( ',' TerminalIdent )*
 *   ;
 * ---------------------------------------------------------------------------
 *)
PROCEDURE terminalAliasList ( VAR lookahead : SymbolT ) : AstT;

BEGIN
  
  (* TO DO *)
  
  RETURN ast
END terminalAliasList;


(* ---------------------------------------------------------------------------
 * Private function literal(lookahead)
 * ---------------------------------------------------------------------------
 * Parses  rule  literal,  constructs its AST node,  passes  the new lookahead
 * symbol back in out-parameter lookahead and returns the AST node.
 * Returns NIL on failure.
 *
 * literal :=
 *   String | QuotedLowerLetter | QuotedUpperLetter |
 *   QuotedDigit | QuotedNonAlphaNum | CharCode | EOF
 *   ;
 * ---------------------------------------------------------------------------
 *)
PROCEDURE literal ( VAR lookahead : SymbolT ) : AstT;

BEGIN
  
  (* TO DO *)
  
  RETURN ast
END literal;


(* ---------------------------------------------------------------------------
 * Private function nonTerminalDef(lookahead)
 * ---------------------------------------------------------------------------
 * Parses rule nonTerminalDef,  constructs its AST node,  passes the new look-
 * ahead symbol back in out-parameter lookahead and returns the AST node.
 * Returns NIL on failure.
 *
 * nonTerminalDef :=
 *   NonTerminalIdent ':=' expression+
 *   ;
 * ---------------------------------------------------------------------------
 *)
PROCEDURE nonTerminalDef ( VAR lookahead : SymbolT ) : AstT;

BEGIN
  
  (* TO DO *)
  
  RETURN ast
END nonTerminalDef;


(* ---------------------------------------------------------------------------
 * Private function expression(lookahead)
 * ---------------------------------------------------------------------------
 * Parses rule expression,  constructs its AST node,  passes the new lookahead
 * symbol back in out-parameter lookahead and returns the AST node.
 * Returns NIL on failure.
 *
 * expression :=
 *   term ( '|' term )*
 *   ;
 * ---------------------------------------------------------------------------
 *)
PROCEDURE expression ( VAR lookahead : SymbolT ) : AstT;

BEGIN
  
  (* TO DO *)
  
  RETURN ast
END expression;


(* ---------------------------------------------------------------------------
 * Private function term(lookahead)
 * ---------------------------------------------------------------------------
 * Parses  rule  term,  constructs its  AST node,  passes  the  new  lookahead
 * symbol back in out-parameter lookahead and returns the AST node.
 * Returns NIL on failure.
 *
 * term :=
 *   factor ( '*' | '+' | '?' )? | literalOrRange
 *   ;
 * ---------------------------------------------------------------------------
 *)
PROCEDURE term ( VAR lookahead : SymbolT ) : AstT;

BEGIN
  
  (* TO DO *)
  
  RETURN ast
END term;


(* ---------------------------------------------------------------------------
 * Private function factor(lookahead)
 * ---------------------------------------------------------------------------
 * Parses  rule  factor,  constructs its AST node,  passes  the new  lookahead
 * symbol back in out-parameter lookahead and returns the AST node.
 * Returns NIL on failure.
 *
 * factor :=
 *   NonTerminalIdent | TerminalIdent | ReswordIdent | '(' expression+ ')'
 *   ;
 * ---------------------------------------------------------------------------
 *)
PROCEDURE factor ( VAR lookahead : SymbolT ) : AstT;

BEGIN
  
  (* TO DO *)
  
  RETURN ast
END factor;


(* ---------------------------------------------------------------------------
 * Private function terminal(lookahead)
 * ---------------------------------------------------------------------------
 * Parses  rule  terminal,  constructs its AST node,  passes the new lookahead
 * symbol back in out-parameter lookahead and returns the AST node.
 * Returns NIL on failure.
 *
 * terminal :=
 *   TerminalIdent ':=' terminalExpression+
 *   ;
 * ---------------------------------------------------------------------------
 *)
PROCEDURE terminal ( VAR lookahead : SymbolT ) : AstT;

BEGIN
  
  (* TO DO *)
  
  RETURN ast
END terminal;


(* ---------------------------------------------------------------------------
 * Private function terminalExpression(lookahead)
 * ---------------------------------------------------------------------------
 * Parses  rule  terminalExpression,  constructs its AST node,  passes the new
 * lookahead symbol back in out-parameter lookahead and returns the AST node.
 * Returns NIL on failure.
 *
 * terminalExpression :=
 *   terminalTerm ( '|' terminalTerm )*
 *   ;
 * ---------------------------------------------------------------------------
 *)
PROCEDURE terminalExpression ( VAR lookahead : SymbolT ) : AstT;

BEGIN
  
  (* TO DO *)
  
  RETURN ast
END terminalExpression;


(* ---------------------------------------------------------------------------
 * Private function terminalTerm(lookahead)
 * ---------------------------------------------------------------------------
 * Parses  rule  terminalTerm,  constructs its AST node,  passes the new look-
 * ahead symbol back in out-parameter lookahead and returns the AST node.
 * Returns NIL on failure.
 *
 * terminalTerm :=
 *   terminalFactor ( '*' | '+' | '?' )? | literalOrRange
 *   ;
 * ---------------------------------------------------------------------------
 *)
PROCEDURE terminalTerm ( VAR lookahead : SymbolT ) : AstT;

BEGIN
  
  (* TO DO *)
  
  RETURN ast
END terminalTerm;


(* ---------------------------------------------------------------------------
 * Private function terminalFactor(lookahead)
 * ---------------------------------------------------------------------------
 * Parses rule terminalFactor,  constructs its AST node,  passes the new look-
 * ahead symbol back in out-parameter lookahead and returns the AST node.
 * Returns NIL on failure.
 *
 * terminalFactor :=
 *   TerminalIdent | '(' terminalExpression+ ')'
 *   ;
 * ---------------------------------------------------------------------------
 *)
PROCEDURE terminalFactor ( VAR lookahead : SymbolT ) : AstT;

BEGIN
  
  (* TO DO *)
  
  RETURN ast
END terminalFactor;


(* ---------------------------------------------------------------------------
 * Private function fragmentDef(lookahead)
 * ---------------------------------------------------------------------------
 * Parses rule  fragmentDef,  constructs its AST node,  passes  the new  look-
 * ahead symbol back in out-parameter lookahead and returns the AST node.
 * Returns NIL on failure.
 *
 * fragmentDef :=
 *   '.' terminalDef
 *   ;
 * ---------------------------------------------------------------------------
 *)
PROCEDURE fragmentDef ( VAR lookahead : SymbolT ) : AstT;

BEGIN
  
  (* TO DO *)
  
  RETURN ast
END fragmentDef;


(* ---------------------------------------------------------------------------
 * Private function literalOrRange(lookahead)
 * ---------------------------------------------------------------------------
 * Parses rule literalOrRange,  constructs its AST node,  passes the new look-
 * ahead symbol back in out-parameter lookahead and returns the AST node.
 * Returns NIL on failure.
 *
 * literalOrRange :=
 *   String | quotedLowerCharOrRange | quotedUpperCharOrRange |
 *   quotedDigitOrRange | charCodeOrRange | QuotedAlphaNum | EOF
 *   ;
 * ---------------------------------------------------------------------------
 *)
PROCEDURE literalOrRange ( VAR lookahead : SymbolT ) : AstT;

BEGIN
  
  (* TO DO *)
  
  RETURN ast
END literalOrRange;


(* ---------------------------------------------------------------------------
 * Private function quotedLowerLetterOrRange(lookahead)
 * ---------------------------------------------------------------------------
 * Parses rule quotedLowerLetterOrRange,  constructs its AST node,  passes the
 * new lookahead symbol back in out-parameter lookahead and returns the AST
 * node.  Returns NIL on failure.
 *
 * quotedLowerLetterOrRange :=
 *   QuotedLowerLetter ( '..' QuotedLowerLetter )?
 *   ;
 * ---------------------------------------------------------------------------
 *)
PROCEDURE quotedLowerLetterOrRange ( VAR lookahead : SymbolT ) : AstT;

BEGIN
  
  (* TO DO *)
  
  RETURN ast
END quotedLowerLetterOrRange;


(* ---------------------------------------------------------------------------
 * Private function quotedUpperLetterOrRange(lookahead)
 * ---------------------------------------------------------------------------
 * Parses rule quotedUpperLetterOrRange,  constructs its AST node,  passes the
 * new lookahead symbol back in out-parameter lookahead and returns the AST
 * node.  Returns NIL on failure.
 *
 * quotedUpperLetterOrRange :=
 *   QuotedUpperLetter ( '..' QuotedUpperLetter )?
 *   ;
 * ---------------------------------------------------------------------------
 *)
PROCEDURE quotedUpperLetterOrRange ( VAR lookahead : SymbolT ) : AstT;

BEGIN
  
  (* TO DO *)
  
  RETURN ast
END quotedUpperLetterOrRange;


(* ---------------------------------------------------------------------------
 * Private function quotedDigitOrRange(lookahead)
 * ---------------------------------------------------------------------------
 * Parses  rule  quotedDigitOrRange,  constructs its AST node,  passes the new
 * lookahead symbol back in out-parameter lookahead and returns the AST node.
 * Returns NIL on failure.
 *
 * quotedDigitOrRange :=
 *   QuotedDigit ( '..' QuotedDigit )?
 *   ;
 * ---------------------------------------------------------------------------
 *)
PROCEDURE quotedDigitOrRange ( VAR lookahead : SymbolT ) : AstT;

BEGIN
  
  (* TO DO *)
  
  RETURN ast
END quotedDigitOrRange;


(* ---------------------------------------------------------------------------
 * Private function charCodeOrRange(lookahead)
 * ---------------------------------------------------------------------------
 * Parses  rule  charCodeOrRange,  constructs  its AST node,  passes  the  new
 * lookahead symbol back in out-parameter lookahead and returns the AST node.
 * Returns NIL on failure.
 *
 * charCodeOrRange :=
 *   CharCode ( '..' CharCode )?

 *   ;
 * ---------------------------------------------------------------------------
 *)
PROCEDURE charCodeOrRange ( VAR lookahead : SymbolT ) : AstT;

BEGIN
  
  (* TO DO *)
  
  RETURN ast
END charCodeOrRange;


END Parser.
