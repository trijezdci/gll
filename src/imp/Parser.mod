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

(* ---------------------------------------------------------------------------
 * Specific Parsing Functions
 * ---------------------------------------------------------------------------
 * Parsing functions represent  non-terminal  production rules in the grammar.
 * Each function is named after the rule that it represents and the  EBNF  for
 * the rule is given in the comment header above the function's declaration.
 *
 * Each function parses the tokenised input stream according to its respective
 * production rule.  In the process  it constructs an  AST node  encoding  the
 * parsed input,  passes the new lookahead symbol back in  parameter lookahead
 * and returns the AST node.
 *
 * If any lexical or syntax errors are detected  that prevent the construction
 * of an AST node then NIL is returned.
 * ---------------------------------------------------------------------------
 *)

(* ---------------------------------------------------------------------------
 * Private function specification(lookahead)
 * ---------------------------------------------------------------------------
 * specification :=
 *   GRAMMAR ident ';'
 *   ( RESERVED reswordList )?
 *   ( definition ';' )+
 *   ENDG ident '.'
 *   ;
 * ---------------------------------------------------------------------------
 *)
PROCEDURE specification ( VAR lookahead : SymbolT ) : AstT;

VAR
  astNode : AstT;
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

  (* ( RESERVED reswordList )? *)
  IF lookahead.token = Token.Reserved THEN
    (* RESERVED *)
    lookahead := Lexer.consumeSym(lexer);

    (* reswordList *)
    IF matchSet(lookahead.token, FIRST(ReswordList)) THEN
      astNode := reswordList(lookahead)
    ELSE (* resync *)
      lookahead := skipToMatchSet(FIRST(Definition))
    END (* IF *)
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

    TO DO : (* verify ident1 = ident2 *)

  ELSE (* resync *)
    lookahead := skipToMatchTokenOrToken(Token.Period, Token.EOF)
  END; (* IF *)

  (* TO DO : build AST node *)

  RETURN ast
END specification;


(* ---------------------------------------------------------------------------
 * Private function definition(lookahead)
 * ---------------------------------------------------------------------------
 * definition :=
 *   ALIAS aliasDef |
 *   nonTerminalDef |
 *   terminalDef |
 *   fragmentDef
 *   ;
 * ---------------------------------------------------------------------------
 *)
PROCEDURE definition ( VAR lookahead : SymbolT ) : AstT;

VAR
  astNode : AstT;

BEGIN

  (* ALIAS aliasDef | *)
  IF lookahead.token = Token.Alias THEN

    (* ALIAS *)
    lookahead := Lexer.consumeSym(lexer);

    (* aliasDef *)
    IF matchSet(FIRST(), lookahead.token) THEN
      astNode := aliasDef(lookahead)
    ELSE (* resync *)
      lookahead := skipToMatchTokenOrSet(Token.Endg, FIRST(definition))
    END; (* IF *)

  (* nonTerminalDef | *)
  ELSIF inFIRST(NonTerminalDef, lookahead.token) THEN
    astNode := nonTerminalDef(lookahead)

  (* terminalDef | *)
  ELSIF inFIRST(TerminalDef, lookahead.token) THEN
    astNode := terminalDef(lookahead)

  (* fragmentDef *)
  ELSIF inFIRST(FragmentDef, lookahead.token) THEN
    astNode := fragmentDef(lookahead)

  ELSE (* unexpected symbol *)

    TO DO : (* report error *)

    astNode := NIL
  END; (* IF *)

  RETURN astNode
END definition;


(* ---------------------------------------------------------------------------
 * Private function reswordList(lookahead)
 * ---------------------------------------------------------------------------
 * reswordList :=
 *   reswordDef ( ',' reswordDef )*
 *   ;
 * ---------------------------------------------------------------------------
 *)
PROCEDURE reswordList ( VAR lookahead : SymbolT ) : AstT;

VAR
  astNode : AstT;

BEGIN

  (* reswordDef *)
  astNode := reswordDef(lookahead);

  (* ( ',' reswordDef )* *)
  WHILE lookahead.token = Token.Comma DO

    (* ',' *)
    lookahead := Lexer.consumeSym(lexer);

    (* reswordDef *)
    IF matchSet(FIRST(ReswordDef), lookahead.token) THEN
      astNode := reswordDef(lookahead)
    ELSE (* resync *)
      lookahead := skipToMatchTokenOrSetOrSet
        (Token.Comma, FIRST(ReswordDef), FOLLOW(ReswordDef))
    END (* IF *)
  END; (* WHILE *)

  RETURN astNode
END reswordList;


(* ---------------------------------------------------------------------------
 * Private function reswordDef(lookahead)
 * ---------------------------------------------------------------------------
 * reswordDef :=
 *   ReswordIdent ( '=' String )?
 *   ;
 * ---------------------------------------------------------------------------
 *)
PROCEDURE reswordDef ( VAR lookahead : SymbolT ) : AstT;

VAR
  astNode : AstT;
  resword, value : StringT;

BEGIN

  (* ReswordIdent *)
  resword := lookahead.lexeme;

  value := NIL;

  (* ( '=' String )? *)
  IF matchToken(Token.Equal, lookahead.token) THEN

    (* '=' *)
    lookahead := Lexer.consumeSym(lexer);

    (* String *)
    IF matchToken(Token.String, lookahead.token) THEN
      value := lookahead.lexeme;
      lookahead := Lexer.consumeSym(lexer)
    ELSE (* resync *)
      lookahead := skipToMatchSet(FOLLOW(ReswordDef))
    END (* IF *)
  END; (* IF *)

  (* build AST node *)
  astNode := AST.NewNode(AstNodeType.Resword, resword, value);

  RETURN astNode
END reswordDef;


(* ---------------------------------------------------------------------------
 * Private function aliasDef(lookahead)
 * ---------------------------------------------------------------------------
 * aliasDef :=
 *   nonTermAliasDef | termAliasDef
 *   ;
 * ---------------------------------------------------------------------------
 *)
PROCEDURE aliasDef ( VAR lookahead : SymbolT ) : AstT;

VAR
  astNode : AstT;

BEGIN

  (* nonTermAliasDef | termAliasDef *)
  IF inFIRST(NonTermAliasDef, lookahead.token) THEN

    (* nonTermAliasDef *)
    astNode := nonTermAliasDef(lookahead)

  ELSIF inFIRST(termAliasDef, lookahead.token) THEN

    (* termAliasDef *)
    astNode := termAliasDef(lookahead)

  ELSE (* unexpected symbol *)

    astNode := NIL;

    (* To DO : report error *)

  END; (* IF *)

  RETURN astNode
END aliasDef;


(* ---------------------------------------------------------------------------
 * Private function nonTermAliasDef(lookahead)
 * ---------------------------------------------------------------------------
 * nonTermAliasDef :=
 *   NonTermAlias ( ',' NonTermAlias )* '=' NonTermAliasValue
 *   ;
 *
 * alias NonTermAlias, NonTermAliasValue = NonTerminalIdent;
 * ---------------------------------------------------------------------------
 *)
PROCEDURE nonTermAliasDef ( VAR lookahead : SymbolT ) : AstT;

VAR
  astNode : AstT;
  value : StringT;
  aliasList : LexQueueT;

BEGIN

  LexQueue.New(aliasList);

  (* NonTermAlias *)
  IF matchToken(Token.NonTerminalIdent, lookahead.token) THEN
    AstQueue.Enqueue(aliasList, lookahead.lexeme);
    lookahead := Lexer.consumeSym(lexer)
  ELSE (* resync *)
    lookahead := skipToMatchTokenOrSet(Token.Comma, FOLLOW(NonTermAliasDef))
  END; (* IF *)

  (* ( ',' NonTermAlias )* *)
  WHILE lookahead.token = Token.Comma THEN
    (* ',' *)
    lookahead := Lexer.consumeSym(lookahead);

    (* NonTermAlias *)
    IF matchToken(Token.NonTerminalIdent, lookahead.token) THEN
      AstQueue.Enqueue(aliasList, lookahead.lexeme)
    ELSE (* resync *)
      lookahead := skipToMatchTokenOrSet(Token.Comma, Token.Equal)
    END (* IF *)
  END; (* WHILE *)

  (* '=' *)
  IF matchToken(Token.Equal, lookahead.token) THEN
    lookahead := Lexer.consumeSym(lookahead)
  ELSE (* resync *)
    lookahead := skipToMatchTokenOrSet
      (Token.NonTerminalIdent, FOLLOW(NonTermAliasDef))
  END; (* IF *)

  value := NIL;

  (* NonTerminalIdent *)
  IF matchToken(Token.NonTerminalIdent, lookahead.token) THEN
    value := lookahead.lexeme;
    lookahead := Lexer.consumeSym(lexer)
  ELSE (* resync *)
    lookahead := skipToMatchSet(FOLLOW(NonTermAliasDef))
  END; (* IF *)

  (* build ast node *)
  astNode := AST.NewListNode(AstNodeType.AliasList, aliasList, value);

  RETURN astNode
END nonTermAliasDef;


(* ---------------------------------------------------------------------------
 * Private function termAliasDef(lookahead)
 * ---------------------------------------------------------------------------
 * termAliasDef :=
 *   TerminalAlias ( ',' TerminalAlias )* '=' termAliasValue
 *
 * alias TerminalAlias = TerminalIdent;
 * ---------------------------------------------------------------------------
 *)
PROCEDURE termAliasDef ( VAR lookahead : SymbolT ) : AstT;

VAR
  astNode, value : AstT;
  aliasList : LexQueueT;

BEGIN

  LexQueue.New(aliasList);

  (* TerminalAlias *)
  IF matchToken(Token.TerminalIdent, lookahead.token) THEN
    AstQueue.Enqueue(aliasList, lookahead.lexeme);
    lookahead := Lexer.consumeSym(lexer)
  ELSE (* resync *)
    lookahead := skipToMatchTokenOrSet(Token.Comma, FOLLOW(TermAliasDef))
  END; (* IF *)

  (* ( ',' TerminalAlias )* *)
  WHILE lookahead.token = Token.Comma THEN
    (* ',' *)
    lookahead := Lexer.consumeSym(lookahead);

    (* TerminalAlias *)
    IF matchToken(Token.TerminalIdent, lookahead.token) THEN
      AstQueue.Enqueue(aliasList, lookahead.lexeme)
    ELSE (* resync *)
      lookahead := skipToMatchTokenOrSet(Token.Comma, Token.Equal)
    END (* IF *)
  END; (* WHILE *)

  (* '=' *)
  IF matchToken(Token.Equal, lookahead.token) THEN
    lookahead := Lexer.consumeSym(lookahead)
  ELSE (* resync *)
    lookahead := skipToMatchTokenOrSet
      (Token.NonTerminalIdent, FOLLOW(TermAliasDef))
  END; (* IF *)

  (* termAliasValue *)
  IF matchSet(FIRST(termAliasValue), lookahead.token) THEN
    value := termAliasValue(lookahead)
  ELSE (* resync *)
    lookahead := skipToMatchSet(FOLLOW(TermAliasDef));
    value := NIL
  END; (* IF *)

  (* build ast node *)
  astNode := AST.NewListNode(AstNodeType.AliasList, aliasList, value);

  RETURN astNode
END termAliasDef;


(* ---------------------------------------------------------------------------
 * Private function termAliasValue(lookahead)
 * ---------------------------------------------------------------------------
 * termAliasValue :=
 *   TerminalIdent | literal
 *   ;
 * ---------------------------------------------------------------------------
 *)
PROCEDURE termAliasValue ( VAR lookahead : SymbolT ) : AstT;

VAR
  astNode : AstT;
  value : StringT;

BEGIN

  (* TerminalIdent | *)
  IF lookahead.token = Token.TerminalIdent THEN
    value := lookahead.lexeme;
    lookahead := Lexer.consumeSym(lexer);
    astNode := AST.NewTerminalNode(AstNodeType.Ident, value)

  (* literal *)
  ELSE
    astNode := literal(lookahead)
  END; (* IF *)

  RETURN astNode
END termAliasValue;


(* ---------------------------------------------------------------------------
 * Private function literal(lookahead)
 * ---------------------------------------------------------------------------
 * literal :=
 *   String | QuotedLowerLetter | QuotedUpperLetter |
 *   QuotedDigit | QuotedNonAlphaNum | CharCode
 *   ;
 * ---------------------------------------------------------------------------
 *)
PROCEDURE literal ( VAR lookahead : SymbolT ) : AstT;

VAR
  astNode : AstT;
  value : StringT;

BEGIN

  value := lookahead.lexeme;

  (* String | QuotedLowerLetter | QuotedUpperLetter |
     QuotedDigit | QuotedNonAlphaNum | *)
  IF lookahead.token # Token.CharCode THEN
    astNode := AST.NewTerminalNode(AstNodeType.Quoted, value)

  (* CharCode *)
  ELSE
    astNode := AST.NewTerminalNode(AstNodeType.CharCode, value)
  END; (* IF *)

  lookahead := Lexer.consumeSym(lexer);

  RETURN astNode
END literal;


(* ---------------------------------------------------------------------------
 * Private function nonTerminalDef(lookahead)
 * ---------------------------------------------------------------------------
 * nonTerminalDef :=
 *   NonTerminalIdent ':=' expression
 *   ;
 * ---------------------------------------------------------------------------
 *)
PROCEDURE nonTerminalDef ( VAR lookahead : SymbolT ) : AstT;

VAR
  ident : StringT;
  astNode, expr : AstT;

BEGIN

  (* NonTerminalIdent *)
  IF matchToken() THEN
    ident := lookahead.lexeme;
    lookahead := Lexer.consumeSym(lexer)
  ELSE (* resync *)
    lookahead := skipToMatchTokenOrSet(Token.Assign, FIRST(Expression));
    ident := NIL
  END; (* IF *)

  (* ':=' *)
  IF matchToken(Token.Assign, lookahead.token) THEN
    lookahead := Lexer.consumeSym(lexer)
  ELSE (* resync *)
    lookahead := skipToMatchSetOrSet(FIRST(Expression), FOLLOW(Expression))
  END; (* IF *)

  (* expression *)
  IF matchSet(FIRST(expression), lookahead.token) THEN
    expr := expression(lookahead)
  ELSE (* resync *)
    lookahead := skipToMatchSet(FOLLOW(Expression));
    expr := NIL
  END; (* IF *)

  (* build AST node *)
  astNode := AST.NewNode(AstNodeType.NonTerminal, ident, expr);

  RETURN astNode
END nonTerminalDef;


(* ---------------------------------------------------------------------------
 * Private function expression(lookahead)
 * ---------------------------------------------------------------------------
 * expression :=
 *   term ( '|' term )*
 *   ;
 * ---------------------------------------------------------------------------
 *)
PROCEDURE expression ( VAR lookahead : SymbolT ) : AstT;

VAR
  astNode, termNode : AstT;
  termList : AstQueueT;

BEGIN

  AstQueue.New(termList);

  (* term *)
  termNode := term(lookahead);
  AstQueue.Enqueue(termList, termNode);

  (* ( '|' term )* *)
  WHILE lookahead.token = Token.VerticalBar) DO
    (* '|' *)
    lookahead := Lexer.consumeSym(lexer);

    (* term *)
    IF matchSet(FIRST(Term), lookahead.token) THEN
      termNode := term(lookahead);
      AstQueue.Enqueue(termList, termNode)
    ELSE (* resync *)
      lookahead := skipToMatchTokenOrSet
        (Token.VerticalBar, FOLLOW(Term))
    END (* IF *)
  END; (* WHILE *)

  (* build AST node *)
  astNode := AST.NewListNode(NodeType.Expression, termList);

  RETURN astNode
END expression;


(* ---------------------------------------------------------------------------
 * Private function term(lookahead)
 * ---------------------------------------------------------------------------
 * term :=
 *   simpleTerm+
 *   ;
 * ---------------------------------------------------------------------------
 *)
PROCEDURE term ( VAR lookahead : SymbolT ) : AstT;

VAR
  astNode, simpleTermNode : AstT;
  simpleTermList : AstQueueT;

BEGIN

  AstQueue.New(simpleTermList);

  (* simpleTerm+ *)
  WHILE inFIRST(Term, lookahead.token) DO
    simpleTermNode := simpleTerm(lookahead);
    AstQueue.Enqueue(simpleTermList, simpleTermNode)
  END; (* WHILE *)

  (* build AST node *)
  astNode := AST.NewListNode(NodeType.Term, simpleTermList);

  RETURN astNode
END term;


(* ---------------------------------------------------------------------------
 * Private function simpleTerm(lookahead)
 * ---------------------------------------------------------------------------
 * simpleTerm :=
 *   factor ( '*' | '+' | '?' )? | literalOrRange
 *   ;
 * ---------------------------------------------------------------------------
 *)
PROCEDURE simpleTerm ( VAR lookahead : SymbolT ) : AstT;

VAR
  astNode, factorNode : AstT;

BEGIN

  (* factor ( '*' | '+' | '?' )? | *)
  IF inFIRST(Factor, lookahead.token) THEN
    factorNode := factor(lookahead);

    (* ( '*' | '+' | '?' )? *)
    CASE lookahead.token OF
    (* '*' | *)
    | Token.Asterisk :
      astNode := AST.NewNode(AstNodeType.OptRepetition, factor)

    (* '+' | *)
    | Token.Plus :
      astNode := AST.NewNode(AstNodeType.Repetition, factor)

    (* '?' *)
    | Token.QMark :
      astNode := AST.NewNode(AstNodeType.Option, factor)
    END; (* CASE *)

  (* literalOrRange *)
  ELSE
    astNode := literalOrRange(lookahead)
  END; (* IF *)

  RETURN astNode
END simpleTerm;


(* ---------------------------------------------------------------------------
 * Private function factor(lookahead)
 * ---------------------------------------------------------------------------
 * factor :=
 *   NonTerminalIdent | TerminalIdent | ReswordIdent | '(' expression ')'
 *   ;
 * ---------------------------------------------------------------------------
 *)
PROCEDURE factor ( VAR lookahead : SymbolT ) : AstT;

VAR
  astNode : AstT;

BEGIN

  (* NonTerminalIdent | TerminalIdent | ReswordIdent | '(' expression ')' *)
  CASE lookahead.token OF
  (* NonTerminalIdent | *)
  | Token.NonTerminalIdent :
    astNode :=
      AST.NewTerminalNode(AstNodeType.NonTerminalIdent, lookahead.lexeme);
    lookahead := Lexer.consumeSym(lexer)

  (* TerminalIdent |*)
  | Token.TerminalIdent :
    astNode :=
      AST.NewTerminalNode(AstNodeType.TerminalIdent, lookahead.lexeme);
    lookahead := Lexer.consumeSym(lexer)

  (* ReswordIdent | *)
  | Token.ReswordIdent :
    astNode :=
      AST.NewTerminalNode(AstNodeType.ReswordIdent, lookahead.lexeme);
    lookahead := Lexer.consumeSym(lexer)

  (* '(' expression ')' *)
  ELSE
    (* '(' *)
    lookahead := Lexer.consumeSym(lexer);

    (* expression *)
    astNode := expression(lookahead)

    (* ')' *)
    IF matchToken(Token.RightParen) THEN
      lookahead := Lexer.consumeSym(lexer)
    ELSE
      lookahead := skipToMatchSet(FOLLOW(Factor))
    END (* IF *)
  END; (* CASE *)

  RETURN astNode
END factor;


(* ---------------------------------------------------------------------------
 * Private function terminalDef(lookahead)
 * ---------------------------------------------------------------------------
 * terminalDef :=
 *   TerminalIdent ':=' ( terminalExpression | '<platform dependent>' )
 *   ;
 * ---------------------------------------------------------------------------
 *)
PROCEDURE terminalDef ( VAR lookahead : SymbolT ) : AstT;

VAR
  astNode : AstT;

BEGIN

  (* TO DO *)

  RETURN astNode
END terminalDef;


(* ---------------------------------------------------------------------------
 * Private function terminalExpression(lookahead)
 * ---------------------------------------------------------------------------
 * terminalExpression :=
 *   terminalTerm ( '|' terminalTerm )*
 *   ;
 * ---------------------------------------------------------------------------
 *)
PROCEDURE terminalExpression ( VAR lookahead : SymbolT ) : AstT;

VAR
  astNode, termNode : AstT;
  termList : AstQueueT;

BEGIN

  AstQueue.New(termList);

  (* terminalTerm *)
  termNode := term(lookahead);
  AstQueue.Enqueue(termList, termNode);

  (* ( '|' terminalTerm )* *)
  WHILE lookahead.token = Token.VerticalBar) DO
    (* '|' *)
    lookahead := Lexer.consumeSym(lexer);

    (* term *)
    IF matchSet(FIRST(TerminalTerm), lookahead.token) THEN
      termNode := term(lookahead);
      AstQueue.Enqueue(termList, termNode)
    ELSE (* resync *)
      lookahead := skipToMatchTokenOrSet
        (Token.VerticalBar, FOLLOW(TerminalTerm))
    END (* IF *)
  END; (* WHILE *)

  (* build AST node *)
  astNode := AST.NewListNode(NodeType.TermExpr, termList);

  RETURN astNode
END terminalExpression;


(* ---------------------------------------------------------------------------
 * Private function terminaTerm(lookahead)
 * ---------------------------------------------------------------------------
 * terminaTerm :=
 *   simpleTerminalTerm+
 *   ;
 * ---------------------------------------------------------------------------
 *)
PROCEDURE terminaTerm ( VAR lookahead : SymbolT ) : AstT;

VAR
  astNode, simpleTermNode : AstT;
  simpleTermList : AstQueueT;

BEGIN

  AstQueue.New(simpleTermList);

  (* simpleTerminalTerm+ *)
  WHILE inFIRST(TerminalSimpleTerm, lookahead.token) DO
    simpleTermNode := simpleTerm(lookahead);
    AstQueue.Enqueue(simpleTermList, simpleTermNode)
  END; (* WHILE *)

  (* build AST node *)
  astNode := AST.NewListNode(NodeType.TerminalTerm, simpleTermList);

  RETURN astNode
END terminaTerm;


(* ---------------------------------------------------------------------------
 * Private function simpleTerminalTerm(lookahead)
 * ---------------------------------------------------------------------------
 * simpleTerminalTerm :=
 *   terminalFactor ( '*' | '+' | '?' )? | literalOrRange
 *   ;
 * ---------------------------------------------------------------------------
 *)
PROCEDURE simpleTerminalTerm ( VAR lookahead : SymbolT ) : AstT;

VAR
  astNode, factorNode : AstT;

BEGIN

  (* terminalFactor ( '*' | '+' | '?' )? | *)
  IF inFIRST(TerminalFactor, lookahead.token) THEN
    factorNode := factor(lookahead);

    (* ( '*' | '+' | '?' )? *)
    CASE lookahead.token OF
    (* '*' | *)
    | Token.Asterisk :
      astNode := AST.NewNode(AstNodeType.OptRepetition, factor)

    (* '+' | *)
    | Token.Plus :
      astNode := AST.NewNode(AstNodeType.Repetition, factor)

    (* '?' *)
    | Token.QMark :
      astNode := AST.NewNode(AstNodeType.Option, factor)
    END; (* CASE *)

  (* literalOrRange *)
  ELSE
    astNode := literalOrRange(lookahead)
  END; (* IF *)

  RETURN astNode
END simpleTerminalTerm;


(* ---------------------------------------------------------------------------
 * Private function terminalFactor(lookahead)
 * ---------------------------------------------------------------------------
 * terminalFactor :=
 *   TerminalIdent | '(' terminalExpression ')'
 *   ;
 * ---------------------------------------------------------------------------
 *)
PROCEDURE terminalFactor ( VAR lookahead : SymbolT ) : AstT;

VAR
  astNode : AstT;

BEGIN

  (* TerminalIdent | '(' expression ')' *)
  IF lookahead.token = Token.TerminalIdent :
    astNode :=
      AST.NewTerminalNode(AstNodeType.TerminalIdent, lookahead.lexeme);
    lookahead := Lexer.consumeSym(lexer)

  (* '(' terminalExpression ')' *)
  ELSE
    (* '(' *)
    lookahead := Lexer.consumeSym(lexer);

    (* terminalExpression *)
    astNode := terminalExpression(lookahead)

    (* ')' *)
    IF matchToken(Token.RightParen) THEN
      lookahead := Lexer.consumeSym(lexer)
    ELSE
      lookahead := skipToMatchSet(FOLLOW(Factor))
    END (* IF *)
  END; (* CASE *)

  RETURN astNode
END terminalFactor;


(* ---------------------------------------------------------------------------
 * Private function fragmentDef(lookahead)
 * ---------------------------------------------------------------------------
 * fragmentDef :=
 *   '.' terminalDef
 *   ;
 * ---------------------------------------------------------------------------
 *)
PROCEDURE fragmentDef ( VAR lookahead : SymbolT ) : AstT;

VAR
  astNode, termDef : AstT;

BEGIN

  (* '.' *)
  lookahead := Lexer.consumeSym(lexer);

  (* terminalDef *)
  IF matchSet(FIRST(TerminalDef), lookahead.token) THEN
    termDef := terminalDef(lookahead);
    astNode := AST.NewNode(AstNodeType.FragmentDef, termDef)
  ELSE (* resync *)
    lookahead := skipToMatchSet(FOLLOW(FragmentDef));
    astNode := NIL
  END; (* IF *)

  RETURN astNode
END fragmentDef;


(* ---------------------------------------------------------------------------
 * Private function nonSemanticDef(lookahead)
 * ---------------------------------------------------------------------------
 * nonSemanticDef :=
 *   '*' terminalDef
 *   ;
 * ---------------------------------------------------------------------------
 *)
PROCEDURE nonSemanticDef ( VAR lookahead : SymbolT ) : AstT;

VAR
  astNode, termDef : AstT;

BEGIN

  (* '*' *)
  lookahead := Lexer.consumeSym(lexer);

  (* terminalDef *)
  IF matchSet(FIRST(TerminalDef), lookahead.token) THEN
    termDef := terminalDef(lookahead);
    astNode := AST.NewNode(AstNodeType.NonSemanticDef, termDef)
  ELSE (* resync *)
    lookahead := skipToMatchSet(FOLLOW(nonSemanticDef));
    astNode := NIL
  END; (* IF *)

  RETURN astNode
END nonSemanticDef;


(* ---------------------------------------------------------------------------
 * Private function literalOrRange(lookahead)
 * ---------------------------------------------------------------------------
 * literalOrRange :=
 *   String | quotedLowerLetterOrRange | quotedUpperLetterOrRange |
 *   quotedDigitOrRange | charCodeOrRange | QuotedNonAlphaNum
 *   ;
 * ---------------------------------------------------------------------------
 *)
PROCEDURE literalOrRange ( VAR lookahead : SymbolT ) : AstT;

VAR
  astNode : AstT;

BEGIN

  CASE lookahead.token OF
    (* String | *)
  | Token.String :
    astNode := AST.NewTerminalNode(AstNodeType.String, lookahead.lexeme);
    lookahead := Lexer.consumeSym(lexer)

  (* quotedLowerLetterOrRange | *)
  | Token.QuotedLowerLetter :
    astNode := quotedLowerLetterOrRange(lookahead)

  (* quotedUpperLetterOrRange | *)
  | Token.QuotedUpperLetter :
    astNode := quotedUpperLetterOrRange(lookahead)

  (* quotedDigitOrRange | *)
  | Token.QuotedDigit :
    astNode := quotedDigitOrRange(lookahead)

  (* charCodeOrRange | *)
  | Token.CharCode :
    astNode := charCodeOrRange(lookahead)

  (* QuotedNonAlphaNum *)
  ELSE
    astNode := AST.NewTerminalNode(AstNodeType.String, lookahead.lexeme);
    lookahead := Lexer.consumeSym(lexer)

  END; (* CASE *)

  RETURN astNode
END literalOrRange;


(* ---------------------------------------------------------------------------
 * Private function quotedLowerLetterOrRange(lookahead)
 * ---------------------------------------------------------------------------
 * quotedLowerLetterOrRange :=
 *   QuotedLowerLetter ( '..' QuotedLowerLetter )?
 *   ;
 * ---------------------------------------------------------------------------
 *)
PROCEDURE quotedLowerLetterOrRange ( VAR lookahead : SymbolT ) : AstT;

VAR
  astNode : AstT;
  left, right : StringT;

BEGIN

  (* QuotedLowerLetter *)
  left := lookahead.lexeme;
  lookahead := Lexer.consumeSym(lexer);

  (* ( '..' QuotedLowerLetter )? *)
  IF lookahead.token = Token.Range THEN
    lookahead := Lexer.consumeSym(lexer);

    (* QuotedLowerLetter *)
    IF matchToken(Token.QuotedLowerLetter, lookahead.token) THEN
      right := lookahead.lexeme;
      lookahead := Lexer.consumeSym(lexer)

    ELSE (* resync *)
      right := NIL;
      lookahead := skipToMatchSet(FOLLOW(QuotedLowerLetterOrRange))
    END (* IF *)

  ELSE
    right := NIL
  END;

  (* build AST node *)
  astNode := AST.NewNode(AstNodeType.Range, left, right);

  RETURN astNode
END quotedLowerLetterOrRange;


(* ---------------------------------------------------------------------------
 * Private function quotedUpperLetterOrRange(lookahead)
 * ---------------------------------------------------------------------------
 * quotedUpperLetterOrRange :=
 *   QuotedUpperLetter ( '..' QuotedUpperLetter )?
 *   ;
 * ---------------------------------------------------------------------------
 *)
PROCEDURE quotedUpperLetterOrRange ( VAR lookahead : SymbolT ) : AstT;

VAR
  astNode : AstT;
  left, right : StringT;

BEGIN

  (* QuotedUpperLetter *)
  left := lookahead.lexeme;
  lookahead := Lexer.consumeSym(lexer);

  (* ( '..' QuotedUpperLetter )? *)
  IF lookahead.token = Token.Range THEN
    lookahead := Lexer.consumeSym(lexer);

    (* QuotedUpperLetter *)
    IF matchToken(Token.QuotedUpperLetter, lookahead.token) THEN
      right := lookahead.lexeme;
      lookahead := Lexer.consumeSym(lexer)

    ELSE (* resync *)
      right := NIL;
      lookahead := skipToMatchSet(FOLLOW(QuotedUpperLetterOrRange))
    END (* IF *)

  ELSE
    right := NIL
  END;

  (* build AST node *)
  astNode := AST.NewNode(AstNodeType.Range, left, right);

  RETURN astNode
END quotedUpperLetterOrRange;


(* ---------------------------------------------------------------------------
 * Private function quotedDigitOrRange(lookahead)
 * ---------------------------------------------------------------------------
 * quotedDigitOrRange :=
 *   QuotedDigit ( '..' QuotedDigit )?
 *   ;
 * ---------------------------------------------------------------------------
 *)
PROCEDURE quotedDigitOrRange ( VAR lookahead : SymbolT ) : AstT;

VAR
  astNode : AstT;
  left, right : StringT;

BEGIN

  (* QuotedDigit *)
  left := lookahead.lexeme;
  lookahead := Lexer.consumeSym(lexer);

  (* ( '..' QuotedDigit )? *)
  IF lookahead.token = Token.Range THEN
    lookahead := Lexer.consumeSym(lexer);

    (* QuotedDigit *)
    IF matchToken(Token.QuotedDigit, lookahead.token) THEN
      right := lookahead.lexeme;
      lookahead := Lexer.consumeSym(lexer)

    ELSE (* resync *)
      right := NIL;
      lookahead := skipToMatchSet(FOLLOW(QuotedDigitOrRange))
    END (* IF *)

  ELSE
    right := NIL
  END;

  (* build AST node *)
  astNode := AST.NewNode(AstNodeType.Range, left, right);

  RETURN astNode
END quotedDigitOrRange;


(* ---------------------------------------------------------------------------
 * Private function charCodeOrRange(lookahead)
 * ---------------------------------------------------------------------------
 * charCodeOrRange :=
 *   CharCode ( '..' CharCode )?

 *   ;
 * ---------------------------------------------------------------------------
 *)
PROCEDURE charCodeOrRange ( VAR lookahead : SymbolT ) : AstT;

VAR
  astNode : AstT;
  left, right : StringT;

BEGIN

  (* CharCode *)
  left := lookahead.lexeme;
  lookahead := Lexer.consumeSym(lexer);

  (* ( '..' CharCode )? *)
  IF lookahead.token = Token.Range THEN
    lookahead := Lexer.consumeSym(lexer);

    (* CharCode *)
    IF matchToken(Token.CharCode, lookahead.token) THEN
      right := lookahead.lexeme;
      lookahead := Lexer.consumeSym(lexer)

    ELSE (* resync *)
      right := NIL;
      lookahead := skipToMatchSet(FOLLOW(CharCodeOrRange))
    END (* IF *)

  ELSE
    right := NIL
  END;

  (* build AST node *)
  astNode := AST.NewNode(AstNodeType.Range, left, right);

  RETURN astNode
END charCodeOrRange;


END Parser.
