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
 * Symbol.mod
 *
 * Implementation of Symbol Type for EBNF Lexer.
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

IMPLEMENTATION MODULE Symbol;

(* Symbol Implementation *)

FROM Token IMPORT TokenT;
FROM String IMPORT StringT;


(* ---------------------------------------------------------------------------
 * Procedure Symbol.Init(sym, token, line, column, lex)
 * ---------------------------------------------------------------------------
 * Initialises the passed in symbol with the given parameters.
 * ---------------------------------------------------------------------------
 *)
PROCEDURE Init
  ( VAR sym : Symbol; t : TokenT; line, column : CARDINAL; lex : StringT );

BEGIN
  sym.token := t;
  sym.line := line;
  sym.column := column;
  sym.lexeme := lex
END Init;


(* ---------------------------------------------------------------------------
 * Function Symbol.token(sym)
 * ---------------------------------------------------------------------------
 * Returns the token of the passed in symbol.
 * ---------------------------------------------------------------------------
 *)
PROCEDURE token ( VAR (*CONST*) sym : Symbol ) : TokenT;

BEGIN
  RETURN sym.token
END token;


(* ---------------------------------------------------------------------------
 * Function Symbol.line(sym)
 * ---------------------------------------------------------------------------
 * Returns the line number of the passed in symbol.
 * ---------------------------------------------------------------------------
 *)
PROCEDURE line ( VAR (*CONST*) sym : Symbol ) : CARDINAL;

BEGIN
  RETURN sym.line
END line;


(* ---------------------------------------------------------------------------
 * Function Symbol.column(sym)
 * ---------------------------------------------------------------------------
 * Returns the column number of the passed in symbol.
 * ---------------------------------------------------------------------------
 *)
PROCEDURE column ( VAR (*CONST*) sym : Symbol ) : CARDINAL;

BEGIN
  RETURN sym.column
END column;


(* ---------------------------------------------------------------------------
 * Function Symbol.lexeme(sym)
 * ---------------------------------------------------------------------------
 * Returns the lexeme of the passed in symbol.
 * ---------------------------------------------------------------------------
 *)
PROCEDURE lexeme ( VAR (*CONST*) sym : Symbol ) : StringT;

BEGIN
  RETURN sym.lexeme
END lexeme;


END Symbol.
