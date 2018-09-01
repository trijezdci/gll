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
 * Token.mod
 *
 * Implementation of EBNF Token module.
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

IMPLEMENTATION MODULE Token;

(* Token Subranges *)

TYPE
  ResWords = TokenT [Alias..Reserved];
  Identifiers = TokenT [ReswordIdent..TerminalIdent];
  Literals = TokenT [CharCode..String];
  OperL1 = TokenT [VerticalBar..VerticalBar];
  OperL2 = TokenT [Plus..QMark];


(* Functions To Determine Token Classification *)

PROCEDURE isResWord ( t : TokenT ) : BOOLEAN;
 (* Returns TRUE if t is a reserved word, otherwise FALSE. *)
BEGIN
  RETURN (t >= MIN(ResWords) AND t <= MAX(ResWords))
END isResWord;


PROCEDURE isIdent ( t : TokenT ) : BOOLEAN;
 (* Returns TRUE if t is an identifier, otherwise FALSE. *)
BEGIN
  RETURN (t >= MIN(Identifiers) AND t <= MAX(Identifiers))
END isResWord;


PROCEDURE isLiteral ( t : TokenT ) : BOOLEAN;
 (* Returns TRUE if t is a character or string, otherwise FALSE. *)
BEGIN
  RETURN (t >= MIN(Literals) AND t <= MAX(Literals))
END isResWord;


PROCEDURE isOperL1 ( t : TokenT ) : BOOLEAN;
 (* Returns TRUE if t is a level-1 operator, otherwise FALSE. *)
BEGIN
  RETURN
    (t >= MIN(OperL1) AND t <= MAX(OperL1))
END isOperL1;


PROCEDURE isOperL2 ( t : TokenT ) : BOOLEAN;
 (* Returns TRUE if t is a level-2 operator, otherwise FALSE. *)
BEGIN
  RETURN
    (t >= MIN(OperL2) AND t <= MAX(OperL2))
END isOperL2;


PROCEDURE isPragma ( t : TokenT ) : BOOLEAN;
 (* Returns TRUE if t is a pragma, otherwise FALSE. *)
BEGIN
  RETURN (t = TokenT.Pragma)
END isPragma;


PROCEDURE isComment ( t : TokenT ) : BOOLEAN;
 (* Returns TRUE if t is a comment, otherwise FALSE. *)
BEGIN
  RETURN (t = TokenT.BlockComment)
END isComment;


END Token.
