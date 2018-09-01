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
 * Lexer.mod
 *
 * Implementation of EBNF lexer module.
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

IMPLEMENTATION MODULE Lexer;

(* Lexer for EBNF grammar specifications *)

IMPORT ASCII, Capabilities, String, Source, Token, Symbol, MatchLex;

FROM String IMPORT StringT;
FROM Source IMPORT SourceT;
FROM Token IMPORT TokenT;
FROM Symbol IMPORT SymbolT;


(* Lexer Type *)

TYPE Lexer = POINTER TO LexerDescriptor;

TYPE LexerDescriptor = RECORD
  source     : SourceT;
  nextSymbol : SymbolT;
  warnings,
  errors     : CARDINAL;
  lastStatus : Status
END; (* LexerDescriptor *)


(* Operations *)

(* ---------------------------------------------------------------------------
 * public procedure Lexer.New(newLexer, filename, status)
 *  creates a new lexer instance, associated with filename, passes status
 * ---------------------------------------------------------------------------
 * pre-conditions:
 *  TO DO
 *
 * post-conditions:
 *  TO DO
 *
 * error-conditions:
 *  TO DO
 * ---------------------------------------------------------------------------
 *)
PROCEDURE New ( VAR newLexer : Lexer; filename : StringT; VAR s : Status );

VAR
  source : SourceT;
  sourceStatus : Source.Status;

BEGIN
  (* lexer must not have been initialised *)
  IF newLexer # NIL THEN
    status := Status.AlreadyInitialised;
    RETURN
  END;

  (* allocate and initialise source *)
  Source.New(source, filename, sourceStatus);
  IF sourceStatus # Source.Status.Success THEN
    s := Status.UnableToAllocate;
    RETURN
  END;

  (* allocate a lexer instance *)
  NEW newLexer;
  IF newLexer = NIL THEN
    s := Status.UnableToAllocate;
    RELEASE source;
    RETURN
  END;

  (* initialise lexer *)
  newLexer^.source := source;
  newLexer^.warnings := 0;
  newLexer^.errors := 0;
  newLexer^.lastStatus := Status.Success;

  (* read the first symbol to be returned *)
  newLexer^.nextSymbol := Lexer.consumeSym(newLexer);

  s := Status.Success;
  RETURN
END New;


(* ---------------------------------------------------------------------------
 * public procedure Lexer.GetSym(lexer, symbol, lookaheadSymbol)
 *  passes and consumes current lookahead symbol, passes new lookahead symbol
 * ---------------------------------------------------------------------------
 * pre-conditions:
 *  TO DO
 *
 * post-conditions:
 *  TO DO
 *
 * error-conditions:
 *  TO DO
 * ---------------------------------------------------------------------------
 *)
PROCEDURE GetSym ( lexer : Lexer; VAR sym, next : SymbolT );

BEGIN
  (* nextSymbol holds current lookahead, pass it back in sym *)
  sym := lexer^.nextSymbol;

  (* consume the current lookahead,
     read the new lookahead symbol, pass it back in next *)
  next := Lexer.consumeSym(lexer);

  RETURN
END GetSym;


(* ---------------------------------------------------------------------------
 * public function consumeSym(lexer)
 *  consumes current lookahead symbol and returns new lookahead symbol
 * ---------------------------------------------------------------------------
 * pre-conditions:
 *  TO DO
 *
 * post-conditions:
 *  TO DO
 *
 * error-conditions:
 *  TO DO
 * ---------------------------------------------------------------------------
 *)
PROCEDURE consumeSym ( lexer : Lexer ) : SymbolT;

VAR
  ch, next, la2 : CHAR;
  source : Source;
  sym : SymbolT;

BEGIN
  (* ensure source is valid *)
  IF lexer = NIL THEN
  (* TO DO: report and handle error *)
    RETURN Symbol.NilSymbol
  END;

  (* shorthand *)
  source := lexer^.source;

  (* all decisions are based on lookahead *)
  next := source.lookaheadChar();

  (* skip any whitespace, tab and new line *)
  WHILE NOT Source.eof(source) AND
    (next = ASCII.SPACE OR next = ASCII.TAB OR next = ASCII.NEWLINE) DO
    next := Source.consumeChar(source)
  END; (* WHILE *)

  (* skip comments unless comments are to be preserved *)
  IF Capabilities.isDisabled(Capabilities.PreserveComments) THEN

    (* skip any block comment *)
    WHILE next = "/" AND Source.la2Char(source) = "*" DO
      MatchLex.BlockComment(source);
      next := Source.lookahead(source)
    END (* WHILE *)

  END; (* IF *)

  (* get current position *)
  Source.GetLineAndColumn(source, sym.line, sym.column);

  (* skip any disabled code section *)
  WHILE next = "?" AND Source.la2Char() = "<" AND sym.column = 1 DO
    MatchLex.DisabledCodeBlock(source);
    next := Source.lookahead(source);
    Source.GetLineAndColumn(source, sym.line, sym.column)
  END; (* WHILE *)

  (* check for end-of-file *)
  IF Source.eof(source) THEN
    sym.token := TokenT.EOF;
    sym.lexeme := 0

  (* check for any other symbol *)
  ELSE
    CASE next OF
    (* next symbol is quoted literal *)
    | ASCII.SINGLEQUOTE,
      ASCII.DOUBLEQUOTE :
        Source.MarkLexeme(source, sym.line, sym.column);
        MatchLex.QuotedLiteral(source, sym.token);
        Source.CopyLexeme(source, lexer^.dict, sym.lexeme)

    (* next symbol is "(" or block comment *)
    | "(" :
        (* consume "(" *)
        next := Source.consumeChar(source);
        Source.GetLineAndColumn(source, sym.line, sym.column);
        sym.token := TokenT.LParen;
        sym.lexeme := Token.lexemeForToken(TokenT.LParen)

    (* next symbol is ")" *)
    | ")" :
        (* consume ")" *)
        next := Source.consumeChar(source);
        Source.GetLineAndColumn(source, sym.line, sym.column);
        sym.value := TokenT.RParen;
        sym.lexeme := Token.lexemeForToken(TokenT.RParen)

    (* next symbol is "*" *)
    | "*" :
        (* consume "*" *)
        next := Source.consumeChar(source);
        Source.GetLineAndColumn(source, sym.line, sym.column);
        sym.token := TokenT.Asterisk;
        sym.lexeme := Token.lexemeForToken(TokenT.Asterisk)

    (* next symbol is "+" *)
    | "+" :
        (* consume "+" *)
        next := Source.consumeChar(source);
        Source.GetLineAndColumn(source, sym.line, sym.column);
        sym.token := TokenT.Plus;
        sym.lexeme := Token.lexemeForToken(TokenT.Plus)

    (* next symbol is "," *)
    | "," :
        (* consume "," *)
        next := Source.consumeChar(source);
        Source.GetLineAndColumn(source, sym.line, sym.column);
        sym.token := TokenT.Comma;
        sym.lexeme := Token.lexemeForToken(TokenT.Comma)

    (* next symbol is "." or ".." *)
    | "." :
        (* consume "." *)
        next := Source.consumeChar(source);
        Source.GetLineAndColumn(source, sym.line, sym.column);

        IF next = "." THEN (* found ".." *)
          (* consume second "." *)
          next := Source.consumeChar(source);
          sym.token := TokenT.DotDot;
          sym.lexeme := Token.lexemeForToken(TokenT.DotDot)

        ELSE (* found sole "." *)
          (* first "." already consumed *)
          sym.token := TokenT.Dot;
          sym.lexeme := Token.lexemeForToken(TokenT.Dot)

        END (* "." and ".." *)

    (* next symbol may be block comment *)
    | "/" :
        IF Capabilities.isEnabled(Capabilities.PreserveComments) THEN
          IF Source.la2Char(source) = "*" THEN (* found '/*' *)
            Source.MarkLexeme(source, sym.line, sym.column);
            MatchLex.BlockComment(source, sym.token);
            Source.CopyLexeme(source, lexer^.dict, sym.lexeme)
          END (* IF *)
        END (* IF *)

    (* next symbol is numeric literal *)
    | "0" .. "9" :
        Source.MarkLexeme(source, sym.line, sym.column);
        MatchLex.NumericLiteral(source, sym.token);
        Source.CopyLexeme(source, lexer^.dict, sym.lexeme)

    (* next symbol may be ":=" *)
    | ":" :
        IF Source.la2Char(source) = "=" THEN (* found ':=' *)
          (* consume ":" and get line and column *)
          next := Source.consumeChar(source);
          Source.GetLineAndColumn(source, sym.line, sym.column);
          (* consume '=' *)
          next := Source.consumeChar(source);
          sym.token := TokenT.Assign;
          sym.lexeme := Token.lexemeForToken(TokenT.Assign)
        END (* ':=' *)

    (* next symbol is ";" *)
    | ";" :
        (* consume ";" *)
        next := Source.consumeChar(source);
        Source.GetLineAndColumn(source, sym.line, sym.column);
        sym.token := TokenT.Semicolon;
        sym.lexeme := Token.lexemeForToken(TokenT.Semicolon)

    (* next symbol is annotation *)
    | "<" :
        Source.MarkLexeme(source, sym.line, sym.column);
        MatchLex.Annotation(source, sym.token);
        Source.CopyLexeme(source, lexer^.dict, sym.lexeme)

    (* next symbol is "=" *)
    | "=" :
        (* consume "=" *)
        next := Source.consumeChar(source);
        Source.GetLineAndColumn(source, sym.line, sym.column);
        sym.token := TokenT.Equal;
        sym.lexeme := Token.lexemeForToken(TokenT.Equal)

    (* next symbol is "?" *)
    | "?" :
        (* consume "?" *)
        next := Source.consumeChar(source);
        Source.GetLineAndColumn(source, sym.line, sym.column);
        sym.token := TokenT.QMark;
        sym.lexeme := Token.lexemeForToken(TokenT.QMark)

    (* next symbol is terminal identifier or reserved word *)
    | "A" .. "Z" :
        Source.MarkLexeme(source, sym.line, sym.column);
        MatchLex.IdentOrResword(source, sym.token);
        Source.CopyLexeme(source, lexer^.dict, sym.lexeme)


    (* next symbol is non-terminal identifier *)
    | "a" .. "z" :
        Source.MarkLexeme(source, sym.line, sym.column);
        MatchLex.Ident(source, sym.token);
        Source.CopyLexeme(source, lexer^.dict, sym.lexeme)

    (* next symbol is "|" *)
    | "|" :
        (* consume "|" *)
        next := Source.consumeChar(source);
        Source.GetLineAndColumn(source, sym.line, sym.column);
        sym.token := TokenT.VerticalBar;
        sym.lexeme := Token.lexemeForToken(TokenT.VerticalBar)

    (* next symbol is invalid *)
    ELSE
      Source.MarkLexeme(source, sym.line, sym.column);
      next := Source.consumeChar(source);
      sym.token := TokenT.Invalid;
      Source.CopyLexeme(source, lexer^.dict, sym.lexeme);
      lexer^.errors := lexer^.errors + 1
    END; (* CASE *)
  END (* IF *);

  (* store symbol for use by lookaheadSym *)
  lexer^.nextSymbol := sym;

  RETURN sym
END consumeSym;


(* ---------------------------------------------------------------------------
 * public function Lexer.lookaheadSym(lexer)
 *  returns current lookahead symbol
 * ---------------------------------------------------------------------------
 * pre-conditions:
 *  TO DO
 *
 * post-conditions:
 *  TO DO
 *
 * error-conditions:
 *  TO DO
 * ---------------------------------------------------------------------------
 *)
PROCEDURE lookaheadSym ( lexer : Lexer ) : SymbolT;

BEGIN
  RETURN lexer^.nextSymbol
END lookaheadSym;


(* ---------------------------------------------------------------------------
 * public function Lexer.status(lexer)
 *  returns status of last operation
 * ---------------------------------------------------------------------------
 * pre-conditions:
 *  TO DO
 *
 * post-conditions:
 *  TO DO
 *
 * error-conditions:
 *  TO DO
 * ---------------------------------------------------------------------------
 *)
PROCEDURE status ( lexer : Lexer ) : Status;

BEGIN

  IF lexer = NIL THEN
    RETURN Status.NotInitialised
  ELSE
    RETURN lexer^.lastStatus
  END (* IF *)

END status;


(* ---------------------------------------------------------------------------
 * public function Lexer.warnCount(lexer)
 *  returns current lexical warning count
 * ---------------------------------------------------------------------------
 * pre-conditions:
 *  TO DO
 *
 * post-conditions:
 *  TO DO
 *
 * error-conditions:
 *  TO DO
 * ---------------------------------------------------------------------------
 *)
PROCEDURE warnCount ( lexer : Lexer ) : CARDINAL;

BEGIN
  RETURN lexer^.warnings
END warnCount;


(* ---------------------------------------------------------------------------
 * public function Lexer.errorCount(lexer)
 *  returns current lexical error count
 * ---------------------------------------------------------------------------
 * pre-conditions:
 *  TO DO
 *
 * post-conditions:
 *  TO DO
 *
 * error-conditions:
 *  TO DO
 * ---------------------------------------------------------------------------
 *)
PROCEDURE errorCount ( lexer : Lexer ) : CARDINAL;

BEGIN
  RETURN lexer^.errors
END errorCount;


(* ---------------------------------------------------------------------------
 * public procedure Lexer.Release(lexer)
 *  releases lexer instance
 * ---------------------------------------------------------------------------
 * pre-conditions:
 *  (1) lexer must not be NIL
 *
 * post-conditions:
 *  (1) lexer is deallocated
 *  (2) NIL is passed back in lexer
 *
 * error-conditions:
 *  (1) reference to lexer remains unmodified
 * ---------------------------------------------------------------------------
 *)
PROCEDURE Release ( VAR lexer : Lexer );

BEGIN
  (* lexer must not be NIL *)
  IF lexer = NIL THEN
    RETURN
  END;

  (* release source and lexer *)
  Source.Release(lexer^.source);
  DISPOSE(lexer)
END Release;


END Lexer.
