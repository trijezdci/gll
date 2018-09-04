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
  source         : SourceT;
  nextSymbol     : SymbolT;
  ignoreComments : BOOLEAN;
  warnings,
  errors         : CARDINAL;
  lastStatus     : Status
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

  (* set comment mode *)
  newLexer^.ignoreComments :=
    Capabilities.isDisabled(Capabilities.PreserveComments);

  (* read the first symbol to be returned *)
  newLexer^.nextSymbol := Lexer.consumeSym(newLexer);

  s := Status.Success
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
PROCEDURE GetSym ( lexer : Lexer; VAR sym, nextSym : SymbolT );

BEGIN
  (* nextSymbol holds current lookahead, pass it back in sym *)
  sym := lexer^.nextSymbol;

  (* consume the current lookahead,
     read the new lookahead symbol, pass it back in nextSym *)
  Lexer.ConsumeSym(lexer, nextSym)
END GetSym;


(* ---------------------------------------------------------------------------
 * public procedure ConsumeSym(lexer)
 *  consumes current lookahead symbol and passes back new lookahead symbol
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
PROCEDURE ConsumeSym ( lexer : Lexer; VAR nextSym : SymbolT );

VAR
  source : SourceT;
  lexeme : StringT;
  nextChar : CHAR;
  line, column : CARDINAL;

BEGIN
  (* ensure source is valid *)
  IF lexer = NIL THEN
    (* TO DO: report error *)
    RETURN
  END;

  (* shorthand *)
  source := lexer^.source;

  (* all decisions are based on lookahead character *)
  nextChar := source.lookaheadChar();

  (* skip non-semantic symbols *)
  WHILE nonSemanticLead(nextChar) DO
    (* skip whitespace, tab and newline *)
    WHILE NOT Source.eof(source) AND
      ((nextChar = " ") OR
       (nextChar = ASCII.TAB) OR
       (nextChar = ASCII.NEWLINE)) DO
      nexChar := Source.consumeChar(source)
    END; (* WHILE *)

    (* skip pragmas *)
    IF lexer^.ignorePragmas THEN
      WHILE (nextChar = "/") AND (Source.la2Char(source) = "*") DO
        nextChar := MatchLex.skipPragma(source)
      END (* WHILE *)
    END; (* IF *)

    (* skip comments *)
    IF lexer^.ignoreComments THEN
      WHILE (nextChar = "/") AND (Source.la2Char(source) = "*") DO
        nextChar := MatchLex.skipBlockComment(source)
      END (* WHILE *)
    END; (* IF *)

    (* skip disabled code block *)
    WHILE (nextChar = "?") AND (Source.la2Char(source) = "<") DO
      nextChar := MatchLex.skipDisabledCodeBlock(source)
    END (* WHILE *)
  END; (* WHILE *)

  (* get current position *)
  Source.GetLineAndColumn(source, line, column);

  (* check for end-of-file *)
  IF Source.eof(source) THEN
    Symbol.Set(nextSym, TokenT.EOF, line, column, NIL)

  (* check for any other symbol *)
  ELSE
    CASE nextChar OF
    (* next symbol is quoted literal *)
    | ASCII.SINGLEQUOTE,
      ASCII.DOUBLEQUOTE :
        MatchLex.QuotedLiteral(source, nextSym)

    (* next symbol is "(" *)
    | "(" :
        (* consume "(" *)
        nextChar := Source.consumeChar(source);
        lexeme := Token.lexemeForToken(TokenT.LParen);
        Symbol.Set(nextSym, TokenT.LParen, line, column, lexeme)

    (* next symbol is ")" *)
    | ")" :
        (* consume ")" *)
        nextChar := Source.consumeChar(source);
        lexeme := Token.lexemeForToken(TokenT.LParen);
        Symbol.Set(nextSym, TokenT.LParen, line, column, lexeme)

    (* next symbol is "*" *)
    | "*" :
        (* consume "*" *)
        nextChar := Source.consumeChar(source);
        lexeme := Token.lexemeForToken(TokenT.Asterisk);
        Symbol.Set(nextSym, TokenT.Asterisk, line, column, lexeme)

    (* next symbol is "+" *)
    | "+" :
        (* consume "+" *)
        nextChar := Source.consumeChar(source);
        lexeme := Token.lexemeForToken(TokenT.Plus);
        Symbol.Set(nextSym, TokenT.Plus, line, column, lexeme)

    (* next symbol is "," *)
    | "," :
        (* consume "," *)
        nextChar := Source.consumeChar(source);
        lexeme := Token.lexemeForToken(TokenT.Comma);
        Symbol.Set(nextSym, TokenT.Comma, line, column, lexeme)

    (* next symbol is "." or ".." *)
    | "." :
        (* consume "." *)
        nextChar := Source.consumeChar(source);

        IF nextChar = "." THEN (* found ".." *)
          (* consume second "." *)
          nextChar := Source.consumeChar(source);
          lexeme := Token.lexemeForToken(TokenT.DotDot);
          Symbol.Set(nextSym, TokenT.DotDot, line, column, lexeme)

        ELSE (* found sole "." *)
          (* first "." already consumed *)
          sym.lexeme := Token.lexemeForToken(TokenT.Dot);
          Symbol.Set(nextSym, TokenT.Dot, line, column, lexeme)
        END (* "." and ".." *)

    (* next symbol may be pragma or block comment *)
    | "/" :
        IF NOT lexer^.ignoreComments THEN
          IF Source.la2Char(source) = "*" THEN (* found '/*' *)

            TO DO

          END (* IF *)
        END (* IF *)

    (* next symbol is character code literal *)
    | "0" :
        MatchLex.CharCodeLiteral(source, nextSym)

    (* next symbol may be ":=" *)
    | ":" :
        IF Source.la2Char(source) = "=" THEN (* found ':=' *)
          (* consume ":" *)
          nextChar := Source.consumeChar(source);
          (* consume "=" *)
          nextChar := Source.consumeChar(source);
          lexeme := Token.lexemeForToken(TokenT.Assign);
          Symbol.Set(nextSym, TokenT.Assign, line, column, lexeme)
        END (* ':=' *)

    (* next symbol is ";" *)
    | ";" :
        (* consume ";" *)
        nextChar := Source.consumeChar(source);
        lexeme := Token.lexemeForToken(TokenT.Semicolon);
        Symbol.Set(nextSym, TokenT.Semicolon, line, column, lexeme)

    (* next symbol may be predefined literal *)
    | "<" :
        MatchLex.PredefineLiteral(source, nextSym)

    (* next symbol is "=" *)
    | "=" :
        (* consume "=" *)
        nextChar := Source.consumeChar(source);
        lexeme := Token.lexemeForToken(TokenT.Equal);
        Symbol.Set(nextSym, TokenT.Equal, line, column, lexeme)

    (* next symbol is "?" *)
    | "?" :
        (* consume "?" *)
        nextChar := Source.consumeChar(source);
        lexeme := Token.lexemeForToken(TokenT.QMark);
        Symbol.Set(nextSym, TokenT.QMark, line, column, lexeme)

    (* next symbol is reserved word or identifier *)
    | "A" .. "Z",
      "a" .. "z" :
        MatchLex.ReswordOrIdent(source, nextSym)

    (* next symbol is "|" *)
    | "|" :
        (* consume "|" *)
        nextChar := Source.consumeChar(source);
        lexeme := Token.lexemeForToken(TokenT.VerticalBar);
        Symbol.Set(nextSym, TokenT.VerticalBar, line, column, lexeme)

    (* next symbol is invalid *)
    ELSE
      (* consume symbol *)
      Source.MarkLexeme(source, line, column);
      nextChar := Source.consumeChar(source);
      Source.CopyLexeme(source, lexeme);
      (* pass invalid symbol *)
      Symbol.Set(nextSym, TokenT.Invalid, line, column, lexeme);
      (* increment error counter *)
      lexer^.errors := lexer^.errors + 1
    END (* CASE *)
  END (* IF *);

  (* store symbol for use by LookaheadSym *)
  lexer^.nextSymbol := nextSym
END ConsumeSym;


(* ---------------------------------------------------------------------------
 * public procedure Lexer.LookaheadSym(lexer)
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
PROCEDURE LookaheadSym ( lexer : Lexer; VAR sym : SymbolT );

BEGIN
   sym := lexer^.nextSymbol
END LookaheadSym;


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
