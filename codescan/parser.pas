(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
Unit Parser;

 Interface
 Uses Classes, FGL, Scanner, Tokens;

 Type TTokenList = specialize TFPGList<PToken_P>;

 { TRange }
 Type TRange = Record
                PBegin, PEnd: TToken_P;
               End;

 { TParser }
 Type TParser = Class
                 Private
                // private fields
                  TokenList: TTokenList; // list of tokens (with stripped comments)

                  DontFailOnEOF, ParsingFORInitInstruction: Boolean;

                 Public
                // public fields
                  TokenPos: Int64; // current token ID (counting from 0)

                  CurrentDeep: Integer; // current brackets' deep (`{` = +1, `}` = -1)

                  Property getPosition: Int64 read TokenPos; // current token position
                  Property getTokenList: TTokenList read TokenList;

                // public methods
                  Constructor Create(Code: TStringList; const TokenFileName: String);
                  Destructor Destroy; override;

                  Function getToken(const Index: uint32): TToken_P;
                  Function getTokenPnt(const Index: uint32): PToken_P;
                  Function getLastToken: TToken_P;
                  Function getCurrentRange(Deep: Integer=1): TRange;

                  Function read: TToken_P;
                  Function read_t: TToken;
                  Function next(const I: Integer=0): TToken_P;
                  Function next_pnt(const I: Integer=0): PToken_P;
                  Function next_t(const I: Integer=0): TToken;
                  Function read_ident: String;
                  Function read_string: String;
                  Function read_int: Integer;
                  Procedure eat(Token: TToken);
                  Procedure semicolon;

                  Procedure skip_parenthesis;
                  Procedure read_until(const Token: TToken);

                  Function Can: Boolean;
                 End;

 Operator in (Token: TToken_P; Range: TRange): Boolean;

 Implementation
Uses mLanguages, SysUtils;

Type EParserError = Class(Exception);

// `TToken_P` in `TRange`
Operator in (Token: TToken_P; Range: TRange): Boolean;
Begin
 if (Token.FileName <> Range.PBegin.FileName) or (Token.FileName <> Range.PEnd.FileName) Then
  Exit(False);

 Result := (Token.Line >= Range.PBegin.Line) and (Token.Line <= Range.PEnd.Line);

 if (Result) Then
  if (Token.Line = Range.PBegin.Line) Then
   Result := Result and (Token.Char >= Range.PBegin.Char);

 if (Result) Then
  if (Token.Line = Range.PEnd.Line) Then
   Result := Result and (Token.Char <= Range.PEnd.Char);
End;

(* TParser.Create *)
{
 Loads code from file and preparses it (removes comments etc.)
}
Constructor TParser.Create(Code: TStringList; const TokenFileName: String);
Var Scanner: TScanner; // token scanner

    Token           : TToken_P; // current token
    PToken          : PToken_P;
    ShortCommentLine: LongWord=0; // short comment (`//`) line

    InLongComment: Boolean = False;
Begin
 TokenPos    := 0;
 CurrentDeep := 0;

 ParsingFORInitInstruction := False;

 { parse it }
 TokenList := TTokenList.Create;

 Scanner := TScanner.Create(Code);

 While (Scanner.Can) do
 Begin
  Token := Scanner.getToken_P;

  if (Token.Token = noToken) Then // skip `noToken`-s
   Continue;

  if (Token.Token = _EOF) Then
   Break;

  Case Token.Token of
   _DOUBLE_SLASH { // }:
    if (not inLongComment) Then
     ShortCommentLine := Token.Line;

   else
    if (Token.Line <> ShortCommentLine) Then // not in short (one-line) comment
    Begin
     if (Token.Token = _LONGCMT_OPEN { /* }) Then
      inLongComment := True Else
     if (Token.Token = _LONGCMT_CLOSE { */ }) Then
      inLongComment := False Else

     if (not inLongComment) Then
     Begin
      New(PToken);
      PToken^          := Token;
      PToken^.Position := TokenList.Count;
      PToken^.FileName := TokenFileName;
      TokenList.Add(PToken);
     End;
    End;
  End;
 End;

 TokenPos      := 0;
 CurrentDeep   := 0;
 DontFailOnEOF := False;

 { destroy objects }
 Scanner.Free;
End;

(* TParser.Destroy *)
Destructor TParser.Destroy;
Var Token: PToken_P;
Begin
 For Token in TokenList Do
  Dispose(Token);
 TokenList.Free;
End;

(* TParser.getToken *)
{
 Returns a token with specified index.
}
Function TParser.getToken(const Index: uint32): TToken_P;
Begin
 Result := TokenList[Index]^;
End;

(* TParser.getTokenPnt *)
{
 Returns a pointer to token with specified index.
}
Function TParser.getTokenPnt(const Index: uint32): PToken_P;
Begin
 Result := TokenList[Index];
End;

(* TParser.getLastToken *)
{
 Returns last non-`noToken` token
}
Function TParser.getLastToken: TToken_P;
Begin
 Exit(TokenList.Last^);
End;

(* TParser.getCurrentRange *)
{
 Returns current scope's range.
}
Function TParser.getCurrentRange(Deep: Integer=1): TRange;
Var TPos: Int64;
Begin
 Try
  DontFailOnEOF := True; // don't fail on case when brackets are unclosed (it would fail with error `unexpected eof`), as this error will be detected and raised later (eg.when parsing a construction)

  if (TokenPos >= TokenList.Count) Then
  Begin
   Result.PBegin := TokenList.Last^;
   Result.PEnd   := TokenList.Last^;
   Exit;
  End;

  TPos          := TokenPos;
  Result.PBegin := TokenList[TokenPos]^;

  if (ParsingFORInitInstruction) Then
  Begin
   read_until(_BRACKET1_CL);
   Deep := 0;

   if (next_t <> _BRACKET3_OP) Then
   Begin
    if (next_t in [_FOR, _WHILE]) Then
    Begin
     read;
     read;
     read_until(_BRACKET1_CL);
    End;

    read_until(_SEMICOLON);
    if (next_t = _ELSE) Then
    Begin
     read;
     read_until(_SEMICOLON);
    End;

    Result.PEnd := TokenList[TokenPos]^;
    TokenPos    := TPos;
    Exit;
   End;
  End;

  Result.PEnd := TokenList[TokenPos]^;

  While (true) Do
  Begin
   if (TokenPos >= TokenList.Count) Then // ending `}` not found
   Begin
    Dec(TokenPos);
    Break;
   End;

   Case TokenList[TokenPos]^.Token of
    _BRACKET3_OP: Inc(Deep);
    _BRACKET3_CL: Dec(Deep);
   End;

   if (Deep = 0) Then
    Break;

   Inc(TokenPos);
  End;

  if (TokenPos >= TokenList.Count) Then
   Dec(TokenPos);

  Result.PEnd := TokenList[TokenPos]^;

  TokenPos := TPos;
 Finally
  DontFailOnEOF := False;
 End;
End;

(* TParser.read *)
{
 Reads a token
}
Function TParser.read: TToken_P;
Begin
 if (TokenPos < 0) or (TokenPos >= TokenList.Count) Then
  raise EParserError.Create(getLangValue(ls_parser_eof));

 Result := TokenList[TokenPos]^;
 Inc(TokenPos);

 Case Result.Token of
  _INVALID_INT   : raise Exception.Create(getLangValue(ls_invalid_int_value));
  _INVALID_FLOAT : raise Exception.Create(getLangValue(ls_invalid_float_value));
  _INVALID_STRING: raise Exception.Create(getLangValue(ls_string_exceeds_line));
 End;
End;

(* TParser.read_t *)
{
 Reads a token's kind; see @TParser.read
}
Function TParser.read_t: TToken;
Begin
 Result := read.Token;
End;

(* TParser.next *)
{
 Returns a next - or previous (when `I` is negative) - token.
}
Function TParser.next(const I: Integer=0): TToken_P;
Begin
 Result := next_pnt(I)^;
End;

(* TParser.next_pnt *)
{
 Returns a next - or previous (when `I` is negative) - token's pointer.
}
Function TParser.next_pnt(const I: Integer=0): PToken_P;
Begin
 if (TokenPos+I >= TokenList.Count) Then
  Result := TokenList.Last Else
 if (TokenPos+I < 0) Then
  Result := TokenList.First Else
  Result := TokenList[TokenPos+I];
End;

(* TParser.next_t *)
{
 Works just as TParser.next, but gets only a token's kind.
}
Function TParser.next_t(const I: Integer=0): TToken;
Begin
 Result := next(I).Token;
End;

(* TParser.read_ident *)
{
 Reads an identifier; displays error `eExpectedIdentifier` when read token isn't an identifier.
}
Function TParser.read_ident: String;
Begin
 if (next_t <> _IDENTIFIER) Then
  raise Exception.Create(Format(getLangValue(ls_expected_identifier), [next.Value]));

 Result := read.Value;
End;

(* TParser.read_string *)
{
 Reads a string; displays error `eExpectedString` when read token isn't a string.
}
Function TParser.read_string: String;
Begin
 if (next_t <> _STRING) Then
  raise Exception.Create(Format(getLangValue(ls_expected_string), [next.Value]));

 Result := read.Value;
End;

(* TParser.read_int *)
{
 Reads an integer value; displays error `eExpectedInt` when read token isn't a string.
}
Function TParser.read_int: Integer;
Begin
 if (next_t <> _INT) Then
  raise Exception.Create(Format(getLangValue(ls_expected_int), [next.Value]));

 Result := StrToInt(read.Value);
End;

(* TParser.eat *)
{
 'eats' a specified token.
 (ie. if current token isn't token passed in the parameter, displays a syntax error).
}
Procedure TParser.eat(Token: TToken);
Begin
 if (read_t <> Token) Then
  raise Exception.Create(Format(getLangValue(ls_expected), [getTokenDisplay(Token), next(-1).Value]));
End;

(* TParser.semicolon *)
{
 Eats a semicolon (`_SEMICOLON` token)
}
Procedure TParser.semicolon;
Begin
 eat(_SEMICOLON);
End;

(* TParser.skip_parenthesis *)
{
 Skips parenthesises
}
Procedure TParser.skip_parenthesis;
Var Deep: Integer = 0;
Begin
 Repeat
  if ((TokenPos >= TokenList.Count) and (DontFailOnEOF)) Then
   Exit;

  Case read_t of
   _BRACKET1_OP, _BRACKET2_OP, _BRACKET3_OP, _LOWER  : Inc(Deep);
   _BRACKET1_CL, _BRACKET2_CL, _BRACKET3_CL, _GREATER: Dec(Deep);
  End;
 Until (Deep = 0);
End;

(* TParser.read_until *)
Procedure TParser.read_until(const Token: TToken);
Var Deep: Integer = 0;
    Tok : TToken;
Begin
 While (true) do
 Begin
  if ((TokenPos >= TokenList.Count) and (DontFailOnEOF)) Then
   Exit;

  Tok := read_t;

  if (Tok = Token) and (Deep = 0) Then
   Break;

  Case Tok of
   _BRACKET1_OP, _BRACKET2_OP, _BRACKET3_OP: Inc(Deep);
   _BRACKET1_CL, _BRACKET2_CL, _BRACKET3_CL: Dec(Deep);
  End;
 End;
End;

(* TParser.Can *)
{
 Returns 'true', if at least one token can be read.
}
Function TParser.Can: Boolean;
Begin
 Result := (TokenPos < TokenList.Count);
End;
End.
