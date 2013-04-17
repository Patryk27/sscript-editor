{$MODE OBJFPC}
{$H+}

Unit SynEditSScript;

 Interface
 Uses Classes, Dialogs, SysUtils, Graphics, SynEditHighlighter;

 Type TRangeState = (rsUnknown, rsCStyleComment);
      TToken = (tNone, tIdent, tMacro, tString, tNumber, tShortComment, tLongComment);

 Type THighlighter = Class(TSynCustomHighlighter)
                      Private
                       fIdentifierAttri, fKeywordAttri, fNumberAttri, fStringAttri,
                       fCommentAttri, fMacroAttri, fPrimaryTypesAttri, fOtherAttri: TSynHighlighterAttributes;
                       fRange: TRangeState;
                       fToken: TToken;

                      Protected
                       fTokenPos, fTokenEnd: Integer;
                       fLineText: String;

                      Public
                       Constructor Create(AOwner: TComponent); override;

                       Procedure SetLine(const NewValue: String; LineNumber: Integer); override;
                       Procedure Next; override;
                       Function GetEol: Boolean; override;
                       Procedure GetTokenEx(out TokenStart: PChar; out TokenLength: Integer); override;
                       Function GetTokenAttribute: TSynHighlighterAttributes; override;

                       Function GetToken: String; override;
                       Function GetTokenPos: Integer; override;
                       Function GetTokenKind: Integer; override;
                       Function GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes; override;

                       Function GetRange: Pointer; override;
                       Procedure SetRange(Value: Pointer); override;
                       Procedure ResetRange; override;
                      End;

 Implementation
Uses mSettings, SynEdit;

{ CreateAttri }
Function CreateAttri(S: TSetting): TSynHighlighterAttributes;
Var F    : TSyntaxFormat;
    Style: TFontStyles = [];
Begin
 Result := TSynHighlighterAttributes.Create(getName(S), getName(S));

 F := getFormat(S);

 if (F.Bold) Then
  Include(Style, fsBold);
 if (F.Italic) Then
  Include(Style, fsItalic);
 if (F.Underline) Then
  Include(Style, fsUnderline);

 Result.Style := Style;

 Result.Foreground := F.FGColor;
 if (F.HasBGColor) Then
  Result.Background := F.BGColor;
End;

{ THighlighter.Create }
Constructor THighlighter.Create(AOwner: TComponent);
Begin
 if not (AOwner is TSynEdit) Then
  raise Exception.Create('AOwner needs to be a TSynEdit!');

 inherited Create(AOwner);

 With TSynEdit(AOwner) do
 Begin
  Font := FetchFont(getFont(sEditorFont));

  Font.Quality := fqClearType;
  Font.Color   := getColor(sEditorForeground);

  Color := getColor(sEditorBackground);
 End;

 fIdentifierAttri := CreateAttri(sIdentFormat);
 AddAttribute(fIdentifierAttri);

 fKeywordAttri := CreateAttri(sKeywordFormat);
 AddAttribute(fKeywordAttri);

 fNumberAttri := CreateAttri(sNumberFormat);
 AddAttribute(fNumberAttri);

 fStringAttri := CreateAttri(sStringFormat);
 AddAttribute(fStringAttri);

 fCommentAttri := CreateAttri(sCommentFormat);
 AddAttribute(fCommentAttri);

 fMacroAttri := CreateAttri(sMacroFormat);
 AddAttribute(fMacroAttri);

 fPrimaryTypesAttri := CreateAttri(sPrimaryTypesFormat);
 AddAttribute(fPrimaryTypesAttri);

 fOtherAttri := CreateAttri(sOtherFormat);
 AddAttribute(fOtherAttri);

 fRange := rsUnknown;
End;

{ THighligter.SetLine }
Procedure THighlighter.SetLine(const NewValue: String; LineNumber: Integer);
Begin
 inherited;
 fLineText := NewValue;
 fTokenEnd := 1;
 Next;
End;

{ THighlighter.Next }
Procedure THighlighter.Next;
Const AlNum  = ['_', 'a'..'z', 'A'..'Z', '0'..'9'];
      Num    = ['0'..'9'];
      HexNum = ['a'..'f', 'A'..'F', '0'..'9'];
Var Len              : Integer;
    StringOpened, Dot: Boolean;

(* ReadString *)
// Term - terminator char ( ' or " )
Function ReadString(const Term: Char): Boolean;
Begin
 Result := False;

 Inc(fTokenEnd); // skip first `"` (or `'`)
 While (fTokenEnd < Len) Do
 Begin
  if (fLineText[fTokenEnd] = Term) and (fLineText[fTokenEnd-1] <> '\') Then
  Begin
   Inc(fTokenEnd);
   Exit(True);
  End;

  Inc(fTokenEnd);
 End;
 Inc(fTokenEnd);
End;

Begin
 fToken    := tNone;
 fTokenPos := fTokenEnd;
 fTokenEnd := fTokenPos;

 Len := Length(fLineText);
 if (fTokenPos > Len) Then
  Exit;

 { c-style (long) comment }
 if (Copy(fLineText, fTokenPos, 2) = '/*') or (fRange = rsCStyleComment) Then
 Begin
  fToken := tLongComment;
  fRange := rsCStyleComment;

  While (fTokenEnd-1 < Len) Do
  Begin
   if (Copy(fLineText, fTokenEnd, 2) = '*/') Then
   Begin
    fRange := rsUnknown;
    Inc(fTokenEnd, 2); // `*/` is also a part of the comment
    Break;
   End;

   Inc(fTokenEnd);
  End;

  if (fTokenEnd > Len) Then
   fTokenEnd := Len+1;

  Exit;
 End;

 fRange := rsUnknown;

 { one-line comment }
 if (Copy(fLineText, fTokenPos, 2) = '//') and (fRange <> rsCStyleComment) Then
 Begin
  fToken := tShortComment;

  fTokenEnd += Len-fTokenPos+1;

  Exit;
 End;

 { hexadecimal number }
 if (Copy(fLineText, fTokenPos, 2) = '0x') Then
 Begin
  Inc(fTokenEnd, 2); // 0x

  fToken := tNumber;

  While (fTokenEnd <= Len) Do
  Begin
   if not (fLineText[fTokenEnd] in HexNum) Then
    Break;

   Inc(fTokenEnd);
  End;

  Exit;
 End;

 { number }
 if (fLineText[fTokenPos] in Num) Then
 Begin
  fToken := tNumber;
  Dot    := False;

  While (fTokenEnd <= Len) Do
  Begin
   if (fLineText[fTokenEnd] = '.') Then
   Begin
    if (Dot) Then
     Break Else
     Dot := True;
    Inc(fTokenEnd);
   End;

   if not (fLineText[fTokenEnd] in Num) Then
    Break;

   Inc(fTokenEnd);
  End;

  Exit;
 End;

 { handle string as single token }
 if (fLineText[fTokenPos] in ['"', '''']) Then
 Begin
  fToken := tString;

  if (fTokenPos = Len) Then // special case
  Begin
   Inc(fTokenEnd);
   Exit;
  End;

  ReadString(fLineText[fTokenPos]);

  Exit;
 End;

 { macro }
 if (fLineText[fTokenPos] = '@') Then
 Begin
  fToken := tMacro;

  StringOpened := False;
  While (fTokenEnd < Len) and (not StringOpened) and (fLineText[fTokenEnd] <> ')') Do
  Begin
   if (fLineText[fTokenEnd] in ['"', '''']) Then // skip string
    if (not ReadString(fLineText[fTokenEnd])) Then
     Break Else
     Dec(fTokenEnd);
   Inc(fTokenEnd);
  End;
  Inc(fTokenEnd);

  if (fTokenEnd > Len) Then
   fTokenEnd := Len+1;

  Exit;
 End;

 { alpha-numeric (identifier) }
 if (fLineText[fTokenPos] in AlNum) Then
 Begin
  fToken := tIdent;

  While (fTokenEnd <= Len) and (fLineText[fTokenEnd] in AlNum) Do
   Inc(fTokenEnd);

  Exit;
 End;

 { not alpha-numeric }
 if not (fLineText[fTokenEnd] in AlNum) Then
 Begin
  Inc(fTokenEnd);
  Exit;
 End;

 if (fLineText[fTokenEnd] in [#9, ' ']) Then
 Begin
  While (fTokenEnd <= Len) and (fLineText[fTokenEnd] in [#0..#32]) Do
   Inc(fTokenEnd);
 End Else
 Begin
  While (fTokenEnd <= Len) and not (fLineText[fTokenEnd] in [#9, ' ']) Do
  Begin
   Inc(fTokenEnd);
   if not (fLineText[fTokenEnd] in AlNum) Then
    Break;
  End;
 End;
End;

{ THighlighter.GetEol }
Function THighlighter.GetEol: Boolean;
Begin
 Result := fTokenPos > Length(fLineText);
End;

{ THighlighter.GetTokenEx }
Procedure THighlighter.GetTokenEx(out TokenStart: PChar; out TokenLength: Integer);
Begin
 TokenStart  := @fLineText[fTokenPos];
 TokenLength := fTokenEnd - fTokenPos;
End;

{ THighlighter.GetTokenAttribute }
Function THighlighter.GetTokenAttribute: TSynHighlighterAttributes;
Var Keyword: String = '';
Begin
 if (fToken in [tShortComment, tLongComment]) Then
  Exit(fCommentAttri);

 { identifier }
 if (fToken = tIdent) Then
 Begin
  Keyword := GetToken;
  if not (Keyword[1] in ['a'..'z']) Then
   Delete(Keyword, 1, 1);

  { keyword }
  Case Keyword of
   'function', 'var', 'const', 'return', 'naked', 'for', 'if', 'else', 'while', 'break', 'continue',
   'in', 'do', 'private', 'public', 'type', 'new', 'delete', 'namespace', 'use', 'cast', 'try', 'catch', 'throw': Exit(fKeywordAttri);
   'void', 'null', 'bool', 'char', 'int', 'float', 'string': Exit(fPrimaryTypesAttri);
  End;

  { just some identifier }
  Exit(fIdentifierAttri);
 End;

 Case fToken of
  tIdent : Exit(fIdentifierAttri);
  tNumber: Exit(fNumberAttri);
  tString: Exit(fStringAttri);
  tMacro : Exit(fMacroAttri);
 End;

 Exit(fOtherAttri);
End;

{ THighlighter.GetToken }
Function THighlighter.GetToken: String;
Begin
 Result := Copy(fLineText, fTokenPos, fTokenEnd-fTokenPos);
End;

{ THighlighter.GetTokenPos }
Function THighlighter.GetTokenPos: Integer;
Begin
 Result := fTokenPos-1;
End;

{ THighlighter.GetDefaultAttribute }
Function THighlighter.GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
Begin
 Case Index of
  SYN_ATTR_COMMENT: Result := fCommentAttri;
  SYN_ATTR_IDENTIFIER: Result := fIdentifierAttri;
//  SYN_ATTR_STRING: Result := fStringAttri;
  //SYN_ATTR_WHITESPACE: Result := fSpaceAttri;
  else Result := nil;
 End;
End;

{ THighlighter.GetTokenKind }
Function THighlighter.GetTokenKind: Integer;
Var A: TSynHighlighterAttributes;
Begin
 Result := 0;
 A := GetTokenAttribute;

 if (A = fIdentifierAttri) Then
  Result := 1;

 if (A = fKeywordAttri) Then
  Result := 2;

 if (A = fNumberAttri) Then
  Result := 3;

 if (A = fStringAttri) Then
  Result := 4;

 if (A = fCommentAttri) Then
  Result := 5;

 if (A = fMacroAttri) Then
  Result := 6;

 if (A = fPrimaryTypesAttri) Then
  Result := 7;

 if (A = fOtherAttri) Then
  Result := 8;
End;

{ THighlighter.GetRange }
Function THighlighter.GetRange: Pointer;
Begin
 Result := Pointer(fRange);
End;

{ THighlighter.SetRange }
Procedure THighlighter.SetRange(Value: Pointer);
Begin
 fRange := TRangeState(Value);
End;

{ THighlighter.ResetRange }
Procedure THighlighter.ResetRange;
Begin
 fRange := rsUnknown;
End;
End.
