(*
 Copyright © by Patryk Wychowaniec, 2013-2014
 All rights reserved.
*)
Unit mFunctions;

 Interface

 Function MPos(const SubStr, Str: String): Integer;

 Function isKeyword(const Str: String): Boolean;
 Function isInternalType(const Str: String): Boolean;
 Function isNumber(const Str: String): Boolean;

 Function isValidFileName(const Str: String): Boolean;

 Function getApplicationDir: String;
 Function getLayoutsDir: String;
 Function getStylesDir: String;
 Function getLanguagesDir: String;

 Implementation
Uses SysUtils, Tokens;

(* MPos *)
{
 Works almost the same way as `System.Pos`, but takes care not to find text inside strings.
}
Function MPos(const SubStr, Str: String): Integer;
Var inString: Boolean = False;
    I       : Integer;
Begin
 Result := 0;

 if (Length(Str) = 0) Then
  Exit;

 I := 1;
 While (I <= Length(Str)) Do
 Begin
  if (Str[I] in ['''', '"']) Then
   inString := not inString Else

  if (not inString) Then
  Begin
   if (Copy(Str, I, Length(SubStr)) = SubStr) Then
    Exit(I);
  End;

  Inc(I);
 End;
End;

(* isKeyword *)
Function isKeyword(const Str: String): Boolean;
Var Tmp: String;
Begin
 For Tmp in Tokens.Keywords Do
  if (Str = Tmp) Then
   Exit(True);

 Exit(False);
End;

(* isInternalType *)
Function isInternalType(const Str: String): Boolean;
Begin
 Case Str of
  'any', 'void', 'bool', 'char', 'int', 'float', 'string': Exit(True);

  else
   Exit(False);
 End;
End;

(* isNumber *)
Function isNumber(const Str: String): Boolean;
Var Ch: Char;
Begin
 Result := True;

 For Ch in Str Do
  if (not (Ch in ['0'..'9'])) Then
   Exit(False);
End;

(* isValidFileName *)
{
 Checks if passed string is a valid Windows file name.
 It is designed to perform a *simple* check, so it may show 'true' even on some
 invalid file names.
 Use with caution.
}
Function isValidFileName(const Str: String): Boolean;
Const InvalidChars: Set of Char = ['\', '/', ':', '*', '?', '"', '<', '>', '|'];
Var Ch: Char;
Begin
 For Ch in Str Do
  if (Ch in InvalidChars) Then
   Exit(False);

 Result := (not DirectoryExists(Str));
End;

(* getApplicationDir *)
{
 Returns full application path with the directory separator at the end.
 Eg.:
   C:\Editor\
}
Function getApplicationDir: String;
Begin
 Result := ExtractFilePath(ParamStr(0));

 if (not (Result[Length(Result)] = DirectorySeparator)) Then
  Result += DirectorySeparator;
End;

(* getLayoutsDir *)
{
 Returns full path to the layouts directory, with the directory separator at the end.
 Eg.:
   C:\Editor\layouts\
}
Function getLayoutsDir: String;
Begin
 Result := getApplicationDir + 'layouts' + DirectorySeparator;
End;

(* getStylesDir *)
{
 Returns full path to the styles directory, with the directory separator at the end.
 Eg.:
   C:\Editor\styles\
}
Function getStylesDir: String;
Begin
 Result := getApplicationDir + 'styles' + DirectorySeparator;
End;

(* getLanguagesDir *)
{
 Returns full path to the languages directory, with the directory separator at the end.
 Eg.:
   C:\Editor\langs\
}
Function getLanguagesDir: String;
Begin
 Result := getApplicationDir + 'langs' + DirectorySeparator;
End;
End.
