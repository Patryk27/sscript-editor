(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
Unit mFunctions;

 Interface

 Function MPos(const SubStr, Str: String): Integer;

 Function isKeyword(const Str: String): Boolean;
 Function isInternalType(const Str: String): Boolean;

 Implementation
Uses Tokens;

(* MPos *)
{
 Works almost the same way as `System.Pos`, but takes care not to find text inside strings.
}
Function MPos(const SubStr, Str: String): Integer;
Var I       : Integer;
    inString: Boolean = False;
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
End.
