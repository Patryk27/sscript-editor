Unit mFunctions;

 Interface

 Function MPos(const SubStr, Str: String): Integer;

 Implementation

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
End.
