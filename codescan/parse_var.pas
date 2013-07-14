(* TCodeScanner.ParseVar *)
Procedure TCodeScanner.ParseVar;
Var Name: String;
    Vari: TVariable;
Begin
 With Parser do
 Begin
  eat(_LOWER); // `<`
  read_and_mark([_GREATER]); // [var type] + `>`

  While (true) Do
  Begin
   Name := read_ident;
   Vari := TVariable.Create(next(-1), getCurrentRange, Name);

   if (inFunction) Then
    CurrentFunction.SymbolList.Add(TSymbol.Create(stVariable, Vari)) Else
    CurrentNamespace.SymbolList.Add(TSymbol.Create(stVariable, Vari));

   if (next_t = _EQUAL) THen
   Begin
    eat(_EQUAL);
    read_and_mark([_SEMICOLON, _COMMA]);
    Dec(TokenPos); // 'read_and_mark' eats previous token
   End;

   if (next_t = _SEMICOLON) Then
   Begin
    eat(_SEMICOLON);
    Break;
   End Else
    eat(_COMMA);
  End;
 End;
End;

(* TCodeScanner.ParseConst *)
Procedure TCodeScanner.ParseConst;
Var Cnst: TVariable;
    Name: String;
Begin
 With Parser do
 Begin
  While (true) Do
  Begin
   Name := read_ident;
   Cnst := TVariable.Create(next(-1), getCurrentRange, Name);

    if (inFunction) Then
     CurrentFunction.SymbolList.Add(TSymbol.Create(stConstant, Cnst)) Else
     CurrentNamespace.SymbolList.Add(TSymbol.Create(stConstant, Cnst));

   eat(_EQUAL);

   read_and_mark([_COMMA, _SEMICOLON]);
   if (next_t(-1) = _SEMICOLON) Then
    Exit;
  End;
 End;
End;
