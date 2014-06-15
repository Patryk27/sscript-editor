(* TCodeScanner.ParseType *)
Procedure TCodeScanner.ParseType;
Var Name: String;
    Typ : TType;
Begin
 With Parser do
 Begin
  eat(_LOWER); // `<`
  read_type;
  eat(_GREATER); // `>`

  Name := read_ident;
  Typ  := TType.Create(self, next(-1), getCurrentRange, Name);

  if (inFunction) Then
   CurrentFunction.SymbolList.Add(TSymbol.Create(stType, Typ)) Else
   CurrentNamespace.SymbolList.Add(TSymbol.Create(stType, Typ));

  semicolon;
 End;
End;
