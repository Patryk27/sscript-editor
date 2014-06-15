(* TCodeScanner.ParseType *)
Procedure TCodeScanner.ParseType;
Var Name, StrType: String;
    SymbolList   : TSymbolList;
    EnumItem     : TVariable;
    Typ          : TType;
Begin
 With Parser do
 Begin
  eat(_LOWER); // `<`
  StrType := read_type;
  eat(_GREATER); // `>`

  Name := read_ident;
  Typ  := TType.Create(self, next(-1), getCurrentRange, Name);

  if (inFunction) Then
   SymbolList := CurrentFunction.SymbolList Else
   SymbolList := CurrentNamespace.SymbolList;

  SymbolList.Add(TSymbol.Create(stType, Typ));

  { type<enum> foo = [foo1, foo2=2+2*2, foo3...]; }
  if (StrType = 'enum') Then
  Begin
   eat(_EQUAL);
   eat(_BRACKET2_OP);

   While (true) Do
   Begin
    Name     := read_ident;
    EnumItem := TVariable.Create(self, next(-1), getCurrentRange, Name);

    SymbolList.Add(TSymbol.Create(stConstant, EnumItem));

    if (next_t = _EQUAL) Then
    Begin
     eat(_EQUAL);
     read_and_mark([_COMMA, _BRACKET2_CL]);
     StepBack;
    End;

    if (next_t = _BRACKET2_CL) Then
    Begin
     eat(_BRACKET2_CL);
     Break;
    End Else
    Begin
     eat(_COMMA);
    End;
   End;
  End;

  semicolon;
 End;
End;
