(* TCodeScanner.ParseFunction *)
Procedure TCodeScanner.ParseFunction;
Var Func: TFunction;
    Vari: TVariable;
Begin
 With Parser do
 Begin
  if (next_t <> _LOWER) Then
   eat(_LOWER);

  read_and_mark([_GREATER]);

  Func      := TFunction.Create(next, getCurrentRange, '');
  Func.Name := read_ident;

  CurrentNamespace.SymbolList.Add(TSymbol.Create(stFunction, Func));

  { parameter list }
  eat(_BRACKET1_OP);

  While (next_t <> _BRACKET1_CL) Do
  Begin
   read_type;

   if (next_t = _IDENTIFIER) Then
   Begin
    Vari      := TVariable.Create(next, Parser.getCurrentRange(0), '');
    Vari.Name := read_ident;
    Func.SymbolList.Add(TSymbol.Create(stVariable, Vari));
   End;

   if (next_t = _EQUAL) Then
   Begin
    eat(_EQUAL);
    read_and_mark([_COMMA, _BRACKET1_CL]);
    Dec(TokenPos);
   End;

   if (next_t <> _BRACKET1_CL) Then
    eat(_COMMA) Else
    Break;
  End;

  eat(_BRACKET1_CL);

  if (next_t = _BRACKET2_OP) Then // special case: function<type> name(param list) [special attributes];
  Begin
   eat(_BRACKET2_OP);
   While (next_t <> _BRACKET2_CL) Do
    read;
   eat(_BRACKET2_CL);

   semicolon;
   Exit;
  End;

  While (next_t <> _BRACKET3_OP) Do // read until `{`
   read;

  inFunction      := True;
  CurrentFunction := Func;

  Func.Range := getCurrentRange(0);

  read_and_mark([_BRACKET3_CL]);

  inFunction      := False;
  CurrentFunction := nil;
 End;
End;
