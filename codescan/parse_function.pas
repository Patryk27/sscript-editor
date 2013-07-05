(* TCodeScanner.ParseFunction *)
// @TODO: circular references
Procedure TCodeScanner.ParseFunction;
Var Func: TFunction;
Begin
 // if function <> function_at_caret then add_ident(read_name()); just_skip_rest_of_the_function_not_parsing_it();

 With Parser do
 Begin
  if (next_t <> _LOWER) Then
   eat(_LOWER);

  read_and_mark([_GREATER]);

  Func      := TFunction.Create(next, getCurrentRange, '');
  Func.Name := read_ident;

  CurrentNamespace.SymbolList.Add(TSymbol.Create(stFunction, Func));

  { parameter list }
  if (next_t <> _BRACKET1_OP) Then
   eat(_BRACKET1_OP);
  read_and_mark([_BRACKET1_CL]);

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
