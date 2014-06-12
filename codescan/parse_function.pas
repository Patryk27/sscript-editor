(* TCodeScanner.ParseFunction *)
Procedure TCodeScanner.ParseFunction;
Var Func: TFunction;
    Vari: TVariable;

    Return   : String;
    Param    : String;
    ParamType: String;
    ParamList: TFunctionParameterList;
Begin
 With Parser do
 Begin
  // read return type
  if (next_t = _LOWER) Then
  Begin
   eat(_LOWER);
   Return := read_type;
   eat(_GREATER);
  End Else
  Begin
   Return := 'void';
  End;

  // prepare class instance
  Func            := TFunction.Create(next, getCurrentRange, '');
  Func.Name       := read_ident;
  Func.ReturnType := Return;

  CurrentNamespace.SymbolList.Add(TSymbol.Create(stFunction, Func));

  { read parameter list }
  SetLength(ParamList, 0);
  eat(_BRACKET1_OP);

  While (next_t <> _BRACKET1_CL) Do
  Begin
   Param := '';

   if (next_t in [_VAR, _CONST]) Then // var, const
   Begin
    Param += read.Value+' ';
   End;

   ParamType := read_type;

   Param += ParamType+' ';

   // read parameter name
   if (next_t = _IDENTIFIER) Then
   Begin
    Vari      := TVariable.Create(next, Parser.getCurrentRange(0), '');
    Vari.Name := read_ident;
    Vari.Typ  := ParamType;

    Func.SymbolList.Add(TSymbol.Create(stVariable, Vari));

    Param += Vari.Name;
   End;

   // read optional default parameter value
   if (next_t = _EQUAL) Then
   Begin
    eat(_EQUAL);
    read_and_mark([_COMMA, _BRACKET1_CL]);
    StepBack(1);
   End;

   SetLength(ParamList, Length(ParamList)+1);
   ParamList[High(ParamList)] := Param;

   if (next_t <> _BRACKET1_CL) Then
    eat(_COMMA) Else
    Break;
  End;

  eat(_BRACKET1_CL);

  // read until function begin
  While (next_t <> _BRACKET3_OP) Do
   read;

  // parse function body
  inFunction      := True;
  CurrentFunction := Func;

  Func.ParamTypes := ParamList;
  Func.Range      := getCurrentRange(0);

  read_and_mark([_BRACKET3_CL]);

  inFunction      := False;
  CurrentFunction := nil;
 End;
End;
