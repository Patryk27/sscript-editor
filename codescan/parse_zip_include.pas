(* TCodeScanner.ParseZipInclude *)
Procedure TCodeScanner.ParseZipInclude(const FileName: String);
Const ChunkSize = 1024;
Var Process    : TProcess;
    OutputChunk: Array[0..ChunkSize] of Char;
    OutputData : String = '';
    Output     : TStringList;
    NumBytes   : uint32;

    Token: TToken_P;
    Range: TRange;

  { Separate }
  Procedure Separate(const Input: String; out BeforeSign, AfterSign: String);
  Var P: uint32;
  Begin
   P := Pos('=', Input);

   BeforeSign := Trim(Copy(Input, 1, P-1));
   AfterSign  := Trim(Copy(Input, P+1, Length(Input)));
  End;

  { ParseOutput }
  Procedure ParseOutput;
  Var Line, Param: String;
      Scan       : Boolean = False;

      Namespace: TNamespace;
      TmpSymbol: TSymbol;

      Data, FuncParams: TStringList;

      SymbolName, PropName, PropValue: String;

      mFunc: TFunction;
      mVar : TVariable;

      LineID, ParamID: uint32;
  Begin
   if (Output.Count = 0) Then // no data to parse
    Exit;

   Data := TStringList.Create;

   Try
    For LineID := 0 To Output.Count-1 Do
    Begin
     Line := Trim(Output[LineID]);

     if (Line = 'libinfo begin') Then
     Begin
      Scan := True;
      Continue;
     End Else

     if (Line = 'libinfo end') Then
     Begin
      Break;
     End Else

     if (Line = 'libinfo exception') Then
     Begin
      raise EParserException.CreateFmt('Compiler crashed: %s', [Output[Output.indexOf(Line)+1]]);
     End;

     if (not Scan) Then
      Continue;

     if (Copy(Line, 1, Pos(' ', Line)-1) = 'namespace') Then // change namespace
     Begin
      Delete(Line, 1, Pos(' ', Line));

      Namespace := findNamespace(Line);

      if (Namespace = nil) Then
      Begin
       Namespace := TNamespace.Create(Token, Parser.getCurrentRange, Line, True);
       NamespaceList.Add(Namespace);
      End;
     End Else
     Begin // parse symbol
      if (Namespace = nil) Then
       raise EParserException.Create('Compiler output is corrupted (#1).');

      Data.Delimiter       := ';';
      Data.StrictDelimiter := True;
      Data.DelimitedText   := Line;

      if (Data[0] = 'function') Then
       mFunc := TFunction.Create(Token, Range, '', True);

      // look for properties
      SymbolName := '';

      For Param in Data Do
      Begin
       Separate(Param, PropName, PropValue);

       if (AnsiCompareStr(PropName, 'name') = 0) Then
       Begin
        SymbolName := PropValue;
       End Else

       if (AnsiCompareStr(PropName, 'type') = 0) Then
       Begin
        if (AnsiCompareStr(Copy(PropValue, 1, 6), 'self::') = 0) Then // trim unnecessary "self::"
         Delete(PropValue, 1, 6);

        mVar.Typ := PropValue;
       End Else

       if (AnsiCompareStr(PropName, 'return') = 0) Then
       Begin
        if (AnsiCompareStr(Copy(PropValue, 1, 6), 'self::') = 0) Then // trim unnecessary "self::"
         Delete(PropValue, 1, 6);

        mFunc.ReturnType := PropValue;
       End Else

       if (AnsiCompareStr(PropName, 'parameter_count') = 0) Then
       Begin
        SetLength(mFunc.ParamTypes, StrToInt(PropValue));
       End Else

       if (AnsiCompareStr(PropName, 'parameter_types') = 0) Then
       Begin
        FuncParams := TStringList.Create;

        Try
         FuncParams.Delimiter       := ',';
         FuncParams.StrictDelimiter := True;
         FuncParams.DelimitedText   := PropValue;

         if (FuncParams.Count > 0) Then
         Begin
          For ParamID := 0 To FuncParams.Count-1 Do
          Begin
           PropValue := Trim(FuncParams[ParamID]);

           if (AnsiCompareStr(Copy(PropValue, 1, 6), 'self::') = 0) Then // trim unnecessary "self::"
            Delete(PropValue, 1, 6);

           mFunc.ParamTypes[ParamID] := PropValue;
          End;
         End;
        Finally
         FuncParams.Free;
        End;
       End;
      End;

      if (Length(SymbolName) = 0) Then
       raise EParserException.Create('Compiler output is corrupted (#2)');

      // sync symbol name
      if (Data[0] = 'function') Then
       mFunc.Name := SymbolName;

      if (Data[0] = 'var') Then
       mVar.Name := SymbolName;

      // do not import internal types (bool/char/int (...))
      if (Namespace.getName = 'self') and (isInternalType(SymbolName)) Then
       Continue;

      // check for duplicates
      TmpSymbol := Namespace.findSymbol(SymbolName);

      if (TmpSymbol <> nil) Then
       Namespace.SymbolList.Remove(TmpSymbol);

      // add symbol to the namespace
      Case Data[0] of
       'function': Namespace.SymbolList.Add(TSymbol.Create(stFunction, mFunc));
       'var'     : Namespace.SymbolList.Add(TSymbol.Create(stVariable, mVar));
       'const'   : Namespace.SymbolList.Add(TSymbol.Create(stConstant, TVariable.Create(Token, Range, SymbolName, True)));
       'type'    : Namespace.SymbolList.Add(TSymbol.Create(stType, TType.Create(Token, Range, SymbolName, True)));

       else
        raise EParserException.CreateFmt('Compiled output is corrupted (#3: %s)', [Data[0]]);
      End;
     End;
    End;
   Finally
    Data.Free;
   End;
  End;

Begin
 if (not FileExists(Config.getString(ceCompilerExecutable))) Then // error: compiler executable not found
  raise EParserException.Create('SScript Compiler is required to parse SScript libraries - but no such found.');

 Token := Parser.next(-2);
 Inc(Token.Char);

 Range.PBegin := Token;
 Range.PEnd   := Token;

 OutputChunk[0] := #0; // just for the compiler to stop complaining about "uninitialized variable"

 Process := TProcess.Create(nil);

 Try
  Process.Executable := Config.getString(ceCompilerExecutable);
  Process.Options    := [poUsePipes, poNoConsole];

  // prepare command line
  Process.Parameters.AddStrings
  (
   [
    '"'+FileName+'"',
    '--compile-mode', 'libinfo',
    '-silent'
   ]
  );

  // execute and wait
  Process.Execute;
  While (Process.Running) Do
  Begin
   NumBytes := Process.Output.Read(OutputChunk[0], ChunkSize);

   OutputData += Copy(OutputChunk, 1, NumBytes);
  End;

  // parse output
  Output := TStringList.Create;

  Try
   Output.Delimiter       := #10;
   Output.StrictDelimiter := True;
   Output.DelimitedText   := OutputData;

   ParseOutput;
  Finally
   Output.Free;
  End;
 Finally
  Process.Free;
 End;
End;
