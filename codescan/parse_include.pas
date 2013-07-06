(* TCodeScanner.ParseInclude *)
Procedure TCodeScanner.ParseInclude;
Var FileName, RealFile     : String;
    CodeScanner            : TCodeScanner;
    Namespace, TmpNamespace: TNamespace;
    NSSymbol, Symbol       : TSymbol;

    Prev: String;
Begin
 With Parser do
 Begin
  eat(_BRACKET1_OP); // `(`
  FileName := read_string;
  eat(_BRACKET1_CL); // `)`

  if (not findFile(FileName, RealFile)) Then // if file not found
   raise EParserError.CreateFmt(getLangValue(ls_unknown_file), [FileName]);

  if (ParsedFiles.IndexOf(RealFile) > -1) Then // file has been already parsed
   Exit;

  Prev                    := CurrentlyParsedFile;
  CurrentlyParsedFile     := RealFile;
  CodeScanner             := TCodeScanner.Create(RealFile, MainFile, CompilerFile, SearchPathsStr, False);
  CodeScanner.ParsedFiles := self.ParsedFiles;

  CodeScanner.Parse;

  For NSSymbol in CodeScanner.SymbolList Do
   if (NSSymbol.Typ = stNamespace) Then
   Begin
    Namespace := NSSymbol.mNamespace;

    TmpNamespace := findNamespace(Namespace.Name);

    if (TmpNamespace = nil) Then
    Begin
     // new namespace
     SymbolList.Add(TSymbol.Create(stNamespace, Namespace));
    End Else
    Begin
     // namespace extends another one
     For Symbol in Namespace.SymbolList Do
      TmpNamespace.SymbolList.Add(Symbol);
    End;
   End;

  ParsedFiles.Add(RealFile, CodeScanner);

  CurrentlyParsedFile := Prev;
 End;
End;
