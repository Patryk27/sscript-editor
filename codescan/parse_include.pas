(* isZipFile *)
Function isZipFile(const FileName: String): Boolean;
Var Stream: TFileStream;
    Header: Array[0..3] of uint8;
    I     : uint8;
Begin
 Stream := TFileStream.Create(FileName, fmOpenRead);

 Try
  For I := Low(Header) To High(Header) Do
   Header[I] := Stream.ReadByte;

  Result := (Header[0] = $50) and (Header[1] = $4B) and
            (Header[2] = $03) and (Header[3] = $04);
 Finally
  Stream.Free;
 End;
End;

// -------------------------------------------------------------------------- //
(* TCodeScanner.ParseInclude *)
Procedure TCodeScanner.ParseInclude;
Var FileName, RealFile     : String;
    CodeScanner            : TCodeScanner;
    Namespace, TmpNamespace: TNamespace;
    Symbol                 : TSymbol;

    Prev: String;
Begin
 With Parser do
 Begin
  eat(_BRACKET1_OP); // `(`
  FileName := read_string;
  eat(_BRACKET1_CL); // `)`

  if (not findFile(FileName, RealFile)) Then // if file not found
   raise EParserError.CreateFmt(getLangValue(ls_unknown_file), [FileName]);

  if (ParsedFiles.indexOf(RealFile) > -1) Then // file has been already parsed
   Exit;

  if (isZipFile(FileName)) Then // unsupported for now :< - @TODO
   Exit;

  Prev                    := CurrentlyParsedFile;
  CurrentlyParsedFile     := RealFile;
  CodeScanner             := TCodeScanner.Create(RealFile, MainFile, CompilerFile, SearchPathsStr, False);
  CodeScanner.ParsedFiles := self.ParsedFiles;

  CodeScanner.Parse;

  For Namespace in CodeScanner.NamespaceList Do
  Begin
   TmpNamespace := findNamespace(Namespace.Name);

   if (TmpNamespace = nil) Then
   Begin
    // new namespace
    NamespaceList.Add(Namespace);
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
