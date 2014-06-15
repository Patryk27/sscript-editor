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
Var CodeScanner, CacheScanner: TCodeScanner;

    FileName, RealFile, Prev: String;

    Namespace, TmpNamespace: TNamespace;
    Symbol                 : TSymbol;
Begin
 CodeScanner  := nil;
 CacheScanner := nil;

 With Parser do
 Begin
  eat(_BRACKET1_OP); // `(`
  FileName := read_string;
  eat(_BRACKET1_CL); // `)`

  // check for file existence
  if (not findFile(FileName, RealFile)) Then
   raise EParserException.Create(Language.getText(ls_unknown_file, [FileName]));

  // if already parsed
  if (ParsedFiles.indexOf(RealFile) > -1) Then
   Exit;

  // check in cache
  if (CSCache <> nil) Then
  Begin
   CacheScanner := TCodeScanner(CSCache.getCodeScanner(RealFile));
  End;

  Prev := CurrentlyParsedFile;

  // not found in cache
  if (CacheScanner = nil) or (CacheScanner = self) Then
  Begin
   // if zip file
   if (isZipFile(RealFile)) Then
   Begin
    CodeScanner := ParseZipInclude(RealFile);
   End Else
   Begin
    CurrentlyParsedFile     := RealFile;
    CodeScanner             := TCodeScanner.Create(RealFile, MainFile, CompilerFile, IncludePathsStr, False);
    CodeScanner.ParsedFiles := self.ParsedFiles;

    CodeScanner.EnableCache(CSCache);
    CodeScanner.Parse;
   End;
  End Else
  Begin
   // found in cache
   CodeScanner := CacheScanner;
  End;

  For Namespace in CodeScanner.NamespaceList Do
  Begin
   TmpNamespace := findNamespace(Namespace.Name);

   if (TmpNamespace = nil) Then
   Begin
    // new namespace
    NamespaceList.Add(Namespace.Clone());
   End Else
   Begin
    // namespace extends another one
    For Symbol in Namespace.SymbolList Do
    Begin
     TmpNamespace.SymbolList.Add(Symbol.Clone());
    End;
   End;
  End;

  ParsedFiles.Add(RealFile, CodeScanner);

  CurrentlyParsedFile := Prev;
 End;
End;
