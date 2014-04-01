(*
 Copyright © by Patryk Wychowaniec, 2013-2014
 All rights reserved.
*)
{$H+}
{$MODESWITCH ADVANCEDRECORDS}
Unit CodeScan;

 Interface
 Uses FGL, Classes, Parser, Tokens;

 { TIdentifier }
 Type TIdentifier =
      Record
       Line, Char, Len: uint32;
       FileName       : String;

       RefLine, RefChar, RefLen: uint32;
       RefFileName             : String;

       Class Operator = (const A, B: TIdentifier): Boolean;
      End;

 { TIdentifierList }
 Type TIdentifierList = specialize TFPGList<TIdentifier>;

 { TFunctionParameterList }
 Type TFunctionParameterList = Array of String;

 // forward declarations
 Type TCodeScanner    = Class;
      TSymbol         = Class;
      TPhysicalSymbol = Class;
      TType           = Class;
      TVariable       = Class;
      TFunction       = Class;
      TNamespace      = Class;

      TSymbolList  = specialize TFPGList<TSymbol>;

 { TSymbolType }
 Type TSymbolType = (stNamespace, stVariable, stConstant, stFunction, stType);

 { TSymbol }
 Type TSymbol =
      Class
       Public
        Typ: TSymbolType;

        mNamespace: TNamespace;
        mType     : TType;
        mVariable : TVariable;
        mFunction : TFunction;

       Public
        Constructor Create(const fType: TSymbolType; const fValue: TPhysicalSymbol);
        Destructor Destroy; override;

        Function getPhysSymbol: TPhysicalSymbol;
        Function getName: String;
        Function getToken: TToken_P;
        Function getRange: TRange;
       End;

 { TPhysicalSymbol }
 Type TPhysicalSymbol =
      Class
       Private
        Name : String;
        Token: TToken_P;
        Range: TRange;

        LibrarySymbol: Boolean;

       Private
        Procedure SymbolCreate; virtual; // called by TPhysicalSymbol.Create() at the end of routine

       Public
        Constructor Create(const fToken: TToken_P; const fRange: TRange; const fName: String; const fLibrarySymbol: Boolean=False);

       Public
        Property getName: String read Name;
        Property getToken: TToken_P read Token;
        Property getRange: TRange read Range;
        Property isLibrarySymbol: Boolean read LibrarySymbol;
       End;

 { TType }
 Type TType = Class(TPhysicalSymbol);

 { TVariable }
 Type TVariable =
      Class(TPhysicalSymbol)
       Private
        Procedure SymbolCreate; override;

       Private
        Value: String;
        Typ  : String;

       Public
        Property getValue: String read Value;
        Property getType: String read Typ;
       End;

 { TFunction }
 Type TFunction =
      Class(TPhysicalSymbol)
       Private
        SymbolList: TSymbolList;

        ReturnType: String;
        ParamTypes: TFunctionParameterList;

       Private
        Procedure SymbolCreate; override;

       Public
        Destructor Destroy; override;

        Function findSymbol(const SymbolName: String): TSymbol;
        Function findPhysSymbol(const SymbolName: String): TPhysicalSymbol;

        Function getParamTypesAsString: String;

       Public
        Property getSymbolList: TSymbolList read SymbolList;
        Property getReturnType: String read ReturnType;
        Property getParamTypes: TFunctionParameterList read ParamTypes;
       End;

 { TNamespace }
 Type TNamespace =
      Class(TPhysicalSymbol)
       Private
        SymbolList: TSymbolList;

       Private
        Procedure SymbolCreate; override;

       Public
        DisposeSymbols: Boolean;

       Public
        Destructor Destroy; override;

        Function findSymbol(const SymbolName: String): TSymbol;
        Function findPhysSymbol(const SymbolName: String): TPhysicalSymbol;

       Public
        Property getSymbolList: TSymbolList read SymbolList;
       End;

 Type TNamespaceList = specialize TFPGList<TNamespace>;

 { TNamespaceVisibility }
 Type TNamespaceVisibility =
      Class
       Public
        Range    : TRange; // range in which namespaces below are visible
        Namespace: TNamespace; // namespace class

       Public
        Constructor Create(const fRange: TRange; const fNamespace: TNamespace);
       End;

 Type TNamespaceVisibilityList = specialize TFPGList<TNamespaceVisibility>;

 { TParsedFilesMap }
 Type TParsedFilesMap = specialize TFPGMap<String, TCodeScanner>;

 { TCodeScanner }
 Type TCodeScanner =
      Class
       Private
        Parser        : TParser;
        SearchPaths   : TStringList;
        SearchPathsStr: String;
        IdentifierList: TIdentifierList;

        ParsedFile, MainFile, CompilerFile: String;

        isMain: Boolean;

        inFunction   : Boolean;
        NamespaceList: TNamespaceList;

        DefaultNamespace, CurrentNamespace: TNamespace;
        NamespaceVisibilityList           : TNamespaceVisibilityList;
        CurrentFunction                   : TFunction;

        ParsedFiles: TParsedFilesMap;

       Private
        Function findNamespace(const Name: String): TNamespace;
        Function findFile(const FName: String; out FoundFile: String): Boolean;

        Procedure ParseInclude;
        Procedure ParseZipInclude(const FileName: String);
        Procedure ParseUse;
        Procedure ParseNamespace;
        Procedure ParseType;
        Procedure ParseFunction;
        Procedure ParseVar;
        Procedure ParseConst;

        Procedure ParseToken;

        Function findSymbol(const SymbolName: String): TPhysicalSymbol;
        Function findSymbol(const NamespaceName, SymbolName: String): TPhysicalSymbol;

        Function read_type: String;
        Function read_and_mark(const TokenUntil: TTokenSet): String;

        Procedure AddIdentifier(const Identifier: TIdentifier);
        Procedure AddIdentifier(const RefTo: TPhysicalSymbol; const Token: TToken_P);

       Public
        CurrentlyParsedFile: String;

       Public
        Constructor Create(const Code: TStrings; const fFileName, fMainFile, fCompilerFile, fSearchPaths: String; const fIsMain: Boolean);
        Constructor Create(const fFileName, fMainFile, fCompilerFile, fSearchPaths: String; const fIsMain: Boolean);
        Destructor Destroy; override;

        Function Parse: TIdentifierList;

       Public
        Property getParser: TParser read Parser;
        Property getNamespaceList: TNamespaceList read NamespaceList;
        Property getNamespaceVisibilityList: TNamespaceVisibilityList read NamespaceVisibilityList;
       End;

 Implementation
Uses mLanguages, mConfiguration, mFunctions, SysUtils, Dialogs, Variants, Process;

(* TIdentifier = TIdentifier *)
Class Operator TIdentifier.=(const A, B: TIdentifier): Boolean;
Begin
 Result := False;
End;

// -------------------------------------------------------------------------- //
(* TSymbol.Create *)
Constructor TSymbol.Create(const fType: TSymbolType; const fValue: TPhysicalSymbol);
Begin
 Typ := fType;

 Case Typ of
  stNamespace           : mNamespace := TNamespace(fValue);
  stType                : mType      := TType(fValue);
  stVariable, stConstant: mVariable  := TVariable(fValue);
  stFunction            : mFunction  := TFunction(fValue);

  else
   raise EParserException.CreateFmt('TSymbol.Create() -> unknown symbol type: %d', [ord(Typ)]);
 End;
End;

(* TSymbol.Destroy *)
Destructor TSymbol.Destroy;
Begin
 getPhysSymbol.Free;
End;

(* TSymbol.getPhysSymbol *)
Function TSymbol.getPhysSymbol: TPhysicalSymbol;
Begin
 Case Typ of
  stNamespace           : Result := mNamespace;
  stType                : Result := mType;
  stVariable, stConstant: Result := mVariable;
  stFunction            : Result := mFunction;

  else
   raise EParserException.CreateFmt('TSymbol.getPhysSymbol() -> unknown symbol type: %d', [ord(Typ)]);
 End;
End;

(* TSymbol.getName *)
Function TSymbol.getName: String;
Begin
 Exit(getPhysSymbol.Name);
End;

(* TSymbol.getToken *)
Function TSymbol.getToken: TToken_P;
Begin
 Exit(getPhysSymbol.Token);
End;

(* TSymbol.getRange *)
Function TSymbol.getRange: TRange;
Begin
 Exit(getPhysSymbol.Range);
End;

(* TPhysicalSymbol.SymbolCreate *)
Procedure TPhysicalSymbol.SymbolCreate;
Begin
 // dummy
End;

// -------------------------------------------------------------------------- //
(* TPhysicalSymbol.Create *)
Constructor TPhysicalSymbol.Create(const fToken: TToken_P; const fRange: TRange; const fName: String; const fLibrarySymbol: Boolean);
Begin
 Token         := fToken;
 Range         := fRange;
 Name          := fName;
 LibrarySymbol := fLibrarySymbol;

 SymbolCreate;
End;

// -------------------------------------------------------------------------- //
(* TVariable.SymbolCreate *)
Procedure TVariable.SymbolCreate;
Begin
End;

// -------------------------------------------------------------------------- //
(* TFunction.SymbolCreate *)
Procedure TFunction.SymbolCreate;
Begin
 SymbolList := TSymbolList.Create;
End;

(* TFunction.Destroy *)
Destructor TFunction.Destroy;
Var Symbol: TSymbol;
Begin
 For Symbol in SymbolList Do
  Symbol.Free;
 SymbolList.Free;
End;

(* TFunction.findSymbol *)
Function TFunction.findSymbol(const SymbolName: String): TSymbol;
Begin
 For Result in SymbolList Do
  if (Result.getName = SymbolName) Then
   Exit;

 Exit(nil);
End;

(* TFunction.findPhysSymbol *)
Function TFunction.findPhysSymbol(const SymbolName: String): TPhysicalSymbol;
Var Symbol: TSymbol;
Begin
 Symbol := findSymbol(SymbolName);

 if (Symbol = nil) Then
  Result := nil Else
  Result := Symbol.getPhysSymbol;
End;

(* TFunction.getParamTypesAsString *)
Function TFunction.getParamTypesAsString: String;
Var I: int32;
Begin
 Result := '';

 For I := 0 To High(ParamTypes) Do
 Begin
  Result += ParamTypes[I];

  if (I <> High(ParamTypes)) Then
   Result += ', ';
 End;
End;

// -------------------------------------------------------------------------- //
(* TNamespace.SymbolCreate *)
Procedure TNamespace.SymbolCreate;
Begin
 SymbolList     := TSymbolList.Create;
 DisposeSymbols := True;
End;

(* TNamespace.Destroy *)
Destructor TNamespace.Destroy;
Var Symbol: TSymbol;
Begin
 if (DisposeSymbols) Then
 Begin
  For Symbol in SymbolList Do
   Symbol.Free;
  SymbolList.Free;
 End;
End;

(* TNamespace.findSymbol *)
Function TNamespace.findSymbol(const SymbolName: String): TSymbol;
Begin
 For Result in SymbolList Do
  if (Result.getName = SymbolName) Then
   Exit;

 Exit(nil);
End;

(* TNamespace.findPhysSymbol *)
Function TNamespace.findPhysSymbol(const SymbolName: String): TPhysicalSymbol;
Var Symbol: TSymbol;
Begin
 Symbol := findSymbol(SymbolName);

 if (Symbol = nil) Then
  Result := nil Else
  Result := Symbol.getPhysSymbol;
End;

// -------------------------------------------------------------------------- //
(* TNamespaceVisibility.Create *)
Constructor TNamespaceVisibility.Create(const fRange: TRange; const fNamespace: TNamespace);
Begin
 Range     := fRange;
 Namespace := fNamespace;
End;

// -------------------------------------------------------------------------- //
(* TCodeScanner.findNamespace *)
Function TCodeScanner.findNamespace(const Name: String):TNamespace;
Var Symbol: TNamespace;
Begin
 For Symbol in NamespaceList Do
  if (Symbol.Name = Name) Then
   Exit(Symbol);

 Exit(nil);
End;

(* TCodeScanner.findFile *)
Function TCodeScanner.findFile(const FName: String; out FoundFile: String): Boolean;
Var Path: String;
Begin
 For Path in SearchPaths Do
 Begin
  if (Length(Path) > 0) Then
   if not (Path[Length(Path)] in ['\', '/']) Then
    Path += DirectorySeparator;

  if (FileExists(Path+FName)) Then
  Begin
   FoundFile := Path+FName;
   Exit(True);
  End;
 End;

 Exit(False);
End;

{$I parse_include.pas}
{$I parse_zip_include.pas}
{$I parse_use.pas}
{$I parse_namespace.pas}
{$I parse_type.pas}
{$I parse_function.pas}
{$I parse_var.pas}

(* TCodeScanner.ParseToken *)
Procedure TCodeScanner.ParseToken;
Var Token: TToken_P;
Begin
 Token := Parser.read;

 if (Token.Token = _EOF) Then
  Exit;

 if (Token.Token = _BRACKET3_OP) Then
 Begin
  Parser.IncCurrentDeep;
  Exit;
 End;

 if (Token.Token = _BRACKET3_CL) Then
 Begin
  Parser.DecCurrentDeep;
  Exit;
 End;

 if (inFunction) Then
 Begin
  { in local scope }
  // everything located inside a function is parsed in `read_and_mark`
 End Else
 Begin
  { in global scope }
  Case Token.Token of
   _AT       : ParseInclude;
   _USE      : ParseUse;
   _FUNCTION : ParseFunction;
   _NAMESPACE: ParseNamespace;
   _TYPE     : ParseType;
   _VAR      : ParseVar;
   _CONST    : ParseConst;

   _PUBLIC, _PRIVATE:; // @TODO (?)

   else
    raise EParserException.Create(Language.getText(ls_unexpected, [VarToStr(Token.Value)]));
  End;
 End;
End;

(* TCodeScanner.findSymbol *)
Function TCodeScanner.findSymbol(const SymbolName: String): TPhysicalSymbol;
Var NamespaceVis: TNamespaceVisibility;
Begin
 Result := nil;

 if (inFunction) Then
  Result := CurrentFunction.findPhysSymbol(SymbolName);

 if (Result = nil) Then
  Result := CurrentNamespace.findPhysSymbol(SymbolName);

 if (Result = nil) Then
 Begin
  For NamespaceVis in NamespaceVisibilityList Do
  Begin
   if (Parser.next(-1) in NamespaceVis.Range) Then
   Begin
    Result := NamespaceVis.Namespace.findPhysSymbol(SymbolName);

    if (Result <> nil) Then // symbol found!
     Break;
   End;
  End;
 End;
End;

(* TCodeScanner.findSymbol *)
Function TCodeScanner.findSymbol(const NamespaceName, SymbolName: String): TPhysicalSymbol;
Var Namespace: TNamespace;
Begin
 Namespace := findNamespace(NamespaceName);

 if (Namespace = nil) Then
  Result := nil Else
  Result := Namespace.findPhysSymbol(SymbolName);
End;

(* TCodeScanner.read_type *)
Function TCodeScanner.read_type: String;
Var NSName, IdentName: String;
    Token            : TToken_P;
Begin
 Result := '';
 Token  := Parser.read;

 { type name }
 if (Token.Token = _IDENTIFIER) Then
 Begin
  // namespace name::type name
  if (Parser.next_t = _DOUBLE_COLON) Then
  Begin
   Parser.eat(_DOUBLE_COLON);

   NSName    := Token.Value;
   IdentName := Parser.read_ident;

   AddIdentifier(findSymbol(NSName, IdentName), Token);
   Result := NSName+'::'+IdentName;
  End Else

  // type name
  Begin
   IdentName := Token.Value;

   AddIdentifier(findSymbol(IdentName), Token);
   Result := IdentName;
  End;
 End Else

 { function / function<return type>(parameter list) }
 if (Token.Token = _FUNCTION) Then
 Begin
  Result := 'function';

  if (Parser.next_t = _LOWER) Then
  Begin
   Parser.eat(_LOWER);
   read_type();
   Parser.eat(_GREATER);

   if (Parser.next_t <> _BRACKET1_OP) Then
    Parser.eat(_BRACKET1_OP);
   read_and_mark([_BRACKET1_CL]);
  End;
 End Else

 { unexpected token }
  raise EParserException.Create(Language.getText(ls_unexpected, [VarToStr(Token.Value)]));

 // read optional array modifier
 With Parser do
 Begin
  While (next_t = _BRACKET2_OP) Do
  Begin
   eat(_BRACKET2_OP);
   eat(_BRACKET2_CL);

   Result += '[]';
  End;
 End;
End;

(* TCodeScanner.read_and_mark *)
Function TCodeScanner.read_and_mark(const TokenUntil: TTokenSet): String;
Var BrDeep: Integer;
    Token : TToken_P;

    NamespaceName, IdentifierName: String;
Begin
 Result := '';
 BrDeep := 0;

 Repeat
  Token := Parser.read;

  if (BrDeep < 0) Then
   Exit;

  if (Token.Token in TokenUntil) and (Token.Token in [_BRACKET1_OP, _BRACKET2_OP, _BRACKET3_OP, _BRACKET1_CL, _BRACKET2_CL, _BRACKET3_CL]) and (BrDeep = 0) Then
   Exit;

  if (Token.Value <> null) and (not (Token.Token in TokenUntil)) Then
  Begin
   if (Token.Token = _STRING) Then
    Result += '"'+VarToStr(Token.Value)+'"' Else
    Result += VarToStr(Token.Value);
  End;

  Case Token.Token of
   _BRACKET1_OP, _BRACKET2_OP, _BRACKET3_OP: Inc(BrDeep);
   _BRACKET1_CL, _BRACKET2_CL, _BRACKET3_CL: Dec(BrDeep);

   { identifier }
   _IDENTIFIER:
   Begin
    if (Parser.next_t = _DOUBLE_COLON) and (Parser.next_t(1) = _IDENTIFIER) Then { if next is `::`, we have a namespace reference, like `std::newline()` }
    Begin
     Parser.eat(_DOUBLE_COLON); // `::`

     NamespaceName  := Token.Value;
     IdentifierName := Parser.read_ident;

     AddIdentifier(findNamespace(NamespaceName), Parser.next(-3));
     AddIdentifier(findSymbol(NamespaceName, IdentifierName), Parser.next(-1));
    End Else
    Begin
     AddIdentifier(findSymbol(Token.Value), Parser.next(-1));
    End;
   End;

   { type }
   _TYPE: ParseType;

   { var }
   _VAR: ParseVar;

   { const }
   _CONST: ParseConst;

   { use }
   _USE: ParseUse;
  End;

  if (Token.Token in TokenUntil) and (BrDeep = 0) Then
   Exit;
 Until (False);
End;

(* TCodeScanner.AddIdentifier *)
Procedure TCodeScanner.AddIdentifier(const Identifier: TIdentifier);
Begin
 IdentifierList.Add(Identifier);
End;

(* TCodeScanner.AddIdentifier *)
Procedure TCodeScanner.AddIdentifier(const RefTo: TPhysicalSymbol; const Token: TToken_P);
Var Identifier: TIdentifier;
Begin
 if (RefTo = nil) Then
  Exit;

 Identifier.Char        := Token.Char;
 Identifier.Line        := Token.Line;
 Identifier.Len         := Length(Token.Value);
 Identifier.FileName    := Token.FileName;

 Identifier.RefChar     := RefTo.Token.Char;
 Identifier.RefLine     := RefTo.Token.Line;
 Identifier.RefLen      := Length(RefTo.Token.Value);
 Identifier.RefFileName := RefTo.Token.FileName;

 AddIdentifier(Identifier);
End;

// -------------------------------------------------------------------------- //
(* TCodeScanner.Create *)
Constructor TCodeScanner.Create(const Code: TStrings; const fFileName, fMainFile, fCompilerFile, fSearchPaths: String; const fIsMain: Boolean);
Var Str: String;
    I  : uint16;
Begin
 Parser         := TParser.Create(TStringList(Code), fFileName);
 ParsedFile     := fFileName;
 MainFile       := fMainFile;
 CompilerFile   := fCompilerFile;
 SearchPathsStr := fSearchPaths;

 isMain := fIsMain;
 if (isMain) Then
  ParsedFiles := TParsedFilesMap.Create Else
  ParsedFiles := nil; // should be set manually then

 SearchPaths               := TStringList.Create;
 SearchPaths.Delimiter     := ';';
 SearchPaths.DelimitedText := SearchPathsStr;

 if (SearchPaths.Count > 0) Then
 Begin
  For I := 0 To SearchPaths.Count-1 Do
  Begin
   Str := SearchPaths[I];

   Str := StringReplace(Str, '$file', ExtractFilePath(fFileName), [rfReplaceAll]);
   Str := StringReplace(Str, '$main', ExtractFilePath(fMainFile), [rfReplaceAll]);
   Str := StringReplace(Str, '$compiler', ExtractFilePath(fCompilerFile), [rfReplaceAll]);

   SearchPaths[I] := Str;
  End;
 End;
End;

(* TCodeScanner.Create *)
Constructor TCodeScanner.Create(const fFileName, fMainFile, fCompilerFile, fSearchPaths: String; const fIsMain: Boolean);
Var Code: TStringList;
Begin
 Code := TStringList.Create;
 Code.LoadFromFile(fFileName);
 Try
  Create(Code, fFileName, fMainFile, fCompilerFile, fSearchPaths, fIsMain);
 Finally
  Code.Free;
 End;
End;

(* TCodeScanner.Destroy *)
Destructor TCodeScanner.Destroy;
Var NSV: TNamespaceVisibility;
    NS : TNamespace;
Begin
 // identifier list
 IdentifierList.Free;

 // namespace list
 For NS in NamespaceList Do
  NS.Free;
 NamespaceList.Free;

 // namespace visibility list
 For NSV in NamespaceVisibilityList Do
  NSV.Free;
 NamespaceVisibilityList.Free;

 // parser
 Parser.Free;

 // included files
 if (isMain) Then
 Begin
  //For I := 0 To ParsedFiles.Count-1 Do
  // if (ParsedFiles.Data[I] <> nil) and (ParsedFiles.Data[I] <> self) Then
  //  TCodeScanner(ParsedFiles.Data[I]).Free; // @TODO

  ParsedFiles.Free;
 End;
End;

(* TCodeScanner.Parse *)
Function TCodeScanner.Parse: TIdentifierList;
Begin
 if (ParsedFiles = nil) Then
  raise EParserException.Create('ParsedFiles = nil, this was not supposed to happen!');

 CurrentlyParsedFile := ParsedFile;

 IdentifierList := TIdentifierList.Create;
 Result         := IdentifierList;

 NamespaceVisibilityList := TNamespaceVisibilityList.Create;
 NamespaceList           := TNamespaceList.Create;

 if (Parser.getTokenList.Count = 0) Then
  Exit;

 DefaultNamespace := TNamespace.Create(Parser.next, Parser.getCurrentRange, 'self');
 NamespaceList.Add(DefaultNamespace);

 CurrentNamespace := DefaultNamespace;
 NamespaceVisibilityList.Add(TNamespaceVisibility.Create(Parser.getCurrentRange, DefaultNamespace));

 inFunction      := False;
 CurrentFunction := nil;

 NamespaceVisibilityList.Add(TNamespaceVisibility.Create(Parser.getCurrentRange, DefaultNamespace));

 While (Parser.Can) Do
  ParseToken;
End;
End.
