(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
Unit CodeScan;

 Interface
 Uses FGL, Classes, Parser, Tokens;

 Type PIdentifier = ^TIdentifier;
      TIdentifier = Record
                     Line, Char, Len: Integer;
                     FileName       : String;

                     RefLine, RefChar, RefLen: Integer;
                     RefFileName             : String;
                    End;

 Type TIdentifierList = specialize TFPGList<PIdentifier>;

 Type TCodeScanner = Class;

 // forward declarations
 Type TSymbol         = Class;
      TPhysicalSymbol = Class;
      TType           = Class;
      TVariable       = Class;
      TFunction       = Class;
      TNamespace      = Class;

      TSymbolList  = specialize TFPGList<TSymbol>;

 // TSymbol
 Type TSymbolType = (stNamespace, stVariable, stConstant, stFunction, stType);
 Type TSymbol = Class
                 Public
                  Typ: TSymbolType;

                  mNamespace: TNamespace;
                  mType     : TType;
                  mVariable : TVariable;
                  mFunction : TFunction;

                  Constructor Create(fType: TSymbolType; fValue: TPhysicalSymbol);
                  Destructor Destroy; override;

                  Function getPhysSymbol: TPhysicalSymbol;
                  Function getName: String;
                  Function getToken: TToken_P;
                  Function getRange: TRange;
                 End;

 // TPhysicalSymbol
 Type TPhysicalSymbol = Class
                         Public
                          Token: TToken_P;
                          Range: TRange;
                          Name : String;

                          Constructor Create(fToken: TToken_P; fRange: TRange; fName: String);
                         End;

 // TType
 Type TType = Class(TPhysicalSymbol);

 // TVariable
 Type TVariable = Class(TPhysicalSymbol)
                   Public
                    Value: TTokenList;

                    Constructor Create(fToken: TToken_P; fRange: TRange; fName: String);
                   End;

 // TFunction
 Type TFunction = Class(TPhysicalSymbol)
                   Public
                    SymbolList: TSymbolList;

                    Constructor Create(fToken: TToken_P; fRange: TRange; fName: String);
                    Destructor Destroy; override;

                    Function findPhysSymbol(const SymbolName: String): TPhysicalSymbol;
                   End;

 // TNamespace
 Type TNamespace = Class(TPhysicalSymbol)
                    Public
                     SymbolList: TSymbolList;

                     Constructor Create(fToken: TToken_P; fRange: TRange; fName: String);
                     Destructor Destroy; override;

                     Function findPhysSymbol(const SymbolName: String): TPhysicalSymbol;
                    End;

 Type TNamespaceList = specialize TFPGList<TNamespace>;

 // TNamespaceVisibility
 Type TNamespaceVisibility = Class
                              Public
                               Range    : TRange; // range, in which namespaces below are visible
                               Namespace: TNamespace; // namespace class

                               Constructor Create(fRange: TRange; fNamespace: TNamespace);
                              End;

 Type TNamespaceVisibilityList = specialize TFPGList<TNamespaceVisibility>;

 // TParsedFilesMap
 Type TParsedFilesMap = specialize TFPGMap<String, TCodeScanner>;

 { TCodeScanner }
 Type TCodeScanner = Class
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

                       Function findNamespace(const Name: String): TNamespace;
                       Function findFile(const FName: String; out FoundFile: String): Boolean;

                       Procedure ParseInclude;
                       Procedure ParseUse;
                       Procedure ParseNamespace;
                       Procedure ParseType;
                       Procedure ParseFunction;
                       Procedure ParseVar;
                       Procedure ParseConst;

                       Procedure ParseToken;

                       Procedure read_and_mark(TokenUntil: TTokenSet);

                       Procedure AddIdentifier(Identifier: TIdentifier);
                       Procedure AddIdentifier(RefTo: TPhysicalSymbol; Token: TToken_P);

                      Public
                       CurrentlyParsedFile: String;

                       Constructor Create(Code: TStrings; fFileName, fMainFile, fCompilerFile, fSearchPaths: String; const fIsMain: Boolean);
                       Constructor Create(fFileName, fMainFile, fCompilerFile, fSearchPaths: String; const fIsMain: Boolean);
                       Destructor Destroy; override;

                       Function Parse: TIdentifierList;

                       Property getParser: TParser read Parser;
                       Property getNamespaceList: TNamespaceList read NamespaceList;
                       Property getNamespaceVisibilityList: TNamespaceVisibilityList read NamespaceVisibilityList;
                      End;

 Implementation
Uses mLanguages, SysUtils, Dialogs, Variants;

(* TSymbol.Create *)
Constructor TSymbol.Create(fType: TSymbolType; fValue: TPhysicalSymbol);
Begin
 Typ := fType;

 Case Typ of
  stNamespace           : mNamespace := TNamespace(fValue);
  stType                : mType      := TType(fValue);
  stVariable, stConstant: mVariable  := TVariable(fValue);
  stFunction            : mFunction  := TFunction(fValue);

  else
   raise Exception.CreateFmt('TSymbol.Create() -> unknown symbol type: %d', [ord(Typ)]);
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
   raise Exception.CreateFmt('TSymbol.getPhysSymbol() -> unknown symbol type: %d', [ord(Typ)]);
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

// -------------------------------------------------------------------------- //
(* TPhysicalSymbol.Create *)
Constructor TPhysicalSymbol.Create(fToken: TToken_P; fRange: TRange; fName: String);
Begin
 Token := fToken;
 Range := fRange;
 Name  := fName;
End;

// -------------------------------------------------------------------------- //
(* TVariable.Create *)
Constructor TVariable.Create(fToken: TToken_P; fRange: TRange; fName: String);
Begin
 inherited Create(fToken, fRange, fName);

 Value := nil;
End;

// -------------------------------------------------------------------------- //
(* TFunction.Create *)
Constructor TFunction.Create(fToken: TToken_P; fRange: TRange; fName: String);
Begin
 inherited Create(fToken, fRange, fName);

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

(* TFunction.findPhysSymbol *)
Function TFunction.findPhysSymbol(const SymbolName: String): TPhysicalSymbol;
Var Symbol: TSymbol;
Begin
 For Symbol in SymbolList Do
  if (Symbol.getName = SymbolName) Then
   Exit(Symbol.getPhysSymbol);

 Exit(nil);
End;

// -------------------------------------------------------------------------- //
(* TNamespace.Create *)
Constructor TNamespace.Create(fToken: TToken_P; fRange: TRange; fName: String);
Begin
 inherited Create(fToken, fRange, fName);

 SymbolList := TSymbolList.Create;
End;

(* TNamespace.Destroy *)
Destructor TNamespace.Destroy;
Var Symbol: TSymbol;
Begin
 For Symbol in SymbolList Do
  Symbol.Free;
 SymbolList.Free;
End;

(* TNamespace.findPhysSymbol *)
Function TNamespace.findPhysSymbol(const SymbolName: String): TPhysicalSymbol;
Var Symbol: TSymbol;
Begin
 For Symbol in SymbolList Do
  if (Symbol.getName = SymbolName) Then
   Exit(Symbol.getPhysSymbol);

 Exit(nil);
End;

// -------------------------------------------------------------------------- //
(* TNamespaceVisibility.Create *)
Constructor TNamespaceVisibility.Create(fRange: TRange; fNamespace: TNamespace);
Begin
 Range     := fRange;
 Namespace := fNamespace;
End;

// -------------------------------------------------------------------------- //
(* TCodeScanner.findNamespace *)
Function TCodeScanner.findNamespace(const Name: String): TNamespace;
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
  Inc(Parser.CurrentDeep);
  Exit;
 End;

 if (Token.Token = _BRACKET3_CL) Then
 Begin
  Dec(Parser.CurrentDeep);
  Exit;
 End;

 if (Token.Token = _AT) and (Parser.next.Value = 'visibility') Then // `@visibility` macro
 Begin
  With Parser do
  Begin
   read_ident;
   eat(_BRACKET1_OP);
   read_string;
   eat(_BRACKET1_CL);
  End;

  Exit;
 End;

 if (inFunction) Then
 Begin
  { inside function }
  // everything inside function is parsed in `read_and_mark`
 End Else
 Begin
  { outside function }
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
    raise EParserError.CreateFmt(getLangValue(ls_unexpected), [VarToStr(Token.Value)]);
  End;
 End;
End;

(* TCodeScanner.read_and_mark *)
Procedure TCodeScanner.read_and_mark(TokenUntil: TTokenSet);
Var BrDeep: Integer;
    Token : TToken_P;

    Symbol      : TPhysicalSymbol;
    Namespace   : TNamespace;
    NamespaceVis: TNamespaceVisibility;
Begin
 BrDeep := 0;

 Repeat
  Token := Parser.read;
  if (BrDeep < 0) Then
   Exit;

  Case Token.Token of
   _BRACKET1_OP, _BRACKET2_OP, _BRACKET3_OP: Inc(BrDeep);
   _BRACKET1_CL, _BRACKET2_CL, _BRACKET3_CL: Dec(BrDeep);

   { identifier }
   _IDENTIFIER:
   Begin
    Symbol := nil;

    if (Parser.next_t = _DOUBLE_COLON) and (Parser.next_t(2) = _IDENTIFIER) Then { if next is `::`, we have a namespace reference, like `std::newline()` }
    Begin
     Parser.eat(_DOUBLE_COLON); // `::`

     Namespace := findNamespace(Token.Value);
     if (Namespace <> nil) Then
     Begin
      AddIdentifier(Namespace, Parser.next(-2));

      Symbol := Namespace.findPhysSymbol(Parser.read_ident);

      if (Symbol <> nil) Then
       AddIdentifier(Symbol, Parser.next(-1));
     End Else
      Parser.eat(_IDENTIFIER);
    End Else
    Begin
     if (inFunction) Then
      Symbol := CurrentFunction.findPhysSymbol(Token.Value);

     if (Symbol = nil) Then
      Symbol := CurrentNamespace.findPhysSymbol(Token.Value);

     if (Symbol = nil) Then
     Begin
      For NamespaceVis in NamespaceVisibilityList Do
      Begin
       if (Parser.next(-1) in NamespaceVis.Range) Then
        Symbol := NamespaceVis.Namespace.findPhysSymbol(Token.Value);

       if (Symbol <> nil) Then // symbol found!
        Break;
      End;
     End;

     AddIdentifier(Symbol, Parser.next(-1));
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
Procedure TCodeScanner.AddIdentifier(Identifier: TIdentifier);
Var Ident: PIdentifier;
Begin
 New(Ident);
 Ident^ := Identifier;
 IdentifierList.Add(Ident);
End;

(* TCodeScanner.AddIdentifier *)
Procedure TCodeScanner.AddIdentifier(RefTo: TPhysicalSymbol; Token: TToken_P);
Var Ident: TIdentifier;
Begin
 if (RefTo = nil) Then
  Exit;

 Ident.Char        := Token.Char;
 Ident.Line        := Token.Line;
 Ident.Len         := Length(Token.Value);
 Ident.FileName    := Token.FileName;
 Ident.RefChar     := RefTo.Token.Char;
 Ident.RefLine     := RefTo.Token.Line;
 Ident.RefLen      := Length(RefTo.Token.Value);
 Ident.RefFileName := RefTo.Token.FileName;

 AddIdentifier(Ident);
End;

// -------------------------------------------------------------------------- //
(* TCodeScanner.Create *)
Constructor TCodeScanner.Create(Code: TStrings; fFileName, fMainFile, fCompilerFile, fSearchPaths: String; const fIsMain: Boolean);
Var I  : Integer;
    Str: String;
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

 For I := 0 To SearchPaths.Count-1 Do
 Begin
  Str := SearchPaths[I];

  Str := StringReplace(Str, '$file', ExtractFilePath(fFileName), [rfReplaceAll]);
  Str := StringReplace(Str, '$main', ExtractFilePath(fMainFile), [rfReplaceAll]);
  Str := StringReplace(Str, '$compiler', ExtractFilePath(fCompilerFile), [rfReplaceAll]);

  SearchPaths[I] := Str;
 End;
End;

(* TCodeScanner.Create *)
Constructor TCodeScanner.Create(fFileName, fMainFile, fCompilerFile, fSearchPaths: String; const fIsMain: Boolean);
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
Var Ident: PIdentifier;
    NSV  : TNamespaceVisibility;
    NS   : TNamespace;
Begin
 { IdentifierList }
 For Ident in IdentifierList Do
  Dispose(Ident);
 IdentifierList.Free;

 { NamespaceList }
 For NS in NamespaceList Do
  NS.Free;
 NamespaceList.Free;

 { NamespaceVisibilityList }
 For NSV in NamespaceVisibilityList Do
  NSV.Free;
 NamespaceVisibilityList.Free;

 { Parser }
 Parser.Free;

 { Included files }
 if (isMain) Then
 Begin
  //For I := 0 To ParsedFiles.Count-1 Do
  // if (ParsedFiles.Data[I] <> nil) and (ParsedFiles.Data[I] <> self) Then
  //  TCodeScanner(ParsedFiles.Data[I]).Free; // @TODO: WHY THE FUCK THIS DOES NOT WORK?!

  ParsedFiles.Free;
 End;
End;

(* TCodeScanner.Parse *)
Function TCodeScanner.Parse: TIdentifierList;
Begin
 if (ParsedFiles = nil) Then
  raise Exception.Create('ParsedFiles = nil, this was not supposed to happen!');

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
