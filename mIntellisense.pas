(*
 Copyright © by Patryk Wychowaniec, 2013-2014
 All rights reserved.
*)
Unit mIntellisense;

 Interface
 Uses Classes, CodeScanner;

 Const IntellisenseDelimiter = #10;

 Procedure MakeIntellisenseIdentifierList(const Result: TStrings; const NamespaceList: TNamespaceList; const ReachableNamespaces: TStringList; const FunctionName, Identifier: String);

 Implementation
Uses mLogger, SysUtils, Dialogs;

Const TypeNames: Array[TSymbolType] of String = ('namespace', 'var', 'const', 'function', 'type');

(* MakeIntellisenseIdentifierList *)
Procedure MakeIntellisenseIdentifierList(const Result: TStrings; const NamespaceList: TNamespaceList; const ReachableNamespaces: TStringList; const FunctionName, Identifier: String);

  { TryToAdd }
  Procedure TryToAdd(const SymbolName, WholeData: String);
  Begin
   if (Length(Identifier) = 0) or (Copy(SymbolName, 1, Length(Identifier)) = Identifier) Then
   Begin
    if (Result.indexOf(WholeData) = -1) Then
     Result.Add(WholeData);
   End;
  End;

  { ParseSymbol }
  Procedure ParseSymbol(const Parent, Symbol: TSymbol);
  Var Name: String;
      Tmp : TSymbol;

      mFunc: TFunction;
      mVar : TVariable;
  Begin
   if (Parent <> nil) Then
   Begin
    if (Parent.Typ = stFunction) and (Parent.getName <> FunctionName) Then // not our function
     Exit;
   End;

   Name := Format('%s%s', [TypeNames[Symbol.Typ], IntellisenseDelimiter]);

   // add "global/local" informant
   if (Symbol.Typ in [stVariable, stConstant, stType]) Then
   Begin
    if (Parent = nil) or (Parent.Typ = stNamespace) Then
     Name += 'global ' Else
     Name += 'local ';
   End;

   Name += Format('%s%s%s', [TypeNames[Symbol.Typ], IntellisenseDelimiter, Symbol.getName]);

   // special: variable
   if (Symbol.Typ = stVariable) Then
   Begin
    mVar := Symbol.mVariable;

    Name += Format('%s%s', [IntellisenseDelimiter, mVar.getType]);
   End Else

   // special: const
   if (Symbol.Typ = stConstant) Then
   Begin
    mVar := Symbol.mVariable;

    Name += Format('%s%s', [IntellisenseDelimiter, mVar.getValue]);
   End Else

   // special: function
   if (Symbol.Typ = stFunction) Then
   Begin
    mFunc := Symbol.mFunction;

    Name += Format('%s%s%s(%s)', [IntellisenseDelimiter, mFunc.getReturnType, IntellisenseDelimiter, mFunc.getParamTypesAsString]);
   End;

   // if in intellisense search mode (eg.user when user wrote "ab", we're only adding to the list those identifiers, which names starts with "ab" and so on)
   TryToAdd(Symbol.getName, Name);

   // if function, parse also each function symbol
   if (Symbol.Typ = stFunction) Then
   Begin
    For Tmp in Symbol.mFunction.getSymbolList Do
     ParseSymbol(Symbol, Tmp);
   End;
  End;

  { ParseNamespace }
  Procedure ParseNamespace(const NS: TNamespace);
  Var Symbol: TSymbol;

      Reachable: Boolean = False;
      Name     : String;
  Begin
   // parse namespace symbol
   TryToAdd(NS.getName, Format('namespace'+IntellisenseDelimiter+'namespace'+IntellisenseDelimiter+'%s', [NS.getName]));

   // check if namespace is reachable
   For Name in ReachableNamespaces Do
   Begin
    if (NS.getName = Name) Then
    Begin
     Reachable := True;
     Break;
    End;
   End;

   // if not, give up
   if (not Reachable) Then
    Exit;

   // parse namespace's symbols
   For Symbol in NS.getSymbolList Do
    ParseSymbol(nil, Symbol);
  End;

Var NS: TNamespace;
Begin
 With Result do
 Begin
  Clear;

  For NS in NamespaceList Do
   ParseNamespace(NS);
 End;
End;
End.
