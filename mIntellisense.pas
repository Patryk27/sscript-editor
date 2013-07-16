(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
Unit mIntellisense;

 Interface
 Uses Classes, CodeScan;

 Procedure MakeIntellisenseIdentifierList(Result: TStrings; NamespaceList: TNamespaceList; ReachableNamespaces: TStringList; FunctionName, Identifier: String);

 Implementation
Uses SysUtils, Dialogs;

(* MakeIntellisenseIdentifierList *)
Procedure MakeIntellisenseIdentifierList(Result: TStrings; NamespaceList: TNamespaceList; ReachableNamespaces: TStringList; FunctionName, Identifier: String);

  // ParseSymbol
  Procedure ParseSymbol(Parent, Symbol: TSymbol);
  Var Name: String = '';
      Tmp : TSymbol;
  Begin
   if (Parent <> nil) Then
    if (Parent.Typ = stFunction) and (Parent.getName <> FunctionName) Then // not our function
     Exit;

   Case Symbol.Typ of
    stNamespace: Name := 'namespace';

    stType:
     if (Parent = nil) or (Parent.Typ = stNamespace) Then
      Name := 'global type' Else
      Name := 'local type';

    stVariable:
     if (Parent = nil) or (Parent.Typ = stNamespace) Then
      Name := 'global var' Else
      Name := 'local var';

    stConstant:
     if (Parent = nil) or (Parent.Typ = stNamespace) Then
      Name := 'global const' Else
      Name := 'local const';

    stFunction:
     Name := 'function';
   End;

   Name += '|'+Symbol.getName;

   if (Copy(Symbol.getName, 1, Length(Identifier)) = Identifier) or (Length(Identifier) = 0) Then
    Result.Add(Name);

   if (Symbol.Typ = stFunction) Then
   Begin
    For Tmp in Symbol.mFunction.SymbolList Do
     ParseSymbol(Symbol, Tmp);
   End;
  End;

  // ParseNamespace
  Procedure ParseNamespace(const NS: TNamespace);
  Var Symbol: TSymbol;
      BTmp  : Boolean = False;
      Name  : String;
  Begin
   For Name in ReachableNamespaces Do // not our namespace
    if (NS.Name = Name) Then
     BTmp := True;

   if (not BTmp) Then
    Exit;

   For Symbol in NS.SymbolList Do
    ParseSymbol(nil {NS}, Symbol);
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
