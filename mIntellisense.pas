(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
Unit mIntellisense;

 Interface
 Uses Classes, CodeScan;

 Procedure MakeIntellisenseIdentifierList(Result: TStrings; SymbolList: TSymbolList; NamespaceList: TStringList; FunctionName, Identifier: String);

 Implementation

(* MakeIntellisenseIdentifierList *)
Procedure MakeIntellisenseIdentifierList(Result: TStrings; SymbolList: TSymbolList; NamespaceList: TStringList; FunctionName, Identifier: String);

  // ParseSymbol
  Procedure ParseSymbol(Parent, Symbol: TSymbol);
  Var Name: String = '';
      Tmp : TSymbol;
      BTmp: Boolean;
  Begin
   if (Parent <> nil) Then
   Begin
    if (Parent.Typ = stFunction) and (Parent.getName <> FunctionName) Then // not our function
     Exit;

    BTmp := False;
    For Name in NamespaceList Do // not our namespace
     if (Parent.getName = Name) Then
      BTmp := True;

    if (not BTmp) Then
     Exit;
   End;

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

   if (Symbol.Typ = stNamespace) Then
   Begin
    For Tmp in Symbol.mNamespace.SymbolList Do
     ParseSymbol(Symbol, Tmp);
   End Else

   if (Symbol.Typ = stFunction) Then
   Begin
    For Tmp in Symbol.mFunction.SymbolList Do
     ParseSymbol(Symbol, Tmp);
   End;
  End;

Var Symbol: TSymbol;
Begin
 With Result do
 Begin
  Clear;

  For Symbol in SymbolList Do
   ParseSymbol(nil, Symbol);
 End;
End;
End.
