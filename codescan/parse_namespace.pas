(* TCodeScanner.ParseNamespace *)
Procedure TCodeScanner.ParseNamespace;
Var Name                    : String;
    Deep                    : Integer;
    Namespace, PrevNamespace: TNamespace;
Begin
 // @TODO: backup 'SelectedNamespaces'

 With Parser do
 Begin
  Name := read_ident;

  Namespace := findNamespace(Name);
  if (Namespace = nil) Then // new namespace
  Begin
   Namespace := TNamespace.Create(next(-1), getCurrentRange, Name);
   SymbolList.Add(TSymbol.Create(stNamespace, Namespace));
  End Else
  Begin
   AddIdentifier(Namespace, Parser.next(-1)); // add identifier reference
  End;

  PrevNamespace    := CurrentNamespace;
  CurrentNamespace := Namespace;

  Deep := CurrentDeep;
  Repeat
   ParseToken;
  Until (CurrentDeep = Deep);

  CurrentNamespace := PrevNamespace;
 End;

 // @TODO: restore 'SelectedNamespaces'
End;
