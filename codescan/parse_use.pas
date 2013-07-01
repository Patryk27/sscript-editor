(* TCodeScanner.ParseUse *)
Procedure TCodeScanner.ParseUse;
Var Name: String;

  // AddNamespace
  Procedure AddNamespace(Name: String; NS: TNamespace);
  Var Tmp: TNamespace;
  Begin
   if (NS = nil) Then
    raise EParserError.CreateFmt(getLangValue(ls_unknown_namespace), [Name]);

   For Tmp in SelectedNamespaces Do
    if (Tmp = NS) Then
     Exit;

   SelectedNamespaces.Add(NS);
   NamespaceVisibilityList.Add(TNamespaceVisibility.Create(Parser.getCurrentRange, NS));
   AddIdentifier(NS, Parser.next(-1));
  End;

Begin
 With Parser do
 Begin
  if (next_t = _SEMICOLON) Then // `use;` sets the global (default) namespace
  Begin
   SelectedNamespaces.Clear;
   SelectedNamespaces.Add(DefaultNamespace);
   eat(_SEMICOLON);
   Exit;
  End;

  Repeat
   Name := read_ident;
   AddNamespace(Name, findNamespace(Name));

   if (next_t = _SEMICOLON) Then
   Begin
    semicolon;
    Exit;
   End Else
    eat(_COMMA);
  Until (False);
 End;
End;
