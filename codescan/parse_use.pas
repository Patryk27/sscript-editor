(* TCodeScanner.ParseUse *)
Procedure TCodeScanner.ParseUse;
Var Name: String;

  { AddNamespace }
  Procedure AddNamespace(const Name: String; const NS: TNamespace);
  Begin
   if (NS = nil) Then
    raise EParserError.Create(Language.getText(ls_unknown_namespace, [Name]));

   NamespaceVisibilityList.Add(TNamespaceVisibility.Create(Parser.getCurrentRange, NS));
   AddIdentifier(NS, Parser.next(-1));
  End;

Begin
 With Parser do
 Begin
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
