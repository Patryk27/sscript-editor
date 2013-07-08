(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
{$MODE OBJFPC}{$H+}
Unit mProject;

 Interface
 Uses uMainForm, CodeScan, Tokens, LCLType, FileUtil, Classes, ComCtrls, Controls, Graphics, SynEdit, SynEditSScript, FGL,
      SynEditMiscClasses, SynHighlighterPas, SynHighlighterCpp, SynHighlighterJava, SynHighlighterIni, VirtualTrees,
      SynCompletion, mIntellisense, Process;

 Type TProjectType = (ptApplication, ptLibrary);

 Type TProject = Class;

 { TCard }
 Type TCardSaveReason = (csrClosingCard);

 Type TCard = Class
               Private
                Project: TProject;

                isMain, Named    : Boolean;
                FileName, Caption: String;
                SynEdit          : TSynEdit;
                Tab              : TTabSheet;
                ErrorLine        : Integer;
                ShouldBeReparsed : Boolean;
                CodeScanner      : TCodeScanner;
                IdentifierList   : TIdentifierList;

                Parsing: Boolean;

                Intellisense: TSynCompletion;

                Procedure UpdateIntellisense;

                Procedure Editor_OnKeyPress(Sender: TObject; var Key: Char);
                Procedure Editor_OnKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
                Procedure Editor_OnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
                Procedure Editor_OnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
                Procedure Editor_OnClick(Sender: TObject);
                Procedure Editor_OnSpecialLineMarkup(Sender: TObject; Line: Integer; var Special: Boolean; Markup: TSynSelectedColor);
                Procedure Editor_OnChange(Sender: TObject);

                Function Intellisense_OnPaintItem(const AKey: String; ACanvas: TCanvas; X, Y: integer; Selected: Boolean; Index: Integer): Boolean;
                Procedure Intellisense_OnSearchPosition(var Position: Integer);
                Procedure Intellisense_OnShow(Sender: TObject);
                Procedure Intellisense_OnHide(Sender: TObject);
                Procedure Intellisense_OnCodeCompletion(var Value: String; SourceValue: String; var SourceStart, SourceEnd: TPoint; KeyChar: TUTF8Char; Shift: TShiftState);
                Procedure Intellisense_OnKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

               Public
                Property getFileName: String read FileName;
                Property getShouldBeReparsed: Boolean read ShouldBeReparsed;

                Constructor Create(const fFileName, fCaption: String; fProject: TProject);
                Destructor Destroy; override;

                Function Save(const fFileName: String=''): Boolean;
                Function Save(const Reason: TCardSaveReason): Boolean;

                Procedure SetFocus;
                Procedure Update;
                Procedure RefreshControls;

                Procedure ReCaption(const NewCaption: String);

                Procedure Parse(const ForceParse: Boolean=False);

                Function GetCharAtCaret: Char;
                Function GetTokenPAtCaret: TToken_P;
                Function GetNamespaceAtCaret: TNamespace;
                Function GetFunctionAtCaret: TFunction;
                Function GetIdentifierAtCaret: String;
                Function GetFunctionNameAtCaret: String;
                Function GetNamespacesAtCaret: TStringList;
               End;

 // TMessage
 Type PMessage = ^TMessage;
      TMessage = Record
                  Line, Char    : Integer;
                  FileName, Text: String;
                 End;

 // lists
 Type TCardList = specialize TFPGList<TCard>;
 Type TMessageList = specialize TFPGList<PMessage>;

 // TCompilerSwitches
 Type TCompilerSwitch = (_initcode, _Cconst);
 Type TCompilerSwitches = Set of TCompilerSwitch;

 // TVMSwitches
 Type TVMSwitch = (c_time, c_wait, c_err);
 Type TVMSwitches = Set of TVMSwitch;

 { TProject }
 Type TProject = Class
                  Private
                   CardList   : TCardList;
                   MessageList: TMessageList;

                   ParseError: Record
                                Any       : Boolean;
                                Line, Char: Integer;
                                FileName  : String;
                               End;

                   Function CreateCard(const cFileName: String; LoadFile: Boolean): TCard;
                   Function CreateCardEx(const cFileName, cCaption: String; LoadFile: Boolean): TCard;
                   Function FindCard(const cFileName: String): TCard;
                   Function getMainCard: TCard;

                   Function MakeAbsolutePath(const FileName: String): String;
                   Function MakeRelativePath(const FileName: String): String;
                   Procedure CheckPaths;

                   Procedure SaveIfPossible;

                  Public
                   // project state
                   Named, Saved: Boolean;

                   // paths and other text data
                   FileName                  : String;
                   IncludePath, OutputFile   : String;
                   HeaderFile, BytecodeOutput: String;

                   // project info
                   ProjectType: TProjectType;

                   // compiler
                   CompilerSwitches     : TCompilerSwitches;
                   OptimizationLevel    : uint8; // 0..3
                   OtherCompilerSwitches: String;
                   CompilerOutput       : String;

                   // vm
                   VMSwitches     : TVMSwitches;
                   OtherVMSwitches: String;

                   VMProcess: TProcess;

                   // methods
                   Constructor Create;
                   Destructor Destroy; override;

                   Procedure NewProject(const Typ: TProjectType);
                   Procedure NewNoNameCard;

                   Function Save(const fFileName: String=''): Boolean;
                   Procedure SaveCurrentCard;
                   Procedure SaveCurrentCardAs;
                   Function Open(const fFileName: String=''): Boolean;
                   Function OpenCard(const fFileName: String): TCard;

                   Procedure UpdateCards;
                   Procedure SwapCards(A, B: Integer);
                   Procedure CloseCard(ID: Integer);
                   Procedure CloseCardsExcluding(ID: Integer);
                   Procedure RaiseMessage(ID: Integer);

                   Procedure RefreshControls;

                   Function Compile: Boolean;
                   Procedure Run;
                   Procedure StopProgram;

                   Function isEverythingSaved: Boolean;
                   Function getCurrentCard: TCard;
                   Function getCurrentEditor: TSynEdit;

                   Function FindIdentifier(const CaretXY: TPoint): PIdentifier;
                   Procedure JumpToDeclaration(const Ident: PIdentifier);

                   Procedure UpdateIdentifierList;
                  End;

 Function getCompilerSwitchName(const S: TCompilerSwitch; DeleteFirstChar: Boolean=True): String;
 Function getVMSwitchName(const S: TVMSwitch; DeleteFirstChars: Boolean=True): String;

 Var Project: TProject = nil; // currently opened project

 Implementation
Uses mSettings, mLanguages, uIdentifierListForm, uCompileStatusForm, uCodeEditor, Parser,
     Dialogs, SysUtils, Forms, DOM, XMLWrite, XMLRead, TypInfo;

{ getCompilerSwitchName }
Function getCompilerSwitchName(const S: TCompilerSwitch; DeleteFirstChar: Boolean=True): String;
Begin
 Result := GetEnumName(TypeInfo(TCompilerSwitch), Integer(S));

 if (DeleteFirstChar) Then
  Delete(Result, 1, 1);
End;

{ getVMSwitchName }
Function getVMSwitchName(const S: TVMSwitch; DeleteFirstChars: Boolean=True): String;
Begin
 Result := GetEnumName(TypeInfo(TVMSwitch), Integer(S));

 if (DeleteFirstChars) Then
  Delete(Result, 1, 2);
End;

// -------------------------------------------------------------------------- //
(* TCard.UpdateIntellisense *)
{
 Updates the Intellisense window.
}
Procedure TCard.UpdateIntellisense;
Var NS: TStringList;
Begin
 NS := GetNamespacesAtCaret;
 Try
  MakeIntellisenseIdentifierList(Intellisense.ItemList, CodeScanner.getSymbolList, NS, GetFunctionNameAtCaret, GetIdentifierAtCaret);
 Finally
  NS.Free;
 End;
End;

(* TCard.Editor_OnKeyPress *)
Procedure TCard.Editor_OnKeyPress(Sender: TObject; var Key: Char);
Var Str: String = '';
Begin
 if (Key in ['(', '[', '<', '{']) Then
  if (mSettings.getBoolean(sAddBrackets)) Then
  Begin
   Case Key of
    '(': Str := '()';
    '[': Str := '[]';
    '<': Str := '<>';
    '{': Str := '{'#13#10#13#10'}';
   End;

   SynEdit.InsertTextAtCaret(Str);
   SynEdit.CaretX := SynEdit.CaretX-1;

   if (Key = '{') Then
   Begin
    SynEdit.CaretX := 1;
    SynEdit.CaretY := SynEdit.CaretY-1;
   End;

   Key := #0;
  End;

 ErrorLine := -1;
 SynEdit.Invalidate;
End;

(* TCard.Editor_OnKeyDown *)
Procedure TCard.Editor_OnKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
Var Card: TCard;
Begin
 ErrorLine := -1;
 SynEdit.Invalidate;

 if (Shift = [ssCtrl]) and (Key = VK_SPACE) Then // Intellisense shortcut; @TODO: this should be configurable from settings
 Begin
  Key := 0;
  Parse;

  if (CodeScanner <> nil) Then
  Begin
   if (Project.ParseError.Any) Then // cannot open Intellisense if parsing wasn't successful
   Begin
    if (Project.ParseError.FileName = 'main.ss') and (not Project.Named) Then // special case
     Card := Project.FindCard('main.ss') Else
     Card := Project.OpenCard(Project.ParseError.FileName);

    if (Card <> nil) Then
    Begin
     Card.SynEdit.CaretX := Project.ParseError.Char;
     Card.SynEdit.CaretY := Project.ParseError.Line;
     Card.ErrorLine      := Project.ParseError.Line;
     Card.SetFocus;
    End;
   End Else
   Begin
    UpdateIntellisense;
    Intellisense.Execute('',
                         CodeEditor.Left+SynEdit.Left+SynEdit.CaretXPix+12,
                         CodeEditor.Top+SynEdit.CaretYPix+SynEdit.Font.Size+CodeEditor.Panel1.Height+64);
   End;
  End;
 End;
End;

(* TCard.Editor_OnMouseUp *)
Procedure TCard.Editor_OnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
Begin
 ErrorLine := -1;
 SynEdit.Invalidate;
End;

(* TCard.Editor_OnMouseDown *)
Procedure TCard.Editor_OnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
Begin
 ErrorLine := -1;
 SynEdit.Invalidate;
End;

(* TCard.Editor_OnClick *)
Procedure TCard.Editor_OnClick(Sender: TObject);
Begin
 ErrorLine := -1;
 SynEdit.Invalidate;
End;

(* TCard.Editor_OnSpecialLineMarkup *)
Procedure TCard.Editor_OnSpecialLineMarkup(Sender: TObject; Line: Integer; var Special: Boolean; Markup: TSynSelectedColor);
Begin
 if (Line = ErrorLine) Then
 Begin
  Markup.Background := $50A0FF;
  Special           := True;
 End;
End;

(* TCard.Editor_OnChange *)
Procedure TCard.Editor_OnChange(Sender: TObject);
Begin
 if (Intellisense.isActive) Then
  UpdateIntellisense;

 ShouldBeReparsed := True;
End;

(* TCard.Intellisense_OnPaintItem *)
Function TCard.Intellisense_OnPaintItem(const AKey: String; ACanvas: TCanvas; X, Y: integer; Selected: Boolean; Index: Integer): Boolean;
Var ItemText, ItemLeft, ItemRight: String;
    Rect                         : TRect;
begin
 Rect.Left := X;
 Rect.Top  := Y;

 With ACanvas do
 Begin
  Font.Name := 'Courier New'; // @TODO: this should be configurable
  Font.Size := 10;

  Rect.Bottom := Rect.Top+Intellisense.FontHeight;
  Rect.Right  := Rect.Left+Intellisense.TheForm.Width;
 End;

 With ACanvas do
 Begin
  if (Selected) Then
   Brush.Color := $00A00000;

  FillRect(Rect);

  ItemText := AKey;

  ItemLeft  := Copy(ItemText, 1, Pos('|', ItemText)-1);
  ItemRight := Copy(ItemText, Pos('|', ItemText)+1, Length(ItemText));

  //Font.Style := [fsItalic];
  TextOut(Rect.Left, Rect.Top, ItemLeft);

  Font.Style := [fsBold];
  TextOut(Rect.Left+110, Rect.Top, ItemRight);

  Font.Style := [];
 End;

 Result := True;
End;

(* TCard.Intellisense_OnSearchPosition *)
Procedure TCard.Intellisense_OnSearchPosition(var Position: Integer);
Var I    : Integer;
    Value: String;
Begin
 With Intellisense do
  For I := 0 To ItemList.Count-1 Do
  Begin
   Value := ItemList[I];
   Delete(Value, 1, Pos('|', Value));

   if (Copy(Value, 1, Length(CurrentString)) = CurrentString) Then
   Begin
    Position := I;
    Exit;
   End;
  End;
End;

(* TCard.Intellisense_OnShow *)
Procedure TCard.Intellisense_OnShow(Sender: TObject);
Begin
 With TForm(Sender) do
 Begin
  Width  := getInteger(sIntellisenseWidth);
  Height := getInteger(sIntellisenseHeight);
 End;
End;

(* TCard.Intellisense_OnHide *)
Procedure TCard.Intellisense_OnHide(Sender: TObject);
Begin
 With TForm(Sender) do
 Begin
  setInteger(sIntellisenseWidth, Width);
  setInteger(sIntellisenseHeight, Height);
 End;
End;

(* TCard.Intellisense_OnCodeCompletion *)
Procedure TCard.Intellisense_OnCodeCompletion(var Value: String; SourceValue: String; var SourceStart, SourceEnd: TPoint; KeyChar: TUTF8Char; Shift: TShiftState);
Begin
 Delete(Value, 1, Pos('|', Value));
End;

(* TCard.Intellisense_OnKeyDown *)
Procedure TCard.Intellisense_OnKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
Begin
 Case Key of
  { left }
  VK_LEFT:
  Begin
   if (SynEdit.Lines[SynEdit.CaretY-1][SynEdit.CaretX-1] in ['a'..'z', 'A'..'Z', '0'..'9', '_']) Then
    SynEdit.CaretX := SynEdit.CaretX-1;

   UpdateIntellisense;
  End;

  { right }
  VK_RIGHT:
  Begin
   SynEdit.CaretX := SynEdit.CaretX+1;
   if (Length(GetIdentifierAtCaret) = 0) Then
    SynEdit.CaretX := SynEdit.CaretX-1;

   UpdateIntellisense;
  End;
 End;
End;

(* TCard.Create *)
{
 Creates a card with cation `fCaption` and loads into it file named `fFileName`
}
Constructor TCard.Create(const fFileName, fCaption: String; fProject: TProject);
Begin
 Project := fProject;

 FileName := fFileName;
 Caption  := fCaption;

 ErrorLine        := -1;
 ShouldBeReparsed := True;
 CodeScanner      := nil;

 Named  := False;
 isMain := False;

 SynEdit := TSynEdit.Create(MainForm);
 Tab     := TTabSheet.Create(MainForm);

 SynEdit.Keystrokes.Delete(SynEdit.Keystrokes.FindKeycode(VK_U, [ssShift, ssCtrl])); // @TODO: this shouldn't be hardcoded

 { Tab }
 Tab.Caption     := fCaption;
 Tab.PageControl := CodeEditor.Tabs;
 Tab.PopupMenu   := CodeEditor.Tabs.PopupMenu;

 { SynEdit }
 With SynEdit do
 Begin
  Parent      := Tab;
  Align       := alClient;
  PopupMenu   := CodeEditor.SynEditPopup;

  OnKeyPress          := @Editor_OnKeyPress;
  OnKeyDown           := @Editor_OnKeyDown;
  OnMouseUp           := @Editor_OnMouseUp;
  OnMouseDown         := @Editor_OnMouseDown;
  OnClick             := @Editor_OnClick;
  OnSpecialLineMarkup := @Editor_OnSpecialLineMarkup;
  OnChange            := @Editor_OnChange;

  Modified := False;
 End;

 { Intellisense }
 Intellisense := TSynCompletion.Create(nil);
 With Intellisense do
 Begin
  Editor := SynEdit;

  ShowSizeDrag := True;

  OnPaintItem      := @Intellisense_OnPaintItem;
  OnSearchPosition := @Intellisense_OnSearchPosition;
  OnCodeCompletion := @Intellisense_OnCodeCompletion;
  OnKeyDown        := @Intellisense_OnKeyDown;

  TheForm.OnShow := @Intellisense_OnShow;
  TheForm.OnHide := @Intellisense_OnHide;
 End;

 { Tabs }
 With CodeEditor.Tabs do
  ActivePageIndex := PageCount-1;

 RefreshControls;

 SynEdit.SetFocus;
End;

(* TCard.Destroy *)
{
 Destroys card's components and then removes the card.
}
Destructor TCard.Destroy;
Begin
 SynEdit.Free;
 Tab.Destroy;
End;

(* TCard.Save *)
{
 Saves card's editor content.
 Returns `true` if content was successfully saved or `false` otherwise.
}
Function TCard.Save(const fFileName: String=''): Boolean;
Begin
 Result := False;

 if (not Named) and (fFileName = '') Then
 Begin
  { open save dialog }
  With TSaveDialog.Create(MainForm) do
  Begin
   Try
    Title    := getLangValue(ls_module_saving);
    Filter   := getLangValue(ls_filter_module);
    FileName := ExtractFileName(self.FileName);

    if (Execute) Then
    Begin
     self.FileName         := FileName;
     self.Named            := True;
     self.ShouldBeReparsed := True;

     ReCaption(ExtractFileName(FileName));
    End Else
     Exit(False);
   Finally
    Free;
   End;
  End;
 End;

 if (not Named) Then
 Begin
  FileName := fFileName;
  Named    := True;
 End;

 SynEdit.Lines.SaveToFile(FileName);
 SynEdit.Modified := False;

 Exit(True);
End;

(* TCard.Save *)
{
 Displays a prompt and then saves the card.
 Returns `true` if content was successfully saved or `false` otherwise.
}
Function TCard.Save(const Reason: TCardSaveReason): Boolean;
Begin
 Result := False;

 if (Reason = csrClosingCard) Then
 Begin
  Case MessageDlg(getLangValue(ls_title_card_close), getLangValue(ls_msg_card_close), mtWarning, mbYesNoCancel, '') of
   { yes }
   mrYes:
   Begin
    Save();
    Exit(True);
   End;

   { no }
   mrNo: Exit(True);

   { cancel }
   mrCancel: Exit(False);
  End;

  Exit(True);
 End;
End;

(* TCard.SetFocus *)
Procedure TCard.SetFocus;
Begin
 CodeEditor.Show;
 CodeEditor.SetFocus;
 CodeEditor.Tabs.ActivePageIndex := Project.CardList.IndexOf(self);

 if (not CodeEditor.Tabs.CanFocus) Then // @TODO
  raise Exception.Create('CodeEditor.Tabs.CanFocus = False, this was not supposed to happen!');

 CodeEditor.Tabs.SetFocus;
 CodeEditor.Tabs.ActivePage.SetFocus;

 SynEdit.SetFocus;
End;

(* TCard.Update *)
{
 Updates card
}
Procedure TCard.Update;
Begin
 // update card's caption
 if (SynEdit.Modified) Then
  Tab.Caption := '* '+Caption Else
  Tab.Caption := Caption;
End;

(* TCard.RefreshControls *)
{
 Refreshes card's controls.
}
Procedure TCard.RefreshControls;
Begin
 With SynEdit do
 Begin
  // recreate highlighter
  if (Highlighter <> nil) Then
   Highlighter.Free;

  SynEdit.Font.Quality := fqCleartype; // @TODO: this should be configurable

  Case LowerCase(ExtractFileExt(FileName)) of
   '.ss'        : Highlighter := THighlighter.Create(SynEdit);
   '.pas', '.pp': Highlighter := TSynPasSyn.Create(SynEdit);
   '.c', '.cpp' : Highlighter := TSynCppSyn.Create(SynEdit);
   '.java'      : Highlighter := TSynJavaSyn.Create(SynEdit);
   '.ini'       : Highlighter := TSynIniSyn.Create(SynEdit);
  End;

  // update SynEdit
  With SynEdit do
  Begin
   if (mSettings.getBoolean(sScrollPastEOL)) Then
    Options := Options + [eoScrollPastEOL] Else
    Options := Options - [eoScrollPastEOL];
  End;
 End;
End;

(* TCard.ReCaption *)
{
 Sets new caption for card.
}
Procedure TCard.ReCaption(const NewCaption: String);
Begin
 Caption     := NewCaption;
 Tab.Caption := Caption;
End;

(* TCard.Parse *)
Procedure TCard.Parse(const ForceParse: Boolean=False);
Var Msg, ErrFile: String;
    Card        : TCard;

    null_token: TToken_P;
    null_range: TRange;
Begin
 // @TODO: if some card doesn't have to be reparsed, let the parser use its CodeScanner instead of parsing it again.

 if (Parsing) or ((not ForceParse) and (not ShouldBeReparsed)) or (CompareText(ExtractFileExt(getFileName), '.ss') <> 0) Then
  Exit;

 With CompileStatusForm.CompileStatus do // clear parser's error message
  if (Items.GetLastNode <> nil) and (Items.GetLastNode.Data = Pointer($CAFEBABE)) Then
   Items.Delete(Items.GetLastNode);

 Parsing                := False;
 Project.ParseError.Any := False;
 ShouldBeReparsed       := False;

 if (CodeScanner <> nil) Then // free previous code scanner
  FreeAndNil(CodeScanner);

 Parsing     := True;
 CodeScanner := TCodeScanner.Create(SynEdit.Lines, getFileName, Project.getMainCard.getFileName, getString(sCompilerFile), Project.IncludePath, True); // parse!
 Try
  Try
   IdentifierList := CodeScanner.Parse;
  Except
   On E: Exception Do
   Begin
    With Project.ParseError do
    Begin
     Any      := True;
     FileName := CodeScanner.CurrentlyParsedFile; // file, where the error has been raised
     Line     := CodeScanner.getParser.next(-1).Line;
     Char     := CodeScanner.getParser.next(-1).Char;

     ErrFile := FileName;
    End;

    if (AnsiCompareFileName(ErrFile, getFileName) = 0) Then
     Card := self Else
     Begin
      Card := Project.FindCard(ErrFile);

      if (Card = nil) Then
       Card := Project.OpenCard(ErrFile);
     End;

    if (Card <> nil) Then
     CodeEditor.Tabs.ActivePageIndex := Project.CardList.IndexOf(Card);

    Msg := Format(getLangValue(ls_codescan_failed), [ErrFile, Project.ParseError.Line, Project.ParseError.Char, E.Message]);

    With CompileStatusForm.CompileStatus do
    Begin
     if (Items.GetLastNode <> nil) and (Items.GetLastNode.Data = Pointer($CAFEBABE)) Then
      Items.GetLastNode.Text := Msg Else
      With Items.Add(nil, Msg) do
      Begin
       Data          := Pointer($CAFEBABE);
       ImageIndex    := 3;
       SelectedIndex := ImageIndex;
      End;
    End;
   End;
  End;
 Finally
  Parsing := False;
 End;

 if (CodeScanner <> nil) Then
  With CodeScanner.getSymbolList do
  Begin
   // these identifiers are not visible in the identifier list, but they are in the Intellisense form
   null_token.Char     := 0;
   null_token.Line     := 0;
   null_token.FileName := '';
   null_token.Value    := '';

   null_range.PBegin    := null_token;
   null_range.PEnd      := null_token;
   null_range.PEnd.Char := High(LongWord);
   null_range.PEnd.Line := High(LongWord);

   Add(TSymbol.Create(stConstant, TVariable.Create(null_token, null_range, 'null')));
   Add(TSymbol.Create(stType, TVariable.Create(null_token, null_range, 'any')));
   Add(TSymbol.Create(stType, TVariable.Create(null_token, null_range, 'void')));
   Add(TSymbol.Create(stType, TVariable.Create(null_token, null_range, 'bool')));
   Add(TSymbol.Create(stType, TVariable.Create(null_token, null_range, 'char')));
   Add(TSymbol.Create(stType, TVariable.Create(null_token, null_range, 'int')));
   Add(TSymbol.Create(stType, TVariable.Create(null_token, null_range, 'float')));
   Add(TSymbol.Create(stType, TVariable.Create(null_token, null_range, 'string')));
  End;
End;

(* TCard.GetCharAtCaret *)
{
 Returns char located at editor's caret.
}
Function TCard.GetCharAtCaret: Char;
Begin
 With SynEdit do
  Exit(Lines[CaretY-1][CaretX]);
End;

(* TCard.GetTokenPAtCaret *)
{
 Returns `Tokens.Token_P` located at editor's caret.
}
Function TCard.GetTokenPAtCaret: TToken_P;
Begin
 Result.Line     := SynEdit.CaretY;
 Result.Char     := SynEdit.CaretX{-1};
 Result.FileName := getFileName;
End;

(* TCard.GetNamespaceAtCaret *)
Function TCard.GetNamespaceAtCaret: TNamespace;

   // ParseSymbol
   Procedure ParseSymbol(Symbol: TSymbol);
   Var Tmp: TSymbol;
   Begin
    if (Symbol.Typ = stNamespace) and (Symbol.getToken.FileName = FileName) and (GetTokenPAtCaret in Symbol.getRange) Then
    Begin
     Result := Symbol.mNamespace;
     Exit;
    End;
   End;

Var Symbol: TSymbol;
Begin
 Result := nil;

 if (CodeScanner = nil) or (Project.ParseError.Any) Then
  Exit;

 For Symbol in CodeScanner.getSymbolList Do
  ParseSymbol(Symbol);
End;

(* TCard.GetFunctionAtCaret *)
Function TCard.GetFunctionAtCaret: TFunction;

   // ParseSymbol
   Procedure ParseSymbol(Symbol: TSymbol);
   Var Tmp: TSymbol;
   Begin
    if (Symbol.Typ = stFunction) and (Symbol.getToken.FileName = FileName) and (GetTokenPAtCaret in Symbol.getRange) Then
    Begin
     Result := Symbol.mFunction;
     Exit;
    End;

    if (Symbol.Typ = stNamespace) Then
     For Tmp in Symbol.mNamespace.SymbolList Do
      ParseSymbol(Tmp);
   End;

Var Symbol: TSymbol;
Begin
 Result := nil;

 if (CodeScanner = nil) or (Project.ParseError.Any) Then
  Exit;

 For Symbol in CodeScanner.getSymbolList Do
  ParseSymbol(Symbol);
End;

(* TCard.GetIdentifierAtCaret *)
{
 Returns identifier located backwards at editor's caret.
}
Function TCard.GetIdentifierAtCaret: String;
Var PosX: Integer;
Begin
 Result := SynEdit.LineText;
 PosX   := SynEdit.CaretX-1;

 if (PosX < 0) or (PosX > Length(Result)) Then
  Exit('');

 While ((PosX > 0) and (Result[PosX] in ['a'..'z', 'A'..'Z', '0'..'9', '_'])) Do
  Dec(PosX);

 While ((PosX < Length(Result)) and (Result[PosX] in ['0'..'9'])) Do // identifier cannot begin with a number
  Inc(PosX);

 Delete(Result, SynEdit.CaretX, Length(Result));
 Delete(Result, 1, PosX);

 Result := Trim(Result);
End;

(* TCard.GetFunctionNameAtCaret *)
{
 Returns function's name in which editor's caret is located.
}
Function TCard.GetFunctionNameAtCaret: String;
Var Func: TFunction;
Begin
 Func := GetFunctionAtCaret;

 if (Func = nil) Then
  Result := '' Else
  Result := Func.Name;
End;

(* TCard.GetNamespacesAtCaret *)
Function TCard.GetNamespacesAtCaret: TStringList;
Var NSV      : TNamespaceVisibility;
    Symbol   : TSymbol;
    Token    : TToken_P;
    Line     : String;
    Tmp, PosX: Integer;
Begin
 Result := TStringList.Create;

 if (CodeScanner = nil) Then
  Exit;

 Token := GetTokenPAtCaret;

 For NSV in CodeScanner.getNamespaceVisibilityList Do
  if (Token in NSV.Range) Then
   Result.Add(NSV.Namespace.Name);

 For Symbol in CodeScanner.getSymbolList Do
  if (Symbol.Typ = stNamespace) and (Token in Symbol.mNamespace.Range) Then
  Begin
   Result.Add(Symbol.getName);
   Break; // namespaces cannot be inlined in each other, so just stop.
  End;

 // `namespace::identifier` construction
 Line := SynEdit.LineText;
 PosX := SynEdit.CaretX-1;

 if (PosX < 0) or (PosX > Length(Line)) Then // shouldn't happen (?)
  Exit;

 While ((PosX > 0) and (Line[PosX] in ['a'..'z', 'A'..'Z', '0'..'9', '_'])) Do
  Dec(PosX);

 if (Copy(Line, PosX-1, 2) = '::') Then // namespace operator
 Begin
  Tmp := SynEdit.CaretX;

  SynEdit.CaretX := PosX-1; // kinda ugly hack :P
  Result.Insert(0, GetIdentifierAtCaret);

  SynEdit.CaretX := Tmp;
 End;
End;

// -------------------------------------------------------------------------- //
(* TProject.CreateCard *)
{
 Creates a new card from specified file name and optionally loads that file (if `LoadFile = true`).
}
Function TProject.CreateCard(const cFileName: String; LoadFile: Boolean): TCard;
Begin
 Result := CreateCardEx(cFileName, ExtractFileName(cFileName), LoadFile);
End;

(* TProject.CreateCardEx *)
{
 Creates a new card.
}
Function TProject.CreateCardEx(const cFileName, cCaption: String; LoadFile: Boolean): TCard;
Var Text: String;
Begin
 Result := TCard.Create(cFileName, cCaption, self);

 if (LoadFile) Then // load from file?
 Begin
  if (FileExists(cFileName)) Then
  Begin
   With Result do
   Begin
    Named    := True;
    Saved    := True;
    FileName := cFileName;

    SynEdit.Lines.LoadFromFile(cFileName);
   End;
  End Else
  Begin
   FreeAndNil(Result);

   Text := Format(getLangValue(ls_msg_file_not_found), [cFileName]);
   Application.MessageBox(PChar(Text), PChar(getLangValue(ls_msg_error)), MB_IconError);
   Exit;
  End;
 End Else // just create an empty card
 Begin
  With Result do
  Begin
   Named := False;
   Saved := False;

   SynEdit.Modified := True;
  End;
 End;

 CardList.Add(Result);
End;

(* TProject.FindCard *)
{
 Finds a card with specified file name and returns it.
 Returns `nil` when a such card is not found.
}
Function TProject.FindCard(const cFileName: String): TCard;
Var I: Integer;
Begin
 Result := nil;

 For I := 0 To CardList.Count-1 Do
  if (AnsiCompareFileName(CardList[I].FileName, cFileName) = 0) Then
   Exit(CardList[I]);
End;

(* TProject.getMainCard *)
{
 Returns main card class.
}
Function TProject.getMainCard: TCard;
Var Card: TCard;
Begin
 Result := nil;

 For Card in CardList Do
  if (Card.isMain) Then
   Exit(Card);
End;

(* TProject.CheckPaths *)
{
 Checks compiler or/and (depending on project's type) virtual machine existence and displays an error when at least one of them isn't found.
}
Procedure TProject.CheckPaths;
Var Fail: Boolean;
Begin
 Case ProjectType of
  ptApplication: Fail := (not FileExists(getString(sCompilerFile))) or (not FileExists(getString(sVMFile))); // an application needs a compiler and a virtual machine
  ptLibrary    : Fail := (not FileExists(getString(sCompilerFile))); // a library needs only to be compiled (it cannot be run)
  else Fail := True;
 End;

 if (Fail) Then
  Application.MessageBox(PChar(getLangValue(ls_msg_compiler_or_vm_not_found)), PChar(getLangValue(ls_msg_warn)), MB_IconWarning);
End;

(* TProject.SaveIfPossible *)
{
 Saves project if all of the cards had been already saved.
}
Procedure TProject.SaveIfPossible;
Var CanSave: Boolean;
    Card   : TCard;
Begin
 CanSave := Named;

 For Card in CardList Do
  if (not Card.Named) Then
   CanSave := False;

 if (CanSave) Then
  Save;
End;

(* TProject.MakeAbsolutePath *)
{
 Creates a absolute path relative to project's file name.
}
Function TProject.MakeAbsolutePath(const FileName: String): String;
Begin
 Result := CreateAbsolutePath(FileName, ExtractFilePath(self.FileName));
End;

(* TProject.MakeRelativePath *)
{
 Creates a relative path relative to project's file name.
}
Function TProject.MakeRelativePath(const FileName: String): String;
Begin
 Result := CreateRelativePath(FileName, ExtractFilePath(self.FileName));
End;

(* TProject.Create *)
{
 Constructor for class `TProject`
}
Constructor TProject.Create;
Begin
 MainForm.setMainMenu(stDisabled);

 CardList    := TCardList.Create;
 MessageList := TMessageList.Create;
 ProjectType := ptApplication;

 ParseError.Any := False;

 VMProcess := nil;
End;

(* TProject.Destroy *)
{
 Frees each its card, disables main-menu and changes main form's caption.
}
Destructor TProject.Destroy;
Var Card: TCard;
Begin
 MainForm.setMainMenu(stDisabled);

 For Card in CardList Do
  With Card do
  Begin
   SynEdit.Free;
   Tab.Free;
  End;

 CardList.Free;
 MessageList.Free;
 MainForm.Caption := uMainForm.sCaption;
End;

(* TProject.NewProject *)
{
 Creates a new project of given type and enables main-menu.
}
Procedure TProject.NewProject(const Typ: TProjectType);
Begin
 MainForm.setMainMenu(stEnabled);

 Named := False;
 Saved := False;

 CompilerSwitches      := [_initcode, _Cconst]; // `-initcode`, `-Cconst` are enabled by default
 OtherCompilerSwitches := '';
 OptimizationLevel     := 2;

 VMSwitches      := [c_wait]; // `-wait` is enabled by default
 OtherVMSwitches := '';

 IncludePath    := '$file;$compiler';
 ProjectType    := Typ;
 OutputFile     := '';
 HeaderFile     := '';
 BytecodeOutput := '';

 CardList.Clear;
 MessageList.Clear;

 CheckPaths;

 if (CreateCard('main.ss', False) = nil) Then
  raise Exception.Create('This shouldn''t happen!');

 { sample program }
 With getCurrentEditor.Lines do
 Begin
  if (ProjectType = ptApplication) Then
  Begin
   Add('@("stdlib\\stdio.ss")');
   Add('');
   Add('use std;');
   Add('');
   Add('function<int> main()');
   Add('{');
   Add(' println("Hello World! :)");');
   Add(' return 0;');
   Add('}');
  End Else
  Begin
   Add('public function<string> test()');
   Add('{');
   Add(' return "Hello World from a Library! :)";');
   Add('}');
  End;
 End;

 With getCurrentCard do // this card is the main card
  isMain := True;
End;

(* TProject.NewNoNameCard *)
{
 Creates a new "no-name" card.
}
Procedure TProject.NewNoNameCard;
Var I   : Integer;
    Name: String;
Begin
 For I := 0 To 256 Do
 Begin
  Name := 'no_name_'+IntToStr(I)+'.ss';
  if (FindCard(Name) = nil) Then
  Begin
   CreateCard(Name, False);
   Exit;
  End;
 End;

// raise Exception.Create('Too many files opened.');
End;

(* TProject.Save *)
{
 Saves project.
}
Function TProject.Save(const fFileName: String=''): Boolean;
Var Doc               : TXMLDocument;
    Root, Parent, Node: TDOMNode;
    I                 : Integer;

    Switch  : TCompilerSwitch;
    VMSwitch: TVMSwitch;

  // WriteValue
  Procedure WriteValue(const Root: TDomNode; sName, sValue: String);
  Var Value: TDomNode;
  Begin
   Node  := Doc.CreateElement(sName);
   Value := Doc.CreateTextNode(sValue);
   Node.AppendChild(Value);

   Root.AppendChild(Node);
  End;

  // WriteValue
  Procedure WriteValue(const Root: TDomNode; Name: String; Value: Integer);
  Begin
   WriteValue(Root, Name, IntToStr(Value));
  End;

  // WriteValue
  Procedure WriteValue(const Root: TDomNode; Name: String; Value: Extended);
  Begin
   WriteValue(Root, Name, FloatToStr(Value));
  End;

Begin
 Result := False;

 if (not Named) and (fFileName = '') Then
 Begin
  { display save dialog }
  With TSaveDialog.Create(MainForm) do
  Begin
   Try
    Title  := getLangValue(ls_project_saving);
    Filter := getLangValue(ls_filter_project);

    if (Execute) Then
    Begin
     self.FileName := FileName;
     self.Named    := True;
    End Else
     Exit;
   Finally
    Free;
   End;
  End;
 End;

 if (not Named) Then
 Begin
  FileName := fFileName;
  Named    := True;
 End;

 if (OutputFile = '') Then
 Begin
  OutputFile := ExtractFileName(FileName);

  if (CompareText(Copy(OutputFile, Length(OutputFile)-3, 4), '.ssp') = 0) Then
   Delete(OutputFile, Length(OutputFile)-3, 4);

  Case ProjectType of
   ptApplication: OutputFile += '.ssc';
   ptLibrary    : OutputFile += '.ssm';
  End;
 End;

 self.Saved := True;
 FileName   := MakeAbsolutePath(FileName);

 if (CompareText(ExtractFileExt(FileName), '.ssp') <> 0) Then
  FileName += '.ssp';

 Try
  Doc := TXMLDocument.Create;

  { create root node }
  Root := Doc.CreateElement('project');
  Doc.AppendChild(Root);
  Root := Doc.DocumentElement;

  { save config }
  Parent := Doc.CreateElement('config');

  WriteValue(Parent, 'version', uMainForm.iVersion);
  WriteValue(Parent, 'project_type', ord(ProjectType));
  WriteValue(Parent, 'card_count', CardList.Count);
  WriteValue(Parent, 'opened_card', CodeEditor.Tabs.ActivePageIndex);
  WriteValue(Parent, 'output_file', OutputFile);
  WriteValue(Parent, 'header_file', HeaderFile);
  WriteValue(Parent, 'bytecode_output', BytecodeOutput);

  Root.AppendChild(Parent);

  { save compiler data }
  Parent := Doc.CreateElement('compiler');

  WriteValue(Parent, 'switches', OtherCompilerSwitches);
  WriteValue(Parent, 'include_path', IncludePath);
  WriteValue(Parent, 'optimization_level', OptimizationLevel);

  For Switch in TCompilerSwitches Do
   WriteValue(Parent, getCompilerSwitchName(Switch, False), Integer(Switch in CompilerSwitches));

  Root.AppendChild(Parent);

  { save vm data }
  Parent := Doc.CreateElement('vm');

  WriteValue(Parent, 'switches', OtherVMSwitches);

  For VMSwitch in TVMSwitches Do
   WriteValue(Parent, getVMSwitchName(VMSwitch, False), Integer(VMSwitch in VMSwitches));

  Root.AppendChild(Parent);

  { save card list }
  For I := 0 To CardList.Count-1 Do
   With CardList[I] do
   Begin
    if (not Save) Then
     Case MessageDlg(getLangValue(ls_module_saving), getLangValue(ls_msg_module_saving), mtWarning, mbYesNo, 0) of
      mrYes: if (not Save) Then
              Exit; // stop saving the project
      else Exit; // stop saving the project
     End;

    Parent := Doc.CreateElement('card_'+IntToStr(I));

    WriteValue(Parent, 'caption', Caption);
    WriteValue(Parent, 'file', MakeRelativePath(FileName));
    WriteValue(Parent, 'caret_x', SynEdit.CaretX);
    WriteValue(Parent, 'caret_y', SynEdit.CaretY);
    WriteValue(Parent, 'is_main', Integer(isMain));

    Root.AppendChild(Parent);
   End;

  WriteXMLFile(Doc, FileName);
 Finally
  Doc.Free;
 End;

 MainForm.Caption := uMainForm.sCaption+' - '+FileName;
 Exit(True);
End;

(* TProject.SaveCurrentCard *)
{
 Saves currently opened card.
}
Procedure TProject.SaveCurrentCard;
Begin
 if (not Named) Then
  Exit;

 CardList[CodeEditor.Tabs.ActivePageIndex].Save;
End;

(* TProject.SaveCurrentCard *)
{
 Saves currently opened card with new name.
}
Procedure TProject.SaveCurrentCardAs;
Var oldFileName: String;
Begin
 if (not Named) Then
  Exit;

 With CardList[CodeEditor.Tabs.ActivePageIndex] do
 Begin
  oldFileName := FileName;
  Named       := False;
  if (Save) Then
   DeleteFile(oldFileName) Else
   Named := True;
 End;
End;

(* TProject.Open *)
{
 Opens a new project.
}
Function TProject.Open(const fFileName: String=''): Boolean;
Var Doc         : TXMLDocument;
    Root, Parent: TDOMNode;
    CardCount, I: Integer;

    Switch  : TCompilerSwitch;
    VMSwitch: TVMSwitch;

    OpenedCard      : Integer;
    cCaption, cFile : String;
    cIsMain         : Boolean;

    OldWorkingDir: String;

    Version: Single;

  // ReadStringValue
  Function ReadStringValue(Node: TDOMNode; Name: String): String;
  Begin
   if (Node = nil) Then // parent node does not exist
    Exit('');

   Node := Node.FindNode(Name);

   if (Node = nil) Then // child (text) node not found
    Exit('');

   Result := Node.TextContent;
  End;

  // ReadIntegerValue
  Function ReadIntegerValue(const Node: TDOMNode; const Name: String; const Default: Integer=0): Integer;
  Var Tmp: String;
  Begin
   Tmp := ReadStringValue(Node, Name);

   if (not TryStrToInt(Tmp, Result)) Then
    Result := Default;
  End;

  // ReadFloatValue
  Function ReadFloatValue(const Node: TDOMNode; const Name: String; const Default: Integer=0): Extended;
  Var Tmp: String;
  Begin
   Tmp := ReadStringValue(Node, Name);

   if (not TryStrToFloat(Tmp, Result)) Then
    Result := Default;
  End;

Begin
 Result := False;

 FileName := MakeAbsolutePath(fFileName);
 Named    := True;
 Saved    := True;

 MainForm.setMainMenu(stDisabled);

 Try
  Try
   OldWorkingDir := GetCurrentDir;
   SetCurrentDir(ExtractFilePath(fFileName));

   { open and parse file }
   ReadXMLFile(Doc, FileName);
   Root := Doc.DocumentElement;

   { read config }
   Parent := Root.FindNode('config');

   Version        := ReadFloatValue(Parent, 'version');
   ProjectType    := TProjectType(ReadIntegerValue(Parent, 'project_type', ord(ptApplication)));
   CardCount      := ReadIntegerValue(Parent, 'card_count');
   OpenedCard     := ReadIntegerValue(Parent, 'opened_card');
   OutputFile     := ReadStringValue(Parent, 'output_file');
   HeaderFile     := ReadStringValue(Parent, 'header_file');
   BytecodeOutput := ReadStringValue(Parent, 'bytecode_output');

   { read compiler data }
   Parent := Root.FindNode('compiler');

   OtherCompilerSwitches := ReadStringValue(Parent, 'switches');
   IncludePath           := ReadStringValue(Parent, 'include_path');
   OptimizationLevel     := ReadIntegerValue(Parent, 'optimization_level', 2);

   CompilerSwitches := [];
   For Switch in TCompilerSwitches Do
    if (ReadIntegerValue(Parent, getCompilerSwitchName(Switch, False)) = 1) Then
     CompilerSwitches += [Switch];

   { read vm data }
   Parent := Root.FindNode('vm');

   OtherVMSwitches := ReadStringValue(Parent, 'switches');

   VMSwitches := [];
   For VMSwitch in TVMSwitches Do
    if (ReadIntegerValue(Parent, getVMSwitchName(VMSwitch, False)) = 1) Then
     VMSwitches += [VMSwitch];

   if (CardCount = 0) THen
    raise Exception.Create('File damaged: CardCount = 0');

   { read cards }
   For I := 0 To CardCount-1 Do
   Begin
    Parent := Root.FindNode('card_'+IntToStr(I));

    cCaption := ReadStringValue(Parent, 'caption');
    cFile    := MakeAbsolutePath(ReadStringValue(Parent, 'file'));

    if (CreateCardEx(cFile, cCaption, True) <> nil) Then
    Begin
     // update card's editor's caret position
     getCurrentEditor.CaretX := ReadIntegerValue(Parent, 'caret_x');
     getCurrentEditor.CaretY := ReadIntegerValue(Parent, 'caret_y');

     // read other card's data
     cIsMain := Boolean(ReadIntegerValue(Parent, 'is_main'));

     if (cIsMain) and (getMainCard <> nil) Then
      cIsMain := False; // there cannot be more than one `main-marked` card

     With getCurrentCard do
     Begin
      Named  := True;
      isMain := Boolean(cIsMain);
     End;
    End;
   End;
  Finally
   Doc.Free;
  End;
 Except
  SetCurrentDir(OldWorkingDir);
  Exit(False);
 End;

 { add new file to the recently-opened list }
 uMainForm.AddRecentlyOpened(FileName);

 { version check }
 if (Version = 0) Then
  Version := 0.1;

 if (Version < iVersion) Then // older than ours?
  Application.MessageBox(PChar(getLangValue(ls_msg_version_conflict_older)), PChar(getLangValue(ls_msg_warn)), MB_IconWarning);

 if (Version > iVersion) Then // newer than ours?
  Application.MessageBox(PChar(getLangValue(ls_msg_version_conflict_newer)), PChar(getLangValue(ls_msg_warn)), MB_IconWarning);

 { ... and do some other things }
 With MainForm do
 Begin
  setMainMenu(stEnabled);

  CodeEditor.Tabs.ActivePageIndex := OpenedCard;
  Caption                         := uMainForm.sCaption+' - '+FileName;
 End;

 CheckPaths;

 Exit(True);
End;

(* TProject.OpenCard *)
{
 Opens a new card from specified file; checks for duplicates.
}
Function TProject.OpenCard(const fFileName: String): TCard;
Begin
 Result := nil;

 if (not FileExists(fFileName)) Then // file not found
  Exit;

 Result := FindCard(fFileName);
 if (Result <> nil) Then // this card had been opened before
 Begin
  CodeEditor.Tabs.ActivePageIndex := CardList.IndexOf(Result); // focus card
  Exit;
 End;

 Result := CreateCard(fFileName, True);
End;

(* TProject.UpdateCards *)
{
 Updates all cards.
}
Procedure TProject.UpdateCards;
Var Card: TCard;
Begin
 For Card in CardList Do
  Card.Update;
End;

(* TProject.SwapCards *)
{
 Swaps two cards.
}
Procedure TProject.SwapCards(A, B: Integer);
Begin
 CardList.Exchange(A, B);
End;

(* TProject.CloseCard *)
{
 Closes card with specified ID.
}
Procedure TProject.CloseCard(ID: Integer);
Begin
 if (CardList = nil) Then
  Exit;

 // is the only opened card?
 if (CardList.Count <= 1) Then
 Begin
  Application.MessageBox(PChar(getLangValue(ls_msg_close_last_card)), PChar(getLangValue(ls_msg_info)), MB_IconInformation);
  Exit;
 End;

 // is main?
 if (CardList[ID].isMain) Then
 Begin
  Application.MessageBox(PChar(getLangValue(ls_msg_close_main_card)), PChar(getLangValue(ls_msg_info)), MB_IconInformation);
  Exit;
 End;

 // is modified?
 if (CardList[ID].SynEdit.Modified) Then
  if (not CardList[ID].Save(csrClosingCard)) Then
   Exit;

 CardList.Delete(ID);
 CodeEditor.Tabs.Pages[ID].Free;

 SaveIfPossible;
End;

(* TProject.CloseCardsExcluding *)
{
 Closes all cards excluding main and specified one.
}
Procedure TProject.CloseCardsExcluding(ID: Integer);
Var Card, Current: TCard;
    I            : Integer;
Begin
 Current := CardList[ID];

 I := 0;
 Repeat
  Card := CardList[I];

  // is main?
  if (Card.isMain) Then
  Begin
   Inc(I);
   Continue;
  End;

  // is modified?
  if (Card.SynEdit.Modified) Then
   if (not Card.Save(csrClosingCard)) Then
   Begin
    Inc(I);
    Continue;
   End;

  if (Card <> Current) Then
  Begin
   CardList.Remove(Card);
   Card.Free;
  End Else
   Inc(I);
 Until (I >= CardList.Count);

 SaveIfPossible;
End;

(* TProject.RaiseMessage *)
{
 Raises a message from the list.
}
Procedure TProject.RaiseMessage(ID: Integer);
Var Card: TCard;
Begin
 Card := FindCard(MessageList[ID]^.FileName);

 if (Card = nil) Then
 Begin
  Card := CreateCard(MessageList[ID]^.FileName, True);

  if (Card = nil) Then // couldn't open card
   Exit;
 End;

 Card.ErrorLine                  := MessageList[ID]^.Line;
 CodeEditor.Tabs.ActivePageIndex := CardList.IndexOf(Card);

 With Card do
 Begin
  SynEdit.CaretX := MessageList[ID]^.Char;
  SynEdit.CaretY := MessageList[ID]^.Line;
  SynEdit.Invalidate;

  CodeEditor.Tabs.SetFocus;
  Tab.SetFocus;
  SynEdit.SetFocus;
 End;
End;

(* TProject.RefreshControls *)
{
 Refreshes each card's controls.
}
Procedure TProject.RefreshControls;
Var Card: TCard;
Begin
 For Card in CardList Do
  Card.RefreshControls;
End;

(* TProject.Compile *)
{
 Compiles project.
}
Function TProject.Compile: Boolean;
Type TMessageIcon = (miNone, miHint, miWarn, miError);
Const MAX_BYTES = 10240;
Var sOutputFile: String;

    Switch: TCompilerSwitch;
    Card  : TCard;

    Process  : TProcess;
    InputFile: String = '';
    Output   : TStringList;
    NumBytes : LongInt;
    BytesRead: LongInt;

    TmpOutput: PChar;

    I, P, Line, Char              : Integer;
    Base, Message, mFileName, Posi: String;

    AnyError: Boolean = False;

  // AddUserSwitches
  Procedure AddUserSwitches;
  Var SwitchesList    : TStringList;
      Switches, Switch: String;
      I               : uint32;
  Begin
   SwitchesList      := TStringList.Create;
   SwitchesList.Text := StringReplace(OtherCompilerSwitches, '|', sLineBreak, [rfReplaceAll]); // get user switches

   Try
    For Switches in SwitchesList Do
    Begin
     Switches += ' ';
     Switch   := '';

     For I := 1 To Length(Switches) Do
      if (Switches[I] = ' ') Then
      Begin
       if (Length(Switch) = 0) Then
        Continue;

       Process.Parameters.Add(Switch);
       Switch := '';
      End Else
       Switch += Switches[I];
    End;
   Finally
    SwitchesList.Free;
   End;
  End;

  // AddText
  Procedure AddText(const Str: String; Icon: TMessageIcon=miNone; DataInt: Integer=0);
  Begin
   With CompileStatusForm.CompileStatus do
    With Items.Add(nil, Str) do
    Begin
     Data          := Pointer(DataInt);
     ImageIndex    := ord(Icon);
     SelectedIndex := ImageIndex;
    End;
  End;

  // AddError
  Procedure AddError(Line, Char: Integer; Base, Message, FileName: String);
  Var Card: TCard;
      Info: PMessage;
  Begin
   AnyError := True;

   { open card }
   Card := FindCard(FileName);
   if (Card = nil) Then
   Begin
    Card := CreateCard(FileName, True);
    if (Card = nil) Then // failed
     Exit;
   End;

   { modify card }
   With Card do
   Begin
    if (ErrorLine = -1) Then
    Begin
     ErrorLine := Line;
     With SynEdit do
     Begin
      CaretX := Char;
      CaretY := Line;
      Invalidate;
     End;
    End;

    New(Info);
    Info^.Line     := Line;
    Info^.Char     := Char;
    Info^.FileName := FileName;
    Info^.Text     := Message;
    MessageList.Add(Info);

    AddText(Base, miError, MessageList.Count);
   End;
  End;

  // AddMsg
  Procedure AddMsg(Icon: TMessageIcon; Line, Char: Integer; Base, Message, FileName: String);
  Var Card: TCard;
      Info: PMessage;
  Begin
   { open card }
   Card := FindCard(FileName);
   if (Card = nil) Then
   Begin
    Card := CreateCard(FileName, True);
    if (Card = nil) Then // failed
     Exit;
   End;

   New(Info);
   Info^.Line     := Line;
   Info^.Char     := Char;
   Info^.FileName := FileName;
   Info^.Text     := Message;
   MessageList.Add(Info);

   AddText(Base, Icon, MessageList.Count);
  End;
Begin
 if (not FileExists(getString(sCompilerFile))) Then // compiler file not found
 Begin
  Application.MessageBox(PChar(getLangValue(ls_msg_compiler_not_found)), PChar(getLangValue(ls_msg_error)), MB_IconError);
  Exit(False);
 End;

 if (not Save) Then
  Exit(False);

 CompileStatusForm.Show;

 MessageList.Clear;

 { each card }
 For Card in CardList Do
 Begin
  Card.ErrorLine       := -1;
  Card.SynEdit.Enabled := False;
 End;

 InputFile   := getMainCard.FileName;
 sOutputFile := MakeAbsolutePath(OutputFile);
 DeleteFile(sOutputFile); // remove previous output file

 Application.ProcessMessages; // process LCL

 With CompileStatusForm.CompileStatus.Items do
 Begin
  Clear; // clear previous build messages
  AddText(Format(getLangValue(ls_compilation_started), [TimeToStr(Time)]));

  Process            := TProcess.Create(nil);
  Process.Options    := [poUsePipes, poNoConsole];
  Process.Executable := getString(sCompilerFile);

  { generate command line }
  Process.Parameters.AddStrings(
  [
   '"'+InputFile+'"',
   '-includepath', '"'+IncludePath+'"',
   '-o', '"'+sOutputFile+'"'
  ]
  );

  // optimization level
  if (OptimizationLevel <> 0) Then
   Process.Parameters.Add('-O'+IntToStr(OptimizationLevel));

  // if application
  if (ProjectType = ptApplication) Then
   Process.Parameters.AddStrings(['-Cm', 'app']);

  // if library
  if (ProjectType = ptLibrary) Then
  Begin
   Process.Parameters.AddStrings(['-Cm', 'lib']);

   if (HeaderFile <> '') Then
    Process.Parameters.AddStrings(['-h', '"'+MakeAbsolutePath(HeaderFile)+'"']);
  End;

  // generate output mnemonic bytecode?
  if (BytecodeOutput <> '') Then
   Process.Parameters.AddStrings(['-bytecode', '"'+MakeAbsolutePath(BytecodeOutput)+'"']);

  // add compile switches
  For Switch in CompilerSwitches Do
   Process.Parameters.Add('-'+getCompilerSwitchName(Switch));

  AddUserSwitches;

  { run compiler }
  Process.Execute;

  BytesRead := 0;

  { read output }
  TmpOutput      := GetMem(MAX_BYTES+1);
  CompilerOutput := Process.Parameters.Text+#13#10#13#10;

  While (Process.Running) Do
  Begin
   NumBytes := Process.Output.Read(TmpOutput[0], MAX_BYTES);

   TmpOutput[NumBytes] := #0;
   CompilerOutput += TmpOutput;

   Inc(BytesRead, NumBytes);
   Sleep(50);
  End;

  Repeat
   NumBytes := Process.Output.Read(TmpOutput[0], MAX_BYTES);

   TmpOutput[NumBytes] := #0;
   CompilerOutput += TmpOutput;

   Inc(BytesRead, NumBytes);
  Until (NumBytes <= 0);

  FreeMem(TmpOutput);
  Process.Free;

  { parse compiler output }
  Output      := TStringList.Create;
  Output.Text := CompilerOutput;

  For I := 0 To Output.Count-1 Do
  Begin
   Base := Output[I];

   (* @TODO: don't repeat yourself (!) *)

   { error }
   P := Pos('Error:', Base);
   if (P > 0) Then
   Begin
    Message   := Trim(Copy(Base, P+6, Length(Base)));
    mFileName := Trim(Copy(Base, 1, Pos('(', Base)-1));
    Posi      := Trim(Copy(Base, Pos('(', Base)+1, Pos(')', Base)-Pos('(', Base)-1));

    Line := StrToInt(Copy(Posi, 1, Pos(',', Posi)-1));
    Char := StrToInt(Copy(Posi, Pos(',', Posi)+1, Length(Posi)));

    AddError(Line, Char+1, Base, Message, mFileName);

    Continue;
   End;

   { warning }
   P := Pos('Warning:', Base);
   if (P > 0) Then
   Begin
    Message   := Trim(Copy(Base, P+8, Length(Base)));
    mFileName := Trim(Copy(Base, 1, Pos('(', Base)-1));
    Posi      := Trim(Copy(Base, Pos('(', Base)+1, Pos(')', Base)-Pos('(', Base)-1));

    Line := StrToInt(Copy(Posi, 1, Pos(',', Posi)-1));
    Char := StrToInt(Copy(Posi, Pos(',', Posi)+1, Length(Posi)));

    AddMsg(miWarn, Line, Char+1, Base, Message, mFileName);

    Continue;
   End;

   { hint }
   P := Pos('Hint:', Base);
   if (P > 0) Then
   Begin
    Message   := Trim(Copy(Base, P+5, Length(Base)));
    mFileName := Trim(Copy(Base, 1, Pos('(', Base)-1));
    Posi      := Trim(Copy(Base, Pos('(', Base)+1, Pos(')', Base)-Pos('(', Base)-1));

    Line := StrToInt(Copy(Posi, 1, Pos(',', Posi)-1));
    Char := StrToInt(Copy(Posi, Pos(',', Posi)+1, Length(Posi)));

    AddMsg(miHint, Line, Char+1, Base, Message, mFileName);

    Continue;
   End;

   { note }
   P := Pos('Note:', Base);
   if (P > 0) Then
   Begin
    Message   := Trim(Copy(Base, P+5, Length(Base)));
    mFileName := Trim(Copy(Base, 1, Pos('(', Base)-1));
    Posi      := Trim(Copy(Base, Pos('(', Base)+1, Pos(')', Base)-Pos('(', Base)-1));

    Line := StrToInt(Copy(Posi, 1, Pos(',', Posi)-1));
    Char := StrToInt(Copy(Posi, Pos(',', Posi)+1, Length(Posi)));

    AddMsg(miHint, Line, Char+1, Base, Message, mFileName);

    Continue;
   End;
  End;

  { check if the output file exists }
  if (not AnyError) and (not FileExists(sOutputFile)) Then
   AddError(0, 0, Format(getLangValue(ls_output_not_found), [sOutputFile]), '', getMainCard.getFileName);

  { show finish message }
  if (not AnyError) Then
   AddText(Format(getLangValue(ls_compilation_finished), [TimeToStr(Time), OutputFile])) Else
   AddText(Format(getLangValue(ls_compilation_stopped), [TimeToStr(Time)]));
 End;

 For Card in CardList Do // enable editors
  Card.SynEdit.Enabled := True;

 if (MessageList.Count > 0) Then // raise first message
  RaiseMessage(0);

 Application.ProcessMessages; // process LCL

 Exit(not AnyError);
End;

(* TProject.Run *)
{
 Runs project.
}
Procedure TProject.Run;
Var TmpCaption, sOutputFile, CCommandLine: String;
    Switch                               : TVMSwitch;
    Tries                                : Integer = 0;
Begin
 if (VMProcess <> nil) Then // another VM process is already running
  Exit;

 if (ProjectType = ptLibrary) Then // cannot run a library
  Exit;

 if (not FileExists(getString(sVMFile))) Then // check virtual machine
 Begin
  Application.MessageBox(PChar(getLangValue(ls_msg_vm_not_found)), PChar(getLangValue(ls_msg_error)), MB_IconError);
  Exit;
 End;

 { check path }
 sOutputFile := MakeAbsolutePath(OutputFile);

 { wait for file }
 Sleep(25);
 While (not FileExists(sOutputFile)) Do
 Begin
  Inc(Tries);
  Sleep(50);

  if (Tries > 20) Then // output file not found
  Begin
   Application.MessageBox(PChar(Format(getLangValue(ls_outputfile_not_found), [sOutputFile])), PChar(getLangValue(ls_msg_error)), MB_IconError);
   Exit;
  End;
 End;

 { generate command line }
 CCommandLine := '';

 For Switch in VMSwitches Do
  CCommandLine += ' -'+getVMSwitchName(Switch, True);

 CCommandLine += ' '+OtherVMSwitches;

 { run program }
 VMProcess := TProcess.Create(nil);

 With VMProcess do
 Begin
  Options          := [];
  CurrentDirectory := ExtractFilePath(ParamStr(0));

  {$IFDEF LINUX}
   Options += [poUsePipes];
   CommandLine := 'xterm -e ''sh run.sh "'+getString(sVMFile)+'" "'+sOutputFile+'" "'+CCommandLine+'";read''';
  {$ELSE}
   CommandLine := '"'+getString(sVMFile)+'" "'+sOutputFile+'" '+CCommandLine;
  {$ENDIF}
 End;

 TmpCaption       := MainForm.Caption;
 MainForm.Caption := sCaption+' [ '+getLangValue(ls_vm_running)+' ]';

 VMProcess.Execute;
 While (VMProcess <> nil) and (VMProcess.Running) Do
  Application.ProcessMessages;

 MainForm.Caption := TmpCaption;

 FreeAndNil(VMProcess);
End;

(* TProject.StopProgram *)
{
 Halts virtual machine.
}
Procedure TProject.StopProgram;
Begin
 if (VMProcess = nil) Then
  Application.MessageBox(PChar(getLangValue(ls_vm_instance_not_running)), PChar(getLangValue(ls_msg_info)), MB_IconInformation) Else
  Begin
   VMProcess.Terminate(0);
   FreeAndNil(VMProcess);
  End;
End;

(* TProject.isEverythingSaved *)
{
 Returns `true` if project and all of its card are saved, `false` otherwise.
}
Function TProject.isEverythingSaved: Boolean;
Var Card: TCard;
Begin
 Result := True;

 if (not Saved) Then
  Exit(False);

 For Card in CardList Do
  if (Card.SynEdit.Modified) Then
   Exit(False);
End;

(* TProject.getCurrentCard *)
{
 Returns current card class.
}
Function TProject.getCurrentCard: TCard;
Begin
 if (CardList.Count = 0) Then // shouldn't happen
  Exit(nil);

 With CodeEditor.Tabs do
 Begin
  if (ActivePageIndex < 0) Then
   ActivePageIndex := 0;
  if (ActivePageIndex >= CardList.Count) Then
   ActivePageIndex := CardList.Count-1;

  Exit(CardList[ActivePageIndex]);
 End;
End;

(* TProject.getCurrentEditor *)
{
 Returns current card's editor.
}
Function TProject.getCurrentEditor: TSynEdit;
Begin
 if (CardList.Count = 0) Then
  Exit(nil);

 Result := getCurrentCard.SynEdit;
End;

(* TProject.FindIdentifier *)
{
 Returns parsed identifier at specified position or `nil` when such identifier couldn't be found.
}
Function TProject.FindIdentifier(const CaretXY: TPoint): PIdentifier;
Var Card : TCard;
    Ident: PIdentifier;
Begin
 Card := getCurrentCard;

 Card.Parse;

 if (Card.IdentifierList = nil) Then
  Exit(nil);

 For Ident in Card.IdentifierList Do
  if (Ident^.Char = CaretXY.X) and (Ident^.Line = CaretXY.Y) Then
   Exit(Ident);

 Exit(nil);
End;

(* TProject.JumpToDeclaration *)
{
 Jumps to identifier's declaration.
}
Procedure TProject.JumpToDeclaration(const Ident: PIdentifier);
Var Card: TCard;
Begin
 if (Ident = nil) Then
  Exit;

 Card := FindCard(Ident^.RefFileName);
 if (Card = nil) Then
 Begin
  Card := CreateCard(Ident^.RefFileName, True);

  if (Card = nil) Then // eg.file not found
   Exit;
 End;

 With Card.SynEdit do
 Begin
  CaretX := Ident^.RefChar;
  CaretY := Ident^.RefLine;
  SelEnd := SelStart+Ident^.RefLen;
 End;

 CodeEditor.Tabs.ActivePageIndex := CardList.IndexOf(Card);
End;

(* TProject.UpdateIdentifierList *)
Procedure TProject.UpdateIdentifierList;
Var Card  : TCard;
    Symbol: TSymbol;

    // MarkAllAsRemoved
    Procedure MarkAllAsRemoved;
    Var Data: PNodeData;
        Node: PVirtualNode;
    Begin
     With IdentifierListForm.IdentifierList do
     Begin
      Node := GetFirst;
      if (Node = nil) Then
       Exit;

      Repeat
       Node := GetNext(Node);

       Data := GetNodeData(Node);
       if (Data <> nil) Then
        Data^.Removed := True;

       if (GetNext(Node) = Node) Then
        Break;
      Until (Node = GetLast);
     End;
    End;

    // DeleteRemoved
    Procedure DeleteRemoved;
    Var Data: PNodeData;
        Node: PVirtualNode;
    Label Again;
    Begin
     With IdentifierListForm.IdentifierList do
     Begin
     Again:
      Node := GetFirst;
      if (Node = nil) Then
       Exit;

      Repeat
       if (Node = nil) Then
        Node := GetFirst Else
        Node := GetNext(Node);

       Data := GetNodeData(Node);
       if (Data <> nil) and (Data^.Removed) Then
       Begin
        DeleteNode(Node);
        goto Again;
       End;

       if (GetNext(Node) = Node) Then
        Break;
      Until (Node = GetLast);
     End;
    End;

    // FindNode
    Function FindNode(NParent: PVirtualNode; const NCaption: String): PVirtualNode;
    Var Data: TNodeData;
    Begin
     Result := nil;

     With IdentifierListForm.IdentifierList do
     Begin
      if (GetFirst = nil) Then
       Exit;

      if (NParent = nil) Then
       NParent := RootNode;

      Repeat
       if (Result = nil) Then
        Result := GetFirst Else
        Result := GetNext(Result);

       Data := PNodeData(GetNodeData(Result))^;

       if (AnsiCompareStr(Data.Caption, NCaption) = 0) and (Result^.Parent = NParent) Then
        Exit;
      Until (Result = GetLast);

      Exit(nil);
     End;
    End;

    // AddNode
    Function AddNode(const Node: PVirtualNode; const Symbol: TSymbol; const ImageIndex: Integer): PVirtualNode;
    Var Data: PNodeData;
    Begin
     With IdentifierListForm do
     Begin
      Result := FindNode(Node, Symbol.getName);
      if (Result <> nil) Then
      Begin
       Data          := IdentifierList.GetNodeData(Result);
       Data^.Symbol  := Symbol;
       Data^.Removed := False;
       Exit;
      End;

      Result := IdentifierList.AddChild(Node);

      Data          := IdentifierList.GetNodeData(Result);
      Data^.Typ     := ndSymbol;
      Data^.Symbol  := Symbol;
      Data^.Caption := Symbol.getName;
      Data^.Removed := False;
     End;
    End;

    // AddNode
    Function AddNode(const Node: PVirtualNode; const Value: String; const ImageIndex: Integer): PVirtualNode;
    Var Data: PNodeData;
    Begin
     With IdentifierListForm do
     Begin
      Result := FindNode(Node, Value);
      if (Result <> nil) Then
      Begin
       Data          := IdentifierList.GetNodeData(Result);
       Data^.Removed := False;
       Exit;
      End;

      Result := IdentifierList.AddChild(Node);

      Data          := IdentifierList.GetNodeData(Result);
      Data^.Typ     := ndText;
      Data^.Symbol  := nil;
      Data^.Caption := Value;
     End;
    End;

    // ParseSymbol
    Procedure ParseSymbol(Node: PVirtualNode; Symbol: TSymbol);
    Var Ns, Types, Funcs, Vars, Cnsts: PVirtualNode;
        Tmp                          : TSymbol;
    Begin
     if (Symbol = nil) Then // shouldn't really happen
      Exit;

     if (AnsiCompareFileName(Symbol.getPhysSymbol.Token.FileName, Card.FileName) <> 0) Then
      Exit;

     { namespace }
     if (Symbol.Typ = stNamespace) Then
     Begin
      Ns := AddNode(Node, Symbol, 0); // @TODO: nice icon

      Types := AddNode(Ns, getLangValue(ls_types), 0);
      Funcs := AddNode(Ns, getLangValue(ls_functions), 0);
      Vars  := AddNode(Ns, getLangValue(ls_variables), 0);
      Cnsts := AddNode(Ns, getLangValue(ls_constants), 0);

      For Tmp in Symbol.mNamespace.SymbolList Do
       Case Tmp.Typ of
        stType    : ParseSymbol(Types, Tmp);
        stFunction: ParseSymbol(Funcs, Tmp);
        stVariable: ParseSymbol(Vars, Tmp);
        stConstant: ParseSymbol(Cnsts, Tmp);
       End;

      With IdentifierListForm.IdentifierList do // remove empty nodes
      Begin
       if (Types^.ChildCount = 0) Then
        DeleteNode(Types);

       if (Funcs^.ChildCount = 0) Then
        DeleteNode(Funcs);

       if (Vars^.ChildCount = 0) Then
        DeleteNode(Vars);

       if (Cnsts^.ChildCount = 0) Then
        DeleteNode(Cnsts);

       if (Symbol.getName = 'self') and (Ns^.ChildCount = 0) Then // @TODO
        DeleteNode(Ns);
      End;
     End Else

     { function }
     if (Symbol.Typ = stFunction) Then
     Begin
      AddNode(Node, Symbol, 0); // @TODO: nice icon
     End Else

     { type }
     if (Symbol.Typ = stType) Then
     Begin
      AddNode(Node, Symbol, 0); // @TODO: nice icon
     End Else

     { variable }
     if (Symbol.Typ = stVariable) Then
     Begin
      AddNode(Node, Symbol, 0); // @TODO: nice icon
     End;

     { constant }
     if (Symbol.Typ = stConstant) Then
     Begin
      AddNode(Node, Symbol, 0);
     End;
    End;

Begin
 Card := getCurrentCard;

 if (Card = nil) Then
  Exit;

 Card.Parse;

 if (ParseError.Any) Then // do not update tree, if any error was raised.
  Exit;

 With IdentifierListForm do
 Begin
  IdentifierList.BeginUpdate;

  MarkAllAsRemoved;

  if (Card.CodeScanner <> nil) Then
   With Card.CodeScanner do
   Begin
    For Symbol in getSymbolList Do
     ParseSymbol(nil, Symbol);
   End;

  // @TODO: show program's dependencies (includes)

  DeleteRemoved;

  IdentifierList.EndUpdate;
 End;
End;
End.
