{$MODE OBJFPC}
{$H+}

Unit mProject;

 Interface
 Uses uMainForm, Windows, Classes, ComCtrls, Controls, Graphics, SynEdit, SynEditSScript, FGL;

 Type TProjectType = (ptApplication, ptLibrary);

 { TCard }
 Type TCardSaveReason = (csrClosingCard);

 Type TCard = Class
               Private
                isMain, Named    : Boolean;
                FileName, Caption: String;
                SynEdit          : TSynEdit;
                Tab              : TTabSheet;
                ErrorLine        : Integer;

                Procedure Editor_OnKeyPress(Sender: TObject; var Key: Char);
                Procedure Editor_OnKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
                Procedure Editor_OnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
                Procedure Editor_OnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
                Procedure Editor_OnClick(Sender: TObject);
                Procedure Editor_OnSpecialLineColors(Sender: TObject; Line: Integer; var Special: Boolean; var FG, BG: TColor);

               Public
                Property getFileName: String read FileName;

                Constructor Create(const fFileName, fCaption: String);
                Destructor Destroy; override;

                Function Save(const fFileName: String=''): Boolean;
                Function Save(const Reason: TCardSaveReason): Boolean;

                Procedure Update;
                Procedure RefreshControls;

                Procedure ReCaption(const NewCaption: String);
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
 Type TCompilerSwitch = (_ninit, _Or, _Of, _Op, _O1, _dbg);
 Type TCompilerSwitches = Set of TCompilerSwitch;

 // TVMSwitches
 Type TVMSwitch = (c_time, c_wait);
 Type TVMSwitches = Set of TVMSwitch;

 { TProject }
 Type TProject = Class
                  Private
                   CardList   : TCardList;
                   MessageList: TMessageList;

                   Function CreateCard(const cFileName: String; LoadFile: Boolean): Boolean;
                   Function CreateCardEx(const cFileName, cCaption: String; LoadFile: Boolean): Boolean;
                   Function FindCard(const cFileName: String): Integer;
                   Function getMainCard: TCard;

                   Function MakeFullPath(const FileName: String): String;
                   Function MakeRelativePath(const FileName: String): String;
                   Procedure CheckPaths;

                   Procedure SaveIfCan;

                  Public
                   // project state
                   Named, Saved: Boolean;

                   // paths and other text data
                   FileName, CompilerFile, VMFile: String;
                   IncludePath, OutputFile       : String;
                   HeaderFile, BytecodeOutput    : String;

                   // project info
                   ProjectType: TProjectType;

                   // compiler
                   CompilerSwitches     : TCompilerSwitches;
                   OtherCompilerSwitches: String;

                   // vm
                   VMSwitches     : TVMSwitches;
                   OtherVMSwitches: String;

                   // methods
                   Constructor Create;
                   Destructor Destroy; override;

                   Procedure NewProject(const Typ: TProjectType);
                   Procedure NewNoNameCard;

                   Function Save(const fFileName: String=''): Boolean;
                   Procedure SaveCurrentCard;
                   Procedure SaveCurrentCardAs;
                   Function Open(const fFileName: String=''): Boolean;
                   Function OpenCard(const fFileName: String): Boolean;

                   Procedure UpdateCards;
                   Procedure SwapCards(A, B: Integer);
                   Procedure CloseCard(ID: Integer);
                   Procedure CloseCardsExcluding(ID: Integer);
                   Procedure RaiseMessage(ID: Integer);

                   Procedure RefreshControls;

                   Function Compile: Boolean;
                   Procedure Run;

                   Function isEverythingSaved: Boolean;
                   Function getCurrentCard: TCard;
                   Function getCurrentEditor: TSynEdit;
                  End;

 Function getSwitchName(const S: TCompilerSwitch; DeleteFirstChar: Boolean=True): String;
 Function getVMSwitchName(const S: TVMSwitch; DeleteFirstChars: Boolean=True): String;

 Implementation
Uses mSettings, mLanguages, Dialogs, SysUtils, Forms, DOM, XMLWrite, XMLRead, TypInfo, Process;

Function PathIsRelative(pszPath: PChar): Boolean; stdcall; external 'shlwapi.dll' name 'PathIsRelativeA';

{ getSwitchName }
Function getSwitchName(const S: TCompilerSwitch; DeleteFirstChar: Boolean=True): String;
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

{ TCard.Editor_OnKeyPress }
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

{ TCard.Editor_OnKeyDown }
Procedure TCard.Editor_OnKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
Begin
 ErrorLine := -1;
 SynEdit.Invalidate;
End;

{ TCard.Editor_OnMouseUp }
Procedure TCard.Editor_OnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
Begin
 ErrorLine := -1;
 SynEdit.Invalidate;
End;

{ TCard.Editor_OnMouseDown }
Procedure TCard.Editor_OnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
Begin
 ErrorLine := -1;
 SynEdit.Invalidate;
End;

{ TCard.Editor_OnClick }
Procedure TCard.Editor_OnClick(Sender: TObject);
Begin
 ErrorLine := -1;
 SynEdit.Invalidate;
End;

{ TCard.Editor_OnSpecialLineColors }
Procedure TCard.Editor_OnSpecialLineColors(Sender: TObject; Line: Integer; var Special: Boolean; var FG, BG: TColor);
Begin
 if (Line = ErrorLine) Then
 Begin
  BG      := RGB(255, 160, 80);
  Special := True;
 End;
End;

{ TCard.Create }
Constructor TCard.Create(const fFileName, fCaption: String);
Begin
 FileName := fFileName;
 Caption  := fCaption;

 ErrorLine := -1;

 Named  := False;
 isMain := False;

 SynEdit := TSynEdit.Create(MainForm);
 Tab     := TTabSheet.Create(MainForm);

 { Tab }
 Tab.Caption     := fCaption;
 Tab.PageControl := MainForm.Tabs;
 Tab.PopupMenu   := MainForm.Tabs.PopupMenu;

 { SynEdit }
 With SynEdit do
 Begin
  Highlighter := THighlighter.Create(SynEdit);
  Parent      := Tab;
  Align       := alClient;
  PopupMenu   := MainForm.SynEditPopup;

  OnKeyPress          := @Editor_OnKeyPress;
  OnKeyDown           := @Editor_OnKeyDown;
  OnMouseUp           := @Editor_OnMouseUp;
  OnMouseDown         := @Editor_OnMouseDown;
  OnClick             := @Editor_OnClick;
  OnSpecialLineColors := @Editor_OnSpecialLineColors;

  Modified := False;
 End;

 { Tabs }
 With MainForm.Tabs do
  ActivePageIndex := PageCount-1;

 SynEdit.SetFocus;
End;

{ TCard.Destroy }
Destructor TCard.Destroy;
Begin
 SynEdit.Free;
 Tab.Destroy;
End;

{ TCard.Save }
Function TCard.Save(const fFileName: String=''): Boolean;
Begin
 Result := False;

 if (not Named) and (fFileName = '') Then
 Begin
  { open save dialog }
  With TSaveDialog.Create(MainForm) do
  Begin
   Try
    Title    := getLangValue('module_saving');
    Filter   := getLangValue('filter_module');
    FileName := ExtractFileName(self.FileName);

    if (Execute) Then
    Begin
     self.FileName := FileName;
     self.Named    := True;

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

{ TCard.Save }
Function TCard.Save(const Reason: TCardSaveReason): Boolean;
Begin
 Case MessageDlg(getLangValue('title_card_close'), getLangValue('msg_card_close'), mtWarning, mbYesNoCancel, '') of
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

{ TCard.Update }
Procedure TCard.Update;
Begin
 // update card's caption
 if (SynEdit.Modified) Then
  Tab.Caption := '* '+Caption Else
  Tab.Caption := Caption;
End;

{ TCard.RefreshControls }
Procedure TCard.RefreshControls;
Begin
 With SynEdit do
 Begin
  // recreate highlighter
  Highlighter.Free;
  Highlighter := THighlighter.Create(SynEdit);

  // update SynEdit
  With SynEdit do
  Begin
   if (mSettings.getBoolean(sScrollPastEOL)) Then
    Options := Options + [eoScrollPastEOL] Else
    Options := Options - [eoScrollPastEOL];
  End;
 End;
End;

{ TCard.ReCaption }
Procedure TCard.ReCaption(const NewCaption: String);
Begin
 Caption     := NewCaption;
 Tab.Caption := Caption;
End;

{ TProject.CreateCard }
Function TProject.CreateCard(const cFileName: String; LoadFile: Boolean): Boolean;
Begin
 Result := CreateCardEx(cFileName, ExtractFileName(cFileName), LoadFile);
End;

{ TProject.CreateCardEx }
Function TProject.CreateCardEx(const cFileName, cCaption: String; LoadFile: Boolean): Boolean;
Var Card: TCard;
    Text: String;
Begin
 Result := False;
 Card   := TCard.Create(cFileName, cCaption);

 if (LoadFile) Then
 Begin
  if (FileExists(cFileName)) Then
  Begin
   With Card do
   Begin
    Named    := True;
    Saved    := True;
    FileName := cFileName;

    SynEdit.Lines.LoadFromFile(cFileName);
   End;
  End Else
  Begin
   Card.Free;
   Text := Format(getLangValue('msg_file_not_found'), [cFileName]);
   Application.MessageBox(PChar(Text), PChar(getLangValue('msg_err')), MB_IconError);
   Exit;
  End;
 End Else
 Begin
  With Card do
  Begin
   Named := False;
   Saved := False;

   SynEdit.Modified := True;
  End;
 End;

 CardList.Add(Card);
 Exit(True);
End;

{ TProject.FindCard }
Function TProject.FindCard(const cFileName: String): Integer;
Var I: Integer;
Begin
 Result := -1;

 For I := 0 To CardList.Count-1 Do
  if (CardList[I].FileName = cFileName) Then
   Exit(I);
End;

{ TProject.getMainCard }
Function TProject.getMainCard: TCard;
Var Card: TCard;
Begin
 Result := nil;

 For Card in CardList Do
  if (Card.isMain) Then
   Exit(Card);
End;

{ TProject.CheckPaths }
Procedure TProject.CheckPaths;
Var Fail: Boolean;
Begin
 Case ProjectType of
  ptApplication: Fail := (not FileExists(CompilerFile)) or (not FileExists(VMFile)); // an application needs a compiler and a virtual machine
  ptLibrary    : Fail := (not FileExists(CompilerFile)); // a library needs only a compiler
  else Fail := True;
 End;

 if (Fail) Then
  Application.MessageBox(PChar(getLangValue('msg_compiler_or_vm_not_found')), PChar(getLangValue('msg_warn')), MB_IconWarning);
End;

{ TProject.SaveIfCan }
Procedure TProject.SaveIfCan;
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

{ TProject.MakeFullPath }
Function PathCanonicalize(lpszDst: PChar; lpszSrc: PChar): LongBool; stdcall; external 'shlwapi.dll' name 'PathCanonicalizeA';

Function TProject.MakeFullPath(const FileName: String): String;
Var Directory: String;
    Dst      : Array[0..MAX_PATH-1] of Char;
Begin
 Directory := ExtractFilePath(self.FileName);

 if (PathIsRelative(PChar(FileName))) Then
 Begin
  PathCanonicalize(@Dst[0], PChar(IncludeTrailingBackslash(Directory) + FileName));
  Result := Dst;
 End Else
  Result := FileName;
End;

{ TProject.MakeRelativePath }
Function PathRelativePathTo(pszPath: PChar; pszFrom: PChar; dwAttrFrom: DWORD; pszTo: PChar; dwAtrTo: DWORD): LongBool; stdcall; external 'shlwapi.dll' name 'PathRelativePathToA';

Function TProject.MakeRelativePath(const FileName: String): String;
Var Directory: String;
    Path     : Array[0..MAX_PATH-1] of Char;
Begin
 Directory := ExtractFilePath(self.FileName);

 if (PathIsRelative(PChar(FileName))) Then // path is already relative
  Result := FileName Else
  Begin
   PathRelativePathTo(@Path[0], PChar(Directory), FILE_ATTRIBUTE_DIRECTORY, PChar(FileName), 0);
   Result := Path;
  End;
End;

{ TProject.Create }
Constructor TProject.Create;
Begin
 CardList    := TCardList.Create;
 MessageList := TMessageList.Create;
 ProjectType := ptApplication;
End;

{ TProject.Destroy }
Destructor TProject.Destroy;
Var Card: TCard;
Begin
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

{ TProject.NewProject }
Procedure TProject.NewProject(const Typ: TProjectType);
Begin
 Named := False;
 Saved := False;

 CompilerSwitches      := [_O1]; // `-O1` is enabled by default
 OtherCompilerSwitches := '';

 VMSwitches      := [c_wait]; // `-wait` is switched by default
 OtherVMSwitches := '';

 IncludePath    := '$file;$compiler';
 ProjectType    := Typ;
 OutputFile     := '';
 HeaderFile     := '';
 BytecodeOutput := '';

 CardList.Clear;
 MessageList.Clear;

 CompilerFile := getString(sCompilerFile);
 VMFile       := getString(sVMFile);

 CheckPaths;

 CreateCard('main.ss', False);

 { sample program }
 With getCurrentEditor.Lines do
 Begin
  if (ProjectType = ptApplication) Then
  Begin
   Add('@("stdlib\\stdio.ss")');
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

 With getCurrentCard do
 Begin
  isMain := True;
 End;
End;

{ TProject.NewNoNameCard }
Procedure TProject.NewNoNameCard;
Var I   : Integer;
    Name: String;
Begin
 For I := 0 To 256 Do
 Begin
  Name := 'bez_nazwy_'+IntToStr(I)+'.ss';
  if (FindCard(Name) = -1) Then
  Begin
   CreateCard(Name, False);
   Exit;
  End;
 End;
End;

{ TProject.Save }
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
    Title  := getLangValue('project_saving');
    Filter := getLangValue('filter_project');

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
 FileName   := MakeFullPath(FileName);

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
  WriteValue(Parent, 'compiler', CompilerFile);
  WriteValue(Parent, 'vm', VMFile);
  WriteValue(Parent, 'card_count', CardList.Count);
  WriteValue(Parent, 'opened_card', MainForm.Tabs.ActivePageIndex);
  WriteValue(Parent, 'output_file', OutputFile);
  WriteValue(Parent, 'header_file', HeaderFile);
  WriteValue(Parent, 'bytecode_output', BytecodeOutput);

  Root.AppendChild(Parent);

  { save compiler data }
  Parent := Doc.CreateElement('compiler');

  WriteValue(Parent, 'switches', OtherCompilerSwitches);
  WriteValue(Parent, 'include_path', IncludePath);

  For Switch in TCompilerSwitches Do
   WriteValue(Parent, getSwitchName(Switch, False), Integer(Switch in CompilerSwitches));

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
     Case MessageDlg(getLangValue('module_saving'), getLangValue('msg_module_saving'), mtWarning, mbYesNo, 0) of
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

{ TProject.SaveCurrentCard }
Procedure TProject.SaveCurrentCard;
Begin
 if (not Named) Then
  Exit;

 CardList[MainForm.Tabs.ActivePageIndex].Save;
End;

{ TProject.SaveCurrentCard }
Procedure TProject.SaveCurrentCardAs;
Var oldFileName: String;
Begin
 if (not Named) Then
  Exit;

 With CardList[MainForm.Tabs.ActivePageIndex] do
 Begin
  oldFileName := FileName;
  Named       := False;
  if (Save) Then
   DeleteFile(oldFileName) Else
   Named := True;
 End;
End;

{ TProject.Open }
Function TProject.Open(const fFileName: String=''): Boolean;
Var Doc         : TXMLDocument;
    Root, Parent: TDOMNode;
    CardCount, I: Integer;

    Switch  : TCompilerSwitch;
    VMSwitch: TVMSwitch;

    OpenedCard      : Integer;
    cCaption, cFile : String;
    cIsMain         : Boolean;

    Version: Extended;

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
Function ReadIntegerValue(Node: TDOMNode; Name: String): Integer;
Var Tmp: String;
Begin
 Tmp := ReadStringValue(Node, Name);

 if (not TryStrToInt(Tmp, Result)) Then
  Result := 0;
End;

// ReadFloatValue
Function ReadFloatValue(Node: TDOMNode; Name: String): Extended;
Var Tmp: String;
Begin
 Tmp := ReadStringValue(Node, Name);

 if (not TryStrToFloat(Tmp, Result)) Then
  Result := 0;
End;

Begin
 Result := False;

 FileName := MakeFullPath(fFileName);
 Named    := True;
 Saved    := True;

 Try
  Try
   { open and parse file }
   ReadXMLFile(Doc, FileName);
   Root := Doc.DocumentElement;

   { read config }
   Parent := Root.FindNode('config');

   Version        := ReadFloatValue(Parent, 'version');
   ProjectType    := TProjectType(ReadIntegerValue(Parent, 'project_type'));
   CompilerFile   := ReadStringValue(Parent, 'compiler');
   VMFile         := ReadStringValue(Parent, 'vm');
   CardCount      := ReadIntegerValue(Parent, 'card_count');
   OpenedCard     := ReadIntegerValue(Parent, 'opened_card');
   OutputFile     := ReadStringValue(Parent, 'output_file');
   HeaderFile     := ReadStringValue(Parent, 'header_file');
   BytecodeOutput := ReadStringValue(Parent, 'bytecode_output');

   { read compiler data }
   Parent := Root.FindNode('compiler');

   OtherCompilerSwitches := ReadStringValue(Parent, 'switches');
   IncludePath           := ReadStringValue(Parent, 'include_path');

   CompilerSwitches := [];
   For Switch in TCompilerSwitches Do
    if (ReadIntegerValue(Parent, getSwitchName(Switch, False)) = 1) Then
     CompilerSwitches += [Switch];

   { read vm data }
   Parent := Root.FindNode('vm');

   OtherVMSwitches := ReadStringValue(Parent, 'switches');

   VMSwitches := [];
   For VMSwitch in TVMSwitches Do
    if (ReadIntegerValue(Parent, getVMSwitchName(VMSwitch, False)) = 1) Then
     VMSwitches += [VMSwitch];

   { read cards }
   For I := 0 To CardCount-1 Do
   Begin
    Parent := Root.FindNode('card_'+IntToStr(I));

    cCaption := ReadStringValue(Parent, 'caption');
    cFile    := MakeFullPath(ReadStringValue(Parent, 'file'));

    if (CreateCardEx(cFile, cCaption, True)) Then
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
  Exit(False);
 End;

 // add new file to the recently-opened list
 uMainForm.AddRecentlyOpened(FileName);

 // version check
 if (Version = 0) Then
  Version := 0.1;

 if (Version < iVersion) Then
  Application.MessageBox(PChar(getLangValue('msg_version_conflict_old')), PChar(getLangValue('msg_warn')), MB_IconWarning);

 if (Version > iVersion) Then
  Application.MessageBox(PChar(getLangValue('msg_version_conflict_new')), PChar(getLangValue('msg_warn')), MB_IconWarning);

 // ... and do some other things
 MainForm.Tabs.ActivePageIndex := OpenedCard;

 CheckPaths;

 MainForm.Caption := uMainForm.sCaption+' - '+FileName;
 Exit(True);
End;

{ TProject.OpenCard }
Function TProject.OpenCard(const fFileName: String): Boolean;
Var I: Integer;
Begin
 Result := False;

 if (not FileExists(fFileName)) Then // file not found
  Exit(False);

 I := FindCard(fFileName);
 if (I <> -1) Then // this card had been opened before
 Begin
  MainForm.Tabs.ActivePageIndex := I; // focus card
  Exit(True);
 End;

 Result := CreateCard(fFileName, True);
End;

{ TProject.UpdateCards }
Procedure TProject.UpdateCards;
Var Card: TCard;
Begin
 For Card in CardList Do
  Card.Update;
End;

{ TProject.SwapCards }
Procedure TProject.SwapCards(A, B: Integer);
Begin
 CardList.Exchange(A, B);
End;

{ TProject.CloseCard }
Procedure TProject.CloseCard(ID: Integer);
Begin
 // is the only opened card?
 if (CardList.Count <= 1) Then
 Begin
  Application.MessageBox(PChar(getLangValue('msg_close_last_card')), PChar(getLangValue('msg_info')), MB_IconInformation);
  Exit;
 End;

 // is main?
 if (CardList[ID].isMain) Then
 Begin
  Application.MessageBox(PChar(getLangValue('msg_close_main_card')), PChar(getLangValue('msg_info')), MB_IconInformation);
  Exit;
 End;

 // is modified?
 if (CardList[ID].SynEdit.Modified) Then
  if (not CardList[ID].Save(csrClosingCard)) Then
   Exit;

 CardList.Delete(ID);
 MainForm.Tabs.Pages[ID].Free;

 SaveIfCan;
End;

{ TProject.CloseCards }
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

 SaveIfCan;
End;

{ TProject.RaiseMessage }
Procedure TProject.RaiseMessage(ID: Integer);
Var Card: Integer;
Begin
 Card := FindCard(MessageList[ID]^.FileName);

 if (Card = -1) Then
 Begin
  if (not CreateCard(MessageList[ID]^.FileName, True)) Then
   Exit;
  Card := CardList.Count-1;
 End;

 CardList[Card].ErrorLine      := MessageList[ID]^.Line;
 MainForm.Tabs.ActivePageIndex := Card;

 With CardList[Card] do
 Begin
  SynEdit.CaretX := MessageList[ID]^.Char;
  SynEdit.CaretY := MessageList[ID]^.Line;
  SynEdit.Invalidate;

  MainForm.Tabs.SetFocus;
  Tab.SetFocus;
  SynEdit.SetFocus;
 End;
End;

{ TProject.RefreshControls }
Procedure TProject.RefreshControls;
Var Card: TCard;
Begin
 For Card in CardList Do
  Card.RefreshControls;
End;

{ TProject.Compile }
Function TProject.Compile: Boolean;
Type TMessageIcon = (miNone, miHint, miWarn, miError);
Const READ_BYTES = 2048;
Var sOutputFile: String;

    Switch: TCompilerSwitch;
    Card  : TCard;

    Process    : TProcess;
    InputFile  : String = '';
    CommandLine: String = '';
    MemStream  : TMemoryStream;
    Output     : TStringList;
    NumBytes   : LongInt;
    BytesRead  : LongInt;

    I, P, Line, Char              : Integer;
    Base, Message, mFileName, Posi: String;

    AnyError: Boolean = False;

// AddText
Procedure AddText(const Str: String; Icon: TMessageIcon=miNone; DataInt: Integer=0);
Begin
 With MainForm.CompileStatus do
  With Items.Add(nil, Str) do
  Begin
   Data          := Pointer(DataInt);
   ImageIndex    := ord(Icon);
   SelectedIndex := ImageIndex;
  End;
End;

// AddError
Procedure AddError(Line, Char: Integer; Base, Message, FileName: String);
Var Card: Integer;
    Info: PMessage;
Begin
 AnyError := True;

 { open card }
 Card := FindCard(FileName);
 if (Card = -1) Then
 Begin
  if (not CreateCard(FileName, True)) Then // failed
   Exit;
  Card := CardList.Count-1;
 End;

 { modify card }
 With CardList[Card] do
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

Begin
 if (not FileExists(CompilerFile)) Then
 Begin
  Application.MessageBox(PChar(getLangValue('msg_compiler_not_found')), PChar(getLangValue('msg_err')), MB_IconError);
  Exit(False);
 End;

 if (not Save) Then
  Exit(False);

 MessageList.Clear;

 { each card }
 For Card in CardList Do
 Begin
  Card.ErrorLine       := -1;
  Card.SynEdit.Enabled := False;
 End;

 InputFile   := getMainCard.FileName;
 sOutputFile := MakeFullPath(OutputFile);
 DeleteFile(sOutputFile); // remove previous output file

 Application.ProcessMessages;

 With MainForm.CompileStatus.Items do
 Begin
  Clear;
  AddText(Format(getLangValue('compilation_started'), [TimeToStr(Time)]));

  Process         := TProcess.Create(nil);
  Process.Options := [poUsePipes, poNoConsole];

  { generate command line }
  CommandLine :=
  '"'+CompilerFile+'" "'+InputFile+'"'+
  ' -quiet'+
  ' -includepath "'+IncludePath+'"'+
  ' -o "'+sOutputFile+'"';

  if (ProjectType = ptLibrary) Then
  Begin
   CommandLine += ' -Clib';
   if (HeaderFile <> '') Then
    CommandLine += ' -h "'+MakeFullPath(HeaderFile)+'"';
  End;

  if (BytecodeOutput <> '') Then
   CommandLine += ' -s "'+MakeFullPath(BytecodeOutput)+'"';

  For Switch in CompilerSwitches Do
   CommandLine += ' -'+getSwitchName(Switch);
  CommandLine += ' '+OtherCompilerSwitches;

  { run compiler }
  Process.CommandLine := CommandLine;

  Process.Execute;

  MemStream := TMemoryStream.Create;
  BytesRead := 0;

  While (Process.Running) Do
  Begin
   MemStream.SetSize(BytesRead + READ_BYTES);
   NumBytes := Process.Output.Read((MemStream.Memory + BytesRead)^, READ_BYTES);

   Inc(BytesRead, NumBytes);
   Sleep(100);
  End;

  Repeat
   MemStream.SetSize(BytesRead + READ_BYTES);

   NumBytes := Process.Output.Read((MemStream.Memory + BytesRead)^, READ_BYTES);

   Inc(BytesRead, NumBytes);
  Until (NumBytes <= 0);

  Process.Free;

  { parse results }
  Output := TStringList.Create;
  Output.LoadFromStream(MemStream);

  For I := 0 To Output.Count-1 Do
  Begin
   Base := Output[I];

   { error }
   P := Pos('Error:', Base);
   if (P > 0) Then
   Begin
    Message   := Trim(Copy(Base, P+6, Length(Base)));
    mFileName := Trim(Copy(Base, 1, Pos('(', Base)-1));
    Posi      := Trim(Copy(Base, Pos('(', Base)+1, Pos(')', Base)-Pos('(', Base)-1));

    Line := StrToInt(Copy(Posi, 1, Pos(',', Posi)-1));
    Char := StrToInt(Copy(Posi, Pos(',', Posi)+1, Length(Posi)));

    AddError(Line, Char, Base, Message, mFileName);
   End;
  End;

  if (not AnyError) Then
   AddText(Format(getLangValue('compilation_finished'), [TimeToStr(Time), OutputFile])) Else
   AddText(Format(getLangValue('compilation_stopped'), [TimeToStr(Time)]));
 End;

 For Card in CardList Do
  Card.SynEdit.Enabled := True;

 if (MessageList.Count > 0) Then
  RaiseMessage(0);

 Application.ProcessMessages;

 Exit(not AnyError);
End;

{ TProject.Run }
Procedure TProject.Run;
Var sOutputFile, CommandLine: String;
    Switch                  : TVMSwitch;
Begin
 if (ProjectType = ptLibrary) Then // cannot run a library
  Exit;

 if (not FileExists(VMFile)) Then // virtual machine found?
 Begin
  Application.MessageBox(PChar(getLangValue('msg_vm_not_found')), PChar(getLangValue('msg_err')), MB_IconError);
  Exit;
 End;

 { check path }
 if (PathIsRelative(PChar(OutputFile))) Then
  sOutputFile := ExtractFilePath(FileName)+OutputFile Else
  sOutputFile := OutputFile;

 { generate command line }
 CommandLine := '"'+sOutputFile+'" ';

 For Switch in VMSwitches Do
  CommandLine += ' -'+getVMSwitchName(Switch, True);

 CommandLine += ' '+OtherVMSwitches;

 { run program }
 ExecuteProcess(VMFile, CommandLine, []);
End;

{ TProject.isEverythingSaved }
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

{ TProject.getCurrentCard }
Function TProject.getCurrentCard: TCard;
Begin
 if (CardList.Count = 0) Then
  Exit(nil);

 With MainForm.Tabs do
 Begin
  if (ActivePageIndex = -1) Then
   ActivePageIndex := 0;
  if (ActivePageIndex >= CardList.Count) Then
   ActivePageIndex := CardList.Count-1;

  Exit(CardList[ActivePageIndex]);
 End;
End;

{ TProject.getCurrentEditor }
Function TProject.getCurrentEditor: TSynEdit;
Begin
 if (CardList.Count = 0) Then
  Exit(nil);

 Result := getCurrentCard.SynEdit;
End;

End.
