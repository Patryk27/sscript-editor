(*
 Copyright © by Patryk Wychowaniec, 2013-2014
 All rights reserved.
*)
unit uMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, Forms, Controls,
  Graphics, Dialogs, Menus, ExtCtrls, mSettings, LCLType, Buttons;

 // types
 Type TState = (stEnabled, stDisabled);

type
  { TMainForm }
  TMainForm = class(TForm)
    bbNewModule: TBitBtn;
    bbOpen: TBitBtn;
    bbSave: TBitBtn;
    bbSaveAll: TBitBtn;
    bbStopProgram: TBitBtn;
    bbRun: TBitBtn;
    menuCode: TMenuItem;
    MenuItem1: TMenuItem;
    oStopProgram: TMenuItem;
    MenuItem7: TMenuItem;
    oRun: TMenuItem;
    oResetLayout: TMenuItem;
    oLayoutManager: TMenuItem;
    menuWindow: TMenuItem;
    oIdentifierList: TMenuItem;
    oCompileStatus: TMenuItem;
    oCodeEditor: TMenuItem;
    oUncommentSelected: TMenuItem;
    oCommentSelected: TMenuItem;
    oReplace: TMenuItem;
    MenuItem11: TMenuItem;
    oGotoLine: TMenuItem;
    oFind: TMenuItem;
    oFindNext: TMenuItem;
    oFindPrev: TMenuItem;
    menuSearch: TMenuItem;
    oSelectAll: TMenuItem;
    oSelectWord: TMenuItem;
    oSelectLine: TMenuItem;
    MenuItem8:TMenuItem;
    MenuItem9: TMenuItem;
    oShowCompilerOutput:TMenuItem;
    oCloseProject:TMenuItem;
    oCloseCurrentCard:TMenuItem;
    menuHelp: TMenuItem;
    menuEnvironment: TMenuItem;
    oRecentlyOpened: TMenuItem;
    oNewProj_App: TMenuItem;
    oNewProj_Library: TMenuItem;
    oEvSettings: TMenuItem;
    oAbout: TMenuItem;
    MainMenu: TMainMenu;
    menuFile: TMenuItem;
    menuCompile: TMenuItem;
    oProjectSettings: TMenuItem;
    oOpenProject: TMenuItem;
    MenuItem3: TMenuItem;
    oSave: TMenuItem;
    MenuItem6: TMenuItem;
    oSaveAs: TMenuItem;
    oSaveAll: TMenuItem;
    oBuild: TMenuItem;
    oBuildAndRun: TMenuItem;
    oNewProject: TMenuItem;
    MenuItem2: TMenuItem;
    menuEdit: TMenuItem;
    MenuItem5: TMenuItem;
    menuProject: TMenuItem;
    oPaste: TMenuItem;
    oCopy: TMenuItem;
    oCut: TMenuItem;
    oRedo: TMenuItem;
    oUndo: TMenuItem;
    oExit: TMenuItem;
    oNewModule: TMenuItem;
    MenuItem4: TMenuItem;
    oOpen: TMenuItem;
    LayoutSaveTimer: TTimer;
    UpdateTimer: TTimer;
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: Array of String);
    procedure oCodeEditorClick(Sender: TObject);
    procedure oCommentSelectedClick(Sender: TObject);
    procedure oCompileStatusClick(Sender: TObject);
    procedure oIdentifierListClick(Sender: TObject);
    procedure oLayoutManagerClick(Sender: TObject);
    procedure oReplaceClick(Sender: TObject);
    procedure oFindClick(Sender: TObject);
    procedure oFindNextClick(Sender: TObject);
    procedure oFindPrevClick(Sender: TObject);
    procedure oGotoLineClick(Sender: TObject);
    procedure oResetLayoutClick(Sender: TObject);
    procedure oRunClick(Sender: TObject);
    procedure oSelectAllClick(Sender: TObject);
    procedure oSelectLineClick(Sender: TObject);
    procedure oSelectWordClick(Sender: TObject);
    procedure oShowCompilerOutputClick(Sender:TObject);
    procedure oAboutClick(Sender: TObject);
    procedure oCloseCurrentCardClick(Sender:TObject);
    procedure oCloseProjectClick(Sender:TObject);
    procedure oEvSettingsClick(Sender: TObject);
    procedure oNewProj_AppClick(Sender: TObject);
    procedure oNewProj_LibraryClick(Sender: TObject);
    procedure oProjectSettingsClick(Sender: TObject);
    procedure oExitClick(Sender: TObject);
    procedure oOpenProjectClick(Sender: TObject);
    procedure oBuildAndRunClick(Sender: TObject);
    procedure oNewModuleClick(Sender: TObject);
    procedure oSaveAllClick(Sender: TObject);
    procedure oSaveAsClick(Sender: TObject);
    procedure oSaveClick(Sender: TObject);
    procedure oBuildClick(Sender: TObject);
    procedure oCopyClick(Sender: TObject);
    procedure oCutClick(Sender: TObject);
    procedure oOpenClick(Sender: TObject);
    procedure oPasteClick(Sender: TObject);
    procedure oRedoClick(Sender: TObject);
    procedure oStopProgramClick(Sender: TObject);
    procedure oUncommentSelectedClick(Sender: TObject);
    procedure oUndoClick(Sender: TObject);
    procedure UpdateTimerTimer(Sender: TObject);

  private
   Procedure RecentlyOpened_Click(Sender: TObject);
   Procedure AppIdle(Sender: TObject; var Done: Boolean);

  public
   Procedure OnLanguageLoaded;

   Procedure setMainMenu(State: TState);
   Procedure UpdateRecentlyOpened;
  end;

 // consts
 Const vMajor   = 0.3;
       vMinor   = 4;

       iVersion: Single = 100*vMajor+vMinor; // this is saved into the `version` field in project files.

       sVersion = '0.3.4 nightly';
       sCaption = 'SScript Editor v'+sVersion;

 // variables
 Var MainForm      : TMainForm;
     RecentlyOpened: TStringList = nil;

 // procedures
 Procedure AddRecentlyOpened(const FileName: String);

 Implementation
Uses mProject, mLanguages, mFunctions, mLayout, ClipBrd, uProjectSettings, uEvSettingsForm, uAboutForm, uCompilerOutput, uFindForm, uIdentifierListForm,
     uCompileStatusForm, uCodeEditor, uLayoutManagerForm;

{$R *.lfm}

(* AddRecentlyOpened *)
Procedure AddRecentlyOpened(const FileName: String);
Begin
 With RecentlyOpened do
 Begin
  if (IndexOf(FileName) = -1) Then // don't duplicate
  Begin
   Add(FileName);
   While (Count > 8) Do // if more than 8 items on the list, remove the first
    Delete(0);
  End Else
   Exchange(IndexOf(FileName), 0); // move at the beginning
 End;
 setRecentlyOpened(RecentlyOpened);
End;

(* getProjectPnt *)
Function getProjectPnt: Pointer;
Begin
 Result := Project;
End;

// -------------------------------------------------------------------------- //
(* SaveProject *)
Function SaveProject: Boolean;
Var Save: Boolean;
Begin
 Result := True;

 if (Project = nil) Then
  Exit;

 if (Project.Named) Then
 Begin
  if (not Project.isEverythingSaved) Then
  Begin
   { ask user }
   Save := False;
   Case MessageDlg(getLangValue(ls_project_saving), getLangValue(ls_msg_unsaved_files), mtConfirmation, mbYesNoCancel, '') of
    mrYes   : Save := True;
    mrNo    : Save := False;
    mrCancel: Exit(False);
   End;
  End;

  if (Save) Then
   Project.Save;

  Exit(True);
 End;

 { ask user }
 Save := False;
 Case MessageDlg(getLangValue(ls_project_saving), getLangValue(ls_msg_unsaved_project), mtConfirmation, mbYesNoCancel, '') of
  mrYes   : Save := True;
  mrNo    : Save := False;
  mrCancel: Exit(False);
 End;

 if (Save) Then
  Project.Save;
End;

// -------------------------------------------------------------------------- //
(* TMainForm.RecentlyOpened_Click *)
Procedure TMainForm.RecentlyOpened_Click(Sender: TObject);
Begin
 if (SaveProject) Then
 Begin
  // close current project
  if (Project = nil) Then
   Project := TProject.Create Else
   Begin
    Project.Free;
    Project := TProject.Create;
   End;

  // open the project
  if (not Project.Open(TMenuItem(Sender).Caption)) Then // if failed
  Begin
   With RecentlyOpened Do
    Delete(IndexOf(TMenuItem(Sender).Caption)); // remove not-existing file from the list
   setRecentlyOpened(RecentlyOpened);
   setMainMenu(stDisabled);

   Application.MessageBox(PChar(getLangValue(ls_msg_project_open_failed)), PChar(getLangValue(ls_msg_error)), MB_IconError);
   FreeAndNil(Project);
  End;
 End;
End;

(* TMainForm.AppIdle *)
Procedure TMainForm.AppIdle(Sender: TObject; var Done: Boolean);
Begin
End;

(* TMainForm.UpdateRecentlyOpened *)
Procedure TMainForm.UpdateRecentlyOpened;
Var Str     : String;
    MenuItem: TMenuItem;
Begin
 if (RecentlyOpened <> nil) Then // free previous instance
  RecentlyOpened.Free;

 RecentlyOpened := getRecentlyOpened; // get new list

 oRecentlyOpened.Clear;
 For Str in RecentlyOpened Do // update MainMenu's items
 Begin
  With oRecentlyOpened do
  Begin
   MenuItem         := TMenuItem.Create(oRecentlyOpened);
   MenuItem.Caption := Str;
   MenuItem.OnClick := @RecentlyOpened_Click;
   oRecentlyOpened.Add(MenuItem);
  End;
 End;
End;

(* TMainForm.onLanguageLoaded *)
Procedure TMainForm.onLanguageLoaded;
Var FileName, MsgText: String;

  // Map
  Procedure Map(const Button: TBitBtn; const MenuItem: TMenuItem);
  Begin
   Button.Glyph := MenuItem.Bitmap;
   Button.Hint  := MenuItem.Caption;
  End;

Begin
 { set icons and hints }
 Map(bbNewModule, oNewModule);
 Map(bbOpen, oOpen);
 Map(bbSave, oSave);
 Map(bbSaveAll, oSaveAll);
 Map(bbRun, oBuildAndRun);
 Map(bbStopProgram, oStopProgram);

 { if specified in parameter, try to open a project }
 SetCurrentDir(ExtractFilePath(ParamStr(1)));

 FileName := ParamStr(1);
 if (FileExists(FileName)) and (CompareText(ExtractFileExt(FileName), '.ssp') = 0) Then // check for file existence, and also check the file extension
 Begin
  Project := TProject.Create;
  if (not Project.Open(FileName)) Then // is failed to open
  Begin
   MsgText := Format(getLangValue(ls_msg_project_open_failed_ex), [FileName]);
   Application.MessageBox(PChar(MsgText), PChar(getLangValue(ls_msg_error)), MB_IconError);
   Project.Free;
  End;
 End Else

 { or else open recent project, if set }
 if (getBoolean(sOpenRecentProject)) Then
 Begin
  FileName := getString(sRecentProject);
  Project  := TProject.Create;

  if (not Project.Open(FileName)) Then // try to open
   FreeAndNil(Project);
 End;
End;

(* TMainForm.setMainMenu *)
Procedure TMainForm.setMainMenu(State: TState);
Var B: Boolean;
    C: Integer;
Begin
 B := (State = stEnabled);

 For C := 0 To ComponentCount-1 Do // iterate each component
  if (Components[C] is TControl) Then
  Begin
   With Components[C] as TControl do
   Begin
    Case Tag of
     1: Enabled := B;
     2: Enabled := not B;
    End;
   End;
  End Else

  if (Components[C] is TMenuItem) Then
  Begin
   With Components[C] as TMenuItem do
   Begin
    Case Tag of
     1: Enabled := B;
     2: Enabled := not B;
    End;
   End;
  End;
End;

(* TMainForm.FormCreate *)
procedure TMainForm.FormCreate(Sender: TObject);
begin
 ShowHint := True;

 // set some values
 Application.OnIdle := @AppIdle;
 DoubleBuffered     := True;
 Caption            := sCaption;

 setMainMenu(stDisabled);
 UpdateRecentlyOpened;
end;

(* TMainForm.FormDropFiles *)
procedure TMainForm.FormDropFiles(Sender: TObject; const FileNames: Array of String);
Var I: Integer = 0;
begin
 // trying to open a project?
 if (CompareText(ExtractFileExt(FileNames[0]), '.ssp') = 0) Then
 Begin
  if (not SaveProject) Then
   Exit;

  if (Project <> nil) Then
   Project.Free;
  Project := TProject.Create;

  if (not Project.Open(FileNames[0])) Then // failed
  Begin
   Application.MessageBox(PChar(Format(getLangValue(ls_msg_project_open_failed_ex), [FileNames[0]])), PChar(getLangValue(ls_msg_error)), MB_IconError);
   Exit;
  End;

  Inc(I); // do not open project file as a module file
 End;

 // no project opened/created so far?
 if (Project = nil) Then
  Case MessageDlg(getLangValue(ls_file_opening), getLangValue(ls_msg_create_new_project), mtConfirmation, mbYesNo, '') of
   mrNo: Exit;
   mrYes:
   Begin
    Project := TProject.Create;
    Project.NewProject(ptApplication);
   End;
  End;

 // open each file in its own card
 For I := I To High(FileNames) Do
  Project.OpenCard(FileNames[I]);
end;

(* TMainForm.oCodeEditorClick *)
procedure TMainForm.oCodeEditorClick(Sender: TObject);
begin
 CodeEditor.Show;
end;

(* TMainForm.oCommentSelectedClick *)
procedure TMainForm.oCommentSelectedClick(Sender: TObject);
Var Line            : Integer;
    Editor          : TSynEdit;
    List            : TStringList;
    CaretPos, BB, BE: TPoint;
    Str             : String;
begin
 Editor   := Project.getCurrentEditor;
 CaretPos := Editor.CaretXY;
 BB       := Editor.BlockBegin;
 BE       := Editor.BlockEnd;

 List := TStringList.Create;
 Try
  List.Text := Editor.SelText;

  if (List.Count = 1) Then
  Begin
   List[0] := '/*'+TrimRight(List[0])+' */';
   Inc(Be.X, 4);
  End Else
  Begin
   if (List.Count > 0) Then
    Inc(BE.X, 2);

   For Line := 0 To List.Count-1 Do
    List[Line] := '//'+List[Line];
  End;

  Str := List.Text;
  Delete(Str, Length(Str)-Length(LineEnding)+1, Length(LineEnding));
  Editor.SelText := Str;
 Finally
  Editor.CaretXY    := CaretPos;
  Editor.BlockBegin := BB;
  Editor.BlockEnd   := BE;
  List.Free;
 End;
end;

(* TMainForm.oCompileStatusClick *)
procedure TMainForm.oCompileStatusClick(Sender: TObject);
begin
 CompileStatusForm.Show;
end;

(* TMainForm.oIdentifierListClick *)
procedure TMainForm.oIdentifierListClick(Sender: TObject);
begin
 IdentifierListForm.Show;
end;

(* TMainForm.oLayoutManagerClick *)
procedure TMainForm.oLayoutManagerClick(Sender: TObject);
begin
 LayoutManagerForm.Run;
end;

(* TMainForm.oReplaceClick *)
procedure TMainForm.oReplaceClick(Sender: TObject);
begin
 FindForm.Run(frReplace);
end;

(* TMainForm.oFindClick *)
procedure TMainForm.oFindClick(Sender: TObject);
begin
 FindForm.Run(frFind);
end;

(* TMainForm.oFindNextClick *)
procedure TMainForm.oFindNextClick(Sender: TObject);
begin
 FindForm.btnFind.Click;
end;

(* TMainForm.oFindPrevClick *)
procedure TMainForm.oFindPrevClick(Sender: TObject);
Var Prev: Integer;
begin
 With FindForm.rgSearchDir do
 Begin
  Prev := ItemIndex;

  if (ItemIndex = 0) Then
   ItemIndex := 1 Else
   ItemIndex := 0;

  FindForm.btnFind.Click;

  ItemIndex := Prev;
 End;
end;

(* TMainForm.oGotoLineClick *)
procedure TMainForm.oGotoLineClick(Sender: TObject);
Var LineStr      : String;
    Line, LineMax: Integer;
begin
 LineMax := Project.getCurrentEditor.Lines.Count;
 LineStr := IntToStr(Project.getCurrentEditor.CaretY);

 if (InputQuery(getLangValue(ls_goto_line_title),
                Format(getLangValue(ls_goto_line), [1, LineMax]),
                LineStr)) Then
 Begin
  if (TryStrToInt(LineStr, Line)) Then
   Project.getCurrentEditor.CaretY := Line;
 End;
end;

(* TMainForm.oResetLayout *)
procedure TMainForm.oResetLayoutClick(Sender: TObject);
begin
 CurrentLayout.Reset;
 CurrentLayout.Apply;
end;

(* TMainForm.oRunClick *)
procedure TMainForm.oRunClick(Sender: TObject);
begin
 Project.Run;
end;

(* TMainForm.oSelectAllClick *)
procedure TMainForm.oSelectAllClick(Sender: TObject);
begin
 Project.getCurrentEditor.SelectAll;
end;

(* TMainForm.oSelectLineClick *)
procedure TMainForm.oSelectLineClick(Sender: TObject);
begin
 Project.getCurrentEditor.SelectLine;
end;

(* TMainForm.oSelectWordClick *)
procedure TMainForm.oSelectWordClick(Sender: TObject);
begin
 Project.getCurrentEditor.SelectWord;
end;

(* TMainForm.oShowCompilerOutputClick *)
procedure TMainForm.oShowCompilerOutputClick(Sender:TObject);
begin
 CompilerOutputForm.Output.Text := Project.CompilerOutput;
 CompilerOutputForm.ShowModal;
end;

(* TMainForm.oAboutClick *)
procedure TMainForm.oAboutClick(Sender: TObject);
begin
 AboutForm.ShowModal;
end;

(* TMainForm.oCloseCurrentCardClick *)
procedure TMainForm.oCloseCurrentCardClick(Sender:TObject);
begin
 CodeEditor.opCloseCard.Click;
end;

(* TMainForm.oCloseProjectClick *)
procedure TMainForm.oCloseProjectClick(Sender:TObject);
begin
 if (SaveProject) Then
  if (Project <> nil) Then
   FreeAndNil(Project);
end;

(* TMainForm.oEvSettingsClick *)
procedure TMainForm.oEvSettingsClick(Sender: TObject);
begin
 EvSettingsForm.Run;
end;

(* TMainForm.oNewProj_AppClick *)
procedure TMainForm.oNewProj_AppClick(Sender: TObject);
begin
 if (SaveProject) Then
 Begin
  if (Project = nil) Then
   Project := TProject.Create Else
   Begin
    FreeAndNil(Project);
    Project := TProject.Create;
   End;

  Project.NewProject(ptApplication);

  Caption := sCaption+' - '+getLangValue(ls_new_app);
 End;
end;

(* TMainForm.oNewProj_LibraryClick *)
procedure TMainForm.oNewProj_LibraryClick(Sender: TObject);
begin
 if (SaveProject) Then
 Begin
  if (Project = nil) Then
   Project := TProject.Create Else
   Begin
    FreeAndNil(Project);
    Project := TProject.Create;
   End;

  Project.NewProject(ptLibrary);

  Caption := sCaption+' - '+getLangValue(ls_new_lib);
 End;
end;

(* TMainForm.oProjectSettingsClick *)
procedure TMainForm.oProjectSettingsClick(Sender: TObject);
begin
 ProjectSettingsForm.Run;
end;

(* TMainForm.oExitClick *)
procedure TMainForm.oExitClick(Sender: TObject);
begin
 Close;
end;

(* TMainForm.oOpenProjectClick *)
procedure TMainForm.oOpenProjectClick(Sender: TObject);
begin
 if (SaveProject) Then
 Begin
  if (Project <> nil) Then
   FreeAndNil(Project);

  oOpen.Click;
 End;
end;

(* TMainForm.oCompileAndRunClick *)
procedure TMainForm.oBuildAndRunClick(Sender: TObject);
begin
 if (Project.Compile) Then
  Project.Run;
end;

(* TMainForm.oNewModuleCkick *)
procedure TMainForm.oNewModuleClick(Sender: TObject);
begin
 Project.NewNoNameCard;
end;

(* TMainForm.oSaveAllClick *)
procedure TMainForm.oSaveAllClick(Sender: TObject);
begin
 Project.Save;
end;

(* TMainForm.oSaveAsClick *)
procedure TMainForm.oSaveAsClick(Sender: TObject);
begin
 Project.Save;
 Project.SaveCurrentCardAs;
end;

(* TMainForm.oSaveClick *)
procedure TMainForm.oSaveClick(Sender: TObject);
begin
 Project.Save;
 Project.SaveCurrentCard;
end;

(* TMainForm.oBuildClick *)
procedure TMainForm.oBuildClick(Sender: TObject);
begin
 Project.Compile;
end;

(* TMainForm.oCopyClick *)
procedure TMainForm.oCopyClick(Sender: TObject);
begin
 Project.getCurrentEditor.CopyToClipboard;
end;

(* TMainForm.oCutClick *)
procedure TMainForm.oCutClick(Sender: TObject);
begin
 Project.getCurrentEditor.CutToClipboard;
end;

(* TMainForm.FormCloseQuery *)
procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
 if (Project <> nil) and (Project.VMProcess <> nil) Then // if VM is running
 Begin
  Case MessageDlg(getLangValue(ls_msg_info), getLangValue(ls_msg_stop_vm), mtConfirmation, [mbYes, mbNo], 0) of
   mrYes:
   Begin
    Project.VMProcess.Terminate(0);
    FreeAndNil(Project.VMProcess);
   End;

   mrNo:
   Begin
    CanClose := False;
    Exit;
   End;
  End;
 End;

 CanClose := SaveProject;

 // save current project as a "Recent" and add it into the "Recently opened" list (if possible)
 if (CanClose) and (Project <> nil) Then
  if (Project.Named) Then
  Begin
   setString(sRecentProject, Project.FileName);
   AddRecentlyOpened(Project.FileName);
  End;
end;

(* TMainForm.oOpenClick *)
procedure TMainForm.oOpenClick(Sender: TObject);
Var I: Integer;
begin
 // create open dialog
 With TOpenDialog.Create(MainForm) do
 Begin
  Try
   if (Project = nil) Then // no project opened - oShowCompilerOutput project opening dialog
   Begin
    Title  := getLangValue(ls_project_opening);
    Filter := getLangValue(ls_filter_project);
   End Else
   Begin // in other case - oShowCompilerOutput module opening dialog
    Title  := getLangValue(ls_file_opening);
    Filter := getLangValue(ls_filter_any_file);
   End;

   Options := [ofPathMustExist, ofFileMustExist];

   if (Execute) Then
   Begin
    { opening a project }
    if (Project = nil) or (CompareText(ExtractFileExt(FileName), '.ssp') = 0) Then
    Begin
     if (SaveProject) Then
     Begin
      CompileStatusForm.CompileStatus.Items.Clear;

      For I := 0 To CodeEditor.Tabs.PageCount-1 Do
       CodeEditor.Tabs.Pages[0].Free;

      Project := TProject.Create;
      if (not Project.Open(FileName)) Then // failed
      Begin
       Application.MessageBox(PChar(getLangValue(ls_msg_project_open_failed)), PChar(getLangValue(ls_msg_error)), MB_IconError);
       FreeAndNil(Project);
      End;
     End;
    End Else

    { opening a module }
    Begin
     if (Project.OpenCard(FileName) = nil) Then // failed
      Application.MessageBox(PChar(getLangValue(ls_msg_module_open_failed)), PChar(getLangValue(ls_msg_error)), MB_IconError);
    End;
   End;
  Finally
   Free;
  End;
 End;
end;

(* TMainForm.oPasteClick *)
procedure TMainForm.oPasteClick(Sender: TObject);
begin
 Project.getCurrentEditor.PasteFromClipboard;
end;

(* TMainForm.oRedoClick *)
procedure TMainForm.oRedoClick(Sender: TObject);
begin
 Project.getCurrentEditor.Redo;
end;

(* TMainForm.oStopProgram *)
procedure TMainForm.oStopProgramClick(Sender: TObject);
begin
 Project.StopProgram;
end;

(* TMainForm.oUncommentSelectedClick *)
procedure TMainForm.oUncommentSelectedClick(Sender: TObject);
Var Line, Char      : Integer;
    Editor          : TSynEdit;
    List            : TStringList;
    CaretPos, BB, BE: TPoint;
    Str             : String;
begin
 Editor   := Project.getCurrentEditor;
 CaretPos := Editor.CaretXY;
 BB       := Editor.BlockBegin;
 BE       := Editor.BlockEnd;

 List := TStringList.Create;
 Try
  List.Text := Editor.SelText;

  if (List.Count = 1) Then
  Begin
   Str := List[0];

   if (MPos('/*', Str) > 0) and (MPos('*/', Str) > 0) Then
   Begin
    Delete(Str, MPos('/*', Str), 2);
    Delete(Str, MPos('*/', Str), 2);
   End;

   List[0] := Str;
  End;

  For Line := 0 To List.Count-1 Do
  Begin
   Str := List[Line];

   For Char := 1 To Length(Str) Do
   Begin
    if not (Str[Char] in ['/', ' ']) Then
     Break;

    if (Copy(Str, Char, 2) = '//') Then
    Begin
     if (Line = List.Count-1) Then
      Dec(BE.X, 2);

     Delete(Str, Char, 2);
     Break;
    End;
   End;

   List[Line] := Str;
  End;

  Str := List.Text;
  Delete(Str, Length(Str)-Length(LineEnding)+1, Length(LineEnding));
  Editor.SelText := Str;
 Finally
  Editor.CaretXY    := CaretPos;
  Editor.BlockBegin := BB;
  Editor.BlockEnd   := BE;
  List.Free;
 End;
end;

(* TMainForm.oUndoClick *)
procedure TMainForm.oUndoClick(Sender: TObject);
begin
 Project.getCurrentEditor.Undo;
end;

(* TMainForm.UpdateTimerTimer *)
procedure TMainForm.UpdateTimerTimer(Sender: TObject);
begin
 bbStopProgram.Enabled := (Project <> nil) and (Project.VMProcess <> nil);
 bbRun.Enabled         := (Project <> nil) and (not bbStopProgram.Enabled);
 oRun.Enabled          := bbRun.Enabled;
 oBuild.Enabled        := bbRun.Enabled;
 oBuildAndRun.Enabled  := bbRun.Enabled;
 oStopProgram.Enabled  := bbStopProgram.Enabled;

 if (Project <> nil) Then
 Begin
  oNewProject.Enabled  := bbRun.Enabled;
  oOpenProject.Enabled := bbRun.Enabled;
 End;
end;
end.
