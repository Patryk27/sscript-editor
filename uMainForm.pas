(*
 Copyright © by Patryk Wychowaniec, 2013-2014
 All rights reserved.
*)
unit uMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, Forms, Controls,
  Graphics, Dialogs, Menus, ExtCtrls, mConfiguration, LCLType, Buttons;

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
    oFindPrevious: TMenuItem;
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
    oNewProject_Application: TMenuItem;
    oNewProject_Library: TMenuItem;
    oEvSettings: TMenuItem;
    oAbout: TMenuItem;
    MainMenu: TMainMenu;
    menuFile: TMenuItem;
    menuRun: TMenuItem;
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
    procedure oFindPreviousClick(Sender: TObject);
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
    procedure oNewProject_ApplicationClick(Sender: TObject);
    procedure oNewProject_LibraryClick(Sender: TObject);
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

   Procedure setMainMenu(const State: TState);
   Procedure UpdateRecentlyOpened;
  end;

 // consts
 Const vMajor = 0.3;
       vMinor = 4;

       EditorVersion: Single = 100*vMajor+vMinor; // this is saved into the `version` field in project files.

       StringVersion = '0.3.4 nightly';
       BaseCaption   = 'SScript Editor v'+StringVersion;

 // variables
 Var MainForm      : TMainForm;
     RecentlyOpened: TStringList = nil;

 // procedures
 Procedure AddRecentlyOpened(const FileName: String);

 Implementation
Uses mProject, mLanguages, mFunctions, mLayouts, mStyles, mMessages,
     ClipBrd,
     uProjectSettings, uEvSettingsForm, uAboutForm, uCompilerOutput, uFindForm, uIdentifierListForm,
     uCompileStatusForm, uCodeEditor, uLayoutManagerForm;

{$R *.lfm}

(* AddRecentlyOpened *)
Procedure AddRecentlyOpened(const FileName: String);
Begin
 With RecentlyOpened do
 Begin
  if (IndexOf(FileName) = -1) Then
  Begin
   // if new, insert at the beginning
   Insert(0, FileName);
  End Else
  Begin
   // if duplicated, move at the beginning
   Exchange(IndexOf(FileName), 0);
  End;

  // remove the first elements, until no more than specified number of items are lying on the list
  While (Count > Config.getInteger(ceMaxRecentlyOpened)) Do
   Delete(0);
 End;

 Config.setRecentlyOpened(RecentlyOpened);
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
   Case MessageDlg(Language.getText(ls_project_saving), Language.getText(ls_dlg_unsaved_files), mtConfirmation, mbYesNoCancel, '') of
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
 Case MessageDlg(Language.getText(ls_project_saving), Language.getText(ls_dlg_unsaved_project), mtConfirmation, mbYesNoCancel, '') of
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
  // create a new project instance
  Project.Free;
  Project := TProject.Create;

  // open the specified project
  if (not Project.Load(TMenuItem(Sender).Caption)) Then // if failed
  Begin
   // change main menu state
   setMainMenu(stDisabled);

   // display error
   ErrorMessage(ls_msg_project_open_failed);

   // dispose project instance
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
Var MenuItem: TMenuItem;
    Str     : String;
Begin
 // update the recently opened string list
 RecentlyOpened.Free;
 RecentlyOpened := Config.getRecentlyOpened;

 // update the menu items
 oRecentlyOpened.Clear;
 For Str in RecentlyOpened Do
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
Procedure TMainForm.OnLanguageLoaded;

  { Map }
  Procedure Map(const Button: TBitBtn; const MenuItem: TMenuItem);
  Begin
   Button.Glyph := MenuItem.Bitmap;
   Button.Hint  := MenuItem.Caption;
  End;

  { TryToOpen }
  Function TryToOpen(const FileName: String): Boolean;
  Begin
   if (not FileExists(FileName)) Then
    Exit(False);

   if (CompareText(ExtractFileExt(FileName), '.ssp') <> 0) Then
    Exit(False);

   Project := TProject.Create;

   // try to open
   if (Project.Load(FileName)) Then
    Exit(True);

   // if failed, display error message
   ErrorMessage(ls_msg_project_open_failed_ex, [FileName]);

   // and dispose the project instance
   FreeAndNil(Project);
   Exit(False);
  End;

Begin
 // associate icons and hints
 Map(bbNewModule, oNewModule);
 Map(bbOpen, oOpen);
 Map(bbSave, oSave);
 Map(bbSaveAll, oSaveAll);
 Map(bbRun, oBuildAndRun);
 Map(bbStopProgram, oStopProgram);

 // try to open a project specified in parameter or the recent one
 if (not TryToOpen(ParamStr(1))) Then
 Begin
  if (Config.getBoolean(ceLoadRecentProject)) Then
  Begin
   if (not TryToOpen(Config.getString(ceRecentProject))) Then
   Begin
    Config.setString(ceRecentProject, '');
   End;
  End;
 End;
End;

(* TMainForm.setMainMenu *)
Procedure TMainForm.setMainMenu(const State: TState);
Var B: Boolean;
    C: Integer;
Begin
 B := (State = stEnabled);

 For C := 0 To ComponentCount-1 Do // iterate each component
 Begin
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
End;

(* TMainForm.FormCreate *)
Procedure TMainForm.FormCreate(Sender: TObject);
begin
 ShowHint := True;

 // set some values
 Application.OnIdle := @AppIdle;
 DoubleBuffered     := True;
 Caption            := BaseCaption;

 setMainMenu(stDisabled);
 UpdateRecentlyOpened;
end;

(* TMainForm.FormDropFiles *)
Procedure TMainForm.FormDropFiles(Sender: TObject; const FileNames: Array of String);
Var I: Integer = 0;
begin
 // trying to open a project?
 if (CompareText(ExtractFileExt(FileNames[0]), '.ssp') = 0) Then
 Begin
  if (not SaveProject) Then
   Exit;

  Project.Free;
  Project := TProject.Create;

  if (not Project.Load(FileNames[0])) Then // failed
  Begin
   ErrorMessage(ls_msg_project_open_failed_ex, [FileNames[0]]);
   Exit;
  End;

  Inc(I); // do not open project file as a module file
 End;

 // no project opened/created so far?
 if (Project = nil) Then
 Begin
  Case MessageDlg(Language.getText(ls_file_opening), Language.getText(ls_dlg_create_new_project), mtConfirmation, mbYesNo, '') of
   mrNo: Exit;
   mrYes:
   Begin
    Project := TProject.Create;
    Project.NewProject(ptApplication);
   End;
  End;
 End;

 // open each file on its own card
 For I := I To High(FileNames) Do
  Project.OpenCard(FileNames[I]);
end;

(* TMainForm.oCodeEditorClick *)
Procedure TMainForm.oCodeEditorClick(Sender: TObject);
begin
 CodeEditor.Show;
end;

(* TMainForm.oCommentSelectedClick *)
Procedure TMainForm.oCommentSelectedClick(Sender: TObject);
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
Procedure TMainForm.oCompileStatusClick(Sender: TObject);
begin
 CompileStatusForm.Show;
end;

(* TMainForm.oIdentifierListClick *)
Procedure TMainForm.oIdentifierListClick(Sender: TObject);
begin
 IdentifierListForm.Show;
end;

(* TMainForm.oLayoutManagerClick *)
Procedure TMainForm.oLayoutManagerClick(Sender: TObject);
begin
 LayoutManagerForm.Run;
end;

(* TMainForm.oReplaceClick *)
Procedure TMainForm.oReplaceClick(Sender: TObject);
begin
 FindForm.Run(frReplace);
end;

(* TMainForm.oFindClick *)
Procedure TMainForm.oFindClick(Sender: TObject);
begin
 FindForm.Run(frFind);
end;

(* TMainForm.oFindNextClick *)
Procedure TMainForm.oFindNextClick(Sender: TObject);
begin
 FindForm.btnFind.Click;
end;

(* TMainForm.oFindPrevClick *)
Procedure TMainForm.oFindPreviousClick(Sender: TObject);
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
Procedure TMainForm.oGotoLineClick(Sender: TObject);
Var LineStr      : String;
    Line, LineMax: Integer;
begin
 LineMax := Project.getCurrentEditor.Lines.Count;
 LineStr := IntToStr(Project.getCurrentEditor.CaretY);

 if (InputQuery(Language.getText(ls_goto_line_title),
                Format(Language.getText(ls_goto_line), [1, LineMax]),
                LineStr)) Then
 Begin
  if (TryStrToInt(LineStr, Line)) Then
   Project.getCurrentEditor.CaretY := Line;
 End;
end;

(* TMainForm.oResetLayout *)
Procedure TMainForm.oResetLayoutClick(Sender: TObject);
begin
 With LayoutManager.getCurrentLayout do
 Begin
  Reset;
  Apply;
 End;
end;

(* TMainForm.oRunClick *)
Procedure TMainForm.oRunClick(Sender: TObject);
begin
 Project.Run;
end;

(* TMainForm.oSelectAllClick *)
Procedure TMainForm.oSelectAllClick(Sender: TObject);
begin
 Project.getCurrentEditor.SelectAll;
end;

(* TMainForm.oSelectLineClick *)
Procedure TMainForm.oSelectLineClick(Sender: TObject);
begin
 Project.getCurrentEditor.SelectLine;
end;

(* TMainForm.oSelectWordClick *)
Procedure TMainForm.oSelectWordClick(Sender: TObject);
begin
 Project.getCurrentEditor.SelectWord;
end;

(* TMainForm.oShowCompilerOutputClick *)
Procedure TMainForm.oShowCompilerOutputClick(Sender: TObject);
begin
 CompilerOutputForm.Output.Text := Project.CompilerOutput;
 CompilerOutputForm.ShowModal;
end;

(* TMainForm.oAboutClick *)
Procedure TMainForm.oAboutClick(Sender: TObject);
begin
 AboutForm.ShowModal;
end;

(* TMainForm.oCloseCurrentCardClick *)
Procedure TMainForm.oCloseCurrentCardClick(Sender: TObject);
begin
 CodeEditor.opCloseCard.Click;
end;

(* TMainForm.oCloseProjectClick *)
Procedure TMainForm.oCloseProjectClick(Sender: TObject);
begin
 if (SaveProject) Then
  FreeAndNil(Project);
end;

(* TMainForm.oEvSettingsClick *)
Procedure TMainForm.oEvSettingsClick(Sender: TObject);
begin
 EvSettingsForm.Run;
end;

(* TMainForm.oNewProj_AppClick *)
Procedure TMainForm.oNewProject_ApplicationClick(Sender: TObject);
begin
 if (SaveProject) Then
 Begin
  FreeAndNil(Project);
  Project := TProject.Create;

  Project.NewProject(ptApplication);

  Caption := Format('%s - %s', [BaseCaption, Language.getText(ls_new_app)]);
 End;
end;

(* TMainForm.oNewProj_LibraryClick *)
Procedure TMainForm.oNewProject_LibraryClick(Sender: TObject);
begin
 if (SaveProject) Then
 Begin
  FreeAndNil(Project);
  Project := TProject.Create;

  Project.NewProject(ptLibrary);

  Caption := Format('%s - %s', [BaseCaption, Language.getText(ls_new_lib)]);
 End;
end;

(* TMainForm.oProjectSettingsClick *)
Procedure TMainForm.oProjectSettingsClick(Sender: TObject);
begin
 ProjectSettingsForm.Run;
end;

(* TMainForm.oExitClick *)
Procedure TMainForm.oExitClick(Sender: TObject);
begin
 Close;
end;

(* TMainForm.oOpenProjectClick *)
Procedure TMainForm.oOpenProjectClick(Sender: TObject);
begin
 if (SaveProject) Then
 Begin
  FreeAndNil(Project);

  oOpen.Click;
 End;
end;

(* TMainForm.oCompileAndRunClick *)
Procedure TMainForm.oBuildAndRunClick(Sender: TObject);
begin
 if (Project.Compile) Then
  Project.Run;
end;

(* TMainForm.oNewModuleCkick *)
Procedure TMainForm.oNewModuleClick(Sender: TObject);
begin
 Project.NewNoNameCard;
end;

(* TMainForm.oSaveAllClick *)
Procedure TMainForm.oSaveAllClick(Sender: TObject);
begin
 Project.Save;
end;

(* TMainForm.oSaveAsClick *)
Procedure TMainForm.oSaveAsClick(Sender: TObject);
begin
 Project.Save;
 Project.SaveCurrentCardAs;
end;

(* TMainForm.oSaveClick *)
Procedure TMainForm.oSaveClick(Sender: TObject);
begin
 Project.Save;
 Project.SaveCurrentCard;
end;

(* TMainForm.oBuildClick *)
Procedure TMainForm.oBuildClick(Sender: TObject);
begin
 Project.Compile;
end;

(* TMainForm.oCopyClick *)
Procedure TMainForm.oCopyClick(Sender: TObject);
begin
 Project.getCurrentEditor.CopyToClipboard;
end;

(* TMainForm.oCutClick *)
Procedure TMainForm.oCutClick(Sender: TObject);
begin
 Project.getCurrentEditor.CutToClipboard;
end;

(* TMainForm.FormCloseQuery *)
Procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
 if (Project <> nil) and (Project.VMProcess <> nil) Then // if VM is running
 Begin
  Case MessageDlg(Language.getText(ls_msg_info), Language.getText(ls_dlg_stop_vm), mtConfirmation, [mbYes, mbNo], 0) of
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

 // save current project as the "Recent" and add it into the "Recently opened" list (if possible)
 if (CanClose) and (Project <> nil) Then
 Begin
  if (Project.Named) Then
  Begin
   Config.setString(ceRecentProject, Project.FileName);
   AddRecentlyOpened(Project.FileName);
  End;
 End;
end;

(* TMainForm.oOpenClick *)
Procedure TMainForm.oOpenClick(Sender: TObject);
begin
 // create open dialog
 With TOpenDialog.Create(MainForm) do
 Begin
  Try
   if (Project = nil) Then // no project opened - show project opening dialog
   Begin
    Title  := Language.getText(ls_project_opening);
    Filter := Language.getText(ls_filter_project);
   End Else
   Begin // in other case - show module opening dialog
    Title  := Language.getText(ls_file_opening);
    Filter := Language.getText(ls_filter_any_file);
   End;

   Options := [ofPathMustExist, ofFileMustExist];

   if (Execute) Then
   Begin
    { opening a project }
    if (Project = nil) or (CompareText(ExtractFileExt(FileName), '.ssp') = 0) Then
    Begin
     if (SaveProject) Then
     Begin
      FreeAndNil(Project);
      Project := TProject.Create;

      // try to load the project
      if (not Project.Load(FileName)) Then
      Begin
       ErrorMessage(ls_msg_project_open_failed);
       FreeAndNil(Project);
      End;
     End;
    End Else

    { opening a module }
    Begin
     // try to open card
     if (Project.OpenCard(FileName) = nil) Then
     Begin
      ErrorMessage(ls_msg_module_open_failed);
     End;
    End;
   End;
  Finally
   Free;
  End;
 End;
end;

(* TMainForm.oPasteClick *)
Procedure TMainForm.oPasteClick(Sender: TObject);
begin
 Project.getCurrentEditor.PasteFromClipboard;
end;

(* TMainForm.oRedoClick *)
Procedure TMainForm.oRedoClick(Sender: TObject);
begin
 Project.getCurrentEditor.Redo;
end;

(* TMainForm.oStopProgram *)
Procedure TMainForm.oStopProgramClick(Sender: TObject);
begin
 Project.StopProgram;
end;

(* TMainForm.oUncommentSelectedClick *)
Procedure TMainForm.oUncommentSelectedClick(Sender: TObject);
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
Procedure TMainForm.oUndoClick(Sender: TObject);
begin
 Project.getCurrentEditor.Undo;
end;

(* TMainForm.UpdateTimerTimer *)
Procedure TMainForm.UpdateTimerTimer(Sender: TObject);
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
