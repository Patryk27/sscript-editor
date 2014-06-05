(*
 Copyright © by Patryk Wychowaniec, 2013-2014
 All rights reserved.
*)
unit uEvSettingsForm;

{$mode objfpc}{$H+}

interface

uses
  {$IF defined(Windows)}
   Windows, ShellApi,
  {$ENDIF}

  Classes, LCLType, SysUtils, FileUtil, SynEdit, Forms, Controls, Graphics, Dialogs,
  ComCtrls, ExtCtrls, StdCtrls, Buttons, EditBtn, Spin, ColorBox, Registry;

type

  { TEvSettingsForm }

  TEvSettingsForm = class(TForm)
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    Bevel4: TBevel;
    Bevel5: TBevel;
    btnDeleteStyle: TBitBtn;
    btnNewStyle: TBitBtn;
    btnCancel: TBitBtn;
    btnRenameStyle: TBitBtn;
    btnSave: TBitBtn;
    btnVMSelect: TBitBtn;
    btnAssociateEditor: TButton;
    cbAddBrackets:TCheckBox;
    cbBold: TCheckBox;
    cbEnableUndoAfterSave: TCheckBox;
    cbItalic: TCheckBox;
    cbLanguages:TComboBox;
    cbOpenRecentProject: TCheckBox;
    cbScrollPastLineLength: TCheckBox;
    cbStyleList: TComboBox;
    cbEnableLogging: TCheckBox;
    cbRewriteLog: TCheckBox;
    cbBackgroundColor: TCheckBox;
    cbUnderline: TCheckBox;
    cbTextColor: TCheckBox;
    cbEnableIntellisense: TCheckBox;
    clrbBackgroundColor: TColorBox;
    clrbTextColor: TColorBox;
    clrbFontColor: TColorBox;
    clrbBackgroundColor2: TColorBox;
    cbFontList: TComboBox;
    btnCompilerSelect: TBitBtn;
    Editor_General: TPage;
    EXEOpen: TOpenDialog;
    FileTimer: TTimer;
    eCompilerExecutable: TLabeledEdit;
    eVMExecutable: TLabeledEdit;
    eLogFile: TLabeledEdit;
    lblFontSize: TLabel;
    lblBackgroundColor: TLabel;
    lblFontColor: TLabel;
    lblFontName: TLabel;
    lblMaxRecentlyOpened: TLabel;
    lblCurrentLanguage:TLabel;
    Environment_Language:TPage;
    Editor_CodeCompletion:TPage;
    Environment_Files: TPage;
    lblUndoLimit: TLabel;
    lbTokenKinds: TListBox;
    Editor_Generala: TPage;
    pcCodeCompletion: TPageControl;
    Pages: TNotebook;
    Editor_SyntaxHighlighting: TPage;
    SettingList: TTreeView;
    SampleCode: TSynEdit;
    seMaxRecentlyOpened: TSpinEdit;
    seFontSize: TSpinEdit;
    seUndoLimit: TSpinEdit;
    tsFont: TTabSheet;
    tsSyntaxHighlighting: TTabSheet;

    Procedure btnAssociateEditorClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    Procedure btnDeleteStyleClick(Sender: TObject);
    Procedure btnNewStyleClick(Sender: TObject);
    Procedure btnRenameStyleClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnVMSelectClick(Sender: TObject);
    procedure btnCompilerSelectClick(Sender: TObject);
    Procedure cbBackgroundColorChange(Sender: TObject);
    Procedure cbBoldChange(Sender: TObject);
    Procedure cbEnableLoggingChange(Sender: TObject);
    Procedure cbFontListChange(Sender: TObject);
    Procedure cbFontListKeyPress(Sender: TObject; var Key: char);
    Procedure cbItalicChange(Sender: TObject);
    Procedure cbStyleListChange(Sender: TObject);
    Procedure cbTextColorChange(Sender: TObject);
    Procedure cbUnderlineChange(Sender: TObject);
    Procedure clrbBackgroundColor2Change(Sender: TObject);
    Procedure clrbBackgroundColorChange(Sender: TObject);
    Procedure clrbFontColorChange(Sender: TObject);
    Procedure clrbTextColorChange(Sender: TObject);
    procedure eCompilerExecutableChange(Sender: TObject);
    procedure eVMExecutableChange(Sender: TObject);
    procedure FileTimerTimer(Sender: TObject);
    Procedure lbTokenKindsSelectionChange(Sender: TObject; User: boolean);
    Procedure seFontSizeChange(Sender: TObject);
    procedure SettingListChange(Sender:TObject;Node:TTreeNode);

  private
   DontUpdate: Boolean;

   Function AskForStyleName(var StyleName: String; out StyleFile: String): Boolean;

   procedure UpdateStyleList;

   Procedure ApplyStyleChanges;

   Procedure UpdateSampleCode;
   Procedure UpdateSyntaxHighlightingComponents;

   Procedure SaveCurrentStyle;

  public
   Procedure Run;
  end;

var
  EvSettingsForm: TEvSettingsForm;

implementation
Uses mConfiguration, mStyles, mProject, mLanguages, mFunctions, mMessages,
     SynEditSScript;

Const PageArray: Array[0..6] of String =
      ('Environment_Files', 'Environment_Files', 'Environment_Language', 'Editor_General', 'Editor_General', 'Editor_CodeCompletion', 'Editor_SyntaxHighlighting');

Var ConfigBackup: TConfiguration;

    SelectedStyle: TStyle;
    isStyleSaved : Boolean;

    StyleList: TStringList; // the same as 'StyleManager.getStyleList'

    Highlighter: TSScriptHighlighter = nil;
    CheckTime  : Integer = 0;

    SelSyntaxFormatKind: TSyntaxFormatEnum;
    SelSyntaxFormat    : TSyntaxFormat;

{$R *.lfm}

// getLanguageName
Function getLanguageName(FileName: String): String;
Begin
 Result := Copy(FileName, 1, LastDelimiter('.', FileName)-1);
End;

// SearchLanguages
Procedure SearchLanguages;
Var M: TSearchRec;
Begin
 With EvSettingsForm.cbLanguages do
 Begin
  Clear;
  Items.Add('English');
 End;

 if (FindFirst(getLanguagesDir+'*.lng', faAnyFile, M) <> 0) Then
  Exit; // no files found

 Repeat
  EvSettingsForm.cbLanguages.Items.Add(getLanguageName(M.Name)); // add next file
 Until (FindNext(M) <> 0);

 FindClose(M);

 With EvSettingsForm.cbLanguages do
 Begin
  ItemIndex := Items.IndexOf(getLanguageName(Config.getString(ceLanguage)));

  if (ItemIndex = -1) Then
   ItemIndex := 0;
 End;
End;

// -------------------------------------------------------------------------- //
(* TEvSettingsForm.AskForStyleName *)
Function TEvSettingsForm.AskForStyleName(var StyleName: String; out StyleFile: String): Boolean;
Label Again; // well, at least its better then recursion :P
Begin
 Again:

 if (InputQuery(Language.getText(ls_caption_dlg_style_name), Language.getText(ls_dlg_style_name), StyleName)) Then
 Begin
  StyleFile := getStylesDir+GenerateStyleFileName(StyleName);

  // check for duplicates
  if (cbStyleList.Items.indexOf(StyleName) > -1) or (FileExists(StyleFile)) Then
  Begin
   ErrorMessage(ls_msg_style_already_exists);
   goto Again;
  End;

  Exit(True);
 End Else
  Exit(False);
End;

(* TEvSettigsForm.UpdateStyleList *)
Procedure TEvSettingsForm.UpdateStyleList;
Var Tmp: Boolean;
    I  : uint32;
Begin
 Tmp        := DontUpdate;
 DontUpdate := True;

 StyleManager.UpdateStyleList;
 StyleList := StyleManager.getStyleList;

 cbStyleList.Items.Clear;

 if (StyleList.Count > 0) Then
 Begin
  cbStyleList.ItemIndex := -1;

  // iterate each item
  For I := 0 To StyleList.Count-1 Do
  Begin
   cbStyleList.Items.Add(StyleList.Names[I]);

   if (AnsiCompareStr(StyleList.Names[I], SelectedStyle.getName) = 0) Then
    cbStyleList.ItemIndex := I;
  End;

  if (cbStyleList.ItemIndex = -1) Then
   raise Exception.Create('Something went wrong: cbStyleList.ItemIndex = -1');
 End Else
 Begin
  raise Exception.Create('Something went wrong: StyleList.Count = 0');
 End;

 DontUpdate := Tmp;
End;

(* TEvSettingsForm.ApplyStyleChanges *)
{
 Applies changes made on the "Syntax highlighting" and "Font" tab.
}
Procedure TEvSettingsForm.ApplyStyleChanges;
Var mColor: TColor;
    mFont : TFont;
Begin
 if (DontUpdate) Then
  Exit;

 isStyleSaved := False;

 // editor background color
 mColor       := SelectedStyle.getColor(ceEditorBackground);
 mColor.Color := clrbBackgroundColor2.Selected;
 SelectedStyle.setColor(ceEditorBackground, mColor);

 // editor font
 mFont := SelectedStyle.getFont(feEditorFont);

 if (cbFontList.ItemIndex > -1) Then
  mFont.Name  := cbFontList.Items[cbFontList.ItemIndex];

 mFont.Size  := seFontSize.Value;
 mFont.Color := clrbFontColor.Selected;
 SelectedStyle.setFont(feEditorFont, mFont);

 // selected syntax format
 SelSyntaxFormat.HasFGColor := cbTextColor.Checked;
 SelSyntaxFormat.HasBGColor := cbBackgroundColor.Checked;

 SelSyntaxFormat.FGColor := clrbTextColor.Selected;
 SelSyntaxFormat.BGColor := clrbBackgroundColor.Selected;

 SelSyntaxFormat.Bold      := cbBold.Checked;
 SelSyntaxFormat.Italic    := cbItalic.Checked;
 SelSyntaxFormat.Underline := cbUnderline.Checked;

 SelectedStyle.setSyntaxFormat(SelSyntaxFormatKind, SelSyntaxFormat);

 // update sample code
 UpdateSampleCode;
End;

(* TEvSettingsForm.UpdateSampleCode *)
{
 Updates the SynEdit with sample code.
}
Procedure TEvSettingsForm.UpdateSampleCode;
Begin
 Highlighter.Free;

 Highlighter            := TSScriptHighlighter.Create(SampleCode);
 SampleCode.Highlighter := Highlighter;

 Highlighter.UpdateStyle(SelectedStyle);
End;

(* TEvSettingsForm.UpdateSyntaxHighlightingComponents *)
Procedure TEvSettingsForm.UpdateSyntaxHighlightingComponents;
Var EditorFont: TFont;
Begin
 // fetch font
 EditorFont := SelectedStyle.getFont(feEditorFont);

 // update font list
 cbFontList.ItemIndex := cbFontList.Items.indexOf(EditorFont.Name);

 // update font size and color
 seFontSize.Value              := EditorFont.Size;
 clrbFontColor.Selected        := EditorFont.Color;
 clrbBackgroundColor2.Selected := SelectedStyle.getColor(ceEditorBackground).Color;

 // update the "Syntax highlighting" tab
 lbTokenKinds.OnSelectionChange(nil, False);
End;

(* TEvSettingsForm.SaveCurrentStyle *)
Procedure TEvSettingsForm.SaveCurrentStyle;
Begin
 if (isStyleSaved) Then
  Exit;

 SelectedStyle.SaveToFile;

 isStyleSaved := True;
End;

(* TEvSettingsForm.Run *)
Procedure TEvSettingsForm.Run;
Begin
 DontUpdate := True;

 // make a backup of current configuration
 ConfigBackup := TConfiguration.Create;

 // clone current style
 SelectedStyle := TStyle.Create(StyleManager.getCurrentStyle.getFileName);
 isStyleSaved  := False;

 // update font list
 cbFontList.Items := Screen.Fonts;

 UpdateSyntaxHighlightingComponents;

 // update style list
 UpdateStyleList;

 isStyleSaved := True;

 // update sample code
 UpdateSampleCode;

 // load config
 seMaxRecentlyOpened.Value   := Config.getInteger(ceMaxRecentlyOpened);
 cbOpenRecentProject.Checked := Config.getBoolean(ceLoadRecentProject);
 eCompilerExecutable.Text    := Config.getString(ceCompilerExecutable);
 eVMExecutable.Text          := Config.getString(ceVMExecutable);
 cbEnableLogging.Checked     := Config.getBoolean(ceEnableLogging);
 cbRewriteLog.Checked        := Config.getBoolean(ceRewriteLog);
 eLogFile.Text               := Config.getString(ceLogFile);

 cbEnableUndoAfterSave.Checked  := Config.getBoolean(ceUndoAfterSave);
 seUndoLimit.Value              := Config.getInteger(ceUndoLimit);
 cbScrollPastLineLength.Checked := Config.getBoolean(ceScrollPastEOL);

 cbEnableIntellisense.Checked := Config.getBoolean(ceEnableIntellisense);
 cbAddBrackets.Checked        := Config.getBoolean(ceAddBrackets);

 // search languages
 SearchLanguages;

 // open the first page
 SettingList.Selected := SettingList.Items[0];

 // show form
 CheckTime := 0;
 FileTimer.OnTimer(FileTimer);

 DontUpdate := False;
 Paint;
 ShowModal;

 // optionally dispose config backup
 if (ConfigBackup <> Config) Then
  ConfigBackup.Free;

 if (SelectedStyle <> StyleManager.getCurrentStyle) Then
  SelectedStyle.Free;
End;

(* TEvSettingsForm.btnSaveClick *)
Procedure TEvSettingsForm.btnSaveClick(Sender: TObject);
Var ShowEnvRestartMessage: Boolean = False;
    Lang                 : String;
begin
 // hide form
 Hide;

 // has the language been changed?
 if (cbLanguages.Items.Count = 0) Then
 Begin
  Lang := '';
 End Else
 Begin
  if (cbLanguages.ItemIndex = 0) Then // English (default) language
   Lang := '' { English language doesn't have its corresponding language file, as it is internal } Else
   Lang := cbLanguages.Items[cbLanguages.ItemIndex]+'.lng';
 End;

 if (Lang <> Config.getString(ceLanguage)) Then
 Begin
  ShowEnvRestartMessage := True;
 End;

 // has the logger setings been changed?
 if (cbEnableLogging.Enabled <> Config.getBoolean(ceEnableLogging)) or
    (cbRewriteLog.Enabled <> Config.getBoolean(ceEnableLogging)) or
    (CompareFilenames(eLogFile.Text, Config.getString(ceLogFile)) <> 0) Then
 Begin
  ShowEnvRestartMessage := True;
 End;

 // optionally show message about IDE required to be restarted to see the changes
 if (ShowEnvRestartMessage) Then
 Begin
  InfoMessage(ls_msg_env_restart);
  // @TODO: "would you like to do it now?" dialog?
 End;

 // save new settings
 Config.setInteger(ceMaxRecentlyOpened, seMaxRecentlyOpened.Value);
 Config.setBoolean(ceLoadRecentProject, cbOpenRecentProject.Checked);
 Config.setString(ceCompilerExecutable, eCompilerExecutable.Text);
 Config.setString(ceVMExecutable, eVMExecutable.Text);
 Config.setBoolean(ceEnableLogging, cbEnableLogging.Checked);
 Config.setBoolean(ceRewriteLog, cbRewriteLog.Checked);
 Config.setString(ceLogFile, eLogFile.Text);

 Config.setBoolean(ceUndoAfterSave, cbEnableUndoAfterSave.Checked);
 Config.setInteger(ceUndoLimit, seUndoLimit.Value);
 Config.setBoolean(ceScrollPastEOL, cbScrollPastLineLength.Checked);

 Config.setBoolean(ceEnableIntellisense, cbEnableIntellisense.Checked);
 Config.setBoolean(ceAddBrackets, cbAddBrackets.Checked);

 Config.setString(ceLanguage, Lang);

 // save style
 SaveCurrentStyle;
 Config.setString(ceStyleFile, ExtractFileName(StyleList.Values[StyleList.Names[cbStyleList.ItemIndex]]));

 // force style reload
 StyleManager.ReloadCurrentStyle;

 // refresh project, if possible
 if (Project <> nil) Then
  Project.RefreshSettings;

 // close form
 Close;
end;

(* TEvSettingsForm.VMFile_SelectClick *)
Procedure TEvSettingsForm.btnVMSelectClick(Sender: TObject);
begin
 if (EXEOpen.Execute) Then
  eVMExecutable.Text := EXEOpen.FileName;
end;

(* TEvSettingsForm.CompilerFile_SelectClick *)
Procedure TEvSettingsForm.btnCompilerSelectClick(Sender: TObject);
begin
 if (EXEOpen.Execute) Then
  eCompilerExecutable.Text := EXEOpen.FileName;
end;

(* TEvSettingsForm.cbBackgroundColorChange *)
Procedure TEvSettingsForm.cbBackgroundColorChange(Sender: TObject);
Begin
 clrbBackgroundColor.Enabled := cbBackgroundColor.Checked;
 ApplyStyleChanges;
End;

(* TEvSettingsForm.cbBoldChange *)
Procedure TEvSettingsForm.cbBoldChange(Sender: TObject);
Begin
 ApplyStyleChanges;
end;

(* TEvSettingsForm.cbEnableLoggingChange *)
Procedure TEvSettingsForm.cbEnableLoggingChange(Sender: TObject);
Var Tmp: Boolean;
Begin
 Tmp := cbEnableLogging.Checked;

 cbRewriteLog.Enabled := Tmp;
 eLogFile.Enabled     := Tmp;
End;

(* TEvSettingsForm.cbFontListChange *)
Procedure TEvSettingsForm.cbFontListChange(Sender: TObject);
Begin
 ApplyStyleChanges;
End;

(* TEvSettingsForm.cbFontListKeyPress *)
Procedure TEvSettingsForm.cbFontListKeyPress(Sender: TObject; var Key: char);
Begin
 if (Key = #13) Then
 Begin
  cbFontList.ItemIndex := cbFontList.Items.indexOf(cbFontList.Text);

  ApplyStyleChanges;
 End;
End;

(* TEvSettingsForm.cbItalicChange *)
Procedure TEvSettingsForm.cbItalicChange(Sender: TObject);
Begin
 ApplyStyleChanges;
end;

(* TEvSettingsForm.cbStyleListChange *)
Procedure TEvSettingsForm.cbStyleListChange(Sender: TObject);
Begin
 if (DontUpdate) Then
  Exit;

 // if current style has been modified, ask user if he wants to save the changes
 if (not isStyleSaved) Then
 Begin
  Case MessageDlg(Language.getText(ls_dlg_save_style), mtConfirmation, [mbYes, mbNo], 0) of
   mrYes: SaveCurrentStyle;
  End;
 End;

 // reload style
 SelectedStyle.Free;
 SelectedStyle := TStyle.Create(StyleList.Values[StyleList.Names[cbStyleList.ItemIndex]]);
 isStyleSaved  := True;

 UpdateSyntaxHighlightingComponents;
 UpdateSampleCode;
End;

(* TEvSettingsForm.cbTextColorChange *)
Procedure TEvSettingsForm.cbTextColorChange(Sender: TObject);
Begin
 clrbTextColor.Enabled := cbTextColor.Checked;
 ApplyStyleChanges;
End;

(* TEvSettingsForm.cbUnderlineChange *)
Procedure TEvSettingsForm.cbUnderlineChange(Sender: TObject);
Begin
 ApplyStyleChanges;
end;

(* TEvSettingsForm.clrbBackgroundColor2Change *)
Procedure TEvSettingsForm.clrbBackgroundColor2Change(Sender: TObject);
Begin
 ApplyStyleChanges;
End;

(* TEvSettingsForm.clrbBackgroundColorChange *)
Procedure TEvSettingsForm.clrbBackgroundColorChange(Sender: TObject);
Begin
 ApplyStyleChanges;
end;

(* TEvSettingsForm.clrbFontColorChange *)
Procedure TEvSettingsForm.clrbFontColorChange(Sender: TObject);
Begin
 ApplyStyleChanges;
End;

(* TEvSettingsForm.clrbTextColorChange *)
Procedure TEvSettingsForm.clrbTextColorChange(Sender: TObject);
Begin
 ApplyStyleChanges;
end;

(* TEvSettingsForm.eCompilerFileChange *)
Procedure TEvSettingsForm.eCompilerExecutableChange(Sender: TObject);
begin
 CheckTime := 4;
end;

(* TEvSettingsForm.eVMFileChange *)
Procedure TEvSettingsForm.eVMExecutableChange(Sender: TObject);
begin
 CheckTime := 4;
end;

(* TEvSettingsForm.FileTimerTimer *)
Procedure TEvSettingsForm.FileTimerTimer(Sender: TObject);
begin
 Dec(CheckTime);

 // update file status
 if (CheckTime < 0) Then
 Begin
  if (FileExists(eCompilerExecutable.Text)) Then
   eCompilerExecutable.Color := clWhite Else
   eCompilerExecutable.Color := clRed;

  if (FileExists(eVMExecutable.Text)) Then
   eVMExecutable.Color := clWhite Else
   eVMExecutable.Color := clRed;

  if (isValidFileName(eLogFile.Text)) Then
   eLogFile.Color := clWhite Else
   eLogFile.Color := clRed;
 End;
end;

(* TEvSettingsForm.lbTokenKindsSelectionChange *)
Procedure TEvSettingsForm.lbTokenKindsSelectionChange(Sender: TObject; User: boolean);
Begin
 DontUpdate := True;

 SelSyntaxFormatKind := TSyntaxFormatEnum(lbTokenKinds.ItemIndex);
 SelSyntaxFormat     := SelectedStyle.getSyntaxFormat(SelSyntaxFormatKind);

 cbTextColor.Checked    := SelSyntaxFormat.HasFGColor;
 clrbTextColor.Selected := SelSyntaxFormat.FGColor;

 cbBackgroundColor.Checked    := SelSyntaxFormat.HasBGColor;
 clrbBackgroundColor.Selected := SelSyntaxFormat.BGColor;

 cbBold.Checked      := SelSyntaxFormat.Bold;
 cbItalic.Checked    := SelSyntaxFormat.Italic;
 cbUnderline.Checked := SelSyntaxFormat.Underline;

 DontUpdate := False;
End;

(* TEvSettingsForm.seFontSizeChange *)
Procedure TEvSettingsForm.seFontSizeChange(Sender: TObject);
Begin
 ApplyStyleChanges;
End;

(* TEvSettingsForm.SettingChange *)
Procedure TEvSettingsForm.SettingListChange(Sender: TObject; Node: TTreeNode);
Var Page: uint32;
Begin
 if (Node = nil) Then
  Exit;

 For Page := 0 To Pages.PageCount-1 Do
 Begin
  if (Pages.Page[Page].Name = PageArray[Node.AbsoluteIndex]) Then
  Begin
   Pages.PageIndex := Page;
   Exit;
  End;
 End;
End;

(* TEvSettingsForm.btnCancelClick *)
Procedure TEvSettingsForm.btnCancelClick(Sender: TObject);
begin
 // if (not isStyleSaved) or (...) Then // "you'll lose the changes you've made" @TODO

 // restore backup config
 Config.Free;
 Config := ConfigBackup;

 // close form
 Close;
end;

(* TEvSettingsForm.btnAssociateEditorClick *)
Procedure TEvSettingsForm.btnAssociateEditorClick(Sender: TObject);
Var Reg: TRegistry;

 { AssociateExtension }
 Procedure AssociateExtension(const Ext: String);
 Var App: String;
 Begin
  App := ParamStr(0);

  Reg.OpenKey('.'+Ext, True);
  Reg.WriteString('', Ext + 'file');
  Reg.CloseKey;

  Reg.CreateKey(Ext + 'file');
  Reg.OpenKey(Ext + 'file\DefaultIcon', True);
  Reg.WriteString('', App + ',0');
  Reg.CloseKey;

  Reg.OpenKey(Ext + 'file\shell\open\command', True);
  Reg.WriteString('', App + ' "%1"');
  Reg.CloseKey;
 End;

Begin
 Reg := TRegistry.Create;

 Try
  Try
   Reg.RootKey := HKEY_CLASSES_ROOT;

   {$IF defined(Windows)}
    AssociateExtension('ssp');
    AssociateExtension('ss');

    SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, nil, nil);
   {$ELSE}
    raise Exception.Create('Unsupported.');
   {$ENDIF}

   InfoMessage(ls_info_editor_has_been_associated);
  Finally
   Reg.Free;
  End;
 Except
  ErrorMessage(ls_err_cannot_associate_editor);
 End;
End;

(* TEvSettingsForm.btnDeleteStyleClick *)
Procedure TEvSettingsForm.btnDeleteStyleClick(Sender: TObject);
Begin
 Case MessageDlg(Language.getText(ls_dlg_remove_style), mtConfirmation, [mbYes, mbNo], 0) of
  mrNo: Exit;
 End;

 // remove style file
 DeleteFile(SelectedStyle.getFileName);
 FreeAndNil(SelectedStyle);

 // delete items from the lists
 StyleList.Delete(cbStyleList.ItemIndex);
 cbStyleList.Items.Delete(cbStyleList.ItemIndex);

 // check if user didn't remove the last style on the list
 if (StyleList.Count = 0) Then
 Begin
  // if so, create a new default one
  SelectedStyle := TStyle.Create;
  SelectedStyle.ChangeName('Default');
  SelectedStyle.SaveToFile(getStylesDir+'default.xml');
 End Else
 Begin
  // if not, load the first style on the list
  SelectedStyle := TStyle.Create(StyleList.Values[StyleList.Names[0]]);
 End;

 UpdateStyleList;
 UpdateSyntaxHighlightingComponents;
 UpdateSampleCode;
End;

(* TEvSettingsForm.btnNewStyleClick *)
Procedure TEvSettingsForm.btnNewStyleClick(Sender: TObject);
Var StyleName, StyleFile: String;
    TmpStyle            : TStyle;
Begin
 StyleName := '';

 // make sure that current style is saved
 if (not isStyleSaved) Then
 Begin
  Case MessageDlg(Language.getText(ls_dlg_save_style), mtConfirmation, [mbYes, mbNo], 0) of
   mrYes: SaveCurrentStyle;
   mrNo : Exit;
  End;
 End;

 // ask user to choose what to do
 Case QuestionDlg(tsSyntaxHighlighting.Caption, Language.getText(ls_what_do_you_want_to_do), mtConfirmation, [mrYes, Language.getText(ls_dlg_newstyle_create), mrNo, Language.getText(ls_dlg_newstyle_clone), mrCancel, Language.getText(ls_cancel)], 0) of
  mrYes:
  Begin
   if (not AskForStyleName(StyleName, StyleFile)) Then
    Exit;

   SelectedStyle.Free;
   SelectedStyle := TStyle.Create;
   SelectedStyle.ChangeName(StyleName);
   SelectedStyle.SaveToFile(StyleFile);

   UpdateStyleList;
   UpdateSyntaxHighlightingComponents;
   UpdateSampleCode;
  End;

  mrNo:
  Begin
   if (not AskForStyleName(StyleName, StyleFile)) Then
    Exit;

   TmpStyle      := SelectedStyle;
   SelectedStyle := TStyle.Create(TmpStyle);
   SelectedStyle.ChangeName(StyleName);
   SelectedStyle.SaveToFile(StyleFile);

   TmpStyle.Free;

   UpdateStyleList;
   UpdateSyntaxHighlightingComponents;
   UpdateSampleCode;
  End;
 End;
End;

(* TEvSettingsForm.btnRenameStyleClick *)
Procedure TEvSettingsForm.btnRenameStyleClick(Sender: TObject);
Var StyleName, StyleFile: String;
Begin
 StyleName := SelectedStyle.getName;

 // save current style, if necessary
 if (not isStyleSaved) Then
 Begin
  Case MessageDlg(Language.getText(ls_dlg_save_style), mtConfirmation, [mbYes, mbNo], 0) of
   mrNo: Exit;
  End;
 End;

 // ask user to enter the new name
 if (AskForStyleName(StyleName, StyleFile)) Then
 Begin
  // change style name
  SelectedStyle.ChangeName(StyleName);

  // change style file
  if (CompareFilenames(Config.getString(ceStyleFile), SelectedStyle.getFileName) = 0) Then
   Config.setString(ceStyleFile, StyleFile);

  DeleteFile(SelectedStyle.getFileName);
  SelectedStyle.SaveToFile(StyleFile);

  SaveCurrentStyle;

  // update style list
  UpdateStyleList;
 End;
End;
end.

