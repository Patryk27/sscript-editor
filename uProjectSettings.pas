(*
 Copyright © by Patryk Wychowaniec, 2013-2014
 All rights reserved.
*)
unit uProjectSettings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  StdCtrls, Buttons, Spin;

type

  { TProjectSettingsForm }

  TProjectSettingsForm = class(TForm)
    Bevel1: TBevel;
    cbGCMemoryLimit: TComboBox;
    lblGCMemoryLimit: TLabel;
    mVMOtherSwitches: TMemo;
    mOtherSwitches: TMemo;
    rgOptimizationLevel: TRadioGroup;
    eGCMemoryLimit: TSpinEdit;
    vm_jit: TCheckBox;
    cs__internal_const:TCheckBox;
    vm_time:TCheckBox;
    vm_wait:TCheckBox;
    eBytecodeOutputFile: TEdit;
    lblBytecodeOutputFile: TLabel;
    btnSave: TButton;
    btnCancel: TButton;
    FileTimer: TTimer;
    EXEOpen: TOpenDialog;
    eOutputFile: TEdit;
    eIncludePath: TEdit;
    lblIncludePath: TLabel;
    lblIncludePathDescription: TLabel;
    lblOutputFile: TLabel;
    lblOtherSwitches2:TLabel;
    VMPage_Parameters:TPage;
    CompilerPage_Paths: TPage;
    lblOtherSwitches: TLabel;
    CompilerPage_Switches: TPage;
    Pages: TNotebook;
    SettingList: TTreeView;
    procedure btnCancelClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure SettingListChange(Sender:TObject;Node:TTreeNode);
  private
    { private declarations }
  public
   Procedure Run;
  end;

var
  ProjectSettingsForm: TProjectSettingsForm;

implementation
Uses uMainForm, mProject, mLanguages;

{$R *.lfm}

(* TProjectSettingsForm.Run *)
Procedure TProjectSettingsForm.Run;
Var Switch  : TCompilerSwitch;
    VMSwitch: TVMSwitch;
Begin
 With Project do
 Begin
  // read paths
  eOutputFile.Text     := OutputFile;
  eBytecodeOutputFile.Text := BytecodeOutput;

  // read compiler paths and switches
  mOtherSwitches.Text           := StringReplace(OtherCompilerSwitches, '|', sLineBreak, [rfReplaceAll]);
  eIncludePath.Text             := IncludePath;
  rgOptimizationLevel.ItemIndex := OptimizationLevel;

  For Switch in TCompilerSwitches Do
   TCheckBox(FindComponent(getCompilerSwitchName(Switch, False))).Checked := Switch in CompilerSwitches;

  // read VM switches
  mVMOtherSwitches.Text     := StringReplace(OtherVMSwitches, '|', sLineBreak, [rfReplaceAll]);
  eGCMemoryLimit.Value      := GCMemoryLimit;
  cbGCMemoryLimit.ItemIndex := GCMemoryLimitUnit;

  For VMSwitch in TVMSwitches Do
   TCheckBox(FindComponent(getVMSwitchName(VMSwitch, False))).Checked := VMSwitch in VMSwitches;
 End;

 ShowModal;
End;

(* TProjectSettingsForm.btnSaveClick *)
procedure TProjectSettingsForm.btnSaveClick(Sender: TObject);
Var Switch  : TCompilerSwitch;
    VMSwitch: TVMSwitch;
begin
 With Project do
 Begin
  // save paths
  OutputFile     := eOutputFile.Text;
  BytecodeOutput := eBytecodeOutputFile.Text;

  // save compiler paths and switches
  OtherCompilerSwitches := StringReplace(mOtherSwitches.Text, sLineBreak, '|', [rfReplaceAll]);
  IncludePath           := eIncludePath.Text;
  OptimizationLevel     := rgOptimizationLevel.ItemIndex;

  CompilerSwitches := [];
  For Switch in TCompilerSwitches Do
   if (TCheckBox(FindComponent(getCompilerSwitchName(Switch, False))).Checked) Then
    Include(CompilerSwitches, Switch);

  // save VM switches
  OtherVMSwitches   := StringReplace(mVMOtherSwitches.Text, sLineBreak, '|', [rfReplaceAll]);
  GCMemoryLimit     := eGCMemoryLimit.Value;
  GCMemoryLimitUnit := cbGCMemoryLimit.ItemIndex;

  VMSwitches := [];
  For VMSwitch in TVMSwitches Do
   if (TCheckBox(FindComponent(getVMSwitchName(VMSwitch, False))).Checked) Then
    Include(VMSwitches, VMSwitch);

  Saved := False;

  if (Named) Then
   MainForm.Caption := Format('%s - %s <*>', [uMainForm.BaseCaption, FileName]) Else
   MainForm.Caption := Format('%s - %s <*>', [uMainForm.BaseCaption, Language.getText(ls_new_project_caption)]);
 End;

 Close;
end;

(* TProjectSettingsForm.SettingChange *)
procedure TProjectSettingsForm.SettingListChange(Sender:TObject; Node:TTreeNode);
Var Page: Integer;
begin
 With SettingList do
 Begin
  if (Node = nil) Then
   Exit;

  if (Node.Count = 0) Then
   Page := Node.ImageIndex Else
   Page := Node.Items[0].ImageIndex;

  Pages.PageIndex := Page;
 End;
end;

(* TProject.SettingsForm.btnCancelClick *)
procedure TProjectSettingsForm.btnCancelClick(Sender: TObject);
begin
 Close;
end;

end.

