(*
 Copyright © by Patryk Wychowaniec, 2013
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
    Label1: TLabel;
    mVMOtherSwitches: TMemo;
    mOtherSwitches: TMemo;
    rgOptimizationLevel: TRadioGroup;
    eGCMemoryLimit: TSpinEdit;
    vm_jit: TCheckBox;
    cs_Cconst:TCheckBox;
    vm_time:TCheckBox;
    vm_wait:TCheckBox;
    eHeaderFile: TEdit;
    eBytecodeOutput: TEdit;
    Label7: TLabel;
    Label8: TLabel;
    btnSave: TButton;
    btnCancel: TButton;
    FileTimer: TTimer;
    EXEOpen: TOpenDialog;
    eOutputFile: TEdit;
    eIncludePath: TEdit;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label9:TLabel;
    p_4:TPage;
    p_3: TPage;
    Label3: TLabel;
    cs_initcode: TCheckBox;
    p_2: TPage;
    Pages: TNotebook;
    Setting: TTreeView;
    procedure btnCancelClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure SettingChange(Sender:TObject;Node:TTreeNode);
  private
    { private declarations }
  public
   Procedure Run;
  end;

var
  ProjectSettingsForm: TProjectSettingsForm;

implementation
Uses uMainForm, mProject;

{$R *.lfm}

(* TProjectSettingsForm.Run *)
Procedure TProjectSettingsForm.Run;
Var Switch  : TCompilerSwitch;
    VMSwitch: TVMSwitch;
Begin
 With Project do
 Begin
  { read paths }
  eOutputFile.Text     := OutputFile;
  eBytecodeOutput.Text := BytecodeOutput;
  eHeaderFile.Text     := HeaderFile;

  { read compiler paths and switches }
  mOtherSwitches.Text           := StringReplace(OtherCompilerSwitches, '|', sLineBreak, [rfReplaceAll]);
  eIncludePath.Text             := IncludePath;
  rgOptimizationLevel.ItemIndex := OptimizationLevel;

  For Switch in TCompilerSwitches Do
   TCheckBox(FindComponent(getCompilerSwitchName(Switch, False))).Checked := Switch in CompilerSwitches;

  { read vm switches }
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
  { save paths }
  OutputFile     := eOutputFile.Text;
  BytecodeOutput := eBytecodeOutput.Text;
  HeaderFile     := eHeaderFile.Text;

  { save compiler paths and switches }
  OtherCompilerSwitches := StringReplace(mOtherSwitches.Text, sLineBreak, '|', [rfReplaceAll]);
  IncludePath           := eIncludePath.Text;
  OptimizationLevel     := rgOptimizationLevel.ItemIndex;

  CompilerSwitches := [];
  For Switch in TCompilerSwitches Do
   if (TCheckBox(FindComponent(getCompilerSwitchName(Switch, False))).Checked) Then
    Include(CompilerSwitches, Switch);

  { save vm switches }
  OtherVMSwitches   := StringReplace(mVMOtherSwitches.Text, sLineBreak, '|', [rfReplaceAll]);
  GCMemoryLimit     := eGCMemoryLimit.Value;
  GCMemoryLimitUnit := cbGCMemoryLimit.ItemIndex;

  VMSwitches := [];
  For VMSwitch in TVMSwitches Do
   if (TCheckBox(FindComponent(getVMSwitchName(VMSwitch, False))).Checked) Then
    Include(VMSwitches, VMSwitch);

  Saved := False;

  if (Named) Then
   MainForm.Caption := uMainForm.sCaption+' - '+FileName+' <*>' Else
   MainForm.Caption := uMainForm.sCaption+' - nowy projekt <*>';
 End;

 Close;
end;

(* TProjectSettingsForm.SettingChange *)
procedure TProjectSettingsForm.SettingChange(Sender:TObject; Node:TTreeNode);
Var Page: Integer;
begin
 With Setting do
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

