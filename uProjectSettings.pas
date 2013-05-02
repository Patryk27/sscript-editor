unit uProjectSettings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  StdCtrls, Buttons;

type

  { TProjectSettingsForm }

  TProjectSettingsForm = class(TForm)
    Bevel1: TBevel;
    _Ou: TCheckBox;
    c_err:TCheckBox;
    _Cconst:TCheckBox;
    c_time:TCheckBox;
    c_wait:TCheckBox;
    eVMSwitches:TEdit;
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
    _O1: TCheckBox;
    _Op: TCheckBox;
    eCompilerSwitches: TEdit;
    GroupBox2: TGroupBox;
    Label3: TLabel;
    _Of: TCheckBox;
    _initcode: TCheckBox;
    _Or: TCheckBox;
    GroupBox1: TGroupBox;
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

{ TProjectSettingsForm }

{ TProjectSettingsForm.Run }
Procedure TProjectSettingsForm.Run;
Var Switch  : TCompilerSwitch;
    VMSwitch: TVMSwitch;
Begin
 With TProject(getProjectPnt) do
 Begin
  { read paths }
  eOutputFile.Text     := OutputFile;
  eBytecodeOutput.Text := BytecodeOutput;
  eHeaderFile.Text     := HeaderFile;

  { read compiler paths and switches }
  eCompilerSwitches.Text := OtherCompilerSwitches;
  eIncludePath.Text      := IncludePath;

  For Switch in TCompilerSwitches Do
   TCheckBox(FindComponent(getSwitchName(Switch, False))).Checked := Switch in CompilerSwitches;

  { read vm switches }
  eVMSwitches.Text := OtherVMSwitches;

  For VMSwitch in TVMSwitches Do
   TCheckBox(FindComponent(getVMSwitchName(VMSwitch, False))).Checked := VMSwitch in VMSwitches;
 End;

 ShowModal;
End;

procedure TProjectSettingsForm.btnSaveClick(Sender: TObject);
Var Switch  : TCompilerSwitch;
    VMSwitch: TVMSwitch;
begin
 With TProject(getProjectPnt) do
 Begin
  { save paths }
  OutputFile     := eOutputFile.Text;
  BytecodeOutput := eBytecodeOutput.Text;
  HeaderFile     := eHeaderFile.Text;

  { save compiler paths and switches }
  OtherCompilerSwitches := eCompilerSwitches.Text;
  IncludePath           := eIncludePath.Text;

  CompilerSwitches := [];
  For Switch in TCompilerSwitches Do
   if (TCheckBox(FindComponent(getSwitchName(Switch, False))).Checked) Then
    Include(CompilerSwitches, Switch);

  { save vm switches }
  OtherVMSwitches := eVMSwitches.Text;

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

procedure TProjectSettingsForm.SettingChange(Sender:TObject;Node:TTreeNode);
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

procedure TProjectSettingsForm.btnCancelClick(Sender: TObject);
begin
 Close;
end;

end.

