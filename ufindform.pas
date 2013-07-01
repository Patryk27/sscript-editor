(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
unit uFindForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, LCLType, SynEdit, SynEditTypes;

Type TFindOrReplace = (frFind, frReplace);

type

  { TFindForm }

  TFindForm = class(TForm)
    Bevel1: TBevel;
    btnFind: TButton;
    btnCancel: TButton;
    btnReplaceAll: TButton;
    cgOptions: TCheckGroup;
    cbReplace: TCheckBox;
    eReplace: TEdit;
    eFind: TEdit;
    Label1: TLabel;
    rgSearchIn: TRadioGroup;
    rgSearchDir: TRadioGroup;
    rgSearchPosition: TRadioGroup;
    procedure btnCancelClick(Sender: TObject);
    procedure btnFindClick(Sender: TObject);
    procedure btnReplaceAllClick(Sender: TObject);
    procedure cbReplaceChange(Sender: TObject);
    procedure eFindKeyPress(Sender: TObject; var Key: char);
    procedure eReplaceKeyPress(Sender: TObject; var Key: char);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
  private
   Procedure OnReplace(Sender: TObject; const ASearch, AReplace: String; Line, Column: Integer; var ReplaceAction: TSynReplaceAction);
  public
   Procedure Run(const Mode: TFindOrReplace);
  end;

var
  FindForm: TFindForm;

implementation
Uses mProject, mLanguages;
Var ReplaceAll: Boolean;

{$R *.lfm}

// -------------------------------------------------------------------------- //
(* TFindForm.OnReplace *)
Procedure TFindForm.OnReplace(Sender: TObject; const ASearch, AReplace: String; Line, Column: Integer; var ReplaceAction: TSynReplaceAction);
Begin
 Case MessageDlg(getLangValue(ls_replace),
                 Format(getLangValue(ls_replace_msg), [ASearch, AReplace]),
                 mtConfirmation, [mbYes, mbNo, mbCancel, mbYesToAll], 0) of
  mrYes     : ReplaceAction := raReplace;
  mrYesToAll: ReplaceAction := raReplaceAll;
  mrNo      : ReplaceAction := raSkip;
  mrCancel  : ReplaceAction := raCancel;

  else
   ReplaceAction := raCancel;
 End;
End;

(* TFindForm.Run *)
Procedure TFindForm.Run(const Mode: TFindOrReplace);
Begin
 Show;
 eFind.SetFocus;
 eFind.SelectAll;
 Hide;

 ReplaceAll := False;

 Case Mode of
  frFind   : cbReplace.Checked := False;
  frReplace: cbReplace.Checked := True;
 End;

 cbReplace.OnChange(nil);

 ShowModal;
End;

// -------------------------------------------------------------------------- //
(* TFindForm.btnCancelClick *)
procedure TFindForm.btnCancelClick(Sender: TObject);
begin
 Close;
end;

(* TFindForm.btnFindClick *)
procedure TFindForm.btnFindClick(Sender: TObject);
Const RegexSpecial: String = '[\^$.|?*+(){}';
Var Editor    : TSynEdit;
    Options   : TSynSearchOptions = [];
    Replaced  : Integer;
    TextToFind: String;
    Ch        : Char;
begin
 Hide;

 TextToFind := eFind.Text;

 Editor := Project.getCurrentEditor;

 Case rgSearchIn.ItemIndex of // search in
  0 { selected text only }:
   Options += [ssoSelectedOnly];

  1 { whole file }:
   Options += [ssoEntireScope];
 End;

 Case rgSearchPosition.ItemIndex of // search mode
  0 { from the cursor }:
  Begin
   Options -= [ssoEntireScope];
   Options += [ssoFindContinue];
  End;

  1 { from the beginning }:
  Begin
   Editor.CaretX := 0;
   Editor.CaretY := 0;
  End;
 End;

 Case rgSearchDir.ItemIndex of // search direction
  0 { up }:
   Options += [ssoBackwards];
 End;

 if (cgOptions.Checked[0]) Then // if case-sensitive
  Options += [ssoMatchCase];

 if (cgOptions.Checked[1]) Then // if whole word only
 Begin
  For Ch in RegexSpecial Do
   TextToFind := StringReplace(TextToFind, Ch, '\'+Ch, [rfReplaceAll]);
  Options += [ssoWholeWord, ssoRegExpr];
 End;

 if (cgOptions.Checked[2]) Then // if regexp
  Options += [ssoRegExpr];

 if (cgOptions.Checked[3]) Then // if multiline regexp
  Options += [ssoRegExprMultiLine];

 if (cbReplace.Checked) Then // if replace-mode
 Begin
  Options += [ssoReplace, ssoPrompt];

  if (ReplaceAll) Then
   Options += [ssoReplaceAll];
 End;

 // replace!
 Editor.OnReplaceText := @OnReplace;
 Replaced             := Editor.SearchReplace(TextToFind, eReplace.Text, Options);

 if (Replaced = 0) Then
  Application.MessageBox(PChar(Format(getLangValue(ls_find_not_found), [eFind.Text])), PChar(getLangValue(ls_find_title)), MB_ICONINFORMATION);

 Close;
end;

(* TFindForm.btnReplaceAllClick *)
procedure TFindForm.btnReplaceAllClick(Sender: TObject);
begin
 ReplaceAll := True;
 btnFind.Click;
end;

(* TFindForm.cbReplaceChange *)
procedure TFindForm.cbReplaceChange(Sender: TObject);
begin
 eReplace.Enabled      := cbReplace.Checked;
 btnReplaceAll.Visible := cbReplace.Checked;

 if (eReplace.Enabled) Then
 Begin
  FindForm.Caption := getLangValue(ls_replace);
  btnFind.Caption  := FindForm.Caption;
 End Else
 Begin
  FindForm.Caption := getLangValue(ls_find);
  btnFind.Caption  := FindForm.Caption;
 End;
end;

(* TFindForm.eTextKeyPress *)
procedure TFindForm.eFindKeyPress(Sender: TObject; var Key: char);
begin
 if (Key = #13) Then
  btnFind.Click;
end;

(* TFindForm.eReplaceKeyPress *)
procedure TFindForm.eReplaceKeyPress(Sender: TObject; var Key: char);
begin
 if (Key = #13) Then
  btnFind.Click;
end;

(* TFindForm.FormCreate *)
procedure TFindForm.FormCreate(Sender: TObject);
begin
 cgOptions.Checked[0] := True; // Case-sensitive
end;

(* TFindForm.FormKeyDown *)
procedure TFindForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
 if (Key = VK_ESCAPE) Then
  Close;
end;

(* TFindForm.FormShow *)
procedure TFindForm.FormShow(Sender: TObject);
begin
 eFind.Left  := 10+Label1.Left+Label1.Width;
 eFind.Width := Width - eFind.Left - 6;

 eReplace.Left  := 5+cbReplace.Left+cbReplace.Width;
 eReplace.Width := Width - eReplace.Left - 6;

 btnReplaceAll.Width := Canvas.TextWidth(btnReplaceAll.Caption)+8;
end;

end.
