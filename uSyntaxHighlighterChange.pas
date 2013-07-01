(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
unit uSyntaxHighlighterChange;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, mSettings;

type

  { TSyntaxHighlighterChange }

  TSyntaxHighlighterChange = class(TForm)
    Bevel1: TBevel;
    Bevel2: TBevel;
    btnSave: TButton;
    btnCancel: TButton;
    btnFGColor: TButton;
    btnBGColor: TButton;
    cBGColor: TCheckBox;
    ColorDialog: TColorDialog;
    lTest: TLabel;
    cItalic: TCheckBox;
    cUnderline: TCheckBox;
    cBold: TCheckBox;
    procedure btnBGColorClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnFGColorClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure cBGColorChange(Sender: TObject);
    procedure cBoldChange(Sender: TObject);
    procedure cItalicChange(Sender: TObject);
    procedure cUnderlineChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private
   Procedure UpdateTest;
  public
   Function Run(fDefault: TSyntaxFormat): TSyntaxFormat;
  end;

var
  SyntaxHighlighterChange: TSyntaxHighlighterChange;

implementation
Var Save  : Boolean;
    Format: TSyntaxFormat;

{$R *.lfm}

{ TSyntaxHighlighterChange }

{ TSyntaxHighlighterChange.UpdateTest }
Procedure TSyntaxHighlighterChange.UpdateTest;
Begin
 lTest.Font := FetchFont(getFont(sEditorFont));
 With lTest.Font do
 Begin
  Bold      := Format.Bold;
  Italic    := Format.Italic;
  Underline := Format.Underline;

  Color := Format.FGColor;
 End;

 if (Format.HasBGColor) Then
  lTest.Color := Format.BGColor Else
  lTest.Color := getColor(sEditorBackground);
End;

{ TSyntaxHighlighterChange.Run }
Function TSyntaxHighlighterChange.Run(fDefault: TSyntaxFormat): TSyntaxFormat;
Begin
 Save   := False;
 Format := fDefault;

 cBold.Checked      := Format.Bold;
 cItalic.Checked    := Format.Italic;
 cUnderline.Checked := Format.Underline;
 cBGColor.Checked   := Format.HasBGColor;

 UpdateTest;

 ShowModal;

 if (Save) Then
  Result := Format Else
  Result := fDefault;
End;

procedure TSyntaxHighlighterChange.btnCancelClick(Sender: TObject);
begin
 Close;
end;

procedure TSyntaxHighlighterChange.btnBGColorClick(Sender: TObject);
begin
 ColorDialog.Color := Format.BGColor;
 if (ColorDialog.Execute) Then
 Begin
  Format.BGColor    := ColorDialog.Color;
  Format.HasBGColor := True;
  cBGColor.Checked  := True;
 End;
 UpdateTest;
end;

procedure TSyntaxHighlighterChange.btnFGColorClick(Sender: TObject);
begin
 ColorDialog.Color := Format.FGColor;
 if (ColorDialog.Execute) Then
  Format.FGColor := ColorDialog.Color;
 UpdateTest;
end;

procedure TSyntaxHighlighterChange.btnSaveClick(Sender: TObject);
begin
 Save := True;

 Close;
end;

procedure TSyntaxHighlighterChange.cBGColorChange(Sender: TObject);
begin
 Format.HasBGColor := cBGColor.Checked;
 UpdateTest;
end;

procedure TSyntaxHighlighterChange.cBoldChange(Sender: TObject);
begin
 Format.Bold := cBold.Checked;
 UpdateTest;
end;

procedure TSyntaxHighlighterChange.cItalicChange(Sender: TObject);
begin
 Format.Italic := cItalic.Checked;
 UpdateTest;
end;

procedure TSyntaxHighlighterChange.cUnderlineChange(Sender: TObject);
begin
 Format.Underline := cUnderline.Checked;
 UpdateTest;
end;

procedure TSyntaxHighlighterChange.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
 btnCancel.Click;
end;

end.

