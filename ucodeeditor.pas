(*
 Copyright © by Patryk Wychowaniec, 2013-2014
 All rights reserved.
*)
unit uCodeEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, ExtendedNotebook, Forms, Controls, Graphics,
  Dialogs, ExtCtrls, Menus, LCLType, StdCtrls, ComCtrls;

type

  { TCodeEditor }

  TCodeEditor = class(TForm)
    Panel1: TPanel;
    CurrentFunction: TEdit;
    MenuItem10: TMenuItem;
    opFindDeclaration: TMenuItem;
    opOpenInclude: TMenuItem;
    opCloseAll: TMenuItem;
    opCloseCard: TMenuItem;
    opCopy: TMenuItem;
    opCut: TMenuItem;
    opPaste: TMenuItem;
    StatusBar: TStatusBar;
    SynEditPopup: TPopupMenu;
    Tabs: TExtendedNotebook;
    TabsPopup: TPopupMenu;
    TabsUpdate: TTimer;
    CurrentFunctionUpdate: TTimer;
    IdentifierListUpdate: TTimer;
    procedure CurrentFunctionUpdateTimer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure IdentifierListUpdateTimer(Sender: TObject);
    procedure opCloseAllClick(Sender: TObject);
    procedure opCloseCardClick(Sender: TObject);
    procedure opFindDeclarationClick(Sender: TObject);
    procedure opCopyClick(Sender: TObject);
    procedure opCutClick(Sender: TObject);
    procedure opOpenIncludeClick(Sender: TObject);
    procedure opPasteClick(Sender: TObject);
    procedure TabsChange(Sender: TObject);
    procedure TabsTabDragDropEx(Sender, Source: TObject; OldIndex,
      NewIndex: Integer; CopyDrag: Boolean; var Done: Boolean);
    procedure TabsUpdateTimer(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  CodeEditor: TCodeEditor;

implementation
Uses uMainForm, mProject, mFunctions, mLanguages, mMessages, CodeScan;

{$R *.lfm}

(* TCodeEditor.TabsTabDragDropEx *)
procedure TCodeEditor.TabsTabDragDropEx(Sender, Source: TObject; OldIndex,
  NewIndex: Integer; CopyDrag: Boolean; var Done: Boolean);
begin
 if (Project <> nil) Then
  Project.SwapCards(OldIndex, NewIndex);
end;

(* TCodeEditor.opCutClick *)
procedure TCodeEditor.opCutClick(Sender: TObject);
begin
 Project.getCurrentEditor.CutToClipboard;
end;

(* TCodeEditor.opOpenIncludeClick *)
procedure TCodeEditor.opOpenIncludeClick(Sender: TObject);
begin
 raise Exception.Create('Unimplemented!'); // @TODO
end;

(* TCodeEditor.opPasteClick *)
procedure TCodeEditor.opPasteClick(Sender: TObject);
begin
 Project.getCurrentEditor.PasteFromClipboard;
end;

(* TCodeEditor.TabsChange *)
procedure TCodeEditor.TabsChange(Sender: TObject);
begin
end;

(* TCodeEditor.opCopyClick *)
procedure TCodeEditor.opCopyClick(Sender: TObject);
begin
 Project.getCurrentEditor.CopyToClipboard;
end;

(* TCodeEditor.opFindDeclarationClick *)
procedure TCodeEditor.opFindDeclarationClick(Sender: TObject);
Var Identifier: TIdentifier;
    Word      : String;
begin
 With Project do
 Begin
  getCurrentEditor.SelectWord;
  Word := getCurrentEditor.SelText;

  // if keyword
  if (isKeyword(Word)) Then
  Begin
   InfoMessage(ls_its_keyword);
  End Else

  // if internal type
  if (isInternalType(Word)) Then
  Begin
   InfoMessage(ls_its_internal_type);
  End Else

  // if number
  if (isNumber(Word)) Then
  Begin
   InfoMessage(ls_its_number);
  End Else

  // otherwise, try to find it
  Begin
   if (Project.FindIdentifier(getCurrentEditor.BlockBegin, Identifier)) Then // if found...
   Begin // jump to its declaration
    JumpToDeclaration(Identifier);
   End Else
   Begin // display error
    ErrorMessage(ls_declaration_not_found);
   End;
  End;
 End;
end;

(* TCodeEditor.opCloseCardClick *)
procedure TCodeEditor.opCloseCardClick(Sender: TObject);
begin
 Project.CloseCard(CodeEditor.Tabs.ActivePageIndex); // close current card
end;

(* TCodeEditor.opCloseAllClick *)
procedure TCodeEditor.opCloseAllClick(Sender: TObject);
begin
 Project.CloseCardsExcluding(CodeEditor.Tabs.ActivePageIndex); // close each card except that currently opened
end;

(* TCodeEditor.CurrentFunctionUpdateTimer *)
procedure TCodeEditor.CurrentFunctionUpdateTimer(Sender: TObject);
Var Func     : TFunction;
    Namespace: TNamespace;
    Str      : String = '';
begin
 if (Project = nil) or (Project.getCurrentCard = nil) Then
 Begin
  CurrentFunction.Text := '';
  Exit;
 End;

 Namespace := Project.getCurrentCard.GetNamespaceAtCaret;
 Func      := Project.getCurrentCard.GetFunctionAtCaret;

 if (Func = nil) Then
 Begin
  if (Namespace <> nil) Then
   Str := 'namespace '+Namespace.getName;
 End Else
 Begin
  Str := 'function<'+Func.getReturnType+'> ';

  if (Namespace = nil) or (Namespace.getName = 'self') Then
   Str += Func.getName Else
   Str += Namespace.getName+'::'+Func.getName;

  Str += '('+Func.getParamTypesAsString+')';
 End;

 if (CurrentFunction.Text <> Str) Then
  CurrentFunction.Text := Str;
end;

(* TCodeEditor.FormCreate *)
procedure TCodeEditor.FormCreate(Sender: TObject);
begin
 AllowDropFiles := True;
end;

(* TCodeEditor.FormDropFiles *)
procedure TCodeEditor.FormDropFiles(Sender: TObject; const FileNames: Array of String);
begin
 MainForm.FormDropFiles(Sender, FileNames);
end;

(* TCodeEditor.IdentifierListUpdateTimer *)
procedure TCodeEditor.IdentifierListUpdateTimer(Sender: TObject);
begin
 if (Project <> nil) Then
  Project.UpdateIdentifierList;
end;

(* TCodeEditor.TabsUpdateTimer *)
procedure TCodeEditor.TabsUpdateTimer(Sender: TObject);
begin
 if (Project = nil) or (Project.getCurrentCard = nil) Then // no project created / no card opened
  Exit;

 Project.UpdateCards;

 With Project.getCurrentEditor do
  CodeEditor.StatusBar.Panels[0].Text := IntToStr(CaretY)+': '+IntToStr(CaretX);

 With Project.getCurrentCard do
  CodeEditor.StatusBar.Panels[1].Text := getFileName;
end;

end.

