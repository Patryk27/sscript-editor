(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
unit uCodeEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, ExtendedNotebook, Forms, Controls, Graphics,
  Dialogs, ExtCtrls, Menus, AnchorDocking, LCLType, StdCtrls;

type

  { TCodeEditor }

  TCodeEditor = class(TForm)
    CurrentFunction: TEdit;
    MenuItem10: TMenuItem;
    opFindDeclaration: TMenuItem;
    opOpenInclude: TMenuItem;
    opCloseAll: TMenuItem;
    opCloseCard: TMenuItem;
    opCopy: TMenuItem;
    opCut: TMenuItem;
    opPaste: TMenuItem;
    Panel1: TPanel;
    SynEditPopup: TPopupMenu;
    Tabs: TExtendedNotebook;
    TabsPopup: TPopupMenu;
    TabsUpdate: TTimer;
    CurrentFunctionUpdate: TTimer;
    IdentifierListUpdate: TTimer;
    procedure CurrentFunctionUpdateTimer(Sender: TObject);
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
Uses uMainForm, mProject, mFunctions, mLanguages, CodeScan;

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
Var Ident: PIdentifier;
    Word : String;
begin
 With Project do
 Begin
  getCurrentEditor.SelectWord;
  Word := getCurrentEditor.SelText;

  if (isKeyword(Word)) Then // if keyword...
   Application.MessageBox(PChar(getLangValue(ls_its_keyword)), PChar(getLangValue(ls_msg_error)), MB_ICONERROR) Else

  if (isInternalType(Word)) Then // if internal type...
   Application.MessageBox(PChar(getLangValue(ls_its_internal_type)), PChar(getLangValue(ls_msg_info)), MB_ICONINFORMATION) Else

  Begin
   Ident := Project.FindIdentifier(getCurrentEditor.BlockBegin);

   if (Ident <> nil) Then
    JumpToDeclaration(Ident) Else
    Application.MessageBox(PChar(getLangValue(ls_declaration_not_found)), PChar(getLangValue(ls_msg_error)), MB_ICONERROR);
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
 if (Project <> nil) Then
 Begin
  if (Project.getCurrentCard <> nil) Then
  Begin
   Namespace := Project.getCurrentCard.GetNamespaceAtCaret;
   Func      := Project.getCurrentCard.GetFunctionAtCaret;

   if (Func = nil) Then
   Begin
    if (Namespace <> nil) Then
     Str := 'namespace '+Namespace.Name;
   End Else
   Begin
    if (Namespace = nil) or (Namespace.Name = 'self') Then
     Str := 'function '+Func.Name Else
     Str := 'function '+Namespace.Name+'::'+Func.Name;
   End;

   if (CurrentFunction.Text <> Str) Then
    CurrentFunction.Text := Str;
  End Else
   CurrentFunction.Text := '';
 End Else
  CurrentFunction.Text := '';
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
 if (Project = nil) Then // no project created
  Exit;

 if (Project.getCurrentCard = nil) Then // no card opened
  Exit;

 Project.UpdateCards;

 With Project.getCurrentEditor do
  MainForm.StatusBar.Panels[0].Text := IntToStr(CaretY)+': '+IntToStr(CaretX);

 With Project.getCurrentCard do
  MainForm.StatusBar.Panels[1].Text := getFileName;
end;

end.

