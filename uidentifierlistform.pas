(*
 Copyright © by Patryk Wychowaniec, 2013-2014
 All rights reserved.
*)
unit uIdentifierListForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, VirtualTrees, SynEdit, CodeScan;

  Type PNodeData = ^TNodeData;
       TNodeData = Record
                    Typ    : (ndSymbol, ndText);
                    Symbol : TSymbol;
                    Caption: String;
                    Removed: Boolean;
                   End;

type

  { TIdentifierListForm }

  TIdentifierListForm = class(TForm)
    IdentifierListUpdate: TTimer;
    IdentifierList: TVirtualStringTree;
    procedure IdentifierListChange(Sender: TBaseVirtualTree; Node: PVirtualNode
      );
    procedure IdentifierListDblClick(Sender: TObject);
    procedure IdentifierListFocusChanged(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex);
    procedure IdentifierListFreeNode(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure IdentifierListGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure IdentifierListGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: String);
    procedure IdentifierListUpdateTimer(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  IdentifierListForm: TIdentifierListForm;

implementation
Uses mProject;

{$R *.lfm}

{ TIdentifierListForm }

(* TIdentifierListForm.IdentifierListDblClick *)
procedure TIdentifierListForm.IdentifierListDblClick(Sender: TObject);
Var Card  : TCard;
    Editor: TSynEdit;
    Data  : TNodeData;
begin
 With IdentifierListForm do
 Begin
  if (Project = nil) or (IdentifierList.FocusedNode = nil) Then
   Exit;

  Data := PNodeData(IdentifierList.GetNodeData(IdentifierList.FocusedNode))^;

  if (Data.Symbol = nil) Then
   Exit;

  Card := Project.getCurrentCard;
  if (Card = nil) Then
   Exit;
  Card.SetFocus;

  Editor := Project.getCurrentEditor;
  if (Editor = nil) Then
   Exit;

  Editor.CaretX := Data.Symbol.getToken.Char;
  Editor.CaretY := Data.Symbol.getToken.Line;
  Editor.SelEnd := Editor.SelStart+Length(Data.Symbol.getToken.Value);
 End;
end;

(* TIdentifierListForm.IdentifierListFocusChanged *)
procedure TIdentifierListForm.IdentifierListFocusChanged(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
begin
 IdentifierList.Refresh;
end;

(* TIdentifierListForm.IdentifierListFreeNode *)
procedure TIdentifierListForm.IdentifierListFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
// Dispose(PNodeData(IdentifierList.GetNodeData(Node)));
end;

(* TIdentifierListForm.IdentifierListGetNodeDataSize *)
procedure TIdentifierListForm.IdentifierListGetNodeDataSize(
  Sender: TBaseVirtualTree; var NodeDataSize: Integer);
begin
 NodeDataSize := sizeof(TNodeData);
end;

(* TIdentifierListForm.IdentifierListGetText *)
procedure TIdentifierListForm.IdentifierListGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
begin
 CellText := PNodeData(IdentifierList.GetNodeData(Node))^.Caption;
end;

(* TIdentifierListForm.IdentifierListChange *)
procedure TIdentifierListForm.IdentifierListChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
 IdentifierList.Refresh;
end;

(* TIdentifierListForm.IdentifierListUpdateTimer *)
procedure TIdentifierListForm.IdentifierListUpdateTimer(Sender: TObject);
begin
 if (Project <> nil) and (Project.getCurrentCard <> nil) Then
 Begin
  if (Project.getCurrentCard.getShouldBeReparsed) Then
   Project.UpdateIdentifierList;
 End Else
  IdentifierListForm.IdentifierList.Clear;
end;

end.

