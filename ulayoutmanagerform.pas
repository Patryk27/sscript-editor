(*
 Copyright © by Patryk Wychowaniec, 2013-2014
 All rights reserved.
*)
unit uLayoutManagerForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Menus, LCLType;

type

  { TLayoutManagerForm }

  TLayoutManagerForm = class(TForm)
    Bevel1: TBevel;
    Bevel2: TBevel;
    btnApplySelected: TButton;
    btnSaveCurrent: TButton;
    btnCreateNew: TButton;
    lblSavedLayouts: TLabel;
    lblClickToPreview: TLabel;
    lblCurrentLayoutDesc: TLabel;
    lblCurrentLayout: TLabel;
    lbLayoutList: TListBox;
    LayoutListPopup: TPopupMenu;
    opRenameSelected: TMenuItem;
    opRemoveSelected: TMenuItem;
    procedure btnApplySelectedClick(Sender: TObject);
    Procedure btnCreateNewClick(Sender: TObject);
    procedure btnSaveCurrentClick(Sender: TObject);
    Procedure FormResize(Sender: TObject);
    procedure lbLayoutListClick(Sender: TObject);
    procedure lbLayoutListDblClick(Sender: TObject);
    procedure opRemoveSelectedClick(Sender: TObject);
    procedure opRenameSelectedClick(Sender: TObject);
  private
   Procedure UpdateLayoutList;

  public
   Procedure Run;
  end;

var
  LayoutManagerForm: TLayoutManagerForm;

implementation
Uses mLayouts, mLanguages, mConfiguration, mFunctions, mMessages;
Var LayoutList: TStringList; // the same as 'LayoutManager.getLayoutList'

{$R *.lfm}

(* TLayoutManagerForm.LayoutListClick *)
procedure TLayoutManagerForm.lbLayoutListClick(Sender: TObject);
begin
 if (lbLayoutList.ItemIndex > -1) Then
 Begin
  With TLayout.Create(LayoutList.Values[LayoutList.Names[lbLayoutList.ItemIndex]]) do
  Begin
   Try
    Apply;
    LayoutManagerForm.SetFocus;
   Finally
    Free;
   End;
  End;
 End;
end;

(* TLayoutManagerForm.LayoutListDblClick *)
procedure TLayoutManagerForm.lbLayoutListDblClick(Sender: TObject);
begin
 btnApplySelected.Click;
end;

(* TLayoutManagerForm.opRemoveSelectedClick *)
procedure TLayoutManagerForm.opRemoveSelectedClick(Sender: TObject);
Var Selected: TLayout;
    FileName: String;
begin
 // check if any layout is selected
 if (lbLayoutList.ItemIndex = -1) Then
 Begin
  ErrorMessage(ls_msg_nothing_is_selected);
  Exit;
 End;

 // fetch its file name
 FileName := LayoutList.Values[LayoutList.Names[lbLayoutList.ItemIndex]];
 Selected := TLayout.Create(FileName);

 Try
  // ask user to confirm
  Case MessageDlg(Caption, Format(Language.getText(ls_dlg_layout_remove), [Selected.getName]), mtConfirmation, [mbYes, mbNo], 0) of
   mrYes:
   Begin
    DeleteFile(FileName);
    UpdateLayoutList;

    if (FileName = Config.getString(ceLayoutFile)) Then
    Begin
     Config.Delete(ceLayoutFile);
     LayoutManager.ReloadCurrentLayout;
    End;
   End;
  End;
 Finally
  Selected.Free;
 End;
end;

(* TLayoutManagerForm.opRenameSelectedClick *)
procedure TLayoutManagerForm.opRenameSelectedClick(Sender: TObject);
Var FileName, NewFile: String;
    LayoutName       : String;
    Selected         : TLayout;
begin
 // check if any layout is selected
 if (lbLayoutList.ItemIndex = -1) Then
 Begin
  ErrorMessage(ls_msg_nothing_is_selected);
  Exit;
 End;

 // fetch its file name
 FileName := LayoutList.Values[LayoutList.Names[lbLayoutList.ItemIndex]];
 Selected := TLayout.Create(FileName);

 Try
  LayoutName := Selected.getName;

  // ask user to enter a new name
  if (InputQuery(Caption, Language.getText(ls_msg_layout_name), LayoutName)) Then
  Begin
   // change name
   Selected.ChangeName(LayoutName);

   // generate new file name
   NewFile := getLayoutsDir+GenerateLayoutFileName(Selected.getName);

   // if layout with that name already exists
   if (FileExists(NewFile)) or (LayoutList.IndexOfName(Selected.getName) > -1) Then
   Begin
    Case MessageDlg(Caption, Format(Language.getText(ls_dlg_replace_layout), [Selected.getName]), mtConfirmation, [mbYes, mbNo], 0) of
     mrNo: Exit;
    End;
   End;

   // re-create layout file
   DeleteFile(FileName);
   Selected.SaveToFile(NewFile);

   // if currently selected layout name was changed, update settings // @TODO: CompareFilenames?
   if (FileName = Config.getString(ceLayoutFile)) Then
   Begin
    Config.setString(ceLayoutFile, NewFile);
    LayoutManager.ReloadCurrentLayout;
   End;

   UpdateLayoutList;
  End;
 Finally
  Selected.Free;
 End;
end;

(* TLayoutManagerForm.btnSaveCurrentClick *)
procedure TLayoutManagerForm.btnSaveCurrentClick(Sender: TObject);
Var LayoutName, FileName: String;
    Layout              : TLayout;
begin
 LayoutName := '';

 if (InputQuery(Caption, Language.getText(ls_msg_layout_name), LayoutName)) Then
 Begin
  FileName := getLayoutsDir+GenerateLayoutFileName(LayoutName);

  // check if layout doesn't already exist
  if (FileExists(FileName)) or (LayoutList.IndexOfName(LayoutName) > -1) Then
  Begin
   Case MessageDlg(Caption, Format(Language.getText(ls_dlg_replace_layout), [LayoutName]), mtConfirmation, [mbYes, mbNo], 0) of
    mrNo: Exit;
   End;
  End;

  // create a new instance of current layout
  Layout := TLayout.Create(LayoutManager.getCurrentLayout);
  Try
   Layout.ChangeName(LayoutName);
   Layout.SaveToFile(FileName);
  Finally
   Layout.Free;
  End;

  // update layout list
  UpdateLayoutList;
 End;
end;

Procedure TLayoutManagerForm.FormResize(Sender: TObject);
Begin
 lblClickToPreview.Left := (Width div 2) - (lblClickToPreview.Width div 2);
end;

(* TLayoutManagerForm.btnApplySelectedClick *)
procedure TLayoutManagerForm.btnApplySelectedClick(Sender: TObject);
begin
 // check if any layout is selected
 if (lbLayoutList.ItemIndex = -1) Then
 Begin
  ErrorMessage(ls_msg_nothing_is_selected);
  Exit;
 End;

 // change setting
 Config.setString(ceLayoutFile, LayoutList.Values[LayoutList.Names[lbLayoutList.ItemIndex]]);

 // reload layout
 LayoutManager.ReloadCurrentLayout;

 // close form
 Close;
end;

(* TLayoutManagerForm.btnCreateNewClick *)
Procedure TLayoutManagerForm.btnCreateNewClick(Sender: TObject);
Var LayoutName: String = '';
    LayoutFile: String;
    Layout    : TLayout;
Begin
 // ask for layout name
 if (InputQuery(Caption, Language.getText(ls_msg_layout_name), LayoutName)) Then
 Begin
  // create an empty layout
  Layout := TLayout.Create;
  Layout.ChangeName(LayoutName);

  // generate layout file name
  LayoutFile := getLayoutsDir+GenerateLayoutFileName(LayoutName);

  // check if layout file doesn't already exist
  if (FileExists(LayoutFile)) or (LayoutList.IndexOfName(LayoutName) > -1) Then
  Begin
   // display error and give up
   ErrorMessage(ls_msg_layout_already_exists);
   Exit;
  End;

  Try
   Layout.SaveToFile(LayoutFile);
   UpdateLayoutList;
  Finally
   Layout.Free;
  End;
 End;
End;

(* TLayoutManagerForm.UpdateLayoutList *)
Procedure TLayoutManagerForm.UpdateLayoutList;
Var I: int16;
Begin
 // fetch layout list
 LayoutManager.UpdateLayoutList;
 LayoutList := LayoutManager.getLayoutList;

 // sync fetched layout list with the component on the form
 lbLayoutList.Clear;
 For I := 0 To LayoutList.Count-1 Do
  lbLayoutList.Items.Add(LayoutList.Names[I]);

 // adjust label position
 lblCurrentLayout.Left    := lblCurrentLayoutDesc.Left+lblCurrentLayoutDesc.Width+5;
 lblCurrentLayout.Caption := LayoutManager.getCurrentLayout.getName;
End;

(* TLayoutManagerForm.Run *)
Procedure TLayoutManagerForm.Run;
Var Current: TLayout;
Begin
 Current := LayoutManager.getCurrentLayout;

 // update current layout
 Current.Update;

 // save it, just preventively
 Current.SaveToFile(Config.getString(ceLayoutFile));

 // update the layout list
 UpdateLayoutList;

 // show form
 ShowModal;

 // re-aplly current layout
 Current.Apply;
End;
end.

