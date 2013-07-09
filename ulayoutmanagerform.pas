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
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    lblCurrentLayout: TLabel;
    LayoutList: TListBox;
    LayoutListPopup: TPopupMenu;
    opRenameSelected: TMenuItem;
    opRemoveSelected: TMenuItem;
    procedure btnApplySelectedClick(Sender: TObject);
    procedure btnSaveCurrentClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LayoutListClick(Sender: TObject);
    procedure LayoutListDblClick(Sender: TObject);
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
Uses mLayout, mLanguages, mSettings;
Var Layouts: TStringList;

{$R *.lfm}

{ GenerateLayoutFileName }
Function GenerateLayoutFileName(const LayoutName: String): String;
Var Ch: Char;
Begin
 Result := '';

 For Ch in LayoutName Do
  if (Ch in ['a'..'z', 'A'..'Z', '0'..'9', '_']) Then
   Result += Ch Else
   Result += '_';

 Result += '.xml';
End;

// -------------------------------------------------------------------------- //
(* TLayoutManagerForm.FormShow *)
procedure TLayoutManagerForm.FormShow(Sender: TObject);
begin
 Label2.Left := (Width div 2) - (Label2.Width div 2);
end;

(* TLayoutManagerForm.LayoutListClick *)
procedure TLayoutManagerForm.LayoutListClick(Sender: TObject);
begin
 if (LayoutList.ItemIndex > -1) Then
  With TLayout.Create(Layouts.Values[Layouts.Names[LayoutList.ItemIndex]]) do
  Begin
   Try
    Apply;
    LayoutManagerForm.SetFocus;
   Finally
    Free;
   End;
  End;
end;

(* TLayoutManagerForm.LayoutListDblClick *)
procedure TLayoutManagerForm.LayoutListDblClick(Sender: TObject);
begin
 btnApplySelected.Click;
end;

(* TLayoutManagerForm.opRemoveSelectedClick *)
procedure TLayoutManagerForm.opRemoveSelectedClick(Sender: TObject);
Var Selected: TLayout;
    FileName: String;
begin
 if (LayoutList.ItemIndex = -1) Then // nothing is selected
 Begin
  Application.MessageBox(PChar(getLangValue(ls_msg_nothing_is_selected)), PChar(getLangValue(ls_msg_error)), MB_IconError);
  Exit;
 End;

 FileName := Layouts.Values[Layouts.Names[LayoutList.ItemIndex]];
 Selected := TLayout.Create(FileName);

 Try
  Case MessageDlg(Caption, Format(getLangValue(ls_msg_layout_remove), [Selected.Name]), mtConfirmation, [mbYes, mbNo], 0) of
   mrYes:
   Begin
    DeleteFile(FileName);
    UpdateLayoutList;

    if (FileName = getString(sLayoutFile)) Then
    Begin
     DeleteSetting(sLayoutFile);
     mLayout.Reload;
    End;
   End;
  End;
 Finally
  Selected.Free;
 End;
end;

(* TLayoutManagerForm.opRenameSelectedClick *)
procedure TLayoutManagerForm.opRenameSelectedClick(Sender: TObject);
Var Selected         : TLayout;
    FileName, NewFile: String;
begin
 if (LayoutList.ItemIndex = -1) Then // nothing is selected
 Begin
  Application.MessageBox(PChar(getLangValue(ls_msg_nothing_is_selected)), PChar(getLangValue(ls_msg_error)), MB_IconError);
  Exit;
 End;

 FileName := Layouts.Values[Layouts.Names[LayoutList.ItemIndex]];
 Selected := TLayout.Create(FileName);

 Try
  if (InputQuery(Caption, getLangValue(ls_msg_layout_name), Selected.Name)) Then
  Begin
   DeleteFile(FileName);
   NewFile := getLayoutDir+GenerateLayoutFileName(Selected.Name);

   Selected.SaveToFile(NewFile);

   if (FileName = getString(sLayoutFile)) Then // if changed name of currently selected layout // @TODO: CompareFilenames?
   Begin
    setString(sLayoutFile, NewFile);
    mLayout.Reload;
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

 if (InputQuery(Caption, getLangValue(ls_msg_layout_name), LayoutName)) Then
 Begin
  FileName := getLayoutDir+GenerateLayoutFileName(LayoutName);

  if (FileExists(FileName)) or (Layouts.IndexOfName(Name) > -1) Then // if layout already exists
  Begin
   Application.MessageBox(PChar(getLangValue(ls_msg_layout_already_exists)), PChar(getLangValue(ls_msg_error)), MB_IconError);
   btnSaveCurrent.Click;
   Exit;
  End;

  Layout := TLayout.Create(CurrentLayout);
  Try
   Layout.Name := LayoutName;
   Layout.SaveToFile(FileName);
  Finally
   Layout.Free;
  End;

  UpdateLayoutList; // update layout list
 End;
end;

(* TLayoutManagerForm.btnApplySelectedClick *)
procedure TLayoutManagerForm.btnApplySelectedClick(Sender: TObject);
begin
 if (LayoutList.ItemIndex = -1) Then // nothing is selected
 Begin
  Application.MessageBox(PChar(getLangValue(ls_msg_nothing_is_selected)), PChar(getLangValue(ls_msg_error)), MB_IconError);
  Exit;
 End;

 setString(sLayoutFile, Layouts.Values[Layouts.Names[LayoutList.ItemIndex]]); // save
 mLayout.Reload; // reload layout
 Close; // close form
end;

(* TLayoutManagerForm.UpdateLayoutList *)
Procedure TLayoutManagerForm.UpdateLayoutList;
Var I: Integer;
Begin
 if (Layouts <> nil) Then // prevent memleaks
  Layouts.Free;
 Layouts := mLayout.FindLayouts;

 LayoutList.Clear;
 For I := 0 To Layouts.Count-1 Do
  LayoutList.Items.Add(Layouts.Names[I]);

 lblCurrentLayout.Left    := Label3.Left+Label3.Width+5;
 lblCurrentLayout.Caption := CurrentLayout.Name;
End;

(* TLayoutManagerForm.Run *)
Procedure TLayoutManagerForm.Run;
Begin
 CurrentLayout.Update;
 CurrentLayout.SaveToFile(getString(sLayoutFile));

 UpdateLayoutList;

 ShowModal;

 CurrentLayout.Apply;
End;
end.

