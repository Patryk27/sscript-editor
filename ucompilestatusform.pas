(*
 Copyright © by Patryk Wychowaniec, 2013-2014
 All rights reserved.
*)
unit uCompileStatusForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  Menus, ClipBrd;

type

  { TCompileStatusForm }

  TCompileStatusForm = class(TForm)
    CompileStatus: TTreeView;
    CompileStatusPopup: TPopupMenu;
    MessagesImageList: TImageList;
    opSaveMessages: TMenuItem;
    opSaveMessagesToClipboard: TMenuItem;
    opSaveSelectedMessage: TMenuItem;
    procedure CompileStatusClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure opSaveMessagesClick(Sender: TObject);
    procedure opSaveMessagesToClipboardClick(Sender: TObject);
    procedure opSaveSelectedMessageClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  CompileStatusForm: TCompileStatusForm;

implementation
Uses mProject, mLanguages;

{$R *.lfm}

{ TCompileStatusForm }

(* TCompileStatusForm.FormCreate *)
procedure TCompileStatusForm.FormCreate(Sender: TObject);
begin
end;

(* TCompileStatusForm.CompileStatusClick *)
procedure TCompileStatusForm.CompileStatusClick(Sender: TObject);
Var Data: LongWord;
begin
 if (CompileStatus.Selected = nil) Then // nothing is selected
  Exit;

 Data := LongWord(CompileStatus.Selected.Data);

 if (Data = $CAFEBABE) Then // special case, see `mProject.pas :: TCard.Parse;`
 Begin
 End Else

 if (Data > 0) Then // when `Data == 0`, it's just an editor's message, not a compiler error, warning or hint, so there's no need to 'raise' it
  Project.RaiseMessage(Data-1);
end;

(* TCompileStatusForm.opSaveMessagesClick *)
procedure TCompileStatusForm.opSaveMessagesClick(Sender:TObject);
begin
 // run save dialog
 With TSaveDialog.Create(self) do
  Try
   Title  := getLangValue(ls_file_saving);
   Filter := getLangValue(ls_filter_text_files);

   if (Execute) Then
    CompileStatus.SaveToFile(FileName);
  Finally
   Free;
  End;
end;

(* TCompileStatusForm.opSaveMessagesToClipboardClick *)
procedure TCompileStatusForm.opSaveMessagesToClipboardClick(Sender:TObject);
Var Stream: TStringStream;
begin
 // copy to the clipboard
 Stream := TStringStream.Create('');
 CompileStatus.SaveToStream(Stream);
 Clipboard.SetTextBuf(PChar(Stream.DataString));
 Stream.Free;
end;

(* TCompileStatusForm.opSaveSelectedMessageClick *)
procedure TCompileStatusForm.opSaveSelectedMessageClick(Sender:TObject);
begin
 if (CompileStatus.Selected = nil) Then // anything selected?
  Exit;

 Clipboard.SetTextBuf(PChar(CompileStatus.Selected.Text)); // copy to the clipboard
end;

end.

