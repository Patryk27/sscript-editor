(*
 Copyright © by Patryk Wychowaniec, 2013-2014
 All rights reserved.
*)
unit uAboutForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls;

type

  { TAboutForm }

  TAboutForm = class(TForm)
    ImgLogo: TImage;
    mText: TMemo;
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  AboutForm: TAboutForm;

implementation
Uses uMainForm;

{$R *.lfm}

(* TAboutForm.FormCreate *)
procedure TAboutForm.FormCreate(Sender: TObject);

  { L }
  Procedure L(Text: String=''; const AddNewline: Boolean=True); inline;
  Begin
   if (AddNewline) Then
    Text += LineEnding;

   mText.Text := mText.Text+Text;
  End;

begin
 mText.Text := '';

 L('SScript Editor v.'+uMainForm.sVersion);
 L('by Patryk Wychowaniec (http://github.com/Piterolex, http://sscript.4programmers.net/)');
 L();
 L('');
 L('Used icon packs:');
 L(' http://www.famfamfam.com/lab/icons/silk/');
 L(' http://tango.freedesktop.org/Tango_Icon_Library');

 L; L; L; L; L; L;
 L('PS: GLaDOS is still alive! Be beware of cake!', False);
end;

end.

