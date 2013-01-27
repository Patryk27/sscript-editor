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

{ TAboutForm }

procedure TAboutForm.FormCreate(Sender: TObject);

Procedure L(const Text: String='');
Begin
 mText.Text := mText.Text+Text+#13#10;
End;

begin
 mText.Text := '';

 L('SScript Editor v.'+uMainForm.sVersion);
 L('~Patryk Wychowaniec (http://sscript.xorg.pl/)');
 L();
 L('');
 L('Użyte zestawy ikon:');
 L(' http://www.famfamfam.com/lab/icons/silk/');
 L(' http://tango.freedesktop.org/Tango_Icon_Library');
end;

end.

