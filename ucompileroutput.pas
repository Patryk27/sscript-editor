unit uCompilerOutput;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,StdCtrls;

type

  { TCompilerOutputForm }

  TCompilerOutputForm = class(TForm)
  Output:TMemo;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  CompilerOutputForm: TCompilerOutputForm;

implementation

{$R *.lfm}

end.

