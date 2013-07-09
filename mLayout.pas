(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
Unit mLayout;

 Interface
 Uses XMLPropStorage, Classes, SysUtils, Forms;

 Type TLayoutForm = Record
                     Name                    : String;
                     Top, Left, Width, Height: Integer;
                     Visible                 : Boolean;
                    End;

 Type TLayout = Class
                 Public
                  Name : String;
                  Forms: Array of TLayoutForm;

                  Constructor Create;
                  Constructor Create(Layout: TLayout);
                  Constructor Create(const FileName: String);

                  Procedure LoadFromFile(const FileName: String);
                  Procedure SaveToFile(const FileName: String);

                  Procedure Reset;
                  Procedure Update;
                  Procedure Apply;
                 End;

 Var CurrentLayout: TLayout;

 Procedure Reload;
 Function FindLayouts: TStringList;

 Implementation
Uses mSettings;

(* TLayout.Create *)
Constructor TLayout.Create;
Begin
End;

(* TLayout.Create *)
Constructor TLayout.Create(Layout: TLayout);
Var I: Integer;
Begin
 Name := Layout.Name;

 SetLength(Forms, Length(Layout.Forms));
 For I := Low(Forms) To High(Forms) Do
 Begin
  Forms[I].Name     := Layout.Forms[I].Name;
  Forms[I].Top      := Layout.Forms[I].Top;
  Forms[I].Left     := Layout.Forms[I].Left;
  Forms[I].Width    := Layout.Forms[I].Width;
  Forms[I].Height   := Layout.Forms[I].Height;
  Forms[I].Visible  := Layout.Forms[I].Visible;
 End;
End;

(* TLayout.Create *)
Constructor TLayout.Create(const FileName: String);
Begin
 LoadFromFile(FileName);
End;

(* TLayout.LoadFromFile *)
Procedure TLayout.LoadFromFile(const FileName: String);
Var XML: TXMLConfigStorage;

   // LoadForm
   Function LoadForm(Name: String): TLayoutForm;
   Begin
    Result.Name     := Name;
    Result.Top      := XML.GetValue(Name+'/top', 0);
    Result.Left     := XML.GetValue(Name+'/left', 0);
    Result.Width    := XML.GetValue(Name+'/width', 100);
    Result.Height   := XML.GetValue(Name+'/height', 100);
    Result.Visible  := XML.GetValue(Name+'/visible', True);
   End;

Begin
 XML := TXMLConfigStorage.Create(FileName, True);

 Try
  Name := XML.GetValue('name', Name);

  SetLength(Forms, 3);
  Forms[0] := LoadForm('CodeEditor');
  Forms[1] := LoadForm('IdentifierListForm');
  Forms[2] := LoadForm('CompileStatusForm');
 Finally
  XML.Free;
 End;
End;

(* TLayout.SaveToFile *)
Procedure TLayout.SaveToFile(const FileName: String);
Var XML : TXMLConfigStorage;
    Form: TLayoutForm;

   // SaveForm
   Procedure SaveForm(Form: TLayoutForm);
   Begin
    With Form do
    Begin
     XML.SetValue(Name+'/top', Top);
     XML.SetValue(Name+'/left', Left);
     XML.SetValue(Name+'/width', Width);
     XML.SetValue(Name+'/height', Height);
     XML.SetValue(Name+'/visible', Visible);
    End;
   End;

Begin
 if (not DirectoryExists(ExtractFileDir(FileName))) Then
  mkdir(ExtractFileDir(FileName));

 XML := TXMLConfigStorage.Create(FileName, False);

 Try
  XML.SetValue('name', Name);

  For Form in Forms Do
   SaveForm(Form);
 Finally
  XML.Free;
 End;
End;

(* TLayout.Reset *)
Procedure TLayout.Reset;
Begin
 SetLength(Forms, 3);

 { code editor form }
 With Forms[0] do
 Begin
  Name    := 'CodeEditor';
  Top     := 88;
  Left    := 4;
  Width   := Screen.Width-245;
  Height  := Screen.Height-250;
  Visible := True;
 End;

 { identifier list form }
 With Forms[1] do
 Begin
  Name    := 'IdentifierListForm';
  Top     := 88;
  Left    := Screen.Width-222;
  Width   := 200;
  Height  := Screen.Height-250;
  Visible := True;
 End;

 { compile status form }
 With Forms[2] do
 Begin
  Name    := 'CompileStatusForm';
  Top     := Screen.Height-140;
  Left    := 4;
  Width   := Screen.Width-24;
  Height  := 75;
  Visible := True;
 End;
End;

(* TLayout.Update *)
Procedure TLayout.Update;
Var I   : Integer;
    Form: TCustomForm;
Begin
 For I := Low(Forms) To High(Forms) Do
 Begin
  Form := Screen.FindForm(Forms[I].Name);
  if (Form = nil) Then
   raise Exception.CreateFmt('TLayout.Update() -> form does not exist: %s', [Forms[I].Name]);

  With Form do
  Begin
   Forms[I].Top     := Top;
   Forms[I].Left    := Left;
   Forms[I].Width   := ClientWidth;
   Forms[I].Height  := ClientHeight;
   Forms[I].Visible := Visible;
  End;
 End;
End;

(* TLayout.Apply *)
Procedure TLayout.Apply;
Var I   : Integer;
    Form: TCustomForm;
Begin
 For I := Low(Forms) To High(Forms) Do
 Begin
  Form := Screen.FindForm(Forms[I].Name);
  if (Form = nil) Then
   raise Exception.CreateFmt('TLayout.Apply() -> form does not exist: %s', [Forms[I].Name]);

  With Form do
  Begin
   Top          := Forms[I].Top;
   Left         := Forms[I].Left;
   ClientWidth  := Forms[I].Width;
   ClientHeight := Forms[I].Height;
   Visible      := Forms[I].Visible;
  End;
 End;
End;

// -------------------------------------------------------------------------- //
(* Reload *)
Procedure Reload;
Var LayoutFile: String;
Begin
 LayoutFile := getString(sLayoutFile);

 if (not FileExists(LayoutFile)) Then // if selected layout file doesn't exist, select the default one
 Begin
  CurrentLayout      := TLayout.Create;
  CurrentLayout.Name := 'Default';
  CurrentLayout.Reset;
 End Else
  CurrentLayout := TLayout.Create(LayoutFile);

 CurrentLayout.Apply;
End;

(* FindLayouts *)
{
 Returns list containing found layouts' files and names.
}
Function FindLayouts: TStringList;
Var M     : TSearchRec;
    Path  : String;
    Layout: TLayout;
Begin
 Path := getLayoutDir;

 FindFirst(Path+'*.xml', faAnyFile, M);

 Result := TStringList.Create;
 Repeat
  Layout := TLayout.Create(Path+M.Name);

  Try
   Result.Add(Layout.Name+'='+Path+M.Name);
  Finally
   Layout.Free;
  End;
 Until (FindNext(M) <> 0);

 FindClose(M);
End;
End.
