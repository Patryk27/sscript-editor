(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
Unit mLayout;

 Interface
 Uses XMLPropStorage, SysUtils, Forms;
 Var LayoutFile: String;

 Procedure LoadLayout(const FileName: String);
 Procedure SaveLayout(const FileName: String);
 Procedure SetDefaultLayout;

 Implementation
Uses uMainForm, uCodeEditor, uIdentifierListForm, uCompileStatusForm;

(* LoadLayout *)
Procedure LoadLayout(const FileName: String);
Var XML: TXMLConfigStorage;

   // LoadForm
   Procedure LoadForm(Form: TForm);
   Begin
    Form.Top           := XML.GetValue(Form.Name+'/top', 0);
    Form.Left          := XML.GetValue(Form.Name+'/left', 0);
    Form.ClientWidth   := XML.GetValue(Form.Name+'/width', 100);
    Form.ClientHeight  := XML.GetValue(Form.Name+'/height', 100);
    Form.Visible       := XML.GetValue(Form.Name+'/visible', True);
   End;

Begin
 XML := TXMLConfigStorage.Create(FileName, True);

 Try
  LoadForm(CodeEditor);
  LoadForm(IdentifierListForm);
  LoadForm(CompileStatusForm);
 Finally
  XML.Free;
 End;
End;

(* SaveLayout *)
Procedure SaveLayout(const FileName: String);
Var XML: TXMLConfigStorage;

   // SaveForm
   Procedure SaveForm(Form: TForm);
   Begin
    XML.SetValue(Form.Name+'/top', Form.Top);
    XML.SetValue(Form.Name+'/left', Form.Left);
    XML.SetValue(Form.Name+'/width', Form.ClientWidth);
    XML.SetValue(Form.Name+'/height', Form.ClientHeight);
    XML.SetValue(Form.Name+'/visible', Form.Visible);
   End;

Begin
 XML := TXMLConfigStorage.Create(FileName, False);

 Try
  SaveForm(CodeEditor);
  SaveForm(IdentifierListForm);
  SaveForm(CompileStatusForm);
 Finally
  XML.Free;
 End;
End;

(* SetDefaultLayout *)
Procedure SetDefaultLayout;
Begin
 { code editor form }
 With CodeEditor do
 Begin
  Top    := 88;
  Left   := 4;
  Width  := Screen.Width-245;
  Height := Screen.Height-250;
 End;

 { identifier list form }
 With IdentifierListForm do
 Begin
  Top    := 88;
  Left   := Screen.Width-222;
  Width  := 200;
  Height := Screen.Height-250;
 End;

 { compile status form }
 With CompileStatusForm do
 Begin
  Top    := Screen.Height-140;
  Left   := 4;
  Width  := Screen.Width-24;
  Height := 75;
 End;

 // ----- //
 CodeEditor.Show;
 IdentifierListForm.Show;
 CompileStatusForm.Show;

 if (MainForm.CanFocus) Then
  MainForm.SetFocus;
End;

initialization
 LayoutFile := ExtractFilePath(ParamStr(0))+'layout.xml';
End.
