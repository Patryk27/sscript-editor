(*
 SScript Editor
 Copyright © by Patryk Wychowaniec, 2013

 -------------------------------------------------------------------------------
 SScript Compiler is free software; you can redistribute it and/or modify
 it under the terms of the GNU Lesser General Public License as published by
 the Free Software Foundation; either version 2.1 of the License, or
 (at your option) any later version.

 SScript Compiler is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 GNU Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public License
 along with SScript Compiler; if not, write to the Free Software
 Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA 
*)
program editor;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, SysUtils, Forms, lazcontrols,
  runtimetypeinfocontrols, anchordockpkg, AnchorDocking, uMainForm, uProjectSettings,
  uAboutForm, uEvSettingsForm, uSyntaxHighlighterChange, Controls,

  mLanguages, mSettings, uCompilerOutput, uFindForm,
  uIdentifierListForm, uCompileStatusForm, uCodeEditor, virtualtreeview_package,
  Dialogs, XMLPropStorage;

{$R *.res}

Var XML: TXMLConfigStorage;
begin
 RequireDerivedFormResource := True;
 Application.Initialize;

 Application.CreateForm(TMainForm, MainForm);
 Application.CreateForm(TProjectSettingsForm, ProjectSettingsForm);
 Application.CreateForm(TAboutForm, AboutForm);
 Application.CreateForm(TEvSettingsForm, EvSettingsForm);
 Application.CreateForm(TSyntaxHighlighterChange, SyntaxHighlighterChange);
 Application.CreateForm(TCompilerOutputForm, CompilerOutputForm);
 Application.CreateForm(TFindForm, FindForm);
 Application.CreateForm(TIdentifierListForm, IdentifierListForm);
 Application.CreateForm(TCompileStatusForm, CompileStatusForm);
 Application.CreateForm(TCodeEditor, CodeEditor);

 DockMaster.MakeDockSite(MainForm, [akTop, akLeft, akRight, akBottom], admrpChild);
 DockMaster.MakeDockable(CompileStatusForm);
 DockMaster.MakeDockable(IdentifierListForm);
 DockMaster.MakeDockable(CodeEditor);

 { load layout (if possible) }
 if (FileExists('layout.xml')) Then
 Begin
  XML := TXMLConfigStorage.Create('layout.xml', True);
  Try
   DockMaster.LoadLayoutFromConfig(XML, False);
  Finally
   XML.Free;
  End;
 End;

 { load language }
 LoadLanguageFile(ExtractFilePath(ParamStr(0))+'lang/'+getString(sLanguage));

 { run application }
 Application.Run;

 { save layout }
 XML := TXMLConfigStorage.Create('layout.xml', False);
 Try
  DockMaster.SaveLayoutToConfig(XML);
  XML.WriteToDisk;
 Finally
  XML.Free;
 End;
end.

