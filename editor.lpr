(*
 SScript Editor
 Copyright © by Patryk Wychowaniec, 2013-2014

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
{$MODE OBJFPC}{$H+}
{.$APPTYPE CONSOLE}
program editor;
uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}

  Interfaces, SysUtils, Forms, lazcontrols,
  runtimetypeinfocontrols, uMainForm, uProjectSettings,
  uAboutForm, uEvSettingsForm, Controls,

  mLanguages, mConfiguration, mFunctions, mLayouts, mStyles, mLogger,

  uCompilerOutput, uFindForm,
  uIdentifierListForm, uCompileStatusForm, uCodeEditor, virtualtreeview_package,
  Dialogs, uLayoutManagerForm;

{$R *.res}

Begin
 RequireDerivedFormResource := True;

 Application.Initialize;
 Application.Title := 'SScript Editor';

 DefaultFormatSettings.DecimalSeparator := '.';

 // set working directory (in case we're loading project by double-click (paramstr(1)) from a directory other than the editor's)
 SetCurrentDir(ExtractFilePath(ParamStr(0)));

 // load configuration
 Config := TConfiguration.Create;

 // begin log
 Log := TLogger.Create;

 Try
  // create forms
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TProjectSettingsForm, ProjectSettingsForm);
  Application.CreateForm(TAboutForm, AboutForm);
  Application.CreateForm(TEvSettingsForm, EvSettingsForm);
  Application.CreateForm(TCompilerOutputForm, CompilerOutputForm);
  Application.CreateForm(TFindForm, FindForm);
  Application.CreateForm(TIdentifierListForm, IdentifierListForm);
  Application.CreateForm(TCompileStatusForm, CompileStatusForm);
  Application.CreateForm(TCodeEditor, CodeEditor);
  Application.CreateForm(TLayoutManagerForm, LayoutManagerForm);

  // load layout
  LayoutManager := TLayoutManager.Create;
  LayoutManager.ReloadCurrentLayout;

  // load style
  StyleManager := TStyleManager.Create;
  StyleManager.ReloadCurrentStyle;

  // load language
  Language := TLanguage.Create(getLanguagesDir+Config.getString(ceLanguage));
  MainForm.OnLanguageLoaded;

  Log.Writeln('-- starting application main thread --');
  Application.Run;

  // save layout
  With LayoutManager.getCurrentLayout do
  Begin
   Update;
   SaveToFile(getLayoutsDir+Config.getString(ceLayoutFile));
  End;
 Except
  On E: Exception Do
  Begin
   Log.LogException(E);
   raise;
  End;
 End;

 Log.Writeln('-- end --');
End.
