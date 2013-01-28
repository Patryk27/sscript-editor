Unit mLanguages;

 Interface

 Procedure SaveLanguageFile(const FileName: String);
 Procedure LoadLanguageFile(const FileName: String);
 Function getLangValue(const Name: String): String;

 Implementation
Uses mSettings, Forms, Classes, TypInfo, IniFiles, SysUtils, Dialogs, ComCtrls;
Const Properties: Array[1..3] of string = ('Caption', 'Hint', 'Text');

{ SaveLanguageFile }
Procedure SaveLanguageFile(const FileName: String);
Var Form, I, P   : Integer;
    Comp         : TComponent;
    FormName, Tmp: String;
    Ini          : TIniFile;
Begin
 Ini := TIniFile.Create(FileName);

 // save each form
 For Form := 0 To Application.ComponentCount-1 Do
  With Application do
   With Components[Form] do
   Begin
    FormName := TForm(Application.Components[Form]).Name;
    Ini.WriteString(Name, 'Caption', TForm(Application.Components[Form]).Caption);

    // save each component
    For I := 0 To ComponentCount-1 Do
    Begin
     Comp := Components[I];

     { TTreeView }
     if (Comp is TTreeView) Then
     Begin
      With (Comp as TTreeView) do
      Begin
       For P := 0 To Items.Count-1 Do
        Ini.WriteString(FormName+'_'+Comp.Name, IntToStr(P), Items[P].Text);

       Continue; // proceed to the next control
      End;
     End;

     { standard control }
     For P := Low(Properties) To High(Properties) Do
      if (isPublishedProp(Comp, Properties[P])) Then
      Begin
       Tmp := GetPropValue(Comp, Properties[P]);

       if (Tmp <> '') Then
       Begin
        Tmp := StringReplace(Tmp, #13#10, '%newline%', [rfReplaceAll]);

        Ini.WriteString(FormName+'_'+Comp.Name, Properties[P], Tmp);
       End;
      End;
    End;
   End;

 Ini.Free;
End;

{ LoadLanguageFile }
Procedure LoadLanguageFile(const FileName: String);
Var Form, I, P   : Integer;
    Comp         : TComponent;
    FormName, Tmp: String;
    Ini          : TIniFile;
Begin
 Ini := TIniFile.Create(FileName);

 // load each form
 For Form := 0 To Application.ComponentCount-1 Do
  With Application do
   With Components[Form] do
   Begin
    FormName := TForm(Application.Components[Form]).Name;

    With TForm(Application.Components[Form]) do
     Caption := Ini.ReadString(Name, 'Caption', Caption);

    // load each component
    For I := 0 To ComponentCount-1 Do
    Begin
     Comp := Components[I];

     { TTreeView }
     if (Comp is TTreeView) Then
     Begin
      With (Comp as TTreeView) do
      Begin
       For P := 0 To Items.Count-1 Do
        Items[P].Text := Ini.ReadString(FormName+'_'+Comp.Name, IntToStr(P), Items[P].Text);

       Continue; // proceed to the next control
      End;
     End;

     { standard control }
     For P := Low(Properties) To High(Properties) Do
      if (isPublishedProp(Comp, Properties[P])) Then
      Begin
       Tmp := Ini.ReadString(FormName+'_'+Comp.Name, Properties[P], GetPropValue(Comp, Properties[P]));

       Tmp := StringReplace(Tmp, '%newline%', #13#10, [rfReplaceAll]);

       SetPropValue(Comp, Properties[P], Tmp);
      End;
    End;
   End;

 Ini.Free;
End;

{ getLangValue }
Function getLangValue(const Name: String): String;
Var Ini: TIniFile;
Begin
 Ini    := TIniFile.Create('lang\'+getString(sLanguage));
 Result := Ini.ReadString('Strings', Name, '');
 Ini.Free;

 if (Result = '') Then // text not found
 Begin
  Case Name of
   'title_card_close': Result := 'Closing card';

   'msg_ev_restart': Result := 'You must restart the environment to see the changes.';
   'msg_card_close': Result := 'You''re about to close an unsaved card.'#13#10'Save it?';
   'msg_file_not_found': Result := 'Cannot find file: %s';
   'msg_module_saving': Result := 'To save a project, each module has to be named and has to have a corresponding file on disk.'#13#10'Open the save dialog again?'#13#10'(if you choose `No`, you''ll stop saving the project)';
   'msg_compiler_or_vm_not_found': Result := 'The compiler or virtual machine file cannot be found.';
   'msg_close_last_card': Result := 'You cannot close the last card!';
   'msg_close_main_card': Result := 'You cannot close the main card!';
   'msg_compiler_not_found': Result := 'Compiler file not found!';
   'msg_vm_not_found': Result := 'Virtual machine file not found!';
   'msg_unsaved_files': Result := 'There are unsaved files in your project.'#13#10'Save them?';
   'msg_unsaved_project': Result := 'Your project isn''t saved; you may lose data.'#13#10'Save it?';
   'msg_project_open_failed': Result := 'Couldn''t open project file';
   'msg_project_open_failed_ex': Result := 'Couldn''t open project file: %s';
   'msg_module_open_failed': Result := 'Couldn''t open module file';
   'msg_create_new_project': Result := 'Create a new project (application)?';

   'msg_info': Result := 'Information';
   'msg_warn': Result := 'Warning';
   'msg_err': Result := 'Error';

   'remove_ext': Result := 'Remove opening *.ssp by a double click';
   'add_ext': Result := 'Open when double click *.ssp file';

   'file_saving': Result := 'Saving file';
   'file_opening': Result := 'Opening file';
   'project_saving': Result := 'Saving project';
   'project_opening': Result := 'Opening project';
   'module_saving': Result := 'Saving module';
   'module_opening': Result := 'Opening module';

   'compilation_started': Result := '[ %s ] - Compilation started...';
   'compilation_finished': Result := '[ %s ] - Compilation successfully finished; project built (%s) :)';
   'compilation_stopped': Result := '[ %s ] - Compilation interrupted by error';

   'new_app': Result := 'new application';
   'new_lib': Result := 'new library';

   'filter_project': Result := 'SScript Editor Project (*.ssp)|*.ssp';
   'filter_module': Result := 'SScript Code (*.ss)|*.ss';
   'filter_any_file': Result := 'All files (*.*)|*.*';
  End;
 End;
End;
End.
