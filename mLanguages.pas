(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
Unit mLanguages;

 Interface
 Type LString =
 (
  ls_title_card_close,

  ls_msg_card_close, ls_msg_env_restart, ls_msg_file_not_found, ls_msg_module_saving, ls_msg_compiler_or_vm_not_found,
  ls_msg_close_last_card, ls_msg_close_main_card, ls_msg_compiler_not_found, ls_msg_vm_not_found, ls_msg_unsaved_files,
  ls_msg_unsaved_project, ls_msg_project_open_failed, ls_msg_project_open_failed_ex, ls_msg_module_open_failed, ls_msg_create_new_project,
  ls_msg_version_conflict_older, ls_msg_version_conflict_newer,

  ls_msg_info, ls_msg_warn, ls_msg_error,

  ls_remove_ext, ls_add_ext,

  ls_file_saving, ls_file_opening, ls_module_saving, ls_module_opening, ls_project_saving, ls_project_opening,

  ls_compilation_started, ls_compilation_finished, ls_compilation_stopped, ls_output_not_found,

  ls_new_app, ls_new_lib,

  ls_filter_project, ls_filter_module, ls_filter_any_file, ls_filter_text_files,

  ls_goto_line_title, ls_goto_line,
  ls_find, ls_replace, ls_replace_msg,
  ls_find_title, ls_find_not_found,

  ls_declaration_not_found, ls_its_keyword, ls_its_internal_type,

  ls_namespaces, ls_types, ls_functions, ls_variables, ls_constants,

  ls_parser_eof, ls_invalid_int_value, ls_invalid_float_value, ls_string_exceeds_line, ls_expected_identifier, ls_expected_string,
  ls_expected_int, ls_expected, ls_unexpected,
  ls_unknown_namespace, ls_unknown_file,
  ls_codescan_failed
 );

 Procedure LoadLanguageFile(const FileName: String);
 Function getLangValue(const Name: LString): String;

 Implementation
Uses mSettings, Forms, Classes, TypInfo, IniFiles, SysUtils, Dialogs, ComCtrls, ExtCtrls;
Const Properties: Array[1..3] of string = ('Caption', 'Hint', 'Text');

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
      End;
     End;

     { TRadioGroup }
     if (Comp is TRadioGroup) Then
     Begin
      With (Comp as TRadioGroup) do
      Begin
       For P := 0 To Items.Count-1 Do
        Items[P] := Ini.ReadString(FormName+'_'+Comp.Name, IntToStr(P), Items[P]);
      End;
     End;

     { TCheckGroup }
     if (Comp is TCheckGroup) Then
     Begin
      With (Comp as TCheckGroup) do
      Begin
       For P := 0 To Items.Count-1 Do
        Items[P] := Ini.ReadString(FormName+'_'+Comp.Name, IntToStr(P), Items[P]);
      End;
     End;

     { other control's properties }
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

{ getLStringName }
Function getLStringName(const Name: LString): String;
Begin
 Result := GetEnumName(TypeInfo(LString), Integer(Name));

 Delete(Result, 1, 3); // remove `ls_`
End;

{ getLangValue }
Function getLangValue(const Name: LString): String;
Var Ini: TIniFile;
Begin
 Ini    := TIniFile.Create(ExtractFilePath(ParamStr(0))+'lang/'+getString(sLanguage));
 Result := Ini.ReadString('Strings', getLStringName(Name), '');
 Ini.Free;

 if (Result = '') Then // text not found
 Begin
  Case Name of
   ls_title_card_close: Result := 'Closing card';

   ls_msg_env_restart             : Result := 'You must restart the environment to see the changes.';
   ls_msg_card_close              : Result := 'You''re about to close an unsaved card.'#13#10'Save it?';
   ls_msg_file_not_found          : Result := 'Cannot find file: %s';
   ls_msg_module_saving           : Result := 'To save a project, each module has to be named and has to have a corresponding file on disk.'#13#10'Open the save dialog again?'#13#10'(if you choose `No`, you''ll stop saving the project)';
   ls_msg_compiler_or_vm_not_found: Result := 'The compiler or virtual machine file cannot be found.';
   ls_msg_close_last_card         : Result := 'You cannot close the last card!';
   ls_msg_close_main_card         : Result := 'You cannot close the main card!';
   ls_msg_compiler_not_found      : Result := 'Compiler file not found!';
   ls_msg_vm_not_found            : Result := 'Virtual machine file not found!';
   ls_msg_unsaved_files           : Result := 'There are unsaved files in your project.'#13#10'Save them?';
   ls_msg_unsaved_project         : Result := 'Your project isn''t saved; you may lose data.'#13#10'Save it?';
   ls_msg_project_open_failed     : Result := 'Couldn''t open project file';
   ls_msg_project_open_failed_ex  : Result := 'Couldn''t open project file: %s';
   ls_msg_module_open_failed      : Result := 'Couldn''t open module file';
   ls_msg_create_new_project      : Result := 'Create a new project (application)?';
   ls_msg_version_conflict_older  : Result := 'This project seems to be created from older version of this editor; you may need to check project settings';
   ls_msg_version_conflict_newer  : Result := 'This project seems to be created from newer version of this editor - it might not work correctly';

   ls_msg_info : Result := 'Information';
   ls_msg_warn : Result := 'Warning';
   ls_msg_error: Result := 'Error';

   ls_remove_ext: Result := 'Remove opening *.ssp by a double click';
   ls_add_ext   : Result := 'Open when double click *.ssp file';

   ls_file_saving    : Result := 'Saving file';
   ls_file_opening   : Result := 'Opening file';
   ls_project_saving : Result := 'Saving project';
   ls_project_opening: Result := 'Opening project';
   ls_module_saving  : Result := 'Saving module';
   ls_module_opening : Result := 'Opening module';

   ls_compilation_started : Result := '[ %s ] - Compilation started...';
   ls_compilation_finished: Result := '[ %s ] - Compilation successfully finished; project has been built (%s) :)';
   ls_compilation_stopped : Result := '[ %s ] - Compilation interrupted by error';
   ls_output_not_found    : Result := 'Output file not found (%s)! Check compiler''s output.';

   ls_new_app: Result := 'new application';
   ls_new_lib: Result := 'new library';

   ls_filter_project   : Result := 'SScript Editor Project (*.ssp)|*.ssp';
   ls_filter_module    : Result := 'SScript Code (*.ss)|*.ss';
   ls_filter_any_file  : Result := 'All files (*.*)|*.*';
   ls_filter_text_files: Result := 'Text files (*.txt)|*.txt';

   ls_goto_line_title: Result := 'Goto line';
   ls_goto_line      : Result := 'Goto line (%d-%d):';

   ls_find          : Result := 'Find';
   ls_replace       : Result := 'Replace';
   ls_replace_msg   : Result := 'Do you want to replace this occurrence of `%s` with `%s`?';
   ls_find_title    : Result := 'Find';
   ls_find_not_found: Result := 'Expression `%s` not found!';

   ls_declaration_not_found: Result := 'Identifier not found!';
   ls_its_keyword          : Result := 'It''s a keyword!';
   ls_its_internal_type    : Result := 'It''s an internal type, it doesn''t have its declaration.';

   ls_namespaces: Result := 'Namespaces';
   ls_types     : Result := 'Types';
   ls_functions : Result := 'Functions';
   ls_variables : Result := 'Variables';
   ls_constants : Result := 'Constants';

   ls_parser_eof         : Result := 'Unexpected end-of-file';
   ls_invalid_int_value  : Result := 'Invalid integer value';
   ls_invalid_float_value: Result := 'Invalid float value';
   ls_string_exceeds_line: Result := 'String exceeds line';
   ls_expected_identifier: Result := 'Expected identifier, but `%s` found';
   ls_expected_string    : Result := 'Expected string, but `%s` found';
   ls_expected_int       : Result := 'Expected int, but `%s` found';
   ls_expected           : Result := 'Expected `%s` but `%s` found';
   ls_unexpected         : Result := '`%s` was unexpected';

   ls_unknown_namespace: Result := 'Unknown namespace: `%s`';
   ls_unknown_file     : Result := 'Unknown file: `%s`';

   ls_codescan_failed: Result := '%s -> %d: %d - %s';

   else
    Exit('<unknown string>');
  End;
 End;

 Result := StringReplace(Result, '%newline%', #13#10, [rfReplaceAll]);
End;
End.
