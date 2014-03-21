(*
 Copyright © by Patryk Wychowaniec, 2013-2014
 All rights reserved.
*)
Unit mConfiguration;

 Interface
 Uses FileUtil, XMLPropStorage, Dialogs, Graphics, Classes, Variants;

 { TConfigEnum }
 Type TConfigEnum =
      (
       ceCompilerExecutable, ceVMExecutable,
       ceRecentlyOpened, ceMaxRecentlyOpened, ceLoadRecentProject, ceRecentProject,
       ceAddBrackets, ceScrollPastEOL,
       ceEnableLogging, ceRewriteLog,
       ceLanguage, ceLayoutFile, ceStyleFile, ceLogFile,
       ceIntellisenseWidth, ceIntellisenseHeight
      );

 Const DefaultSettings: Array[TConfigEnum] of String =
 (
  'compiler\compiler.exe', 'compiler\vm.exe',
  '', '8', 'false', '',
  'false', 'false',
  'true', 'false',
  '', 'default.xml', 'default.xml', 'editor.log',
  '350', '300'
 );

 { TConfiguration }
 Type TConfiguration =
      Class
       Private
        FileName: String;
        XML     : TXMLConfigStorage;

       Public
        Constructor Create;
        Destructor Destroy; override;

        Procedure Reload;

        Function getString(const Name: TConfigEnum): String;
        Function getInteger(const Name: TConfigEnum): Integer;
        Function getFloat(const Name: TConfigEnum): Extended;
        Function getBoolean(const Name: TConfigEnum): Boolean;
        Function getRecentlyOpened: TStringList;

        Procedure setString(const Name: TConfigEnum; const Value: String);
        Procedure setInteger(const Name: TConfigEnum; const Value: Integer);
        Procedure setFloat(const Name: TConfigEnum; const Value: Extended);
        Procedure setBoolean(const Name: TConfigEnum; const Value: Boolean);
        Procedure setRecentlyOpened(const Value: TStringList);

        Procedure Delete(const Name: TConfigEnum);
       End;

 // configuration class instance
 Var Config: TConfiguration;

 Implementation
Uses mMessages, mFunctions, SysUtils, TypInfo;

(* getSettingName *)
Function getSettingName(const S: TConfigEnum): String;
Begin
 Result := GetEnumName(TypeInfo(TConfigEnum), Integer(S));
End;

// -------------------------------------------------------------------------- //
(* TConfiguration.Create *)
{
 Loads settings from specified file.
}
Constructor TConfiguration.Create;
Begin
 FileName := getApplicationDir+'config.xml';

 Reload;
End;

(* TConfiguration.Destroy *)
Destructor TConfiguration.Destroy;
Begin
 XML.Free;

 inherited Destroy;
End;

(* TConfiguration.Reload *)
{
 Reloads settings.
}
Procedure TConfiguration.Reload;
Begin
 XML.Free;

 Try
  XML := TXMLConfigStorage.Create(FileName, FileExists(FileName));
 Except
  On E: Exception Do
  Begin
   ErrorMessage('An exception occured during the configuration file parsing.'#13#10#13#10'The configuration file (config.xml) will be removed to prevent further events like this, and so the IDE settings will be restored to its defaults.'#13#10+'(of course you can also terminate the editor *now* and try to fix the problem by yourself, if you want - or just click "ok" and let it ''fix'' itself)'+#13#10#13#10'Exception message:'#13#10+E.Message, 'Error');

   XML.Free;
   DeleteFile(FileName);
   Reload;
  End;
 End;
End;

(* TConfiguration.getString *)
Function TConfiguration.getString(const Name: TConfigEnum): String;
Begin
 Result := XML.GetValue('config/'+getSettingName(Name)+'/value', DefaultSettings[Name]);

 Result := StringReplace(Result, '%appdir%', getApplicationDir, [rfReplaceAll]);
End;

(* TConfiguration.getInteger *)
Function TConfiguration.getInteger(const Name: TConfigEnum): Integer;
Begin
 Result := StrToIntDef(getString(Name), StrToInt(DefaultSettings[Name]));
End;

(* TConfiguration.getFloat *)
Function TConfiguration.getFloat(const Name: TConfigEnum): Extended;
Begin
 Result := StrToFloatDef(getString(Name), StrToFloat(DefaultSettings[Name]));
End;

(* TConfiguration.getBoolean *)
Function TConfiguration.getBoolean(const Name: TConfigEnum): Boolean;
Begin
 Case getString(Name) of
  'true', '1': Result := True;

  else
   Result := False;
 End;
End;

(* TConfiguration.getRecentlyOpened *)
Function TConfiguration.getRecentlyOpened: TStringList;
Var I: uint16;
Begin
 Result := TStringList.Create;

 Result.Delimiter       := ',';
 Result.StrictDelimiter := True;
 Result.DelimitedText   := getString(ceRecentlyOpened);

 // exclude not existing files from the list
 I := 0;

 While (I < Result.Count) Do
 Begin
  if (not FileExists(Result[I])) Then
   Result.Delete(I) Else
   Inc(I);
 End;
End;

(* TConfiguration.setString *)
Procedure TConfiguration.setString(const Name: TConfigEnum; const Value: String);
Begin
 XML.SetValue('config/'+getSettingName(Name)+'/value', Value);
 XML.WriteToDisk;
End;

(* TConfiguration.setInteger *)
Procedure TConfiguration.setInteger(const Name: TConfigEnum; const Value: Integer);
Begin
 setString(Name, IntToStr(Value));
End;

(* TConfiguration.setFloat *)
Procedure TConfiguration.setFloat(const Name: TConfigEnum; const Value: Extended);
Begin
 setString(Name, FloatToStr(Value));
End;

(* TConfiguration.setBoolean *)
Procedure TConfiguration.setBoolean(const Name: TConfigEnum; const Value: Boolean);
Begin
 if (Value) Then
  setString(Name, 'true') Else
  setString(Name, 'false');
End;

(* TConfiguration.setRecentlyOpened *)
Procedure TConfiguration.setRecentlyOpened(const Value: TStringList);
Var Item, Data: String;
Begin
 Data := '';

 For Item in Value Do
  Data += Item+',';

 if (Length(Data) > 0) and (Data[Length(Data)] = ',') Then
  System.Delete(Data, Length(Data), 1);

 setString(ceRecentlyOpened, Data);
End;

(* TConfiguration.Delete *)
Procedure TConfiguration.Delete(const Name: TConfigEnum);
Begin
 XML.DeleteValue('config/'+getSettingName(Name));
 XML.WriteToDisk;
End;

finalization
 Config.Free;
End.
