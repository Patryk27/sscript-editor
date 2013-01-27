{$H+}
{$MODE DELPHI}

{$DEFINE NOT_PRODUCTION}

Unit mSettings;

 Interface
 Uses Dialogs, Graphics, Classes;

 { TSetting }
 Type TSetting = (sSplitter1, sCompilerFile, sVMFile, sIdentFormat, sKeywordFormat,
                  sNumberFormat, sStringFormat, sCommentFormat, sMacroFormat, sPrimaryTypesFormat,
                  sOtherFormat, sEditorBackground, sEditorForeground, sEditorFont,
                  sRecentlyOpened);

 { TSyntaxFormat }
 Type TSyntaxFormat = Packed Record
                       Bold, Italic, Underline, HasBGColor: Boolean;
                       FGColor, BGColor                   : LongWord;
                      End;

 { TFont }
 Type TFont = Record
               Name                   : String;
               Size                   : Integer;
               Bold, Italic, Underline: Boolean;
              End;

 Const FileName = 'settings.ini';
       DefaultValues: Array[TSetting] of String =
 (
  '4.8',
  {$IFDEF NOT_PRODUCTION}
  '..\compiler\compiler.exe',
  '..\vm\vm.exe',
  {$ELSE}
  'compiler\compiler.exe',
  'compiler\vm.exe',
  {$ENDIF}
  'false,false,false,false,0,0',
  'true,false,false,false,8135705,0',
  'true,false,false,false,15728885,0',
  'false,false,false,false,16711680,0',
  'false,false,false,false,7895160,0',
  'false,false,false,false,46080,0',
  'false,false,false,false,9836825,0',
  'false,false,false,false,240,0',
  '16777215',
  '0',
  'Courier New,10,false,false,false',
  ''
 );

 Procedure ReloadConfig;
 Procedure FreeConfig;

 Function getName(S: TSetting): String;
 Function CreateFont(F: Graphics.TFont): TFont;
 Function FetchFont(F: TFont): Graphics.TFont;

 Function getString(S: TSetting): String;
 Function getInteger(S: TSetting): Integer;
 Function getFloat(S: TSetting): Extended;
 Function getFormat(S: TSetting): TSyntaxFormat;
 Function getFont(S: TSetting): TFont;
 Function getColor(S: TSetting): TColor;
 Function getRecentlyOpened: TStringList;

 Procedure setString(S: TSetting; Value: String);
 Procedure setInteger(S: TSetting; Value: Integer);
 Procedure setFloat(S: TSetting; Value: Extended);
 Procedure setFormat(S: TSetting; Value: TSyntaxFormat);
 Procedure setFont(S: TSetting; Value: TFont);
 Procedure setColor(S: TSetting; Value: TColor);
 Procedure setRecentlyOpened(S: TStringList);

 Implementation
Uses IniFiles, SysUtils, TypInfo;
Var Ini: TIniFile = nil;

// s2b
Function s2b(const S: String): Boolean;
Begin
 Result := (S = 'true');
End;

// s2lw
Function s2lw(const S: String): LongWord;
Begin
 Result := StrToInt(S);
End;

// b2s
Function b2s(const B: Boolean): String;
Const BoolTable: Array[Boolean] of String = ('false', 'true');
Begin
 Result := BoolTable[B];
End;

// lw2s
Function lw2s(const LW: LongWord): String;
Begin
 Result := IntToStr(LW);
End;

{ ReloadConfig }
Procedure ReloadConfig;
Begin
 FreeConfig;
 Ini := TIniFile.Create(ExtractFilePath(ParamStr(0))+FileName);
End;

{ FreeConfig }
Procedure FreeConfig;
Begin
 if (Ini <> nil) Then
  Ini.Free;
 Ini := nil;
End;

{ getName }
Function getName(S: TSetting): String;
Begin
 Result := GetEnumName(TypeInfo(TSetting), Integer(S));
End;

{ CreateFont }
Function CreateFont(F: Graphics.TFont): TFont;
Begin
 Result.Name      := F.Name;
 Result.Size      := F.Size;
 Result.Bold      := F.Bold;
 Result.Italic    := F.Italic;
 Result.Underline := F.Underline;
End;

{ FetchFont }
Function FetchFont(F: TFont): Graphics.TFont;
Begin
 Result := Graphics.TFont.Create;

 Result.Name      := F.Name;
 Result.Size      := F.Size;
 Result.Bold      := F.Bold;
 Result.Italic    := F.Italic;
 Result.Underline := F.Underline;
End;

{ getString }
Function getString(S: TSetting): String;
Begin
 Result := Ini.ReadString('settings', getName(S), DefaultValues[S]);
End;

{ getInteger }
Function getInteger(S: TSetting): Integer;
Begin
 Result := Ini.ReadInteger('settings', getName(S), StrToInt(DefaultValues[S]));
End;

{ getFloat }
Function getFloat(S: TSetting): Extended;
Begin
 Result := Ini.ReadFloat('settings', getName(S), StrToFloat(DefaultValues[S]));
End;

{ getFormat }
Function getFormat(S: TSetting): TSyntaxFormat;
Var Str   : String;
    Values: TStringList;
Begin
 Str := Ini.ReadString('settings', getName(S), DefaultValues[S]);

 Values := TStringList.Create;
 ExtractStrings([','], [], PChar(Str), Values);

 Result.Bold       := s2b(Values[0]);
 Result.Italic     := s2b(Values[1]);
 Result.Underline  := s2b(Values[2]);
 Result.HasBGColor := s2b(Values[3]);
 Result.FGColor    := s2lw(Values[4]);
 Result.BGColor    := s2lw(Values[5]);

 Values.Free;
End;

{ getFont }
Function getFont(S: TSetting): TFont;
Var Str   : String;
    Values: TStringList;
Begin
 Str := Ini.ReadString('settings', getName(S), DefaultValues[S]);

 Values := TStringList.Create;
 ExtractStrings([','], [], PChar(Str), Values);

 Result.Name      := Values[0];
 Result.Size      := s2lw(Values[1]);
 Result.Bold      := s2b(Values[2]);
 Result.Italic    := s2b(Values[3]);
 Result.Underline := s2b(Values[4]);
End;

{ getColor }
Function getColor(S: TSetting): TColor;
Begin
 Result := Ini.ReadInteger('settings', getName(S), StrToInt(DefaultValues[S]));
End;

{ getRecentlyOpened }
Function getRecentlyOpened: TStringList;
Begin
 Result := TStringList.Create;
 ExtractStrings([','], [], PChar(getString(sRecentlyOpened)), Result);
End;

{ setString }
Procedure setString(S: TSetting; Value: String);
Begin
 Ini.WriteString('settings', getName(S), Value);
End;

{ setInteger }
Procedure setInteger(S: TSetting; Value: Integer);
Begin
 Ini.WriteInteger('settings', getName(S), Value);
End;

{ setFloat }
Procedure setFloat(S: TSetting; Value: Extended);
Begin
 Ini.WriteFloat('settings', getName(S), Value);
End;

{ setFormat }
Procedure setFormat(S: TSetting; Value: TSyntaxFormat);
Var Str: String;
Begin
 Str := b2s(Value.Bold) +','+ b2s(Value.Italic) +','+ b2s(Value.Underline) +','+ b2s(Value.HasBGColor) +','+
        lw2s(Value.FGColor) +','+ lw2s(Value.BGColor);

 Ini.WriteString('settings', getName(S), Str);
End;

{ setFont }
Procedure setFont(S: TSetting; Value: TFont);
Var Str: String;
Begin
 Str := Value.Name +','+ lw2s(Value.Size) +','+ b2s(Value.Bold) +','+ b2s(Value.Italic) +','+ b2s(Value.Underline);

 Ini.WriteString('settings', getName(S), Str);
End;

{ setColor }
Procedure setColor(S: TSetting; Value: TColor);
Begin
 Ini.WriteInteger('settings', getName(S), Value);
End;

{ setRecentlyOpened }
Procedure setRecentlyOpened(S: TStringList);
Var Save, Str: String;
Begin
 Save := '';

 For Str in S Do
  Save += Str+',';

 if (Copy(Save, Length(Save), 1) = ',') Then
  Delete(Save, Length(Save), 1);

 setString(sRecentlyOpened, Save);
End;

initialization
 ReloadConfig;
End.
