(*
 Copyright © by Patryk Wychowaniec, 2013-2014
 All rights reserved.
*)
{$MODESWITCH ADVANCEDRECORDS}
Unit mStyles;

 Interface
 Uses SysUtils, Graphics, Classes, XMLPropStorage, SynEditHighlighter;

 { EStyleException }
 Type EStyleException = Class(Exception);

 { TSyntaxFormatEnum }
 Type TSyntaxFormatEnum =
      (sfeIdentifier, sfeKeyword, sfeString, sfeNumber, sfeMacro, sfeComment, sfePrimaryType, sfeOther);

 { TFontEnum }
 Type TFontEnum =
      (feEditorFont);

 { TColorEnum }
 Type TColorEnum =
      (ceEditorBackground);

 { TSyntaxFormat }
 Type TSyntaxFormat =
      Record
       FGColor, BGColor       : uint32; // foreground and background color
       HasFGColor, HasBGColor : Boolean;
       Bold, Italic, Underline: Boolean;

       Class Function Create(const fFGColor, fBGColor: uint32; const fHasFGColor, fHasBGColor, fBold, fItalic, fUnderline: Boolean): TSyntaxFormat; static;
       Function Convert: TSynHighlighterAttributes;
      End;

 { TFont }
 Type TFont =
      Record
       Name                   : String;
       Size                   : uint16;
       Color                  : uint32;
       Bold, Italic, Underline: Boolean;

       Class Function Create(const fName: String; const fSize: uint16; const fColor: uint32; const fBold, fItalic, fUnderline: Boolean): TFont; static;
       Class Function Create(const Font: Graphics.TFont): TFont; static;
       Function Convert: Graphics.TFont;
      End;

 { TColor }
 Type TColor =
      Record
       Color: uint32;

       Class Function Create(const fColor: uint32): TColor; static;
      End;

 { TSyntaxFormatArray }
 Type TSyntaxFormatArray = Array[TSyntaxFormatEnum] of TSyntaxFormat;

 { TFontArray }
 Type TFontArray = Array[TFontEnum] of TFont;

 { TColorArray }
 Type TColorArray = Array[TColorEnum] of TColor;

 { TStyle }
 Type TStyle =
      Class
       Private
        FileName: String;

        Name: String;

        SyntaxFormatData: TSyntaxFormatArray;
        FontData        : TFontArray;
        ColorData       : TColorArray;

       Public
        Constructor Create;
        Constructor Create(const Style: TStyle);
        Constructor Create(const fFileName: String);
        Destructor Destroy; override;

        Procedure LoadFromFile(const fFileName: String);
        Procedure SaveToFile(const fFileName: String='');

        Procedure ChangeName(const Value: String);
        Procedure Reset;

        Procedure setSyntaxFormat(const Index: TSyntaxFormatEnum; const Value: TSyntaxFormat);
        Procedure setFont(const Index: TFontEnum; const Value: TFont);
        Procedure setColor(const Index: TColorEnum; const Value: TColor);

        Function getSyntaxFormat(const Index: TSyntaxFormatEnum): TSyntaxFormat;
        Function getFont(const Index: TFontEnum): TFont;
        Function getColor(const Index: TColorEnum): TColor;

       Public
        Property getName: String read Name;
        Property getFileName: String read FileName;
       End;

 { TStyleManager }
 Type TStyleManager =
      Class
       Private
        StyleList   : TStringList;
        CurrentStyle: TStyle;

       Public
        Constructor Create;
        Destructor Destroy; override;

        Procedure ReloadCurrentStyle;
        Procedure UpdateStyleList;

       Public
        Property getStyleList: TStringList read StyleList;
        Property getCurrentStyle: TStyle read CurrentStyle;
       End;

 // style manager instance
 Var StyleManager: TStyleManager;

 Function GenerateStyleFileName(const StyleName: String): String;

 Implementation
Uses mConfiguration, mFunctions, mLogger, LCLIntf, TypInfo;

(* GenerateStyleFileName *)
Function GenerateStyleFileName(const StyleName: String): String;
Var Ch: Char;
Begin
 Result := 'style_';

 For Ch in StyleName Do
 Begin
  if (Ch in ['a'..'z', 'A'..'Z', '0'..'9', '_']) Then
   Result += Ch Else
   Result += '_';
 End;

 Result += '.xml';
End;

(* TSyntaxFormat.Create *)
Class Function TSyntaxFormat.Create(const fFGColor, fBGColor: uint32; const fHasFGColor, fHasBGColor, fBold, fItalic, fUnderline: Boolean): TSyntaxFormat;
Begin
 Result.FGColor    := fFGColor;
 Result.BGColor    := fBGColor;
 Result.Bold       := fBold;
 Result.Italic     := fItalic;
 Result.Underline  := fUnderline;
 Result.HasFGColor := fHasFGColor;
 Result.HasBGColor := fHasBGColor;
End;

(* TSyntaxFormat.Convert *)
{
 Converts TSyntaxFormat record to TSynHighlighterAttributes.
}
Function TSyntaxFormat.Convert: TSynHighlighterAttributes;
Var FontStyle: TFontStyles = [];

    Name: String = '';
    I   : uint8;
Begin
 For I := 0 To 4 Do
  Name += chr(ord('a')+Random(21));

 Result := TSynHighlighterAttributes.Create(Name, Name);

 if (Bold) Then
  Include(FontStyle, fsBold);

 if (Italic) Then
  Include(FontStyle, fsItalic);

 if (Underline) Then
  Include(FontStyle, fsUnderline);

 Result.Style := FontStyle;

 if (HasFGColor) Then
  Result.Foreground := FGColor;

 if (HasBGColor) Then
  Result.Background := BGColor;
End;

// -------------------------------------------------------------------------- //
(* TFont.Create *)
Class Function TFont.Create(const fName: String; const fSize: uint16; const fColor: uint32; const fBold, fItalic, fUnderline: Boolean): TFont;
Begin
 Result.Name      := fName;
 Result.Size      := fSize;
 Result.Color     := fColor;
 Result.Bold      := fBold;
 Result.Italic    := fItalic;
 Result.Underline := fUnderline;
End;

(* TFont.Create *)
Class Function TFont.Create(const Font: Graphics.TFont): TFont;
Begin
 Result.Name      := Font.Name;
 Result.Size      := Font.Size;
 Result.Color     := Font.Color;
 Result.Bold      := Font.Bold;
 Result.Italic    := Font.Italic;
 Result.Underline := Font.Underline;
End;

(* TFont.Convert *)
{
 Converts TFont record to a Graphics.TFont
}
Function TFont.Convert: Graphics.TFont;
Begin
 Result := Graphics.TFont.Create;

 Result.Name      := Name;
 Result.Size      := Size;
 Result.Color     := Color;
 Result.Bold      := Bold;
 Result.Italic    := Italic;
 Result.Underline := Underline;
End;

// -------------------------------------------------------------------------- //
(* TColor.Create *)
Class Function TColor.Create(const fColor: uint32): TColor;
Begin
 Result.Color := fColor;
End;

// -------------------------------------------------------------------------- //
(* TStyle.Create *)
Constructor TStyle.Create;
Begin
 Reset;
End;

(* TStyle.Create *)
Constructor TStyle.Create(const Style: TStyle);
Var SFE: TSyntaxFormatEnum;
    FE : TFontEnum;
    CE : TColorEnum;
Begin
 For SFE in TSyntaxFormatEnum Do
  SyntaxFormatData[SFE] := Style.SyntaxFormatData[SFE];

 For FE in TFontEnum Do
  FontData[FE] := Style.FontData[FE];

 For CE in TColorEnum Do
  ColorData[CE] := Style.ColorData[CE];
End;

(* TStyle.Create *)
Constructor TStyle.Create(const fFileName: String);
Begin
 Reset;
 LoadFromFile(fFileName);
End;

(* TStyle.Destroy *)
Destructor TStyle.Destroy;
Begin
 inherited Destroy;
End;

(* TStyle.LoadFromFile *)
{
 Loads style from specified file.
}
Procedure TStyle.LoadFromFile(const fFileName: String);
Var XML: TXMLConfigStorage;

  { LoadSyntaxFormat }
  Function LoadSyntaxFormat(const Index: TSyntaxFormatEnum): TSyntaxFormat;
  Var Default: TSyntaxFormat;
      Path   : String;
  Begin
   Default := SyntaxFormatData[Index];
   Path    := 'syntaxformat/'+GetEnumName(TypeInfo(TSyntaxFormatEnum), ord(Index))+'/';

   Result.FGColor    := XML.GetValue(Path+'FGColor', Default.FGColor);
   Result.BGColor    := XML.GetValue(Path+'BGColor', Default.BGColor);
   Result.Bold       := XML.GetValue(Path+'Bold', Default.Bold);
   Result.Italic     := XML.GetValue(Path+'Italic', Default.Italic);
   Result.Underline  := XML.GetValue(Path+'Underline', Default.Underline);
   Result.HasFGColor := XML.GetValue(Path+'HasFGColor', Default.HasFGColor);
   Result.HasBGColor := XML.GetValue(Path+'HasBGColor', Default.HasBGColor);
  End;

  { LoadFont }
  Function LoadFont(const Index: TFontEnum): TFont;
  Var Default: TFont;
      Path   : String;
  Begin
   Default := FontData[Index];
   Path    := 'font/'+GetEnumName(TypeInfo(TFontEnum), ord(Index))+'/';

   Result.Name      := XML.GetValue(Path+'Name', Default.Name);
   Result.Size      := XML.GetValue(Path+'Size', Default.Size);
   Result.Color     := XML.GetValue(Path+'Color', Default.Color);
   Result.Bold      := XML.GetValue(Path+'Bold', Default.Bold);
   Result.Italic    := XML.GetValue(Path+'Italic', Default.Italic);
   Result.Underline := XML.GetValue(Path+'Underline', Default.Underline);
  End;

  { LoadColor }
  Function LoadColor(const Index: TColorEnum): TColor;
  Var Default: TColor;
      Path   : String;
  Begin
   Default := ColorData[Index];
   Path    := 'color/'+GetEnumName(TypeInfo(TColorEnum), ord(Index))+'/';

   Result.Color := XML.GetValue(Path+'Color', Default.Color);
  End;

Var SFE: TSyntaxFormatEnum;
    FE : TFontEnum;
    CE : TColorEnum;
Begin
 FileName := fFileName;

 Log.Writeln('Loading style from file: %s', [FileName]);

 if (not FileExists(FileName)) Then
  raise EStyleException.CreateFmt('Style file does not exist: %s', [FileName]);

 XML := TXMLConfigStorage.Create(FileName, True);

 Try
  Name := XML.GetValue('info/name', 'Default');

  For SFE in TSyntaxFormatEnum Do
   SyntaxFormatData[SFE] := LoadSyntaxFormat(SFE);

  For FE in TFontEnum Do
   FontData[FE] := LoadFont(FE);

  For CE in TColorEnum Do
   ColorData[CE] := LoadColor(CE);
 Finally
  XML.Free;
 End;

 Log.Writeln('Style loaded; name: %s', [Name]);
End;

(* TStyle.SaveToFile *)
{
 Saves style to specified file.
 If "fFileName" is an empty string, saves to the file name set by the last "LoadFromFile" call.
 When no such call happened (so "self.FileName" is also empty), an exception is raised.
}
Procedure TStyle.SaveToFile(const fFileName: String);
Var XML: TXMLConfigStorage;

  { WriteSyntaxFormat }
  Procedure WriteSyntaxFormat(const Index: TSyntaxFormatEnum);
  Var Value: TSyntaxFormat;
      Path : String;
  Begin
   Value := SyntaxFormatData[Index];
   Path  := 'syntaxformat/'+GetEnumName(TypeInfo(TSyntaxFormatEnum), ord(Index))+'/';

   XML.SetValue(Path+'FGColor', Value.FGColor);
   XML.SetValue(Path+'BGColor', Value.BGColor);
   XML.SetValue(Path+'Bold', Value.Bold);
   XML.SetValue(Path+'Italic', Value.Italic);
   XML.SetValue(Path+'Underline', Value.Underline);
   XML.SetValue(Path+'HasFGColor', Value.HasFGColor);
   XML.SetValue(Path+'HasBGColor', Value.HasBGColor);
  End;

  { WriteFont }
  Procedure WriteFont(const Index: TFontEnum);
  Var Value: TFont;
      Path : String;
  Begin
   Value := FontData[Index];
   Path  := 'font/'+GetEnumName(TypeInfo(TFontEnum), ord(Index))+'/';

   XML.SetValue(Path+'Name', Value.Name);
   XML.SetValue(Path+'Size', Value.Size);
   XML.SetValue(Path+'Color', Value.Color);
   XML.SetValue(Path+'Bold', Value.Bold);
   XML.SetValue(Path+'Italic', Value.Italic);
   XML.SetValue(Path+'Underline', Value.Underline);
  End;

  { WriteColor }
  Procedure WriteColor(const Index: TColorEnum);
  Var Value: TColor;
      Path : String;
  Begin
   Value := ColorData[Index];
   Path  := 'color/'+GetEnumName(TypeInfo(TColorEnum), ord(Index))+'/';

   XML.SetValue(Path+'Color', Value.Color);
  End;

Var Dir: String;
    SFE: TSyntaxFormatEnum;
    FE : TFontEnum;
    CE : TColorEnum;
Begin
 if (Length(FileName) = 0) and (Length(fFileName) = 0) Then
  raise EStyleException.Create('Don''t know where to save style!');

 if (Length(fFileName) > 0) Then
  FileName := fFileName;

 Log.Writeln('Saving style "%s" to file: %s', [Name, FileName]);

 // create directory, if not exists
 Dir := ExtractFileDir(FileName);

 if (not DirectoryExists(Dir)) Then
 Begin
  Log.Writeln('Destination directory ("%s") does not exist; trying to create...', [Dir]);
  mkdir(Dir);
 End;

 // save data
 XML := TXMLConfigStorage.Create(FileName, False);

 Try
  XML.SetValue('info/name', Name);

  For SFE in TSyntaxFormatEnum Do
   WriteSyntaxFormat(SFE);

  For FE in TFontEnum Do
   WriteFont(FE);

  For CE in TColorEnum Do
   WriteColor(CE);
 Finally
  XML.WriteToDisk;
  XML.Free;
 End;

 Log.Writeln('Style saved.');
End;

(* TStyle.ChangeName *)
{
 Changes style name.
}
Procedure TStyle.ChangeName(const Value: String);
Begin
 Log.Writeln('Style name changed: "%s" -> "%s"', [Name, Value]);
 Name := Value;
End;

(* TStyle.Reset *)
{
 Resets style data to defaults.
}
Procedure TStyle.Reset;
Begin
 SyntaxFormatData[sfeIdentifier]  := TSyntaxFormat.Create(0, 0, False, False, False, False, False);
 SyntaxFormatData[sfeKeyword]     := TSyntaxFormat.Create(8135705, 0, True, False, True, False, False);
 SyntaxFormatData[sfeNumber]      := TSyntaxFormat.Create(15728885, 0, True, False, True, False, False);
 SyntaxFormatData[sfeString]      := TSyntaxFormat.Create(16711680, 0, True, False, False, False, False);
 SyntaxFormatData[sfeComment]     := TSyntaxFormat.Create(7895160, 0, True, False, False, False, False);
 SyntaxFormatData[sfeMacro]       := TSyntaxFormat.Create(46080, 0, True, False, False, False, False);
 SyntaxFormatData[sfePrimaryType] := TSyntaxFormat.Create(9836825, 0, True, False, False, False, False);
 SyntaxFormatData[sfeOther]       := TSyntaxFormat.Create(240, 0, True, False, False, False, False);

 FontData[feEditorFont] := TFont.Create('Courier New', 10, 0, False, False, False);

 ColorData[ceEditorBackground] := TColor.Create(RGB(255, 255, 255));
End;

(* TStyle.setSyntaxFormat *)
Procedure TStyle.setSyntaxFormat(const Index: TSyntaxFormatEnum; const Value: TSyntaxFormat);
Begin
 SyntaxFormatData[Index] := Value;
End;

(* TStyle.setFont *)
Procedure TStyle.setFont(const Index: TFontEnum; const Value: TFont);
Begin
 FontData[Index] := Value;
End;

(* TStyle.setColor *)
Procedure TStyle.setColor(const Index: TColorEnum; const Value: TColor);
Begin
 ColorData[Index] := Value;
End;

(* TStyle.getSyntaxFormat *)
Function TStyle.getSyntaxFormat(const Index: TSyntaxFormatEnum): TSyntaxFormat;
Begin
 Result := SyntaxFormatData[Index];
End;

(* TStyle.getFont *)
Function TStyle.getFont(const Index: TFontEnum): TFont;
Begin
 Result := FontData[Index];
End;

(* TStyle.getColor *)
Function TStyle.getColor(const Index: TColorEnum): TColor;
Begin
 Result := ColorData[Index];
End;

// -------------------------------------------------------------------------- //
(* TStyleManager.Create *)
Constructor TStyleManager.Create;
Begin
 Log.Writeln('TStyleManager.Create()');

 StyleList    := TStringList.Create;
 CurrentStyle := nil;

 UpdateStyleList;
End;

(* TStyleManager.Destroy *)
Destructor TStyleManager.Destroy;
Begin
 Log.Writeln('TStyleManager.Destroy()');

 StyleList.Free;
 CurrentStyle.Free;

 inherited Destroy;
End;

(* TStyleManager.ReloadCurrentStyle *)
{
 Reloads and applies current style (according to the config) or sets the default
 one if settings are empty.
}
Procedure TStyleManager.ReloadCurrentStyle;
Var StyleFile: String;
Begin
 FreeAndNil(CurrentStyle);
 StyleFile := getStylesDir+Config.getString(ceStyleFile);

 Log.Writeln('Reloading current style; style file: %s', [StyleFile]);

 if (FileExists(StyleFile)) Then
 Begin
  CurrentStyle := TStyle.Create(StyleFile);
 End Else
 Begin
  Log.Writeln('Style file does not exist (probably first IDE run) - loading the default one and saving it as "default.xml"');

  CurrentStyle := TStyle.Create;
  CurrentStyle.ChangeName('Default');
  CurrentStyle.SaveToFile(getStylesDir+'default.xml');

  Config.setString(ceStyleFile, 'default.xml');
 End;
End;

(* TStyleManager.UpdateStyleList *)
Procedure TStyleManager.UpdateStyleList;
Var Style: TStyle;
    Path : String;
    M    : TSearchRec;
Begin
 Log.Writeln('Updating style list...');

 StyleList.Clear;

 Path := getStylesDir;
 FindFirst(Path+'*.*', faAnyFile, M);

 While (FindNext(M) = 0) Do
 Begin
  if ((M.Attr and faDirectory) = faDirectory) Then
   Continue;

  Style := TStyle.Create(Path+M.Name);

  Try
   StyleList.Add(Style.Name+'='+Path+M.Name);
   Log.Writeln('> %s', [StyleList[StyleList.Count-1]]);
  Finally
   Style.Free;
  End;
 End;

 FindClose(M);

 Log.Writeln('That''s all; total: %d style(s) found. ', [StyleList.Count]);
End;

finalization
 StyleManager.Free;
End.
