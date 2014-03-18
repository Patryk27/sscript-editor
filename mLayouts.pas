(*
 Copyright © by Patryk Wychowaniec, 2013-2014
 All rights reserved.
*)
Unit mLayouts;

 Interface
 Uses XMLPropStorage, Classes, SysUtils, Forms;

 { TLayoutForm }
 Type TLayoutForm =
      Record
       Name                    : String;
       Top, Left, Width, Height: Integer;
       Visible                 : Boolean;
       State                   : TWindowState;
      End;

 { TLayoutFormList }
 Type TLayoutFormList = Array of TLayoutForm;

 { TLayout }
 Type TLayout =
      Class
       Private
        Name : String;
        Forms: TLayoutFormList;

       Public
        Constructor Create;
        Constructor Create(const Layout: TLayout);
        Constructor Create(const FileName: String);

        Procedure LoadFromFile(const FileName: String);
        Procedure SaveToFile(const FileName: String);

        Procedure Reset;
        Procedure Update;
        Procedure Apply;

        Procedure ChangeName(const fName: String);

       Public
        Property getName: String read Name;
        Property getForms: TLayoutFormList read Forms;
       End;

 { TLayoutManager }
 Type TLayoutManager =
      Class
       Private
        LayoutList   : TStringList;
        CurrentLayout: TLayout;

       Public
        Constructor Create;
        Destructor Destroy; override;

        Procedure ReloadCurrentLayout;
        Procedure UpdateLayoutList;

       Public
        Property getLayoutList: TStringList read LayoutList;
        Property getCurrentLayout: TLayout read CurrentLayout;
       End;

 // layout manager instance
 Var LayoutManager: TLayoutManager;

 Implementation
Uses mSettings;

(* TLayout.Create *)
{
 Creates an empty, clean layout.
}
Constructor TLayout.Create;
Begin
 Reset;
End;

(* TLayout.Create *)
{
 Creates a copy of layout.
}
Constructor TLayout.Create(const Layout: TLayout);
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
  Forms[I].State    := Layout.Forms[I].State;
 End;
End;

(* TLayout.Create *)
{
 Loads layout from the specified file.
}
Constructor TLayout.Create(const FileName: String);
Begin
 LoadFromFile(FileName);
End;

(* TLayout.LoadFromFile *)
{
 Loads layout from the specified file.
}
Procedure TLayout.LoadFromFile(const FileName: String);
Var XML: TXMLConfigStorage;

  { LoadForm }
  Function LoadForm(const Name: String): TLayoutForm;
  Begin
   Result.Name     := Name;
   Result.Top      := XML.GetValue(Name+'/top', 0);
   Result.Left     := XML.GetValue(Name+'/left', 0);
   Result.Width    := XML.GetValue(Name+'/width', 100);
   Result.Height   := XML.GetValue(Name+'/height', 100);
   Result.Visible  := XML.GetValue(Name+'/visible', True);
   Result.State    := TWindowState(XML.GetValue(Name+'/state', ord(wsNormal)));
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
{
 Saves layout to the specified file.
}
Procedure TLayout.SaveToFile(const FileName: String);
Var XML : TXMLConfigStorage;
    Form: TLayoutForm;

  { SaveForm }
  Procedure SaveForm(const Form: TLayoutForm);
  Begin
   With Form do
   Begin
    XML.SetValue(Name+'/top', Top);
    XML.SetValue(Name+'/left', Left);
    XML.SetValue(Name+'/width', Width);
    XML.SetValue(Name+'/height', Height);
    XML.SetValue(Name+'/visible', Visible);
    XML.SetValue(Name+'/state', ord(State));
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
  XML.WriteToDisk;
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
  Height  := Screen.Height-265;
  Visible := True;
  State   := wsNormal;
 End;

 { identifier list form }
 With Forms[1] do
 Begin
  Name    := 'IdentifierListForm';
  Top     := 88;
  Left    := Screen.Width-222;
  Width   := 200;
  Height  := Screen.Height-265;
  Visible := True;
  State   := wsNormal;
 End;

 { compile status form }
 With Forms[2] do
 Begin
  Name    := 'CompileStatusForm';
  Top     := Screen.Height-145;
  Left    := 4;
  Width   := Screen.Width-24;
  Height  := 75;
  Visible := True;
  State   := wsNormal;
 End;
End;

(* TLayout.Update *)
Procedure TLayout.Update;
Var Form: TCustomForm;
    I   : uint16;
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
   Forms[I].Width   := Width;
   Forms[I].Height  := Height;
   Forms[I].Visible := Visible;
   Forms[I].State   := WindowState;
  End;
 End;
End;

(* TLayout.Apply *)
Procedure TLayout.Apply;
Var Form: TCustomForm;
    I   : uint16;
Begin
 For I := Low(Forms) To High(Forms) Do
 Begin
  Form := Screen.FindForm(Forms[I].Name);

  if (Form = nil) Then
   raise Exception.CreateFmt('TLayout.Apply() -> form does not exist: %s', [Forms[I].Name]);

  With Form do
  Begin
   Top         := Forms[I].Top;
   Left        := Forms[I].Left;
   Width       := Forms[I].Width;
   Height      := Forms[I].Height;
   Visible     := Forms[I].Visible;
   WindowState := Forms[I].State;
  End;
 End;
End;

(* TLayout.ChangeName *)
Procedure TLayout.ChangeName(const fName: String);
Begin
 Name := fName;
End;

// -------------------------------------------------------------------------- //
(* TLayoutManager.Create *)
Constructor TLayoutManager.Create;
Begin
 LayoutList    := TStringList.Create;
 CurrentLayout := nil;

 UpdateLayoutList;
End;

(* TLayoutManager.Destroy *)
Destructor TLayoutManager.Destroy;
Begin
 LayoutList.Free;
 CurrentLayout.Free;

 inherited Destroy;
End;

(* TLayoutManager.ReloadCurrentLayout *)
{
 Reloads and applies current layout according to the settings file or sets the
 default one if settings are empty.
}
Procedure TLayoutManager.ReloadCurrentLayout;
Var LayoutFile: String;
Begin
 LayoutFile := getString(sLayoutFile);

 if (CurrentLayout <> nil) Then
  FreeAndNil(CurrentLayout);

 if (not FileExists(LayoutFile)) Then // if selected layout file doesn't exist, select the default one
 Begin
  CurrentLayout      := TLayout.Create;
  CurrentLayout.Name := 'Default';
  CurrentLayout.Reset;
 End Else
 Begin
  CurrentLayout := TLayout.Create(LayoutFile);
 End;

 CurrentLayout.Apply;
End;

(* TLayoutManager.UpdateLayoutList *)
{
 Updates the layout list according to the "layouts" directory.
 Layout list is generated in format:
   Layout name = full path to the layout file
}
Procedure TLayoutManager.UpdateLayoutList;
Var Layout: TLayout;
    Path  : String;
    M     : TSearchRec;
Begin
 LayoutList.Clear;

 Path := getLayoutDir;

 FindFirst(Path+'*.xml', faAnyFile, M);

 While (FindNext(M) = 0) Do
 Begin
  Layout := TLayout.Create(Path+M.Name);

  Try
   LayoutList.Add(Layout.Name+'='+Path+M.Name);
  Finally
   Layout.Free;
  End;
 End;

 FindClose(M);
End;

initialization
 LayoutManager := TLayoutManager.Create;

finalization
 LayoutManager.Free;
End.
