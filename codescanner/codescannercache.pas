(*
 Copyright © by Patryk Wychowaniec, 2013-2014
 All rights reserved.
*)
Unit CodeScannerCache;

 Interface
 Uses SysUtils;

 { ECodeScannerCacheException }
 Type ECodeScannerCacheException = Class(Exception);

 { TCacheRecord }
 Type TCacheRecord =
      Record
       FileName, FileHash: String;
       CodeScanner       : TObject;
      End;

 { TCacheList }
 Type TCacheList = Array of TCacheRecord;

 { TObjectList }
 Type TObjectList = Array of TObject;

 { TCodeScannerCache }
 Type TCodeScannerCache =
      Class
       Private
        List         : TCacheList;
        ObjectsToFree: TObjectList;

       Public
        Constructor Create;
        Destructor Destroy; override;

        Procedure AddOrUpdate(const FileName: String; const CodeScanner: TObject);

        Procedure InvalidateFile(const FileName: String);
        Procedure PurgeMemory;

        Procedure FreeNamespace(const NS: TObject);

        Function getCodeScanner(const FileName: String): TObject;
        Function findCodeScanner(const CodeScanner: TObject): Boolean;
       End;

 Implementation
Uses CodeScanner, MD5;

(* TCodeScannerCache.Create *)
Constructor TCodeScannerCache.Create;
Begin
 SetLength(List, 0);
 SetLength(ObjectsToFree, 0);
End;

(* TCodeScannerCache.Destroy *)
Destructor TCodeScannerCache.Destroy;
Begin
 PurgeMemory;

 inherited Destroy;
End;

(* TCodeScannerCache.InvalidateFile *)
Procedure TCodeScannerCache.InvalidateFile(const FileName: String);
Var I: Integer;
Begin
 For I := Low(List) To High(List) Do
 Begin
  if (List[I].FileName = FileName) Then
  Begin
   List[I].FileHash := '';
   Exit;
  End;
 End;
End;

(* TCodeScannerCache.PurgeMemory *)
Procedure TCodeScannerCache.PurgeMemory;
Var Rec: TCacheRecord;
    Obj: TObject;
Begin
 For Rec in List Do
  TCodeScanner(Rec.CodeScanner).Free;

 For Obj in ObjectsToFree Do
  Obj.Free;

 SetLength(List, 0);
 SetLength(ObjectsToFree, 0);
End;

(* TCodeScannerCache.FreeNamespace *)
Procedure TCodeScannerCache.FreeNamespace(const NS: TObject);
Var Obj: TObject;
Begin
 // check for duplicates
 For Obj in ObjectsToFree Do
  if (Obj = NS) Then
   Exit;

 // resize array
 SetLength(ObjectsToFree, Length(ObjectsToFree)+1);

 // put element
 ObjectsToFree[High(ObjectsToFree)] := NS;
End;

(* TCodeScannerCache.AddOrUpdate *)
Procedure TCodeScannerCache.AddOrUpdate(const FileName: String; const CodeScanner: TObject);
Var Updated: Boolean = False;
    Rec    : TCacheRecord;
    I      : Integer;
Begin
 // check if file exists
 if (not FileExists(FileName)) Then
  raise ECodeScannerCacheException.CreateFmt('File not found: %s', [FileName]);

 // prepare record
 Rec.FileName    := FileName;
 Rec.FileHash    := MDPrint(MDFile(FileName, MD_VERSION_5));
 Rec.CodeScanner := CodeScanner;

 // update
 For I := Low(List) To High(List) Do
 Begin
  if (List[I].FileName = FileName) Then
  Begin
   List[I] := Rec;
   Updated := True;
   Break;
  End;
 End;

 // or add new
 if (not Updated) Then
 Begin
  SetLength(List, Length(List)+1);
  List[High(List)] := Rec;
 End;
End;

(* TCodeScannerCache.getCodeScanner *)
Function TCodeScannerCache.getCodeScanner(const FileName: String): TObject;
Var Hash: String;
    I   : Integer;
Begin
 Result := nil;

 // check if file exists
 if (not FileExists(FileName)) Then
  Exit;

 // compute its hash
 Hash := MDPrint(MDFile(FileName, MD_VERSION_5));

 // find appropriate scanner
 For I := Low(List) To High(List) Do
 Begin
  if (List[I].FileHash = Hash) Then
   Exit(List[I].CodeScanner);
 End;
End;

(* TCodeScannerCache.findCodeScanner *)
Function TCodeScannerCache.findCodeScanner(const CodeScanner: TObject): Boolean;
Var I: Integer;
Begin
 Result := False;

 For I := Low(List) To High(List) Do
 Begin
  if (List[I].CodeScanner = CodeScanner) Then
   Exit(True);
 End;
End;
End.
