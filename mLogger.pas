(*
 Copyright © by Patryk Wychowaniec, 2013-2014
 All rights reserved.
*)
Unit mLogger;

 Interface
 Uses SysUtils;

 Const LogFile = 'editor.log';

 { TLogger }
 Type TLogger =
      Class
       Private
        FileHandle: TextFile;
        LogEnabled: Boolean;

        MessagesSinceLastFlush: uint32;

       Public
        Constructor Create;
        Destructor Destroy; override;

        Procedure Writeln(const Msg: String);
        Procedure Writeln(const Msg: String; const Args: Array of Const);

        Procedure LogException(const E: Exception);
       End;

 // logger class instance
 Var Log: TLogger;

 Implementation
Uses mConfiguration, mFunctions, mMessages;

(* TLogger.Create *)
Constructor TLogger.Create;
Var FileName: String;
Begin
 FileName   := getApplicationDir+LogFile;
 LogEnabled := Config.getBoolean(ceEnableLogging);

 Try
  if (LogEnabled) Then
  Begin
   AssignFile(FileHandle, FileName);

   if (Config.getBoolean(ceRewriteLog)) or (not FileExists(FileName)) Then
    Rewrite(FileHandle) Else
    Append(FileHandle);

   Writeln('-- log begin --');
  End;
 Except
  On E: Exception Do
  Begin
   LogEnabled := False;

   ErrorMessage
   (
     'An exception occured during opening of the log file (%s)'#13#10+
     #13#10+
     'The logging feature for this IDE session has been disabled to prevent any further exceptions.'#13#10+
     'Try to restart the IDE.'#13#10+
     #13#10+
     'Exception message:'#13#10+
     '%s',
     [FileName, E.Message], 'Error'
   );
  End;
 End;
End;

(* TLogger.Destroy *)
Destructor TLogger.Destroy;
Begin
 if (LogEnabled) Then
 Begin
  Writeln('-- log end --');
  Flush(FileHandle);
  CloseFile(FileHandle);
 End;

 inherited Destroy;
End;

(* TLogger.Writeln *)
{
 Writes a message to the log.
}
Procedure TLogger.Writeln(const Msg: String);
Begin
 if (LogEnabled) Then
 Begin
  // write message
  System.Writeln(FileHandle, Format('[%s %s] %s', [DateToStr(Date), TimeToStr(Time), Msg]));

  // increase the counter
  Inc(MessagesSinceLastFlush);

  // flush every 15 messages
  if (MessagesSinceLastFlush > 15) Then
  Begin
   Flush(FileHandle);
   MessagesSinceLastFlush := 0;
  End;
 End;
End;

(* TLogger.Writeln *)
{
 Writes a formatted message to log.
}
Procedure TLogger.Writeln(const Msg: String; const Args: Array of const);
Begin
 Writeln(Format(Msg, Args));
End;

(* TLogger.LogException *)
Procedure TLogger.LogException(const E: Exception);
Var I: int8;
Begin
 Writeln('!!! An exception was raised !!!');
 Writeln('');
 Writeln('Message: %s', [E.Message]);
 Writeln('Class: %s', [E.ClassName]);
 Writeln('');
 Writeln('Stacktrace:');
 Writeln(BacktraceStrFunc(ExceptAddr));

 For I := 0 To ExceptFrameCount-1 Do
  Writeln(BacktraceStrFunc(ExceptFrames[I]));
End;

finalization
 Log.Free;
End.
