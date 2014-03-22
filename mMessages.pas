(*
 Copyright © by Patryk Wychowaniec, 2013-2014
 All rights reserved.
*)
Unit mMessages;

 Interface
 Uses mLanguages;

 Procedure InfoMessage(const Msg: String; Title: String='');
 Procedure InfoMessage(const Msg: String; const Args: Array of Const; Title: String='');
 Procedure InfoMessage(const Msg: LString);
 Procedure InfoMessage(const Msg: LString; const Args: Array of Const);

 Procedure WarningMessage(const Msg: String; Title: String='');
 Procedure WarningMessage(const Msg: String; const Args: Array of Const; Title: String='');
 Procedure WarningMessage(const Msg: LString);
 Procedure WarningMessage(const Msg: LString; const Args: Array of Const);

 Procedure ErrorMessage(const Msg: String; Title: String='');
 Procedure ErrorMessage(const Msg: String; const Args: Array of Const; Title: String='');

 Procedure ErrorMessage(const Msg: LString);
 Procedure ErrorMessage(const Msg: LString; const Args: Array of Const);
 Procedure ErrorMessage(const Msg: LString; const Args: Array of Const; const Title: LString);

 Implementation
Uses mLogger, SysUtils, Forms, LCLType;

(* InfoMessage *)
{
 Shows a modal information message.
}
Procedure InfoMessage(const Msg: String; Title: String);
Begin
 if (Title = '') Then
  Title := Language.getText(ls_msg_info);

 if (Log <> nil) Then
 Begin
  Log.Writeln('Information message:');
  Log.Writeln('Title = %s', [Title]);
  Log.Writeln('Msg = %s', [Msg]);
 End;

 Application.MessageBox(PChar(Msg), PChar(Title), MB_ICONINFORMATION);
End;

(* InfoMessage *)
{
 Shows a modal information message.
}
Procedure InfoMessage(const Msg: String; const Args: Array of const; Title: String);
Begin
 InfoMessage(Format(Msg, Args), Title);
End;

(* InfoMessage *)
{
 Shows a modal information message.
}
Procedure InfoMessage(const Msg: LString);
Begin
 InfoMessage(Msg, []);
End;

(* InfoMessage *)
{
 Shows a modal information message.
}
Procedure InfoMessage(const Msg: LString; const Args: Array of const);
Begin
 InfoMessage(Format(Language.getText(Msg), Args));
End;

(* WarningMessage *)
{
 Shows a modal warning message.
}
Procedure WarningMessage(const Msg: String; Title: String);
Begin
 if (Title = '') Then
  Title := Language.getText(ls_msg_warn);

 if (Log <> nil) Then
 Begin
  Log.Writeln('Warning message:');
  Log.Writeln('Title = %s', [Title]);
  Log.Writeln('Msg = %s', [Msg]);
 End;

 Application.MessageBox(PChar(Msg), PChar(Title), MB_ICONWARNING);
End;

(* WarningMessage *)
{
 Shows a modal warning message.
}
Procedure WarningMessage(const Msg: String; const Args: Array of const; Title: String);
Begin
 WarningMessage(Format(Msg, Args), Title);
End;

(* WarningMessage *)
{
 Shows a modal warning message.
}
Procedure WarningMessage(const Msg: LString);
Begin
 WarningMessage(Msg, []);
End;

(* WarningMessage *)
{
 Shows a modal warning message.
}
Procedure WarningMessage(const Msg: LString; const Args: Array of const);
Begin
 WarningMessage(Format(Language.getText(Msg), Args));
End;

(* ErrorMessage *)
{
 Shows a modal error message.
}
Procedure ErrorMessage(const Msg: String; Title: String);
Begin
 if (Title = '') Then
  Title := Language.getText(ls_msg_error);

 if (Log <> nil) Then
 Begin
  Log.Writeln('Error message:');
  Log.Writeln('Title = %s', [Title]);
  Log.Writeln('Msg = %s', [Msg]);
 End;

 Application.MessageBox(PChar(Msg), PChar(Title), MB_ICONERROR);
End;

(* ErrorMessage *)
{
 Shows a modal error message.
}
Procedure ErrorMessage(const Msg: String; const Args: Array of const; Title: String);
Begin
 ErrorMessage(Format(Msg, Args), Title);
End;

(* ErrorMessage *)
{
 Shows a modal error message.
}
Procedure ErrorMessage(const Msg: LString);
Begin
 ErrorMessage(Msg, []);
End;

(* ErrorMessage *)
{
 Shows a modal error message.
}
Procedure ErrorMessage(const Msg: LString; const Args: Array of Const);
Begin
 ErrorMessage(Msg, Args, ls_msg_error);
End;

(* ErrorMessage *)
{
 Shows a modal error message
}
Procedure ErrorMessage(const Msg: LString; const Args: Array of Const; const Title: LString);
Begin
 ErrorMessage(Language.getText(Msg, Args), Language.getText(Title));
End;
End.
