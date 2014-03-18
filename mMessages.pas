(*
 Copyright © by Patryk Wychowaniec, 2013-2014
 All rights reserved.
*)
Unit mMessages;

 Interface
 Uses mLanguages;

 Procedure ErrorMessage(const Msg: LString);
 Procedure ErrorMessage(const Msg: LString; const Args: Array of Const);

 Implementation
Uses SysUtils, Forms, LCLType;

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
Procedure ErrorMessage(const Msg: LString; const Args: Array of const);
Begin
 Application.MessageBox(PChar(Format(getLangValue(Msg), Args)), PChar(getLangValue(ls_msg_error)), MB_ICONERROR);
End;
End.
