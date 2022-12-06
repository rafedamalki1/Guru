/*KRYSSDEF.I*/
FUNCTION DisableWindowClose RETURNS LOGICAL
    ( INPUT winhandle AS HANDLE  /* parameter-definitions */ )  
    IN Guru.Konstanter:hpWinFunc.

/*
&GLOB MF_BYPOSITION 1024
&GLOB MF_REMOVE     256
Procedure GetSystemMenu External "user32":
  define input parameter hwnd       as  long.
  define input parameter bRevert    as  long.
  define return parameter lRetCode  as  long.
End.

Procedure GetMenuItemCount External "user32":
  define input parameter hMenu      as  long.
  define return parameter iRetCode  as  long.
End.

Procedure DrawMenuBar External "user32":
  define input parameter hMenu      as  long.
  define return parameter iRetCode  as  long.
End.

Procedure RemoveMenu External "user32":
  define input parameter hMenu      as  long.
  define input parameter nPosition  as  long.
  define input parameter wFlags     as  long.
  define return parameter iRetCode  as  long.
End.
/*
Procedure GetParent External "user32":
  define input  parameter thishwnd     as long.
  define return parameter parenthwnd   as long.
End.
*/
FUNCTION DisableWindowClose RETURNS LOGICAL
  ( INPUT winhandle AS HANDLE  /* parameter-definitions */ )  FORWARD.
  */
