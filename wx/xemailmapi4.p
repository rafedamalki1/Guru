/*The Tapi (Telephony API) can be used to dial a telephone number or pager. 

This source is from Johann van der Merwe.
His introduction says:
This is an example of what we use. The code is not cleaned up. We also
use our own dialing properties (for instance a outside line). Hope this
helps. Use MSDN to look up tapiRequestMakeCall for parameter info.
DEFI: */
DO ON ERROR UNDO, RETURN "ERROR-DEFI":U:
 
  DEFINE INPUT PARAMETER TelNoStr AS CHARACTER NO-UNDO. 
 
  DEFINE VARIABLE GetLine     AS CHARACTER NO-UNDO.
  DEFINE VARIABLE TelNo       AS MEMPTR    NO-UNDO.
  DEFINE VARIABLE i           AS INTEGER   NO-UNDO.
  DEFINE VARIABLE ReturnValue AS INTEGER   NO-UNDO.
END.
 
Main: DO ON ERROR UNDO, RETURN "ERROR-MAIN":U:
 
  GET-KEY-VALUE SECTION "Modem":U KEY "GetLine":U VALUE GetLine.
  IF GetLine = "?":U OR GetLine = ? THEN 
     GetLine = "":U.
 
  SET-SIZE(TelNo) = LENGTH(GetLine + TelNoStr) + 1.
  DO i = 1 TO LENGTH(TelNoStr + GetLine): 
     PUT-BYTE(TelNo,i) = ASC(SUBSTRING(GetLine + TelNoStr,i,1)).
  END.
  PUT-BYTE(TelNo,i) = 0.
  RUN tapiRequestMakeCall (INPUT GET_POINTER-VALUE(TelNo), 
                           INPUT "0":U, 
                           INPUT "0":U, 
                           INPUT "0":U,
                           OUTPUT ReturnValue).
END. 
 
PROCEDURE tapiRequestMakeCall EXTERNAL "tapi32.dll":U:
  DEFINE INPUT  PARAMETER lpszDestAddress AS LONG.
  DEFINE INPUT  PARAMETER lpszAppName     AS CHARACTER.
  DEFINE INPUT  PARAMETER lpszCalledParty AS CHARACTER.
  DEFINE INPUT  PARAMETER lpszComment     AS CHARACTER.
  DEFINE RETURN PARAMETER ReturnValue     AS LONG.
END PROCEDURE.

 

