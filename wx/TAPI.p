/*TAPI.P*/
/*Syntax to call is */
/*run tapi.p ('(800) 555-1234','Called persons Name').*/

/*Connecting computers to phone systems range from being simple to
complex, depending on your voice communications system. On my computer I
added a second 2400 baud modem so I wouldn't tie up main modem for
controlling customers' computers.

Sincerely Rick - NPO Solutions.  */

/* start tapi.p */
DEFINE INPUT PARAMETER TelNoStr AS CHARACTER NO-UNDO. 
DEFINE INPUT PARAMETER TelNoName AS CHARACTER NO-UNDO. 

DEFINE VARIABLE GetLine AS CHARACTER NO-UNDO.
DEFINE VARIABLE TelNo AS MEMPTR NO-UNDO.
DEFINE VARIABLE i AS INTEGER NO-UNDO.

GET-KEY-VALUE SECTION "Modem":U KEY "GetLine":U VALUE GetLine.
IF GetLine = "?":U OR GetLine = ? THEN 
GetLine = "":U.

SET-SIZE(TelNo) = LENGTH(GetLine + TelNoStr) + 1.
DO i = 1 TO LENGTH(TelNoStr + GetLine): 
PUT-BYTE(TelNo,i) = ASC(SUBSTRING(GetLine + TelNoStr,i,1)).
END.
/*PUT-BYTE(TelNo,i) = 0.*/
RUN tapiRequestMakeCall (INPUT TelNo, 
INPUT "0":U, 
INPUT TelNoName, 
INPUT "0":U).

PROCEDURE tapiRequestMakeCall EXTERNAL "tapi32.dll":U:
DEFINE INPUT PARAMETER lpszDestAddress AS MEMPTR.
DEFINE INPUT PARAMETER lpszAppName AS CHARACTER.
DEFINE INPUT PARAMETER lpszCalledParty AS CHARACTER.
DEFINE INPUT PARAMETER lpszComment AS CHARACTER.
END PROCEDURE.

/* end tapi.p */
