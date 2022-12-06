/*TELAPI.P*/
DEF VAR FEL AS CHAR.
run call-numbeR (INPUT '070-3736616',OUTPUT FEL).
DISP FEL.
PROCEDURE call-number:
define input parameter ntele# as character no-undo.
define output parameter lsucc# as logical no-undo initial false.
define variable nteleorig# as char no-undo.
define variable nline# as char no-undo.
define variable atele# as memptr no-undo.
define variable atell# as integer no-undo.

assign nteleorig# = ntele#
ntele# = replace(ntele#," ","")
ntele# = replace(ntele#,"-","").

do atell# = 1 to length(ntele#):
if lookup(substring(ntele#,atell#,1),"0,1,2,3,4,5,6,7,8,9") = 0 then
do:
return "Invalid character in phone number: " + nteleorig#.
end.
end.

if length(ntele#) > 10 then
return "Phone number too long: " + nteleorig#.

if length(ntele#) < 5 then
return "Phone number too short: " + nteleorig#.

assign lsucc# = true.

GET-KEY-VALUE SECTION "Modem":U KEY "GetLine":U VALUE nline#.
IF nline# = "?":U OR nline# = ? THEN nline# = "":U.
SET-SIZE(atele#) = LENGTH(nline# + ntele#) + 1.
DO atell# = 1 TO LENGTH(ntele# + nline#):
PUT-BYTE(atele#,atell#) = ASC(SUBSTRING(nline# + ntele#,atell#,1)).
END.
PUT-BYTE(atele#,atell#) = 0.
RUN tapiRequestMakeCall (INPUT atele#,
INPUT "0":U,
INPUT "0":U,
INPUT "0":U).

END PROCEDURE. /* bel-nummer */


PROCEDURE tapiRequestMakeCall EXTERNAL "tapi32.dll":U:
DEFINE INPUT PARAMETER lpszDestAddress AS MEMPTR.
DEFINE INPUT PARAMETER lpszAppName AS CHARACTER.
DEFINE INPUT PARAMETER lpszCalledParty AS CHARACTER.
DEFINE INPUT PARAMETER lpszComment AS CHARACTER.
END PROCEDURE.
