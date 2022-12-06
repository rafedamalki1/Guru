/*APPCONPAS.P
kollar att det �r r�tt version p� server och klient
*/

SESSION:SERVER-CONNECTION-BOUND-REQUEST = TRUE.


 
&Scoped-define NEW NEW GLOBAL

&Scoped-define NEW 
DEFINE INPUT PARAMETER user_id AS CHARACTER.
DEFINE INPUT PARAMETER password AS CHARACTER.
DEFINE INPUT PARAMETER app_server_info AS CHARACTER.
DEFINE VARIABLE koppla AS CHARACTER NO-UNDO.
DEFINE VARIABLE ivar AS INTEGER NO-UNDO.
DEFINE VARIABLE lognamvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE kopplaut AS CHARACTER NO-UNDO.
DEFINE VARIABLE feltxt AS CHARACTER NO-UNDO.
DEFINE VARIABLE pasoevar AS LOGICAL NO-UNDO.
DEFINE VARIABLE filnamn AS CHARACTER NO-UNDO.
{VALDBDEF.I}
{VALDBALL.I}
IF user_id NE {APPCON1.i} THEN QUIT.
IF password NE {APPCON2.i} THEN QUIT.
pasoevar = TRUE. 

filnamn = "appconPas.txt".
DELETE ALIAS RT9 . 
IF WEEKDAY(TODAY) = 2 THEN DO:
   OUTPUT TO VALUE(Guru.Konstanter:guruvar + "appconPas.txt").
   PUT "T�MD" TODAY " " STRING(TIME,"HH:MM:SS") SKIP.
   OUTPUT CLOSE. 
   OUTPUT TO VALUE(Guru.Konstanter:guruvar + "felappPAS.txt").
   PUT "T�MD" TODAY " " STRING(TIME,"HH:MM:SS") SKIP.
   OUTPUT CLOSE.       
END.
FIND FIRST valdbtemp WHERE valdbtemp.GFORETAG = TRIM(app_server_info) NO-ERROR.
IF NOT AVAILABLE valdbtemp THEN DO:
   OUTPUT TO VALUE(Guru.Konstanter:guruvar + "felappPAS.txt") APPEND.
   PUT UNFORMATTED app_server_info TODAY STRING(TIME,"hh:mm:ss") SKIP.
   OUTPUT CLOSE.  
    
   RETURN.
END.

OUTPUT TO VALUE(Guru.Konstanter:guruvar + "felappPAS.txt") APPEND.
PUT UNFORMATTED "OK " valdbtemp.GFORETAG valdbtemp.DBNAMN TODAY STRING(TIME,"hh:mm:ss") SKIP.
OUTPUT CLOSE.  
