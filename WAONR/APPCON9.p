/**/

/*körs numera via flex men kollar bara att det är rätt version på server och klient
OCH TÖMMER LOGGFILES
*/

 
&Scoped-define NEW NEW GLOBAL
/*{EGENBVAR.I}*/
/*SESSION:NUMERIC-FORMAT = "AMERICAN".                                                                                    
SESSION:NUMERIC-FORMAT = "EUROPEAN"*/

&Scoped-define NEW 
DEFINE INPUT PARAMETER user_id AS CHARACTER.
DEFINE INPUT PARAMETER password AS CHARACTER.
DEFINE INPUT PARAMETER app_server_info AS CHARACTER.
DEFINE VARIABLE koppla AS CHARACTER NO-UNDO.
DEFINE VARIABLE ivar AS INTEGER NO-UNDO.
DEFINE VARIABLE lognamvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE kopplaut AS CHARACTER NO-UNDO.
DEFINE VARIABLE filnamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE feltxt AS CHARACTER NO-UNDO.
DEFINE VARIABLE pasoevar AS LOGICAL NO-UNDO.
{VALDBDEF.I}
{VALDBALL.I}
IF user_id NE {APPCON1.i} THEN QUIT.
IF password NE {APPCON2.i} THEN QUIT.


filnamn = "appcon.txt".
DELETE ALIAS RT9 . 
IF WEEKDAY(TODAY) = 2 THEN DO:
   OUTPUT TO VALUE(Guru.Konstanter:guruvar + "appcon.txt").
   PUT "TÖMD" TODAY " " STRING(TIME,"HH:MM:SS") SKIP.
   OUTPUT CLOSE.  
   OUTPUT TO VALUE(Guru.Konstanter:guruvar + "felapp.txt").
   PUT "TÖMD" TODAY " " STRING(TIME,"HH:MM:SS") SKIP.
   OUTPUT CLOSE.     
END.
FIND FIRST valdbtemp WHERE valdbtemp.GFORETAG = TRIM(app_server_info) NO-ERROR.
IF NOT AVAILABLE valdbtemp THEN DO:
   OUTPUT TO VALUE(Guru.Konstanter:guruvar + "felapp.txt") APPEND.
   PUT UNFORMATTED "1 " app_server_info " " TODAY " " STRING(TIME,"hh:mm:ss") SKIP.
   OUTPUT CLOSE.  
    
   RETURN.
END.

