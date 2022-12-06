/*APPCLASSER.p*/

 
DEFINE INPUT PARAMETER app_server_info AS CHARACTER.
DEFINE OUTPUT PARAMETER classerkoll AS CHARACTER NO-UNDO.
{VALDBTEMP.I} 
{VALDBALL.I}
FIND FIRST valdbtemp WHERE valdbtemp.GFORETAG = TRIM(app_server_info) NO-ERROR.
IF AVAILABLE valdbtemp THEN DO:
   classerkoll = valdbtemp.DBCACHE.
   Guru.Konstanter:varforetypchar[48] = valdbtemp.DBCACHE.
END.   
