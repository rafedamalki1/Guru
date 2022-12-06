/*AUTOLAPP.P*/
&Scoped-define NEW NEW
{GLOBVAR2DEL1.I}
DEFINE VARIABLE dbfilename AS CHARACTER NO-UNDO.
DEFINE VARIABLE prognamnque AS CHARACTER NO-UNDO. 
DEFINE VARIABLE progflytt AS CHARACTER NO-UNDO.
DEFINE VARIABLE prognamnque2 AS CHARACTER NO-UNDO.
{VALDBDEF.I}
{VALDBLAPP.I}
FIND FIRST valdbtemp NO-LOCK NO-ERROR.
IF AVAILABLE valdbtemp THEN DO:
   progflytt = SUBSTRING(valdbtemp.DBPLATS,1,INDEX(valdbtemp.DBPLATS,"DB\") - 1).
   prognamnque = progflytt + "autotid.txt". 
   prognamnque2 = progflytt + "autotidkop.txt".
END.
ELSE DO:
   prognamnque = "autotid.txt".
   prognamnque2 = "autotidkop.txt".
END.   
OUTPUT TO VALUE(prognamnque) APPEND.
PUT "START AUTOKÖRNING " TODAY " " STRING(TIME,"HH:MM:SS") SKIP.
OUTPUT CLOSE.
OPEN QUERY vq FOR EACH valdbtemp WHERE valdbtemp.DBNAMN NE "UTBI" NO-LOCK.
GET FIRST vq NO-LOCK.
DO WHILE AVAILABLE(valdbtemp): 
   {AppSprinSet.I}
   dbfilename = valdbtemp.DBNAMN.
   RUN val_UI.
   IF CONNECTED(LDBNAME(1)) THEN DO:  
      OUTPUT TO  VALUE(prognamnque) APPEND.
      PUT "db conn" TODAY " " STRING(TIME,"HH:MM:SS") SKIP .
      OUTPUT CLOSE.      
      RUN ALIASSATT.P.
      RUN MEDDBORT.P.
      RUN textut_UI (INPUT "MEDDBORT KLAR"). 
   END.  
   ELSE DO:
      OUTPUT TO  VALUE(prognamnque) APPEND.
      PUT UNFORMATTED valdbtemp.DBCON  "db ej conn" TODAY " " STRING(TIME,"HH:MM:SS") SKIP .
      OUTPUT CLOSE.
      RETURN.
   END.   
   {DBBACKAI.I}
   GET NEXT vq NO-LOCK.
END.
OUTPUT TO VALUE(prognamnque) APPEND.
PUT "SLUT AUTOKÖRNING " TODAY " " STRING(TIME,"HH:MM:SS") SKIP.
OUTPUT CLOSE.
PROCEDURE val_UI :
   DEFINE VARIABLE kommando AS CHARACTER NO-UNDO.
   kommando = valdbtemp.DBPLATS + valdbtemp.DBNAMN.
   kommando = kommando + " -P " + QUOTER({setpwd.I}) +  " -U " + QUOTER({setuser.I}).
   CONNECT VALUE(kommando) NO-ERROR. 
   /*
   CONNECT VALUE(valdbtemp.DBCON) NO-ERROR.
   */         
END PROCEDURE.

PROCEDURE textut_UI:
   DEFINE INPUT PARAMETER meddvar AS CHARACTER NO-UNDO.
   OUTPUT TO VALUE(prognamnque) APPEND.
   PUT UNFORMATTED meddvar " " valdbtemp.FORETAG " " TODAY " " STRING(TIME,"HH:MM:SS") SKIP.
   OUTPUT CLOSE.
END PROCEDURE.
