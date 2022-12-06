/*serverUPDATE.p*/

DEFINE VARIABLE computername  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE complength    AS INTEGER     NO-UNDO INITIAL 128.
DEFINE VARIABLE retvalue      AS INTEGER     NO-UNDO.
DEFINE VARIABLE provag AS CHARACTER NO-UNDO.
DEFINE VARIABLE outanvanv AS CHARACTER NO-UNDO.
DEFINE VARIABLE outdatornamn AS CHARACTER NO-UNDO.

{VALDBDEF.I}
{VALDBALL.I}
FUNCTION namndb RETURNS CHARACTER :   
   IF PDBNAME(1) = ? THEN RETURN ?.
   ELSE IF R-INDEX(PDBNAME(1),"\") = 0 THEN RETURN PDBNAME(1).
   ELSE RETURN SUBSTRING(PDBNAME(1),R-INDEX(PDBNAME(1),"\") + 1).
     
END FUNCTION.

{Computer_LanIP.I}


MESSAGE "Du kör för ip : " Computer_LanIP
VIEW-AS ALERT-BOX.
FIND FIRST valdbtemp WHERE valdbtemp.DBCON MATCHES "*" + Computer_LanIP + "*" NO-LOCK NO-ERROR.
IF NOT AVAILABLE valdbtemp THEN DO:
   MESSAGE "Söker på maskin namn!"
   VIEW-AS ALERT-BOX.
   computername = FILL(" ", complength).
   RUN GetComputerNameA (INPUT-OUTPUT computername, OUTPUT complength, OUTPUT retvalue).
   computername = RIGHT-TRIM(computername).
   Computer_LanIP = computername.
   FIND FIRST valdbtemp WHERE valdbtemp.DBCON MATCHES "*" + Computer_LanIP + "*" NO-LOCK NO-ERROR.
   IF NOT AVAILABLE valdbtemp THEN DO:
      MESSAGE "Hittar inga databaser!"
      VIEW-AS ALERT-BOX.
      RETURN.
   END.   
END.

FOR EACH valdbtemp WHERE valdbtemp.DBCON MATCHES "*" + Computer_LanIP + "*" NO-LOCK:
   IF valdbtemp.FORETAG = "SEKG" OR valdbtemp.FORETAG = "DARPLU" THEN MESSAGE "SEKG OCH DARPLU KÖRS INTE"
                                      VIEW-AS ALERT-BOX.
 
   DO:
      MESSAGE valdbtemp.DBCON " ?"  VIEW-AS ALERT-BOX
      QUESTION BUTTONS YES-NO  UPDATE val AS LOGICAL. 
      IF val = TRUE THEN DO:
         {AppSprinSet.I}      
         RUN val_UI.
         IF CONNECTED(LDBNAME(1)) THEN DO:       
            /* IF prognamn NE "" THEN SAVE CACHE COMPLETE VALUE(LDBNAME(1)) TO VALUE(prognamn).*/
             RUN ALIASSATT.P.     
              
         
            RUN UPPDATERASTART.P. 
            DISCONNECT VALUE(LDBNAME(1)) NO-ERROR.
         END.
         ELSE DO:
            MESSAGE valdbtemp.DBCON " Gick inte att ansluta!"
            VIEW-AS ALERT-BOX.
         END.   
      END.
   END.     
END.
MESSAGE "Klart för IP: " Computer_LanIP
VIEW-AS ALERT-BOX.
PROCEDURE val_UI :
   DEFINE VARIABLE koppla AS CHARACTER NO-UNDO.
   koppla = valdbtemp.DBCON + " -ld rt9" + " -P " + QUOTER({setpwd.I}) +  " -U " + QUOTER({setuser.I}).
   
   CONNECT VALUE(koppla) NO-ERROR.       
END PROCEDURE.


PROCEDURE GetComputerNameA EXTERNAL "kernel32":
   DEFINE INPUT-OUTPUT PARAMETER lpszName AS CHAR.
   DEFINE OUTPUT PARAMETER lpdwcBuffer AS LONG.
   DEFINE RETURN PARAMETER ReturnValue AS LONG.
END PROCEDURE.


/*
QUIT.
*/
