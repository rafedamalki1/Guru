/*DEPCHECKLU.P*/
&Scoped-define NEW NEW
&Scoped-define SHARED SHARED
{GLOBVAR2DEL1.I}


DEFINE VARIABLE inventant AS INTEGER NO-UNDO.
DEFINE VARIABLE inventante AS INTEGER NO-UNDO.
DEFINE VARIABLE datvar AS DATE NO-UNDO.

DEFINE VARIABLE forsta AS LOGICAL NO-UNDO.
DEFINE BUFFER mtrldepbuff FOR MTRLDEP.
  
  
DEFINE VARIABLE skick AS LOGICAL NO-UNDO.
DEFINE VARIABLE efel AS CHARACTER FORMAT "X(30)" NO-UNDO.
DEFINE VARIABLE servervar AS CHARACTER LABEL "Smtp Server" NO-UNDO.
DEFINE VARIABLE franvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE tillvar AS CHARACTER NO-UNDO.
DEFINE TEMP-TABLE eposttemp NO-UNDO
   FIELD EPOST AS CHARACTER
   FIELD MEDD AS CHARACTER
   INDEX EPOST EPOST.
FIND FIRST FORETAG NO-LOCK NO-ERROR.
Guru.Konstanter:globforetag = FORETAG.FORETAG.
RUN STYRFORE.P (INPUT FORETAG.FORETAG).
ASSIGN
/*vad gäller för luleå????*/
/*tillvar = "lena@elpool.se"
franvar = "NOREPLY"
servervar = "130.1.27.253".
*/


forsta = TRUE.
OUTPUT TO D:\elpool\DELAD\PRO9s\depchecklu.txt APPEND.
datvar = 01/01/90.  
FOR EACH MTRLDEP WHERE MTRLDEP.DEPNR = 1 AND 
MTRLDEP.IBDATUM NE ? BY MTRLDEP.IBDATUM.   
   IF MTRLDEP.IBDATUM > datvar THEN DO:           
      datvar = MTRLDEP.IBDATUM.
   END.         
END.
/*datvar -SISTA INVENTERINGSDATUM*/ 
/*sista invenetringen plus alla uttag returer och mottagningar 
   skall överensstämma med aktuellt lagersaldo*/
FOR EACH MTRLDEP WHERE MTRLDEP.DEPNR = 1 AND 
MTRLDEP.IBDATUM = datvar NO-LOCK:
   /*alla inventerade poster sista inventering*/
   inventant = MTRLDEP.INVANT.
   FOR EACH BERBEST WHERE BERBEST.DEPNR = 1 AND BERBEST.ENR = MTRLDEP.ENR AND
   (BERBEST.LEVDATUM >= MTRLDEP.INVDATUM AND BERBEST.LEVDATUM <= TODAY) NO-LOCK:
      /*alla uttag och returer sedan sista inventeringen*/
      IF BERBEST.UTTAG = TRUE THEN inventant = inventant - BERBEST.ANTAL.
      ELSE inventant = inventant + BERBEST.ANTAL.
   END.
   FOR EACH BESTDEP WHERE BESTDEP.DEPNR = 1 AND BESTDEP.ENR = MTRLDEP.ENR AND
   (BESTDEP.LEVDATUM >= MTRLDEP.INVDATUM AND BESTDEP.LEVDATUM <= TODAY) AND BESTDEP.BERED = FALSE AND
   BESTDEP.LEVNAMN = "" NO-LOCK:
      /*alla mottagna leveranser och korrigeringar i lager. Ej returer BESTDEP.BERED = TRUE*/
      inventant = inventant + BESTDEP.ANTAL.
   END.
   inventante = inventant.
   FIND FIRST mtrldepbuff WHERE mtrldepbuff.DEPNR = 1 AND mtrldepbuff.ENR = MTRLDEP.ENR AND mtrldepbuff.IBDATUM = ? NO-LOCK NO-ERROR.
   IF AVAILABLE mtrldepbuff THEN DO:
      IF inventant NE mtrldepbuff.SALDO THEN DO:
         FOR EACH BERBEST WHERE BERBEST.DEPNR = 1 AND BERBEST.ENR = MTRLDEP.ENR AND
         BERBEST.LEVDATUM = MTRLDEP.INVDATUM  NO-LOCK:
            IF BERBEST.UTTAG = TRUE THEN inventante = inventante + BERBEST.ANTAL.
            ELSE inventante = inventante - BERBEST.ANTAL.
         END.
         FOR EACH BESTDEP WHERE BESTDEP.DEPNR = 1 AND BESTDEP.ENR = MTRLDEP.ENR AND
         BESTDEP.LEVDATUM = MTRLDEP.INVDATUM AND BESTDEP.BERED = FALSE AND
         BESTDEP.LEVNAMN = "" NO-LOCK:
            inventante = inventante - BESTDEP.ANTAL.
         END.
         IF inventante NE mtrldepbuff.SALDO THEN DO:
            DISPLAY mtrldepbuff.ENR mtrldepbuff.SALDO inventant inventante MTRLDEP.INVDATUM MTRLDEP.INVANT TODAY datvar.            
            /*Borde skicka mail , men vet inte mailserver luleå*/
            /*IF forsta = TRUE THEN DO:
               forsta = FALSE.
               FIND FIRST eposttemp WHERE eposttemp.EPOST = tillvar AND 
               LENGTH(eposttemp.MEDD,"CHARACTER") < 30000
               NO-LOCK NO-ERROR.
               IF NOT AVAILABLE eposttemp THEN DO:
                  CREATE eposttemp.
                  eposttemp.EPOST = tillvar.
                  eposttemp.MEDD = STRING(TODAY) + " Depå check Luleå" + CHR(10).                                    
               END.
               eposttemp.MEDD = eposttemp.MEDD + mtrldepbuff.ENR + " " + STRING(mtrldepbuff.SALDO) + " " + STRING(inventant) + " " + STRING(inventante) 
               + " " + STRING(MTRLDEP.INVDATUM) + " " + STRING(MTRLDEP.INVANT) + " " + STRING(TODAY) + " " + STRING(datvar) + CHR(10).         
            END.
            ELSE DO:
               eposttemp.MEDD = eposttemp.MEDD + mtrldepbuff.ENR + " " + STRING(mtrldepbuff.SALDO) + " " + STRING(inventant) + " " + STRING(inventante) 
                  + " " + STRING(MTRLDEP.INVDATUM) + " " + STRING(MTRLDEP.INVANT) + " " + STRING(TODAY) + " " + STRING(datvar) + CHR(10).         
            END.            */
         END.
      END.
   END.
END.
OUTPUT CLOSE.


