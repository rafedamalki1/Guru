/*SJINTY2.P*/

DEFINE VARIABLE stim AS DECIMAL NO-UNDO.
DEFINE VARIABLE dagnr AS INTEGER NO-UNDO. 
DEFINE VARIABLE stdatum AS DATE NO-UNDO.
DEFINE VARIABLE innandat AS DATE NO-UNDO.
DEFINE VARIABLE efterdat AS DATE NO-UNDO.
DEFINE VARIABLE hjdagar AS LOGICAL NO-UNDO.
DEFINE VARIABLE sdagar AS INTEGER NO-UNDO. 
DEFINE VARIABLE tidtabrec AS RECID NO-UNDO.
DEFINE VARIABLE granssj AS INTEGER NO-UNDO. 
DEFINE BUFFER tidbuff FOR TIDREGITAB.
DEFINE BUFFER persbuff FOR PERSONALTAB.

DEFINE INPUT PARAMETER pkod AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER hjdatum AS DATE NO-UNDO.

FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = pkod NO-LOCK NO-ERROR.
FIND FIRST ANSTFORMTAB WHERE ANSTFORMTAB.ANSTALLNING = PERSONALTAB.ANSTALLNING
USE-INDEX ANSTF NO-LOCK NO-ERROR.

FIND FIRST FORETAG NO-LOCK NO-ERROR.
IF FORETAG.FORETAG = "SUND" THEN granssj = 15.
IF FORETAG.FORETAG = "SNAT" THEN granssj = 15.
IF FORETAG.FORETAG = "MISV" THEN granssj = 8.
IF FORETAG.FORETAG = "ELPA" THEN granssj = 8.   

ASSIGN
sdagar = 1
stdatum = hjdatum
innandat = hjdatum
hjdatum = hjdatum - 1.
REPEAT:
   hjdagar = FALSE.
   RUN dag_UI.
   FIND PREV tidbuff WHERE tidbuff.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
   tidbuff.TIDLOG = TRUE AND tidbuff.datum = hjdatum AND
   tidbuff.AONR = "110"  NO-LOCK NO-ERROR.
   IF NOT AVAILABLE tidbuff THEN LEAVE.      
   sdagar = sdagar + 1.   
   IF sdagar = granssj THEN DO:
      ASSIGN
      hjdagar = FALSE
      innandat = hjdatum
      hjdatum = hjdatum - 1.
      RUN dag_UI.
      FIND PREV tidbuff WHERE tidbuff.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
      tidbuff.TIDLOG = TRUE AND tidbuff.datum = hjdatum AND
      tidbuff.AONR = "110"  NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidbuff THEN DO:         
         RUN medd_UI (INPUT innandat, INPUT stdatum )  .   
      END.
      LEAVE.
   END.   
   ELSE IF hjdagar = TRUE AND sdagar > granssj THEN DO:
      ASSIGN
      hjdagar = FALSE
      innandat = hjdatum
      hjdatum = hjdatum - 1.
      RUN dag_UI.
      FIND PREV tidbuff WHERE tidbuff.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
      tidbuff.TIDLOG = TRUE AND tidbuff.datum = hjdatum AND
      tidbuff.AONR = "110"  NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidbuff THEN DO:
         RUN medd_UI (INPUT innandat, INPUT stdatum ).   
      END.
      LEAVE.
   END.
   innandat = hjdatum.
   IF sdagar > granssj THEN LEAVE.
   hjdatum = hjdatum - 1.
END.

/* checka framåt att det inte ligger mer sjukdom framåt*/
IF sdagar < granssj  THEN DO:   
   ASSIGN
   efterdat = hjdatum
   hjdatum = stdatum + 1.
   REPEAT:
      hjdagar = FALSE.
      RUN dagpl_UI.
      FIND NEXT tidbuff WHERE tidbuff.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
      tidbuff.TIDLOG = TRUE AND tidbuff.datum = hjdatum AND
      tidbuff.AONR = "110"  NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidbuff THEN LEAVE.         
      sdagar = sdagar + 1.   
      IF sdagar = granssj THEN DO:
         ASSIGN
         hjdagar = FALSE
         efterdat = hjdatum.         
         RUN medd_UI (INPUT innandat, INPUT efterdat ) .            
         LEAVE.
      END.   
      ELSE IF hjdagar = TRUE AND sdagar > granssj THEN DO:
         ASSIGN
         hjdagar = FALSE
         efterdat = hjdatum.         
         RUN medd_UI (INPUT innandat, INPUT efterdat ).            
         LEAVE.
      END.
      IF sdagar > granssj THEN LEAVE.
      hjdatum = hjdatum + 1.
   END.
END.

PROCEDURE dag_UI.
   REPEAT:
      FIND FIRST OVERAVTAB WHERE OVERAVTAB.DATUM = hjdatum AND 
      OVERAVTAB.KOD = ANSTFORMTAB.KOD USE-INDEX ODATUM NO-LOCK NO-ERROR.
      IF NOT AVAILABLE OVERAVTAB THEN dagnr = WEEKDAY(hjdatum).
      ELSE dagnr = OVERAVTAB.EQDAG.
      IF dagnr = 1 OR dagnr = 7 THEN DO:
         ASSIGN
         hjdatum = hjdatum - 1
         sdagar = sdagar + 1.
         IF sdagar = granssj THEN ASSIGN hjdagar = TRUE.
      END.
      ELSE LEAVE.         
   END.   
END.
PROCEDURE dagpl_UI.
   REPEAT:
      FIND FIRST OVERAVTAB WHERE OVERAVTAB.DATUM = hjdatum AND 
      OVERAVTAB.KOD = ANSTFORMTAB.KOD USE-INDEX ODATUM NO-LOCK NO-ERROR.
      IF NOT AVAILABLE OVERAVTAB THEN dagnr = WEEKDAY(hjdatum).
      ELSE dagnr = OVERAVTAB.EQDAG.
      IF dagnr = 1 OR dagnr = 7 THEN DO:
         ASSIGN
         hjdatum = hjdatum + 1          
         sdagar = sdagar + 1.
         IF sdagar = granssj THEN ASSIGN hjdagar = TRUE.
      END.
      ELSE LEAVE.         
   END.   
END.
PROCEDURE medd_UI:  
   DEFINE INPUT PARAMETER stdatum AS DATE NO-UNDO.
   DEFINE INPUT PARAMETER sldatum AS DATE NO-UNDO. 
   FIND FIRST persbuff WHERE persbuff.PERSONALKOD = PERSONALTAB.TIDSGODK NO-LOCK NO-ERROR.
   IF AVAILABLE persbuff THEN DO:   
      FIND FIRST ANVANDARE WHERE ANVANDARE.PERSONALKOD = persbuff.PERSONALKOD 
      NO-LOCK NO-ERROR.
      IF NOT AVAILABLE ANVANDARE THEN RETURN.         
      CREATE MEDDELANDE.
      ASSIGN               
      MEDDELANDE.SANDARE = "Sjukintyg"
      MEDDELANDE.EMOTAGET = FALSE
      MEDDELANDE.SDATUM = TODAY
      MEDDELANDE.MEDD = PERSONALTAB.PERSONALKOD + " " + PERSONALTAB.FORNAMN + " " + PERSONALTAB.EFTERNAMN + " " + PERSONALTAB.PERSONNUMMER +  CHR(10) 
      MEDDELANDE.MOTTAGARE = ANVANDARE.ANVANDARE.      
      MEDDELANDE.MEDD = MEDDELANDE.MEDD + " " +  "har registrerat sjukdom från " + STRING(stdatum,("9999/99/99")) + " till " + STRING(sldatum,("9999/99/99")) +  CHR(10).   
   END.
END PROCEDURE.


   
