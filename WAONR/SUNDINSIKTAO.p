/*SUNDINSIKTAO.p*/
{NAMNDB.I}
{AMERICANEUROPEAN.I}
DEFINE TEMP-TABLE projtt NO-UNDO
   FIELD Projektnummer AS CHARACTER
   FIELD Projektnamn   AS CHARACTER
   FIELD Projektledare AS CHARACTER
   FIELD Verksamhet    AS CHARACTER
   INDEX Projektnummer Projektnummer.
EMPTY TEMP-TABLE projtt NO-ERROR. 
DEFINE VARIABLE aonrvar AS CHARACTER NO-UNDO.   
DEFINE DATASET sundaoDS XML-NODE-NAME "BODY"  FOR projtt.  
FIND FIRST FORETAG NO-LOCK NO-ERROR.
IF FORETAG.FORETAG = "SUND" THEN DO:
   IF  namndb() = "UTBI" THEN RETURN.
   ELSE DO:
      OPEN QUERY aq FOR EACH AONRTAB WHERE AONRTAB.AONRAVDATUM =  01/01/91 NO-LOCK.
      RUN aonrxmltt_UI.
      OPEN QUERY aq FOR EACH AONRTAB WHERE YEAR(AONRTAB.AONRAVDATUM) =  YEAR(TODAY) NO-LOCK.
      RUN aonrxmltt_UI.
      FOR EACH projtt NO-LOCK:
        IF projtt.Projektnummer BEGINS "S" THEN DO: 
            aonrvar = projtt.Projektnummer.
            projtt.Projektnummer = "".                 
            projtt.Projektnummer = SUBSTRING(aonrvar,2,LENGTH(aonrvar) - 1).
         END. 
      END.
      
      
      
      RUN aonrwxml_UI.
   END.   
END. 
{EUROPEANAMERICAN.I}


PROCEDURE aonrxmltt_UI :
   
   GET FIRST aq NO-LOCK.
   DO WHILE AVAILABLE(AONRTAB):   
      IF AONRTAB.OMRADE = "" THEN.
      ELSE DO:
         FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = AONRTAB.STARTDAG NO-LOCK NO-ERROR.
         FIND FIRST projtt WHERE projtt.Projektnummer = TIDREGITAB.AONR NO-LOCK NO-ERROR.
         IF NOT AVAILABLE projtt THEN DO:
            CREATE projtt.
            ASSIGN 
            projtt.Projektnummer = AONRTAB.AONR
            projtt.Projektnamn   = AONRTAB.ORT
            projtt.Verksamhet    = AONRTAB.OMRADE.
            IF AVAILABLE PERSONALTAB THEN projtt.Projektledare = PERSONALTAB.PERSONALKOD.
         END.   
         
         
      END.   
      GET NEXT aq NO-LOCK.
   END.
END PROCEDURE. 
PROCEDURE aonrwxml_UI :
   DEFINE VARIABLE cFile AS CHARACTER NO-UNDO.
   /*
   cFile = "d:\delad\SERVER\pro10s\EXPORT\InsiktAo" + STRING(TODAY,"999999") + ".xml". 
   */
   cFile = "d:\delad\SERVER\pro10s\EXPORT\InsiktAo.xml".
   DEFINE VARIABLE utfillong AS LONGCHAR NO-UNDO.   
   DATASET  sundaoDS:WRITE-XML("LONGCHAR", utfillong,FALSE,"iso8859-1").
   COPY-LOB FROM utfillong TO FILE cFile.
END PROCEDURE.

