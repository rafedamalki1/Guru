/*SUNDINSIKTTID.p*/

DEFINE NEW SHARED VARIABLE musz     AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE nytid    AS DECIMAL FORMAT "99.99" NO-UNDO.
DEFINE NEW SHARED VARIABLE sekunder AS INTEGER FORMAT "-9999999" NO-UNDO.
DEFINE            VARIABLE timtid   AS DECIMAL FORMAT "99.99" NO-UNDO. 
DEFINE QUERY tidq FOR TIDREGITAB.
DEFINE QUERY tidfq FOR TIDFEL.
 
{AMERICANEUROPEAN.I}  
DEFINE TEMP-TABLE insikttidtt NO-UNDO
   FIELD Projektnummer AS CHARACTER
   FIELD Timmar        AS DECIMAL
   INDEX Projektnummer Projektnummer.
DEFINE DATASET sundtidDS XML-NODE-NAME "BODY"  FOR insikttidtt.
DEFINE VARIABLE aonrvar AS CHARACTER NO-UNDO.

OPEN QUERY aq FOR EACH AONRTAB WHERE AONRTAB.AONRAVDATUM =  01/01/91 NO-LOCK.
RUN aonrxmltt_UI.
RUN aonrfelxmltt_UI.
OPEN QUERY aq FOR EACH AONRTAB WHERE YEAR(AONRTAB.AONRAVDATUM) =  YEAR(TODAY) NO-LOCK.
RUN aonrxmltt_UI.
RUN aonrfelxmltt_UI.
FOR EACH insikttidtt WHERE insikttidtt.Timmar = 0 NO-LOCK:
   DELETE insikttidtt. 
END.


FOR EACH insikttidtt NO-LOCK:
   IF insikttidtt.Projektnummer BEGINS "S" THEN DO: 
      aonrvar = insikttidtt.Projektnummer.
      insikttidtt.Projektnummer = "".                 
      insikttidtt.Projektnummer = SUBSTRING(aonrvar,2,LENGTH(aonrvar) - 1).
   END.
   insikttidtt.Timmar = ROUND(insikttidtt.Timmar,2). 
END.

RUN tidwxml_UI.


PROCEDURE aonrxmltt_UI :
   DEFINE VARIABLE aonrvar AS CHARACTER NO-UNDO.
   GET FIRST aq NO-LOCK.
   DO WHILE AVAILABLE(AONRTAB):   
      IF AONRTAB.OMRADE = "" THEN.
      ELSE DO:
         OPEN QUERY tidq FOR EACH TIDREGITAB WHERE TIDREGITAB.AONR = AONRTAB.AONR AND
         TIDREGITAB.VECKOKORD NE "" AND  TIDREGITAB.TIDLOG = TRUE NO-LOCK. 
         RUN tidxmltt_UI.
      END.   
      GET NEXT aq NO-LOCK.
   END.
END PROCEDURE. 

PROCEDURE aonrfelxmltt_UI :
   DEFINE VARIABLE aonrvar AS CHARACTER NO-UNDO.
   GET FIRST aq NO-LOCK.
   DO WHILE AVAILABLE(AONRTAB):   
      IF AONRTAB.OMRADE = "" THEN.
      ELSE DO:
         OPEN QUERY tidfq FOR EACH TIDFEL WHERE
         TIDFEL.AONR = AONRTAB.AONR AND
         TIDFEL.FELKORD NE "" AND  TIDFEL.TIDLOG = TRUE   NO-LOCK. 
         RUN tidfelxmltt_UI.
      END.   
      GET NEXT aq NO-LOCK.
   END.
END PROCEDURE. 



PROCEDURE tidxmltt_UI :
   GET FIRST tidq NO-LOCK.
   DO WHILE AVAILABLE(TIDREGITAB):
      IF TIDREGITAB.PRISTYP = "EJ.KOSTN." THEN.
      ELSE IF TIDREGITAB.PRISTYP = "FRÅNVARO." THEN.
      ELSE IF TIDREGITAB.AONR = "" THEN.
      ELSE DO:    
         FIND FIRST insikttidtt WHERE insikttidtt.Projektnummer = TIDREGITAB.AONR NO-LOCK NO-ERROR.
         IF NOT AVAILABLE insikttidtt THEN DO:
            CREATE insikttidtt.
            insikttidtt.Projektnummer = TIDREGITAB.AONR.             
         END.                                             
         IF TIDREGITAB.PRISTYP = 'RESTID...' AND TIDREGITAB.LONTILLAGG NE " " THEN. 
         ELSE DO:
            /*TIMMAR*/  
            nytid = TIDREGITAB.TOTALT.
            RUN TIMSEK.P.
            timtid = (sekunder / 3600).
            ASSIGN               
            insikttidtt.Timmar = insikttidtt.Timmar + timtid.              
         END.               
      END.
      GET NEXT tidq NO-LOCK.    
   END. 
           
END PROCEDURE.
   
PROCEDURE tidFelxmltt_UI :
   GET FIRST tidfq NO-LOCK.
   DO WHILE AVAILABLE(TIDFEL):
      IF TIDFEL.PRISTYP = "EJ.KOSTN." THEN.
      ELSE IF TIDFEL.PRISTYP = "FRÅNVARO." THEN.
      ELSE IF TIDFEL.AONR = "" THEN.
      ELSE DO:      
         FIND FIRST insikttidtt WHERE insikttidtt.Projektnummer = TIDFEL.AONR NO-LOCK NO-ERROR.
         IF NOT AVAILABLE insikttidtt THEN DO:
            CREATE insikttidtt.
            insikttidtt.Projektnummer = TIDFEL.AONR.             
         END.                                             
         IF TIDFEL.PRISTYP = 'RESTID...' AND TIDFEL.LONTILLAGG NE " " THEN. 
         ELSE DO:
            /*TIMMAR*/  
            nytid = TIDFEL.TOTALT.
            RUN TIMSEK.P.
            timtid = (sekunder / 3600).
            IF TIDFEL.DEBET = FALSE THEN timtid = -1 * timtid.
            ASSIGN               
            insikttidtt.Timmar = insikttidtt.Timmar + timtid.              
         END.               
      END.
      GET NEXT tidfq NO-LOCK.    
   END. 
           
END PROCEDURE.




{EUROPEANAMERICAN.I}
PROCEDURE tidwxml_UI :
   DEFINE VARIABLE cFile AS CHARACTER NO-UNDO.
   /*
   cFile = "d:\delad\SERVER\pro10s\EXPORT\InsiktTid" + STRING(TODAY,"999999") + ".xml". 
   */
   cFile = "d:\delad\SERVER\pro10s\EXPORT\InsiktTid.xml".
   DEFINE VARIABLE utfillong AS LONGCHAR NO-UNDO.   
   DATASET  sundtidDS:WRITE-XML("LONGCHAR", utfillong,FALSE,"iso8859-1").
   COPY-LOB FROM utfillong TO FILE cFile.
END PROCEDURE.