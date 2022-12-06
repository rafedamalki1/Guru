/*AUTOAOSU.P 
via AUREG.P

*/

&Scoped-define NEW NEW
{GLOBVAR2DEL1.I}
DEFINE BUFFER medbuff FOR MEDDELANDE.
FIND FIRST FORETAG NO-LOCK NO-ERROR.
DEFINE VARIABLE projrapp AS CHARACTER NO-UNDO. 
DEFINE VARIABLE stdatum AS DATE NO-UNDO.
FIND FIRST FORETAG WHERE NO-LOCK NO-ERROR.
Guru.Konstanter:globforetag = FORETAG.FORETAG. 

{NAMNDB.I}
{AMERICANEUROPEAN.I}
DEFINE TEMP-TABLE projtt NO-UNDO
   FIELD Projektnummer AS CHARACTER
   FIELD Projektnamn   AS CHARACTER
   FIELD Projektledare AS CHARACTER
   FIELD Verksamhet    AS CHARACTER
   INDEX Projektnummer Projektnummer.
EMPTY TEMP-TABLE projtt NO-ERROR.    
DEFINE DATASET sundaoDS XML-NODE-NAME "BODY"  FOR projtt.   
RUN STYRFORE.P (INPUT FORETAG.FORETAG).

FIND FIRST INTERNFAKTKOLL WHERE YEAR(INTERNFAKTKOLL.VDATUM) = YEAR(TODAY) AND
MONTH(INTERNFAKTKOLL.VDATUM) = MONTH(TODAY) NO-LOCK NO-ERROR.
IF AVAILABLE INTERNFAKTKOLL THEN RETURN.

DO TRANSACTION:
   CREATE INTERNFAKTKOLL.
   ASSIGN   
   INTERNFAKTKOLL.VECKOKORD = "AVSLUTKOLL"
   INTERNFAKTKOLL.VDATUM = TODAY.    
END.
OPEN QUERY aoq FOR EACH AONRTAB WHERE AONRTAB.AUTOREG = TRUE 
USE-INDEX AONR NO-LOCK.
GET FIRST aoq NO-LOCK.    
DO WHILE AVAILABLE(AONRTAB):
   DO TRANSACTION:
      GET CURRENT aoq EXCLUSIVE-LOCK NO-WAIT.
      IF LOCKED(AONRTAB) = FALSE THEN DO: 
         /*DELNR*/
         IF FORETAG.FORETAG = "SNAT" THEN  RUN aonrav_UI.
         
         ELSE IF AONRTAB.DELNR = 0 THEN RUN aonrav_UI.
         
         ASSIGN AONRTAB.AUTOREG = FALSE.
            
      END.
   END.
   GET NEXT aoq NO-LOCK.     
END.


IF FORETAG.FORETAG = "SNAT" THEN DO:   
   OPEN QUERY aq FOR EACH AONRTAB WHERE AONRTAB.AONRAVDATUM >= TODAY - 660 AND
   AONRTAB.AONRAVDATUM <= TODAY - 630 NO-LOCK.
   GET FIRST aq NO-LOCK.
   DO WHILE AVAILABLE(AONRTAB):   
      RUN medd_UI.
      GET NEXT aq NO-LOCK.
   END.
END.  
{EUROPEANAMERICAN.I}



PROCEDURE aonrav_UI :
   DEFINE VARIABLE aonrvar AS CHARACTER NO-UNDO.
   DEFINE VARIABLE delnrvar AS INTEGER NO-UNDO.
   DEFINE VARIABLE avdatchar AS CHARACTER NO-UNDO.
   DEFINE VARIABLE avdatvar AS DATE NO-UNDO.
   ASSIGN
   aonrvar = AONRTAB.AONR
   delnrvar = AONRTAB.DELNR.
   /*ÄNDRAT 2008-02-19
   IF AONRTAB.AONRAVDATUM = 01/01/91 THEN avdatchar = "".
   ELSE avdatchar = STRING(AONRTAB.AONRAVDATUM  + 60,"9999/99/99").
   IF FORETAG.FORETAG = "SUND" OR FORETAG.FORETAG = "SNAT" THEN DO:
      IF AONRTAB.AONRAVDATUM NE 01/01/91 THEN DO:
         IF YEAR(AONRTAB.AONRAVDATUM) < YEAR(TODAY) THEN.
         ELSE DO:         
            avdatvar = DATE(12,31,YEAR(AONRTAB.AONRAVDATUM)). 
            avdatchar = STRING(avdatvar,"9999/99/99").
         END.
      END.
   END.
   */
   IF  namndb() = "UTBI" THEN RETURN.
   IF AONRTAB.AONRAVDATUM = 01/01/91 THEN avdatchar = "".
   ELSE DO:
      avdatvar = AONRTAB.AONRAVDATUM  + 60.
      IF MONTH(avdatvar) = 12 THEN  avdatvar = DATE(MONTH(avdatvar),31,YEAR(avdatvar)).
      ELSE avdatvar = DATE(MONTH(avdatvar) + 1,01,YEAR(avdatvar)) - 1.       
      avdatchar = STRING(avdatvar,"9999/99/99").
   END.
   IF AONRTAB.OMRADE NE "" THEN DO:
      FIND FIRST OMRADETAB WHERE OMRADETAB.OMRADE = AONRTAB.OMRADE NO-LOCK NO-ERROR.
      FIND FIRST AVDELNING WHERE AVDELNING.AVDELNINGNR = OMRADETAB.AVDELNINGNR NO-LOCK NO-ERROR.
      FIND FIRST JURPERS WHERE JURPERS.JUDID = AVDELNING.POSTANST NO-LOCK NO-ERROR.
      FIND FIRST AONRTIDLAGE WHERE AONRTIDLAGE.AONR = AONRTAB.AONR AND AONRTIDLAGE.DELNR = AONRTAB.DELNR AND
      AONRTIDLAGE.IDTIDLAG = "AOUPPLAGT" NO-LOCK NO-ERROR.
      stdatum = ?.
      IF AVAILABLE AONRTIDLAGE THEN DO:
         stdatum = AONRTIDLAGE.DATUM1.         
      END.
      IF stdatum = ? THEN DO:
         FIND FIRST TIDREGITAB WHERE TIDREGITAB.AONR = AONRTAB.AONR AND TIDREGITAB.DELNR = AONRTAB.DELNR 
         USE-INDEX AONR NO-LOCK NO-ERROR.
         IF AVAILABLE TIDREGITAB THEN DO:
            stdatum = TIDREGITAB.DATUM.
         END.
         ELSE stdatum = TODAY.
      END.
      
      IF JURPERS.JUDID = "ELNÄT" THEN DO:
         IF LENGTH(AONRTAB.AONR) = 4 THEN aonrvar = aonrvar + "0".
         IF Guru.Konstanter:globforetag = "SNAT" THEN DO:
            /*SNATBERGET*/
            OUTPUT TO d:\delad\pro10s\EXPORT\nyproj.txt APPEND.
         END.
         ELSE OUTPUT TO d:\delad\SERVER\pro10s\EXPORT\nyproj.txt APPEND.
         ASSIGN projrapp = "".
         ASSIGN
         SUBSTRING(projrapp,1,5) = aonrvar
         SUBSTRING(projrapp,7,30) = SUBSTRING(AONRTAB.ORT,1,30)
         SUBSTRING(projrapp,38,10) = STRING(stdatum,"9999/99/99")
         SUBSTRING(projrapp,49,10) = avdatchar.
         SUBSTRING(projrapp,60,3) = STRING(AONRTAB.DELNR,"999").
         PUT UNFORMATTED projrapp.
         PUT SKIP.         
         OUTPUT CLOSE.
         /*os appnend*/
         IF Guru.Konstanter:globforetag = "SNAT" THEN DO:
            /*SNATBERGET*/
             OUTPUT TO d:\delad\pro10s\EXPORT\allanyproj.txt APPEND.
         END.
         ELSE  OUTPUT TO d:\delad\SERVER\pro10s\EXPORT\allanyproj.txt APPEND.               
         PUT UNFORMATTED projrapp.
         PUT SKIP.
         OUTPUT CLOSE.
      END.
      IF JURPERS.JUDID = "SEAB" THEN DO:
         IF LENGTH(AONRTAB.AONR) = 4 THEN aonrvar = aonrvar + "0".
         OUTPUT TO d:\delad\SERVER\pro10s\EXPORT\nyprojSEAB.txt APPEND.
         ASSIGN projrapp = "".
         IF aonrvar BEGINS "S" THEN DO:                  
            SUBSTRING(projrapp,1,5) = SUBSTRING(aonrvar,2,5).
         END.
         ELSE DO:
            SUBSTRING(projrapp,1,5) = aonrvar.
         END.
         ASSIGN
         SUBSTRING(projrapp,7,30) = SUBSTRING(AONRTAB.ORT,1,30)
         SUBSTRING(projrapp,38,10) = STRING(stdatum,"9999/99/99")
         SUBSTRING(projrapp,49,10) = avdatchar.
         PUT UNFORMATTED projrapp.
         PUT SKIP.         
         OUTPUT CLOSE.
         /*os appnend*/
         OUTPUT TO d:\delad\SERVER\pro10s\EXPORT\allanyprojSEAB.txt APPEND.               
         PUT UNFORMATTED projrapp.
         PUT SKIP.
         OUTPUT CLOSE.
      END.     
      IF JURPERS.JUDID = "ServaNet" THEN DO:
         IF LENGTH(AONRTAB.AONR) = 4 THEN aonrvar = aonrvar + "0".
         IF Guru.Konstanter:globforetag = "SNAT" THEN DO:
            /*SNATBERGET*/
             OUTPUT TO d:\delad\pro10s\EXPORT\nyprojSN.txt APPEND.
         END.
         ELSE  OUTPUT TO d:\delad\SERVER\pro10s\EXPORT\nyprojSN.txt APPEND.
         
         ASSIGN projrapp = "".
         ASSIGN
         SUBSTRING(projrapp,1,5) = aonrvar
         SUBSTRING(projrapp,7,30) = SUBSTRING(AONRTAB.ORT,1,30)
         SUBSTRING(projrapp,38,10) = STRING(stdatum,"9999/99/99")
         SUBSTRING(projrapp,49,10) = avdatchar.
         SUBSTRING(projrapp,60,3) = STRING(AONRTAB.DELNR,"999").
         PUT UNFORMATTED projrapp.
         PUT SKIP.         
         OUTPUT CLOSE.
         /*os appnend*/
         IF Guru.Konstanter:globforetag = "SNAT" THEN DO:
            /*SNATBERGET*/
             OUTPUT TO d:\delad\pro10s\EXPORT\allanyprojSN.txt APPEND.
         END.
         ELSE  OUTPUT TO d:\delad\SERVER\pro10s\EXPORT\allanyprojSN.txt APPEND.
         PUT UNFORMATTED projrapp.
         PUT SKIP.
         OUTPUT CLOSE.
      END.
      IF JURPERS.JUDID = "REKO" THEN DO:
         IF LENGTH(AONRTAB.AONR) = 4 THEN aonrvar = aonrvar + "0".
         IF Guru.Konstanter:globforetag = "SNAT" THEN DO:
            /*SNATBERGET*/
             OUTPUT TO d:\delad\pro10s\EXPORT\nyprojREKO.txt APPEND.
         END.
         ELSE  OUTPUT TO d:\delad\SERVER\pro10s\EXPORT\nyprojREKO.txt APPEND.
         ASSIGN projrapp = "".
         ASSIGN
         SUBSTRING(projrapp,1,5) = aonrvar
         SUBSTRING(projrapp,7,30) = SUBSTRING(AONRTAB.ORT,1,30)
         SUBSTRING(projrapp,38,10) = STRING(stdatum,"9999/99/99")
         SUBSTRING(projrapp,49,10) = avdatchar.
         PUT UNFORMATTED projrapp.
         PUT SKIP.         
         OUTPUT CLOSE.
         /*os appnend*/
         IF Guru.Konstanter:globforetag = "SNAT" THEN DO:
            /*SNATBERGET*/
             OUTPUT TO d:\delad\pro10s\EXPORT\allanyprojREKO.txt APPEND.
         END.
         ELSE  OUTPUT TO d:\delad\SERVER\pro10s\EXPORT\allanyprojREKO.txt APPEND.
         PUT UNFORMATTED projrapp.
         PUT SKIP.
         OUTPUT CLOSE.
      END.
   END.
END PROCEDURE.
PROCEDURE medd_UI:  
   FIND FIRST ANVANDARE WHERE ANVANDARE.PERSONALKOD = AONRTAB.ARBANSVARIG 
   NO-LOCK NO-ERROR.
   IF NOT AVAILABLE ANVANDARE THEN DO:
      FIND FIRST ANVANDARE WHERE ANVANDARE.ANVANDARE = "SEKAN" 
      NO-LOCK NO-ERROR.
      IF NOT AVAILABLE ANVANDARE THEN RETURN.
   END.
   FIND FIRST MEDDELANDE WHERE MEDDELANDE.SANDARE = "SLUTBESIKTNING" AND 
   MEDDELANDE.MOTTAGARE = ANVANDARE.ANVANDARE AND LENGTH(MEDDELANDE.MEDD,"CHARACTER") < 30000 
   EXCLUSIVE-LOCK NO-ERROR.
   IF NOT AVAILABLE MEDDELANDE THEN DO:
      CREATE MEDDELANDE.
      ASSIGN               
      MEDDELANDE.SANDARE = "SLUTBESIKTNING"
      MEDDELANDE.EMOTAGET = FALSE
      MEDDELANDE.SDATUM = TODAY
      MEDDELANDE.MEDD = CAPS(Guru.Konstanter:gaok) + " FÖR SLUT BESIKTNING" + CHR(10) 
      MEDDELANDE.MOTTAGARE = ANVANDARE.ANVANDARE.
   END.
   MEDDELANDE.MED = MEDDELANDE.MED + AONRTAB.AONR + " " +  
   STRING(DELNR,Guru.Konstanter:varforetypchar[1]) + " " + AONRTAB.ORT +  CHR(10).   
END PROCEDURE.
