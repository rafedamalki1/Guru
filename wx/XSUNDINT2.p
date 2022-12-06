/*XSUNDINT2.P*/   
/*DUBBELKOLL
  BEGÄR BOKDATUM I TRANS + VERKSAMHET
  2. KOLLA OM AONR VERIFNUMMER + FINNS MED TIDIGARE DATUM.
  3. OM JA STOPPA INLÄSNINGEN OCH VARNA.
  4. OM NEJ OK.
  
*/
DEFINE STREAM eko.  
DEFINE STREAM ekospar.
DEFINE NEW SHARED VARIABLE globforetag LIKE FORETAG.FORETAG NO-UNDO.    
DEFINE VARIABLE kommandoprog AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE tider AS CHARACTER NO-UNDO. 
DEFINE VARIABLE indate AS DATE NO-UNDO. 
DEFINE VARIABLE rad LIKE KOSTREG.RADNR NO-UNDO.
DEFINE VARIABLE prognamnvar AS CHARACTER FORMAT "X(50)" NO-UNDO.                
DEFINE VARIABLE progkopia AS CHARACTER FORMAT "X(50)" NO-UNDO.                
DEFINE VARIABLE progque AS CHARACTER FORMAT "X(50)" NO-UNDO.                
DEFINE VARIABLE filename AS CHARACTER NO-UNDO.
DEFINE VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE kommando AS CHARACTER FORMAT "X(132)" NO-UNDO.
DEFINE VARIABLE words AS CHARACTER FORMAT "X(132)" NO-UNDO.
DEFINE VARIABLE koant AS INTEGER NO-UNDO.
DEFINE VARIABLE rrakn AS INTEGER NO-UNDO.
DEFINE TEMP-TABLE slutut
   FIELD DEBKRED AS LOGICAL 
   FIELD OMRADE AS CHARACTER
   FIELD PROJEKT AS CHARACTER 
   FIELD DELNR LIKE AONRTAB.DELNR 
   FIELD VERDATUM AS CHARACTER
   FIELD KOSTNADSSLAG AS CHARACTER
   FIELD ANTAL AS DECIMAL 
   FIELD BELOPP AS DECIMAL       
   FIELD K1 AS CHARACTER 
   FIELD K2 AS CHARACTER
   FIELD K2POS8 AS CHARACTER
   INDEX ORG IS PRIMARY DEBKRED OMRADE PROJEKT KOSTNADSSLAG K1 K2.
DEFINE TEMP-TABLE tidin
   FIELD RAD AS INTEGER
   FIELD TIN AS CHARACTER FORMAT "X(132)"
   FIELD DATUM AS DATE
   INDEX DATUM IS PRIMARY DATUM
   INDEX RAD RAD.
DEFINE TEMP-TABLE supertidin LIKE tidin.
DEFINE TEMP-TABLE tidinkonto
   FIELD TIN AS CHARACTER FORMAT "X(132)"
   FIELD DATUM AS DATE
   INDEX DATUM IS PRIMARY DATUM.
DEFINE TEMP-TABLE infil
   FIELD PROGNAMN AS CHARACTER FORMAT "X(78)" 
   INDEX PRO IS PRIMARY PROGNAMN.
DEFINE TEMP-TABLE kostemp 
   FIELD ANVANDARE  LIKE KOSTREG.ANVANDARE 
   FIELD AONR       LIKE KOSTREG.AONR 
   FIELD BENAMNING  LIKE KOSTREG.BENAMNING 
   FIELD BETDATUM   LIKE KOSTREG.BETDATUM 
   FIELD BOKKONTO   LIKE KOSTREG.BOKKONTO 
   FIELD DELNR      LIKE KOSTREG.DELNR 
   FIELD FAKBES     LIKE KOSTREG.FAKBES 
   FIELD FAKTNR     LIKE KOSTREG.FAKTNR 
   FIELD FAKTURERAD LIKE KOSTREG.FAKTURERAD 
   FIELD INKOMST    LIKE KOSTREG.INKOMST 
   FIELD KOSTAUTO   LIKE KOSTREG.KOSTAUTO 
   FIELD LEVKOD     LIKE KOSTREG.LEVKOD 
   FIELD MASKKOST   LIKE KOSTREG.MASKKOST 
   FIELD MOMS       LIKE KOSTREG.MOMS 
   FIELD MTRL       LIKE KOSTREG.MTRL 
   FIELD OVRKR      LIKE KOSTREG.OVRKR 
   FIELD PERSKOST   LIKE KOSTREG.PERSKOST 
   FIELD RADNR      LIKE KOSTREG.RADNR 
   FIELD REGDATUM   LIKE KOSTREG.REGDATUM 
   FIELD TRAKTKOST  LIKE KOSTREG.TRAKTKOST
   INDEX KOST IS PRIMARY AONR DELNR RADNR.
   
FIND FIRST FORETAG USE-INDEX FORETAG NO-LOCK NO-ERROR.
globforetag = FORETAG.FORETAG.   
IF globforetag = "ELPA" THEN DO:            
   prognamnvar = "\\pc012\d\delad\PRO9\guru\import\GURUIN.TXT".
   progkopia = "\\pc012\d\delad\PRO9\guru\import\GN9PST" + STRING(TODAY,"999999").    
   progque = "\\pc012\d\delad\PRO9\guru\import\GN9PST.Q".
   kommandoprog ="\\pc012\d\delad\pro9\guru\import\guruin.txt".
   kommando = "DIR/a:-d /b \\pc012\d\delad\pro9\guru\import\GURU.* > \\pc012\d\delad\pro9\guru\import\GURUIN.TXT".   
END.
ELSE DO:            
   kommandoprog = "D:\DELAD\SERVER\PRO9S\import\GURUIN.TXT".
   kommando = "DIR/a:-d /b D:\DELAD\SERVER\PRO9S\import\GURU.* > D:\DELAD\SERVER\PRO9S\import\GURUIN.TXT".   
END.
OS-DELETE VALUE(kommandoprog) NO-ERROR.
OS-COMMAND SILENT VALUE(kommando).
INPUT FROM VALUE(kommandoprog) NO-ECHO.
REPEAT:
   DO TRANSACTION: 
      CREATE infil.
      ASSIGN.
      IMPORT infil   NO-ERROR.
   END.
END.
INPUT CLOSE.
FOR EACH infil:   
   IF INDEX(infil.PROGNAMN,"GURU.") = 0 THEN DO:       
      DELETE infil.
      NEXT.
   END.
END.
FOR EACH infil:  
   FOR EACH tidinkonto USE-INDEX DATUM:
      DELETE tidinkonto.
   END.
   FOR EACH tidin USE-INDEX DATUM:
      DELETE tidin.
   END. 
   FOR EACH slutut:
      DELETE slutut.
   END.
   FOR EACH kostemp:
      DELETE kostemp.
   END.
   tider = REPLACE(STRING(TIME,"HH:MM"),":","").
   IF globforetag = "ELPA" THEN DO:            
      progkopia = "\\pc012\d\delad\PRO9\guru\imKOPIA\" + infil.PROGNAMN + tider.          
      prognamnvar = "\\pc012\d\delad\PRO9\guru\import\" + infil.PROGNAMN.
      progque = "\\pc012\d\delad\PRO9\guru\import\GN9PST.Q".
   END.
   RUN in_UI.      
   OS-RENAME VALUE(prognamnvar) VALUE(progkopia).
   RUN ut_UI.
END.

PROCEDURE in_UI:
   FOR EACH tidin USE-INDEX DATUM:
      DELETE tidin.
   END.   
   OS-DELETE SILENT VALUE(progque).       
   kommando = SEARCH("quoter.exe").
   IF kommando = ? THEN RETURN.
   ELSE OS-COMMAND SILENT VALUE(kommando) VALUE(prognamnvar) > VALUE(progque).      
   INPUT FROM VALUE(progque) NO-ECHO.
   /*CONVERT TARGET "iso8859-1" SOURCE "ibm850".*/
   rrakn = 0.
   REPEAT:
      SET words VIEW-AS EDITOR INNER-CHARS 50 INNER-LINES 3 WITH FRAME DDD WIDTH 80.   
      REPEAT:
         IF INDEX(words,'"',1) = 0 THEN LEAVE.
         words = REPLACE(words,'"',' ').
      END.
      rrakn = rrakn + 1.
      CREATE tidin.   
      ASSIGN tidin.TIN = words
      tidin.RAD = rrakn.
   END.
   INPUT CLOSE.  
                                                                     
   FOR EACH tidin USE-INDEX DATUM:  
      /*DELNR OK*/
      rad = rad + 1.                                                                      
      FIND FIRST AONRTAB WHERE AONRTAB.AONR = SUBSTRING(tidin.TIN,1,5) AND 
      AONRTAB.DELNR = INTEGER(SUBSTRING(tidin.TIN,8,3))    
      USE-INDEX AONR NO-LOCK NO-ERROR.   
      indate = DATE(SUBSTRING(TIDIN.TIN,143,8)).       
      tidin.DATUM = indate.
      IF AVAILABLE AONRTAB THEN DO:
         FIND LAST KOSTREG WHERE KOSTREG.AONR = AONRTAB.AONR AND 
         KOSTREG.DELNR = AONRTAB.DELNR
         USE-INDEX KOST NO-LOCK NO-ERROR.  
         rad = 1.                                                                      
         IF AVAILABLE KOSTREG THEN rad = KOSTREG.RADNR + 1.     
         indate = DATE(SUBSTRING(TIDIN.TIN,143,8)).       
         tidin.DATUM = indate.
         CREATE KOSTREG.
         ASSIGN  
         KOSTREG.RADNR = rad
         KOSTREG.AONR = AONRTAB.AONR
         KOSTREG.DELNR = AONRTAB.DELNR
         KOSTREG.REGDATUM = indate       
         KOSTREG.BETDATUM = TODAY
         KOSTREG.BENAMNING = TRIM(SUBSTRING(tidin.TIN,80,35)) + " " + SUBSTRING(tidin.TIN,44,35)
         KOSTREG.BOKKONTO = SUBSTRING(tidin.TIN,37,6)
         KOSTREG.FAKTNR = SUBSTRING(tidin.TIN,30,6)
         KOSTREG.FAKTURERAD = ?
         KOSTREG.LEVKOD = ""
         SUBSTRING(KOSTREG.ANVANDARE,1,12) = "EKONOM"
         KOSTREG.KOSTAUTO = TRUE. 
         VALIDATE KOSTREG.
         /*
         3 intäkt
         40-46 mtrl
         47-49 frmtj
         5 övrigt
         6 övrigt    (saknades 18/2 )
         7 personal  - ändrat till övrigt 18/2 2002 av Lena
         8 tas ej med  - ändrat till övrigt  19/2 2002 av Lena
         90-92 tas ej med ändrat till övrigt  3/4 2002 av Anders
         93 intäkt
         94-99 övrigt  (saknades 18/2)
         */
         IF SUBSTRING(KOSTREG.BOKKONTO,1,1) = "3"OR
            SUBSTRING(KOSTREG.BOKKONTO,1,2) = "93" THEN DO:
            ASSIGN KOSTREG.INKOMST = DECIMAL(SUBSTRING(tidin.TIN,12,15)) * -1.
         END.  
         IF SUBSTRING(KOSTREG.BOKKONTO,1,2)= "40" OR 
            SUBSTRING(KOSTREG.BOKKONTO,1,2)= "41" OR
            SUBSTRING(KOSTREG.BOKKONTO,1,2)= "42" OR
            SUBSTRING(KOSTREG.BOKKONTO,1,2)= "43" OR
            SUBSTRING(KOSTREG.BOKKONTO,1,2)= "44" OR
            SUBSTRING(KOSTREG.BOKKONTO,1,2)= "45" OR
            SUBSTRING(KOSTREG.BOKKONTO,1,2)= "46" THEN DO:
              ASSIGN KOSTREG.MTRL = DECIMAL(SUBSTRING(tidin.TIN,12,15)). 
         END. 
         IF SUBSTRING(KOSTREG.BOKKONTO,1,2)= "47" OR 
            SUBSTRING(KOSTREG.BOKKONTO,1,2)= "48" OR
            SUBSTRING(KOSTREG.BOKKONTO,1,2)= "49" THEN DO:
            ASSIGN KOSTREG.MASKKOST = DECIMAL(SUBSTRING(tidin.TIN,12,15)). 
         END.
         IF SUBSTRING(KOSTREG.BOKKONTO,1,1)= "5" OR 
            SUBSTRING(KOSTREG.BOKKONTO,1,1)= "6" OR
            SUBSTRING(KOSTREG.BOKKONTO,1,1)= "7" OR
            SUBSTRING(KOSTREG.BOKKONTO,1,1)= "8" OR
            SUBSTRING(KOSTREG.BOKKONTO,1,2)= "90" OR
            SUBSTRING(KOSTREG.BOKKONTO,1,2)= "91" OR
            SUBSTRING(KOSTREG.BOKKONTO,1,2)= "92" OR
            SUBSTRING(KOSTREG.BOKKONTO,1,2)= "94" OR
            SUBSTRING(KOSTREG.BOKKONTO,1,2)= "95" OR
            SUBSTRING(KOSTREG.BOKKONTO,1,2)= "96" OR
            SUBSTRING(KOSTREG.BOKKONTO,1,2)= "97" OR
            SUBSTRING(KOSTREG.BOKKONTO,1,2)= "98" OR
            SUBSTRING(KOSTREG.BOKKONTO,1,2)= "99" THEN DO:
            ASSIGN KOSTREG.OVRKR = DECIMAL(SUBSTRING(tidin.TIN,12,15)). 
         END.
         /*Ändrat av Lena till övrigkostnad efter samtal med Ingrid 18/2 2002
         IF SUBSTRING(KOSTREG.BOKKONTO,1,1)= "7" THEN DO:      
            ASSIGN KOSTREG.PERSKOST = DECIMAL(SUBSTRING(tidin.TIN,12,15)).
         END.                                 */
      END.
   END.
END PROCEDURE.
PROCEDURE ut_UI :
   IF globforetag = "SUND" THEN DO:            
      ASSIGN
      prognamnvar = "D:\DELAD\SERVER\PRO9S\export\KOSTREG\GURUUT"
      progkopia = "D:\DELAD\SERVER\PRO9S\EXKOPIA\GURUUT" + STRING(TODAY,"99999999").
   END.
   IF globforetag = "elpa" THEN DO:            
      ASSIGN
      prognamnvar = "\\pc012\d\delad\PRO9\guru\export\KOSTREG\GURUUT"
      progkopia = "\\pc012\d\delad\PRO9\guru\EXKOPIA\GURUUT" + STRING(TODAY,"99999999").
   END.
   
   
   FOR EACH tidin USE-INDEX DATUM:   
      FIND FIRST AONRTAB WHERE AONRTAB.AONR = SUBSTRING(tidin.TIN,1,5) AND 
      AONRTAB.DELNR = INTEGER(SUBSTRING(tidin.TIN,8,3))    
      USE-INDEX AONR NO-LOCK NO-ERROR.   
      IF AVAILABLE AONRTAB THEN DO:
         RUN sut_UI.     
      END.
      ELSE DO:
         RUN sut_UI.
         RUN motkont_UI.
         DELETE tidin.
      END.      
   END.
   FOR EACH tidin USE-INDEX DATUM:
      RUN konto_UI.
   END.
   FOR EACH tidinkonto USE-INDEX DATUM:   
      RUN skut_UI.     
   END.
   OUTPUT STREAM eko TO VALUE(prognamnvar) APPEND.
   PUT STREAM eko UNFORMATTED "GURU "  TODAY SKIP.
   OUTPUT STREAM ekospar TO VALUE(progkopia) APPEND.
   FOR EACH supertidin:
      PUT STREAM eko UNFORMATTED
      supertidin.TIN AT 1 SKIP.
      PUT STREAM ekospar UNFORMATTED
      supertidin.TIN AT 1 SKIP.
   END.
   OUTPUT STREAM eko CLOSE. 
   OUTPUT STREAM ekospar CLOSE.
END PROCEDURE.
PROCEDURE motkont_UI:
   /*post ut med -tecken för aonr som ej finns i guru*/
   DEFINE VARIABLE nypris AS DECIMAL NO-UNDO.
   DEFINE VARIABLE nyantal AS DECIMAL NO-UNDO.
   ASSIGN
   nyantal = -1 * DECIMAL(SUBSTRING(tidin.TIN,127,15))
   nypris = -1 * DECIMAL(SUBSTRING(tidin.TIN,12,15)).
   ASSIGN
   SUBSTRING(tidin.TIN,1,5) = "99999"
   SUBSTRING(tidin.TIN,12,15) = STRING(nypris,"->>>>>>>>>>9.99")
   SUBSTRING(tidin.TIN,127,15) = STRING(nyantal,"->>>>>>>>>>9.99").
   CREATE supertidin.
   BUFFER-COPY tidin TO supertidin.          
END PROCEDURE.
PROCEDURE sut_UI:
   /*post ut med -tecken*/
   DEFINE VARIABLE nypris AS DECIMAL NO-UNDO.
   nypris = -1 * DECIMAL(SUBSTRING(tidin.TIN,12,15)).
   SUBSTRING(tidin.TIN,12,15) = STRING(nypris,"->>>>>>>>>>9.99").
   CREATE supertidin.
   BUFFER-COPY tidin TO supertidin.     
      
END PROCEDURE.
PROCEDURE skut_UI:
   /*kontofördelning ut*/
   CREATE supertidin.
   BUFFER-COPY tidinkonto TO supertidin.  
END PROCEDURE.

PROCEDURE konto_UI:
   /*kontofördelning*/
   DEFINE VARIABLE nypris AS DECIMAL NO-UNDO.
   DEFINE VARIABLE kontonr AS INTEGER NO-UNDO.
   DEFINE VARIABLE kollpris AS DECIMAL NO-UNDO.
   FOR EACH slutut:
      DELETE slutut.
   END.   
   koant = 0.
   nypris = -1 * DECIMAL(SUBSTRING(tidin.TIN,12,15)).
   OPEN QUERY qa FOR EACH AONRKONTKOD WHERE AONRKONTKOD.AONR = SUBSTRING(tidin.TIN,1,5) AND 
   AONRKONTKOD.DELNR = INTEGER(SUBSTRING(tidin.TIN,8,3)) NO-LOCK.
   GET FIRST qa NO-LOCK.
   IF NOT AVAILABLE AONRKONTKOD  THEN RETURN.
   DO WHILE AVAILABLE(AONRKONTKOD):
      CREATE slutut.
      ASSIGN  
      slutut.PROJEKT = AONRKONTKOD.AONR           
      slutut.DELNR = AONRKONTKOD.DELNR      
      slutut.K1 = AONRKONTKOD.K1               
      slutut.K2 = AONRKONTKOD.K2                       
      slutut.BELOPP = ROUND((nypris * AONRKONTKOD.SATS%) / 100,2).                     
      koant = koant + 1.
      GET NEXT qa NO-LOCK.
   END.
   FOR EACH slutut:
      kollpris = kollpris + slutut.BELOPP.
   END.
   IF nypris NE kollpris THEN DO:
      FIND LAST slutut NO-ERROR.
      slutut.BELOPP = slutut.BELOPP + (nypris - kollpris). 
   END.
   IF koant = 1 THEN DO:
      FIND FIRST supertidin WHERE supertidin.DATUM = tidin.DATUM AND supertidin.RAD = tidin.RAD NO-ERROR.       
      IF AVAILABLE supertidin THEN DO:
         FIND LAST slutut NO-ERROR.
         IF SUBSTRING(supertidin.TIN,116,4) = slutut.K1 AND 
         SUBSTRING(supertidin.TIN,152,8) = slutut.K2 THEN DO:                    
            DELETE supertidin.
            DELETE tidin.
            FOR EACH slutut:                                         
               DELETE slutut.
            END.   
         END.
      END.
   END.     
   FOR EACH slutut:                                         
      IF LENGTH(slutut.K2) = 1 THEN slutut.K2 = slutut.K2 + "0000000".
      ELSE IF LENGTH(slutut.K2) = 2 THEN slutut.K2 = slutut.K2 + "000000".
      ELSE IF LENGTH(slutut.K2) = 3 THEN slutut.K2 = slutut.K2 + "00000".
      ELSE IF LENGTH(slutut.K2) = 4 THEN slutut.K2 = slutut.K2 + "0000".
      ELSE IF LENGTH(slutut.K2) = 5 THEN slutut.K2 = slutut.K2 + "000".
      ELSE IF LENGTH(slutut.K2) = 6 THEN slutut.K2 = slutut.K2 + "00".
      ELSE IF LENGTH(slutut.K2) = 7 THEN slutut.K2 = slutut.K2 + "0".      
      CREATE tidinkonto.
      ASSIGN
      tidinkonto.DATUM = tidin.DATUM
      tidinkonto.TIN = tidin.TIN.
      ASSIGN
      SUBSTRING(tidinkonto.TIN,12,15) = STRING(slutut.BELOPP,"->>>>>>>>>>9.99")
      SUBSTRING(tidinkonto.TIN,116,4) = slutut.K1
      SUBSTRING(tidinkonto.TIN,152,8) = slutut.K2.
      DELETE slutut.
   END.
END PROCEDURE.



