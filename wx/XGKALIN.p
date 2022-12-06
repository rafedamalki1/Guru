/*XGKALIN.P*/   
/*Totalutläsning*/
/*
SMÅLAND
KONTO "68400"  TAS EJ MED.
KONTO SOM BÖRJAR PÅ 3 ÄR INTÄKT
KONTO >= "44000" AND KONTO <= "46000" ÄR MATERIAL
KONTO SOM BÖRJAR PÅ  5 ÄR ÖVRIGT
KONTO >= "60000" AND KONTO <= "64900" ÄR ÖVRIGT
KONTO >= "65000" AND KONTO <= "66190" ÄR ÖVRIGT OBS! Jag har ingen post för tjänst. Ett alternati kan vara 
                                                     Personalkostnader.    
KONTO >= "69100" AND KONTO <= "69999" ÄR ÖVRIGT
KONTO >= "73200" AND KONTO <= "76990" ÄR ÖVRIGT
Allt annat ÄR ÖVRIGT. 

KALMAR
KONTO "65922"  TAS EJ MED.
KONTO "68300"  TAS EJ MED.
KONTO SOM BÖRJAR PÅ 3 ÄR INTÄKT
KONTO SOM BÖRJAR PÅ 4 ÄR MATRIEL
KONTO SOM BÖRJAR PÅ 5 ÄR ÖVRIGT
KONTO >= "60000" AND KONTO <= "65800" ÄR ÖVRIGT OBS! Jag har ingen post för tjänst. Ett alternati kan vara 
                                                     Personalkostnader.    
KONTO >= "65900" AND KONTO <= "65915" ÄR ÖVRIGT
KONTO >= "65917" AND KONTO <= "65921" ÄR ÖVRIGT OBS! Jag har ingen post för tjänst. Ett alternati kan vara 
                                                     Personalkostnader.    
KONTO >= "65923" AND KONTO <= "66190" ÄR ÖVRIGT OBS! Jag har ingen post för tjänst. Ett alternati kan vara 
                                                     Personalkostnader.    
KONTO >= "69100" AND KONTO <= "69999" ÄR ÖVRIGT OBS! Jag har ingen post för tjänst. Ett alternati kan vara 
                                                     Personalkostnader.    
KONTO >= "73200" AND KONTO <= "76990" ÄR ÖVRIGT
KONTO >= "97041" ÄR MATRIEL
KONTO >= "97461" ÄR MATRIEL
Allt annat ÄR ÖVRIGT. 

*/
DEFINE NEW SHARED VARIABLE globforetag LIKE FORETAG.FORETAG NO-UNDO.    
DEFINE VARIABLE tider AS CHARACTER NO-UNDO. 
DEFINE VARIABLE indate AS DATE NO-UNDO. 
DEFINE VARIABLE rad LIKE KOSTREG.RADNR NO-UNDO.
DEFINE VARIABLE iaonrvar AS INTEGER NO-UNDO.
DEFINE VARIABLE prognamn AS CHARACTER FORMAT "X(50)" NO-UNDO.                
DEFINE VARIABLE prognamnvar AS CHARACTER FORMAT "X(50)" NO-UNDO.                
DEFINE VARIABLE progkopia AS CHARACTER FORMAT "X(50)" NO-UNDO.                
DEFINE VARIABLE utprogkopia AS CHARACTER FORMAT "X(50)" NO-UNDO.                
DEFINE VARIABLE progque AS CHARACTER FORMAT "X(50)" NO-UNDO.                
DEFINE VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE kommando AS CHARACTER FORMAT "X(132)" NO-UNDO.
DEFINE VARIABLE words AS CHARACTER FORMAT "X(132)" NO-UNDO.
DEFINE VARIABLE filnamn AS CHARACTER FORMAT "x(78)" LABEL "File" NO-UNDO.
DEFINE VARIABLE attrlist AS CHARACTER FORMAT "x(6)" LABEL "Attributes" NO-UNDO.
DEFINE VARIABLE dirlist AS CHARACTER FORMAT "x(78)" LABEL "Directory" NO-UNDO.
DEFINE TEMP-TABLE tidin
   FIELD TIN AS CHARACTER FORMAT "X(132)".  
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
IF globforetag = "GKAL" THEN DO:            
   /*GKAL
   SYD OCH NORD
   */
   tider = REPLACE(STRING(TIME,"HH:MM"),":","").             
   
   prognamn = "\\granguru\guru_ser\server\PRO9S\import\".
   progkopia = "\\granguru\guru_ser\server\PRO9S\import\backineko\".
   /*
   prognamn = "D:\DELAD\server\PRO9S\import\".
   progkopia = "D:\DELAD\server\PRO9S\import\backineko\".    
   */
   progque = prognamn + "GN6M7.Q".
END.
IF globforetag = "ELPA" THEN DO:            
   prognamn = "D:\DELAD\PRO9\guru\import\".
   progkopia = "D:\DELAD\PRO9\guru\import\backineko\".       
   progque = "D:\DELAD\PRO9\guru\import\GN6M7.Q".
END.
musz = TRUE.
INPUT FROM OS-DIR(prognamn) NO-ECHO.
REPEAT:
   /*Hämtar filnamn, hela sökvägen och vilken typ av fil det är*/
   SET filnamn dirlist attrlist.
   IF filnamn = "GN6PST.txt" THEN DO:
      musz = FALSE.
      prognamnvar = dirlist. 
      RUN startin_UI.
      RUN kalmar_UI.
   END.
   IF filnamn = "GM7PST.txt" THEN DO:
      musz = FALSE.
      prognamnvar = dirlist. 
      RUN startin_UI.
      RUN smaland_UI.
   END.        
END.
IF globforetag = "GKAL" THEN DO:            
   IF musz = FALSE THEN DO:  
      OUTPUT TO \\192.121.248.232\guru_ser\server\PRO9S\autotid.txt  APPEND.
      PUT "DET FANNS FIL FRÅN EKO " globforetag " " TODAY " " STRING(TIME,"HH:MM:SS") SKIP.
      OUTPUT CLOSE.   
   END. 
   IF musz = TRUE THEN DO:
      OUTPUT TO \\192.121.248.232\guru_ser\server\PRO9S\autotid.txt  APPEND.
      PUT "DET FANNS INGEN FIL FRÅN EKO " globforetag " " TODAY " " STRING(TIME,"HH:MM:SS") SKIP.
      OUTPUT CLOSE.
      musz = FALSE.
      RETURN.
   END.
END.
PROCEDURE startin_UI:
   FOR EACH tidin:
      DELETE tidin.
   END.
   OS-DELETE SILENT VALUE(progque).       
   kommando = SEARCH("quoter.exe").
   IF kommando = ? THEN RETURN.
   ELSE OS-COMMAND SILENT VALUE(kommando) VALUE(prognamnvar) > VALUE(progque).      
   INPUT FROM VALUE(progque) NO-ECHO CONVERT TARGET "iso8859-1" SOURCE "ibm850".
   REPEAT:
      SET words VIEW-AS EDITOR INNER-CHARS 50 INNER-LINES 3 WITH FRAME DDD WIDTH 80.   
      REPEAT:
         IF INDEX(words,'"',1) = 0 THEN LEAVE.
         words = REPLACE(words,'"',' ').
      END.
      CREATE tidin.   
      ASSIGN tidin.TIN = words.   
   END.
   INPUT CLOSE.     
   FOR EACH tidin:   
      IF SUBSTRING(TIDIN.TIN,1,1) = "S" THEN DELETE tidin.
      ELSE IF SUBSTRING(TIDIN.TIN,3,1) = "7" THEN DELETE tidin.
      ELSE IF SUBSTRING(TIDIN.TIN,3,1) = "9" THEN DELETE tidin.
   END.
END PROCEDURE.
PROCEDURE smaland_UI.
   FOR EACH tidin:   
      IF SUBSTRING(tidin.TIN,3,5) = "68400" THEN DO:
         DELETE tidin.
         NEXT.
      END.
      IF SUBSTRING(tidin.TIN,15,6) = "" THEN NEXT. /*AONR*/
      rad = rad + 1.                                                                      
      RUN aokoll_UI.     
      IF AVAILABLE AONRTAB THEN DO:
         FIND LAST KOSTREG WHERE KOSTREG.AONR = AONRTAB.AONR AND 
         KOSTREG.DELNR = AONRTAB.DELNR
         USE-INDEX KOST NO-LOCK NO-ERROR.  
         rad = 1.                                                                      
         IF AVAILABLE KOSTREG THEN rad = KOSTREG.RADNR + 1. 
         indate = DATE(INTEGER(SUBSTRING(tidin.TIN,72,2)),01,INTEGER(SUBSTRING(tidin.TIN,68,4))).
         IF MONTH(indate) = 12 THEN DO:
            indate = DATE(12,31,YEAR(indate)).
         END.
         ELSE DO:   
            indate = DATE((MONTH(indate) + 1),01,YEAR(indate)) - 1.
         END.
         /* ej vid totalåterläsning*/
         /*IF indate <= TODAY - 35 THEN indate = TODAY.*/
         CREATE KOSTREG.
         ASSIGN  
         KOSTREG.RADNR = rad
         KOSTREG.AONR = AONRTAB.AONR
         KOSTREG.DELNR = AONRTAB.DELNR
         KOSTREG.REGDATUM = TODAY       
         KOSTREG.REGDATUM = indate 
         KOSTREG.BETDATUM = TODAY
         KOSTREG.BENAMNING = SUBSTRING(tidin.TIN,86,19) 
         KOSTREG.BOKKONTO = SUBSTRING(tidin.TIN,3,5)
         KOSTREG.FAKTNR = SUBSTRING(tidin.TIN,37,8)
         KOSTREG.FAKTURERAD = ?
         KOSTREG.LEVKOD = ""
         SUBSTRING(KOSTREG.ANVANDARE,1,12) = "EKONOM"
         KOSTREG.KOSTAUTO = TRUE. 
         VALIDATE KOSTREG.
         IF SUBSTRING(KOSTREG.BOKKONTO,1,1) = "3" THEN DO:
            ASSIGN KOSTREG.INKOMST = DECIMAL(SUBSTRING(tidin.TIN,143,20)) * -1 / 1000.
         END.  
         ELSE IF KOSTREG.BOKKONTO >= "44000" AND KOSTREG.BOKKONTO <= "46000" THEN DO:
            ASSIGN KOSTREG.MTRL = DECIMAL(SUBSTRING(tidin.TIN,143,20)) / 1000. 
         END. 
         ELSE IF SUBSTRING(KOSTREG.BOKKONTO,1,1) = "5" THEN DO:
            ASSIGN KOSTREG.OVRKR = DECIMAL(SUBSTRING(tidin.TIN,143,20)) / 1000. 
         END. 
         ELSE IF KOSTREG.BOKKONTO >= "60000" AND KOSTREG.BOKKONTO <= "64900" THEN DO:
            ASSIGN KOSTREG.OVRKR = DECIMAL(SUBSTRING(tidin.TIN,143,20)) / 1000.
         END. 
         ELSE IF KOSTREG.BOKKONTO >= "65000" AND KOSTREG.BOKKONTO <= "66190" THEN DO:
            ASSIGN KOSTREG.OVRKR = DECIMAL(SUBSTRING(tidin.TIN,143,20)) / 1000.
         END.
         ELSE IF KOSTREG.BOKKONTO >= "69100" AND KOSTREG.BOKKONTO <= "69999" THEN DO:
            ASSIGN KOSTREG.OVRKR = DECIMAL(SUBSTRING(tidin.TIN,143,20)) / 1000.
         END.
         ELSE IF KOSTREG.BOKKONTO >= "73200" AND KOSTREG.BOKKONTO <= "76990" THEN DO:
            ASSIGN KOSTREG.OVRKR = DECIMAL(SUBSTRING(tidin.TIN,143,20)) / 1000.
         END.
         ELSE DO:
            ASSIGN KOSTREG.OVRKR = DECIMAL(SUBSTRING(tidin.TIN,143,20)) / 1000.
         END.
      END.
      DELETE tidin.
   END.
   utprogkopia = progkopia + filnamn + STRING(TODAY,"99999999").
   OS-RENAME VALUE(dirlist) VALUE(utprogkopia).
END PROCEDURE.
PROCEDURE kalmar_UI.
   FOR EACH tidin:   
      IF SUBSTRING(tidin.TIN,3,5) = "65922" THEN DO:
         DELETE tidin.
         NEXT.
      END.
      IF SUBSTRING(tidin.TIN,3,5) = "68300" THEN DO:
         DELETE tidin.
         NEXT.
      END.
      IF SUBSTRING(tidin.TIN,15,6) = "" THEN NEXT. /*AONR*/
      rad = rad + 1.                                       
      RUN aokoll_UI.
      IF AVAILABLE AONRTAB THEN DO:
         FIND LAST KOSTREG WHERE KOSTREG.AONR = AONRTAB.AONR AND 
         KOSTREG.DELNR = AONRTAB.DELNR
         USE-INDEX KOST NO-LOCK NO-ERROR.  
         rad = 1.                                                                      
         IF AVAILABLE KOSTREG THEN rad = KOSTREG.RADNR + 1. 
         indate = DATE(INTEGER(SUBSTRING(tidin.TIN,72,2)),01,INTEGER(SUBSTRING(tidin.TIN,68,4))).
         IF MONTH(indate) = 12 THEN DO:
            indate = DATE(12,31,YEAR(indate)).
         END.
         ELSE DO:   
            indate = DATE((MONTH(indate) + 1),01,YEAR(indate)) - 1.
         END.
         IF indate <= TODAY - 35 THEN indate = TODAY.
         CREATE KOSTREG.
         ASSIGN  
         KOSTREG.RADNR = rad
         KOSTREG.AONR = AONRTAB.AONR
         KOSTREG.DELNR = AONRTAB.DELNR
         KOSTREG.REGDATUM = TODAY       
         KOSTREG.REGDATUM = indate 
         KOSTREG.BETDATUM = TODAY
         KOSTREG.BENAMNING = SUBSTRING(tidin.TIN,86,19) 
         KOSTREG.BOKKONTO = SUBSTRING(tidin.TIN,3,5)
         KOSTREG.FAKTNR = SUBSTRING(tidin.TIN,37,8)
         KOSTREG.FAKTURERAD = ?
         KOSTREG.LEVKOD = ""
         SUBSTRING(KOSTREG.ANVANDARE,1,12) = "EKONOM"
         KOSTREG.KOSTAUTO = TRUE. 
         VALIDATE KOSTREG.
         IF SUBSTRING(KOSTREG.BOKKONTO,1,1) = "3" THEN DO:
            ASSIGN KOSTREG.INKOMST = DECIMAL(SUBSTRING(tidin.TIN,143,20)) * -1 / 1000.
         END.  
         ELSE IF SUBSTRING(KOSTREG.BOKKONTO,1,1) = "4" THEN DO:
            ASSIGN KOSTREG.MTRL = DECIMAL(SUBSTRING(tidin.TIN,143,20)) / 1000. 
         END. 
         ELSE IF SUBSTRING(KOSTREG.BOKKONTO,1,1) = "5" THEN DO:
            ASSIGN KOSTREG.OVRKR = DECIMAL(SUBSTRING(tidin.TIN,143,20)) / 1000. 
         END. 
         ELSE IF KOSTREG.BOKKONTO >= "60000" AND KOSTREG.BOKKONTO <= "65800" THEN DO: 
            ASSIGN KOSTREG.OVRKR = DECIMAL(SUBSTRING(tidin.TIN,143,20)) / 1000.
         END. 
         ELSE IF KOSTREG.BOKKONTO >= "65900" AND KOSTREG.BOKKONTO <= "65915" THEN DO: 
            ASSIGN KOSTREG.OVRKR = DECIMAL(SUBSTRING(tidin.TIN,143,20)) / 1000.
         END. 
         ELSE IF KOSTREG.BOKKONTO >= "65917" AND KOSTREG.BOKKONTO <= "65921" THEN DO: 
            ASSIGN KOSTREG.OVRKR = DECIMAL(SUBSTRING(tidin.TIN,143,20)) / 1000.
         END. 
         ELSE IF KOSTREG.BOKKONTO >= "65923" AND KOSTREG.BOKKONTO <= "66190" THEN DO: 
            ASSIGN KOSTREG.OVRKR = DECIMAL(SUBSTRING(tidin.TIN,143,20)) / 1000.
         END. 
         ELSE IF KOSTREG.BOKKONTO >= "69100" AND KOSTREG.BOKKONTO <= "69999" THEN DO: 
            ASSIGN KOSTREG.OVRKR = DECIMAL(SUBSTRING(tidin.TIN,143,20)) / 1000.
         END. 
         ELSE IF KOSTREG.BOKKONTO >= "73200" AND KOSTREG.BOKKONTO <= "76990" THEN DO: 
            ASSIGN KOSTREG.OVRKR = DECIMAL(SUBSTRING(tidin.TIN,143,20)) / 1000.
         END. 
         ELSE IF KOSTREG.BOKKONTO >= "97041" THEN DO: 
            ASSIGN KOSTREG.MTRL = DECIMAL(SUBSTRING(tidin.TIN,143,20)) / 1000.
         END. 
         ELSE IF KOSTREG.BOKKONTO >= "97461" THEN DO: 
            ASSIGN KOSTREG.MTRL = DECIMAL(SUBSTRING(tidin.TIN,143,20)) / 1000.
         END.
         ELSE DO:
            ASSIGN KOSTREG.OVRKR = DECIMAL(SUBSTRING(tidin.TIN,143,20)) / 1000.
         END.
      END.
      DELETE tidin.
   END.
   utprogkopia = progkopia + filnamn + STRING(TODAY,"99999999").
   OS-RENAME VALUE(dirlist) VALUE(utprogkopia).
END PROCEDURE.
PROCEDURE aokoll_UI:
   FIND FIRST AONRTAB WHERE AONRTAB.AONR = SUBSTRING(tidin.TIN,15,6) /*AND 
   AONRTAB.DELNR = INTEGER(SUBSTRING(tidin.TIN,21,3))    */
   USE-INDEX AONR NO-LOCK NO-ERROR.   
   IF NOT AVAILABLE AONRTAB THEN DO:
      IF SUBSTRING(tidin.TIN,15,1) = "0" THEN DO:
         FIND FIRST AONRTAB WHERE AONRTAB.AONR = SUBSTRING(tidin.TIN,16,5) 
         /*AND AONRTAB.DELNR = INTEGER(SUBSTRING(tidin.TIN,21,3))    */
         USE-INDEX AONR NO-LOCK NO-ERROR.   
      END.
   END.
END PROCEDURE.
