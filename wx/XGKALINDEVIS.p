/*XGKALINDEVIS.P*/   
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
DEFINE VARIABLE bokdatum AS DATE NO-UNDO. 
DEFINE VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE rad LIKE KOSTREG.RADNR NO-UNDO.
DEFINE VARIABLE iaonrvar AS INTEGER NO-UNDO.
DEFINE VARIABLE prognamn AS CHARACTER FORMAT "X(50)" NO-UNDO.                
DEFINE VARIABLE prognamnvar AS CHARACTER FORMAT "X(50)" NO-UNDO.                
DEFINE VARIABLE progkopia AS CHARACTER FORMAT "X(50)" NO-UNDO.                
DEFINE VARIABLE utprogkopia AS CHARACTER FORMAT "X(50)" NO-UNDO.                
DEFINE VARIABLE progque AS CHARACTER FORMAT "X(50)" NO-UNDO.                

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
IF globforetag = "ELPA" THEN DO:        
   ASSIGN
   prognamn = "\\pc012\D\DELAD\PRO9\guru\import\"
   progkopia = "\\pc012\D\DELAD\PRO9\guru\import\". 
END.
ELSE IF globforetag = "GKAL" THEN DO:
   
   ASSIGN
   prognamn = "\\goliat\DELAD\server\PRO9s\import\"
   progkopia = "\\goliat\DELAD\server\PRO9s\import\imkopia\".    
   
END.
DEFINE TEMP-TABLE felmeddftptemp 
  FIELD FELMEDD AS CHARACTER
  FIELD VAL AS INTEGER.
DEFINE TEMP-TABLE provag
   FIELD VAGNR AS INTEGER
   FIELD VAG AS CHARACTER
   INDEX VAGNR IS PRIMARY VAGNR.
IF globforetag = "ELPA" THEN globforetag = globforetag.
ELSE DO:
   /*VILKA FILER FINNS /*InGuru InHR InEconoma*/ */
   RUN FTPDIR.P (INPUT "gep", INPUT "vsf17ggr", 
                 INPUT "/InGuru",INPUT "tran004",
                 INPUT "192.168.69.34", 
                 OUTPUT TABLE felmeddftptemp,
                 OUTPUT TABLE provag).
   OUTPUT TO D:\delad\server\PRO9S\autotid.txt APPEND.
   FOR EACH felmeddftptemp:
      PUT UNFORMATTED felmeddftptemp.FELMEDD SKIP.
   END.
   OUTPUT CLOSE.
   EMPTY TEMP-TABLE felmeddftptemp NO-ERROR. 

   FOR EACH provag:
      IF provag.VAG NE "" THEN DO:
         /*HÄMTA FIL*/
         RUN FTPFILE.P  (INPUT "gep", INPUT "vsf17ggr", INPUT FALSE, INPUT 1,
                         INPUT prognamn + provag.VAG, INPUT "/InGuru/" + provag.VAG,
                         INPUT "192.168.69.34", OUTPUT TABLE felmeddftptemp).      
         OUTPUT TO D:\delad\server\PRO9S\autotid.txt APPEND.
         FOR EACH felmeddftptemp:
            PUT UNFORMATTED provag.VAG felmeddftptemp.FELMEDD SKIP.
         END.
         
         OUTPUT CLOSE.
         FIND FIRST felmeddftptemp NO-ERROR.  
         IF AVAILABLE felmeddftptemp THEN DO:
            IF felmeddftptemp.FELMEDD = 'Fil mottagen...' THEN DO:
               /*TA BORT REMOTEFILE*/
               RUN FTPFILE.P  (INPUT "gep", INPUT "vsf17ggr", INPUT ?, INPUT 1,
                               INPUT "", INPUT "/InGuru/" + provag.VAG,
                               INPUT "192.168.69.34", OUTPUT TABLE felmeddftptemp).                     
            END.
         END.
         EMPTY TEMP-TABLE felmeddftptemp NO-ERROR. 
      END.
   END.
END.

tider = REPLACE(STRING(TIME,"HH:MM"),":","").             
progque = prognamn + "GN6M7.Q".
INPUT FROM OS-DIR(prognamn) NO-ECHO.
REPEAT:
   /*Hämtar filnamn, hela sökvägen och vilken typ av fil det är*/
   SET filnamn dirlist attrlist.
   IF filnamn MATCHES "tran004" THEN DO:      
      prognamnvar = dirlist. 
      RUN startin_UI.
      RUN kalmar_UI.
      utprogkopia = progkopia + filnamn.
      utprogkopia = REPLACE(utprogkopia,".txt",tider + ".txt").
      OS-RENAME VALUE(dirlist) VALUE(utprogkopia).      
   END.
   /*
   IF filnamn MATCHES "DAGJ*.txt" THEN DO:
      prognamnvar = dirlist. 
      RUN startin_UI.
      RUN smaland_UI.
      utprogkopia = progkopia + filnamn.
      OS-RENAME VALUE(dirlist) VALUE(utprogkopia).
   END.        
   */
END.
PROCEDURE startin_UI:
   EMPTY TEMP-TABLE tidin NO-ERROR. 
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
  
END PROCEDURE.

PROCEDURE kalmar_UI.
   FOR EACH tidin:   
      IF SUBSTRING(tidin.TIN,142,6) = "" THEN DO:
         DELETE tidin.
         NEXT. /*AONR*/
      END.
      /*Elnät 101,210,301,302*/
      /*Småländsk 101,210,341,342*/
      IF SUBSTRING(tidin.TIN,25,3) = "101" THEN  musz = musz.
      ELSE IF SUBSTRING(tidin.TIN,25,3) = "210" THEN  musz = musz.
      ELSE IF SUBSTRING(tidin.TIN,25,3) = "301" THEN  musz = musz.
      ELSE IF SUBSTRING(tidin.TIN,25,3) = "302" THEN  musz = musz.      
      ELSE IF SUBSTRING(tidin.TIN,25,3) = "341" THEN  musz = musz.
      ELSE IF SUBSTRING(tidin.TIN,25,3) = "342" THEN  musz = musz.
      ELSE DO:
         DELETE tidin.
         NEXT.
      END.
      IF SUBSTRING(tidin.TIN,130,5) = "68400" OR  
         SUBSTRING(tidin.TIN,130,5) = "65922" OR
         SUBSTRING(tidin.TIN,130,5) = "68300" THEN DO: 
         DELETE tidin.
         NEXT.
      END.
      rad = rad + 1.                                       
      RUN aokoll_UI.
      IF AVAILABLE AONRTAB THEN DO:
         RUN kostreg_UI.
         IF SUBSTRING(KOSTREG.BOKKONTO,1,1) = "3" THEN DO:
            ASSIGN KOSTREG.INKOMST = DECIMAL(SUBSTRING(tidin.TIN,77,15)) * -1.
         END.  
         ELSE IF SUBSTRING(KOSTREG.BOKKONTO,1,1) = "4" THEN DO:
            ASSIGN KOSTREG.MTRL = DECIMAL(SUBSTRING(tidin.TIN,77,15)). 
         END. 
         ELSE IF SUBSTRING(KOSTREG.BOKKONTO,1,1) = "5" THEN DO:
            ASSIGN KOSTREG.OVRKR = DECIMAL(SUBSTRING(tidin.TIN,77,15)). 
         END. 
         ELSE IF KOSTREG.BOKKONTO >= "60000" AND KOSTREG.BOKKONTO <= "65800" THEN DO: 
            ASSIGN KOSTREG.OVRKR = DECIMAL(SUBSTRING(tidin.TIN,77,15)).
         END. 
         ELSE IF KOSTREG.BOKKONTO >= "65900" AND KOSTREG.BOKKONTO <= "65915" THEN DO: 
            ASSIGN KOSTREG.OVRKR = DECIMAL(SUBSTRING(tidin.TIN,77,15)).
         END. 
         ELSE IF KOSTREG.BOKKONTO >= "65917" AND KOSTREG.BOKKONTO <= "65921" THEN DO: 
            ASSIGN KOSTREG.OVRKR = DECIMAL(SUBSTRING(tidin.TIN,77,15)).
         END. 
         ELSE IF KOSTREG.BOKKONTO >= "65923" AND KOSTREG.BOKKONTO <= "66190" THEN DO: 
            ASSIGN KOSTREG.OVRKR = DECIMAL(SUBSTRING(tidin.TIN,77,15)).
         END. 
         ELSE IF KOSTREG.BOKKONTO >= "69100" AND KOSTREG.BOKKONTO <= "69999" THEN DO: 
            ASSIGN KOSTREG.OVRKR = DECIMAL(SUBSTRING(tidin.TIN,77,15)).
         END. 
         ELSE IF KOSTREG.BOKKONTO >= "73200" AND KOSTREG.BOKKONTO <= "76990" THEN DO: 
            ASSIGN KOSTREG.OVRKR = DECIMAL(SUBSTRING(tidin.TIN,77,15)).
         END. 
         ELSE IF KOSTREG.BOKKONTO >= "97041" THEN DO: 
            ASSIGN KOSTREG.MTRL = DECIMAL(SUBSTRING(tidin.TIN,77,15)).
         END. 
         ELSE IF KOSTREG.BOKKONTO >= "97461" THEN DO: 
            ASSIGN KOSTREG.MTRL = DECIMAL(SUBSTRING(tidin.TIN,77,15)).
         END.
         ELSE DO:
            ASSIGN KOSTREG.OVRKR = DECIMAL(SUBSTRING(tidin.TIN,77,15)).
         END.
      END.
      DELETE tidin.
   END.
   
END PROCEDURE.
PROCEDURE kostreg_UI:
   FIND LAST KOSTREG WHERE KOSTREG.AONR = AONRTAB.AONR AND 
   KOSTREG.DELNR = AONRTAB.DELNR
   USE-INDEX KOST NO-LOCK NO-ERROR.  
   rad = 1.                                                                      
   IF AVAILABLE KOSTREG THEN rad = KOSTREG.RADNR + 1. 
   IF INTEGER(SUBSTRING(tidin.TIN,12,2)) > 12  THEN SUBSTRING(tidin.TIN,12,2) = "12".
   bokdatum = DATE(INTEGER(SUBSTRING(tidin.TIN,12,2)),01,INTEGER(SUBSTRING(tidin.TIN,8,4))).
   CREATE KOSTREG.
   ASSIGN  
   KOSTREG.RADNR = rad
   KOSTREG.AONR = AONRTAB.AONR
   KOSTREG.DELNR = AONRTAB.DELNR
   KOSTREG.REGDATUM = bokdatum 
   KOSTREG.BETDATUM = TODAY
   KOSTREG.BENAMNING = SUBSTRING(tidin.TIN,41,36) 
   KOSTREG.BOKKONTO = SUBSTRING(tidin.TIN,130,5)
   KOSTREG.FAKTNR = SUBSTRING(tidin.TIN,16,9)
   KOSTREG.FAKTURERAD = ?
   KOSTREG.LEVKOD = ""
   SUBSTRING(KOSTREG.ANVANDARE,1,12) = "EKONOMdevis"
   KOSTREG.KOSTAUTO = TRUE. 
   VALIDATE KOSTREG.
END PROCEDURE.
PROCEDURE aokoll_UI:
   FIND FIRST AONRTAB WHERE AONRTAB.AONR = SUBSTRING(tidin.TIN,142,6) AND AONRTAB.DELNR = 0
   USE-INDEX AONR NO-LOCK NO-ERROR.   
   IF NOT AVAILABLE AONRTAB THEN DO:
      IF SUBSTRING(tidin.TIN,142,1) = "0" THEN DO:
         FIND FIRST AONRTAB WHERE AONRTAB.AONR = SUBSTRING(tidin.TIN,143,5) AND AONRTAB.DELNR = 0 
         
         USE-INDEX AONR NO-LOCK NO-ERROR.   
      END.
   END.
   IF globforetag = "elpa" THEN DO:
      IF NOT AVAILABLE AONRTAB THEN DO TRANSACTION:
         CREATE AONRTAB.
         ASSIGN
         AONRTAB.AONR = SUBSTRING(tidin.TIN,142,6).
         AONRTAB.ORT = "IN FRÅN DEVIS".
      END.
   END.
   
END PROCEDURE.
/*
PROCEDURE smaland_UI.
   FOR EACH tidin:   
      IF SUBSTRING(tidin.TIN,141,6) = "" THEN DO:
         DELETE tidin.
         NEXT. /*AONR*/
      END.
      /*Småländsk 101,210,341,342*/
      IF SUBSTRING(tidin.TIN,25,3) = "101" THEN  musz = musz.
      ELSE IF SUBSTRING(tidin.TIN,25,3) = "210" THEN  musz = musz.
      ELSE IF SUBSTRING(tidin.TIN,25,3) = "341" THEN  musz = musz.
      ELSE IF SUBSTRING(tidin.TIN,25,3) = "342" THEN  musz = musz.
      ELSE DO:
         DELETE tidin.
         NEXT.
      END.
      rad = rad + 1.                                                                      
      RUN aokoll_UI.     
      IF AVAILABLE AONRTAB THEN DO:
         RUN kostreg_UI.
         IF SUBSTRING(KOSTREG.BOKKONTO,1,1) = "3" THEN DO:
            ASSIGN KOSTREG.INKOMST = DECIMAL(SUBSTRING(tidin.TIN,77,15)).
         END.  
         ELSE IF KOSTREG.BOKKONTO >= "44000" AND KOSTREG.BOKKONTO <= "46000" THEN DO:
            ASSIGN KOSTREG.MTRL = DECIMAL(SUBSTRING(tidin.TIN,77,15)). 
         END. 
         ELSE IF SUBSTRING(KOSTREG.BOKKONTO,1,1) = "5" THEN DO:
            ASSIGN KOSTREG.OVRKR = DECIMAL(SUBSTRING(tidin.TIN,77,15)). 
         END. 
         ELSE IF KOSTREG.BOKKONTO >= "60000" AND KOSTREG.BOKKONTO <= "64900" THEN DO:
            ASSIGN KOSTREG.OVRKR = DECIMAL(SUBSTRING(tidin.TIN,77,15)).
         END. 
         ELSE IF KOSTREG.BOKKONTO >= "65000" AND KOSTREG.BOKKONTO <= "66190" THEN DO:
            ASSIGN KOSTREG.OVRKR = DECIMAL(SUBSTRING(tidin.TIN,77,15)).
         END.
         ELSE IF KOSTREG.BOKKONTO >= "69100" AND KOSTREG.BOKKONTO <= "69999" THEN DO:
            ASSIGN KOSTREG.OVRKR = DECIMAL(SUBSTRING(tidin.TIN,77,15)).
         END.
         ELSE IF KOSTREG.BOKKONTO >= "73200" AND KOSTREG.BOKKONTO <= "76990" THEN DO:
            ASSIGN KOSTREG.OVRKR = DECIMAL(SUBSTRING(tidin.TIN,77,15)).
         END.
         ELSE DO:
            ASSIGN KOSTREG.OVRKR = DECIMAL(SUBSTRING(tidin.TIN,77,15)).
         END.
      END.
      DELETE tidin.
   END.   
END PROCEDURE.
*/
