/*ESIN.P FAKTURUR IN*/       
DEFINE VARIABLE rad LIKE KOSTREG.RADNR NO-UNDO.
DEFINE VARIABLE prognamn AS CHARACTER FORMAT "X(20)" NO-UNDO.                
DEFINE VARIABLE prognamndat AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE prognamnque AS CHARACTER FORMAT "X(20)" NO-UNDO.                
DEFINE VARIABLE words AS CHARACTER FORMAT "X(132)" NO-UNDO.
DEFINE VARIABLE kommando AS CHARACTER FORMAT "X(132)" NO-UNDO.
DEFINE VARIABLE kommandoprog AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE TEMP-TABLE tidin
   FIELD TIN AS CHARACTER FORMAT "X(132)". 
DEFINE TEMP-TABLE infil
   FIELD PROGNAMN AS CHARACTER FORMAT "X(78)" 
   INDEX PRO IS PRIMARY PROGNAMN.

FOR EACH infil:
   DELETE infil.
END.
    
FOR EACH tidin:
   DELETE tidin.
END.
IF OPSYS = "UNIX" THEN DO: 
   kommandoprog = "/eko1/guru/import/GURUIN*.TXT".
   kommando = "ls /eko1/guru/import/GURUTI*.DAT > /eko1/guru/import/GURUIN*.TXT".   
END.    
ELSE DO: 
   kommandoprog = "C:\delad\elpao\GURUIN*.TXT".
   kommando = "DIR/a:-d /b C:\delad\elpao\GURUTI*.DAT > C:\delad\elpao\GURUIN*.TXT".
   
END. 
OS-DELETE VALUE(kommandoprog).
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
   IF INDEX(infil.PROGNAMN,".DAT") = 0 THEN DO:       
      DELETE infil.
      NEXT.
   END.
   infil.PROGNAMN = SUBSTRING(infil.PROGNAMN,1,INDEX(infil.PROGNAMN,".DAT") - 1).   
END.
FOR EACH infil:  
   IF OPSYS = "UNIX" THEN DO:          
      prognamn = infil.PROGNAMN + ".OLD".                
      prognamndat = infil.PROGNAMN + ".DAT".
      prognamnque = infil.PROGNAMN + ".q".
   END. 
   ELSE DO:
      prognamn = "C:\delad\elpao\" + infil.PROGNAMN + ".OLD".             
      prognamndat = "C:\delad\elpao\" + infil.PROGNAMN + ".DAT".
      prognamnque = "C:\delad\elpao\" + infil.PROGNAMN + ".q".
   END.   
   RUN in_UI.
END.
PROCEDURE in_UI:  
   FOR EACH tidin:
      DELETE tidin.
   END.   
   IF OPSYS = "UNIX" THEN DO:
      kommando = SEARCH("quoter").
      IF kommando = ? THEN DO:          
         NEXT.       
      END.   
   END.
   ELSE DO:      
      kommando = SEARCH("quoter.exe").
      IF kommando = ? THEN NEXT.       
   END.   
   OS-COMMAND SILENT VALUE(kommando) VALUE(prognamndat) > VALUE(prognamnque).
   IF OS-ERROR > 0 THEN DO:         
      NEXT.
   END.   
   INPUT FROM VALUE(prognamnque)  NO-ECHO
   CONVERT TARGET "iso8859-1" SOURCE "swedish-7-bit".
   REPEAT:
      SET words VIEW-AS EDITOR INNER-CHARS 50 INNER-LINES 3 WITH FRAME DDD WIDTH 80.   
      CREATE TIDIN.   
      ASSIGN TIDIN.TIN = words.   
   END.
   INPUT CLOSE.  
  /* RUN skapaao_UI.      */
   OS-RENAME VALUE(prognamndat) VALUE(prognamn).  
   OS-DELETE VALUE(prognamnque).    
END PROCEDURE.
             
PROCEDURE skapaao_UI:                                                                     
   FOR EACH tidin:   
      /*DELNR OK*/
      IF SUBSTRING(TIDIN.TIN,71,2) = "" THEN DO:
         FIND FIRST AONRTAB WHERE AONRTAB.AONR = SUBSTRING(TIDIN.TIN,65,6) AND 
         AONRTAB.DELNR = 0    
         USE-INDEX AONR NO-LOCK NO-ERROR.
      END.
      ELSE DO:
         FIND FIRST AONRTAB WHERE AONRTAB.AONR = SUBSTRING(TIDIN.TIN,65,6) AND 
         AONRTAB.DELNR = INTEGER(SUBSTRING(TIDIN.TIN,71,2))    
         USE-INDEX AONR NO-LOCK NO-ERROR.
      END.
      IF AVAILABLE AONRTAB THEN DO:
         FIND LAST KOSTREG WHERE KOSTREG.AONR = AONRTAB.AONR AND 
         KOSTREG.DELNR = AONRTAB.DELNR
         USE-INDEX KOST NO-LOCK NO-ERROR.  
         rad = 1.                                                                      
         IF AVAILABLE KOSTREG THEN rad = KOSTREG.RADNR + 1. 
         CREATE KOSTREG.
         ASSIGN  
         KOSTREG.RADNR = rad
         KOSTREG.AONR = AONRTAB.AONR
         KOSTREG.DELNR = AONRTAB.DELNR
         KOSTREG.REGDATUM  = DATE(SUBSTRING(TIDIN.TIN,73,8))       
         KOSTREG.BENAMNING = SUBSTRING(TIDIN.TIN,10,42) 
         KOSTREG.BOKKONTO = SUBSTRING(TIDIN.TIN,52,4)
         KOSTREG.FAKTNR = SUBSTRING(TIDIN.TIN,1,9)
         KOSTREG.FAKTURERAD = ?
         KOSTREG.LEVKOD = ""
         KOSTREG.ANVANDARE = "ADEEKO". 
         VALIDATE KOSTREG.
         IF KOSTREG.BOKKONTO >= "3010" AND KOSTREG.BOKKONTO <= "3500" THEN DO:
            ASSIGN KOSTREG.INKOMST = DECIMAL(SUBSTRING(TIDIN.TIN,96,1) + 
            SUBSTRING(TIDIN.TIN,81,15)) / -100.
         END.  
         IF KOSTREG.BOKKONTO >= "4000" AND KOSTREG.BOKKONTO <= "4599" THEN DO:
            ASSIGN KOSTREG.MTRL = DECIMAL(SUBSTRING(TIDIN.TIN,96,1) + 
            SUBSTRING(TIDIN.TIN,81,15)) / 100. 
         END.
         IF KOSTREG.BOKKONTO >= "4900" AND KOSTREG.BOKKONTO <= "4911" THEN DO:
            ASSIGN KOSTREG.MTRL = DECIMAL(SUBSTRING(TIDIN.TIN,96,1) + 
            SUBSTRING(TIDIN.TIN,81,15)) / 100. 
         END. 
         IF KOSTREG.BOKKONTO >= "4600" AND KOSTREG.BOKKONTO <= "4699" THEN DO:
            ASSIGN KOSTREG.MASKKOST = DECIMAL(SUBSTRING(TIDIN.TIN,96,1) + 
            SUBSTRING(TIDIN.TIN,81,15)) / 100. 
         END.
         IF KOSTREG.BOKKONTO >= "4701" AND KOSTREG.BOKKONTO <= "4899" THEN DO:
            ASSIGN KOSTREG.MASKKOST = DECIMAL(SUBSTRING(TIDIN.TIN,96,1) + 
            SUBSTRING(TIDIN.TIN,81,15)) / 100. 
         END.  
         IF KOSTREG.BOKKONTO = "4700" THEN DO:
            ASSIGN KOSTREG.OVRKR = DECIMAL(SUBSTRING(TIDIN.TIN,96,1) + 
            SUBSTRING(TIDIN.TIN,81,15)) / 100. 
         END. 
         IF KOSTREG.BOKKONTO >= "5000" AND KOSTREG.BOKKONTO <= "5999" THEN DO:
            ASSIGN KOSTREG.PERSKOST = DECIMAL(SUBSTRING(TIDIN.TIN,96,1) + 
            SUBSTRING(TIDIN.TIN,81,15)) / 100. 
         END.                         
         IF KOSTREG.BOKKONTO >= "6000" AND KOSTREG.BOKKONTO <= "8999" THEN DO:
            ASSIGN KOSTREG.OVRKR = DECIMAL(SUBSTRING(TIDIN.TIN,96,1) + 
            SUBSTRING(TIDIN.TIN,81,15)) / 100. 
         END.                  
      END.
   END.           
END PROCEDURE.
 

                
