
   
   /*xonemtrliberej.p*/
 DEFINE TEMP-TABLE mtrltraff NO-UNDO  
   FIELD enr     AS  CHARACTER     
    FIELD benamning     AS  CHARACTER                           
   FIELD levkod    AS  CHARACTER
   FIELD anttraff AS INTEGER 
   FIELD finnsiupplagg AS character
   INDEX ENR levkod ENR.                            
            
define buffer a for aonrtab.
/*output to c:\aa\MTRLEJTRAFFber.txt.*/
output to D:\elpool\mtrlejiber\MTRLEJTRAFFber.txt.
EMPTY TEMP-TABLE mtrltraff  NO-ERROR. 
for each AONRTAB WHERE AONRTAB.AONRAVDATUM = 01/01/91 NO-LOCK:
   FIND FIRST BEREDNING WHERE BEREDNING.AONR = AONRTAB.AONR AND BEREDNING.DELNR = AONRTAB.DELNR  NO-LOCK NO-ERROR.
   IF AVAILABLE BEREDNING THEN DO:
      FOR EACH BERMTRL  WHERE BERMTRL.AONR = STRING(BEREDNING.BERNR) AND BERMTRL.OMRADE = BEREDNING.OMRADE NO-LOCK:
         IF BERMTRL.LEVKOD = "99" THEN.
         ELSE DO:
            FIND FIRST MTRL WHERE MTRL.Enr = BERMTRL.ENR AND MTRL.LEVKOD = BERMTRL.LEVKOD NO-LOCK NO-ERROR.
            IF NOT AVAILABLE MTRL THEN DO:
               /*DO TRANSACTION:
                  FIND CURRENT BERMTRL EXCLUSIVE-LOCK NO-ERROR.
                  BERMTRL.PRIS = 0.
               END.*/                  
               put unformatted aonrtab.omrade " " aonrtab.aonr  " " aonrtab.delnr  " " BEREDNING.BERNR " " BERMTRL.ENR " " BERMTRL.LEVKOD " antal " BERMTRL.ANTAL skip.
               FIND FIRST mtrltraff  WHERE mtrltraff.Enr = BERMTRL.ENR AND mtrltraff.LEVKOD = BERMTRL.LEVKOD  NO-ERROR.
               IF NOT AVAILABLE mtrltraff THEN DO:
                  CREATE mtrltraff.
                  ASSIGN mtrltraff.enr = BERMTRL.ENR
                  mtrltraff.benamning = BERMTRL.benamning
                  mtrltraff.LEVKOD = BERMTRL.LEVKOD . 
               END.
               mtrltraff.ANTTRAFF = mtrltraff.ANTTRAFF + 1.
            END.
         END.   
      END.
      
   END.   
END.   

  
 output close.
 FOR EACH mtrltraff:
    FIND FIRST MTRLBER  WHERE MTRLBER.ENR =  mtrltraff.enr AND MTRLBER.LEVKOD =  mtrltraff.LEVKOD  NO-LOCK NO-ERROR.
    IF AVAILABLE MTRLBER THEN DO:
       mtrltraff.finnsiupplagg = "ja".
    END.
 END.
 output to D:\elpool\mtrlejiber\MTRLEJtraff.txt.
/*  output to c:\aa\MTRLEJtraff.txt.*/
 FOR EACH mtrltraff:
    put unformatted mtrltraff.enr ";" mtrltraff.benamning ";" mtrltraff.LEVKOD ";" mtrltraff.anttraff ";" mtrltraff.finnsiupplagg  skip.
 END.
 output close.
 
 
    
