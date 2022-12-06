
   
   /*xoneAONRDUB.p*/
 DEFINE TEMP-TABLE mtrltraff NO-UNDO  
   FIELD enr     AS  CHARACTER                            
   FIELD levkod    AS  CHARACTER
   FIELD anttraff AS INTEGER 
   INDEX ENR levkod ENR.                            
            
define buffer AOBUFF for aonrtab.
/*output to c:\aa\MTRLEJTRAFFber.txt.*/
output to D:\elpool\AONRDUBB.txt.
EMPTY TEMP-TABLE mtrltraff  NO-ERROR. 
for each AONRTAB WHERE AONRTAB.AONRAVDATUM = 01/01/91 NO-LOCK:
   FIND FIRST AOBUFF WHERE AOBUFF.AONR =  AONRTAB.AONR AND AOBUFF.DELNR = AONRTAB.DELNR AND RECID(AOBUFF) NE RECID(AONRTAB) NO-LOCK NO-ERROR.
   IF AVAILABLE AOBUFF THEN DO:    
      put UNFORMATTED "AONR " aonrtab.omrade " " aonrtab.aonr  " " aonrtab.delnr  " " aonrtab.AONRAVDATUM " " aonrtab.ORT  skip.
      put UNFORMATTED "AOBUFF " AOBUFF.omrade " " AOBUFF.aonr  " " AOBUFF.delnr  " " AOBUFF.AONRAVDATUM " " AOBUFF.ORT  skip.      
   END.   
END.   

  
 output close.
 
 
 
    
