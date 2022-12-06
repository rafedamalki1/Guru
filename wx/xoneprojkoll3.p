
   
   /*xoneprojkoll2.p*/       
DEFINE NEW SHARED VARIABLE quotervar AS CHARACTER FORMAT "X(256)" NO-UNDO.



DEFINE VARIABLE gurubilder AS CHARACTER NO-UNDO.

DEFINE VARIABLE musz AS LOGICAL NO-UNDO.

DEFINE VARIABLE rad AS INTEGER NO-UNDO.
DEFINE VARIABLE prognamn AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE prognamndat AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE prognamnque AS CHARACTER FORMAT "X(20)" NO-UNDO.                
DEFINE VARIABLE words AS CHARACTER FORMAT "X(132)" NO-UNDO.
DEFINE VARIABLE kommando AS CHARACTER  NO-UNDO.
DEFINE VARIABLE kommando2 AS CHARACTER  NO-UNDO.
DEFINE VARIABLE kommandoprog AS CHARACTER FORMAT "X(20)" NO-UNDO.


DEFINE TEMP-TABLE tidinah
   FIELD AONRDELNR                AS CHARACTER       
   FIELD NYAONR1          AS CHARACTER   
   FIELD NYAONR2              AS CHARACTER
   FIELD NYARB              AS INTEGER
   FIELD activitysec      AS CHARACTER   
   
   INDEX AONRDELNR IS PRIMARY AONRDELNR.
   
DEFINE TEMP-TABLE aotemp
   FIELD GAONR                AS CHARACTER
   FIELD AONR                AS CHARACTER       
   FIELD DELNR          AS INTEGER
   FIELD NYAONR        AS CHARACTER        
   FIELD NYDELNR         AS INTEGER
   FIELD NYARB              AS INTEGER
   FIELD activitysec      AS CHARACTER
   INDEX AONRDELNR IS PRIMARY AONR DELNR.   
   
DEFINE BUFFER aotbuff FOR aotemp.   

   
DEFINE VARIABLE filnamn AS CHARACTER NO-UNDO.   

FIND FIRST FORETAG NO-LOCK NO-ERROR.
{muswait.i} 

EMPTY TEMP-TABLE tidinah NO-ERROR. 
   

   filnamn = "D:\elpool\fixAO-nummer till IFS projekt GURR 2020 11 03.skv".

      
   RUN in_UI.   
   RUN skapaenr_UI.
PROCEDURE in_UI: 

   EMPTY TEMP-TABLE tidinah NO-ERROR.
   
   INPUT FROM VALUE(filnamn) NO-ECHO.
   REPEAT:
      DO TRANSACTION: 
         CREATE tidinah.
         ASSIGN.
         IMPORT DELIMITER ";" tidinah   NO-ERROR.
      END.               
   END.
   FOR EACH tidinah WHERE tidinah.AONRDELNR = "":
      DELETE tidinah.
   END.           
END PROCEDURE.

PROCEDURE skapaenr_UI:   
   
   EMPTY TEMP-TABLE aotemp NO-ERROR.
      
      
       
   FOR EACH tidinah NO-LOCK:                                          
      CREATE aotemp.
      ASSIGN
      aotemp.GAONR = tidinah.AONRDELNR
      aotemp.AONR = SUBSTRING(tidinah.AONRDELNR,1,6).
      aotemp.DELNR = INTEGER(SUBSTRING(tidinah.AONRDELNR,7,2)).
      aotemp.NYAONR = tidinah.NYAONR1 + "." + tidinah.NYAONR2.
      aotemp.NYARB = tidinah.NYARB.
      aotemp.activitysec = tidinah.activitysec. 
   END.
   
   
   FOR EACH aotemp WHERE NO-LOCK:
      FIND FIRST aotbuff WHERE aotbuff.NYAONR = aotemp.NYAONR AND  aotbuff.GAONR NE aotemp.GAONR  NO-LOCK NO-ERROR.
      IF AVAILABLE aotbuff THEN DO:
         ASSIGN aotemp.NYDELNR = aotemp.DELNR.
      END.
      ELSE DO:
         ASSIGN aotemp.NYDELNR = aotemp.NYARB
         aotemp.NYARB = 0.
      END.      
   END.
   
   OUTPUT TO D:\elpool\AONRTRAFFA.txt.     
   FOR EACH aotemp  NO-LOCK:
      FIND FIRST AONRTAB  WHERE AONRTAB.AONR = aotemp.AONR
      AND AONRTAB.DELNR = aotemp.DELNR  NO-LOCK NO-ERROR.
      IF AVAILABLE AONRTAB THEN DO:
         PUT UNFORMATTED AONRTAB.AONR " "  AONRTAB.DELNR SKIP.
      END.         
   END.      
   OUTPUT CLOSE.
   OUTPUT TO D:\elpool\AONREJTRAFFA.txt.     
   FOR EACH aotemp  NO-LOCK:
      FIND FIRST AONRTAB  WHERE AONRTAB.AONR = aotemp.AONR
      AND AONRTAB.DELNR = aotemp.DELNR  NO-LOCK NO-ERROR.
      IF NOT AVAILABLE AONRTAB THEN DO:
         PUT UNFORMATTED aotemp.AONR " "  aotemp.DELNR SKIP.
      END.         
   END.      
   OUTPUT CLOSE.
   OUTPUT TO D:\elpool\AONRbytARB0.txt.
   FOR EACH aotemp WHERE aotemp.NYARB = 0 NO-LOCK:
      FIND FIRST AONRTAB  WHERE AONRTAB.AONR = aotemp.AONR
      AND AONRTAB.DELNR = aotemp.DELNR  NO-LOCK NO-ERROR.
      IF AVAILABLE AONRTAB THEN DO:      
         PUT UNFORMATTED aotemp.AONR " "  aotemp.DELNR " " aotemp.nyaonr  " " aotemp.nydelnr " " aotemp.nyarb " "  aotemp.activitysec SKIP.
      
      END.
   END.
   OUTPUT CLOSE.
   OUTPUT TO D:\elpool\AONRbytARBne0.txt.
   FOR EACH aotemp WHERE aotemp.NYARB NE 0 NO-LOCK:
      FIND FIRST AONRTAB  WHERE AONRTAB.AONR = aotemp.AONR
      AND AONRTAB.DELNR = aotemp.DELNR  NO-LOCK NO-ERROR.
      IF AVAILABLE AONRTAB THEN DO:      
         PUT UNFORMATTED aotemp.AONR " "  aotemp.DELNR " " aotemp.nyaonr  " " aotemp.nydelnr " " aotemp.nyarb " "  aotemp.activitysec SKIP.
      
      END.
   END.
   OUTPUT CLOSE.               
         
       
END PROCEDURE.   

                

   

