
   
   /*xoneprojkollaoref.p*/       
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
   FIELD AONR                AS CHARACTER.

DEFINE TEMP-TABLE aotemp

   FIELD AONR                AS CHARACTER       
   FIELD DELNR          AS INTEGER
   FIELD ch6        AS CHARACTER        
   FIELD ch7         AS CHARACTER   
   INDEX AONRDELNR IS PRIMARY AONR DELNR.   
   
DEFINE BUFFER aotbuff FOR aotemp.   

   
DEFINE VARIABLE filnamn AS CHARACTER NO-UNDO.   

FIND FIRST FORETAG NO-LOCK NO-ERROR.
{muswait.i} 

EMPTY TEMP-TABLE tidinah NO-ERROR. 
   

   filnamn = "D:\elpool\kollAOREF.skv".

      
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
   FOR EACH tidinah WHERE tidinah.AONR = "":
      DELETE tidinah.
   END.           
END PROCEDURE.

PROCEDURE skapaenr_UI:   
   
   EMPTY TEMP-TABLE aotemp NO-ERROR.
      
      
       
   FOR EACH tidinah NO-LOCK:                                          
      CREATE aotemp.
      ASSIGN     
      aotemp.AONR = tidinah.AONR
      aotemp.DELNR = 0.
       
   END.
   
   
   
   FOR EACH aotemp WHERE NO-LOCK:
      FIND FIRST EXTRADATA WHERE EXTRADATA.PROGRAM = "AOREF" AND                   
      EXTRADATA.HUVUDCH = aotemp.AONR AND EXTRADATA.HUVUDINT = aotemp.DELNR   NO-LOCK NO-ERROR.
      IF AVAILABLE EXTRADATA  THEN DO:        
         assign            
         aotemp.ch7 = EXTRADATA.SOKCHAR[7]
         aotemp.ch6 = EXTRADATA.SOKCHAR[6].
      END.              
   END.
   
   OUTPUT TO D:\elpool\kollaorefut.txt.     
   FOR EACH aotemp  NO-LOCK:      
         PUT UNFORMATTED aotemp.AONR " "  aotemp.DELNR " "  aotemp.ch6 " "  aotemp.ch7 SKIP.
         
   END.      
   OUTPUT CLOSE.
         
       
END PROCEDURE.   

                

   

