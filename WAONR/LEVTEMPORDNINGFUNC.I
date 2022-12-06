
/*------------------------------------------------------------------------
    File        : LEVTEMPORDNINGFUNC.I
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Fri Nov 18 14:40:39 CET 2016
    Notes       :
  ----------------------------------------------------------------------*/
  
  
/*  
FUNCTION LevOrdFunk RETURNS INTEGER (lkod AS CHARACTER):   
  IF Guru.Konstanter:globforetag = "" THEN DO:
      FIND FIRST FORETAG WHERE NO-LOCK NO-ERROR.
      Guru.Konstanter:globforetag = FORETAG.FORETAG.
   END.   
   IF Guru.Konstanter:globforetag = "ELPA" THEN DO:
     /*
      IF lkod = "1" THEN RETURN 3.
      ELSE IF lkod = "5" THEN RETURN 1.
      ELSE IF lkod = "2" THEN RETURN 2.
      ELSE 
      */
      RETURN INTEGER(lkod). 
      
   END. 
   IF Guru.Konstanter:globforetag = "GRAN" THEN DO:
      /*   0    DEPÅ*/    
      IF lkod = "8"     THEN RETURN 1.
      ELSE IF lkod = "1"     THEN RETURN 2.
      ELSE IF lkod = "5"     THEN RETURN 3.
      ELSE IF lkod = "32"    THEN RETURN 4.  
      ELSE IF lkod = "30"    THEN RETURN 5.  
      ELSE IF lkod = "31"    THEN RETURN 6. 
      ELSE IF lkod = "34"    THEN RETURN 7. 
      ELSE IF lkod = "36"    THEN RETURN 8. 
      ELSE IF lkod = "12"    THEN RETURN 9. 
      ELSE IF lkod = "44"    THEN RETURN 10. 
      ELSE IF lkod = "47"    THEN RETURN 11. 
      ELSE IF lkod = "40"    THEN RETURN 12. 
      ELSE IF lkod = "46"    THEN RETURN 13. 
      ELSE IF lkod = "9"     THEN RETURN 14. 
      ELSE IF lkod = "100"   THEN RETURN 15. 
      ELSE IF lkod = "101"   THEN RETURN 16. 
      ELSE IF lkod = "102"   THEN RETURN 17. 
      ELSE IF lkod = "103"   THEN RETURN 18. 
      ELSE IF lkod = "104"   THEN RETURN 19.    
      ELSE RETURN INTEGER(lkod). 
      RETURN 999.
   END. 
   ELSE DO:
      RETURN INTEGER(lkod). 
   END.   
   RETURN 999.    
END FUNCTION.  
*/

FUNCTION LevOrdFunk RETURNS INTEGER (lkod AS CHARACTER):
  IF lkod = "" THEN RETURN 0. 
  
  DEFINE VARIABLE lastord AS INTEGER NO-UNDO.
  FIND FIRST EXTRADATA WHERE EXTRADATA.PROGRAM = "INLKAT" AND EXTRADATA.HUVUDCH = lkod NO-LOCK NO-ERROR.
  IF AVAILABLE EXTRADATA THEN DO:
     IF EXTRADATA.SOKINT[1] = 1000 THEN.
     ELSE IF EXTRADATA.SOKINT[1] > 0 THEN RETURN EXTRADATA.SOKINT[1].
  END.   
  FOR LAST EXTRADATA WHERE EXTRADATA.PROGRAM = "INLKAT" AND EXTRADATA.SOKINT[1] NE 1000 NO-LOCK BY EXTRADATA.SOKINT[1].
     lastord = EXTRADATA.SOKINT[1] + 1.
  END.   
  DO TRANSACTION:
     FIND FIRST EXTRADATA WHERE EXTRADATA.PROGRAM = "INLKAT" AND EXTRADATA.HUVUDCH = lkod EXCLUSIVE-LOCK NO-ERROR.
     IF AVAILABLE EXTRADATA THEN DO:
        EXTRADATA.SOKINT[1] = lastord.
        RETURN EXTRADATA.SOKINT[1].
     END.
     ELSE RETURN  lastord.  
  END.   
     
      
END FUNCTION.
   