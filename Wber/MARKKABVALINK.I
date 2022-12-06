/*MARKKABVALINK.I*/
   DEFINE VARIABLE enval AS LOGICAL NO-UNDO.
   enval = FALSE.
   IF ink_urval.GRUPP = 0 THEN enval = TRUE. 
   ELSE IF LOOKUP(STRING(ink_urval.GRUPP),Guru.Konstanter:varforetypchar[5]) > 0 THEN enval = TRUE.  
   
    
