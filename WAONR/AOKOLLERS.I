/*AOKOLLERS.I*/         
IF Guru.Konstanter:appcon THEN DO:                           
   RUN AOVALK.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
   (INPUT regdatum,INPUT ?,INPUT personaltemp.PERSONALKOD,
    INPUT utsokaonr.AONR,INPUT utsokaonr.DELNR,
    OUTPUT TABLE felmeddtemp).            
END.
ELSE DO:
   RUN AOVALK.P 
   (INPUT regdatum,INPUT ?,INPUT personaltemp.PERSONALKOD,
    INPUT utsokaonr.AONR,INPUT utsokaonr.DELNR,
    OUTPUT TABLE felmeddtemp).
END.  

FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
IF AVAILABLE felmeddtemp THEN DO:
   MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX.
   DELETE felmeddtemp.
   RETURN NO-APPLY.      
END.
