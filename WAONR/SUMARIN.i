/*SUMARIN.I*/   
   FIND FIRST VKORN NO-LOCK NO-ERROR.
   IF NOT AVAILABLE VKORN THEN DO:
      RETURN.
   END.
   IF VKORN.VECKOK = FALSE THEN DO:
      FRAME DIALOG-1:TITLE = "Uppf�ljning per dag k�rs nu " + " " + STRING(TIME,"HH:MM").
      IF Guru.Konstanter:appcon THEN DO:                                                    
         RUN SUMDAG.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT.
      END.
      ELSE DO:
         RUN SUMDAG.P.
      END.
   END.     
  

