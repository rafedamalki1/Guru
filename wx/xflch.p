   FIND FIRST FLEXAVT WHERE FLEXAVT.PERSONALKOD = "120" USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.
   IF NOT AVAILABLE FLEXAVT THEN .
   ELSE IF FLEXAVT.FLEXTID = TRUE THEN DO:   
      
      FIND FIRST FLEXREG WHERE FLEXREG.KOD = FLEXAVT.FLEXKOD USE-INDEX FLEXREG
      NO-LOCK NO-ERROR.   
      MESSAGE FLEXREG.SALDOKORD VIEW-AS ALERT-BOX.
      FOR EACH FLEXDAG WHERE FLEXDAG.PERSONALKOD = "120" AND FLEXDAG.FELMED = "Ingen registrering gjord"
      AND FLEXDAG.DATUM LE FLEXREG.SALDOKORD USE-INDEX FLEX EXCLUSIVE-LOCK:
         DISPLAY FLEXDAG.
      END.
   END.
   