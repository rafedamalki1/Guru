      /*BESTALLTTEMPTILL.I*/
     
      {BESTALLTTEMPBORT.I}         
      IF Guru.Konstanter:varforetypval[3] >= 1 THEN DO:
         FIND FIRST omrtemp WHERE omrtemp.NAMN = CMB_OMR:SCREEN-VALUE NO-LOCK NO-ERROR.         
         IF AVAILABLE omrtemp THEN DO:
            FIND FIRST bestkundallt WHERE bestkundallt.BESTID = omrtemp.OMRADE NO-LOCK NO-ERROR.
            IF NOT AVAILABLE bestkundallt THEN DO:
               CREATE bestkundallt.
               ASSIGN 
               bestkundallt.VIBESTID = omrtemp.OMRADE 
               bestkundallt.BESTID = omrtemp.OMRADE 
               bestkundallt.BESTNAMN = omrtemp.NAMN.                                  
               CREATE bestkundextra.
               ASSIGN 
               bestkundextra.VIBESTID = omrtemp.OMRADE 
               bestkundextra.BESTID = omrtemp.OMRADE 
               bestkundextra.BESTNAMN = omrtemp.NAMN
               bestkundextra.AVDELNING = STRING(omrtemp.AVDELNINGNR).
            END.
         END.
      END.
