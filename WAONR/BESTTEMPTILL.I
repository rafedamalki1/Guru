      /*BESTTEMPTILL.I*/
      {BESTTEMPBORT.I}
      IF Guru.Konstanter:varforetypval[3] >= 1 THEN DO:
         FIND FIRST omrtemp WHERE omrtemp.NAMN = CMB_OMR 
         NO-LOCK NO-ERROR.         
         FIND FIRST besttemp WHERE besttemp.BBESTID = omrtemp.OMRADE
         NO-LOCK NO-ERROR.
         IF NOT AVAILABLE besttemp THEN DO:
            CREATE besttemp.
            ASSIGN besttemp.BBESTID = omrtemp.OMRADE 
            besttemp.BBESTNAMN = omrtemp.NAMN.                      
            status-ok = CMB_BESORG:ADD-LAST(besttemp.BBESTNAMN).
         END.
         IF nyttaovar = TRUE THEN CMB_BESORG = CMB_OMR.
         ELSE DO:
            FIND FIRST besttemp WHERE besttemp.BBESTID = CMB_BESORG
            NO-LOCK NO-ERROR.
            IF NOT AVAILABLE besttemp THEN CMB_BESORG = CMB_OMR.
         END.
         DISPLAY CMB_BESORG WITH FRAME {&FRAME-NAME}.         
      END.
      ELSE DO:
         IF nyttaovar = TRUE THEN DO:
            CMB_BESORG = CMB_OMR.
            DISPLAY CMB_BESORG WITH FRAME {&FRAME-NAME}.
         END.
      END.
