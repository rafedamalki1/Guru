/*BESTTEMPBORT.I*/    
    IF Guru.Konstanter:varforetypval[3] >= 1 THEN DO:
       OPEN QUERY obq FOR EACH omrtemp,
       EACH besttemp WHERE besttemp.BBESTID = omrtemp.OMRADE.
       GET FIRST obq.
       DO WHILE AVAILABLE(besttemp):
          CMB_BESORG:DELETE(besttemp.BBESTNAMN).
          DELETE besttemp.
          GET NEXT obq.
       END.
       /*
       FIND FIRST besttemp WHERE besttemp.BBESTNAMN = CMB_OMR 
       USE-INDEX BBEST NO-LOCK NO-ERROR.
       IF AVAILABLE besttemp THEN DELETE besttemp.
       */
    END.
