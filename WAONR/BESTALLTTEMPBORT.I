/*BESTALLTTEMPBORT.I*/    
    IF Guru.Konstanter:varforetypval[3] >= 1 THEN DO:
       OPEN QUERY obq FOR EACH omrtemp,
       EACH bestkundallt WHERE bestkundallt.BESTID = omrtemp.OMRADE.
       GET FIRST obq.
       DO WHILE AVAILABLE(bestkundallt):  
          DELETE bestkundallt.
          GET NEXT obq.
       END.       
       OPEN QUERY obeq FOR EACH omrtemp,
       EACH bestkundextra WHERE bestkundextra.BESTID = omrtemp.OMRADE.
       GET FIRST obeq.
       DO WHILE AVAILABLE(bestkundextra):  
          DELETE bestkundextra.
          GET NEXT obeq.
       END.  
    END.
    
