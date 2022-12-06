/*DEPA2TRP.I*/
   FIND FIRST trp_mtrl WHERE trp_mtrl.ENR = lin_upp.ENR AND
   trp_mtrl.LEVKOD = "0" NO-LOCK NO-ERROR.  
   IF AVAILABLE trp_mtrl THEN DO:           
      enrvar = lin_upp.ENR.
      ASSIGN
      antvar = lin_upp.TOTMETER
      lin_upp.ENR = trp_mtrl.ENR
      lin_upp.BENAMNING = trp_mtrl.BENAMNING
      lin_upp.ENHET = trp_mtrl.ENHET
      lin_upp.LEVKOD = "0" 
      lin_upp.KLAR2 = TRUE
      lin_upp.PRIS = trp_mtrl.OPRIS
      lin_upp.TOTPRIS = lin_upp.PRIS * lin_upp.TOTMETER
      trp_mtrl.ANTAL = trp_mtrl.ANTAL - antvar.
   END.
