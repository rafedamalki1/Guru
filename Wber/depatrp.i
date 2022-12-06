/*DEPATRP.I*/   
   
   FIND FIRST trp_mtrl WHERE trp_mtrl.ENR = enrvar AND
   trp_mtrl.LEVKOD = "0" NO-LOCK NO-ERROR.   
   IF AVAILABLE trp_mtrl THEN DO:             
      ASSIGN
      antvar = mtrl_temp2.ANTAL
      mtrl_temp2.ENR = trp_mtrl.ENR
      mtrl_temp2.BENAMNING = trp_mtrl.BENAMNING
      mtrl_temp2.ENHET = trp_mtrl.ENHET
      mtrl_temp2.LEVKOD = "0" 
      mtrl_temp2.PRIS = trp_mtrl.OPRIS
      mtrl_temp2.TOTPRIS = mtrl_temp2.PRIS * mtrl_temp2.ANTAL
      mtrl_temp2.KLAR2 = TRUE
      trp_mtrl.ANTAL = trp_mtrl.ANTAL - antvar.
   END.
