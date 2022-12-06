/*DEPA3TRP.I*/   
   FIND FIRST trp_mtrl WHERE trp_mtrl.ENR = skydd_temp2.ENR AND
   trp_mtrl.LEVKOD = "0" NO-LOCK NO-ERROR.   
   IF AVAILABLE trp_mtrl THEN DO:          
      ASSIGN
      antvar = skydd_temp2.ANTAL
      skydd_temp2.ENR = trp_mtrl.ENR                                    
      skydd_temp2.BENAMNING = trp_mtrl.BENAMNING 
      skydd_temp2.ENHET = trp_mtrl.ENHET
      skydd_temp2.KLAR2 = TRUE
      skydd_temp2.LEVKOD = "0" 
      skydd_temp2.PRIS = trp_mtrl.OPRIS
      skydd_temp2.TOTPRIS = skydd_temp2.PRIS * skydd_temp2.ANTAL
      trp_mtrl.ANTAL = trp_mtrl.ANTAL - antvar.
   END.
