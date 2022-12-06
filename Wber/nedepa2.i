/*NEDEPA2.I*/ 
   antvar = trp_mtrl.ANTAL.
   IF lin_upp.TOTMETER > antvar THEN DO:                  
      ASSIGN
      trp_mtrl.ANTAL = 0
      antvar2 = lin_upp.TOTMETER - antvar
      lin_upp.ENR = trp_mtrl.ENR
      lin_upp.BENAMNING = trp_mtrl.BENAMNING
      lin_upp.ENHET = trp_mtrl.ENHET
      lin_upp.LEVKOD = trp_mtrl.LEVKOD
      lin_upp.KLAR2 = TRUE
      lin_upp.TOTMETER = antvar
      lin_upp.PRIS = trp_mtrl.OPRIS
      lin_upp.TOTPRIS = lin_upp.PRIS * lin_upp.TOTMETER.
      FIND FIRST trp_mtrl WHERE trp_mtrl.ENR = enrvar AND
      trp_mtrl.LEVKOD = "0" NO-LOCK NO-ERROR.
      IF AVAILABLE trp_mtrl THEN DO:
         CREATE linbuff.
         ASSIGN                        
         linbuff.UPPLAG = lin_upp.UPPLAG             
         linbuff.ENR = lin_upp.ENR
         linbuff.BENAMNING = lin_upp.BENAMNING 
         linbuff.ENHET = lin_upp.ENHET
         linbuff.KLAR2 = TRUE
         linbuff.PRIS = lin_upp.PRIS
         linbuff.LEVKOD = "0".     
         IF trp_mtrl.ANTAL <= antvar2 THEN DO:                                        
            linbuff.TOTMETER = trp_mtrl.ANTAL.
            ASSIGN
            trp_mtrl.ANTAL = 0
            linbuff.PRIS = trp_mtrl.OPRIS
            linbuff.TOTPRIS = linbuff.PRIS * linbuff.TOTMETER.
         END.
         ELSE DO:                                          
            linbuff.TOTMETER = antvar2.
            ASSIGN
            trp_mtrl.ANTAL = trp_mtrl.ANTAL - antvar2
            linbuff.PRIS = trp_mtrl.OPRIS
            linbuff.TOTPRIS = linbuff.PRIS * linbuff.TOTMETER.
         END.                              
      END.   
   END.
   ELSE DO:                                             
      ASSIGN
      lin_upp.ENR = trp_mtrl.ENR
      lin_upp.BENAMNING = trp_mtrl.BENAMNING
      lin_upp.ENHET = trp_mtrl.ENHET
      lin_upp.LEVKOD = trp_mtrl.LEVKOD
      lin_upp.KLAR2 = TRUE
      trp_mtrl.ANTAL = trp_mtrl.ANTAL - lin_upp.TOTMETER
      lin_upp.PRIS = trp_mtrl.OPRIS
      lin_upp.TOTPRIS = lin_upp.PRIS * lin_upp.TOTMETER.
   END.
