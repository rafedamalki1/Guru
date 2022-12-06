/*NEDEPA3.I*/   
   antvar = trp_mtrl.ANTAL.
   IF skydd_temp2.ANTAL > antvar THEN DO:                  
      ASSIGN
      trp_mtrl.ANTAL = 0
      antvar2 = skydd_temp2.ANTAL - antvar
      skydd_temp2.ENR = trp_mtrl.ENR                                    
      skydd_temp2.BENAMNING = trp_mtrl.BENAMNING 
      skydd_temp2.ENHET = trp_mtrl.ENHET
      skydd_temp2.LEVKOD = trp_mtrl.LEVKOD
      skydd_temp2.KLAR2 = TRUE
      skydd_temp2.ANTAL = antvar
      skydd_temp2.PRIS = trp_mtrl.OPRIS
      skydd_temp2.TOTPRIS = skydd_temp2.PRIS * skydd_temp2.ANTAL.
      FIND FIRST trp_mtrl WHERE trp_mtrl.ENR = enrvar AND
      trp_mtrl.LEVKOD = "0" NO-LOCK NO-ERROR.
      IF AVAILABLE trp_mtrl THEN DO:  
         CREATE skyddbuff.
         ASSIGN      
         skyddbuff.ENR = skydd_temp2.ENR                                    
         skyddbuff.BENAMNING = skydd_temp2.BENAMNING 
         skyddbuff.ENHET = skydd_temp2.ENHET
         skyddbuff.PRIS = skydd_temp2.PRIS
         skyddbuff.LEVKOD = "0"
         skyddbuff.KLAR2 = TRUE.
         IF trp_mtrl.ANTAL <= antvar2 THEN DO:                                        
            skyddbuff.ANTAL = trp_mtrl.ANTAL.
            ASSIGN
            trp_mtrl.ANTAL = 0
            skyddbuff.PRIS = trp_mtrl.OPRIS
            skyddbuff.TOTPRIS = skyddbuff.PRIS * skyddbuff.ANTAL.
         END.
         ELSE DO:                                          
            skyddbuff.ANTAL = antvar2.
            ASSIGN
            trp_mtrl.ANTAL = trp_mtrl.ANTAL - antvar2
            skyddbuff.PRIS = trp_mtrl.OPRIS
            skyddbuff.TOTPRIS = skyddbuff.PRIS * skyddbuff.ANTAL.
         END.                                                     
      END.            
   END.
   ELSE DO:                                             
      ASSIGN
      trp_mtrl.ANTAL = trp_mtrl.ANTAL - skydd_temp2.ANTAL
      skydd_temp2.ENR = trp_mtrl.ENR                                    
      skydd_temp2.BENAMNING = trp_mtrl.BENAMNING 
      skydd_temp2.ENHET = trp_mtrl.ENHET
      skydd_temp2.LEVKOD = trp_mtrl.LEVKOD
      skydd_temp2.KLAR2 = TRUE
      skydd_temp2.PRIS = trp_mtrl.OPRIS
      skydd_temp2.TOTPRIS = skydd_temp2.PRIS * skydd_temp2.ANTAL.
   END. 
