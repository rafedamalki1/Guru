/*NEDEPA.I*/   
   antvar = trp_mtrl.ANTAL.  
   IF mtrl_temp2.ANTAL > antvar THEN DO:                  
      ASSIGN
      trp_mtrl.ANTAL = 0
      antvar2 = mtrl_temp2.ANTAL - antvar
      mtrl_temp2.ANTAL = antvar
      mtrl_temp2.ENR = trp_mtrl.ENR
      mtrl_temp2.BENAMNING = trp_mtrl.BENAMNING
      mtrl_temp2.ENHET = trp_mtrl.ENHET
      mtrl_temp2.LEVKOD = trp_mtrl.LEVKOD
      mtrl_temp2.PRIS = trp_mtrl.OPRIS
      mtrl_temp2.TOTPRIS = mtrl_temp2.PRIS * mtrl_temp2.ANTAL
      mtrl_temp2.KLAR2 = TRUE.    
      FIND FIRST trp_mtrl WHERE trp_mtrl.ENR = enrvar AND
      trp_mtrl.LEVKOD = "0" NO-LOCK NO-ERROR.
      IF AVAILABLE trp_mtrl THEN DO:                                
         CREATE mtrlbuff.
         ASSIGN                        
         mtrlbuff.UPPLAG = mtrl_temp2.UPPLAG 
         mtrlbuff.GRUPP = mtrl_temp2.GRUPP 
         mtrlbuff.XKORD = mtrl_temp2.XKORD 
         mtrlbuff.NUM = mtrl_temp2.NUM
         mtrlbuff.ENR = mtrl_temp2.ENR
         mtrlbuff.BENAMNING = mtrl_temp2.BENAMNING 
         mtrlbuff.ENHET = mtrl_temp2.ENHET
         mtrlbuff.PRIS = mtrl_temp2.PRIS
         mtrlbuff.LEVKOD = "0"
         mtrlbuff.KLAR2 = TRUE
         mtrlbuff.FORNR = mtrl_temp2.FORNR
         mtrlbuff.LINNR = mtrl_temp2.LINNR
         mtrlbuff.NATNR = mtrl_temp2.NATNR
         mtrlbuff.FRI1 = mtrl_temp2.FRI1
         mtrlbuff.FRI2 = mtrl_temp2.FRI2.
         IF trp_mtrl.ANTAL <= antvar2 THEN DO:                                                             
            ASSIGN
            mtrlbuff.ANTAL = trp_mtrl.ANTAL
            trp_mtrl.ANTAL = 0           
            mtrlbuff.PRIS = trp_mtrl.OPRIS
            mtrlbuff.TOTPRIS = mtrlbuff.PRIS * mtrlbuff.ANTAL.
         END.
         ELSE DO:                                                               
            ASSIGN
            trp_mtrl.ANTAL = trp_mtrl.ANTAL - antvar2
            mtrlbuff.ANTAL = antvar2         
            mtrlbuff.PRIS = trp_mtrl.OPRIS
            mtrlbuff.TOTPRIS = mtrlbuff.PRIS * mtrlbuff.ANTAL.
         END.                                                             
      END.            
   END.
   ELSE DO:                        
      ASSIGN
      trp_mtrl.ANTAL = trp_mtrl.ANTAL - mtrl_temp2.ANTAL
      mtrl_temp2.ENR = trp_mtrl.ENR
      mtrl_temp2.BENAMNING = trp_mtrl.BENAMNING
      mtrl_temp2.ENHET = trp_mtrl.ENHET
      mtrl_temp2.LEVKOD = trp_mtrl.LEVKOD
      mtrl_temp2.PRIS = trp_mtrl.OPRIS
      mtrl_temp2.TOTPRIS = mtrl_temp2.PRIS * mtrl_temp2.ANTAL
      mtrl_temp2.KLAR2 = TRUE.
   END.
