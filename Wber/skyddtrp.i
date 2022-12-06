/*SKYDDTRP.I*/   
   OPEN QUERY skyddq FOR EACH KSKYDD WHERE KSKYDD.AONR = valaonr AND
   KSKYDD.OMRADE = valomrade AND KSKYDD.BERED = TRUE
   AND KSKYDD.DATUM = datvar USE-INDEX OMR NO-LOCK.
   GET FIRST skyddq NO-LOCK.
   DO WHILE AVAILABLE(KSKYDD):
      CREATE skydd_temp.
      ASSIGN        
      skydd_temp.ENR = KSKYDD.ENR     
      skydd_temp.BENAMNING = KSKYDD.BENAMNING
      skydd_temp.ENHET = KSKYDD.ENHET
      skydd_temp.ANTAL = KSKYDD.ANTAL * KSKYDD.METER
      skydd_temp.PRIS = KSKYDD.PRIS            
      skydd_temp.LEVKOD = KSKYDD.LEVKOD.
      IF Guru.Konstanter:globforetag = "ELPA" {GLOBVES.I} THEN DO:
         IF KSKYDD.LEVKOD = "12" OR KSKYDD.LEVKOD = "13" THEN skydd_temp.LEVKOD = "16".
      END.
      GET NEXT skyddq NO-LOCK.
   END.    
   CLOSE QUERY skyddq.   
   sumantal = 0.
   FOR EACH skydd_temp BREAK BY skydd_temp.LEVKOD BY skydd_temp.ENR:      
   ACCUMULATE skydd_temp.ANTAL (TOTAL BY skydd_temp.LEVKOD BY skydd_temp.ENR).       
      IF LAST-OF(skydd_temp.ENR) THEN DO TRANSACTION:
         CREATE skydd_temp2.
         ASSIGN                                 
         skydd_temp2.ENR = skydd_temp.ENR
         skydd_temp2.BENAMNING = skydd_temp.BENAMNING 
         skydd_temp2.ENHET = skydd_temp.ENHET   
         skydd_temp2.PRIS = skydd_temp.PRIS
         skydd_temp2.LEVKOD = skydd_temp.LEVKOD                                          
         skydd_temp2.ANTAL = (ACCUM TOTAL skydd_temp.ANTAL) - sumantal                                
         sumantal = ACCUM TOTAL skydd_temp.ANTAL.                       
      END.     
   END.
   FOR EACH skydd_temp2 WHERE mtrl_temp2.ANTAL = 0:
      DELETE skydd_temp2.
   END.
   FOR EACH skydd_temp2:
      skydd_temp2.KLAR2 = FALSE.
   END.
   FOR EACH skydd_temp2 WHERE skydd_temp2.KLAR2 = FALSE:             
      FIND FIRST trp_mtrl WHERE trp_mtrl.ENR = skydd_temp2.ENR AND
      trp_mtrl.LEVKOD = skydd_temp2.LEVKOD AND trp_mtrl.DBEST NE "DEPÅ"       
      NO-LOCK NO-ERROR.
      IF AVAILABLE trp_mtrl THEN DO: 
         enrvar = skydd_temp2.ENR.                           
         IF trp_mtrl.ANTAL > 0 THEN DO:            
            RUN nedepa3_UI.                    
         END.          
         ELSE DO: 
            RUN depa3_UI.               
         END.                 
      END.
      ELSE DO:
         FIND FIRST trp_mtrl WHERE trp_mtrl.ENR = skydd_temp2.ENR AND
         trp_mtrl.LEVKOD NE skydd_temp2.LEVKOD AND trp_mtrl.DBEST NE "DEPÅ"       
         NO-LOCK NO-ERROR.
         IF AVAILABLE trp_mtrl THEN DO:          
            enrvar = skydd_temp2.ENR.
            IF trp_mtrl.ANTAL > 0 THEN DO:            
               RUN nedepa3_UI.               
            END.              
            ELSE DO:
               RUN depa3_UI.
            END.              
         END.
         ELSE DO:
            FIND FIRST trp_mtrl WHERE trp_mtrl.ENR = "E" + skydd_temp2.ENR AND
            trp_mtrl.LEVKOD NE skydd_temp2.LEVKOD AND trp_mtrl.DBEST NE "DEPÅ"       
            NO-LOCK NO-ERROR.
            IF AVAILABLE trp_mtrl THEN DO:   
               enrvar = "E" + skydd_temp2.ENR.      
               IF trp_mtrl.ANTAL > 0 THEN DO:            
                  RUN nedepa3_UI.                  
               END.              
               ELSE DO:
                  RUN depa3_UI.
               END.              
            END.
            ELSE DO:
               FIND FIRST trp_mtrl WHERE trp_mtrl.ENR = SUBSTRING(skydd_temp2.ENR,2,11) AND
               trp_mtrl.LEVKOD NE skydd_temp2.LEVKOD AND trp_mtrl.DBEST NE "DEPÅ"       
               NO-LOCK NO-ERROR.
               IF AVAILABLE trp_mtrl THEN DO:      
                  enrvar = SUBSTRING(skydd_temp2.ENR,2,11).   
                  IF trp_mtrl.ANTAL > 0 THEN DO:            
                     RUN nedepa3_UI.
                  END.              
                  ELSE DO:
                     RUN depa3_UI.
                  END.                 
               END.
               ELSE DO:
                  DELETE skydd_temp2.
               END.
            END.
         END.   
      END.      
   END.
