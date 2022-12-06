/*SUMMATRP.I*/  
   FOR EACH mtrl_temp2:
      mtrl_temp2.KLAR = FALSE.
   END.     
   ASSIGN       
   totalsum = 0
   ediraknare = 0
   byggnr = 0.      
   FIND first ANNNAMN NO-LOCK NO-ERROR.
   OPEN QUERY berq FOR EACH BERUPP WHERE BERUPP.AONR = valaonr AND
   BERUPP.OMRADE = valomrade USE-INDEX OMR NO-LOCK.
   GET FIRST berq NO-LOCK.
   DO WHILE AVAILABLE(BERUPP): 
      upp = FALSE.      
      FIND FIRST mtrl_temp2 WHERE mtrl_temp2.LEVKOD = vald_lev AND 
      mtrl_temp2.UPPLAG = BERUPP.UPPLAG USE-INDEX LISTA NO-LOCK NO-ERROR. 
      IF AVAILABLE mtrl_temp2 THEN DO: 
         ASSIGN
         upp = TRUE.
         IF edivar = FALSE THEN DO:    
            IF mailvar = FALSE THEN DO:
               CREATE tidut.
               ASSIGN
               SUBSTRING(tidut.UT,1) = str0
               SUBSTRING(tidut.UT,132) = "$"
               tant = 1.
            END.
            ELSE DO:   
               CREATE tidut. 
               CREATE tidut. 
               SUBSTRING(tidut.UT,1) = str0.
            END.   
            CREATE tidut.                                       
            ASSIGN
            tant = tant + 1
            SUBSTRING(tidut.UT,1) = "Upplag Nr :" 
            SUBSTRING(tidut.UT,12) = STRING(BERUPP.UPPLAG).
            CREATE tidut.
            ASSIGN
            tant = tant + 1
            SUBSTRING(tidut.UT,1) = "Adress    :"
            SUBSTRING(tidut.UT,12) = BERUPP.ADRESS.
            IF BERUPP.ANMARK NE "" THEN DO:             
               CREATE tidut.
               ASSIGN
               tant = tant + 1 
               uppvar = TRUE
               SUBSTRING(tidut.UT,1) = "Anmärkning:". 
               RUN upplag_UI.
               uppvar = FALSE.
            END.              
            CREATE tidut.
            ASSIGN
            tant = tant + 1
            SUBSTRING(tidut.UT,1) = str0.
         END.
         ELSE DO:
            CREATE tidut.
            ASSIGN
            ediraknare = ediraknare + 1.
            SUBSTRING(tidut.UT,1) = "L01;" + STRING(ediraknare) + 
            ";" + STRING(BERUPP.UPPLAG) + ";" + "20".
            IF BERUPP.ANMARK NE " " THEN DO:
               RUN anmark_UI.                                             
            END.
            IF BERUPP.ADRESS NE " " THEN DO:
               CREATE tidut.
               ASSIGN
               SUBSTRING(tidut.UT,1) = "L07;" + STRING(BERUPP.UPPLAG) + ";" + 
               SUBSTRING(BERUPP.ADRESS,1,35)
               tidut.UT = REPLACE(tidut.UT,CHR(10)," ").
            END.   
         END.   
         bytgrupp = "".    
         FOR EACH mtrl_temp2 WHERE mtrl_temp2.UPPLAG = BERUPP.UPPLAG AND
         mtrl_temp2.LEVKOD = vald_lev AND
         mtrl_temp2.KLAR = FALSE USE-INDEX ORD:             
            RUN ett_UI.                  
         END.  
      END. 
      FIND FIRST lin_upp WHERE lin_upp.UPPLAG = BERUPP.UPPLAG AND
      lin_upp.LEVKOD = vald_lev NO-LOCK NO-ERROR.
      IF AVAILABLE lin_upp THEN DO: 
         IF upp = FALSE THEN DO:
            IF edivar = FALSE THEN DO:
               IF mailvar = FALSE THEN DO:
                  IF edivar = FALSE THEN DO:
                     CREATE tidut.
                     ASSIGN
                     SUBSTRING(tidut.UT,1) = str0
                     SUBSTRING(tidut.UT,132) = "$"
                     tant = 1.
                  END.   
               END.
               ELSE DO:   
                  CREATE tidut. 
                  CREATE tidut. 
                  SUBSTRING(tidut.UT,1) = str0.
               END.      
               CREATE tidut.                                       
               ASSIGN
               tant = tant + 1
               SUBSTRING(tidut.UT,1) = "Upplag Nr :" 
               SUBSTRING(tidut.UT,11) = STRING(BERUPP.UPPLAG).
               CREATE tidut.
               ASSIGN
               tant = tant + 1
               SUBSTRING(tidut.UT,15) = "Adress   :"
               SUBSTRING(tidut.UT,22) = BERUPP.ADRESS.
               IF BERUPP.ANMARK NE "" THEN DO:             
                  CREATE tidut.
                  ASSIGN
                  tant = tant + 1 
                  uppvar = TRUE
                  SUBSTRING(tidut.UT,1) = "Anmärkning:".        
                  RUN upplag_UI.
                  uppvar = FALSE.
               END.              
               CREATE tidut.
               ASSIGN
               tant = tant + 1
               SUBSTRING(tidut.UT,1) = str0.      
            END.
            ELSE DO:
               CREATE tidut.
               ASSIGN
               ediraknare = ediraknare + 1.
               SUBSTRING(tidut.UT,1) = "L01;" + STRING(ediraknare) + 
               ";" + STRING(BERUPP.UPPLAG) + ";" + "20".
               IF BERUPP.ANMARK NE " " THEN DO:
                  RUN anmark_UI.
               END.
               IF BERUPP.ADRESS NE " " THEN DO:
                  CREATE tidut.
                  ASSIGN
                  SUBSTRING(tidut.UT,1) = "L07;" + STRING(BERUPP.UPPLAG) + ";" + 
                  SUBSTRING(BERUPP.ADRESS,1,35)
                  tidut.UT = REPLACE(tidut.UT,CHR(10)," ").
               END.   
            END.   
         END.
         ELSE DO:
            upp = upp.
         END.
         IF edivar = FALSE THEN DO:   
            CREATE tidut. 
            utrec = RECID(tidut).
            CREATE tidut.
            ASSIGN 
            SUBSTRING(tidut.UT,1) = str1
            utrec2 = RECID(tidut).
            kant = 2.          
            CREATE tidut.                                 
            ASSIGN
            kant = kant + 1
            SUBSTRING(tidut.UT,1) = "Linor och kablar till upplag".               
            CREATE tidut.
            ASSIGN 
            SUBSTRING(tidut.UT,1) = str1
            kant = kant + 1. 
            RUN rubrik_UI.
            FOR EACH lin_upp WHERE lin_upp.UPPLAG = BERUPP.UPPLAG AND
            lin_upp.LEVKOD = vald_lev:
               CREATE tidut.
               IF SUBSTRING(lin_upp.ENR,1,1) = "E" THEN DO:         
                  SUBSTRING(tidut.UT,1) = lin_upp.ENR.
               END.
               ELSE DO:      
                  SUBSTRING(tidut.UT,1) = "E" + lin_upp.ENR.
               END.  
               ASSIGN               
               SUBSTRING(tidut.UT,13) = SUBSTRING(lin_upp.BENAMNING,1,35)     
               SUBSTRING(tidut.UT,49) = lin_upp.ENHET
               SUBSTRING(tidut.UT,55) = STRING(lin_upp.TOTMETER,">>>>9").
               IF mailvar = FALSE THEN DO:                                                   
                  IF priset = TRUE THEN DO:
                     ASSIGN
                     SUBSTRING(tidut.UT,61) = STRING(lin_upp.PRIS,">>>>99.99")    
                     SUBSTRING(tidut.UT,71) = STRING(lin_upp.TOTMETER * lin_upp.PRIS,">>>>>9").
                     totalsum = totalsum + (lin_upp.TOTMETER * lin_upp.PRIS).  
                  END.   
               END.               
            END.  
         END.
         ELSE DO:
            FIND FIRST lin_upp WHERE lin_upp.UPPLAG = BERUPP.UPPLAG AND
            lin_upp.LEVKOD = vald_lev NO-LOCK NO-ERROR.
            IF AVAILABLE lin_upp THEN DO:
            
               CREATE tidut.
               ASSIGN
               ediraknare = ediraknare + 1
               byggnr = byggnr + 1
               SUBSTRING(tidut.UT,1) = "L01;" + STRING(ediraknare) + 
               ";" + "B" + STRING(byggnr,"9999") + " L/K" + ";30".
               CREATE tidut.               
               ASSIGN
               SUBSTRING(tidut.UT,1) = "L04;GEN;" + "LINOR KAB " + ";".
               
               FOR EACH lin_upp WHERE lin_upp.UPPLAG = BERUPP.UPPLAG AND
               lin_upp.LEVKOD = vald_lev:
                  CREATE tidut.
                  ASSIGN
                  ediraknare = ediraknare + 1.
                  IF SUBSTRING(lin_upp.ENR,1,1) = "E" THEN DO:
                     SUBSTRING(tidut.UT,1) = "L01;" + STRING(ediraknare) + ";" + TRIM(lin_upp.ENR) + ";40".
                     CREATE tidut.
                     ASSIGN   
                     SUBSTRING(tidut.UT,1) = "L02;" + TRIM(lin_upp.ENR) + ";" + 
                     SUBSTRING(lin_upp.BENAMNING,1,25). 
                     CREATE tidut.
                     ASSIGN   
                     SUBSTRING(tidut.UT,1) = "L03;" + TRIM(STRING(lin_upp.TOTMETER,">>>>9")) + ";" + 
                     lin_upp.ENHET.
                  END.
                  ELSE DO:
                     SUBSTRING(tidut.UT,1) = "L01;" + STRING(ediraknare) + ";" + "E" + TRIM(lin_upp.ENR) + ";40".
                     CREATE tidut.
                     ASSIGN   
                     SUBSTRING(tidut.UT,1) = "L02;" + "E" + TRIM(lin_upp.ENR) + ";" + 
                     SUBSTRING(lin_upp.BENAMNING,1,25). 
                     CREATE tidut.
                     ASSIGN   
                     SUBSTRING(tidut.UT,1) = "L03;" + TRIM(STRING(lin_upp.TOTMETER,">>>>9")) + ";" + 
                     lin_upp.ENHET.
                  END.
               END.
            END.      
         END.   
         IF edivar = FALSE THEN DO:
            IF mailvar = FALSE THEN RUN bryt_UI. 
         END.   
      END.                 
      GET NEXT berq NO-LOCK.   
   END.                     
   CLOSE QUERY berq.
   FIND FIRST mtrl_temp2 WHERE mtrl_temp2.UPPLAG = ? NO-LOCK NO-ERROR.
   IF AVAILABLE mtrl_temp2 THEN DO:  
      IF edivar = FALSE THEN DO: 
         IF mailvar = FALSE THEN DO:
            CREATE tidut. 
            ASSIGN
            tant = 1
            SUBSTRING(tidut.UT,1) = str0
            SUBSTRING(tidut.UT,132) = "$".
         END.
         ELSE DO:
            CREATE tidut. 
            CREATE tidut. 
            SUBSTRING(tidut.UT,1) = str0.
         END.   
         CREATE tidut.                                 
         ASSIGN
         tant = tant + 1
         SUBSTRING(tidut.UT,1) = "Materiel ej kopplat till något upplag".
         CREATE tidut. 
         ASSIGN
         tant = tant + 1
         SUBSTRING(tidut.UT,1) = str0.            
      END.
      ELSE DO:        
         CREATE tidut.
         ASSIGN
         ediraknare = ediraknare + 1.
         SUBSTRING(tidut.UT,1) = "L01;" + STRING(ediraknare) + 
         ";" + "0" + ";20".
         CREATE tidut.
         ASSIGN
         SUBSTRING(tidut.UT,1) = "L07;" + "0;Upplag okänt" + ";;".
      END.   
      bytgrupp = "".      
      FOR EACH mtrl_temp2 WHERE mtrl_temp2.UPPLAG = ? AND mtrl_temp2.LEVKOD = vald_lev AND
      mtrl_temp2.KLAR = FALSE
      USE-INDEX ORD:               
         RUN tva_UI.                  
      END.                   
   END.      
   ELSE DO:  
      musz = musz.     
   END.  
   FIND FIRST skydd_temp2 WHERE skydd_temp2.LEVKOD = vald_lev NO-LOCK NO-ERROR.
   IF AVAILABLE skydd_temp THEN DO:
      IF edivar = FALSE THEN DO:
         CREATE tidut.      
         ASSIGN
         SUBSTRING(tidut.UT,1) = str0.
         IF mailvar = FALSE THEN
         SUBSTRING(tidut.UT,132) = "$".
         CREATE tidut.
         SUBSTRING(tidut.UT,1) = "Kabelskydd / markeringar till schakt".
         CREATE tidut.      
         SUBSTRING(tidut.UT,1) = str0.       
         CREATE tidut.
         RUN rubrik_UI.
      END.
      ELSE DO:
         CREATE tidut.
         ASSIGN
         ediraknare = ediraknare + 1         
         SUBSTRING(tidut.UT,1) = "L01;" + STRING(ediraknare) + 
         ";" + "K/M" + ";20".
         CREATE tidut.
         ASSIGN
         ediraknare = ediraknare + 1
         byggnr = byggnr + 1
         SUBSTRING(tidut.UT,1) = "L01;" + STRING(ediraknare) + 
         ";" + "B" + STRING(byggnr,"9999") + " K/M" + ";30".
         CREATE tidut.               
         ASSIGN
         SUBSTRING(tidut.UT,1) = "L04;GEN;" + "LINOR KAB " + ";".
      END.   
      FOR EACH skydd_temp2 WHERE skydd_temp2.LEVKOD = vald_lev:
         IF edivar = FALSE THEN DO:
            CREATE tidut.
            IF SUBSTRING(skydd_temp2.ENR,1,1) = "E" THEN DO:         
               SUBSTRING(tidut.UT,1) = skydd_temp2.ENR.
            END.
            ELSE DO:      
               SUBSTRING(tidut.UT,1) = "E" + skydd_temp2.ENR.
            END.  
            ASSIGN            
            SUBSTRING(tidut.UT,13) = SUBSTRING(skydd_temp2.BENAMNING,1,35)      
            SUBSTRING(tidut.UT,49) = skydd_temp2.ENHET
            SUBSTRING(tidut.UT,55) = STRING(skydd_temp2.ANTAL,">>>>9").
            IF mailvar = FALSE THEN DO:                                               
               IF priset = TRUE THEN DO:
                  ASSIGN
                  SUBSTRING(tidut.UT,61) = STRING(skydd_temp2.PRIS,">>>>99.99")
                  SUBSTRING(tidut.UT,71) = STRING(skydd_temp2.ANTAL * skydd_temp2.PRIS,">>>>>9").
                  totalsum = totalsum + (skydd_temp2.ANTAL * skydd_temp2.PRIS).
               END.   
            END.           
         END.
         ELSE DO:
            CREATE tidut.
            ASSIGN
            ediraknare = ediraknare + 1.
            IF SUBSTRING(skydd_temp2.ENR,1,1) = "E" THEN DO:
               SUBSTRING(tidut.UT,1) = "L01;" + STRING(ediraknare) + ";" + TRIM(skydd_temp2.ENR) + ";40".
               CREATE tidut.
               ASSIGN   
               SUBSTRING(tidut.UT,1) = "L02;" + TRIM(skydd_temp2.ENR) + ";" + 
               SUBSTRING(skydd_temp2.BENAMNING,1,25). 
               CREATE tidut.
               ASSIGN   
               SUBSTRING(tidut.UT,1) = "L03;" + TRIM(STRING(skydd_temp2.ANTAL,">>>>9")) + ";" + 
               skydd_temp2.ENHET.
            END.
            ELSE DO:
               SUBSTRING(tidut.UT,1) = "L01;" + STRING(ediraknare) + ";" + "E" + TRIM(skydd_temp2.ENR) + ";40".
               CREATE tidut.
               ASSIGN   
               SUBSTRING(tidut.UT,1) = "L02;" + "E" + TRIM(skydd_temp2.ENR) + ";" + 
               SUBSTRING(skydd_temp2.BENAMNING,1,25). 
               CREATE tidut.
               ASSIGN   
               SUBSTRING(tidut.UT,1) = "L03;" + TRIM(STRING(skydd_temp2.ANTAL,">>>>9")) + ";" + 
               skydd_temp2.ENHET.
            END.
         END.   
      END.
   END.      
   
   /*FOR EACH trp_mtrl WHERE NO-LOCK:
      MESSAGE trp_mtrl.ENR "ANTAL" trp_mtrl.ANTAL "LEVKOD" trp_mtrl.LEVKOD "BERLEV" trp_mtrl.BERLEV "DBEST" trp_mtrl.DBEST VIEW-AS ALERT-BOX.
   END.*/

   FIND FIRST trp_mtrl WHERE trp_mtrl.ANTAL > 0 AND trp_mtrl.LEVKOD = vald_lev AND
   trp_mtrl.DBEST NE "RETUR" NO-LOCK NO-ERROR.
   IF AVAILABLE trp_mtrl THEN DO:
      IF edivar = FALSE THEN DO:
         CREATE tidut.      
         ASSIGN
         SUBSTRING(tidut.UT,1) = str0.
         IF mailvar = FALSE THEN
         SUBSTRING(tidut.UT,132) = "$".
         CREATE tidut.
         SUBSTRING(tidut.UT,1) = "Beställning till depå".  
         CREATE tidut.      
         SUBSTRING(tidut.UT,1) = str0.     
         CREATE tidut.
         RUN rubrik_UI.
      END.
      ELSE DO:
         CREATE tidut.
         ASSIGN
         ediraknare = ediraknare + 1.
         SUBSTRING(tidut.UT,1) = "L01;" + STRING(ediraknare) + 
         ";" + "DEP" + ";20".
         CREATE tidut.
         ASSIGN
         SUBSTRING(tidut.UT,1) = "L07;" + "DEP;Beställning till depå" + ";;".
         CREATE tidut.
         ASSIGN
         ediraknare = ediraknare + 1
         byggnr = byggnr + 1
         SUBSTRING(tidut.UT,1) = "L01;" + STRING(ediraknare) + 
         ";" + "B" + STRING(byggnr,"9999") + " DEP" + ";30".
         CREATE tidut.               
         ASSIGN
         SUBSTRING(tidut.UT,1) = "L04;GEN;" + "LINOR KAB " + ";".        
      END.   
      FOR EACH trp_mtrl WHERE trp_mtrl.ANTAL > 0 AND trp_mtrl.LEVKOD = vald_lev: 
         IF trp_mtrl.DBEST NE "RETUR" THEN DO:
            IF edivar = FALSE THEN DO:
               CREATE tidut.
               IF SUBSTRING(trp_mtrl.ENR,1,1) = "E" THEN DO:         
                  SUBSTRING(tidut.UT,1) = trp_mtrl.ENR.
               END.
               ELSE DO:      
                  SUBSTRING(tidut.UT,1) = "E" + trp_mtrl.ENR.
               END.  
               ASSIGN
               SUBSTRING(tidut.UT,13) = SUBSTRING(trp_mtrl.BENAMNING,1,35)                       
               SUBSTRING(tidut.UT,49) = trp_mtrl.ENHET
               SUBSTRING(tidut.UT,55) = STRING(trp_mtrl.ANTAL,">>>>9").
               IF mailvar = FALSE THEN DO:                                                  
                  IF priset = TRUE THEN DO:
                     ASSIGN
                     SUBSTRING(tidut.UT,61) = STRING(trp_mtrl.PRIS,">>>>99.99")
                     SUBSTRING(tidut.UT,71) = STRING(trp_mtrl.ANTAL * trp_mtrl.PRIS,">>>>>9").
                     totalsum = totalsum + (trp_mtrl.ANTAL * trp_mtrl.PRIS).  
                  END.   
               END.                 
            END.
            ELSE DO:
               CREATE tidut.
               ASSIGN
               ediraknare = ediraknare + 1.
               IF SUBSTRING(trp_mtrl.ENR,1,1) = "E" THEN DO:
                  SUBSTRING(tidut.UT,1) = "L01;" + STRING(ediraknare) + ";" + TRIM(trp_mtrl.ENR) + ";40".
                  CREATE tidut.
                  ASSIGN   
                  SUBSTRING(tidut.UT,1) = "L02;" + TRIM(trp_mtrl.ENR) + ";" + 
                  SUBSTRING(trp_mtrl.BENAMNING,1,25). 
                  CREATE tidut.
                  ASSIGN   
                  SUBSTRING(tidut.UT,1) = "L03;" + TRIM(STRING(trp_mtrl.ANTAL,">>>>9")) + ";" + 
                  trp_mtrl.ENHET.
               END.
               ELSE DO:
                  SUBSTRING(tidut.UT,1) = "L01;" + STRING(ediraknare) + ";" + "E" + TRIM(trp_mtrl.ENR) + ";40".
                  CREATE tidut.
                  ASSIGN   
                  SUBSTRING(tidut.UT,1) = "L02;" + "E" + TRIM(trp_mtrl.ENR) + ";" + 
                  SUBSTRING(trp_mtrl.BENAMNING,1,25). 
                  CREATE tidut.
                  ASSIGN   
                  SUBSTRING(tidut.UT,1) = "L03;" + TRIM(STRING(trp_mtrl.ANTAL,">>>>9")) + ";" + 
                  trp_mtrl.ENHET.
               END.                
            END.   
         END.   
      END.
   END. 
   IF edivar = FALSE THEN DO: 
      IF mailvar = FALSE THEN DO: 
         IF priset = TRUE THEN DO:
            CREATE tidut.                    
            CREATE tidut.
            SUBSTRING(tidut.UT,71) = "=======".
            CREATE tidut.
            ASSIGN
            SUBSTRING(tidut.UT,58) = "Summa totalt:"
            SUBSTRING(tidut.UT,71) = STRING(totalsum,">>>>>>9").            
         END.   
      END.   
   END.   
