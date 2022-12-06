/*TVATRP.I*/   
   IF mtrl_temp2.XKORD NE ? THEN DO:         
      ASSIGN
      xkordvar = mtrl_temp2.XKORD
      sumpris = 0
      stopvar = FALSE.                
      FIND FIRST id_temp WHERE id_temp.XKORD = xkordvar AND
      id_temp.ENDKOMB = FALSE USE-INDEX ORD2 NO-LOCK NO-ERROR.
      IF AVAILABLE id_temp THEN DO: 
         stopvar = TRUE. 
         IF edivar = FALSE THEN DO:                                                                                         
            RUN id_UI.   
            CREATE tidut. 
            kant = kant + 1. 
            IF Guru.Konstanter:globforetag = "GRAN" OR Guru.Konstanter:globforetag = "GKAL" OR Guru.Konstanter:globforetag = "FORS" OR Guru.Konstanter:globforetag = "ELKB" OR Guru.Konstanter:globforetag = "NAEK" OR Guru.Konstanter:globforetag = "CPOMA" OR Guru.Konstanter:globforetag = "elpa"  THEN DO:
               FIND FIRST uppl_temp WHERE uppl_temp.NUM = id_temp.NUM
               NO-LOCK NO-ERROR.
               IF AVAILABLE uppl_temp THEN DO:               
                  CREATE tidut.
                  kant = kant + 1.
                  enkabval = FALSE.
                  IF uppl_temp.GRUPP = 0 THEN enkabval = TRUE.   
                  /* Claes vill inte ha markstation som kabelskåp i byggprotokoll , eftersom det bara är förkortningar*/
                  IF Guru.Konstanter:globforetag = "elpa" AND uppl_temp.GRUPP = 12 THEN enkabval = TRUE.                       
                  IF Guru.Konstanter:globforetag = "GRAN" AND uppl_temp.GRUPP = 35 THEN enkabval = TRUE.   
                  IF Guru.Konstanter:globforetag = "NAEK" AND uppl_temp.GRUPP = 35 THEN enkabval = TRUE.
                  IF Guru.Konstanter:globforetag = "CPOMA" AND uppl_temp.GRUPP = 35 THEN enkabval = TRUE.      
                  IF Guru.Konstanter:globforetag = "ELKB" AND uppl_temp.GRUPP = 35 THEN enkabval = TRUE.      
                  IF Guru.Konstanter:globforetag = "FORS" AND uppl_temp.GRUPP = 33 THEN enkabval = TRUE.      
                  IF enkabval = TRUE THEN DO:                  
                     ASSIGN                  
                     SUBSTRING(tidut.UT,1) = "Konstruktion"
                     SUBSTRING(tidut.UT,21) = ":"
                     SUBSTRING(tidut.UT,22) = uppl_temp.F1.
                     CREATE tidut.
                     CREATE tidut.
                     kant = kant + 2.                  
                     IF uppl_temp.GRUPP = 0 THEN RUN kskrubr_UI.
                     ELSE RUN mstnrubr_UI.                     
                  END.
                  ELSE DO:                  
                     ASSIGN                  
                     SUBSTRING(tidut.UT,1) = "Konstruktion"
                     SUBSTRING(tidut.UT,21) = ":"
                     SUBSTRING(tidut.UT,22) = uppl_temp.F1.
                     CREATE tidut.
                     kant = kant + 1.
                  END.
               END.
            END.
            RUN kanmark_UI.
         END.
         ELSE DO:
            CREATE tidut.
            ASSIGN
            ediraknare = ediraknare + 1
            byggnr = byggnr + 1
            idvar = "".                              
            SUBSTRING(tidut.UT,1) = "L01;" + STRING(ediraknare) + ";" +
            "B" + STRING(byggnr,"9999") + " ". 
            IF id_temp.FRI1 NE ? THEN idvar = TRIM(STRING(id_temp.FRI1,">>>>>9")) + ":".
            IF id_temp.FRI2 NE ? THEN idvar = idvar + TRIM(STRING(id_temp.FRI2,">>>>>9")).
            IF id_temp.FRI3 NE ? THEN idvar = idvar + " " + TRIM(id_temp.FRI3).
            IF idvar = "" THEN idvar = TRIM(SUBSTRING(id_temp.NATNR,1,9)).
            IF idvar = "" THEN idvar = "OKÄND".
            tidut.UT = tidut.UT + SUBSTRING(idvar,1,28) + ";30".
            
            CREATE tidut.
            FIND FIRST EDIGRUPP WHERE EDIGRUPP.KONSKOD = id_temp.GRUPP
            NO-LOCK NO-ERROR.
            FIND FIRST uppl_temp WHERE uppl_temp.NUM = id_temp.NUM
            NO-LOCK NO-ERROR.
            ASSIGN
            SUBSTRING(tidut.UT,1) = "L04;GEN;" + EDIGRUPP.BENAMNING +
            ";" + uppl_temp.F1.
         END.
      END.
      ELSE DO:   
         musz = musz.
      END.     
      IF edivar = FALSE THEN DO:
         CREATE tidut.         
         kant = kant + 1.
         RUN rubrik_UI. 
      END.   
      numsok = FALSE.    
      FOR EACH mtrlbuff WHERE mtrlbuff.UPPLAG = ? AND 
      mtrlbuff.XKORD = xkordvar AND mtrlbuff.LEVKOD = vald_lev AND
      mtrlbuff.KLAR = FALSE USE-INDEX ENRX:
         mtrlbuff.KLAR = TRUE.                    
         RUN materiel_UI.                 
      END.
      IF edivar = FALSE THEN DO:
         IF mailvar = FALSE THEN RUN bryt_UI.
      END.   
   END.
   ELSE DO: 
      sumpris = 0.                                      
      FIND FIRST id_temp WHERE id_temp.NUM = mtrl_temp2.NUM 
      NO-LOCK NO-ERROR.
      IF AVAILABLE id_temp THEN DO:                                                                                                        
         IF edivar = FALSE THEN DO:
            RUN id_UI. 
            IF Guru.Konstanter:globforetag = "GRAN" OR Guru.Konstanter:globforetag = "GKAL" OR Guru.Konstanter:globforetag = "FORS" OR Guru.Konstanter:globforetag = "ELKB" OR Guru.Konstanter:globforetag = "NAEK" OR Guru.Konstanter:globforetag = "CPOMA" OR Guru.Konstanter:globforetag = "elpa" THEN DO:
               FIND FIRST uppl_temp WHERE uppl_temp.NUM = id_temp.NUM
               NO-LOCK NO-ERROR.
               IF AVAILABLE uppl_temp THEN DO:               
                  CREATE tidut.
                  kant = kant + 1.
                  enkabval = FALSE.
                  IF uppl_temp.GRUPP = 0 THEN enkabval = TRUE.   
                  /* Claes vill inte ha markstation som kabelskåp i byggprotokoll , eftersom det bara är förkortningar*/
                  IF Guru.Konstanter:globforetag = "elpa" AND uppl_temp.GRUPP = 12 THEN enkabval = TRUE.
                  IF Guru.Konstanter:globforetag = "cVAST" AND uppl_temp.GRUPP = 12 THEN enkabval = TRUE.      
                  IF Guru.Konstanter:globforetag = "cVAST" AND uppl_temp.GRUPP = 33 THEN enkabval = TRUE.      
                  IF Guru.Konstanter:globforetag = "cVAST" AND uppl_temp.GRUPP = 38 THEN enkabval = TRUE.      
                  IF Guru.Konstanter:globforetag = "GRAN" AND uppl_temp.GRUPP = 35 THEN enkabval = TRUE. 
                  IF Guru.Konstanter:globforetag = "NAEK" AND uppl_temp.GRUPP = 35 THEN enkabval = TRUE.     
                  IF Guru.Konstanter:globforetag = "CPOMA" AND uppl_temp.GRUPP = 35 THEN enkabval = TRUE.
                  IF Guru.Konstanter:globforetag = "ELKB" AND uppl_temp.GRUPP = 35 THEN enkabval = TRUE.      
                  IF Guru.Konstanter:globforetag = "FORS" AND uppl_temp.GRUPP = 33 THEN enkabval = TRUE.      
                  IF enkabval = TRUE THEN DO:                  
                     ASSIGN                  
                     SUBSTRING(tidut.UT,1) = "Konstruktion"
                     SUBSTRING(tidut.UT,21) = ":"
                     SUBSTRING(tidut.UT,22) = uppl_temp.F1.
                     CREATE tidut.
                     CREATE tidut.
                     kant = kant + 2.                             
                     IF uppl_temp.GRUPP = 0 THEN RUN kskrubr_UI.
                     ELSE RUN mstnrubr_UI.                     
                  END.
                  ELSE DO:                  
                     ASSIGN                  
                     SUBSTRING(tidut.UT,1) = "Konstruktion"
                     SUBSTRING(tidut.UT,21) = ":"
                     SUBSTRING(tidut.UT,22) = uppl_temp.F1.
                     CREATE tidut.
                     kant = kant + 1.
                  END.
               END.
            END.
            RUN kanmark_UI.
         END.
         ELSE DO:   
            CREATE tidut.
            ASSIGN
            ediraknare = ediraknare + 1
            byggnr = byggnr + 1
            idvar = "".                         
            
            SUBSTRING(tidut.UT,1) = "L01;" + STRING(ediraknare) + ";" +
            "B" + STRING(byggnr,"9999") + " ". 
            IF id_temp.FRI1 NE ? THEN idvar = TRIM(STRING(id_temp.FRI1,">>>>>9")) + ":".
            IF id_temp.FRI2 NE ? THEN idvar = idvar + TRIM(STRING(id_temp.FRI2,">>>>>9")).
            IF id_temp.FRI3 NE ? THEN idvar = idvar + " " + TRIM(id_temp.FRI3).
            IF idvar = "" THEN idvar = TRIM(SUBSTRING(id_temp.NATNR,1,9)).
            IF idvar = "" THEN idvar = "OKÄND".
            tidut.UT = tidut.UT + SUBSTRING(idvar,1,28) + ";30".
            
            CREATE tidut.
            FIND FIRST EDIGRUPP WHERE EDIGRUPP.KONSKOD = id_temp.GRUPP
            NO-LOCK NO-ERROR.
            FIND FIRST uppl_temp WHERE uppl_temp.NUM = id_temp.NUM
            NO-LOCK NO-ERROR.
            ASSIGN
            SUBSTRING(tidut.UT,1) = "L04;GEN;" + EDIGRUPP.BENAMNING +
            ";" + uppl_temp.F1.                           
         END.   
      END.  
      ELSE DO:
         IF edivar = TRUE THEN DO:
            CREATE tidut.
            ASSIGN
            ediraknare = ediraknare + 1
            byggnr = byggnr + 1
            SUBSTRING(tidut.UT,1) = "L01;" + STRING(ediraknare) + 
            ";" + "B" + STRING(byggnr,"9999") + " OKÄND" + ";30".
            CREATE tidut.
            FIND FIRST EDIGRUPP WHERE EDIGRUPP.KONSKOD = mtrl_temp2.GRUPP
            NO-LOCK NO-ERROR.
            FIND FIRST uppl_temp WHERE uppl_temp.NUM = mtrl_temp2.NUM
            NO-LOCK NO-ERROR.
            ASSIGN
            SUBSTRING(tidut.UT,1) = "L04;GEN;" + EDIGRUPP.BENAMNING +
            ";" + uppl_temp.F1.
         END.
         ELSE DO:
            RUN id_UI.
            IF Guru.Konstanter:globforetag = "GRAN" OR Guru.Konstanter:globforetag = "GKAL" OR Guru.Konstanter:globforetag = "FORS" OR Guru.Konstanter:globforetag = "ELKB" OR Guru.Konstanter:globforetag = "NAEK" OR Guru.Konstanter:globforetag = "CPOMA"  OR Guru.Konstanter:globforetag = "elpa" THEN DO:
               FIND FIRST uppl_temp WHERE uppl_temp.NUM = mtrl_temp2.NUM
               NO-LOCK NO-ERROR.
               IF AVAILABLE uppl_temp THEN DO:               
                  CREATE tidut.
                  kant = kant + 1.
                  enkabval = FALSE.
                  IF uppl_temp.GRUPP = 0 THEN enkabval = TRUE.   
                  /* Claes vill inte ha markstation som kabelskåp i byggprotokoll , eftersom det bara är förkortningar*/
                  IF Guru.Konstanter:globforetag = "elpa" AND uppl_temp.GRUPP = 12 THEN enkabval = TRUE.
                  IF Guru.Konstanter:globforetag = "cVAST" AND uppl_temp.GRUPP = 12 THEN enkabval = TRUE.      
                  IF Guru.Konstanter:globforetag = "cVAST" AND uppl_temp.GRUPP = 33 THEN enkabval = TRUE.      
                  IF Guru.Konstanter:globforetag = "cVAST" AND uppl_temp.GRUPP = 38 THEN enkabval = TRUE.      
                  IF Guru.Konstanter:globforetag = "GRAN" AND uppl_temp.GRUPP = 35 THEN enkabval = TRUE. 
                  IF Guru.Konstanter:globforetag = "NAEK" AND uppl_temp.GRUPP = 35 THEN enkabval = TRUE.   
                  IF Guru.Konstanter:globforetag = "CPOMA" AND uppl_temp.GRUPP = 35 THEN enkabval = TRUE.  
                  IF Guru.Konstanter:globforetag = "FORS" AND uppl_temp.GRUPP = 33 THEN enkabval = TRUE.      
                  IF Guru.Konstanter:globforetag = "ELKB" AND uppl_temp.GRUPP = 35 THEN enkabval = TRUE.      
                  IF enkabval = TRUE THEN DO:                  
                     ASSIGN                  
                     SUBSTRING(tidut.UT,1) = "Konstruktion"
                     SUBSTRING(tidut.UT,21) = ":"
                     SUBSTRING(tidut.UT,22) = uppl_temp.F1.
                     CREATE tidut.
                     CREATE tidut.
                     kant = kant + 2.                     
                     IF uppl_temp.GRUPP = 0 THEN RUN kskrubr_UI.
                     ELSE RUN mstnrubr_UI.                     
                  END.
                  ELSE DO:                  
                     ASSIGN                  
                     SUBSTRING(tidut.UT,1) = "Konstruktion"
                     SUBSTRING(tidut.UT,21) = ":"
                     SUBSTRING(tidut.UT,22) = uppl_temp.F1.
                     CREATE tidut.
                     kant = kant + 1.
                  END.
               END.
            END.
            RUN kanmark_UI.
         END.   
      END.
      IF edivar = FALSE THEN DO:
         CREATE tidut.         
         kant = kant + 1.
         RUN rubrik_UI.     
      END.
      numsok = TRUE.   
      FOR EACH mtrlbuff WHERE mtrlbuff.UPPLAG = ? AND 
      mtrlbuff.NUM = mtrl_temp2.NUM AND mtrlbuff.LEVKOD = vald_lev AND
      mtrlbuff.KLAR = FALSE USE-INDEX ENRN:
         mtrlbuff.KLAR = TRUE.                     
         RUN materiel_UI.                 
      END.               
      IF edivar = FALSE THEN DO:
         IF mailvar = FALSE THEN RUN bryt_UI.
      END.   
   END.               
   totalsum = totalsum + sumpris.

