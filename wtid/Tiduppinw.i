/*TIDUPPINW.I*/ 
      DEFINE VARIABLE semdagar AS DECIMAL NO-UNDO.
      DEFINE VARIABLE komptimmar AS DECIMAL NO-UNDO.
      
      IF Guru.Konstanter:globforetag = "SNAT"  THEN DO:         
         musz = FALSE.  
         IF tidallt.AONR = "150" AND tidallt.DELNR = 0 THEN DO:      
            IF MONTH(tidallt.DATUM) < 5 THEN musz = TRUE.
            ELSE IF MONTH(tidallt.DATUM) = 12 AND DAY(tidallt.DATUM) GE 15 THEN musz = TRUE.           
            IF musz = TRUE THEN DO:
               musz = FALSE.
               IF Guru.Konstanter:appcon THEN DO:                           
                  RUN SEMTILLW.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
                  (INPUT pkod , INPUT tidallt.DATUM ,OUTPUT semdagar).
               END.
               ELSE DO:
                  RUN SEMTILLW.P 
                  (INPUT pkod , INPUT tidallt.DATUM ,OUTPUT semdagar).                  
               END.        
               /* VERKAR BLI LÅSNING*/                   
               IF semdagar < 16 THEN DO:      
                  DO TRANSACTION:               
                     ASSIGN tidallt.LONTILLAGG = "5089"
                     tidallt.LONTILLANTAL = 1.
                  END.                               
               END.
            END.                        
         END.     
         ELSE IF tidallt.AONR = "110" AND tidallt.DELNR = 0 THEN DO:       
            IF Guru.Konstanter:appcon THEN DO:                           
               RUN SJINTY2W.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
               (INPUT pkod , INPUT tidallt.DATUM ).
            END.
            ELSE DO:
               RUN SJINTY2W.P 
               (INPUT pkod , INPUT tidallt.DATUM ).                  
            END.        
         END.
         IF tidallt.AONR NE "150" AND tidallt.LONTILLAGG = "5089" THEN DO:
            DO TRANSACTION:               
               ASSIGN tidallt.LONTILLAGG = ""
               tidallt.LONTILLANTAL = 0.
            END. 
         END.         
         IF tidallt.AONR NE "171" AND tidallt.LONTILLAGG = "260" AND tidallt.LONAUTO = TRUE THEN DO:
            DO TRANSACTION:               
               ASSIGN tidallt.LONTILLAGG = ""
               tidallt.LONTILLANTAL = 0.
            END. 
         END.
         IF tidallt.AONR = "171" AND tidallt.DELNR = 0 THEN DO:      
            IF Guru.Konstanter:appcon THEN DO:                           
               RUN VECKHAKLW.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
               (INPUT pkod , INPUT tidallt.DATUM,OUTPUT komptimmar).
            END.
            ELSE DO:
               RUN VECKHAKLW.P 
               (INPUT pkod , INPUT tidallt.DATUM,OUTPUT komptimmar).                  
            END.
            IF komptimmar > 0 THEN DO:                     
               DO TRANSACTION:                 
                  ASSIGN tidallt.LONTILLAGG = "260"                     
                  tidallt.LONTILLANTAL = komptimmar
                  tidallt.LONAUTO = TRUE.
               END.                                              
            END.
         END.
      END.     
      
      IF Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:         
         musz = FALSE.  
         IF tidallt.AONR = "110" AND tidallt.DELNR = 0 THEN DO:       
            IF Guru.Konstanter:appcon THEN DO:                           
               RUN SJINTY2W.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
               (INPUT pkod , INPUT tidallt.DATUM ).
            END.
            ELSE DO:
               RUN SJINTY2W.P 
               (INPUT pkod , INPUT tidallt.DATUM ).                  
            END.        
         END.
         /* msv får inte komptimmar för veckovila halvdag eller klämdag Ingrid 20101108
         msv får nu komptimmar för veckovila halvdag eller klämdag enligt Ulla Backman 20120510
         */
         IF tidallt.AONR NE "171" AND tidallt.LONTILLAGG = "260" AND tidallt.LONAUTO = TRUE THEN DO:
            DO TRANSACTION:               
               ASSIGN tidallt.LONTILLAGG = ""
               tidallt.LONTILLANTAL = 0.
            END. 
         END.
         IF tidallt.AONR = "171" AND tidallt.DELNR = 0 THEN DO:      
            IF Guru.Konstanter:appcon THEN DO:                           
               RUN VECKHAKLW.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
               (INPUT pkod , INPUT tidallt.DATUM,OUTPUT komptimmar).
            END.
            ELSE DO:
               RUN VECKHAKLW.P 
               (INPUT pkod , INPUT tidallt.DATUM,OUTPUT komptimmar).                  
            END.
            IF komptimmar > 0 THEN DO:                     
               DO TRANSACTION:                 
                  ASSIGN tidallt.LONTILLAGG = "260"                     
                  tidallt.LONTILLANTAL = komptimmar
                  tidallt.LONAUTO = TRUE.
               END.                                              
            END.
         END.
      END.

      EMPTY TEMP-TABLE extratidallt NO-ERROR.       
      FOR EACH tidallt WHERE tidallt.PERSONALKOD = "":
         CREATE extratidallt.
         BUFFER-COPY tidallt TO extratidallt.
         DELETE tidallt.
      END.            
      IF Guru.Konstanter:appcon THEN DO:                           
         RUN TIDUPPDW.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT TABLE tidapptemp,INPUT TABLE extratidallt,
          OUTPUT placerarec,OUTPUT TABLE tidallt APPEND).
      END.
      ELSE DO:
         RUN TIDUPPDW.P 
         (INPUT TABLE tidapptemp,INPUT TABLE extratidallt,
          OUTPUT placerarec,OUTPUT TABLE tidallt APPEND).
      END.
      
      
