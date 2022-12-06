/*TIDUPPIN.I*/ 
      DEFINE VARIABLE semdagar AS DECIMAL NO-UNDO.
      DEFINE VARIABLE sjdagar AS DECIMAL NO-UNDO.
      DEFINE VARIABLE komptimmar AS DECIMAL NO-UNDO.      
      IF Guru.Konstanter:globforetag = "SNAT"  THEN DO:         
         musz = FALSE.  
         FIND TIDREGITAB WHERE RECID(TIDREGITAB) = tidtabrec NO-LOCK NO-ERROR.
         IF TIDREGITAB.AONR = "150" AND TIDREGITAB.DELNR = 0 THEN DO:      
            IF MONTH(TIDREGITAB.DATUM) < 5 THEN musz = TRUE.
            ELSE IF MONTH(TIDREGITAB.DATUM) = 12 AND DAY(TIDREGITAB.DATUM) GE 15 THEN musz = TRUE.           
            IF musz = TRUE THEN DO:
               musz = FALSE.
               IF Guru.Konstanter:appcon THEN DO:                           
                  RUN SEMTILL.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
                  (INPUT persrec , INPUT TIDREGITAB.DATUM ,OUTPUT semdagar).
               END.
               ELSE DO:
                  RUN SEMTILL.P 
                  (INPUT persrec , INPUT TIDREGITAB.DATUM ,OUTPUT semdagar).                  
               END.        
               /* VERKAR BLI LÅSNING*/
               IF semdagar < 16 THEN DO:      
                  DO TRANSACTION:
                     FIND TIDREGITAB WHERE RECID(TIDREGITAB) = tidtabrec EXCLUSIVE-LOCK NO-ERROR.      
                     ASSIGN TIDREGITAB.LONTILLAGG = "5089"
                     TIDREGITAB.LONTILLANTAL = 1.
                  END.                               
                  FIND TIDREGITAB WHERE RECID(TIDREGITAB) = tidtabrec NO-LOCK NO-ERROR.                     
               END.
            END.                        
         END.     
         ELSE IF TIDREGITAB.AONR = "110" AND TIDREGITAB.DELNR = 0 THEN DO:       
            IF Guru.Konstanter:appcon THEN DO:                           
               RUN SJINTY2.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
               (INPUT persrec , INPUT TIDREGITAB.DATUM).
            END.
            ELSE DO:
               RUN SJINTY2.P 
               (INPUT persrec , INPUT TIDREGITAB.DATUM).                  
            END.        
         END.
         IF TIDREGITAB.AONR NE "150" AND TIDREGITAB.LONTILLAGG = "5089" THEN DO:
            DO TRANSACTION:               
               FIND TIDREGITAB WHERE RECID(TIDREGITAB) = tidtabrec EXCLUSIVE-LOCK NO-ERROR.      
               ASSIGN TIDREGITAB.LONTILLAGG = ""
               TIDREGITAB.LONTILLANTAL = 0.
            END. 
            RELEASE TIDREGITAB NO-ERROR. 
         END.
         IF TIDREGITAB.AONR NE "171" AND TIDREGITAB.LONTILLAGG = "260" AND TIDREGITAB.LONAUTO = TRUE THEN DO:
            DO TRANSACTION:               
               FIND TIDREGITAB WHERE RECID(TIDREGITAB) = tidtabrec EXCLUSIVE-LOCK NO-ERROR.      
               ASSIGN TIDREGITAB.LONTILLAGG = ""
               TIDREGITAB.LONTILLANTAL = 0.
            END. 
            RELEASE TIDREGITAB NO-ERROR. 
         END.              
         IF TIDREGITAB.AONR = "171" AND TIDREGITAB.DELNR = 0 THEN DO:      
            IF Guru.Konstanter:appcon THEN DO:                           
               RUN VECKHAKL.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
               (INPUT persrec , INPUT TIDREGITAB.DATUM,OUTPUT komptimmar).
            END.
            ELSE DO:
               RUN VECKHAKL.P 
               (INPUT persrec , INPUT TIDREGITAB.DATUM,OUTPUT komptimmar).                  
            END.
            IF komptimmar > 0 THEN DO:      
               DO TRANSACTION:
                  FIND TIDREGITAB WHERE RECID(TIDREGITAB) = tidtabrec EXCLUSIVE-LOCK NO-ERROR.      
                  ASSIGN TIDREGITAB.LONTILLAGG = "260"                     
                  TIDREGITAB.LONTILLANTAL = komptimmar
                  TIDREGITAB.LONAUTO = TRUE.
               END.                               
               FIND TIDREGITAB WHERE RECID(TIDREGITAB) = tidtabrec NO-LOCK NO-ERROR.                     
            END.
         END.         
      END.
      IF Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:         
         musz = FALSE.  
         FIND TIDREGITAB WHERE RECID(TIDREGITAB) = tidtabrec NO-LOCK NO-ERROR.
         IF TIDREGITAB.AONR = "110" AND TIDREGITAB.DELNR = 0 THEN DO:       
            IF Guru.Konstanter:appcon THEN DO:                           
               RUN SJINTY2.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
               (INPUT persrec , INPUT TIDREGITAB.DATUM).
            END.
            ELSE DO:
               RUN SJINTY2.P 
               (INPUT persrec , INPUT TIDREGITAB.DATUM).                  
            END.        
         END.
         /* msv får inte komptimmar för veckovila halvdag eller klämdag Ingrid 20101108
         msv får nu komptimmar för veckovila halvdag eller klämdag enligt Ulla Backman 20120510
         */
         IF TIDREGITAB.AONR NE "171" AND TIDREGITAB.LONTILLAGG = "260" AND TIDREGITAB.LONAUTO = TRUE THEN DO:
            DO TRANSACTION:               
               FIND TIDREGITAB WHERE RECID(TIDREGITAB) = tidtabrec EXCLUSIVE-LOCK NO-ERROR.      
               ASSIGN TIDREGITAB.LONTILLAGG = ""
               TIDREGITAB.LONTILLANTAL = 0.
            END. 
            RELEASE TIDREGITAB NO-ERROR. 
         END.              
         IF TIDREGITAB.AONR = "171" AND TIDREGITAB.DELNR = 0 THEN DO:      
            IF Guru.Konstanter:appcon THEN DO:                           
               RUN VECKHAKL.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
               (INPUT persrec , INPUT TIDREGITAB.DATUM,OUTPUT komptimmar).
            END.
            ELSE DO:
               RUN VECKHAKL.P 
               (INPUT persrec , INPUT TIDREGITAB.DATUM,OUTPUT komptimmar).                  
            END.
            IF komptimmar > 0 THEN DO:      
               DO TRANSACTION:
                  FIND TIDREGITAB WHERE RECID(TIDREGITAB) = tidtabrec EXCLUSIVE-LOCK NO-ERROR.      
                  ASSIGN TIDREGITAB.LONTILLAGG = "260"                     
                  TIDREGITAB.LONTILLANTAL = komptimmar
                  TIDREGITAB.LONAUTO = TRUE.
               END.                               
               FIND TIDREGITAB WHERE RECID(TIDREGITAB) = tidtabrec NO-LOCK NO-ERROR.                     
            END.
         END.
      END.
      IF Guru.Konstanter:appcon THEN DO:                           
         RUN TIDUPPDE.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT TABLE tidapptemp).
      END.
      ELSE DO:
         RUN TIDUPPDE.P 
         (INPUT TABLE tidapptemp).                  
      END.
