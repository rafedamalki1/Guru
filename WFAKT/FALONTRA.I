/*FALONTRA.I*/
DEFINE VARIABLE lonantal AS DECIMAL NO-UNDO.

PROCEDURE utantra_UI:
   DEFINE VARIABLE kodvar AS CHARACTER NO-UNDO.
   DEFINE VARIABLE tillvar AS CHARACTER NO-UNDO.
   REPEAT:
      FIND FIRST sumtidtemp WHERE sumtidtemp.LONTILLAGG NE "" AND sumtidtemp.LONTAGEN = FALSE
      NO-ERROR.
      IF AVAILABLE sumtidtemp THEN DO:        
         ASSIGN
         kodvar = sumtidtemp.ANSF
         tillvar = sumtidtemp.LONTILLAGG.
         FIND FIRST LONTILL WHERE LONTILL.KOD = kodvar AND
         LONTILL.LONTILLAGG = sumtidtemp.LONTILLAGG
         USE-INDEX LON NO-LOCK NO-ERROR.
         IF NOT AVAILABLE LONTILL THEN DO:
            FOR EACH sumtidtemp WHERE sumtidtemp.ANSF = kodvar AND 
            sumtidtemp.LONTILLAGG = tillvar:
               ASSIGN
               sumtidtemp.LONTAGEN = TRUE
               sumtidtemp.LONTILLAGG = ""
               sumtidtemp.LONTILLANTAL = 0.
            END.
            NEXT.
         END.
         ELSE IF SUBSTRING(LONTILL.TYPKOD,5,3) NE "FAK" THEN DO:
            FOR EACH sumtidtemp WHERE sumtidtemp.ANSF = kodvar AND
            sumtidtemp.LONTILLAGG = tillvar:  
               ASSIGN
               sumtidtemp.LONTAGEN = TRUE
               sumtidtemp.LONTILLAGG = ""
               sumtidtemp.LONTILLANTAL = 0.
            END.
            NEXT.
         END.
         ELSE DO:                    
            FOR EACH sumtidtemp WHERE sumtidtemp.ANSF = kodvar AND 
            sumtidtemp.LONTILLAGG = tillvar:  
               ASSIGN
               sumtidtemp.LONTAGEN = TRUE
               sumtidtemp.VILART = LONTILL.VILART.
               IF LONTILL.ENHET = "Km" THEN antalvar = 0.1.
               ELSE antalvar = 1. 

               IF LONTILL.ENHET = "Km" OR LONTILL.ENHET = "MIL" THEN DO:
                  ASSIGN
                  sumtidtemp.LONKOST = sumtidtemp.LONTILLANTAL * antalvar * FAKTREGLER.MIL.                  
                  sumtidtemp.LONTILLANTAL = sumtidtemp.LONTILLANTAL * antalvar.                             
               END.
               ELSE DO:
                  lonantal = sumtidtemp.LONTILLANTAL.
                  IF LONTILL.ENHET = "TI" THEN DO:
                     nytid = lonantal.
                     RUN TIMSEK.P.
                     lonantal = (sekunder / 3600). 
                  END.
                  sumtidtemp.LONKOST = (lonantal * LONTILL.ERSATTNING).                          
                  sumtidtemp.LONTILLANTAL = sumtidtemp.LONTILLANTAL.                             
               END.                             
            END.            
         END.
      END.
      ELSE LEAVE.
   END.
   REPEAT:
      FIND FIRST sumtidtemp WHERE sumtidtemp.TRAKTKOD NE "" AND sumtidtemp.TRATAGEN = FALSE
      NO-ERROR.
      IF AVAILABLE sumtidtemp THEN DO:        
         ASSIGN
         kodvar = sumtidtemp.TRAAVTAL
         tillvar = sumtidtemp.TRAKTKOD.
         FIND FIRST TRAKTATAB WHERE TRAKTATAB.TRAAVTAL = kodvar AND 
         TRAKTATAB.TRAKTKOD = tillvar  
         NO-LOCK NO-ERROR.         
         IF NOT AVAILABLE TRAKTATAB THEN DO:
            FOR EACH sumtidtemp WHERE sumtidtemp.TRAAVTAL = kodvar AND 
            sumtidtemp.TRAKTKOD = tillvar:  
               ASSIGN
               sumtidtemp.TRATAGEN = TRUE
               sumtidtemp.TRAKTKOD = ""
               sumtidtemp.TRAKTANTAL = 0.
            END.
            NEXT.
         END.
         ELSE DO:            
            FIND FIRST TRAKTFLER WHERE TRAKTFLER.TRAAVTAL = kodvar AND 
            TRAKTFLER.TRAKTKOD = tillvar  
            USE-INDEX FLTRAKT NO-LOCK NO-ERROR.        
            FOR EACH sumtidtemp WHERE sumtidtemp.TRAAVTAL = kodvar AND 
            sumtidtemp.TRAKTKOD = tillvar:  
               ASSIGN
               sumtidtemp.TRATAGEN = TRUE
               sumtidtemp.VITRAKT = TRAKTATAB.VILART.
               IF FAKTREGLER.TRAKTRGL = "TIDREDOVISNING" THEN DO:
                  sumtidtemp.TBELOPP = sumtidtemp.TRAKTANTAL * TRAKTATAB.ERSATTNING.
               END.
               ELSE DO:                    
                  IF NOT AVAILABLE TRAKTFLER THEN DO:
                     sumtidtemp.TBELOPP = sumtidtemp.TRAKTANTAL * FAKTREGLER.ENTRAK.                
                  END.  
                  ELSE DO: 
                     sumtidtemp.TBELOPP = sumtidtemp.TRAKTANTAL * FAKTREGLER.FLERTRAK.
                  END.       
               END.                     
            END.            
         END.
      END.
      ELSE LEAVE.
   END.

END PROCEDURE.
PROCEDURE tidregtra_UI :
   GET FIRST faktaonrq NO-LOCK.
   DO WHILE AVAILABLE(FAKTAONR):
      FIND FIRST FAKTKOLL WHERE FAKTKOLL.FAKTNR = FAKTPLAN.FAKTNR AND
      FAKTKOLL.AONR = FAKTAONR.AONR AND 
      FAKTKOLL.DELNR = FAKTAONR.DELNR AND FAKTKOLL.SLUTFAKT = FALSE
      USE-INDEX FASTK NO-LOCK NO-ERROR.
      ASSIGN 
      sumpkod = ""
      sumanstf = "".
      IF AVAILABLE FAKTKOLL THEN DO:
         IF FAKTREGLER.TRAKTRGL = "EGET PRIS" OR FAKTREGLER.TRAKTRGL = "TIDREDOVISNING" THEN DO:                     
            IF FAKTKOLL.VECKOKORD NE "" THEN DO:              
               IF FAKTREGLER.SUMALLAR = TRUE THEN DO:
                  IF Guru.Konstanter:globforetag = "GRAN" THEN DO:
                     OPEN QUERY sumtraktq FOR EACH SUMTRAKT WHERE SUMTRAKT.AONR = FAKTAONR.AONR AND 
                     SUMTRAKT.VECKOKORD >= FAKTKOLL.VECKOKORD 
                     USE-INDEX SUMTRAKT NO-LOCK. 
                  END.
                  ELSE DO:
                     OPEN QUERY sumtraktq FOR EACH SUMTRAKT WHERE SUMTRAKT.AONR = FAKTAONR.AONR AND 
                     SUMTRAKT.DELNR = FAKTAONR.DELNR AND SUMTRAKT.VECKOKORD >= FAKTKOLL.VECKOKORD 
                     USE-INDEX SUMTRAKT NO-LOCK. 
                  END.
               END.
               ELSE DO:
                  IF Guru.Konstanter:globforetag = "GRAN" THEN DO:
                     OPEN QUERY sumtraktq FOR EACH SUMTRAKT WHERE SUMTRAKT.AONR = FAKTAONR.AONR AND 
                     SUMTRAKT.VECKOKORD >= FAKTKOLL.VECKOKORD 
                     AND YEAR(SUMTRAKT.DATUM) = YEAR(FILL-IN-TOMDAT)
                     USE-INDEX SUMTRAKT NO-LOCK.             
                  END.
                  ELSE DO:
                     OPEN QUERY sumtraktq FOR EACH SUMTRAKT WHERE SUMTRAKT.AONR = FAKTAONR.AONR AND 
                     SUMTRAKT.DELNR = FAKTAONR.DELNR AND SUMTRAKT.VECKOKORD >= FAKTKOLL.VECKOKORD 
                     AND YEAR(SUMTRAKT.DATUM) = YEAR(FILL-IN-TOMDAT)
                     USE-INDEX SUMTRAKT NO-LOCK.             
                  END.
               END.              
            END.
            ELSE DO: 
               IF FAKTREGLER.SUMALLAR = TRUE THEN DO:
                  IF Guru.Konstanter:globforetag = "GRAN" THEN DO:
                     OPEN QUERY sumtraktq FOR EACH SUMTRAKT WHERE SUMTRAKT.AONR = FAKTAONR.AONR AND 
                     SUMTRAKT.VECKOKORD NE "" 
                     USE-INDEX SUMTRAKT NO-LOCK.                  
                  END.
                  ELSE DO:
                     OPEN QUERY sumtraktq FOR EACH SUMTRAKT WHERE SUMTRAKT.AONR = FAKTAONR.AONR AND 
                     SUMTRAKT.DELNR = FAKTAONR.DELNR AND SUMTRAKT.VECKOKORD NE "" 
                     USE-INDEX SUMTRAKT NO-LOCK.                  
                  END.
               END.
               ELSE DO:
                  IF Guru.Konstanter:globforetag = "GRAN" THEN DO:
                     OPEN QUERY sumtraktq FOR EACH SUMTRAKT WHERE SUMTRAKT.AONR = FAKTAONR.AONR AND 
                     SUMTRAKT.VECKOKORD NE "" 
                     AND YEAR(SUMTRAKT.DATUM) = YEAR(FILL-IN-TOMDAT)
                     USE-INDEX SUMTRAKT NO-LOCK.                   
                  END.
                  ELSE DO:
                     OPEN QUERY sumtraktq FOR EACH SUMTRAKT WHERE SUMTRAKT.AONR = FAKTAONR.AONR AND 
                     SUMTRAKT.DELNR = FAKTAONR.DELNR AND SUMTRAKT.VECKOKORD NE "" 
                     AND YEAR(SUMTRAKT.DATUM) = YEAR(FILL-IN-TOMDAT)
                     USE-INDEX SUMTRAKT NO-LOCK.                   
                  END.
               END.                           
            END.           
            
            GET FIRST sumtraktq NO-LOCK.        
            DO WHILE AVAILABLE(SUMTRAKT):                                         
               IF Guru.Konstanter:globforetag = "ESAN" OR Guru.Konstanter:globforetag = "ESMA" OR 
                  Guru.Konstanter:globforetag = "ETA" THEN DO:
                  IF SUMTRAKT.DATUM < 01/01/2000 THEN musz = TRUE.
               END.                                              
               IF musz = TRUE THEN DO: 
                  musz = FALSE.
               END.
               ELSE DO:
                  IF SUMTRAKT.DATUM <= FILL-IN-TOMDAT THEN DO:  
                     IF FAKTKOLL.SENASTTID = ? THEN RUN sumtrakt_UI.
                     ELSE IF SUMTRAKT.VECKOKORD = FAKTKOLL.VECKOKORD AND 
                     FAKTKOLL.SENASTTID < SUMTRAKT.DATUM THEN RUN sumtrakt_UI.
                     ELSE IF SUMTRAKT.VECKOKORD > FAKTKOLL.VECKOKORD THEN RUN sumtrakt_UI.
                  END.           
               END.            
               GET NEXT sumtraktq NO-LOCK. 
            END.
         END.
         CLOSE QUERY sumtraktq.                    
         ASSIGN 
         sumpkod = ""
         sumanstf = "".
         IF FAKTREGLER.LONRGL = "ENDAST MILERSÄTTNING" THEN DO: 
            IF FAKTKOLL.VECKOKORD NE "" THEN DO:              
               IF FAKTREGLER.SUMALLAR = TRUE THEN DO:
                  IF Guru.Konstanter:globforetag = "GRAN" THEN DO:
                     OPEN QUERY sumlonq FOR EACH SUMLON WHERE SUMLON.AONR = FAKTAONR.AONR AND 
                     SUMLON.VECKOKORD >= FAKTKOLL.VECKOKORD 
                     USE-INDEX SUMLON NO-LOCK.                   
                  END.
                  ELSE DO:
                     OPEN QUERY sumlonq FOR EACH SUMLON WHERE SUMLON.AONR = FAKTAONR.AONR AND 
                     SUMLON.DELNR = FAKTAONR.DELNR AND SUMLON.VECKOKORD >= FAKTKOLL.VECKOKORD 
                     USE-INDEX SUMLON NO-LOCK.                   
                  END.
               END.
               ELSE DO:
                  IF Guru.Konstanter:globforetag = "GRAN" THEN DO:
                     OPEN QUERY sumlonq FOR EACH SUMLON WHERE SUMLON.AONR = FAKTAONR.AONR AND 
                     SUMLON.VECKOKORD >= FAKTKOLL.VECKOKORD 
                     AND YEAR(SUMLON.DATUM) = YEAR(FILL-IN-TOMDAT)
                     USE-INDEX SUMLON NO-LOCK.                    
                  END.
                  ELSE DO:
                     OPEN QUERY sumlonq FOR EACH SUMLON WHERE SUMLON.AONR = FAKTAONR.AONR AND 
                     SUMLON.DELNR = FAKTAONR.DELNR AND SUMLON.VECKOKORD >= FAKTKOLL.VECKOKORD 
                     AND YEAR(SUMLON.DATUM) = YEAR(FILL-IN-TOMDAT)
                     USE-INDEX SUMLON NO-LOCK.                    
                  END.
   
               END.            
            END.
            ELSE DO: 
               IF FAKTREGLER.SUMALLAR = TRUE THEN DO:
                  IF Guru.Konstanter:globforetag = "GRAN" THEN DO:
                     OPEN QUERY sumlonq FOR EACH SUMLON WHERE SUMLON.AONR = FAKTAONR.AONR AND 
                     SUMLON.VECKOKORD NE "" 
                     USE-INDEX SUMLON NO-LOCK.                    
                  END.
                  ELSE DO:
                     OPEN QUERY sumlonq FOR EACH SUMLON WHERE SUMLON.AONR = FAKTAONR.AONR AND 
                     SUMLON.DELNR = FAKTAONR.DELNR AND SUMLON.VECKOKORD NE "" 
                     USE-INDEX SUMLON NO-LOCK.                    
                  END.
               END.
               ELSE DO:
                  IF Guru.Konstanter:globforetag = "GRAN" THEN DO:
                     OPEN QUERY sumlonq FOR EACH SUMLON WHERE SUMLON.AONR = FAKTAONR.AONR AND 
                     SUMLON.VECKOKORD NE ""
                     AND YEAR(SUMLON.DATUM) = YEAR(FILL-IN-TOMDAT) 
                     USE-INDEX SUMLON NO-LOCK.   
                  END.
                  ELSE DO:
                     OPEN QUERY sumlonq FOR EACH SUMLON WHERE SUMLON.AONR = FAKTAONR.AONR AND 
                     SUMLON.DELNR = FAKTAONR.DELNR AND SUMLON.VECKOKORD NE ""
                     AND YEAR(SUMLON.DATUM) = YEAR(FILL-IN-TOMDAT) 
                     USE-INDEX SUMLON NO-LOCK.   
                  END.
               END.                                
            END.           
         
            GET FIRST sumlonq NO-LOCK.        
            DO WHILE AVAILABLE(SUMLON):                                         
               IF Guru.Konstanter:globforetag = "ESAN" OR Guru.Konstanter:globforetag = "ESMA" OR 
                  Guru.Konstanter:globforetag = "ETA" THEN DO:
                  IF SUMLON.DATUM < 01/01/2000 THEN musz = TRUE.
               END.                                              
               IF musz = TRUE THEN DO: 
                  musz = FALSE.
               END.
               ELSE DO:
                  IF SUMLON.DATUM <= FILL-IN-TOMDAT THEN DO:  
                     IF FAKTKOLL.SENASTTID = ? THEN RUN sumlon_UI.
                     ELSE IF SUMLON.VECKOKORD = FAKTKOLL.VECKOKORD AND 
                     FAKTKOLL.SENASTTID < SUMLON.DATUM THEN RUN sumlon_UI.
                     ELSE IF SUMLON.VECKOKORD > FAKTKOLL.VECKOKORD THEN RUN sumlon_UI.                                      
                  END.
               END.            
               GET NEXT sumlonq NO-LOCK. 
            END.
         END.
         CLOSE QUERY sumlonq.                      
      END.
      GET NEXT faktaonrq NO-LOCK.
   END.  
END PROCEDURE.

PROCEDURE sumlon_UI :
   IF kollvecko < SUMLON.VECKOKORD THEN kollvecko = SUMLON.VECKOKORD. 
   IF sumpkod NE SUMLON.PERSONALKOD THEN DO:      
      sumpkod = SUMLON.PERSONALKOD.
      FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = SUMLON.PERSONALKOD
      USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.
      IF AVAILABLE PERSONALTAB THEN DO:
         FIND FIRST ANSTFORMTAB WHERE 
         ANSTFORMTAB.ANSTALLNING = PERSONALTAB.ANSTALLNING
         USE-INDEX ANSTF NO-LOCK NO-ERROR.
         IF NOT AVAILABLE ANSTFORMTAB THEN FIND FIRST ANSTFORMTAB NO-LOCK NO-ERROR.
         sumanstf = ANSTFORMTAB.KOD.
      END.       
      ELSE DO:                
         FIND FIRST BORTPERS WHERE BORTPERS.PERSONALKOD = SUMLON.PERSONALKOD
         USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.
         IF AVAILABLE BORTPERS THEN DO:
            FIND FIRST ANSTFORMTAB WHERE 
            ANSTFORMTAB.ANSTALLNING = BORTPERS.ANSTALLNING
            USE-INDEX ANSTF NO-LOCK NO-ERROR.
            IF NOT AVAILABLE ANSTFORMTAB THEN FIND FIRST ANSTFORMTAB NO-LOCK NO-ERROR.
            sumanstf = ANSTFORMTAB.KOD.
         END.
      END.
   END.   
   FIND FIRST LONTILL WHERE LONTILL.KOD = sumanstf AND
   LONTILL.LONTILLAGG = SUMLON.LONTILLAGG 
   USE-INDEX LON NO-LOCK NO-ERROR.
   IF NOT AVAILABLE LONTILL THEN RETURN.
   
   FIND FIRST sumtidtemp WHERE 
   sumtidtemp.PERSONALKOD = SUMLON.PERSONALKOD AND
   sumtidtemp.AONR = SUMLON.AONR AND
   sumtidtemp.DELNR = SUMLON.DELNR AND 
   sumtidtemp.LONTILLAGG = SUMLON.LONTILLAGG AND
   sumtidtemp.DATUM = SUMLON.DATUM
   USE-INDEX LON NO-ERROR.       
   IF NOT AVAILABLE sumtidtemp THEN DO:
      FIND FIRST sumtidtemp WHERE 
      sumtidtemp.PERSONALKOD = SUMLON.PERSONALKOD AND
      sumtidtemp.AONR = SUMLON.AONR AND
      sumtidtemp.DELNR = SUMLON.DELNR AND 
      sumtidtemp.LONTILLAGG = "" AND
      sumtidtemp.DATUM = SUMLON.DATUM
      USE-INDEX LON NO-ERROR.      
   END.
   IF NOT AVAILABLE sumtidtemp THEN DO:
      CREATE sumtidtemp.
      ASSIGN
      sumtidtemp.START = 0
      sumtidtemp.SLUT = 0
      sumtidtemp.GSTART = 0 
      sumtidtemp.GSLUT = 0.      
      FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = SUMLON.PERSONALKOD
      USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.
      IF AVAILABLE PERSONALTAB THEN DO: 
         ASSIGN             
         sumtidtemp.PERSMASK = PERSONALTAB.PERSMASK
         sumtidtemp.BEFATTNING = PERSONALTAB.BEFATTNING         
         sumtidtemp.NAMN = SUBSTRING(PERSONALTAB.FORNAMN,1,1) + "." + PERSONALTAB.EFTERNAMN. 
         Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," + PERSONALTAB.PERSONALKOD.  
      END.
   END.
   
   ASSIGN
   sumtidtemp.DATUM = SUMLON.DATUM
   sumtidtemp.PERSONALKOD = SUMLON.PERSONALKOD
   sumtidtemp.AONR = SUMLON.AONR
   sumtidtemp.DELNR = SUMLON.DELNR 
   sumtidtemp.LONTILLAGG = SUMLON.LONTILLAGG
   sumtidtemp.VILART = LONTILL.VILART
   sumtidtemp.MED = TRUE.
   IF LONTILL.ENHET = "Km" THEN antalvar = 0.1.
   ELSE antalvar = 1. 
   IF LONTILL.ENHET = "Km" OR LONTILL.ENHET = "MIL" THEN DO:
      sumtidtemp.LONKOST = sumtidtemp.LONKOST + SUMLON.LONTILLANTAL * antalvar * FAKTREGLER.MIL.     
      sumtidtemp.LONTILLANTAL = sumtidtemp.LONTILLANTAL + (SUMLON.LONTILLANTAL * antalvar).
   END.
   ELSE DO:
      lonantal = SUMLON.LONTILLANTAL.
      IF LONTILL.ENHET = "TI" THEN DO:                                   
         nytid = lonantal.
         RUN TIMSEK.P.
         lonantal = (sekunder / 3600).
      END.
      sumtidtemp.LONKOST = sumtidtemp.LONKOST + (lonantal * LONTILL.ERSATTNING).                          
      sumtidtemp.LONTILLANTAL = sumtidtemp.LONTILLANTAL + lonantal.
   END.
   
   {GDPRLOGGCLIENT.I}
END PROCEDURE.

PROCEDURE sumtrakt_UI :
   IF kollvecko < SUMTRAKT.VECKOKORD THEN kollvecko = SUMTRAKT.VECKOKORD. 
   IF sumpkod NE SUMTRAKT.PERSONALKOD THEN DO:      
      sumpkod = SUMTRAKT.PERSONALKOD.
      FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = SUMTRAKT.PERSONALKOD
      USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.
      IF AVAILABLE PERSONALTAB THEN DO:
         sumanstf = PERSONALTAB.TRAAVTAL.
      END.       
      ELSE DO:                
      END.
   END.      
   FIND FIRST TRAKTATAB WHERE TRAKTATAB.TRAAVTAL = sumanstf AND
   TRAKTATAB.TRAKTKOD = SUMTRAKT.TRAKTKOD USE-INDEX TRAKTKOD NO-LOCK NO-ERROR.
   IF NOT AVAILABLE TRAKTATAB THEN RETURN.
   FIND FIRST sumtidtemp WHERE 
   sumtidtemp.PERSONALKOD = SUMTRAKT.PERSONALKOD AND
   sumtidtemp.AONR = SUMTRAKT.AONR AND
   sumtidtemp.DELNR = SUMTRAKT.DELNR AND 
   sumtidtemp.TRAKTKOD = SUMTRAKT.TRAKTKOD AND
   sumtidtemp.DATUM = SUMTRAKT.DATUM
   USE-INDEX TRAKT NO-ERROR.       
   IF NOT AVAILABLE sumtidtemp THEN DO:
      FIND FIRST sumtidtemp WHERE 
      sumtidtemp.PERSONALKOD = SUMTRAKT.PERSONALKOD AND
      sumtidtemp.AONR = SUMTRAKT.AONR AND
      sumtidtemp.DELNR = SUMTRAKT.DELNR AND 
      sumtidtemp.DATUM = SUMTRAKT.DATUM AND
      sumtidtemp.TRAKTKOD = ""
      USE-INDEX TRAKT NO-ERROR.      
   END.
   IF NOT AVAILABLE sumtidtemp THEN DO:
      CREATE sumtidtemp.      
      ASSIGN
      sumtidtemp.START = 0
      sumtidtemp.SLUT = 0
      sumtidtemp.GSTART = 0 
      sumtidtemp.GSLUT = 0.      
      FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = SUMTRAKT.PERSONALKOD
      USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.
      IF AVAILABLE PERSONALTAB THEN DO: 
         ASSIGN
         sumtidtemp.PERSMASK = PERSONALTAB.PERSMASK
         sumtidtemp.BEFATTNING = PERSONALTAB.BEFATTNING         
         sumtidtemp.NAMN = SUBSTRING(PERSONALTAB.FORNAMN,1,1) + "." + PERSONALTAB.EFTERNAMN.   
         Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," + PERSONALTAB.PERSONALKOD.
      END.
   END.   
   ASSIGN
   sumtidtemp.DATUM = SUMTRAKT.DATUM
   sumtidtemp.PERSONALKOD = SUMTRAKT.PERSONALKOD
   sumtidtemp.AONR = SUMTRAKT.AONR
   sumtidtemp.DELNR = SUMTRAKT.DELNR 
   sumtidtemp.TRAKTKOD = SUMTRAKT.TRAKTKOD
   sumtidtemp.VITRAKT = TRAKTATAB.VILART 
   sumtidtemp.TRAKTANTAL = sumtidtemp.TRAKTANTAL + SUMTRAKT.TRAKTANTAL
   sumtidtemp.MED = TRUE. 
   IF FAKTREGLER.TRAKTRGL = "TIDREDOVISNING" THEN DO:
      sumtidtemp.TBELOPP = sumtidtemp.TBELOPP + 
      SUMTRAKT.TRAKTANTAL * TRAKTATAB.ERSATTNING.
   END.
   ELSE DO:                    
      IF SUMTRAKT.ENDAGS = TRUE THEN DO:
         sumtidtemp.TBELOPP = sumtidtemp.TBELOPP + 
         SUMTRAKT.TRAKTANTAL * FAKTREGLER.ENTRAK.                
      END.  
      ELSE DO: 
         sumtidtemp.TBELOPP = sumtidtemp.TBELOPP + 
         SUMTRAKT.TRAKTANTAL * FAKTREGLER.FLERTRAK.
      END. 
   END. 
   
   {GDPRLOGGCLIENT.I}     
END PROCEDURE.

