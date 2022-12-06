   /*AOPSUMMAU2.I*/   
   ASSIGN   
   arrhjsum = 0   
   arrhjsumtid = 0  
   arrhjsumotid = 0    
   arrhjsumove = 0    
   arrhjsumtra = 0       
   arrhjsumlon = 0.
   CREATE indertemp. 
   IF RAD_PERIOD = 1 THEN DO:       
        
   END.
   IF RAD_PERIOD = 2 OR RAD_PERIOD = 3 THEN DO:
      EMPTY TEMP-TABLE bytaao NO-ERROR.       
      CREATE bytaao.
      ASSIGN
      bytaao.VADGORA = 4
      bytaao.STDATUM = bdatum
      bytaao.SLDATUM = avdatum
      bytaao.ARTAL = YEAR(bdatum)
      bytaao.RAD_PERIOD = RAD_PERIOD
      bytaao.AONR = aonummer
      bytaao.DELNR = delnummer
      bytaao.GLOBFORETAG = Guru.Konstanter:globforetag
      bytaao.INDER = inder
      bytaao.SOK1 = visa.UT
      bytaao.SOK2 = kollvecka
      bytaao.SOK5 = valdelnrlog.
      RUN SKAPAOBY.P(INPUT-OUTPUT TABLE bytaao, INPUT-OUTPUT TABLE dagtemp, INPUT-OUTPUT TABLE indertemp , INPUT-OUTPUT TABLE restid, OUTPUT bytaonrmed, INPUT-OUTPUT TABLE tidut).
      FIND FIRST indertemp NO-ERROR.
      IF RAD_PERIOD = 2 THEN DO:                  
         IF uppvaltemp.VISGODKANDA = FALSE THEN DO:         
            IF valdelnrlog = FALSE THEN DO: 
               OPEN QUERY dagsumq FOR EACH SUMTIDDAG WHERE SUMTIDDAG.DATUM >= bdatum AND
               SUMTIDDAG.DATUM <= avdatum AND SUMTIDDAG.AONR = aonummer AND 
               SUMTIDDAG.DELNR = delnummer USE-INDEX AONR NO-LOCK.
            END.
            ELSE DO:
               OPEN QUERY dagsumq FOR EACH SUMTIDDAG WHERE 
               SUMTIDDAG.DATUM >= bdatum AND
               SUMTIDDAG.DATUM <= avdatum AND SUMTIDDAG.AONR = aonummer  
               USE-INDEX AONR NO-LOCK.            
            END.      
         END.
         ELSE DO:
            IF valdelnrlog = FALSE THEN DO: 
               OPEN QUERY dagsumq FOR EACH SUMTIDDAG WHERE SUMTIDDAG.DATUM >= bdatum AND
               SUMTIDDAG.DATUM <= avdatum AND SUMTIDDAG.AONR = aonummer AND 
               SUMTIDDAG.DELNR = delnummer AND SUMTIDDAG.VECKOKORD NE "" USE-INDEX AONR NO-LOCK.
            END.
            ELSE DO:
               OPEN QUERY dagsumq FOR EACH SUMTIDDAG WHERE 
               SUMTIDDAG.DATUM >= bdatum AND
               SUMTIDDAG.DATUM <= avdatum AND SUMTIDDAG.AONR = aonummer  
               AND SUMTIDDAG.VECKOKORD NE "" USE-INDEX AONR NO-LOCK.            
            END.      
         END.
         GET FIRST dagsumq NO-LOCK.
         DO WHILE AVAILABLE(SUMTIDDAG) TRANSACTION:
            
            {ANVOMRSEKKOLL2.I}
            IF sealltid = TRUE THEN tisek = TRUE.
            IF vallista = 54 THEN tisek = TRUE.
            IF tisek = TRUE THEN DO:
               berindvar = 1.
               IF SUMTIDDAG.OMRADE = SUMTIDDAG.GEOMRADE AND inder = TRUE THEN DO:
                  IF SUMTIDDAG.DATUM >= 01/01/99 THEN berindvar = 1.15.
                  ELSE IF SUMTIDDAG.DATUM < 01/01/99 THEN berindvar = 0.70.
                  indertemp.INDEREKT = indertemp.INDEREKT + SUMTIDDAG.IKOSTNAD * berindvar.           
               END. 
               IF SUMTIDDAG.PRISTYP = "RESTID..." THEN DO: 
                  FIND FIRST restid WHERE restid.AONR = SUMTIDDAG.AONR AND
                  restid.DELNR = SUMTIDDAG.DELNR AND restid.PERSONALKOD = SUMTIDDAG.PERSONALKOD 
                  USE-INDEX AONR EXCLUSIVE-LOCK NO-ERROR.
                  IF NOT AVAILABLE restid THEN CREATE restid.
                  ASSIGN          
                  restid.AONR = SUMTIDDAG.AONR
                  restid.DELNR = SUMTIDDAG.DELNR
                  restid.PERSONALKOD = SUMTIDDAG.PERSONALKOD
                  restid.OMRADE = SUMTIDDAG.OMRADE 
                  restid.TIMMAR = restid.TIMMAR + SUMTIDDAG.TIMMAR.
                  CREATE dagtemp.
                  ASSIGN          
                  dagtemp.AONR = SUMTIDDAG.AONR
                  dagtemp.DELNR = SUMTIDDAG.DELNR 
                  dagtemp.PERSONALKOD = SUMTIDDAG.PERSONALKOD 
                  dagtemp.NAMN = SUBSTRING(SUMTIDDAG.FORNAMN,1,1) + "." + 
                  SUBSTRING(SUMTIDDAG.EFTERNAMN,1,3)
                  dagtemp.POMRADE = SUMTIDDAG.OMRADE
                  dagtemp.GEOMRADE = SUMTIDDAG.GEOMRADE 
                  dagtemp.OMRADE = SUMTIDDAG.OMRADE 
                  dagtemp.OTIMMAR = SUMTIDDAG.OTIMMAR 
                  dagtemp.BELOPP = SUMTIDDAG.BELOPP 
                  dagtemp.OBELOPP = SUMTIDDAG.OBELOPP 
                  dagtemp.TBELOPP = SUMTIDDAG.TBELOPP
                  dagtemp.LONKOST = SUMTIDDAG.LONKOST
                  dagtemp.IKOST = SUMTIDDAG.IKOSTNAD * berindvar
                  dagtemp.PRIS = SUMTIDDAG.PRIS
                  dagtemp.PRISTYP = SUMTIDDAG.PRISTYP
                  dagtemp.PRISI = SUMTIDDAG.PRISI
                  dagtemp.DATUM = SUMTIDDAG.DATUM.
               END.
               ELSE DO:
                  CREATE dagtemp.
                  ASSIGN          
                  dagtemp.AONR = SUMTIDDAG.AONR
                  dagtemp.DELNR = SUMTIDDAG.DELNR 
                  dagtemp.PERSONALKOD = SUMTIDDAG.PERSONALKOD 
                  dagtemp.NAMN = SUBSTRING(SUMTIDDAG.FORNAMN,1,1) + "." + 
                  SUBSTRING(SUMTIDDAG.EFTERNAMN,1,3)
                  dagtemp.GEOMRADE = SUMTIDDAG.GEOMRADE 
                  dagtemp.POMRADE = SUMTIDDAG.OMRADE
                  dagtemp.OMRADE = SUMTIDDAG.OMRADE 
                  dagtemp.TIMMAR = SUMTIDDAG.TIMMAR
                  dagtemp.OTIMMAR = SUMTIDDAG.OTIMMAR 
                  dagtemp.BELOPP = SUMTIDDAG.BELOPP 
                  dagtemp.OBELOPP = SUMTIDDAG.OBELOPP 
                  dagtemp.TBELOPP = SUMTIDDAG.TBELOPP
                  dagtemp.LONKOST = SUMTIDDAG.LONKOST
                  dagtemp.IKOST = SUMTIDDAG.IKOSTNAD * berindvar
                  dagtemp.PRIS = SUMTIDDAG.PRIS
                  dagtemp.PRISTYP = SUMTIDDAG.PRISTYP
                  dagtemp.PRISI = SUMTIDDAG.PRISI
                  dagtemp.DATUM = SUMTIDDAG.DATUM.        
               END.
            END.   
            GET NEXT dagsumq NO-LOCK. 
         END.      
      END. 
      IF RAD_PERIOD = 3 THEN DO:                 
         IF uppvaltemp.VISGODKANDA = FALSE THEN DO:         
            IF valdelnrlog = FALSE THEN DO: 
               OPEN QUERY dagsumq FOR EACH SUMTIDDAG WHERE SUMTIDDAG.AONR = aonummer AND 
               SUMTIDDAG.DELNR = delnummer USE-INDEX AONR NO-LOCK.
            END.
            ELSE DO:
               OPEN QUERY dagsumq FOR EACH SUMTIDDAG WHERE SUMTIDDAG.AONR = aonummer  
               USE-INDEX AONR NO-LOCK.            
            END.    
         END.
         ELSE DO:
            IF valdelnrlog = FALSE THEN DO: 
               OPEN QUERY dagsumq FOR EACH SUMTIDDAG WHERE SUMTIDDAG.AONR = aonummer AND 
               SUMTIDDAG.DELNR = delnummer AND SUMTIDDAG.VECKOKORD NE "" USE-INDEX AONR NO-LOCK.
            END.
            ELSE DO:
               OPEN QUERY dagsumq FOR EACH SUMTIDDAG WHERE SUMTIDDAG.AONR = aonummer  
               AND SUMTIDDAG.VECKOKORD NE "" USE-INDEX AONR NO-LOCK.            
            END.    
         END.
         GET FIRST dagsumq NO-LOCK.
         DO WHILE AVAILABLE(SUMTIDDAG) TRANSACTION:
            berindvar = 1.
            IF SUMTIDDAG.OMRADE = SUMTIDDAG.GEOMRADE AND inder = TRUE THEN DO:
               IF SUMTIDDAG.DATUM >= 01/01/99 THEN berindvar = 1.15.
               ELSE IF SUMTIDDAG.DATUM < 01/01/99 THEN berindvar = 0.70.
               indertemp.INDEREKT = indertemp.INDEREKT + SUMTIDDAG.IKOSTNAD * berindvar.           
            END. 
            IF SUMTIDDAG.PRISTYP = "RESTID..." THEN DO: 
               FIND FIRST restid WHERE restid.AONR = SUMTIDDAG.AONR AND
               restid.DELNR = SUMTIDDAG.DELNR AND restid.PERSONALKOD = SUMTIDDAG.PERSONALKOD 
               USE-INDEX AONR EXCLUSIVE-LOCK NO-ERROR.
               IF NOT AVAILABLE restid THEN CREATE restid.
               ASSIGN          
               restid.AONR = SUMTIDDAG.AONR
               restid.DELNR = SUMTIDDAG.DELNR
               restid.PERSONALKOD = SUMTIDDAG.PERSONALKOD
               restid.OMRADE = SUMTIDDAG.OMRADE 
               restid.TIMMAR = restid.TIMMAR + SUMTIDDAG.TIMMAR.
               CREATE dagtemp.
               ASSIGN          
               dagtemp.AONR = SUMTIDDAG.AONR
               dagtemp.DELNR = SUMTIDDAG.DELNR 
               dagtemp.PERSONALKOD = SUMTIDDAG.PERSONALKOD 
               dagtemp.NAMN = SUBSTRING(SUMTIDDAG.FORNAMN,1,1) + "." + 
               SUBSTRING(SUMTIDDAG.EFTERNAMN,1,3)
               dagtemp.GEOMRADE = SUMTIDDAG.GEOMRADE 
               dagtemp.POMRADE = SUMTIDDAG.OMRADE
               dagtemp.OMRADE = SUMTIDDAG.OMRADE 
               dagtemp.OTIMMAR = SUMTIDDAG.OTIMMAR 
               dagtemp.BELOPP = SUMTIDDAG.BELOPP 
               dagtemp.OBELOPP = SUMTIDDAG.OBELOPP 
               dagtemp.TBELOPP = SUMTIDDAG.TBELOPP
               dagtemp.LONKOST = SUMTIDDAG.LONKOST
               dagtemp.IKOST = SUMTIDDAG.IKOSTNAD * berindvar
               dagtemp.PRIS = SUMTIDDAG.PRIS
               dagtemp.PRISTYP = SUMTIDDAG.PRISTYP
               dagtemp.PRISI = SUMTIDDAG.PRISI
               dagtemp.DATUM = SUMTIDDAG.DATUM.
            END.
            ELSE DO:
               CREATE dagtemp.
               ASSIGN          
               dagtemp.AONR = SUMTIDDAG.AONR
               dagtemp.DELNR = SUMTIDDAG.DELNR 
               dagtemp.PERSONALKOD = SUMTIDDAG.PERSONALKOD 
               dagtemp.NAMN = SUBSTRING(SUMTIDDAG.FORNAMN,1,1) + "." + 
               SUBSTRING(SUMTIDDAG.EFTERNAMN,1,3)
               dagtemp.GEOMRADE = SUMTIDDAG.GEOMRADE 
               dagtemp.POMRADE = SUMTIDDAG.OMRADE
               dagtemp.OMRADE = SUMTIDDAG.OMRADE 
               dagtemp.TIMMAR = SUMTIDDAG.TIMMAR
               dagtemp.OTIMMAR = SUMTIDDAG.OTIMMAR 
               dagtemp.BELOPP = SUMTIDDAG.BELOPP 
               dagtemp.OBELOPP = SUMTIDDAG.OBELOPP 
               dagtemp.TBELOPP = SUMTIDDAG.TBELOPP
               dagtemp.LONKOST = SUMTIDDAG.LONKOST
               dagtemp.IKOST = SUMTIDDAG.IKOSTNAD * berindvar
               dagtemp.PRIS = SUMTIDDAG.PRIS
               dagtemp.PRISTYP = SUMTIDDAG.PRISTYP
               dagtemp.PRISI = SUMTIDDAG.PRISI
               dagtemp.DATUM = SUMTIDDAG.DATUM.        
            END.
            GET NEXT dagsumq NO-LOCK. 
         END.      
      END.
   END.  
   {DAGTEMPBOLAG2.I}
   IF valdelnrlog = TRUE THEN DO: 
      ASSIGN   
      arrhjsum = 0   
      arrhjsumtid = 0  
      arrhjsumotid = 0    
      arrhjsumove = 0    
      arrhjsumtra = 0       
      arrhjsumlon = 0.          
      FOR EACH dagtemp
      BREAK BY dagtemp.AONR BY dagtemp.DELNR: 
         ACCUMULATE 
         dagtemp.BELOPP (TOTAL BY dagtemp.AONR BY dagtemp.DELNR). 
         ACCUMULATE 
         dagtemp.TIMMAR (TOTAL BY dagtemp.AONR BY dagtemp.DELNR). 
         ACCUMULATE 
         dagtemp.OTIMMAR (TOTAL BY dagtemp.AONR BY dagtemp.DELNR). 
         ACCUMULATE 
         dagtemp.OBELOPP (TOTAL BY dagtemp.AONR BY dagtemp.DELNR).  
         ACCUMULATE 
         dagtemp.TBELOPP (TOTAL BY dagtemp.AONR BY dagtemp.DELNR). 
         ACCUMULATE 
         dagtemp.LONKOST (TOTAL BY dagtemp.AONR BY dagtemp.DELNR).   
         ACCUMULATE 
         dagtemp.IKOST (TOTAL BY dagtemp.AONR BY dagtemp.DELNR).
         IF LAST-OF(dagtemp.DELNR) THEN DO TRANSACTION:
            CREATE slutsum2.
            ASSIGN 
            slutsum2.AONR = dagtemp.AONR
            slutsum2.DELNR = dagtemp.DELNR  
            /*slutsum2.GEOMRADE = dagtemp.GEOMRADE  
            slutsum2.OMRADE = dagtemp.OMRADE*/ 
            slutsum2.PERSONALKOD = dagtemp.PERSONALKOD 
            slutsum2.NAMN = dagtemp.NAMN
            slutsum2.BELOPP = (ACCUM TOTAL dagtemp.BELOPP) - arrhjsum                       
            slutsum2.TIMMAR = (ACCUM TOTAL dagtemp.TIMMAR) - arrhjsumtid 
            slutsum2.OTIMMAR = (ACCUM TOTAL dagtemp.OTIMMAR) - arrhjsumotid
            slutsum2.OBELOPP = (ACCUM TOTAL dagtemp.OBELOPP) - arrhjsumove    
            slutsum2.TBELOPP = (ACCUM TOTAL dagtemp.TBELOPP) - arrhjsumtra 
            slutsum2.LONKOST = (ACCUM TOTAL dagtemp.LONKOST) - arrhjsumlon
            slutsum2.IKOST = (ACCUM TOTAL dagtemp.IKOST) - arrhjsumikost.            
            arrhjsum = ACCUM TOTAL dagtemp.BELOPP.  
            arrhjsumtid = ACCUM TOTAL dagtemp.TIMMAR. 
            arrhjsumotid = ACCUM TOTAL dagtemp.OTIMMAR.
            arrhjsumove = ACCUM TOTAL dagtemp.OBELOPP.  
            arrhjsumtra = ACCUM TOTAL dagtemp.TBELOPP.       
            arrhjsumlon = ACCUM TOTAL dagtemp.LONKOST. 
            arrhjsumikost = ACCUM TOTAL dagtemp.IKOST.              
         END.                                                   
      END.                     
      FOR EACH dagtemp:
         ASSIGN dagtemp.DELNR = 0.        
      END.
   END.                 
   ASSIGN     
   arrhjsumikost = 0
   arrhjsum = 0   
   arrhjsumtid = 0  
   arrhjsumotid = 0    
   arrhjsumove = 0    
   arrhjsumtra = 0       
   arrhjsumlon = 0.   
   FOR EACH dagtemp
   BREAK BY dagtemp.AONR BY dagtemp.DELNR BY dagtemp.OMRADE BY dagtemp.PERSONALKOD: 
      ACCUMULATE 
      dagtemp.BELOPP (TOTAL BY dagtemp.AONR BY dagtemp.DELNR BY dagtemp.OMRADE BY dagtemp.PERSONALKOD). 
      ACCUMULATE 
      dagtemp.TIMMAR (TOTAL BY dagtemp.AONR BY dagtemp.DELNR BY dagtemp.OMRADE BY dagtemp.PERSONALKOD). 
      ACCUMULATE 
      dagtemp.OTIMMAR (TOTAL BY dagtemp.AONR BY dagtemp.DELNR BY dagtemp.OMRADE BY dagtemp.PERSONALKOD). 
      ACCUMULATE 
      dagtemp.OBELOPP (TOTAL BY dagtemp.AONR BY dagtemp.DELNR BY dagtemp.OMRADE BY dagtemp.PERSONALKOD).  
      ACCUMULATE 
      dagtemp.TBELOPP (TOTAL BY dagtemp.AONR BY dagtemp.DELNR BY dagtemp.OMRADE BY dagtemp.PERSONALKOD). 
      ACCUMULATE 
      dagtemp.LONKOST (TOTAL BY dagtemp.AONR BY dagtemp.DELNR BY dagtemp.OMRADE BY dagtemp.PERSONALKOD). 
      ACCUMULATE 
      dagtemp.IKOST (TOTAL BY dagtemp.AONR BY dagtemp.DELNR BY dagtemp.OMRADE BY dagtemp.PERSONALKOD).
      IF LAST-OF(dagtemp.PERSONALKOD) THEN DO TRANSACTION:
         CREATE slutsum.
         ASSIGN 
         slutsum.AONR = dagtemp.AONR
         slutsum.DELNR = dagtemp.DELNR  
         slutsum.GEOMRADE = dagtemp.GEOMRADE  
         slutsum.OMRADE = dagtemp.OMRADE 
         slutsum.PERSONALKOD = dagtemp.PERSONALKOD 
         slutsum.NAMN = dagtemp.NAMN
         slutsum.BELOPP = (ACCUM TOTAL dagtemp.BELOPP) - arrhjsum                       
         slutsum.TIMMAR = (ACCUM TOTAL dagtemp.TIMMAR) - arrhjsumtid 
         slutsum.OTIMMAR = (ACCUM TOTAL dagtemp.OTIMMAR) - arrhjsumotid
         slutsum.OBELOPP = (ACCUM TOTAL dagtemp.OBELOPP) - arrhjsumove    
         slutsum.TBELOPP = (ACCUM TOTAL dagtemp.TBELOPP) - arrhjsumtra 
         slutsum.LONKOST = (ACCUM TOTAL dagtemp.LONKOST) - arrhjsumlon
         slutsum.IKOST = (ACCUM TOTAL dagtemp.IKOST) - arrhjsumikost.               
         arrhjsum = ACCUM TOTAL dagtemp.BELOPP.  
         arrhjsumtid = ACCUM TOTAL dagtemp.TIMMAR. 
         arrhjsumotid = ACCUM TOTAL dagtemp.OTIMMAR.
         arrhjsumove = ACCUM TOTAL dagtemp.OBELOPP.  
         arrhjsumtra = ACCUM TOTAL dagtemp.TBELOPP.       
         arrhjsumlon = ACCUM TOTAL dagtemp.LONKOST. 
         arrhjsumikost = ACCUM TOTAL dagtemp.IKOST.                
      END.     
   END.                                                                  
   ASSIGN 
   arrhjsumikost = 0    
   arrhjsum = 0   
   arrhjsumtid = 0  
   arrhjsumotid = 0    
   arrhjsumove = 0    
   arrhjsumtra = 0       
   arrhjsumlon = 0.      
   FOR EACH slutsum NO-LOCK BREAK BY slutsum.OMRADE: 
      ACCUMULATE 
      slutsum.BELOPP (TOTAL BY slutsum.OMRADE). 
      ACCUMULATE 
      slutsum.TIMMAR (TOTAL BY slutsum.OMRADE).
      ACCUMULATE 
      slutsum.OTIMMAR (TOTAL BY slutsum.OMRADE).
      ACCUMULATE 
      slutsum.OBELOPP (TOTAL BY slutsum.OMRADE).  
      ACCUMULATE 
      slutsum.TBELOPP (TOTAL BY slutsum.OMRADE).   
      ACCUMULATE 
      slutsum.LONKOST (TOTAL BY slutsum.OMRADE).   
      ACCUMULATE 
      slutsum.IKOST (TOTAL BY slutsum.OMRADE).  
      IF LAST-OF(slutsum.OMRADE) THEN DO TRANSACTION:         
         CREATE slutsum1.
         ASSIGN 
         slutsum1.AONR = slutsum.AONR
         slutsum1.DELNR = slutsum.DELNR  
         slutsum1.GEOMRADE = slutsum.GEOMRADE  
         slutsum1.OMRADE = slutsum.OMRADE 
         slutsum1.PERSONALKOD = slutsum.PERSONALKOD 
         slutsum1.NAMN = slutsum.NAMN 
         slutsum1.BELOPP = (ACCUM TOTAL slutsum.BELOPP) - arrhjsum                       
         slutsum1.TIMMAR = (ACCUM TOTAL slutsum.TIMMAR) - arrhjsumtid 
         slutsum1.OTIMMAR = (ACCUM TOTAL slutsum.OTIMMAR) - arrhjsumotid
         slutsum1.OBELOPP = (ACCUM TOTAL slutsum.OBELOPP) - arrhjsumove    
         slutsum1.TBELOPP = (ACCUM TOTAL slutsum.TBELOPP) - arrhjsumtra 
         slutsum1.LONKOST = (ACCUM TOTAL slutsum.LONKOST) - arrhjsumlon.
         slutsum1.IKOST = (ACCUM TOTAL slutsum.IKOST) - arrhjsumikost.       
         arrhjsum = ACCUM TOTAL slutsum.BELOPP.  
         arrhjsumtid = ACCUM TOTAL slutsum.TIMMAR. 
         arrhjsumotid = ACCUM TOTAL slutsum.OTIMMAR.
         arrhjsumove = ACCUM TOTAL slutsum.OBELOPP.  
         arrhjsumtra = ACCUM TOTAL slutsum.TBELOPP.         
         arrhjsumlon = ACCUM TOTAL slutsum.LONKOST.
         arrhjsumikost = ACCUM TOTAL slutsum.IKOST.                                  
      END.                 
   END.
   arrhjrestid = 0.       
   FOR EACH restid NO-LOCK BREAK BY restid.OMRADE:       
      ACCUMULATE 
      restid.TIMMAR (TOTAL BY restid.OMRADE).      
      IF LAST-OF(restid.OMRADE) THEN DO TRANSACTION:         
         CREATE restid2.
         ASSIGN                 
         restid2.OMRADE = restid.OMRADE                                
         restid2.TIMMAR = (ACCUM TOTAL restid.TIMMAR) - arrhjrestid.                 
         arrhjrestid = ACCUM TOTAL restid.TIMMAR.                                   
      END.                 
   END. 
   arrhjrestid = 0.        
   FOR EACH restid NO-LOCK BREAK BY restid.AONR BY restid.DELNR:       
      ACCUMULATE 
      restid.TIMMAR (TOTAL BY restid.AONR BY restid.DELNR).      
      IF LAST-OF(restid.DELNR) THEN DO TRANSACTION:         
         CREATE restid3.
         ASSIGN                 
         restid3.AONR = restid.AONR   
         restid3.DELNR = restid.DELNR                                 
         restid3.TIMMAR = (ACCUM TOTAL restid.TIMMAR) - arrhjrestid.                 
         arrhjrestid = ACCUM TOTAL restid.TIMMAR.                                   
      END.                 
   END.        
   arrhjrestid = 0.                                                                                                                  
   CREATE slutsum1.  
   ASSIGN 
   slutsum1.AONR = "SUMMA"
   slutsum1.BELOPP = arrhjsum                       
   slutsum1.TIMMAR = arrhjsumtid 
   slutsum1.OTIMMAR = arrhjsumotid
   slutsum1.OBELOPP = arrhjsumove    
   slutsum1.TBELOPP = arrhjsumtra
   slutsum1.LONKOST = arrhjsumlon.     
   FOR EACH slutsum1 USE-INDEX OMR NO-LOCK:                          
      IF slutsum1.AONR = "SUMMA" THEN NEXT.
      CREATE tidut.      
      IF slutsum1.AONR = "SUMMA" THEN musz = musz.
      ELSE FIND FIRST restid2 WHERE restid2.OMRADE = slutsum1.OMRADE 
      USE-INDEX OMR NO-LOCK NO-ERROR.           
      ASSIGN         
      SUBSTRING(tidut.UT,12) = slutsum1.OMRADE                          
      SUBSTRING(tidut.UT,31) = STRING(slutsum1.TIMMAR,">>>>99")   
      SUBSTRING(tidut.UT,38) = STRING(slutsum1.BELOPP,">>>>>99")
      SUBSTRING(tidut.UT,46) = STRING(slutsum1.OTIMMAR,">>>>99")  
      SUBSTRING(tidut.UT,53) = STRING(slutsum1.OBELOPP,">>>>>99")
      SUBSTRING(tidut.UT,66) = STRING(slutsum1.TBELOPP,">>>>>99")   
      SUBSTRING(tidut.UT,74) = STRING(slutsum1.LONKOST,"->>>>99").                  
      IF slutsum1.AONR = "SUMMA" THEN DO:
         ASSIGN
         SUBSTRING(tidut.UT,1) = slutsum1.AONR.
         IF arrhjrestid > 0 THEN SUBSTRING(tidut.UT,61) = STRING(arrhjrestid,">>99"). 
         IF inder = TRUE THEN DO:  
            IF indertemp.INDEREKT > 0 THEN                                              
            SUBSTRING(tidut.UT,82) = STRING(indertemp.INDEREKT,">>>>>99").
         END.                     
      END.  
      ELSE  IF slutsum1.OMRADE = slutsum1.GEOMRADE AND inder = TRUE THEN DO:
         IF slutsum1.BELOPP + slutsum1.LONKOST > 0 THEN 
         SUBSTRING(tidut.UT,82) =                            
         STRING(slutsum1.IKOST,">>>>>99").         
      END.    
      IF AVAILABLE restid2 THEN DO:                                 
         ASSIGN                                         
         SUBSTRING(tidut.UT,61) = STRING(restid2.TIMMAR,">>99")
         arrhjrestid = arrhjrestid + restid2.TIMMAR  
         arrhjsumtid = arrhjsumtid + restid2.TIMMAR.                  
      END.   
   END.                                    
   FOR EACH slutsum1 WHERE slutsum1.AONR = "SUMMA" USE-INDEX OMR NO-LOCK:                          
      CREATE tidut.                     
      ASSIGN         
      SUBSTRING(tidut.UT,12) = slutsum1.OMRADE                          
      SUBSTRING(tidut.UT,31) = STRING(slutsum1.TIMMAR,">>>>99")   
      SUBSTRING(tidut.UT,38) = STRING(slutsum1.BELOPP,">>>>>99")
      SUBSTRING(tidut.UT,46) = STRING(slutsum1.OTIMMAR,">>>>99")  
      SUBSTRING(tidut.UT,53) = STRING(slutsum1.OBELOPP,">>>>>99")
      SUBSTRING(tidut.UT,66) = STRING(slutsum1.TBELOPP,">>>>>99")   
      SUBSTRING(tidut.UT,74) = STRING(slutsum1.LONKOST,"->>>>99").                  
      ASSIGN
      SUBSTRING(tidut.UT,1) = slutsum1.AONR.
      IF arrhjrestid > 0 THEN SUBSTRING(tidut.UT,61) = STRING(arrhjrestid,">>99"). 
      IF inder = TRUE THEN DO:  
         IF indertemp.INDEREKT > 0 THEN                                              
         SUBSTRING(tidut.UT,82) = STRING(indertemp.INDEREKT,">>>>>99").
      END.                             
   END.      
   DO TRANSACTION:    
      CREATE tidut.
      CREATE tidut.   
      ASSIGN tidut.UT = str2.        
      CREATE tidut.   
      CREATE tidut.   
      ASSIGN tidut.UT = "TOTALT".        
      CREATE tidut.   
      CREATE tidut.
      ASSIGN                  
      SUBSTRING(tidut.UT,1) = "TOTALT ANTAL TIMMAR:"    
      SUBSTRING(tidut.UT,22) = STRING(arrhjsumtid + arrhjsumotid,">>>>>>99").
      CREATE tidut.
      ASSIGN                   
      SUBSTRING(tidut.UT,1) = "TOTAL KOSTNAD       :"    
      SUBSTRING(tidut.UT,22) = 
      STRING(arrhjsum + arrhjsumove + arrhjsumtra + arrhjsumlon + 
      indertemp.INDEREKT,">>>>>>99").               
   END.                                                         
   IF valdelnrlog = TRUE THEN DO TRANSACTION:      
      CREATE tidut.  
      CREATE tidut.
      ASSIGN tidut.UT = str2.
      CREATE tidut.   
      CREATE tidut. 
      SUBSTRING(tidut.UT,1) = "INGÅENDE DELNR MED TIDSKRIVNING".
      CREATE tidut.              
      FOR EACH slutsum2 USE-INDEX OMR NO-LOCK:  
         FIND FIRST restid3 WHERE restid3.AONR = slutsum2.AONR AND
         restid3.DELNR = slutsum2.DELNR USE-INDEX AONR NO-LOCK NO-ERROR.              
         CREATE tidut.
         ASSIGN   
         SUBSTRING(tidut.UT,1) = slutsum2.AONR   
         SUBSTRING(tidut.UT,8) = STRING(slutsum2.DELNR,"999")       
         /*SUBSTRING(tidut.UT,12) = SUBSTRING(slutsum2.OMRADE,1,4) */                                                    
         SUBSTRING(tidut.UT,31) = STRING(slutsum2.TIMMAR,">>>>99")   
         SUBSTRING(tidut.UT,46) = STRING(slutsum2.OTIMMAR,">>>>99")  
         SUBSTRING(tidut.UT,53) = STRING(slutsum2.OBELOPP,">>>>>99")
         SUBSTRING(tidut.UT,66) = STRING(slutsum2.TBELOPP,">>>>>99")  
         SUBSTRING(tidut.UT,74) = STRING(slutsum2.LONKOST,"->>>99").          
         
        /* IF Guru.Konstanter:globforetag = "NORD" THEN Guru.Konstanter:globforetag = Guru.Konstanter:globforetag.                                                          
        på begäran 990615
        
         ELSE 
         */
         SUBSTRING(tidut.UT,38) = STRING(slutsum2.BELOPP,">>>>>99"). 
         IF AVAILABLE restid3 THEN DO:                                 
            ASSIGN                                         
            SUBSTRING(tidut.UT,61) = STRING(restid3.TIMMAR,">>99").                   
         END.               
      END.         
      CREATE tidut.
      CREATE tidut.
      SUBSTRING(tidut.UT,1) = "ÖVRIGA INGÅENDE DELNR".
      FOR EACH AONRTAB WHERE AONRTAB.AONR = aonummer NO-LOCK:  
         FIND FIRST slutsum2 WHERE slutsum2.AONR = AONRTAB.AONR AND
         slutsum2.DELNR = AONRTAB.DELNR USE-INDEX AONR NO-LOCK NO-ERROR.              
         IF NOT AVAILABLE slutsum2 THEN DO:
            FIND FIRST restid3 WHERE restid3.AONR = AONRTAB.AONR AND
            restid3.DELNR = AONRTAB.DELNR USE-INDEX AONR NO-LOCK NO-ERROR.              
            IF NOT AVAILABLE restid3 THEN DO:
               CREATE tidut.
               ASSIGN               
               SUBSTRING(tidut.UT,1) = STRING(AONRTAB.DELNR,Guru.Konstanter:varforetypchar[1]).
            END.
         END.
      END.
   END. 
   IF vallista = 54 THEN.   
   ELSE DO:                                   
      DO TRANSACTION:
         CREATE tidut.  
         CREATE tidut.
         ASSIGN tidut.UT = str2.
         CREATE tidut.
         CREATE tidut. 
         SUBSTRING(tidut.UT,1) = "PERSONER MED TIDSKRIVNING". 
         CREATE tidut.                       
      END.
      FOR EACH slutsum USE-INDEX OMR NO-LOCK:             
         IF valdelnrlog = FALSE THEN DO: 
            FIND FIRST restid WHERE restid.AONR = slutsum.AONR AND
            restid.DELNR = slutsum.DELNR AND restid.PERSONALKOD = slutsum.PERSONALKOD         
            USE-INDEX AONR NO-LOCK NO-ERROR.     
         END.
         ELSE DO:
            FIND FIRST restid WHERE restid.AONR = slutsum.AONR AND
            restid.PERSONALKOD = slutsum.PERSONALKOD
            USE-INDEX AONR NO-LOCK NO-ERROR.     
         END.   
         CREATE tidut.
         ASSIGN   
         SUBSTRING(tidut.UT,1) = slutsum.AONR   
         SUBSTRING(tidut.UT,8) = STRING(slutsum.DELNR,"999")       
         SUBSTRING(tidut.UT,12) = SUBSTRING(slutsum.OMRADE,1,4)                               
         SUBSTRING(tidut.UT,19) = slutsum.PERSONALKOD 
         SUBSTRING(tidut.UT,25) = slutsum.NAMN             
         SUBSTRING(tidut.UT,31) = STRING(slutsum.TIMMAR,">>>>99")   
         SUBSTRING(tidut.UT,46) = STRING(slutsum.OTIMMAR,">>>>99")  
         SUBSTRING(tidut.UT,53) = STRING(slutsum.OBELOPP,">>>>>99")
         SUBSTRING(tidut.UT,66) = STRING(slutsum.TBELOPP,">>>>>99")  
         SUBSTRING(tidut.UT,74) = STRING(slutsum.LONKOST,"->>>99"). 
         IF valdelnrlog = TRUE THEN SUBSTRING(tidut.UT,8,3) = "   " .        
         SUBSTRING(tidut.UT,38) = STRING(slutsum.BELOPP,">>>>>99").
         IF AVAILABLE restid THEN DO:                                 
            ASSIGN                                         
            SUBSTRING(tidut.UT,61) = STRING(restid.TIMMAR,">>99"). 
         END.                     
      END.
   END.      
