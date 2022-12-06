   /*AOPSUMMAUK3.I*/   
   ASSIGN   
   arrhjsum = 0   
   arrhjsumtid = 0  
   arrhjsumotid = 0    
   arrhjsumove = 0    
   arrhjsumtra = 0       
   arrhjsumlon = 0
   arrhjkmbelopp = 0.
   CREATE indertemp. 
   
   IF RAD_PERIOD = 2 OR RAD_PERIOD = 3 THEN DO:
      EMPTY TEMP-TABLE bytaao NO-ERROR.       
      FOR EACH aoval:      
         CREATE bytaao.
         ASSIGN
         bytaao.VADGORA = 4
         bytaao.STDATUM = bdatum
         bytaao.SLDATUM = avdatum
         bytaao.ARTAL = YEAR(bdatum)
         bytaao.RAD_PERIOD = RAD_PERIOD
         bytaao.AONR = aoval.AONR
         bytaao.DELNR = aoval.DELNR
         bytaao.GLOBFORETAG = Guru.Konstanter:globforetag
         bytaao.INDER = inder
         bytaao.SOK1 = visa.UT
         bytaao.SOK2 = kollvecka
         bytaao.SOK5 = valdelnrlog.     
         RUN SKAPAOBY.P(INPUT-OUTPUT TABLE bytaao, INPUT-OUTPUT TABLE dagtemp, INPUT-OUTPUT TABLE indertemp , INPUT-OUTPUT TABLE restid, OUTPUT bytaonrmed, INPUT-OUTPUT TABLE tidut).
         EMPTY TEMP-TABLE bytaao NO-ERROR.       
      END.
      {DAGTEMPBOLAG5.I}
      FIND FIRST indertemp NO-ERROR.
      IF RAD_PERIOD = 2 THEN DO:                  
         ASSIGN
         anr = ""
         dnr = 0.         
         OPEN QUERY dagsumkq FOR EACH aoval USE-INDEX AONR,
         EACH TIDREGITAB WHERE TIDREGITAB.DATUM >= bdatum AND
         TIDREGITAB.DATUM <= avdatum AND TIDREGITAB.AONR = aoval.AONR AND 
         TIDREGITAB.DELNR = aoval.DELNR AND TIDREGITAB.RESMAL NE "" USE-INDEX AONR NO-LOCK.         
         GET FIRST dagsumkq NO-LOCK.
         DO WHILE AVAILABLE(TIDREGITAB) TRANSACTION:
            {DAGTEMPBOLAG3.I}
            {ANVTIDSEKKOLL.I}
            
            IF sealltid = TRUE THEN tisek = TRUE.
            IF bsek = FALSE AND tisek = TRUE  THEN DO:
               FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = TIDREGITAB.PERSONALKOD NO-LOCK NO-ERROR.
               berindvar = 1.
               IF aoval.AONR = anr AND aoval.DELNR = dnr THEN proc1 = proc1.
               ELSE DO:            
                  proc1 = 0.               
                  FIND FIRST AONRKONTKOD WHERE AONRKONTKOD.AONR =  aoval.AONR  AND
                  AONRKONTKOD.DELNR =  aoval.DELNR AND AONRKONTKOD.K2 = hsokkto
                  NO-LOCK NO-ERROR.
                  IF AVAILABLE AONRKONTKOD  THEN DO:
                     proc1 = AONRKONTKOD.SATS% / 100.
                  END.               
               END.
               ASSIGN
               anr = aoval.AONR
               DNR = AOVAL.DELNR.            
               
               CREATE tidut.            
               ASSIGN         
               SUBSTRING(tidut.UT,1) = TIDREGITAB.AONR
               SUBSTRING(tidut.UT,8) = STRING(TIDREGITAB.DELNR)
               SUBSTRING(tidut.UT,12) = PERSONALTAB.PERSONALKOD   
               SUBSTRING(tidut.UT,18) = SUBSTRING(PERSONALTAB.FORNAMN,1,1) + "." + 
               SUBSTRING(PERSONALTAB.EFTERNAMN,1,3)                
               SUBSTRING(tidut.UT,26) = PERSONALTAB.OMRADE               
               SUBSTRING(tidut.UT,31) = STRING(TIDREGITAB.DATUM) 
               SUBSTRING(tidut.UT,40) = STRING(TIDREGITAB.START,"99.99")
               SUBSTRING(tidut.UT,46) = STRING(TIDREGITAB.SLUT,"99.99") 
               SUBSTRING(tidut.UT,52) = STRING(TIDREGITAB.TOTALT,">9.99")
               SUBSTRING(tidut.UT,58) = SUBSTRING(TIDREGITAB.RESMAL,1,60).            
               IF SUBSTRING(TIDREGITAB.RESMAL,159,6) NE "" THEN
               SUBSTRING(tidut.UT,119) = SUBSTRING(TIDREGITAB.RESMAL,159,6).
               Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," + PERSONALTAB.PERSONALKOD.
               /*SUBSTRING(tidut.UT,58) = SUBSTRING(TIDREGITAB.RESMAL,1,30).            
               IF SUBSTRING(TIDREGITAB.RESMAL,159,6) NE "" THEN
               SUBSTRING(tidut.UT,89) = SUBSTRING(TIDREGITAB.RESMAL,159,6).*/
            END.                                       
            GET NEXT dagsumkq NO-LOCK. 
         END.               
      END.
      IF RAD_PERIOD = 3 THEN DO:                           
         ASSIGN
         anr = ""
         dnr = 0.      
         OPEN QUERY dagsumkq FOR EACH aoval,
         EACH TIDREGITAB WHERE TIDREGITAB.AONR = aoval.AONR AND 
         TIDREGITAB.DELNR = aoval.DELNR AND TIDREGITAB.RESMAL NE "" USE-INDEX AONR NO-LOCK.
         
         GET FIRST dagsumkq NO-LOCK.
         DO WHILE AVAILABLE(TIDREGITAB) TRANSACTION:
            {DAGTEMPBOLAG3.I}
            {ANVTIDSEKKOLL.I}
            IF bsek = FALSE AND tisek = true THEN DO:
               berindvar = 1.         
               
               IF aoval.AONR = anr AND aoval.DELNR = dnr THEN proc1 = proc1.
               ELSE DO:            
                  proc1 = 0.               
                  FOR EACH AONRKONTKOD WHERE AONRKONTKOD.AONR =  aoval.AONR AND
                  AONRKONTKOD.DELNR =  aoval.DELNR AND AONRKONTKOD.K2 = hsokkto NO-LOCK:               
                     proc1 = proc1 + AONRKONTKOD.SATS% / 100.
                  END.               
               END.
               ASSIGN
               anr = aoval.AONR
               DNR = AOVAL.DELNR.
               
               CREATE tidut.            
               ASSIGN         
               SUBSTRING(tidut.UT,1) = TIDREGITAB.AONR
               SUBSTRING(tidut.UT,8) = STRING(TIDREGITAB.DELNR)
               SUBSTRING(tidut.UT,12) = PERSONALTAB.PERSONALKOD   
               SUBSTRING(tidut.UT,18) = SUBSTRING(PERSONALTAB.FORNAMN,1,1) + "." + 
               SUBSTRING(PERSONALTAB.EFTERNAMN,1,3)                
               SUBSTRING(tidut.UT,26) = PERSONALTAB.OMRADE   
               SUBSTRING(tidut.UT,31) = STRING(TIDREGITAB.DATUM) 
               SUBSTRING(tidut.UT,40) = STRING(TIDREGITAB.START,"99.99")
               SUBSTRING(tidut.UT,46) = STRING(TIDREGITAB.SLUT,"99.99") 
               SUBSTRING(tidut.UT,52) = STRING(TIDREGITAB.TOTALT,">9.99")
               SUBSTRING(tidut.UT,58) = SUBSTRING(TIDREGITAB.RESMAL,1,30).            
               IF SUBSTRING(TIDREGITAB.RESMAL,159,6) NE "" THEN
               SUBSTRING(tidut.UT,89) = SUBSTRING(TIDREGITAB.RESMAL,159,6).
            END.                                       
            GET NEXT dagsumkq NO-LOCK. 
         END.             
      END.
   END.  
   
