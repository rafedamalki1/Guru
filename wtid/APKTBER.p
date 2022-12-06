/*APKTBER.p*/
DEFINE NEW SHARED VARIABLE fnytid AS DECIMAL FORMAT "-99.99" NO-UNDO.
DEFINE NEW SHARED VARIABLE gvisatidpermanad AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE tidtabrec AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE persrec AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE persrec2 AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE musz AS LOGICAL NO-UNDO.   
DEFINE NEW SHARED VARIABLE regstart LIKE TIDREGITAB.START NO-UNDO. 
DEFINE NEW SHARED VARIABLE regslut LIKE TIDREGITAB.SLUT NO-UNDO.
DEFINE NEW SHARED VARIABLE regvnr AS INTEGER FORMAT "999" NO-UNDO.
DEFINE NEW SHARED VARIABLE regdagnamn AS CHARACTER FORMAT "X(3)" NO-UNDO.        
DEFINE NEW SHARED VARIABLE regdatum AS DATE NO-UNDO.
DEFINE NEW SHARED VARIABLE regmnr AS INTEGER FORMAT "99" NO-UNDO.
DEFINE NEW SHARED VARIABLE regmannamn AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE regar AS INTEGER FORMAT "99" NO-UNDO.
DEFINE NEW SHARED VARIABLE regtotalt LIKE TIDREGITAB.TOTALT NO-UNDO.      
DEFINE NEW SHARED VARIABLE bdatum AS DATE NO-UNDO.
DEFINE NEW SHARED VARIABLE avdatum AS DATE NO-UNDO.
DEFINE NEW SHARED VARIABLE nytid AS DECIMAL FORMAT "99.99" NO-UNDO.
DEFINE NEW SHARED VARIABLE sekunder AS INTEGER FORMAT "-9999999" NO-UNDO.
DEFINE NEW SHARED VARIABLE regstartsek AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE regslutsek AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE frustarten AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE fruslutet AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE kaffestart AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE kaffeslut AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE lunchstarten AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE lunchslutet AS DECIMAL NO-UNDO.

DEFINE VARIABLE periodtot AS DECIMAL FORMAT "99.99" NO-UNDO.
DEFINE VARIABLE pekodtot AS DECIMAL FORMAT "99.99" NO-UNDO.
DEFINE VARIABLE pertot AS INTEGER FORMAT "-9999999" NO-UNDO.
DEFINE VARIABLE pkodtot AS INTEGER FORMAT "-9999999" NO-UNDO.
DEFINE VARIABLE difftot AS DECIMAL FORMAT "-99.99" NO-UNDO.
DEFINE BUFFER tidbuff FOR TIDREGITAB.
DEFINE BUFFER tidbuff2 FOR TIDREGITAB.
DEFINE BUFFER tidbuff3 FOR TIDREGITAB.
&Scoped-define NEW NEW
{TIDPERS.I}

DEFINE TEMP-TABLE invartemp   
   FIELD GA LIKE ANVANDARE.ANVANDARE 
   FIELD GM AS LOGICAL 
   FIELD SK AS LOGICAL 
   FIELD TI AS RECID 
   FIELD PER AS RECID 
   FIELD PER2 AS RECID 
   FIELD MU AS LOGICAL    
   FIELD REGST LIKE TIDREGITAB.START 
   FIELD REGSU LIKE TIDREGITAB.SLUT 
   FIELD RV AS INTEGER FORMAT "999" 
   FIELD RDAG AS CHARACTER FORMAT "X(3)"         
   FIELD RD AS DATE 
   FIELD RM AS INTEGER FORMAT "99" 
   FIELD RMN AS CHARACTER  
   FIELD REGA AS INTEGER FORMAT "99" 
   FIELD RT LIKE TIDREGITAB.TOTALT       
   FIELD BD AS DATE 
   FIELD AD AS DATE 
   FIELD NY AS DECIMAL 
   FIELD SEK AS INTEGER FORMAT "-9999999" 
   FIELD RSEK AS INTEGER 
   FIELD REGS AS INTEGER 
   FIELD GL LIKE FORETAG.FORETAG. 

DEFINE TEMP-TABLE berpers 
   FIELD PERSONALKOD AS CHARACTER
   FIELD DATUM AS DATE
   FIELD DAG AS CHARACTER
   FIELD BEREDSKAP AS CHARACTER 
   FIELD BEREDsKAPSTART AS DECIMAL 
   FIELD BEREDSKAPSLUT AS DECIMAL. 
DEFINE BUFFER berpers2 FOR berpers.
DEFINE VARIABLE str AS CHARACTER FORMAT "X(90)" NO-UNDO.
{TIDUTTTNEW.I}
DEFINE QUERY tidq FOR TIDREGITAB.

DEFINE INPUT PARAMETER TABLE FOR invartemp.
DEFINE INPUT PARAMETER TABLE FOR tidpers.
DEFINE OUTPUT PARAMETER TABLE FOR tidut.
ASSIGN   str=                                                                              
"===========================================================================================================".   

FIND FIRST invartemp NO-ERROR.
ASSIGN
 
gvisatidpermanad = invartemp.GM 
skrivut = invartemp.SK   
tidtabrec = invartemp.TI   
persrec = invartemp.PER   
persrec2 = invartemp.PER2  
musz = invartemp.MU      
regstart = invartemp.REGST   
regslut = invartemp.REGSU  
regvnr = invartemp.RV   
regdagnamn = invartemp.RDAG          
regdatum = invartemp.RD   
regmnr = invartemp.RM  
regmannamn = invartemp.RMN  
regar = invartemp.REGA  
regtotalt = invartemp.RT        
bdatum = invartemp.BD  
avdatum = invartemp.AD  
nytid = invartemp.NY 
sekunder = invartemp.SEK 
regstartsek = invartemp.RSEK  
regslutsek = invartemp.REGS 
Guru.Konstanter:globforetag = invartemp.GL.

RUN huvud_UI.

   {GDPRLOGGCLIENT.I}
PROCEDURE huvud_UI :
   CREATE tidut.
   ASSIGN 
   SUBSTRING(tidut.UT,4) = "Beredskap- Sjukdom"   
   SUBSTRING(tidut.UT,70) = STRING(TODAY)
   SUBSTRING(tidut.UT,80) = STRING(TIME,"HH:MM:SS").
   CREATE tidut.
   ASSIGN 
   SUBSTRING(tidut.UT,4) = "===================".     
   IF Guru.Konstanter:globforetag = "LULE" THEN DO:   
      CREATE tidut.
      CREATE tidut.
      ASSIGN 
      SUBSTRING(tidut.UT,4) = "Vid sjukdom under beredskap bytes lart 325 till 425".      
      CREATE tidut.
      ASSIGN 
      SUBSTRING(tidut.UT,4) = "                                  lart 326 till 426".      
      CREATE tidut.
      ASSIGN 
      SUBSTRING(tidut.UT,4) = "                                  lart 327 till 427".      
      CREATE tidut.
   END.
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "GKAL" THEN DO:
      CREATE tidut.
      CREATE tidut.
      ASSIGN 
      SUBSTRING(tidut.UT,4) = "OBS! Lönearterna är INTE bytta i Guru. Kontrollera tidsedel, om någon löneart måste bytas! ".
      CREATE tidut.
      ASSIGN 
      SUBSTRING(tidut.UT,4) = "Bytet av löneart göres i lönesystemet".   
         
      CREATE tidut.
   END.
   CREATE tidut.                        
   ASSIGN
   SUBSTRING(tidut.UT,1) = "ENHET/SIGN"
   SUBSTRING(tidut.UT,7) = "FÖRNAMN"
   SUBSTRING(tidut.UT,23) = "EFTERNAMN"   
   SUBSTRING(tidut.UT,44) = "VNR"  
   SUBSTRING(tidut.UT,53) = "DAG" 
   SUBSTRING(tidut.UT,57) = "FÖRKLARANDE TEXT".          
   IF gvisatidpermanad = TRUE THEN SUBSTRING(tidut.UT,44,8) = "DATUM   ".
   CREATE tidut.  
   ASSIGN tidut.UT = str.   
   FOR EACH tidpers USE-INDEX PERSONALKOD NO-LOCK: 
      Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," + tidpers.PERSONALKOD.      
      EMPTY TEMP-TABLE berpers NO-ERROR. 
      ASSIGN
      persrec = tidpers.TIDPERSREC
      regdatum = bdatum. 
      FIND PERSONALTAB WHERE RECID(PERSONALTAB) = persrec NO-LOCK NO-ERROR. 
      FIND FIRST ANSTFORMTAB WHERE ANSTFORMTAB.ANSTALLNING = PERSONALTAB.ANSTALLNING
      USE-INDEX ANSTF NO-LOCK NO-ERROR.                       

      FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD AND
      YEAR(TIDREGITAB.DATUM) = regar AND MONTH(TIDREGITAB.DATUM) =  regmnr 
      AND TIDREGITAB.TIDLOG = TRUE AND TIDREGITAB.AONR = "110" USE-INDEX PSTART NO-LOCK NO-ERROR.
      IF AVAILABLE TIDREGITAB THEN DO:         
         OPEN QUERY berq FOR EACH TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD AND
         YEAR(TIDREGITAB.DATUM) = regar AND MONTH(TIDREGITAB.DATUM) =  regmnr 
         AND TIDREGITAB.BEREDSKAP NE ""   USE-INDEX PSTART NO-LOCK.
         GET FIRST berq NO-LOCK.
         DO WHILE AVAILABLE(TIDREGITAB): 
            FIND FIRST BERKOD WHERE BERKOD.BEREDSKAP = TIDREGITAB.BEREDSKAP NO-LOCK NO-ERROR.
            CREATE berpers.
            ASSIGN
            berpers.PERSONALKOD = TIDREGITAB.PERSONALKOD
            berpers.DATUM = TIDREGITAB.DATUM
            berpers.DAG = TIDREGITAB.DAG
            berpers.BEREDSKAP = TIDREGITAB.BEREDSKAP
            berpers.BEREDSKAPSTART = TIDREGITAB.BEREDSKAPSTART  
            berpers.BEREDSKAPSLUT = TIDREGITAB.BEREDSKAPSLUT.
            IF AVAILABLE BERKOD THEN berpers.BEREDSKAP = BERKOD.VILART.
            GET NEXT berq NO-LOCK.         
         END.
      END.
      /* EJ BEREDSKAP OM SJUKDOM*/
      OPEN QUERY sjbeq FOR EACH TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD AND
      YEAR(TIDREGITAB.DATUM) = regar AND MONTH(TIDREGITAB.DATUM) =  regmnr 
      AND TIDREGITAB.TIDLOG = TRUE AND TIDREGITAB.AONR = "110" USE-INDEX PSTART NO-LOCK.
      GET FIRST sjbeq NO-LOCK.
      DO WHILE AVAILABLE(TIDREGITAB): 
         FOR EACH berpers WHERE berpers.PERSONALKOD = tidpers.PERSONALKOD AND
         berpers.DATUM = TIDREGITAB.DATUM AND berpers.BEREDSKAP NE ""  NO-LOCK :         
            IF TIDREGITAB.START LE berpers.BEREDSKAPSTART  AND TIDREGITAB.SLUT GE berpers.BEREDSKAPSLUT  THEN DO:
               /*BEREDSKAP UNDER LUNCH*/
               CREATE tidut.               
               ASSIGN
               SUBSTRING(tidut.UT,1) = tidpers.PERSONALKOD
               SUBSTRING(tidut.UT,7) = tidpers.FORNAMN
               SUBSTRING(tidut.UT,23) = SUBSTRING(tidpers.EFTERNAMN,1,20)    
               SUBSTRING(tidut.UT,44,8) = STRING(TIDREGITAB.DATUM,"99/99/99")
               SUBSTRING(tidut.UT,53) = berpers.DAG 
               SUBSTRING(tidut.UT,57) = SUBSTRING(berpers.BEREDSKAP,1,5)
               SUBSTRING(tidut.UT,63) = STRING(berpers.BEREDSKAPSTART,"99.99")
               SUBSTRING(tidut.UT,69) = STRING(berpers.BEREDSKAPSLUT,"99.99")                                      
               SUBSTRING(tidut.UT,76) = "Beredskap registrerad vid sjukdom,kontrollera lönearter".            
            END.
            ELSE IF TIDREGITAB.START > berpers.BEREDSKAPSTART  THEN DO:
               /*beredskap 0- regstart*/
               FIND LAST tidbuff2 WHERE tidbuff2.PERSONALKOD = tidpers.PERSONALKOD AND
               tidbuff2.DATUM < TIDREGITAB.DATUM  AND tidbuff2.TIDLOG = TRUE USE-INDEX PSTART NO-LOCK NO-ERROR.
               IF AVAILABLE tidbuff2  THEN DO:
                  IF tidbuff2.AONR = "110" THEN DO:
                     CREATE tidut.               
                     ASSIGN
                     SUBSTRING(tidut.UT,1) = tidpers.PERSONALKOD
                     SUBSTRING(tidut.UT,7) = tidpers.FORNAMN
                     SUBSTRING(tidut.UT,23) = SUBSTRING(tidpers.EFTERNAMN,1,20)    
                     SUBSTRING(tidut.UT,44,8) = STRING(TIDREGITAB.DATUM,"99/99/99")
                     SUBSTRING(tidut.UT,53) = berpers.DAG 
                     SUBSTRING(tidut.UT,57) = SUBSTRING(berpers.BEREDSKAP,1,5)
                     SUBSTRING(tidut.UT,63) = STRING(berpers.BEREDSKAPSTART,"99.99")
                     SUBSTRING(tidut.UT,69) = STRING(berpers.BEREDSKAPSLUT,"99.99")                                      
                     SUBSTRING(tidut.UT,76) = "Beredskap registrerad vid sjukdom,kontrollera lönearter".            
                  END.
               END.
            END.
            ELSE IF TIDREGITAB.SLUT < berpers.BEREDSKAPSLUT THEN DO:
               /* beredskap regslut-24*/
               CREATE tidut.               
               ASSIGN
               SUBSTRING(tidut.UT,1) = tidpers.PERSONALKOD
               SUBSTRING(tidut.UT,7) = tidpers.FORNAMN
               SUBSTRING(tidut.UT,23) = SUBSTRING(tidpers.EFTERNAMN,1,20)    
               SUBSTRING(tidut.UT,44,8) = STRING(TIDREGITAB.DATUM,"99/99/99")
               SUBSTRING(tidut.UT,53) = berpers.DAG 
               SUBSTRING(tidut.UT,57) = SUBSTRING(berpers.BEREDSKAP,1,5)
               SUBSTRING(tidut.UT,63) = STRING(berpers.BEREDSKAPSTART,"99.99")
               SUBSTRING(tidut.UT,69) = STRING(berpers.BEREDSKAPSLUT,"99.99")                                      
               SUBSTRING(tidut.UT,76) = "Beredskap registrerad vid sjukdom,kontrollera lönearter".            

               FIND FIRST tidbuff2 WHERE tidbuff2.PERSONALKOD = tidpers.PERSONALKOD AND
               tidbuff2.DATUM > TIDREGITAB.DATUM  AND tidbuff2.TIDLOG = TRUE AND
               tidbuff2.OKOD1 = "" USE-INDEX PSTART NO-LOCK NO-ERROR.
               IF AVAILABLE tidbuff2  THEN DO:
                  IF tidbuff2.DATUM = ( TIDREGITAB.DATUM + 1) THEN DO:                  
                     /*nästa dag */
                     IF tidbuff2.AONR NE "110" THEN DO:                     
                        FIND FIRST berpers2 WHERE berpers2.PERSONALKOD = tidpers.PERSONALKOD AND
                        berpers2.DATUM = tidbuff2.DATUM AND berpers2.BEREDSKAP NE ""  AND berpers2.BEREDSKAPSTART = 00
                        NO-LOCK NO-ERROR.
                        IF AVAILABLE berpers2 THEN DO:
                           CREATE tidut.               
                           ASSIGN
                           SUBSTRING(tidut.UT,1) = tidpers.PERSONALKOD
                           SUBSTRING(tidut.UT,7) = tidpers.FORNAMN
                           SUBSTRING(tidut.UT,23) = SUBSTRING(tidpers.EFTERNAMN,1,20)    
                           SUBSTRING(tidut.UT,44,8) = STRING(berpers2.DATUM,"99/99/99")
                           SUBSTRING(tidut.UT,53) = berpers2.DAG 
                           SUBSTRING(tidut.UT,57) = SUBSTRING(berpers2.BEREDSKAP,1,5)
                           SUBSTRING(tidut.UT,63) = STRING(berpers2.BEREDSKAPSTART,"99.99")
                           SUBSTRING(tidut.UT,69) = STRING(berpers2.BEREDSKAPSLUT,"99.99")                                      
                           SUBSTRING(tidut.UT,76) = "Beredskap registrerad vid sjukdom,kontrollera lönearter".          
                        END.
                     END.
                  END.
                  ELSE DO:
                     /*helg emellan*/
                     IF berpers.BEREDSKAPSLUT = 24 THEN DO:                     
                        /*lägg ut helgen */
                        FOR EACH berpers2 WHERE berpers2.DATUM > TIDREGITAB.DATUM AND berpers2.DATUM < tidbuff2.DATUM:
                           CREATE tidut.               
                           ASSIGN
                           SUBSTRING(tidut.UT,1) = tidpers.PERSONALKOD
                           SUBSTRING(tidut.UT,7) = tidpers.FORNAMN
                           SUBSTRING(tidut.UT,23) = SUBSTRING(tidpers.EFTERNAMN,1,20)    
                           SUBSTRING(tidut.UT,44,8) = STRING(berpers2.DATUM,"99/99/99")
                           SUBSTRING(tidut.UT,53) = berpers2.DAG 
                           SUBSTRING(tidut.UT,57) = SUBSTRING(berpers2.BEREDSKAP,1,5)
                           SUBSTRING(tidut.UT,63) = STRING(berpers2.BEREDSKAPSTART,"99.99")
                           SUBSTRING(tidut.UT,69) = STRING(berpers2.BEREDSKAPSLUT,"99.99")                                      
                           SUBSTRING(tidut.UT,76) = "Beredskap registrerad vid sjukdom,kontrollera lönearter".          
                        END.                     
                        IF tidbuff2.AONR NE "110" THEN DO:
                           FIND FIRST berpers2 WHERE berpers2.PERSONALKOD = tidpers.PERSONALKOD AND
                           berpers2.DATUM = tidbuff2.DATUM AND berpers2.BEREDSKAP NE ""  AND berpers2.BEREDSKAPSTART = 00
                           NO-LOCK NO-ERROR.
                           IF AVAILABLE berpers2 THEN DO:
                              CREATE tidut.               
                              ASSIGN
                              SUBSTRING(tidut.UT,1) = tidpers.PERSONALKOD
                              SUBSTRING(tidut.UT,7) = tidpers.FORNAMN
                              SUBSTRING(tidut.UT,23) = SUBSTRING(tidpers.EFTERNAMN,1,20)    
                              SUBSTRING(tidut.UT,44,8) = STRING(berpers2.DATUM,"99/99/99")
                              SUBSTRING(tidut.UT,53) = berpers2.DAG 
                              SUBSTRING(tidut.UT,57) = SUBSTRING(berpers2.BEREDSKAP,1,5)
                              SUBSTRING(tidut.UT,63) = STRING(berpers2.BEREDSKAPSTART,"99.99")
                              SUBSTRING(tidut.UT,69) = STRING(berpers2.BEREDSKAPSLUT,"99.99")                                      
                              SUBSTRING(tidut.UT,76) = "Beredskap registrerad vid sjukdom,kontrollera lönearter".          
                           END.
                        END.
                     END.
                  END.
               END.
            END.
         END.                     
         GET NEXT sjbeq NO-LOCK.         
      END. 
   END.      
   FIND LAST tidut NO-LOCK NO-ERROR.   
END PROCEDURE.
