/* MERTID.P KOLL AV MERTID */
&Scoped-define NEW 
{TIDALLT.I}
/*DEFINE SHARED VARIABLE globanv LIKE ANVANDARE.ANVANDARE NO-UNDO.*/
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE tidtabrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE persrec AS RECID  NO-UNDO.
DEFINE SHARED VARIABLE regvnr AS INTEGER FORMAT "999" NO-UNDO.
DEFINE SHARED VARIABLE regdatum AS DATE NO-UNDO.
DEFINE SHARED VARIABLE bustart3 LIKE TIDREGITAB.START NO-UNDO.
DEFINE SHARED VARIABLE nytid AS DECIMAL FORMAT "99.99" NO-UNDO.
DEFINE SHARED VARIABLE sekunder AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE bdatum LIKE TIDREGITAB.DATUM NO-UNDO.
DEFINE SHARED VARIABLE avdatum LIKE TIDREGITAB.DATUM NO-UNDO.
DEFINE SHARED VARIABLE regstart LIKE TIDREGITAB.START NO-UNDO.
DEFINE SHARED VARIABLE regslut LIKE TIDREGITAB.SLUT NO-UNDO.
DEFINE SHARED VARIABLE regstartsek AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE regslutsek AS INTEGER NO-UNDO.

DEFINE VARIABLE regdatumspar AS DATE NO-UNDO.  
DEFINE VARIABLE oslut LIKE TIDREGITAB.SLUT NO-UNDO.
DEFINE VARIABLE slutet3 AS DECIMAL FORMAT "99.99" NO-UNDO.
DEFINE VARIABLE starten3 AS DECIMAL FORMAT "99.99" NO-UNDO.
DEFINE VARIABLE hjstart LIKE TIDREGITAB.START NO-UNDO.  
DEFINE VARIABLE hjslut LIKE TIDREGITAB.SLUT NO-UNDO.    
DEFINE VARIABLE ehjstart LIKE TIDREGITAB.START NO-UNDO.  
DEFINE BUFFER tidbuff FOR TIDREGITAB.
FIND PERSONALTAB WHERE RECID(PERSONALTAB) = persrec NO-LOCK NO-ERROR.
FIND FIRST ANSTFORMTAB WHERE ANSTFORMTAB.ANSTALLNING = PERSONALTAB.ANSTALLNING 
USE-INDEX ANSTF NO-LOCK NO-ERROR.
FIND TIDREGITAB WHERE RECID(TIDREGITAB) = tidtabrec NO-LOCK NO-ERROR.
FIND FIRST ORDARB WHERE ORDARB.ANSTALLNING = PERSONALTAB.ANSTALLNING 
USE-INDEX ORDARB NO-LOCK NO-ERROR.
IF NOT AVAILABLE ORDARB THEN DO TRANSACTION:   
   CREATE FELTEXT.
   ASSIGN 
   FELTEXT.ANVANDARE = Guru.Konstanter:globanv                      
   FELTEXT.FELTEXT = "Kontakta Elpool! För nu är det något fel! Ange läge 6."
   FELTEXT.PROGRAM = "MERTID" + STRING(TODAY) + STRING(TIME,"HH:MM") + Guru.Konstanter:globanv.
END. 
ELSE DO: 
   sekunder = ORDARB.START1.
   RUN SEKTIM.P.
   ASSIGN
   hjstart = nytid
   ehjstart = hjstart.
   IF ORDARB.OBKOD NE "" THEN DO:
      FIND FIRST FLEXREG WHERE FLEXREG.KOD = ANSTFORMTAB.KOD NO-LOCK NO-ERROR.
      IF NOT AVAILABLE FLEXREG THEN DO:
         FIND FIRST FLEXREG WHERE FLEXREG.KOD = "" NO-LOCK NO-ERROR.
      END.        
      IF AVAILABLE FLEXREG THEN DO:
         IF MONTH(regdatum) > MONTH(FLEXREG.SOMMARST) AND MONTH(regdatum) < MONTH(FLEXREG.SOMMARSL) THEN DO:
            ASSIGN hjslut = DECIMAL(ORDARB.OBKOD).              
            IF Guru.Konstanter:globforetag = "SUND" AND FLEXREG.KOD BEGINS "T" THEN hjstart = 7.30.
            IF Guru.Konstanter:globforetag = "SUND" AND FLEXREG.KOD = "TU" THEN hjstart = ehjstart.
            
            IF Guru.Konstanter:globforetag = "SNAT" AND FLEXREG.KOD BEGINS "T" THEN hjstart = 7.30.            
                           
            IF Guru.Konstanter:globforetag = "MISV" AND FLEXREG.KOD BEGINS "T" THEN hjstart = 7.30.            
                          
         END.
         ELSE IF MONTH(regdatum) = MONTH(FLEXREG.SOMMARSL) AND DAY(regdatum) <= DAY(FLEXREG.SOMMARSL) THEN DO:
            ASSIGN hjslut = DECIMAL(ORDARB.OBKOD).
            IF Guru.Konstanter:globforetag = "SUND" AND FLEXREG.KOD BEGINS "T" THEN hjstart = 7.30.
            IF Guru.Konstanter:globforetag = "SUND" AND FLEXREG.KOD = "TU" THEN hjstart = ehjstart.
            
            IF Guru.Konstanter:globforetag = "SNAT" AND FLEXREG.KOD BEGINS "T" THEN hjstart = 7.30.            
                           
            IF Guru.Konstanter:globforetag = "MISV" AND FLEXREG.KOD BEGINS "T" THEN hjstart = 7.30.                                     
         END.
         ELSE IF MONTH(regdatum) = MONTH(FLEXREG.SOMMARST) AND DAY(regdatum) >= DAY(FLEXREG.SOMMARST) THEN DO: 
            ASSIGN hjslut = DECIMAL(ORDARB.OBKOD).
            IF Guru.Konstanter:globforetag = "SUND" AND FLEXREG.KOD BEGINS "T" THEN hjstart = 7.30.
            IF Guru.Konstanter:globforetag = "SUND" AND FLEXREG.KOD = "TU" THEN hjstart = ehjstart.
            
            IF Guru.Konstanter:globforetag = "SNAT" AND FLEXREG.KOD BEGINS "T" THEN hjstart = 7.30.            
                           
            IF Guru.Konstanter:globforetag = "MISV" AND FLEXREG.KOD BEGINS "T" THEN hjstart = 7.30.                                     
         END.
         ELSE DO:      
            sekunder = ORDARB.STOPP1.
            RUN SEKTIM.P.
            hjslut = nytid.                
         END.  
      END. 
      ELSE DO:      
         sekunder = ORDARB.STOPP1.
         RUN SEKTIM.P.
         hjslut = nytid.    
      END.  
   END. 
   ELSE DO:   
      sekunder = ORDARB.STOPP1.
      RUN SEKTIM.P.
      hjslut = nytid.    
   END.   
END.      
ASSIGN
starten3 = TIDREGITAB.START
slutet3 = TIDREGITAB.SLUT
regvnr = TIDREGITAB.VECKONUMMER.
RUN SLUTARB.P.
FIND FIRST OVERAVTAB WHERE OVERAVTAB.DATUM = TIDREGITAB.DATUM AND 
OVERAVTAB.KOD = ANSTFORMTAB.KOD USE-INDEX ODATUM NO-LOCK NO-ERROR.
IF AVAILABLE OVERAVTAB AND OVERAVTAB.EQDAG = 1 THEN DO:
   regdatumspar = regdatum.
   RUN OTOLKPR.P.
   regdatum = regdatumspar.
   FIND TIDREGITAB WHERE RECID(TIDREGITAB) = tidtabrec NO-LOCK NO-ERROR.
END.
ELSE IF AVAILABLE OVERAVTAB AND OVERAVTAB.EQDAG = 7 THEN DO:
   regdatumspar = regdatum.
   RUN OTOLKPR.P.
   regdatum = regdatumspar.
   FIND TIDREGITAB WHERE RECID(TIDREGITAB) = tidtabrec NO-LOCK NO-ERROR.
END.
ELSE IF WEEKDAY(TIDREGITAB.DATUM) = 1 OR WEEKDAY(TIDREGITAB.DATUM) = 7 THEN DO:
   regdatumspar = regdatum.
   RUN OTOLKPR.P.
   regdatum = regdatumspar.
   FIND TIDREGITAB WHERE RECID(TIDREGITAB) = tidtabrec NO-LOCK NO-ERROR.
END.
ELSE DO:   /*MERTID BARA OM VARDAG*/
   IF WEEKDAY(TIDREGITAB.DATUM) = 1 OR WEEKDAY(TIDREGITAB.DATUM) = 7 THEN DO:
      ASSIGN
      hjslut = 7.00
      hjstart = 7.00.
   END.     
   IF TIDREGITAB.START < regstart AND TIDREGITAB.SLUT > regstart THEN DO TRANSACTION:         
      IF TIDREGITAB.OVERTIDUTTAG = "F" THEN musz = musz.
      ELSE DO:      
         FIND TIDREGITAB WHERE RECID(TIDREGITAB) = tidtabrec EXCLUSIVE-LOCK NO-ERROR.
         bustart3 = TIDREGITAB.START.              
         CREATE tidbuff.
         CREATE tidallt.
         tidallt.RECTIDVIS = RECID(tidbuff).
         ASSIGN 
         tidbuff.PERSONALKOD = TIDREGITAB.PERSONALKOD
         tidbuff.AONR = TIDREGITAB.AONR
         tidbuff.DELNR = TIDREGITAB.DELNR         
         SUBSTRING(tidbuff.PROGRAM,1,158) = "TIDUPPDE" + STRING(TODAY) + STRING(TIME,"HH:MM") + Guru.Konstanter:globanv
         tidbuff.OVERTIDTILL = TIDREGITAB.OVERTIDTILL
         tidbuff.PRISTYP = TIDREGITAB.PRISTYP
         tidbuff.PRIS = TIDREGITAB.PRIS
         tidbuff.TRAKTAMENTE = TIDREGITAB.TRAKTAMENTE
         tidbuff.DATUM = TIDREGITAB.DATUM 
         tidbuff.UTRYCK = TIDREGITAB.UTRYCK  
         tidbuff.OVERTIDUTTAG = TIDREGITAB.OVERTIDUTTAG
         tidbuff.START = regstart
         tidbuff.SLUT = TIDREGITAB.SLUT
         tidbuff.VECKONUMMER = TIDREGITAB.VECKONUMMER       
         tidbuff.DAG = TIDREGITAB.DAG
         tidbuff.RESMAL = TIDREGITAB.RESMAL.     
         tidtabrec = RECID(tidbuff).
         ASSIGN TIDREGITAB.SLUT = regstart.           
         regdatumspar = regdatum.
         RUN OTOLKPR.P.
         regdatum = regdatumspar.
         FIND TIDREGITAB WHERE RECID(TIDREGITAB) = tidtabrec NO-LOCK NO-ERROR.      
         IF TIDREGITAB.SLUT LE hjslut AND TIDREGITAB.SLUT > hjstart THEN musz = TRUE. 
         FIND TIDREGITAB WHERE RECID(TIDREGITAB) = tidtabrec EXCLUSIVE-LOCK NO-ERROR.
      END.
   END.
   IF regslut < hjslut AND TIDREGITAB.START < hjslut AND
   TIDREGITAB.START GE regstart AND TIDREGITAB.SLUT > regslut THEN DO:    
      IF TIDREGITAB.OVERTIDUTTAG = "F" THEN musz = musz.
      ELSE DO:      
         /* OM ARBETSTIDEN SLUTAR INNAN ORDARB SLUTAR OCH TIDREGISTRERINGEN BÖRJAR
         INNAN ORDARB SLUTAR DVS MERTID SKALL UTVÄRDERAS*/
         IF TIDREGITAB.START < regslut THEN DO TRANSACTION:
            /* OM TIDREGISTRERINGEN BÖRJAR FÖR ARBETSTIDENS SLUT */
            FIND TIDREGITAB WHERE RECID(TIDREGITAB) = tidtabrec EXCLUSIVE-LOCK NO-ERROR.    
            IF TIDREGITAB.LONTILLAGG NE ""  THEN DO:
               /* OM MAN UTGÅR FRÅN EN MERTID OCH ÄNDRAR TILL ORD TID SKALL EJ MERTID FALLA UT*/
               ASSIGN TIDREGITAB.LONTILLAGG = ""
               TIDREGITAB.LONTILLANTAL = 0.
            END.
            ASSIGN
            bustart3 = regslut               
            oslut = TIDREGITAB.SLUT.
            ASSIGN TIDREGITAB.SLUT = regslut      
            nytid = TIDREGITAB.TOTALT.
            RUN TIMSEK.P.                  
            ASSIGN
            nytid = TIDREGITAB.START.
            RUN TIMSEK.P.     
            ASSIGN
            regstartsek = sekunder
            nytid = TIDREGITAB.SLUT.
            RUN TIMSEK.P.         
            ASSIGN
            regslutsek = sekunder
            regdatum = TIDREGITAB.DATUM.         
            RUN TOTTID.P.
            ASSIGN TIDREGITAB.TOTALT = nytid.
     
            CREATE tidbuff.
            CREATE tidallt.
            tidallt.RECTIDVIS = RECID(tidbuff).
            ASSIGN 
            tidbuff.PERSONALKOD = TIDREGITAB.PERSONALKOD 
            tidbuff.AONR = TIDREGITAB.AONR             
            SUBSTRING(tidbuff.PROGRAM,1,158) = "MERTID" + STRING(TODAY) + STRING(TIME,"HH:MM") + Guru.Konstanter:globanv
            tidbuff.OVERTIDTILL = TIDREGITAB.OVERTIDTILL
            tidbuff.DELNR = TIDREGITAB.DELNR
            tidbuff.TRAKTAMENTE = TIDREGITAB.TRAKTAMENTE
            tidbuff.DATUM = TIDREGITAB.DATUM 
            tidbuff.VECKONUMMER = TIDREGITAB.VECKONUMMER 
            tidbuff.OVERTIDUTTAG = TIDREGITAB.OVERTIDUTTAG
            tidbuff.DAG = TIDREGITAB.DAG
            tidbuff.START = regslut 
            tidbuff.SLUT = oslut
            tidbuff.PRISTYP = TIDREGITAB.PRISTYP
            tidbuff.PRIS = TIDREGITAB.PRIS
            tidbuff.RESMAL = TIDREGITAB.RESMAL.     
            ASSIGN
            nytid = tidbuff.START.
            RUN TIMSEK.P.
            ASSIGN
            regstartsek = sekunder
            nytid = tidbuff.SLUT.
            RUN TIMSEK.P.         
            ASSIGN
            regslutsek = sekunder
            regdatum = tidbuff.DATUM.         
            RUN TOTTID.P.
            ASSIGN tidbuff.TOTALT = nytid      
            tidtabrec = RECID(tidbuff).
            FIND TIDREGITAB WHERE RECID(TIDREGITAB) = tidtabrec
            NO-LOCK NO-ERROR.
         END.   
         IF TIDREGITAB.START LE hjslut AND TIDREGITAB.START GE regslut THEN DO TRANSACTION:       
            /* OM TIDREGISTRERINEN BÖRJAR EFTER ARBETETS SLUT */
            FIND FIRST TIDREGITAB WHERE RECID(TIDREGITAB) = tidtabrec
            EXCLUSIVE-LOCK NO-ERROR.
            bustart3 = regslut.
            IF TIDREGITAB.OVERTIDUTTAG = "K" OR TIDREGITAB.OVERTIDUTTAG = "L" THEN DO:    
               IF NOT AVAILABLE ORDARB THEN DO:
                  CREATE FELTEXT.
                  ASSIGN 
                  FELTEXT.ANVANDARE = Guru.Konstanter:globanv                      
                  FELTEXT.FELTEXT = "Kontakta Elpool! För nu är det något fel! Ange läge 7."
                  FELTEXT.PROGRAM = "MERTID" + STRING(TODAY) + STRING(TIME,"HH:MM") + Guru.Konstanter:globanv.             
               END. 
               ASSIGN TIDREGITAB.LONTILLAGG = ORDARB.MERKOD2.
            END.
            ELSE IF TIDREGITAB.OVERTIDUTTAG = "Ö" THEN DO:                          
               IF NOT AVAILABLE ORDARB THEN DO:
                  CREATE FELTEXT.
                  ASSIGN 
                  FELTEXT.ANVANDARE = Guru.Konstanter:globanv                      
                  FELTEXT.FELTEXT = "Kontakta Elpool! För nu är det något fel! Ange läge 8."
                  FELTEXT.PROGRAM = "MERTID" + STRING(TODAY) + STRING(TIME,"HH:MM") + Guru.Konstanter:globanv. 
               END. 
               ASSIGN TIDREGITAB.LONTILLAGG = ORDARB.MERKOD.
            END. 
            ELSE IF TIDREGITAB.OVERTIDUTTAG = "F" THEN DO:                          
               ASSIGN TIDREGITAB.LONTILLAGG = ""
               TIDREGITAB.LONTILLANTAL = 0.
            END.     
            IF (slutet3 > hjslut) OR (slutet3 < hjstart) THEN DO:      
      	      ASSIGN TIDREGITAB.SLUT = hjslut
      	      bustart3 = hjslut
               musz = FALSE	  
               nytid = TIDREGITAB.TOTALT.
      	      RUN TIMSEK.P.
      	      ASSIGN
               nytid = TIDREGITAB.START.
      	      RUN TIMSEK.P.     
      	      ASSIGN
      	      regstartsek = sekunder
      	      nytid = TIDREGITAB.SLUT.
      	      RUN TIMSEK.P.         
         	   ASSIGN
         	   regslutsek = sekunder
         	   regdatum = TIDREGITAB.DATUM.
               RUN TOTTID.P.
         	   ASSIGN TIDREGITAB.TOTALT = nytid.         	  
         	   CREATE tidbuff.
               CREATE tidallt.
               tidallt.RECTIDVIS = RECID(tidbuff).
               ASSIGN 
               tidbuff.PERSONALKOD = TIDREGITAB.PERSONALKOD 
               tidbuff.AONR = TIDREGITAB.AONR 
               SUBSTRING(tidbuff.PROGRAM,1,158) = "MERTID" + STRING(TODAY) + STRING(TIME,"HH:MM") + Guru.Konstanter:globanv
               tidbuff.OVERTIDTILL = TIDREGITAB.OVERTIDTILL
               tidbuff.DELNR = TIDREGITAB.DELNR
               tidbuff.TRAKTAMENTE = TIDREGITAB.TRAKTAMENTE
               tidbuff.DATUM = TIDREGITAB.DATUM 
               tidbuff.VECKONUMMER = TIDREGITAB.VECKONUMMER 
               tidbuff.DAG = TIDREGITAB.DAG      
               tidbuff.OVERTIDUTTAG = TIDREGITAB.OVERTIDUTTAG
               tidbuff.START = hjslut 
               tidbuff.SLUT = slutet3
               tidbuff.PRISTYP = TIDREGITAB.PRISTYP
               tidbuff.PRIS = TIDREGITAB.PRIS
               tidbuff.RESMAL = TIDREGITAB.RESMAL.     
               ASSIGN
               nytid = tidbuff.START.
               RUN TIMSEK.P.  
               ASSIGN
               regstartsek = sekunder
               nytid = tidbuff.SLUT.
               RUN TIMSEK.P.         
               ASSIGN
               regslutsek = sekunder
               regdatum = tidbuff.DATUM.
               RUN TOTTID.P.
               ASSIGN tidbuff.TOTALT = nytid      
               tidtabrec = RECID(tidbuff).
               FIND TIDREGITAB WHERE RECID(TIDREGITAB) = tidtabrec
               NO-LOCK NO-ERROR.	 
            END.
            ELSE IF slutet3 < hjslut THEN DO:
         	   ASSIGN TIDREGITAB.SLUT = slutet3
         	   nytid = TIDREGITAB.TOTALT.      	 
         	   RUN TIMSEK.P.                   
         	   ASSIGN
               nytid = TIDREGITAB.START.
         	   RUN TIMSEK.P.     
         	   ASSIGN
         	   regstartsek = sekunder
         	   nytid = TIDREGITAB.SLUT.
         	   RUN TIMSEK.P.         
         	   ASSIGN
         	   regslutsek = sekunder
               regdatum = TIDREGITAB.DATUM.      	   
         	   RUN TOTTID.P.
         	   ASSIGN TIDREGITAB.TOTALT = nytid.
            END.
         END.
      END.
   END.
   ELSE IF regstart > hjstart AND TIDREGITAB.SLUT LE hjslut AND
   TIDREGITAB.SLUT > hjstart AND TIDREGITAB.START < regstart THEN DO:       
      IF TIDREGITAB.OVERTIDUTTAG = "F" THEN musz = musz.
      ELSE DO:      
         /* MERTID INNAN ARBETSTID  */     
         IF TIDREGITAB.START < hjstart THEN DO TRANSACTION:
            FIND FIRST TIDREGITAB WHERE RECID(TIDREGITAB) = tidtabrec EXCLUSIVE-LOCK NO-ERROR.   
            IF NOT AVAILABLE ORDARB THEN DO:
               CREATE FELTEXT.
               ASSIGN 
               FELTEXT.ANVANDARE = Guru.Konstanter:globanv                      
               FELTEXT.FELTEXT = "Kontakta Elpool! För nu är det något fel! Ange läge 9."
               FELTEXT.PROGRAM = "MERTID" + STRING(TODAY) + STRING(TIME,"HH:MM") + Guru.Konstanter:globanv. 
            END.       
            IF TIDREGITAB.LONTILLAGG = ORDARB.MERKOD OR TIDREGITAB.LONTILLAGG = ORDARB.MERKOD2 THEN DO:
       	      ASSIGN TIDREGITAB.LONTILLAGG = "" TIDREGITAB.LONTILLANTAL = 0.
            END.
            ASSIGN
            bustart3 = starten3
            musz = FALSE.      
            ASSIGN TIDREGITAB.SLUT = hjstart 
            TIDREGITAB.UTRYCKNING = FALSE.      
            nytid = TIDREGITAB.TOTALT.
            RUN TIMSEK.P.
            ASSIGN
            nytid = TIDREGITAB.START.
            RUN TIMSEK.P.     
            ASSIGN
            regstartsek = sekunder
            nytid = TIDREGITAB.SLUT.
            RUN TIMSEK.P.         
            ASSIGN
            regslutsek = sekunder
            regdatum = TIDREGITAB.DATUM.         
            RUN TOTTID.P.
            ASSIGN TIDREGITAB.TOTALT = nytid
            regdatum = TIDREGITAB.DATUM
            regdatum = TIDREGITAB.DATUM.         
            nytid = TIDREGITAB.START.
            RUN TIMSEK.P.     
            ASSIGN
            regstartsek = sekunder
            nytid = TIDREGITAB.SLUT.
            RUN TIMSEK.P.         
            ASSIGN
            regslutsek = sekunder
            regdatum = TIDREGITAB.DATUM.         
            RUN TOTTID.P.
            ASSIGN TIDREGITAB.TOTALT = nytid.
            CREATE tidbuff.
            CREATE tidallt.
            tidallt.RECTIDVIS = RECID(tidbuff).
            IF TIDREGITAB.OVERTIDUTTAG = "K" OR TIDREGITAB.OVERTIDUTTAG = "L" THEN DO: 
               IF NOT AVAILABLE ORDARB THEN DO:
                  CREATE FELTEXT.
                  ASSIGN 
                  FELTEXT.ANVANDARE = Guru.Konstanter:globanv                      
                  FELTEXT.FELTEXT = "Kontakta Elpool! För nu är det något fel! Ange läge 10."
                  FELTEXT.PROGRAM = "MERTID" + STRING(TODAY) + STRING(TIME,"HH:MM") + Guru.Konstanter:globanv. 
               END. 
               ASSIGN tidbuff.LONTILLAGG = ORDARB.MERKOD2.
            END.
            ELSE IF TIDREGITAB.OVERTIDUTTAG = "Ö" THEN DO:                       
               IF NOT AVAILABLE ORDARB THEN DO:
                  CREATE FELTEXT.
                  ASSIGN 
                  FELTEXT.ANVANDARE = Guru.Konstanter:globanv                      
                  FELTEXT.FELTEXT = "Kontakta Elpool! För nu är det något fel! Ange läge 11."
                  FELTEXT.PROGRAM = "MERTID" + STRING(TODAY) + STRING(TIME,"HH:MM") + Guru.Konstanter:globanv. 
               END. 
      	      ASSIGN tidbuff.LONTILLAGG = ORDARB.MERKOD.
            END.         
            ELSE IF TIDREGITAB.OVERTIDUTTAG = "F" THEN DO: 
               IF NOT AVAILABLE ORDARB THEN DO:
                  CREATE FELTEXT.
                  ASSIGN 
                  FELTEXT.ANVANDARE = Guru.Konstanter:globanv                      
                  FELTEXT.FELTEXT = "Kontakta Elpool! För nu är det något fel! Ange läge 11."
                  FELTEXT.PROGRAM = "MERTID" + STRING(TODAY) + STRING(TIME,"HH:MM") + Guru.Konstanter:globanv. 
               END.
               IF TIDREGITAB.LONTILLAGG = ORDARB.MERKOD OR TIDREGITAB.LONTILLAGG = ORDARB.MERKOD2 THEN DO:                      
                  ASSIGN TIDREGITAB.LONTILLAGG = "" TIDREGITAB.LONTILLANTAL = 0.
               END.   
            END.
            ASSIGN 
            tidbuff.PERSONALKOD = TIDREGITAB.PERSONALKOD 
            tidbuff.AONR = TIDREGITAB.AONR 
            SUBSTRING(tidbuff.PROGRAM,1,158) = "MERTID" + STRING(TODAY) + STRING(TIME,"HH:MM") + Guru.Konstanter:globanv
            tidbuff.OVERTIDTILL = TIDREGITAB.OVERTIDTILL
            tidbuff.DELNR = TIDREGITAB.DELNR
            tidbuff.TRAKTAMENTE = TIDREGITAB.TRAKTAMENTE
            tidbuff.DATUM = TIDREGITAB.DATUM 
            tidbuff.VECKONUMMER = TIDREGITAB.VECKONUMMER 
            tidbuff.DAG = TIDREGITAB.DAG                     
            tidbuff.OVERTIDUTTAG = TIDREGITAB.OVERTIDUTTAG
            tidbuff.START = hjstart                          
            tidbuff.SLUT = regstart
            tidbuff.PRISTYP = TIDREGITAB.PRISTYP
            tidbuff.PRIS = TIDREGITAB.PRIS
            tidbuff.RESMAL = TIDREGITAB.RESMAL.     
            IF slutet3 > regstart THEN DO:                    
               ASSIGN
               nytid = tidbuff.START.
         	   RUN TIMSEK.P.  
         	   ASSIGN
         	   regstartsek = sekunder
               nytid = tidbuff.SLUT.
         	   RUN TIMSEK.P.         
         	   ASSIGN
         	   regslutsek = sekunder
         	   regdatum = tidbuff.DATUM.      	   
         	   RUN TOTTID.P.
         	   ASSIGN tidbuff.TOTALT = nytid.
         	   CREATE tidbuff.                    
               CREATE tidallt.
               tidallt.RECTIDVIS = RECID(tidbuff).
         	   ASSIGN 
               tidbuff.PERSONALKOD = TIDREGITAB.PERSONALKOD 
               tidbuff.AONR = TIDREGITAB.AONR 
               SUBSTRING(tidbuff.PROGRAM,1,158) = "MERTID" + STRING(TODAY) + STRING(TIME,"HH:MM") + Guru.Konstanter:globanv
               tidbuff.OVERTIDTILL = TIDREGITAB.OVERTIDTILL
               tidbuff.DELNR = TIDREGITAB.DELNR
               tidbuff.TRAKTAMENTE = TIDREGITAB.TRAKTAMENTE
               tidbuff.DATUM = TIDREGITAB.DATUM 
               tidbuff.VECKONUMMER = TIDREGITAB.VECKONUMMER 
               tidbuff.DAG = TIDREGITAB.DAG   
               tidbuff.OVERTIDUTTAG = TIDREGITAB.OVERTIDUTTAG
               tidbuff.START = regstart 
               tidbuff.SLUT = slutet3
               tidbuff.PRISTYP = TIDREGITAB.PRISTYP
               tidbuff.PRIS = TIDREGITAB.PRIS
               tidbuff.RESMAL = TIDREGITAB.RESMAL.     
               ASSIGN
               nytid = tidbuff.START.
         	   RUN TIMSEK.P.  
         	   ASSIGN
         	   regstartsek = sekunder
         	   nytid = tidbuff.SLUT.
         	   RUN TIMSEK.P.         
         	   ASSIGN
         	   regslutsek = sekunder
         	   regdatum = tidbuff.DATUM.      	   
         	   RUN TOTTID.P.
         	   ASSIGN tidbuff.TOTALT = nytid
               tidtabrec = RECID(tidbuff).       
               FIND TIDREGITAB WHERE RECID(TIDREGITAB) = tidtabrec NO-LOCK NO-ERROR.
            END.
            ELSE IF slutet3 LE regstart THEN DO:
      	      ASSIGN tidbuff.SLUT = slutet3	 
         	   nytid = tidbuff.TOTALT.
         	   RUN TIMSEK.P.                
         	   ASSIGN
               nytid = tidbuff.START.
         	   RUN TIMSEK.P.     
         	   ASSIGN
         	   regstartsek = sekunder
         	   nytid = tidbuff.SLUT.
         	   RUN TIMSEK.P.         
         	   ASSIGN
         	   regslutsek = sekunder
         	   regdatum = tidbuff.DATUM.      	   
         	   RUN TOTTID.P.
         	   ASSIGN tidbuff.TOTALT = nytid.
            END.
         END.
         ELSE IF TIDREGITAB.START GE hjstart THEN DO TRANSACTION:             
            FIND TIDREGITAB WHERE RECID(TIDREGITAB) = tidtabrec
            EXCLUSIVE-LOCK NO-ERROR.
            bustart3 = starten3.
            IF NOT AVAILABLE ORDARB THEN DO:
               CREATE FELTEXT.
               ASSIGN 
               FELTEXT.ANVANDARE = Guru.Konstanter:globanv                      
               FELTEXT.FELTEXT = "Kontakta Elpool! För nu är det något fel! Ange läge 12."
               FELTEXT.PROGRAM = "MERTID" + STRING(TODAY) + STRING(TIME,"HH:MM") + Guru.Konstanter:globanv. 
            END. 
            ASSIGN TIDREGITAB.LONTILLAGG = ORDARB.MERKOD.
            IF TIDREGITAB.OVERTIDUTTAG = "K" THEN DO:
               ASSIGN TIDREGITAB.LONTILLAGG = ORDARB.MERKOD2.
            END.
            ELSE DO:
              ASSIGN TIDREGITAB.LONTILLAGG = ORDARB.MERKOD.
            END.      
            regdatum = TIDREGITAB.DATUM.
            IF slutet3 > regstart THEN DO:               
      	      ASSIGN TIDREGITAB.SLUT = regstart
               nytid = TIDREGITAB.TOTALT.
               RUN TIMSEK.P.                    
           	   ASSIGN
               nytid = TIDREGITAB.START.
         	   RUN TIMSEK.P.     
         	   ASSIGN
         	   regstartsek = sekunder
         	   nytid = TIDREGITAB.SLUT.
         	   RUN TIMSEK.P.         
         	   ASSIGN
         	   regslutsek = sekunder
         	   regdatum = TIDREGITAB.DATUM.      	   
         	   RUN TOTTID.P.
         	   ASSIGN TIDREGITAB.TOTALT = nytid.
         	   CREATE tidbuff.                   
               CREATE tidallt.
               tidallt.RECTIDVIS = RECID(tidbuff).
         	   ASSIGN 
               tidbuff.PERSONALKOD = TIDREGITAB.PERSONALKOD 
               tidbuff.AONR = TIDREGITAB.AONR 
               SUBSTRING(tidbuff.PROGRAM,1,158) = "MERTID" + STRING(TODAY) + STRING(TIME,"HH:MM") + Guru.Konstanter:globanv
               tidbuff.OVERTIDTILL = TIDREGITAB.OVERTIDTILL
               tidbuff.DELNR = TIDREGITAB.DELNR
               tidbuff.TRAKTAMENTE = TIDREGITAB.TRAKTAMENTE
               tidbuff.DATUM = TIDREGITAB.DATUM 
               tidbuff.VECKONUMMER = TIDREGITAB.VECKONUMMER 
               tidbuff.DAG = TIDREGITAB.DAG
               tidbuff.OVERTIDUTTAG = TIDREGITAB.OVERTIDUTTAG
               tidbuff.START = regstart 
               tidbuff.SLUT = slutet3
               tidbuff.PRISTYP = TIDREGITAB.PRISTYP
               tidbuff.PRIS = TIDREGITAB.PRIS
               tidbuff.RESMAL = TIDREGITAB.RESMAL.     
               ASSIGN
               nytid = tidbuff.START.
         	   RUN TIMSEK.P.  
         	   ASSIGN
         	   regstartsek = sekunder
         	   nytid = tidbuff.SLUT.
         	   RUN TIMSEK.P.         
         	   ASSIGN
         	   regslutsek = sekunder
         	   regdatum = tidbuff.DATUM.      	   
         	   RUN TOTTID.P.
         	   ASSIGN tidbuff.TOTALT = nytid
         	   tidtabrec = RECID(tidbuff).
         	   FIND TIDREGITAB WHERE RECID(TIDREGITAB) = tidtabrec NO-LOCK NO-ERROR.
            END.
            ELSE IF slutet3 < regstart THEN DO:
      	      ASSIGN TIDREGITAB.SLUT = slutet3	  
      	      nytid = TIDREGITAB.TOTALT.
      	      RUN TIMSEK.P.
               ASSIGN
               nytid = TIDREGITAB.START.
         	   RUN TIMSEK.P.     
         	   ASSIGN
               regstartsek = sekunder
      	      nytid = TIDREGITAB.SLUT.
      	      RUN TIMSEK.P.         
      	      ASSIGN
      	      regslutsek = sekunder
      	      regdatum = TIDREGITAB.DATUM.   	      
      	      RUN TOTTID.P.
      	      ASSIGN TIDREGITAB.TOTALT = nytid.
            END.
         END.
         IF TIDREGITAB.SLUT GE regstart AND slutet3 > regstart THEN DO TRANSACTION:                                
            CREATE tidbuff.                   
            CREATE tidallt.
            tidallt.RECTIDVIS = RECID(tidbuff).
            ASSIGN 
            tidbuff.PERSONALKOD = TIDREGITAB.PERSONALKOD 
            tidbuff.AONR = TIDREGITAB.AONR 
            SUBSTRING(tidbuff.PROGRAM,1,158) = "MERTID" + STRING(TODAY) + STRING(TIME,"HH:MM") + Guru.Konstanter:globanv
            tidbuff.OVERTIDTILL = TIDREGITAB.OVERTIDTILL
            tidbuff.DELNR = TIDREGITAB.DELNR
            tidbuff.TRAKTAMENTE = TIDREGITAB.TRAKTAMENTE
            tidbuff.DATUM = TIDREGITAB.DATUM 
            tidbuff.VECKONUMMER = TIDREGITAB.VECKONUMMER 
            tidbuff.DAG = TIDREGITAB.DAG
            tidbuff.OVERTIDUTTAG = TIDREGITAB.OVERTIDUTTAG
            tidbuff.START = regstart 
            tidbuff.SLUT = slutet3
            tidbuff.PRISTYP = TIDREGITAB.PRISTYP
            tidbuff.PRIS = TIDREGITAB.PRIS
            tidbuff.RESMAL = TIDREGITAB.RESMAL.     
            ASSIGN
            nytid = tidbuff.START.
            RUN TIMSEK.P.  
            ASSIGN
            regstartsek = sekunder
            nytid = tidbuff.SLUT.
            RUN TIMSEK.P.         
            ASSIGN
            regslutsek = sekunder
            regdatum = tidbuff.DATUM.         
            RUN TOTTID.P.
            ASSIGN tidbuff.TOTALT = nytid
            tidtabrec = RECID(tidbuff).       
            FIND TIDREGITAB WHERE RECID(TIDREGITAB) = tidtabrec NO-LOCK NO-ERROR.
         END.
      END.
   END.
   ELSE IF regslut = hjslut and TIDREGITAB.START < regslut AND TIDREGITAB.SLUT > regslut 
   THEN DO TRANSACTION:   
      IF TIDREGITAB.OVERTIDUTTAG = "F" THEN musz = musz.
      ELSE DO:      
         FIND TIDREGITAB WHERE RECID(TIDREGITAB) = tidtabrec
         EXCLUSIVE-LOCK NO-ERROR.    
         ASSIGN TIDREGITAB.SLUT = hjslut
         bustart3 = hjslut
         musz = FALSE	  
         nytid = TIDREGITAB.TOTALT.
         RUN TIMSEK.P.
         ASSIGN
         nytid = TIDREGITAB.START.
         RUN TIMSEK.P.     
         ASSIGN
         regstartsek = sekunder
         nytid = TIDREGITAB.SLUT.
         RUN TIMSEK.P.         
         ASSIGN
         regslutsek = sekunder
         regdatum = TIDREGITAB.DATUM.      
         RUN TOTTID.P.
         ASSIGN TIDREGITAB.TOTALT = nytid.         
         CREATE tidbuff.
         CREATE tidallt.
         tidallt.RECTIDVIS = RECID(tidbuff).
         ASSIGN 
         tidbuff.PERSONALKOD = TIDREGITAB.PERSONALKOD 
         tidbuff.AONR = TIDREGITAB.AONR 
         SUBSTRING(tidbuff.PROGRAM,1,158) = "MERTID" + STRING(TODAY) + STRING(TIME,"HH:MM") + Guru.Konstanter:globanv
         tidbuff.OVERTIDTILL = TIDREGITAB.OVERTIDTILL
         tidbuff.DELNR = TIDREGITAB.DELNR
         tidbuff.TRAKTAMENTE = TIDREGITAB.TRAKTAMENTE
         tidbuff.DATUM = TIDREGITAB.DATUM 
         tidbuff.VECKONUMMER = TIDREGITAB.VECKONUMMER 
         tidbuff.DAG = TIDREGITAB.DAG      
         tidbuff.OVERTIDUTTAG = TIDREGITAB.OVERTIDUTTAG
         tidbuff.START = hjslut 
         tidbuff.SLUT = slutet3
         tidbuff.PRISTYP = TIDREGITAB.PRISTYP
         tidbuff.PRIS = TIDREGITAB.PRIS
         tidbuff.RESMAL = TIDREGITAB.RESMAL.     
         ASSIGN         
         nytid = tidbuff.START.
         RUN TIMSEK.P.        
         ASSIGN
         regstartsek = sekunder
         nytid = tidbuff.SLUT.
         RUN TIMSEK.P.         
         ASSIGN
         regslutsek = sekunder
         regdatum = tidbuff.DATUM.      
         RUN TOTTID.P.
         ASSIGN tidbuff.TOTALT = nytid      
         tidtabrec = RECID(tidbuff).
         FIND TIDREGITAB WHERE RECID(TIDREGITAB) = tidtabrec
         NO-LOCK NO-ERROR.	 
      END.
   END.
   ELSE IF TIDREGITAB.START GE regslut AND TIDREGITAB.START GE hjslut THEN DO:
      musz = FALSE.
   END.
   ELSE IF TIDREGITAB.SLUT LE regstart AND TIDREGITAB.SLUT LE hjstart THEN DO:
      musz = FALSE.
   END.
END.
    
