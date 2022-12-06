/*TIDUPPDW.P*/
{APP.I}
&Scoped-define NEW NEW
{TIDALLT.I}
DEFINE VARIABLE tidtabrec1 AS RECID NO-UNDO. 
DEFINE VARIABLE hjstart LIKE TIDREGITAB.START NO-UNDO.  
DEFINE VARIABLE hjslut LIKE TIDREGITAB.SLUT NO-UNDO.   
DEFINE VARIABLE regdatumspar AS DATE NO-UNDO.             
DEFINE VARIABLE upprec AS RECID NO-UNDO.
DEFINE VARIABLE lunchreg AS INTEGER NO-UNDO.
DEFINE VARIABLE hjrstartsek AS INTEGER NO-UNDO.
DEFINE VARIABLE hjrslutsek AS INTEGER NO-UNDO.
DEFINE VARIABLE hjarstart AS DECIMAL NO-UNDO.
DEFINE VARIABLE hjarslut AS DECIMAL NO-UNDO.
DEFINE VARIABLE hjstsl AS DECIMAL NO-UNDO.
DEFINE VARIABLE hjalstart AS DECIMAL NO-UNDO.
DEFINE VARIABLE hjalslut AS DECIMAL NO-UNDO.
DEFINE VARIABLE hjrestart AS DECIMAL NO-UNDO.
DEFINE VARIABLE hjreslut AS DECIMAL NO-UNDO.
DEFINE VARIABLE hjlustart AS DECIMAL NO-UNDO.
DEFINE VARIABLE hjluslut AS DECIMAL NO-UNDO.
DEFINE VARIABLE ehjstart LIKE TIDREGITAB.START NO-UNDO.  
DEFINE VARIABLE arbnr AS CHARACTER NO-UNDO.
DEFINE VARIABLE arregvnr AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE extraaonr LIKE TIDREGITAB.AONR NO-UNDO.
DEFINE NEW SHARED VARIABLE extradnr LIKE TIDREGITAB.DELNR NO-UNDO.

DEFINE BUFFER tidbuff FOR TIDREGITAB.        
DEFINE BUFFER tidbuff2 FOR TIDREGITAB.        
DEFINE QUERY traktq FOR tidbuff.
{TIDUPPUTW.I}
RUN REGVEC.P. 
RUN REGDAG.P.           
DEBUGGER:SET-BREAK().
FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = tidapptemp.PERSONALKOD NO-LOCK NO-ERROR.
persrec = RECID(PERSONALTAB).
FIND FIRST FORETAG NO-LOCK NO-ERROR.
   Guru.Konstanter:globforetag = FORETAG.FORETAG.
{FORESTYR.I}
FOR EACH extratidallt:      
   DO TRANSACTION:      
      IF extratidallt.RECTIDVIS = ? THEN DO: 
         CREATE TIDREGITAB.
      END.
      ELSE DO:
         FIND TIDREGITAB WHERE RECID(TIDREGITAB) = extratidallt.RECTIDVIS EXCLUSIVE-LOCK NO-ERROR.
      END.      
      BUFFER-COPY extratidallt EXCEPT extratidallt.RECTIDVIS      
      TO TIDREGITAB.
      TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD.
      tidtabrec = RECID(TIDREGITAB).
      placerarec = RECID(TIDREGITAB).      
      IF Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:
         lunchreg = extratidallt.LAGANTAL.
      END.
   END.
   RUN tider_UI.   
   DELETE extratidallt.
END.
FOR EACH tidallt WHERE tidallt.DATUM = ?:
   DELETE tidallt.
END.
PROCEDURE coptider_UI :   
   FOR EACH tidallt WHERE tidallt.PERSONALKOD = "":
      FIND TIDREGITAB WHERE RECID(TIDREGITAB) = tidallt.RECTIDVIS NO-LOCK NO-ERROR.
      IF AVAILABLE TIDREGITAB THEN DO:
         BUFFER-COPY TIDREGITAB TO tidallt.
         tidallt.RECTIDVIS = RECID(TIDREGITAB).
         IF tidallt.LONTILLAGG NE "" THEN DO:
            FIND FIRST LONTILL WHERE LONTILL.KOD = ANSTFORMTAB.KOD AND 
            LONTILL.LONTILLAGG = tidallt.LONTILLAGG NO-LOCK NO-ERROR.
            IF AVAILABLE LONTILL THEN DO:
               tidallt.VILART = LONTILL.VILART. 
            END.
            tidallt.TYP = "LON".
         END.
         IF tidallt.BEREDSKAP NE "" THEN DO:
            FIND FIRST BERKOD WHERE BERKOD.BEREDSKAPSAVTAL = PERSONALTAB.BEREDSKAPSAVTAL AND
            BERKOD.BEREDSKAP = tidallt.BEREDSKAP NO-LOCK NO-ERROR.
            IF AVAILABLE BERKOD THEN DO:
               tidallt.VILART = BERKOD.VILART. 
            END.
            tidallt.TYP = "BER".
         END.
         IF tidallt.TRAKTKOD NE "" THEN DO:
            FIND FIRST TRAKTATAB WHERE TRAKTATAB.TRAAVTAL = PERSONALTAB.TRAAVTAL AND 
            TRAKTATAB.TRAKTKOD = tidallt.TRAKTKOD NO-LOCK NO-ERROR.    
            IF AVAILABLE TRAKTATAB THEN DO:
               tidallt.VILART = TRAKTATAB.VILART. 
            END.
            tidallt.TYP = "TRA".        
         END.
      END.
      ELSE DELETE tidallt.
      placerarec = RECID(TIDREGITAB).
   END.
END PROCEDURE.
PROCEDURE tider_UI :
   FIND FIRST AONRTAB WHERE AONRTAB.AONR = TIDREGITAB.AONR AND AONRTAB.DELNR = 
   TIDREGITAB.DELNR USE-INDEX AONR NO-LOCK NO-ERROR.
   IF AVAILABLE AONRTAB THEN DO:
      IF AONRTAB.PRISTYP = "FRÅNVARO." AND TIDREGITAB.PRISTYP NE "FRÅNVARO." THEN DO TRANSACTION:
         FIND CURRENT TIDREGITAB EXCLUSIVE-LOCK NO-ERROR.
         ASSIGN TIDREGITAB.PRISTYP = "FRÅNVARO." TIDREGITAB.PRIS = 0.
      END.   
   END.   
   arbnr = TIDREGITAB.AONR.      
   FIND FIRST ANSTFORMTAB WHERE ANSTFORMTAB.ANSTALLNING = PERSONALTAB.ANSTALLNING 
   USE-INDEX ANSTF NO-LOCK NO-ERROR.
   /*ANST*/ 
   IF NOT AVAILABLE TIDREGITAB THEN DO TRANSACTION:
      CREATE FELTEXT.
      ASSIGN 
      FELTEXT.ANVANDARE = Guru.Konstanter:globanv                      
      FELTEXT.FELTEXT = "Kontakta Elpool! För nu är det något fel! Ange läge 2."
      FELTEXT.PROGRAM = "TIDUPPDE" + STRING(TODAY) + STRING(TIME,"HH:MM") + Guru.Konstanter:globanv.   
   END. 
   FIND FIRST ORDARB WHERE ORDARB.ANSTALLNING = PERSONALTAB.ANSTALLNING 
   USE-INDEX ORDARB NO-LOCK NO-ERROR.
   IF NOT AVAILABLE ORDARB THEN DO TRANSACTION:
      CREATE FELTEXT.
      ASSIGN 
      FELTEXT.ANVANDARE = Guru.Konstanter:globanv                      
      FELTEXT.FELTEXT = "Kontakta Elpool! För nu är det något fel! Ange läge 0."
      FELTEXT.PROGRAM = "TIDUPPDE" + STRING(TODAY) + STRING(TIME,"HH:MM") + Guru.Konstanter:globanv.   
   END. 
   ELSE DO: 
      sekunder = ORDARB.START1.
      RUN SEKTIM.P.
      ASSIGN
      hjstart = nytid
      ehjstart = hjstart.
      IF ORDARB.OBKOD NE "" THEN DO:
         IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "LULE" THEN DO:         
            FIND FIRST FLEXAVT WHERE FLEXAVT.PERSONALKOD = PERSONALTAB.PERSONALKOD NO-LOCK NO-ERROR.
            FIND FIRST FLEXREG WHERE FLEXREG.KOD = FLEXAVT.FLEXKOD NO-LOCK NO-ERROR.
            arregvnr = INTEGER(SUBSTRING(STRING(YEAR(regdatum),"9999"),1,3) + STRING(regvnr,"999")).
            FIND FIRST VECKOARBAV WHERE VECKOARBAV.PERSONALKOD = PERSONALTAB.PERSONALKOD AND VECKOARBAV.VECKONUMMER = arregvnr
            NO-LOCK NO-ERROR.            
         END.
         ELSE DO:         
            FIND FIRST FLEXREG WHERE FLEXREG.KOD = ANSTFORMTAB.KOD NO-LOCK NO-ERROR.
         END.
         IF NOT AVAILABLE FLEXREG THEN DO:
            FIND FIRST FLEXREG WHERE FLEXREG.KOD = "" NO-LOCK NO-ERROR.
         END.      
         IF AVAILABLE FLEXREG THEN DO:
            IF MONTH(regdatum) > MONTH(FLEXREG.SOMMARST) AND MONTH(regdatum) < MONTH(FLEXREG.SOMMARSL) THEN DO:               
               ASSIGN hjslut = DECIMAL(ORDARB.OBKOD).                        
               IF Guru.Konstanter:globforetag = "SUND" AND FLEXREG.KOD = "T" THEN hjstart = 7.30.                                                                      
               IF Guru.Konstanter:globforetag = "SNAT" THEN DO:
                  IF AVAILABLE VECKOARBAV THEN DO:
                     IF FLEXREG.KOD = "TE" AND (VECKOARBAV.VECKOSCHEMA = 19 OR VECKOARBAV.VECKOSCHEMA = 31) THEN.
                     ELSE hjstart = 7.30.
                  END.
                  ELSE IF FLEXREG.KOD BEGINS "T" THEN hjstart = 7.30.                                                       
               END.                                        
               IF Guru.Konstanter:globforetag = "MISV" AND FLEXREG.KOD = "T" THEN hjstart = 7.30.                                           
            END.
            ELSE IF MONTH(regdatum) = MONTH(FLEXREG.SOMMARSL) AND DAY(regdatum) <= DAY(FLEXREG.SOMMARSL) THEN DO:
               ASSIGN hjslut = DECIMAL(ORDARB.OBKOD).
               IF Guru.Konstanter:globforetag = "SUND" AND FLEXREG.KOD = "T" THEN hjstart = 7.30.               
               IF Guru.Konstanter:globforetag = "SNAT" THEN DO:
                  IF AVAILABLE VECKOARBAV THEN DO:
                     IF FLEXREG.KOD = "TE" AND (VECKOARBAV.VECKOSCHEMA = 19 OR VECKOARBAV.VECKOSCHEMA = 31) THEN.
                     ELSE hjstart = 7.30.
                  END.
                  ELSE IF FLEXREG.KOD BEGINS "T" THEN hjstart = 7.30.                                                       
               END.                                                           
               IF Guru.Konstanter:globforetag = "MISV" AND FLEXREG.KOD = "T" THEN hjstart = 7.30.
                             
            END.
            ELSE IF MONTH(regdatum) = MONTH(FLEXREG.SOMMARST) AND DAY(regdatum) >= DAY(FLEXREG.SOMMARST) THEN DO: 
               ASSIGN hjslut = DECIMAL(ORDARB.OBKOD).
               IF Guru.Konstanter:globforetag = "SUND" AND FLEXREG.KOD = "T" THEN hjstart = 7.30.               
               IF Guru.Konstanter:globforetag = "SNAT" THEN DO:
                  IF AVAILABLE VECKOARBAV THEN DO:
                     IF FLEXREG.KOD = "TE" AND (VECKOARBAV.VECKOSCHEMA = 19 OR VECKOARBAV.VECKOSCHEMA = 31) THEN.
                     ELSE hjstart = 7.30.
                  END.
                  ELSE IF FLEXREG.KOD BEGINS "T" THEN hjstart = 7.30.                                                       
               END.                                                        
               IF Guru.Konstanter:globforetag = "MISV" AND FLEXREG.KOD = "T" THEN hjstart = 7.30.                             
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
   regdatum = regdatum + 1.
   RUN REGVEC.P. 
   RUN SLUTARB.P.   
   ASSIGN
   hjarstart = regstart
   hjarslut = regslut
   hjalstart = lunchstarten
   hjalslut = lunchslutet
   regdatum = regdatum - 1.
   bustart3 = TIDREGITAB.START.
   RUN REGVEC.P. 
   RUN SLUTARB.P.        
   ASSIGN
   hjrestart = regstart
   hjreslut = regslut
   hjlustart = lunchstarten
   hjluslut = lunchslutet.      
   IF PERSONALTAB.DELTID = FALSE AND regstart = regslut THEN DO TRANSACTION:             
      FIND TIDREGITAB WHERE RECID(TIDREGITAB) = tidtabrec EXCLUSIVE-LOCK NO-ERROR.
      IF TIDREGITAB.START > TIDREGITAB.SLUT THEN DO: 
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
         tidbuff.DATUM = TIDREGITAB.DATUM + 1
         tidbuff.UTRYCK = TIDREGITAB.UTRYCK  
         tidbuff.OVERTIDUTTAG = TIDREGITAB.OVERTIDUTTAG
         tidbuff.RESMAL = TIDREGITAB.RESMAL
         tidbuff.START = 00.00
         tidbuff.SLUT = TIDREGITAB.SLUT.
         tidtabrec = RECID(tidbuff).      
         regdatum = tidbuff.DATUM.
         RUN REGVEC.P. 
         RUN REGDAG.P.           
         regdatum = TIDREGITAB.DATUM.
         ASSIGN 
         tidbuff.VECKONUMMER = regvnr 
         tidbuff.DAG = regdagnamn.
         ASSIGN TIDREGITAB.SLUT = 24.00.        
         FIND TIDREGITAB WHERE RECID(TIDREGITAB) = tidtabrec NO-ERROR.
      END.  
   END.
   ELSE IF PERSONALTAB.DELTID = TRUE THEN DO:                              
      IF regstart = hjstart AND regslut = hjslut THEN DO : 
         /*en deltidare som jobbar full dag vissa dagar ska ha övertid direkt*/                  
         IF TIDREGITAB.START < regstart AND TIDREGITAB.SLUT > regstart THEN DO TRANSACTION:      
            IF TIDREGITAB.OVERTIDUTTAG = "F" THEN musz = musz.
            ELSE DO:      
               bustart3 = TIDREGITAB.START.      
               tidtabrec1 = RECID(TIDREGITAB).
               CREATE tidbuff.
               CREATE tidallt.
               tidallt.RECTIDVIS = RECID(tidbuff).
               ASSIGN 
               tidbuff.PERSONALKOD = TIDREGITAB.PERSONALKOD
               tidbuff.AONR = TIDREGITAB.AONR
               tidbuff.DELNR = TIDREGITAB.DELNR
               SUBSTRING(tidbuff.PROGRAM,1,158) = "TIDUPPDE" + STRING(TODAY) + STRING(TIME,"HH:MM") + Guru.Konstanter:globanv
               tidbuff.OVERTIDTILL = TIDREGITAB.OVERTIDTILL
               tidbuff.PRISTYP     = TIDREGITAB.PRISTYP
               tidbuff.PRIS        = TIDREGITAB.PRIS
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
               RUN prisover_UI (INPUT 1).                        
               RUN OTOLKPR.P.            
               regdatum = regdatumspar.
               FIND TIDREGITAB WHERE RECID(TIDREGITAB) = tidtabrec NO-LOCK NO-ERROR.      
               IF TIDREGITAB.SLUT LE hjslut AND TIDREGITAB.SLUT > hjstart THEN musz = TRUE. 
               FIND TIDREGITAB WHERE RECID(TIDREGITAB) = tidtabrec EXCLUSIVE-LOCK NO-ERROR.            
            END.
         END.
         IF TIDREGITAB.START < regslut AND TIDREGITAB.SLUT > regslut THEN DO TRANSACTION:            
            IF TIDREGITAB.OVERTIDUTTAG = "F" THEN musz = musz.
            ELSE IF Guru.Konstanter:globforetag = "GKAL" AND TIDREGITAB.OVERTIDUTTAG = "I" THEN musz = musz.
            ELSE DO:      
               bustart3 = regslut.                
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
               tidbuff.START = regslut
               tidbuff.SLUT = TIDREGITAB.SLUT
               tidbuff.VECKONUMMER = TIDREGITAB.VECKONUMMER 
               tidbuff.DAG = TIDREGITAB.DAG
               tidbuff.RESMAL = TIDREGITAB.RESMAL.  
               tidtabrec = RECID(tidbuff).       
               ASSIGN TIDREGITAB.SLUT = regslut.      
               IF TIDREGITAB.OANT1 > 0 THEN DO:               
                  ASSIGN TIDREGITAB.OANT1 = 0 TIDREGITAB.OKOD1 = ""
                  TIDREGITAB.OANT2 = 0 TIDREGITAB.OKOD2 = ""
                  TIDREGITAB.OANT3 = 0 TIDREGITAB.OKOD3 = "".
               END.                
               FIND TIDREGITAB WHERE RECID(TIDREGITAB) = tidtabrec NO-ERROR.
            END.
         END.   
         musz = TRUE.
         regdatumspar = regdatum.  
         RUN OTOLKPR.P.            
         regdatum = regdatumspar.

      END.
      ELSE DO:      
         musz = TRUE.      
         RUN MERTID.P.    
      END.
   END.
   ELSE IF TIDREGITAB.SLUT LE regslut AND TIDREGITAB.SLUT > regstart AND
   TIDREGITAB.START GE regstart AND TIDREGITAB.START < regslut AND 
   regstart NE regslut AND TIDREGITAB.START < TIDREGITAB.SLUT THEN DO:
      /*EJ ÖVERTID*/
      IF regstart = 0 AND regslut = 24 THEN DO:
         /*skift måste kolla om registreringen inte är på lunchen då det är övertid 0-6 22-24  lunch 6-22*/
         IF TIDREGITAB.START < lunchstarten AND TIDREGITAB.SLUT LE lunchstarten THEN musz = TRUE.
         ELSE IF TIDREGITAB.START GE lunchslutet AND TIDREGITAB.SLUT > lunchslutet THEN musz = TRUE.          
         ELSE IF TIDREGITAB.START < lunchstarten AND TIDREGITAB.SLUT > lunchslutet THEN DO TRANSACTION:
            /* skift- registrera inte luch uppehållet utan bara 0-6 22-24*/
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
            tidbuff.START = lunchslutet
            tidbuff.SLUT = TIDREGITAB.SLUT
            tidbuff.RESMAL = TIDREGITAB.RESMAL
            regdatum = tidbuff.DATUM.
            
            IF Guru.Konstanter:varforetypval[30] = 1 THEN DO:
               /*ob-sjuk*/
               IF arbnr = "110" THEN DO: 
                  RUN OBBERSJ.P.                  
               END.
               ELSE RUN OBBER.P.
            END.
            ELSE RUN OBBER.P. 
            ASSIGN 
            tidbuff.VECKONUMMER = regvnr 
            tidbuff.DAG = regdagnamn
            tidtabrec1 = RECID(tidbuff).       
            ASSIGN       
            TIDREGITAB.SLUT = lunchstarten.                  
            IF Guru.Konstanter:varforetypval[30] = 1 THEN DO:
               /*ob-sjuk*/              
               IF arbnr = "110" THEN DO: 
                  RUN OBBERSJ.P.                  
               END.                 
               ELSE RUN OBBER.P.
            END.
            ELSE RUN OBBER.P. 
            tidtabrec = tidtabrec1.       
            FIND TIDREGITAB WHERE RECID(TIDREGITAB) = tidtabrec NO-ERROR.
            musz = TRUE.          
         END.
         IF musz = FALSE THEN DO:
            /*skift övertid på "lunchen" 06-22 (schema 0-6 22-24)*/
            bustart3 = TIDREGITAB.START.                      
            musz = TRUE.            
            RUN OTOLKPR.P.
         END.
      END.
      ELSE musz = TRUE.            
   END.
   ELSE IF TIDREGITAB.START > TIDREGITAB.SLUT AND ( regstart = 0 OR regslut = 24) THEN DO TRANSACTION:          
      /*skiftarbeande ska kunna registrera skiftviS ex 18-06 -ej dygnsvis*/      
      musz = TRUE.            
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
      tidbuff.DATUM = TIDREGITAB.DATUM  + 1
      tidbuff.UTRYCK = TIDREGITAB.UTRYCK   
      tidbuff.OVERTIDUTTAG = TIDREGITAB.OVERTIDUTTAG 
      tidbuff.START = 00.00
      tidbuff.SLUT = TIDREGITAB.SLUT
      tidbuff.RESMAL = TIDREGITAB.RESMAL
      regdatum = tidbuff.DATUM
      avdatum = tidbuff.DATUM.                  
      
      RUN REGVEC.P. 
      RUN REGDAG.P.      
      IF Guru.Konstanter:varforetypval[30] = 1 THEN DO:
         /*ob-sjuk*/
         IF arbnr = "110" THEN DO: 
            RUN OBBERSJ.P.            
         END.
         ELSE RUN OBBER.P.
      END.
      ELSE RUN OBBER.P. 
      ASSIGN 
      tidbuff.VECKONUMMER = regvnr 
      tidbuff.DAG = regdagnamn
      tidtabrec1 = RECID(tidbuff).    
      IF hjrestart NE 00 THEN DO:      
         musz = TRUE.            
         bustart3 = tidbuff.START.                      
         RUN OTOLKPR.P.            
      END.

      /*skift -registrera  övertid under lunchen 6-22*/
      IF hjarstart = 0 AND hjarslut = 24 AND hjalstart NE 0 THEN DO:      
         IF tidbuff.SLUT > hjalstart THEN DO:             
            /*lunch-övertid*/
            ASSIGN tidbuff.SLUT = hjalstart.          
            bustart3 = hjalstart.            
            CREATE tidbuff2.
            CREATE tidallt.
            tidallt.RECTIDVIS = RECID(tidbuff2).
            ASSIGN 
            tidbuff2.PERSONALKOD = TIDREGITAB.PERSONALKOD
            tidbuff2.AONR = TIDREGITAB.AONR
            tidbuff2.DELNR = TIDREGITAB.DELNR
            SUBSTRING(tidbuff2.PROGRAM,1,158) = "TIDUPPDE" + STRING(TODAY) + STRING(TIME,"HH:MM") + Guru.Konstanter:globanv
            tidbuff2.OVERTIDTILL = TIDREGITAB.OVERTIDTILL
            tidbuff2.PRISTYP = TIDREGITAB.PRISTYP
            tidbuff2.PRIS = TIDREGITAB.PRIS
            tidbuff2.TRAKTAMENTE = TIDREGITAB.TRAKTAMENTE
            tidbuff2.DATUM = TIDREGITAB.DATUM  + 1
            tidbuff2.UTRYCK = TIDREGITAB.UTRYCK   
            tidbuff2.OVERTIDUTTAG = TIDREGITAB.OVERTIDUTTAG 
            tidbuff2.START = hjalstart
            tidbuff2.SLUT = TIDREGITAB.SLUT
            tidbuff2.RESMAL = TIDREGITAB.RESMAL
            regdatum = tidbuff2.DATUM.            
            RUN REGVEC.P. 
            RUN REGDAG.P.            
            IF Guru.Konstanter:varforetypval[30] = 1 THEN DO:
               /*ob-sjuk*/
               IF arbnr = "110" THEN DO: 
                  RUN OBBERSJ.P.                  
               END.
               ELSE RUN OBBER.P.
            END.
            ELSE RUN OBBER.P. 
            ASSIGN 
            tidbuff2.VECKONUMMER = regvnr 
            tidbuff2.DAG = regdagnamn
            tidtabrec = RECID(tidbuff2).
            musz = TRUE.            
            RUN OTOLKPR.P.            
         END.
      END.
      ELSE IF tidbuff.SLUT > hjarslut AND hjarslut > 0  THEN DO:
         /*övertid i slutet på skiftet ex tid 22-06 sedan 6-9*/
         ASSIGN tidbuff.SLUT = hjarslut.          
         bustart3 = hjarslut.         
         CREATE tidbuff2.
         CREATE tidallt.
         tidallt.RECTIDVIS = RECID(tidbuff2).
         ASSIGN 
         tidbuff2.PERSONALKOD = TIDREGITAB.PERSONALKOD
         tidbuff2.AONR = TIDREGITAB.AONR
         tidbuff2.DELNR = TIDREGITAB.DELNR
         SUBSTRING(tidbuff2.PROGRAM,1,158) = "TIDUPPDE" + STRING(TODAY) + STRING(TIME,"HH:MM") + Guru.Konstanter:globanv
         tidbuff2.OVERTIDTILL = TIDREGITAB.OVERTIDTILL
         tidbuff2.PRISTYP = TIDREGITAB.PRISTYP
         tidbuff2.PRIS = TIDREGITAB.PRIS
         tidbuff2.TRAKTAMENTE = TIDREGITAB.TRAKTAMENTE
         tidbuff2.DATUM = TIDREGITAB.DATUM  + 1
         tidbuff2.UTRYCK = TIDREGITAB.UTRYCK   
         tidbuff2.OVERTIDUTTAG = TIDREGITAB.OVERTIDUTTAG 
         tidbuff2.START = hjarslut
         tidbuff2.SLUT = TIDREGITAB.SLUT
         tidbuff2.RESMAL = TIDREGITAB.RESMAL
         regdatum = tidbuff2.DATUM.
         RUN REGVEC.P. 
         RUN REGDAG.P.         
         IF Guru.Konstanter:varforetypval[30] = 1 THEN DO:
            /*ob-sjuk*/
            IF arbnr = "110" THEN DO: 
               RUN OBBERSJ.P.               
            END.
            ELSE RUN OBBER.P.
         END.
         ELSE RUN OBBER.P. 
         ASSIGN 
         tidbuff2.VECKONUMMER = regvnr 
         tidbuff2.DAG = regdagnamn         
         tidtabrec = RECID(tidbuff2).
         musz = TRUE.            
         RUN OTOLKPR.P.
      END.      
      ASSIGN
      regdatum = TIDREGITAB.DATUM
      bdatum = regdatum.
      ASSIGN       
      TIDREGITAB.SLUT = 24.00.        
      
      IF hjrestart = 0 AND hjreslut = 24 AND hjluslut NE 0 THEN DO:      
         IF TIDREGITAB.START < hjluslut THEN DO:                         
            /*lunch övertid första dygnet*/
            bustart3 = TIDREGITAB.START.
            CREATE tidbuff2.
            CREATE tidallt.
            tidallt.RECTIDVIS = RECID(tidbuff2).
            ASSIGN 
            tidbuff2.PERSONALKOD = TIDREGITAB.PERSONALKOD
            tidbuff2.AONR = TIDREGITAB.AONR
            tidbuff2.DELNR = TIDREGITAB.DELNR
            SUBSTRING(tidbuff2.PROGRAM,1,158) = "TIDUPPDE" + STRING(TODAY) + STRING(TIME,"HH:MM") + Guru.Konstanter:globanv
            tidbuff2.OVERTIDTILL = TIDREGITAB.OVERTIDTILL
            tidbuff2.PRISTYP = TIDREGITAB.PRISTYP
            tidbuff2.PRIS = TIDREGITAB.PRIS
            tidbuff2.TRAKTAMENTE = TIDREGITAB.TRAKTAMENTE
            tidbuff2.DATUM = TIDREGITAB.DATUM 
            tidbuff2.UTRYCK = TIDREGITAB.UTRYCK   
            tidbuff2.OVERTIDUTTAG = TIDREGITAB.OVERTIDUTTAG 
            tidbuff2.START = TIDREGITAB.START
            tidbuff2.SLUT = hjluslut
            tidbuff2.RESMAL = TIDREGITAB.RESMAL
            regdatum = tidbuff2.DATUM.            
            RUN REGVEC.P. 
            RUN REGDAG.P.            
            IF Guru.Konstanter:varforetypval[30] = 1 THEN DO:
               /*ob-sjuk*/
               IF arbnr = "110" THEN DO: 
                  RUN OBBERSJ.P.                  
               END.
               ELSE RUN OBBER.P.
            END.
            ELSE RUN OBBER.P. 
            ASSIGN 
            tidbuff2.VECKONUMMER = regvnr 
            tidbuff2.DAG = regdagnamn.            
            ASSIGN TIDREGITAB.START = hjluslut.          
            tidtabrec = RECID(tidbuff2).
            musz = TRUE.            
            RUN OTOLKPR.P.
         END.
      END.
      ELSE IF hjreslut = 24 AND hjrestart > 0 AND TIDREGITAB.START < hjrestart THEN DO:
         /*övertid före skiftet*/
         bustart3 = TIDREGITAB.START.
         musz  = FALSE.
         CREATE tidbuff2.
         CREATE tidallt.
         tidallt.RECTIDVIS = RECID(tidbuff2).
         ASSIGN 
         tidbuff2.PERSONALKOD = TIDREGITAB.PERSONALKOD
         tidbuff2.AONR = TIDREGITAB.AONR
         tidbuff2.DELNR = TIDREGITAB.DELNR
         SUBSTRING(tidbuff2.PROGRAM,1,158) = "TIDUPPDE" + STRING(TODAY) + STRING(TIME,"HH:MM") + Guru.Konstanter:globanv
         tidbuff2.OVERTIDTILL = TIDREGITAB.OVERTIDTILL
         tidbuff2.PRISTYP = TIDREGITAB.PRISTYP
         tidbuff2.PRIS = TIDREGITAB.PRIS
         tidbuff2.TRAKTAMENTE = TIDREGITAB.TRAKTAMENTE
         tidbuff2.DATUM = TIDREGITAB.DATUM 
         tidbuff2.UTRYCK = TIDREGITAB.UTRYCK   
         tidbuff2.OVERTIDUTTAG = TIDREGITAB.OVERTIDUTTAG 
         tidbuff2.START = TIDREGITAB.START
         tidbuff2.SLUT = hjrestart
         tidbuff2.RESMAL = TIDREGITAB.RESMAL
         regdatum = tidbuff2.DATUM.    
         RUN REGVEC.P. 
         RUN REGDAG.P.         
         IF Guru.Konstanter:varforetypval[30] = 1 THEN DO:
            /*ob-sjuk*/
            IF arbnr = "110" THEN DO: 
               RUN OBBERSJ.P.               
            END.
            ELSE RUN OBBER.P.
         END.
         ELSE RUN OBBER.P. 
         ASSIGN 
         tidbuff2.VECKONUMMER = regvnr 
         tidbuff2.DAG = regdagnamn
         tidtabrec = RECID(tidbuff2).       
         ASSIGN TIDREGITAB.START = hjrestart.          
         musz = TRUE.            
         RUN OTOLKPR.P.
      END.      
      /*skift -registrera inte tid under lunchen 6-22*/      
      IF Guru.Konstanter:varforetypval[30] = 1 THEN DO:
         /*ob-sjuk*/
         IF arbnr = "110" THEN DO: 
            RUN OBBERSJ.P.            
         END.
         ELSE RUN OBBER.P.
      END.
      ELSE RUN OBBER.P. 
      tidtabrec = tidtabrec1.       
      FIND TIDREGITAB WHERE RECID(TIDREGITAB) = tidtabrec NO-ERROR.        
   END.
   ELSE DO TRANSACTION:                
      FIND TIDREGITAB WHERE RECID(TIDREGITAB) = tidtabrec EXCLUSIVE-LOCK NO-ERROR.         
      IF TIDREGITAB.START > TIDREGITAB.SLUT THEN DO:
         /*övertid över dygnbryt*/         
         IF hjarstart = hjarslut THEN hjstsl = 24.
         ELSE hjstsl = hjarstart.
         IF TIDREGITAB.SLUT LE hjstsl AND TIDREGITAB.START GE regslut THEN DO:      
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
            tidbuff.DATUM = TIDREGITAB.DATUM + 1
            tidbuff.UTRYCK = TIDREGITAB.UTRYCK  
            tidbuff.OVERTIDUTTAG = TIDREGITAB.OVERTIDUTTAG 
            tidbuff.START = 00.00
            tidbuff.SLUT = TIDREGITAB.SLUT
            tidbuff.RESMAL = TIDREGITAB.RESMAL.
            regdatum = tidbuff.DATUM.
            RUN REGVEC.P. 
            RUN REGDAG.P.           
            regdatum = TIDREGITAB.DATUM.
            ASSIGN 
            tidbuff.VECKONUMMER = regvnr 
            tidbuff.DAG = regdagnamn.
            tidtabrec = RECID(tidbuff).       
            ASSIGN TIDREGITAB.SLUT = 24.00.
            FIND TIDREGITAB WHERE RECID(TIDREGITAB) = tidtabrec NO-ERROR.
         END.      
         ELSE IF TIDREGITAB.SLUT LE hjstsl AND TIDREGITAB.START < regslut THEN DO:      
            bustart3 = regslut.                
            IF TIDREGITAB.START < regstart THEN DO:             
               CREATE tidbuff.
               CREATE tidallt.
               tidallt.RECTIDVIS = RECID(tidbuff).
               upprec = RECID(tidbuff).
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
               tidbuff.START = TIDREGITAB.START          
               tidbuff.SLUT = regstart
               tidbuff.RESMAL = TIDREGITAB.RESMAL.
               regdatum = tidbuff.DATUM.
               RUN REGVEC.P. 
               RUN REGDAG.P.           
               regdatum = TIDREGITAB.DATUM.
               ASSIGN 
               tidbuff.VECKONUMMER = regvnr 
               tidbuff.DAG = regdagnamn.
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
               tidbuff.SLUT = regslut
               tidbuff.RESMAL = TIDREGITAB.RESMAL.
               regdatum = tidbuff.DATUM.
               RUN REGVEC.P. 
               RUN REGDAG.P.           
               regdatum = TIDREGITAB.DATUM.
               ASSIGN 
               tidbuff.VECKONUMMER = regvnr 
               tidbuff.DAG = regdagnamn.
            END.
            ELSE DO:   
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
               tidbuff.START = TIDREGITAB.START          
               tidbuff.SLUT = regslut
               tidbuff.RESMAL = TIDREGITAB.RESMAL.
               regdatum = tidbuff.DATUM.
               RUN REGVEC.P. 
               RUN REGDAG.P.           
               regdatum = TIDREGITAB.DATUM.
               ASSIGN 
               tidbuff.VECKONUMMER = regvnr 
               tidbuff.DAG = regdagnamn.       
            END.
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
            tidbuff.DATUM = TIDREGITAB.DATUM  + 1
            tidbuff.UTRYCK = TIDREGITAB.UTRYCK   
            tidbuff.OVERTIDUTTAG = TIDREGITAB.OVERTIDUTTAG 
            tidbuff.START = 00.00
            tidbuff.SLUT = TIDREGITAB.SLUT
            tidbuff.RESMAL = TIDREGITAB.RESMAL.
            regdatum = tidbuff.DATUM.
            RUN REGVEC.P. 
            RUN REGDAG.P.           
            regdatum = TIDREGITAB.DATUM.
            ASSIGN 
            tidbuff.VECKONUMMER = regvnr 
            tidbuff.DAG = regdagnamn.
            ASSIGN 
            TIDREGITAB.START = regslut.
            TIDREGITAB.SLUT = 24.00.       
            tidtabrec = RECID(tidbuff).       
            FIND TIDREGITAB WHERE RECID(TIDREGITAB) = tidtabrec NO-ERROR.
         END.       
         ELSE IF TIDREGITAB.SLUT > hjstsl AND TIDREGITAB.START GE regslut THEN DO:      
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
            tidbuff.DATUM = TIDREGITAB.DATUM + 1 
            tidbuff.UTRYCK = TIDREGITAB.UTRYCK   
            tidbuff.OVERTIDUTTAG = TIDREGITAB.OVERTIDUTTAG 
            tidbuff.START = 00.00
            tidbuff.SLUT = hjarstart
            tidbuff.RESMAL = TIDREGITAB.RESMAL.
            regdatum = tidbuff.DATUM.
            RUN REGVEC.P. 
            RUN REGDAG.P.           
            regdatum = TIDREGITAB.DATUM.
            ASSIGN 
            tidbuff.VECKONUMMER = regvnr 
            tidbuff.DAG = regdagnamn. 
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
            tidbuff.DATUM = TIDREGITAB.DATUM  + 1
            tidbuff.UTRYCK = TIDREGITAB.UTRYCK
            tidbuff.OVERTIDUTTAG = TIDREGITAB.OVERTIDUTTAG 
            tidbuff.START = hjarstart
            tidbuff.SLUT = TIDREGITAB.SLUT
            tidbuff.RESMAL = TIDREGITAB.RESMAL.
            regdatum = tidbuff.DATUM.
            RUN REGVEC.P. 
            RUN REGDAG.P.           
            regdatum = TIDREGITAB.DATUM.
            ASSIGN 
            tidbuff.VECKONUMMER = regvnr 
            tidbuff.DAG = regdagnamn.
            ASSIGN TIDREGITAB.SLUT = 24.00.
            tidtabrec = RECID(tidbuff).       
            FIND TIDREGITAB WHERE RECID(TIDREGITAB) = tidtabrec NO-ERROR.
         END.          
         ELSE IF TIDREGITAB.SLUT > hjstsl AND TIDREGITAB.START < regslut THEN DO:               
            IF TIDREGITAB.START < regstart THEN DO:             
               CREATE tidbuff.
               CREATE tidallt.
               tidallt.RECTIDVIS = RECID(tidbuff).
               upprec = RECID(tidbuff).
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
               tidbuff.START = TIDREGITAB.START          
               tidbuff.SLUT = regstart
               tidbuff.RESMAL = TIDREGITAB.RESMAL.
               regdatum = tidbuff.DATUM.
               RUN REGVEC.P. 
               RUN REGDAG.P.           
               regdatum = TIDREGITAB.DATUM.
               ASSIGN 
               tidbuff.VECKONUMMER = regvnr 
               tidbuff.DAG = regdagnamn.
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
               tidbuff.SLUT = regslut
               tidbuff.RESMAL = TIDREGITAB.RESMAL.
               regdatum = tidbuff.DATUM.
               RUN REGVEC.P. 
               RUN REGDAG.P.           
               regdatum = TIDREGITAB.DATUM.
               ASSIGN 
               tidbuff.VECKONUMMER = regvnr 
               tidbuff.DAG = regdagnamn.
            END.
            ELSE DO:                  
               bustart3 = regslut.                
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
               tidbuff.START = TIDREGITAB.START
               tidbuff.SLUT = regslut
               tidbuff.RESMAL = TIDREGITAB.RESMAL.
               regdatum = tidbuff.DATUM.
               RUN REGVEC.P. 
               RUN REGDAG.P.           
               regdatum = TIDREGITAB.DATUM.
               ASSIGN 
               tidbuff.VECKONUMMER = regvnr 
               tidbuff.DAG = regdagnamn. 
            END.
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
            tidbuff.DATUM = TIDREGITAB.DATUM  + 1
            tidbuff.UTRYCK = TIDREGITAB.UTRYCK   
            tidbuff.OVERTIDUTTAG = TIDREGITAB.OVERTIDUTTAG 
            tidbuff.START = 00.00
            tidbuff.SLUT = hjarstart
            tidbuff.RESMAL = TIDREGITAB.RESMAL.
            regdatum = tidbuff.DATUM.
            RUN REGVEC.P. 
            RUN REGDAG.P.           
            regdatum = TIDREGITAB.DATUM.
            ASSIGN 
            tidbuff.VECKONUMMER = regvnr 
            tidbuff.DAG = regdagnamn.
            IF TIDREGITAB.SLUT > hjarslut  THEN DO:
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
               tidbuff.DATUM = TIDREGITAB.DATUM  + 1
               tidbuff.UTRYCK = TIDREGITAB.UTRYCK   
               tidbuff.OVERTIDUTTAG = TIDREGITAB.OVERTIDUTTAG 
               tidbuff.START = hjarstart
               tidbuff.SLUT = hjarslut
               tidbuff.RESMAL = TIDREGITAB.RESMAL.
               regdatum = tidbuff.DATUM.
               RUN REGVEC.P. 
               RUN REGDAG.P.           
               regdatum = TIDREGITAB.DATUM.
               ASSIGN 
               tidbuff.VECKONUMMER = regvnr 
               tidbuff.DAG = regdagnamn.
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
               tidbuff.DATUM = TIDREGITAB.DATUM  + 1
               tidbuff.UTRYCK = TIDREGITAB.UTRYCK   
               tidbuff.OVERTIDUTTAG = TIDREGITAB.OVERTIDUTTAG 
               tidbuff.START = hjarslut
               tidbuff.SLUT = TIDREGITAB.SLUT
               tidbuff.RESMAL = TIDREGITAB.RESMAL.
               regdatum = tidbuff.DATUM.
               RUN REGVEC.P. 
               RUN REGDAG.P.           
               regdatum = TIDREGITAB.DATUM.
               ASSIGN 
               tidbuff.VECKONUMMER = regvnr 
               tidbuff.DAG = regdagnamn.
            END.
            ELSE DO:            
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
               tidbuff.DATUM = TIDREGITAB.DATUM  + 1
               tidbuff.UTRYCK = TIDREGITAB.UTRYCK   
               tidbuff.OVERTIDUTTAG = TIDREGITAB.OVERTIDUTTAG 
               tidbuff.START = hjarstart
               tidbuff.SLUT = TIDREGITAB.SLUT
               tidbuff.RESMAL = TIDREGITAB.RESMAL.
               regdatum = tidbuff.DATUM.
               RUN REGVEC.P. 
               RUN REGDAG.P.           
               regdatum = TIDREGITAB.DATUM.
               ASSIGN 
               tidbuff.VECKONUMMER = regvnr 
               tidbuff.DAG = regdagnamn.
            END.
            ASSIGN 
            TIDREGITAB.START = regslut.
            TIDREGITAB.SLUT = 24.00.
            tidtabrec = RECID(tidbuff).       
            FIND TIDREGITAB WHERE RECID(TIDREGITAB) = tidtabrec NO-ERROR.
         END.

      END.            
      
      ELSE IF TIDREGITAB.START < regstart AND TIDREGITAB.SLUT > regstart THEN DO:      
         IF TIDREGITAB.OVERTIDUTTAG = "F" THEN musz = musz.
         ELSE IF Guru.Konstanter:globforetag = "GKAL" AND TIDREGITAB.OVERTIDUTTAG = "I" THEN musz = musz.
         ELSE DO:      
            bustart3 = TIDREGITAB.START.      
            tidtabrec1 = RECID(TIDREGITAB).
            CREATE tidbuff.
            CREATE tidallt.
            tidallt.RECTIDVIS = RECID(tidbuff).
            ASSIGN 
            tidbuff.PERSONALKOD = TIDREGITAB.PERSONALKOD
            tidbuff.AONR = TIDREGITAB.AONR
            tidbuff.DELNR = TIDREGITAB.DELNR
            SUBSTRING(tidbuff.PROGRAM,1,158) = "TIDUPPDE" + STRING(TODAY) + STRING(TIME,"HH:MM") + Guru.Konstanter:globanv
            tidbuff.OVERTIDTILL = TIDREGITAB.OVERTIDTILL
            tidbuff.PRISTYP     = TIDREGITAB.PRISTYP
            tidbuff.PRIS        = TIDREGITAB.PRIS
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
            RUN prisover_UI (INPUT 1).                        
            RUN OTOLKPR.P.            
            regdatum = regdatumspar.
            FIND TIDREGITAB WHERE RECID(TIDREGITAB) = tidtabrec NO-LOCK NO-ERROR.      
            
            IF TIDREGITAB.SLUT LE hjslut AND TIDREGITAB.SLUT > hjstart THEN DO: 
               FIND FIRST OVERAVTAB WHERE OVERAVTAB.DATUM = regdatum AND 
               OVERAVTAB.KOD = ANSTFORMTAB.KOD USE-INDEX ODATUM NO-LOCK NO-ERROR.
               IF AVAILABLE OVERAVTAB AND (OVERAVTAB.DAGEQ = "HAL" OR OVERAVTAB.DAGEQ = "VAL") THEN DO:
                  IF TIDREGITAB.SLUT LE regslut AND TIDREGITAB.SLUT > hjstart THEN musz = TRUE.
               END.
               ELSE musz = TRUE. 
            END.
            FIND TIDREGITAB WHERE RECID(TIDREGITAB) = tidtabrec EXCLUSIVE-LOCK NO-ERROR.            
         END.
      END.
      IF TIDREGITAB.START < regslut AND TIDREGITAB.SLUT > regslut THEN DO:            
         IF TIDREGITAB.OVERTIDUTTAG = "F" THEN musz = musz.
         ELSE IF Guru.Konstanter:globforetag = "GKAL" AND TIDREGITAB.OVERTIDUTTAG = "I" THEN musz = musz.
         ELSE DO:      
            bustart3 = regslut.                
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
            tidbuff.START = regslut
            tidbuff.SLUT = TIDREGITAB.SLUT
            tidbuff.VECKONUMMER = TIDREGITAB.VECKONUMMER 
            tidbuff.DAG = TIDREGITAB.DAG
            tidbuff.RESMAL = TIDREGITAB.RESMAL.  
            tidtabrec = RECID(tidbuff).       
            ASSIGN TIDREGITAB.SLUT = regslut.      
            IF TIDREGITAB.OANT1 > 0 THEN DO:               
               ASSIGN TIDREGITAB.OANT1 = 0 TIDREGITAB.OKOD1 = ""
               TIDREGITAB.OANT2 = 0 TIDREGITAB.OKOD2 = ""
               TIDREGITAB.OANT3 = 0 TIDREGITAB.OKOD3 = "".
            END.                
            FIND TIDREGITAB WHERE RECID(TIDREGITAB) = tidtabrec NO-ERROR.
         END.
      END.   
      
      ELSE IF TIDREGITAB.START < regslut AND TIDREGITAB.SLUT LE regslut AND
      TIDREGITAB.START > regstart THEN DO:       
          musz = TRUE.
      END.   
      
   END.   
   IF musz = TRUE THEN DO:                 
      musz = FALSE.
      DO TRANSACTION:
         FIND TIDREGITAB WHERE RECID(TIDREGITAB) = tidtabrec EXCLUSIVE-LOCK NO-ERROR.
         RUN prisover_UI (INPUT 2).
      END.
      FIND TIDREGITAB WHERE RECID(TIDREGITAB) = tidtabrec NO-LOCK NO-ERROR.
      /*skift över dygnsbryt*/ 
      IF (regstart = 0 OR regslut = 24) AND  bdatum NE ? AND avdatum NE ? THEN musz = musz.
      ELSE DO:
         ASSIGN
         bdatum = TIDREGITAB.DATUM
         avdatum = TIDREGITAB.DATUM
         regdatum = TIDREGITAB.DATUM.
      END.
   END.    
   ELSE DO:            
       DO TRANSACTION:
         FIND TIDREGITAB WHERE RECID(TIDREGITAB) = tidtabrec EXCLUSIVE-LOCK NO-ERROR.
         RUN prisover_UI (INPUT 3).
      END.
      FIND TIDREGITAB WHERE RECID(TIDREGITAB) = tidtabrec NO-LOCK NO-ERROR.
      regdatumspar = regdatum.           
      RUN OTOLKPR.P.
      
      regdatum = regdatumspar.
      IF upprec NE ? THEN DO:
         
         tidtabrec = upprec.
         FIND TIDREGITAB WHERE RECID(TIDREGITAB) = tidtabrec NO-LOCK NO-ERROR.
         IF AVAILABLE TIDREGITAB THEN DO:
            DO TRANSACTION:
               FIND TIDREGITAB WHERE RECID(TIDREGITAB) = tidtabrec EXCLUSIVE-LOCK NO-ERROR.
               RUN prisover_UI (INPUT 3).
            END.
            FIND TIDREGITAB WHERE RECID(TIDREGITAB) = tidtabrec NO-LOCK NO-ERROR.
            bustart3 = TIDREGITAB.START.      
            regdatumspar = regdatum.                        
            RUN OTOLKPR.P.            
            regdatum = regdatumspar.
            upprec = ?.
         END.
      END.     
   END.    
   DO TRANSACTION:  /*ev recid */            
      FIND FIRST FLEXAVT WHERE FLEXAVT.PERSONALKOD = PERSONALTAB.PERSONALKOD USE-INDEX
      PERSONALKOD NO-LOCK NO-ERROR.
      FIND TIDREGITAB WHERE RECID(TIDREGITAB) = tidtabrec NO-LOCK NO-ERROR.            
      OPEN QUERY traktq
      FOR EACH tidbuff WHERE tidbuff.PERSONALKOD = TIDREGITAB.PERSONALKOD AND
      tidbuff.DATUM = bdatum AND tidbuff.TIDLOG = TRUE 
      USE-INDEX PSTART NO-LOCK.   
      GET FIRST traktq EXCLUSIVE-LOCK.         
      DO WHILE AVAILABLE(tidbuff):                     
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
         regdatum = tidbuff.DATUM
         regvnr = tidbuff.VECKONUMMER.
         RUN TOTTID.P.
         ASSIGN tidbuff.TOTALT = nytid.                    
         IF Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:
            tidbuff.LAGANTAL = lunchreg.
         END.   
         IF NOT AVAILABLE ORDARB THEN DO:
            CREATE FELTEXT.
            ASSIGN 
            FELTEXT.ANVANDARE = Guru.Konstanter:globanv                      
            FELTEXT.FELTEXT = "Kontakta Elpool! För nu är det något fel! Ange läge 1."
            FELTEXT.PROGRAM = "TIDUPPDE" + STRING(TODAY) + STRING(TIME,"HH:MM") + Guru.Konstanter:globanv. 
         END.         
         IF tidbuff.AONR = "171" THEN.
         /*veckovila vid halvdag ska ge 3 timmar - inte arbetad tid*/
         ELSE DO:
            IF tidbuff.LONTILLAGG = "" THEN.
            ELSE DO:         
               IF tidbuff.LONTILLAGG = ORDARB.MERKOD OR tidbuff.LONTILLAGG = ORDARB.MERKOD2 AND 
               tidbuff.LONAUTO = TRUE THEN DO:          
                  ASSIGN tidbuff.LONTILLANTAL = nytid.                   
               END.
            END.   
         END.            
   
         GET NEXT traktq EXCLUSIVE-LOCK.        
      END.                  
      CLOSE QUERY traktq.    
      regdatumspar = regdatum.
      IF avdatum NE bdatum THEN DO:      
         OPEN QUERY traktq
         FOR EACH tidbuff WHERE tidbuff.PERSONALKOD = TIDREGITAB.PERSONALKOD AND
         tidbuff.DATUM = avdatum AND tidbuff.TIDLOG = TRUE 
         USE-INDEX PSTART NO-LOCK.      
         GET FIRST traktq EXCLUSIVE-LOCK.   
         DO WHILE AVAILABLE(tidbuff):
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
            regdatum = tidbuff.DATUM
            regvnr = tidbuff.VECKONUMMER.
            RUN TOTTID.P.
            ASSIGN tidbuff.TOTALT = nytid.                        
            IF NOT AVAILABLE ORDARB THEN DO:
               CREATE FELTEXT.
               ASSIGN 
               FELTEXT.ANVANDARE = Guru.Konstanter:globanv                      
               FELTEXT.FELTEXT = "Kontakta Elpool! För nu är det något fel! Ange läge 3."
               FELTEXT.PROGRAM = "TIDUPPDE" + STRING(TODAY) + STRING(TIME,"HH:MM") + Guru.Konstanter:globanv. 
            END.          
            IF tidbuff.AONR = "171" THEN.
            /*veckovila vid halvdag ska ge 3 timmar - inte arbetad tid*/
            ELSE DO:         
               IF tidbuff.LONTILLAGG = ORDARB.MERKOD OR tidbuff.LONTILLAGG = ORDARB.MERKOD2 AND 
               tidbuff.LONAUTO = TRUE THEN DO:          
                  ASSIGN tidbuff.LONTILLANTAL = nytid.                       
               END.      
            END.
            GET NEXT traktq EXCLUSIVE-LOCK.  
         END. 
      END.
      CLOSE QUERY traktq.    
      regdatum = regdatumspar.
      IF bdatum = regdatum THEN musz = musz. 
      ELSE IF avdatum = regdatum THEN musz = musz.
      ELSE DO : 
         OPEN QUERY traktq
         FOR EACH tidbuff WHERE tidbuff.PERSONALKOD = TIDREGITAB.PERSONALKOD AND
         tidbuff.DATUM = regdatum AND tidbuff.TIDLOG = TRUE 
         USE-INDEX PSTART NO-LOCK.                 
         DO WHILE AVAILABLE(tidbuff):
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
            regdatum = tidbuff.DATUM
            regvnr = tidbuff.VECKONUMMER.
            RUN TOTTID.P.
            ASSIGN tidbuff.TOTALT = nytid.                        
            IF NOT AVAILABLE ORDARB THEN DO:
               CREATE FELTEXT.
               ASSIGN 
               FELTEXT.ANVANDARE = Guru.Konstanter:globanv                      
               FELTEXT.FELTEXT = "Kontakta Elpool! För nu är det något fel! Ange läge 4."
               FELTEXT.PROGRAM = "TIDUPPDE" + STRING(TODAY) + STRING(TIME,"HH:MM") + Guru.Konstanter:globanv. 
            END.          
            IF tidbuff.AONR = "171" THEN.
            /*veckovila vid halvdag ska ge 3 timmar - inte arbetad tid*/
            ELSE DO:         
               IF tidbuff.LONTILLAGG = ORDARB.MERKOD OR tidbuff.LONTILLAGG = ORDARB.MERKOD2 AND 
               tidbuff.LONAUTO = TRUE THEN DO:          
                  ASSIGN tidbuff.LONTILLANTAL = nytid.                      
               END.                            
            END.
            GET NEXT traktq EXCLUSIVE-LOCK.   
         END. 
         CLOSE QUERY traktq.
      END.                     
   END.          

   ASSIGN         
   musz = FALSE
   regdatumspar = regdatum.
   
   IF (Guru.Konstanter:globforetag = "GRAN"  OR Guru.Konstanter:globforetag = "ELPA") THEN DO:        
      IF PERSONALTAB.LAGBAS = TRUE THEN RUN LAGBAS.P.                                 
   END.   
   RUN TRAKTBER.P.      
   regdatum = regdatumspar.      
   IF Guru.Konstanter:varforetypval[30] = 1 THEN DO:
      /*ob-sjuk*/
      IF arbnr = "110" THEN DO: 
         RUN OBBERSJ.P.         
      END.
      ELSE RUN OBBER.P.
   END.
   ELSE RUN OBBER.P. 
   
   regdatum = regdatumspar.   
   /*PROBLEM KALMAR vid registrering 0-7 om 23-24 föregående dag redan finns . Då är regdatum föregående dag.
   Borde alltid vara samma dag som TIDREGITAB */
      
   FIND FIRST TIDREGITAB WHERE RECID(TIDREGITAB) = tidtabrec NO-LOCK NO-ERROR.   
   regdatum = TIDREGITAB.DATUM.   
   RUN REGVEC.P.
   RUN SLUTARB.P. 
   
   IF bustart3 GE regslut OR  bustart3 < regstart OR regdatum > regdatumspar THEN DO TRANSACTION:
      nytid = regstart.
      RUN TIMSEK.P.
      hjrstartsek = sekunder.
      nytid = regslut.
      RUN TIMSEK.P.
      hjrslutsek = sekunder.
      FIND TIDREGITAB WHERE RECID(TIDREGITAB) = tidtabrec EXCLUSIVE-LOCK NO-ERROR.   
      nytid = bustart3.
      RUN TIMSEK.P.  
       musz = FALSE.
      FIND FIRST OVERAVTAB WHERE OVERAVTAB.DATUM = regdatum AND 
      OVERAVTAB.KOD = ANSTFORMTAB.KOD USE-INDEX ODATUM NO-LOCK NO-ERROR.
      IF AVAILABLE OVERAVTAB AND (OVERAVTAB.DAGEQ = "HAL" OR OVERAVTAB.DAGEQ = "VAL" OR OVERAVTAB.DAGEQ = "HAV" OR
      OVERAVTAB.EQDAG = 1 OR OVERAVTAB.EQDAG = 7) THEN musz = TRUE.      
      IF bustart3 GE hjslut OR bustart3 < hjstart OR
      WEEKDAY(regdatum) = 1 OR WEEKDAY(regdatum) = 7 THEN musz = TRUE.
      /*test för registrering sö-må 15-07 lena 20121022*/
      IF regdatum ne regdatumspar THEN DO:
         IF WEEKDAY(regdatumspar) = 1 OR WEEKDAY(regdatumspar) = 7 THEN musz = TRUE.         
      END.   
      /*omgjort 2006-05-30 lena så att ej nya arbetstider måste läggas in*/      
      IF sekunder GE hjrslutsek OR sekunder < hjrstartsek THEN musz = TRUE.
      IF regstart = 7 AND regslut = 7 THEN musz = TRUE.
      IF regstart = 0 AND regslut = 0 THEN musz = TRUE.                        
      IF musz = TRUE THEN musz = FALSE.
      ELSE DO:        
         IF TIDREGITAB.OANT1 NE 0 OR TIDREGITAB.OANT2 NE 0 OR
         TIDREGITAB.OANT3 NE 0  THEN DO:         
            ASSIGN TIDREGITAB.OANT1 = 0 TIDREGITAB.OKOD1 = ""
            TIDREGITAB.OANT2 = 0 TIDREGITAB.OKOD2 = ""
            TIDREGITAB.OANT3 = 0 TIDREGITAB.OKOD3 = "".
         END.
      END.
   END.
   ELSE  IF regstart = 0 AND regslut = 24 THEN DO TRANSACTION:
      /*skift 0-24 lunch 6-22 skall gå att registrera övertid*/
      FIND TIDREGITAB WHERE RECID(TIDREGITAB) = tidtabrec EXCLUSIVE-LOCK NO-ERROR.   
      IF bustart3 GE lunchstarten AND TIDREGITAB.SLUT LE lunchslutet  THEN bustart3 = bustart3.
      ELSE DO:        
         IF TIDREGITAB.OANT1 NE 0 OR TIDREGITAB.OANT2 NE 0 OR
         TIDREGITAB.OANT3 NE 0  THEN DO:         
            ASSIGN TIDREGITAB.OANT1 = 0 TIDREGITAB.OKOD1 = ""
            TIDREGITAB.OANT2 = 0 TIDREGITAB.OKOD2 = ""
            TIDREGITAB.OANT3 = 0 TIDREGITAB.OKOD3 = "".
         END.
      END.   
   END.
   ELSE DO TRANSACTION:
      FIND TIDREGITAB WHERE RECID(TIDREGITAB) = tidtabrec EXCLUSIVE-LOCK NO-ERROR.   
      IF TIDREGITAB.OANT1 NE 0 OR TIDREGITAB.OANT2 NE 0 OR
      TIDREGITAB.OANT3 NE 0 THEN DO:
         ASSIGN TIDREGITAB.OANT1 = 0 TIDREGITAB.OKOD1 = ""
         TIDREGITAB.OANT2 = 0 TIDREGITAB.OKOD2 = ""
         TIDREGITAB.OANT3 = 0 TIDREGITAB.OKOD3 = "".
      END. 
   END. 
   
END PROCEDURE.
PROCEDURE prisover_UI :
   DEFINE INPUT PARAMETER vad AS INTEGER NO-UNDO.
   DEFINE BUFFER pbef FOR PERSONALPRIS.
   IF Guru.Konstanter:varforetypval[4] = 1 THEN vad = vad.                           
   ELSE RETURN.
   IF TIDREGITAB.OVERTIDTILL BEGINS "FELAVHJ" THEN vad = vad.
   ELSE RETURN.
   FIND LAST PERSONALPRIS WHERE 
   PERSONALPRIS.PERSONALKOD = TIDREGITAB.PERSONALKOD AND
   PERSONALPRIS.BEFATTNING  = "FELAVHJÄLP. DAGTID" AND 
   PERSONALPRIS.STARTDATUM <= TIDREGITAB.DATUM  AND 
   PERSONALPRIS.SLUTDATUM >= TIDREGITAB.DATUM 
   NO-LOCK NO-ERROR.
   IF NOT AVAILABLE PERSONALPRIS THEN DO:
      FIND LAST PERSONALPRIS WHERE 
      PERSONALPRIS.PERSONALKOD = TIDREGITAB.PERSONALKOD AND
      PERSONALPRIS.BEFATTNING  = "FELAVHJÄLP. DAGTID" 
      USE-INDEX PSTART NO-LOCK NO-ERROR.
   END.
   FIND LAST pbef WHERE 
   pbef.PERSONALKOD = TIDREGITAB.PERSONALKOD AND
   pbef.BEFATTNING  = "FELAVHJÄLP.ÖVRIG TID" AND 
   pbef.STARTDATUM <= TIDREGITAB.DATUM  AND 
   pbef.SLUTDATUM >= TIDREGITAB.DATUM 
   NO-LOCK NO-ERROR.
   IF NOT AVAILABLE pbef THEN DO:
      FIND LAST pbef WHERE 
      pbef.PERSONALKOD = TIDREGITAB.PERSONALKOD AND
      pbef.BEFATTNING  = "FELAVHJÄLP.ÖVRIG TID" 
      USE-INDEX PSTART NO-LOCK NO-ERROR.
   END.
   IF vad = 1 THEN DO:
      ASSIGN
      TIDREGITAB.OVERTIDTILL = pbef.BEFATTNING      
      TIDREGITAB.PRIS        = pbef.PRIS
      tidbuff.OVERTIDTILL    = PERSONALPRIS.BEFATTNING      
      tidbuff.PRIS           = PERSONALPRIS.PRIS.            
   END.
   IF vad = 2 THEN DO:
      ASSIGN
      TIDREGITAB.OVERTIDTILL = PERSONALPRIS.BEFATTNING      
      TIDREGITAB.PRIS        = PERSONALPRIS.PRIS.    
   END.
   IF vad = 3 THEN DO:
      ASSIGN
      TIDREGITAB.OVERTIDTILL = pbef.BEFATTNING      
      TIDREGITAB.PRIS        = pbef.PRIS.    
   END.
END PROCEDURE.
      
      
      
      
      
      
      
      
      
                                                
   
   
   
