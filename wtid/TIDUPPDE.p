/*TIDUPPDE.P*/
&Scoped-define NEW NEW
{TIDALLT.I}
{APP.I}
DEFINE VARIABLE tidtabrec1 AS RECID NO-UNDO. 
DEFINE VARIABLE hjstart LIKE TIDREGITAB.START NO-UNDO.  
DEFINE VARIABLE hjslut LIKE TIDREGITAB.SLUT NO-UNDO.   
DEFINE VARIABLE regdatumspar AS DATE NO-UNDO.             
DEFINE VARIABLE upprec AS RECID NO-UNDO.
DEFINE VARIABLE hjrstartsek AS INTEGER NO-UNDO.
DEFINE VARIABLE hjrslutsek AS INTEGER NO-UNDO.
DEFINE VARIABLE ehjstart LIKE TIDREGITAB.START NO-UNDO.  
DEFINE VARIABLE arbnr AS CHARACTER NO-UNDO.
DEFINE BUFFER tidbuff FOR TIDREGITAB.        
DEFINE QUERY traktq FOR tidbuff.
DEFINE NEW SHARED VARIABLE extraaonr LIKE TIDREGITAB.AONR NO-UNDO.
DEFINE NEW SHARED VARIABLE extradnr LIKE TIDREGITAB.DELNR NO-UNDO.

{TIDUPPUT.I}
FIND FIRST tidapptemp NO-LOCK NO-ERROR.
ASSIGN
Guru.Konstanter:globforetag = tidapptemp.FORETAG  

persrec = tidapptemp.RECPERS 
tidtabrec = tidapptemp.RECTID
regdatum = tidapptemp.DATUM. 
RUN REGVEC.P. 
RUN REGDAG.P.           
FIND PERSONALTAB WHERE RECID(PERSONALTAB) = persrec NO-LOCK NO-ERROR.
FIND TIDREGITAB WHERE RECID(TIDREGITAB) = tidtabrec NO-LOCK NO-ERROR.
FIND FIRST AONRTAB WHERE AONRTAB.AONR = TIDREGITAB.AONR AND AONRTAB.DELNR = 
TIDREGITAB.DELNR USE-INDEX AONR NO-LOCK NO-ERROR.
IF AVAILABLE AONRTAB THEN DO:
   arbnr = TIDREGITAB.AONR.
   IF AONRTAB.PRISTYP = "FRÅNVARO." AND TIDREGITAB.PRISTYP NE "FRÅNVARO." THEN DO TRANSACTION:
      FIND CURRENT TIDREGITAB EXCLUSIVE-LOCK NO-ERROR.
      ASSIGN TIDREGITAB.PRISTYP = "FRÅNVARO." TIDREGITAB.PRIS = 0.
   END.   
END.   
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
bustart3 = TIDREGITAB.START.
RUN SLUTARB.P.
IF PERSONALTAB.DELTID = FALSE AND regstart = regslut THEN DO TRANSACTION:       
   FIND TIDREGITAB WHERE RECID(TIDREGITAB) = tidtabrec EXCLUSIVE-LOCK NO-ERROR.
   IF TIDREGITAB.START > TIDREGITAB.SLUT THEN DO: 
      bustart3 = TIDREGITAB.START.
      CREATE tidbuff. 
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
   IF regstart = hjstart AND regslut = hjslut THEN DO: 
      /*en deltidare som jobbar full dag vissa dagar ska ha övertid direkt*/      
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
regstart NE regslut AND TIDREGITAB.START < TIDREGITAB.SLUT THEN musz = TRUE.
ELSE DO TRANSACTION:          
   FIND TIDREGITAB WHERE RECID(TIDREGITAB) = tidtabrec EXCLUSIVE-LOCK NO-ERROR.         
   IF TIDREGITAB.START < regstart AND TIDREGITAB.SLUT > regstart THEN DO:      
      IF TIDREGITAB.OVERTIDUTTAG = "F" THEN musz = musz.
      ELSE DO:      
         bustart3 = TIDREGITAB.START.      
         tidtabrec1 = RECID(TIDREGITAB).
         CREATE tidbuff.
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
   IF TIDREGITAB.START < regslut AND TIDREGITAB.SLUT > regslut THEN DO:      
      IF TIDREGITAB.OVERTIDUTTAG = "F" THEN musz = musz.
      ELSE DO:      
         bustart3 = regslut.                
         CREATE tidbuff.
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
   ELSE IF TIDREGITAB.START < regslut AND TIDREGITAB.START > regstart AND
   TIDREGITAB.SLUT < regstart THEN DO:      
      IF TIDREGITAB.OVERTIDUTTAG = "F" THEN musz = musz.
      ELSE DO:      
         bustart3 = regslut.               
         CREATE tidbuff.
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
         FIND TIDREGITAB WHERE RECID(TIDREGITAB) = tidtabrec NO-ERROR.
      END.
   END.   
   ELSE IF TIDREGITAB.START < regslut AND TIDREGITAB.SLUT LE regslut AND
   TIDREGITAB.START > regstart THEN DO:       
       musz = TRUE.
   END.   
   IF TIDREGITAB.START > TIDREGITAB.SLUT AND TIDREGITAB.SLUT LE regstart AND
   TIDREGITAB.START GE regslut THEN DO:      
      bustart3 = TIDREGITAB.START.                
      CREATE tidbuff.
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
   ELSE IF TIDREGITAB.START > TIDREGITAB.SLUT AND TIDREGITAB.SLUT LE regstart AND
   TIDREGITAB.START < regslut THEN DO:      
      bustart3 = regslut.                
      IF TIDREGITAB.START < regstart THEN DO:         
         CREATE tidbuff.
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
   ELSE IF TIDREGITAB.START > TIDREGITAB.SLUT AND TIDREGITAB.SLUT > regstart AND
   TIDREGITAB.START GE regslut AND TIDREGITAB.DAG NE "FRE" THEN DO:      
      bustart3 = TIDREGITAB.START.                
      CREATE tidbuff.
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
      tidbuff.START = regstart
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
   ELSE IF TIDREGITAB.START > TIDREGITAB.SLUT AND TIDREGITAB.SLUT > regstart AND
   TIDREGITAB.START GE regslut AND TIDREGITAB.DAG = "FRE" THEN DO:      
      bustart3 = TIDREGITAB.START.                
      CREATE tidbuff.
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
      ASSIGN TIDREGITAB.SLUT = 24.00.
      tidtabrec = RECID(tidbuff).       
      FIND TIDREGITAB WHERE RECID(TIDREGITAB) = tidtabrec NO-ERROR.
   END. 
   ELSE IF TIDREGITAB.START > TIDREGITAB.SLUT AND TIDREGITAB.SLUT > regstart AND
   TIDREGITAB.START < regslut THEN DO:      
      bustart3 = regslut.                
      CREATE tidbuff.
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
      CREATE tidbuff.
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
      tidbuff.START = regstart
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
END.   
IF musz = TRUE THEN  DO:
   musz = FALSE.
   FIND TIDREGITAB WHERE RECID(TIDREGITAB) = tidtabrec NO-LOCK NO-ERROR.
   ASSIGN
   bdatum = TIDREGITAB.DATUM
   avdatum = TIDREGITAB.DATUM
   regdatum = TIDREGITAB.DATUM.
END.    
ELSE DO: 
   regdatumspar = regdatum.     
   RUN OTOLKPR.P.
   regdatum = regdatumspar.
   IF upprec NE ? THEN DO:
      tidtabrec = upprec.
      FIND TIDREGITAB WHERE RECID(TIDREGITAB) = tidtabrec NO-LOCK NO-ERROR.
      IF AVAILABLE TIDREGITAB THEN DO:
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
         IF ORDARB.MERKOD NE "" THEN DO:
            IF tidbuff.LONTILLAGG = ORDARB.MERKOD  AND tidbuff.LONAUTO = TRUE THEN DO:          
               ASSIGN tidbuff.LONTILLANTAL = nytid.                       
            END.
         END.   
         IF ORDARB.MERKOD2 NE "" THEN DO:
            IF tidbuff.LONTILLAGG = ORDARB.MERKOD2 AND tidbuff.LONAUTO = TRUE THEN DO:          
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
            IF ORDARB.MERKOD NE "" THEN DO:
               IF tidbuff.LONTILLAGG = ORDARB.MERKOD  AND tidbuff.LONAUTO = TRUE THEN DO:          
                  ASSIGN tidbuff.LONTILLANTAL = nytid.                       
               END.
            END.   
            IF ORDARB.MERKOD2 NE "" THEN DO:
               IF tidbuff.LONTILLAGG = ORDARB.MERKOD2 AND tidbuff.LONAUTO = TRUE THEN DO:          
                  ASSIGN tidbuff.LONTILLANTAL = nytid.                       
               END.
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
            IF ORDARB.MERKOD NE "" THEN DO:
               IF tidbuff.LONTILLAGG = ORDARB.MERKOD  AND tidbuff.LONAUTO = TRUE THEN DO:          
                  ASSIGN tidbuff.LONTILLANTAL = nytid.                       
               END.
            END.   
            IF ORDARB.MERKOD2 NE "" THEN DO:
               IF tidbuff.LONTILLAGG = ORDARB.MERKOD2 AND tidbuff.LONAUTO = TRUE THEN DO:          
                  ASSIGN tidbuff.LONTILLANTAL = nytid.                       
               END.
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
   IF arbnr = "110" THEN RUN OBBERSJ.P.
   ELSE RUN OBBER.P.
END.
ELSE RUN OBBER.P.             
regdatum = regdatumspar.
/*PROBLEM KALMAR vid registrering 0-7 om 23-24 föregående dag redan finns . Då är regdatum föregående dag.
Borde alltid vara samma dag som TIDREGITAB */
FIND TIDREGITAB WHERE RECID(TIDREGITAB) = tidtabrec NO-LOCK NO-ERROR.   
regdatum = TIDREGITAB.DATUM.
RUN SLUTARB.P.
IF bustart3 GE regslut OR  bustart3 < regstart THEN DO TRANSACTION:
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
   /*omgjort 2006-05-30 lena så att ej nya arbetstider måste läggas in*/
   IF sekunder GE hjrslutsek OR sekunder < hjrstartsek THEN musz = TRUE.
   IF regstart = 7 AND regslut = 7 THEN musz = TRUE.
   IF regstart = 0 AND regslut = 0 THEN musz = TRUE.
   IF Guru.Konstanter:globforetag = "GRAN"  THEN DO:   
      IF regstart = 14.30 AND regslut = 22.30 AND ( sekunder GE 81000 OR
      sekunder < 52200) THEN musz = TRUE. 
      IF regstart = 14 AND regslut = 22 AND ( sekunder GE 79200 OR
      sekunder < 50400) THEN musz = TRUE.
   END.
   IF Guru.Konstanter:globforetag = "GKRVA" THEN DO:   
      IF regstart = 14 AND regslut = 22 AND ( sekunder GE 79200 OR
      sekunder < 50400) THEN musz = TRUE.
      IF regstart = 6 AND regslut = 14 AND ( sekunder GE 50400 OR
      sekunder < 21600) THEN musz = TRUE. 
      IF regstart = 7 AND regslut = 15 AND ( sekunder GE 54000 OR
      sekunder < 25200) THEN musz = TRUE.
      IF regstart = 7 AND regslut = 7 THEN musz = TRUE.
   END.  
   IF Guru.Konstanter:globforetag = "LULE" THEN DO:   
      /*leab*/
      IF regstart = 14.30 AND regslut = 23 AND ( sekunder GE 82800 OR
      sekunder < 52200) THEN musz = TRUE.
      IF regstart = 6 AND regslut = 14.3 AND ( sekunder GE 52200 OR
      sekunder < 21600) THEN musz = TRUE.       
      IF regstart = 7.3 AND regslut = 9.3 AND ( sekunder GE 34200 OR
      sekunder < 27000) THEN musz = TRUE.       
      IF regstart = 7.3 AND regslut = 12.3 AND ( sekunder GE 45000 OR
      sekunder < 27000) THEN musz = TRUE.       
      IF regstart = 7.3 AND regslut = 14.25 AND ( sekunder GE 51900 OR
      sekunder < 27000) THEN musz = TRUE.       
      IF regstart = 7.3 AND regslut = 13.55 AND ( sekunder GE 50100 OR
      sekunder < 27000) THEN musz = TRUE.      
      /*bio*/
      IF regstart = 14 AND regslut = 22 AND ( sekunder GE 79200 OR
      sekunder < 50400) THEN musz = TRUE.
      IF regstart = 6 AND regslut = 14 AND ( sekunder GE 50400 OR
      sekunder < 21600) THEN musz = TRUE.
      IF regstart = 6 AND regslut = 18 AND ( sekunder GE 64800 OR
      sekunder < 21600) THEN musz = TRUE.
      IF regstart = 0 AND regslut = 6 AND sekunder GE 21600 THEN musz = TRUE.
      IF regstart = 22 AND regslut = 24 AND sekunder < 79200 THEN musz = TRUE. 
      IF regstart = 18 AND regslut = 24 AND sekunder < 64800 THEN musz = TRUE. 

      IF regstart = 7 AND regslut = 7 THEN musz = TRUE.
      IF regstart = 0 AND regslut = 0 THEN musz = TRUE.
   END.  
   IF Guru.Konstanter:globforetag = "GKAL" THEN DO:   
      /*DRAKEN*/
      IF regstart = 15 AND regslut = 23 AND ( sekunder GE 82800 OR
      sekunder < 54000) THEN musz = TRUE. 
      IF regstart = 7 AND regslut = 15 AND ( sekunder GE 54000 OR
      sekunder < 25200) THEN musz = TRUE.
      IF regstart = 23 AND regslut = 24 AND sekunder < 82800 THEN musz = TRUE. 
      IF regstart = 7 AND regslut = 11 AND ( sekunder GE 39600 OR
      sekunder < 25200) THEN musz = TRUE.
      IF regstart = 0 AND regslut = 7 AND sekunder GE 25200 THEN musz = TRUE.
      /*NYBRO*/
      IF regstart = 6 AND regslut = 14 AND ( sekunder GE 50400 OR
      sekunder < 21600) THEN musz = TRUE.
      IF regstart = 14 AND regslut = 22 AND ( sekunder GE 79200 OR
      sekunder < 50400) THEN musz = TRUE.
      IF regstart = 0 AND regslut = 6 AND sekunder GE 21600 THEN musz = TRUE.
      IF regstart = 22 AND regslut = 24 AND sekunder < 79200 THEN musz = TRUE. 
      IF regstart = 18 AND regslut = 24 AND sekunder < 64800 THEN musz = TRUE. 
      IF regstart = 6 AND regslut = 18 AND ( sekunder GE 64800 OR
      sekunder < 21600) THEN musz = TRUE.


      IF regstart = 7 AND regslut = 7 THEN musz = TRUE.

   END.   
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" THEN DO:   
      /*KORSTA*/         
      IF regstart = 15 AND regslut = 22 AND ( sekunder GE 79200 OR
      sekunder < 54000) THEN musz = TRUE. 
      IF regstart = 7 AND regslut = 15 AND ( sekunder GE 54000 OR
      sekunder < 25200) THEN musz = TRUE.
      IF regstart = 22 AND regslut = 24 AND sekunder < 79200 THEN musz = TRUE. 
      IF regstart = 0 AND regslut = 7 AND sekunder GE 25200 THEN musz = TRUE.
      IF regstart = 7 AND regslut = 7 THEN musz = TRUE.
   END.               
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
ELSE DO TRANSACTION:
   FIND TIDREGITAB WHERE RECID(TIDREGITAB) = tidtabrec EXCLUSIVE-LOCK NO-ERROR.   
   IF TIDREGITAB.OANT1 NE 0 OR TIDREGITAB.OANT2 NE 0 OR
   TIDREGITAB.OANT3 NE 0 THEN DO:
      ASSIGN TIDREGITAB.OANT1 = 0 TIDREGITAB.OKOD1 = ""
      TIDREGITAB.OANT2 = 0 TIDREGITAB.OKOD2 = ""
      TIDREGITAB.OANT3 = 0 TIDREGITAB.OKOD3 = "".
   END. 
END. 
RETURN.      
      
      
      
      
      
      
      
      
      
                                                
   
   
   
