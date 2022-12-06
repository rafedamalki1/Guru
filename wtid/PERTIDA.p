/*PERTIDA.P*/
{APP.I}
{TIDAPPDEF.I}
{SOKDEF.I}  
DEFINE INPUT PARAMETER ganv AS CHARACTER NO-UNDO. 
DEFINE INPUT PARAMETER pkod AS CHARACTER NO-UNDO. 
DEFINE INPUT PARAMETER FILL-IN-STARTDAT AS DATE NO-UNDO. 
DEFINE INPUT PARAMETER FILL-IN-STOPPDAT AS DATE NO-UNDO. 
DEFINE INPUT PARAMETER varaonr AS CHARACTER NO-UNDO.  
DEFINE INPUT PARAMETER vardelnr AS INTEGER NO-UNDO. 
DEFINE INPUT PARAMETER CMB_TRAK AS INTEGER NO-UNDO. 
DEFINE INPUT PARAMETER FILL-IN_RESMAL AS CHARACTER NO-UNDO.  
DEFINE INPUT PARAMETER CMB_BEF AS CHARACTER NO-UNDO. 
DEFINE OUTPUT PARAMETER TABLE FOR felmeddtemp.      
DEFINE VARIABLE regdatumspar AS DATE NO-UNDO.
DEFINE VARIABLE maxarbkort AS DECIMAL NO-UNDO.  
DEFINE VARIABLE foremaxarbkort AS DECIMAL NO-UNDO.   
DEFINE VARIABLE seku AS INTEGER NO-UNDO.
{AVAFOR.I}

FUNCTION klock100 RETURNS DECIMAL
  ( INPUT ber60 AS DECIMAL ):
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN  (TRUNCATE(ber60,0) * 3600 + (ber60 - TRUNCATE(ber60,0)) * 100 * 60) / 3600.

END FUNCTION.
FIND FIRST FORETAG NO-LOCK NO-ERROR.
Guru.Konstanter:globforetag = FORETAG.FORETAG.
{FORESTYR.I}

FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = pkod NO-LOCK NO-ERROR.
FIND FIRST AONRTAB WHERE AONRTAB.AONR = varaonr AND AONRTAB.DELNR = vardelnr
NO-LOCK NO-ERROR.
FIND FIRST TIMKOSTNADSTAB WHERE TIMKOSTNADSTAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
TIMKOSTNADSTAB.PRISTYP = AONRTAB.PRISTYP  USE-INDEX PRISPERS NO-LOCK NO-ERROR.               
ASSIGN
persrec = RECID(PERSONALTAB).
regdatum = FILL-IN-STARTDAT.   
/* arbetstidsförkortning*/
IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "GKAL" OR Guru.Konstanter:globforetag = "ELPA" OR Guru.Konstanter:globforetag = "LULE" THEN DO:      
   DEFINE BUFFER tidbuff10 FOR TIDREGITAB.
   DEFINE VARIABLE totarbkort AS DECIMAL NO-UNDO.
   DEFINE VARIABLE avarfor AS INTEGER NO-UNDO.
   totarbkort = 0.                       
   IF Guru.Konstanter:globforetag = "CELPA" OR Guru.Konstanter:globforetag = "GKAL" THEN DO:
      /*Kalmar kan ta ut arbetstidsförkortning i pengar vid tex sjukdom*/
      FOR EACH tidbuff10 WHERE tidbuff10.PERSONALKOD = PERSONALTAB.PERSONALKOD AND tidbuff10.LONTILLAGG = "ATL"
      AND YEAR(tidbuff10.DATUM) = YEAR(FILL-IN-STARTDAT) NO-LOCK:
         totarbkort = totarbkort + klock100(tidbuff10.LONTILLANTAL).
      END.             
   END.
   IF AONRTAB.AONR = "160" AND AONRTAB.DELNR = 0 THEN DO:                   
      FOR EACH tidbuff10 WHERE tidbuff10.PERSONALKOD = PERSONALTAB.PERSONALKOD AND tidbuff10.AONR = "160" AND tidbuff10.DELNR = 0
      AND YEAR(tidbuff10.DATUM) = YEAR(FILL-IN-STARTDAT) AND tidbuff10.TIDLOG = TRUE NO-LOCK:
         totarbkort = totarbkort + klock100(tidbuff10.TOTALT).
      END.               
      regdatum = FILL-IN-STARTDAT.
      RUN REGDAG.P.
      RUN REGVEC.P.
      RUN SLFLARB.P.
      totarbkort = totarbkort + (1 + FILL-IN-STOPPDAT - FILL-IN-STARTDAT ) * klock100(regtotalt).    
      avarfor = 0.
      IF Guru.Konstanter:globforetag = "gkal" OR Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "elpa" THEN DO:   
         RUN havafor (INPUT PERSONALTAB.PERSONALKOD, OUTPUT avarfor).                        
      END.               
      /*ATKGRÄNS*/
      IF Guru.Konstanter:globforetag = "gkal"  THEN foremaxarbkort = 63.
      ELSE IF Guru.Konstanter:globforetag = "lule" THEN foremaxarbkort = 63.
      ELSE IF  Guru.Konstanter:globforetag = "SNAT" THEN foremaxarbkort = 63.
      ELSE IF  Guru.Konstanter:globforetag = "SUND" THEN foremaxarbkort = 54.
      ELSE foremaxarbkort = 63.
      IF avarfor > 0 THEN maxarbkort = avarfor.
      ELSE maxarbkort = foremaxarbkort.      
      IF totarbkort > maxarbkort THEN DO:
         totarbkort = totarbkort - ((1 + FILL-IN-STOPPDAT - FILL-IN-STARTDAT ) * klock100(regtotalt)).
         CREATE felmeddtemp.  
         felmeddtemp.felmedd = "Max " + STRING(maxarbkort) + " timmar per år får skrivas på arbetstidförkortning,du har skrivit"  + STRING(totarbkort) + "timmar".
         RETURN.                  
      END.            
   END.  
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:      
      IF AONRTAB.AONR = "161" THEN DO:                 
         FOR EACH tidbuff10 WHERE tidbuff10.PERSONALKOD = PERSONALTAB.PERSONALKOD AND tidbuff10.AONR = "161"
         AND YEAR(tidbuff10.DATUM) = YEAR(FILL-IN-STARTDAT) AND tidbuff10.TIDLOG = TRUE NO-LOCK:
            totarbkort = totarbkort + klock100(tidbuff10.TOTALT).
         END.                     
         regdatum = FILL-IN-STARTDAT.
         RUN REGDAG.P.
         RUN REGVEC.P.
         RUN SLFLARB.P.                 
         totarbkort = totarbkort + (1 + FILL-IN-STOPPDAT - FILL-IN-STARTDAT ) * klock100(regtotalt).                  
         /*ATKGRÄNS*/
         maxarbkort = 27.
         IF totarbkort > maxarbkort THEN DO:
            totarbkort = totarbkort - ((1 + FILL-IN-STOPPDAT - FILL-IN-STARTDAT ) * klock100(regtotalt)).            
            CREATE felmeddtemp.  
            felmeddtemp.felmedd = "Max " + STRING(maxarbkort) + " timmar per år får skrivas på arbetstidförkortning, du har tidigare skrivit "  + STRING(totarbkort) + "timmar".
            RETURN.                  
         END.            
      END.
   END.
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:      
      IF AONRTAB.AONR = "135" THEN DO:                 
         FOR EACH tidbuff10 WHERE tidbuff10.PERSONALKOD = PERSONALTAB.PERSONALKOD AND tidbuff10.AONR = "135"
         AND YEAR(tidbuff10.DATUM) = YEAR(FILL-IN-STARTDAT) AND tidbuff10.TIDLOG = TRUE NO-LOCK:
            totarbkort = totarbkort + klock100(tidbuff10.TOTALT).
         END.         
         regdatum = FILL-IN-STARTDAT.
         RUN REGDAG.P.
         RUN REGVEC.P.
         RUN SLFLARB.P.
         totarbkort = totarbkort + (FILL-IN-STOPPDAT - FILL-IN-STARTDAT ) * klock100(regtotalt).               
      END.
   END.
END.             
ASSIGN
persrec = RECID(PERSONALTAB).
regdatum = FILL-IN-STARTDAT.   
REPEAT:
   RUN REGVEC.P.
   RUN SLUTARB.P.
   RUN REGDAG.P.   
   IF regslut NE regstart THEN DO:
      DO TRANSACTION:                           
         RUN TIDSTART.P.         
         IF musz = TRUE THEN DO:
            musz = FALSE.
            RETURN.
         END.
         IF musz = FALSE THEN DO:
            RUN TIDSLUT.P.
            IF musz = TRUE THEN DO:
               musz = FALSE.
               RETURN.
            END.
         END.
         IF regstart = 0 AND regslut = 24  THEN DO:
            musz = FALSE.
            /*SKIFT 0-24*/
            IF Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "GKAL" OR Guru.Konstanter:globforetag = "elpa" THEN DO:           
               IF regdatum = FILL-IN-STARTDAT THEN musz = TRUE.
            END.
            IF musz = TRUE THEN musz = FALSE.
            ELSE DO:           
               CREATE TIDREGITAB.                      
               tidtabrec = RECID(TIDREGITAB).
               ASSIGN                
               SUBSTRING(TIDREGITAB.PROGRAM,1,158) = "PERTID" + STRING(TODAY) + STRING(TIME,"HH:MM") + Guru.Konstanter:globanv
               TIDREGITAB.PERSONALKOD = pkod      
               TIDREGITAB.DAG = regdagnamn
               TIDREGITAB.VECKONUMMER = regvnr
               TIDREGITAB.START = regstart 
               TIDREGITAB.SLUT = lunchstarten            
               TIDREGITAB.TRAKTAMENTE = CMB_TRAK
               TIDREGITAB.OVERTIDUTTAG = PERSONALTAB.OVERTIDUTTAG 
               TIDREGITAB.UTRYCKNING = FALSE
               TIDREGITAB.NODF = FALSE
               TIDREGITAB.PRISTYP = AONRTAB.PRISTYP
               TIDREGITAB.PRIS = TIMKOSTNADSTAB.PRISA
               TIDREGITAB.DATUM = regdatum
               TIDREGITAB.AONR = AONRTAB.AONR 
               TIDREGITAB.DELNR = AONRTAB.DELNR
               TIDREGITAB.RESMAL = FILL-IN_RESMAL.                                    
               RUN pris_UI.
               IF TIDREGITAB.PRISTYP = "FRÅNVARO." THEN ASSIGN TIDREGITAB.TRAKTAMENTE = 0.
               nytid = TIDREGITAB.START.
               RUN TIMSEK.P.
               regstartsek = sekunder.
               nytid = TIDREGITAB.SLUT.
               RUN TIMSEK.P.          
               ASSIGN
               regdatumspar = regdatum
               regslutsek = sekunder
               regdatum = TIDREGITAB.DATUM.
               RUN TOTTID.P.
               ASSIGN TIDREGITAB.TOTALT = nytid.  /* obs används till FLEXDAG sedan*/         
               IF musz = TRUE THEN DO:
                  DELETE TIDREGITAB.
                  musz = FALSE.
               END.
               ELSE DO:                        
                  RELEASE TIDREGITAB NO-ERROR.            
                  RUN nytolk_UI.         
               END.
            END.                     
            CREATE TIDREGITAB.                      
            tidtabrec = RECID(TIDREGITAB).
            ASSIGN 
            SUBSTRING(TIDREGITAB.PROGRAM,1,158) = "PERTID" + STRING(TODAY) + STRING(TIME,"HH:MM") + Guru.Konstanter:globanv
            TIDREGITAB.PERSONALKOD = pkod      
            TIDREGITAB.DAG = regdagnamn
            TIDREGITAB.VECKONUMMER = regvnr
            TIDREGITAB.START = lunchslutet
            TIDREGITAB.SLUT = regslut            
            TIDREGITAB.TRAKTAMENTE = CMB_TRAK
            TIDREGITAB.OVERTIDUTTAG = PERSONALTAB.OVERTIDUTTAG 
            TIDREGITAB.UTRYCKNING = FALSE
            TIDREGITAB.NODF = FALSE
            TIDREGITAB.PRISTYP = AONRTAB.PRISTYP
            TIDREGITAB.PRIS = TIMKOSTNADSTAB.PRISA
            TIDREGITAB.DATUM = regdatum
            TIDREGITAB.AONR = AONRTAB.AONR 
            TIDREGITAB.DELNR = AONRTAB.DELNR
            TIDREGITAB.RESMAL = FILL-IN_RESMAL.                                    
            RUN pris_UI.
            IF TIDREGITAB.PRISTYP = "FRÅNVARO." THEN ASSIGN TIDREGITAB.TRAKTAMENTE = 0.
            nytid = TIDREGITAB.START.
            RUN TIMSEK.P.
            regstartsek = sekunder.
            nytid = TIDREGITAB.SLUT.
            RUN TIMSEK.P.          
            ASSIGN
            regdatumspar = regdatum
            regslutsek = sekunder
            regdatum = TIDREGITAB.DATUM.
            RUN TOTTID.P.
            ASSIGN TIDREGITAB.TOTALT = nytid.  /* obs används till FLEXDAG sedan*/                    
         END.
         ELSE DO:         
            CREATE TIDREGITAB.                      
            tidtabrec = RECID(TIDREGITAB).
            ASSIGN 
            SUBSTRING(TIDREGITAB.PROGRAM,1,158) = "PERTID" + STRING(TODAY) + STRING(TIME,"HH:MM") + Guru.Konstanter:globanv
            TIDREGITAB.PERSONALKOD = pkod      
            TIDREGITAB.DAG = regdagnamn
            TIDREGITAB.VECKONUMMER = regvnr
            TIDREGITAB.SLUT = regslut
            TIDREGITAB.START = regstart 
            TIDREGITAB.TRAKTAMENTE = CMB_TRAK
            TIDREGITAB.OVERTIDUTTAG = PERSONALTAB.OVERTIDUTTAG 
            TIDREGITAB.UTRYCKNING = FALSE
            TIDREGITAB.NODF = FALSE
            TIDREGITAB.PRISTYP = AONRTAB.PRISTYP
            TIDREGITAB.PRIS = TIMKOSTNADSTAB.PRISA
            TIDREGITAB.DATUM = regdatum
            TIDREGITAB.AONR = AONRTAB.AONR 
            TIDREGITAB.DELNR = AONRTAB.DELNR
            TIDREGITAB.RESMAL = FILL-IN_RESMAL.                        
            IF Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "Celpa" THEN DO:
               FIND FIRST FLEXAVT WHERE FLEXAVT.PERSONALKOD = PERSONALTAB.PERSONALKOD 
               USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.
               IF AVAILABLE FLEXAVT THEN DO:
                  IF FLEXAVT.FLEXTID = TRUE THEN DO:
                     IF lunchslutet NE lunchstarten THEN DO:                     
                        nytid = lunchslutet.
                        RUN TIMSEK.P.
                        seku = sekunder.
                        nytid = lunchstarten.
                        RUN TIMSEK.P.
                        sekunder = seku - sekunder.               
                        TIDREGITAB.LAGANTAL = sekunder / 60.
                     END.
                  END.
               END.
            END.
            RUN pris_UI.
            IF TIDREGITAB.PRISTYP = "FRÅNVARO." THEN ASSIGN TIDREGITAB.TRAKTAMENTE = 0.
            nytid = TIDREGITAB.START.
            RUN TIMSEK.P.
            regstartsek = sekunder.
            nytid = TIDREGITAB.SLUT.
            RUN TIMSEK.P.          
            ASSIGN
            regdatumspar = regdatum
            regslutsek = sekunder
            regdatum = TIDREGITAB.DATUM.
            RUN TOTTID.P.
            ASSIGN TIDREGITAB.TOTALT = nytid.  /* obs används till FLEXDAG sedan*/         
         END.
         IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "elpa"  THEN DO:                  
            FIND FIRST FLEXAVT WHERE FLEXAVT.PERSONALKOD = PERSONALTAB.PERSONALKOD USE-INDEX
            PERSONALKOD NO-LOCK NO-ERROR.
            IF AVAILABLE FLEXAVT THEN DO:
               IF FLEXAVT.FLEXTID = TRUE THEN DO:                                       
                  TIDREGITAB.OVERTIDUTTAG = "F".                                 
                  FIND FIRST FLEXTID WHERE FLEXTID.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
                  FLEXTID.DATUM = regdatum AND FLEXTID.KNAPP = "In" /*AND FLEXTID.AUTO = "PERI"*/ 
                  EXCLUSIVE-LOCK NO-ERROR.
                  IF NOT AVAILABLE FLEXTID THEN DO:            
                     CREATE FLEXTID.
                     ASSIGN
                     FLEXTID.AUTO = "PERI"
                     FLEXTID.DATUM = regdatum
                     FLEXTID.PERSONALKOD = PERSONALTAB.PERSONALKOD
                     FLEXTID.TID = regstart
                     FLEXTID.KNAPP = "In"
                     FLEXTID.KOM = TRUE
                     FLEXTID.GICK = FALSE.
                  END.
                  ASSIGN   
                  FLEXTID.AONR = varaonr
                  FLEXTID.DELNR = vardelnr
                  FLEXTID.ORSAK = AONRTAB.ORT.
                  FIND FIRST FLEXTID WHERE FLEXTID.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
                  FLEXTID.DATUM = regdatum AND FLEXTID.KNAPP = "Ut" /*AND FLEXTID.AUTO = "PERI"*/ 
                  EXCLUSIVE-LOCK NO-ERROR.
                  IF NOT AVAILABLE FLEXTID THEN DO:
                     CREATE FLEXTID.
                     ASSIGN
                     FLEXTID.AUTO = "PERI"
                     FLEXTID.DATUM = regdatum
                     FLEXTID.PERSONALKOD = PERSONALTAB.PERSONALKOD
                     FLEXTID.TID = regslut
                     FLEXTID.KNAPP = "Ut"
                     FLEXTID.KOM = FALSE
                     FLEXTID.GICK = TRUE.
                  END.
                  ASSIGN   
                  FLEXTID.AONR = varaonr
                  FLEXTID.DELNR = vardelnr
                  FLEXTID.ORSAK = AONRTAB.ORT.
                  regdatum = regdatum.
                  FIND FIRST FLEXDAG WHERE FLEXDAG.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
                  FLEXDAG.DATUM = regdatum USE-INDEX FLEX EXCLUSIVE-LOCK NO-ERROR.     
                  IF NOT AVAILABLE FLEXDAG THEN DO:
                     CREATE FLEXDAG.
                     ASSIGN FLEXDAG.PERSONALKOD = PERSONALTAB.PERSONALKOD
                     FLEXDAG.KONTROLL = "Ejkontroll".
                  END.         
                  ASSIGN
                  FLEXDAG.KONTROLL = "Kontroll"
                  FLEXDAG.DATUM = regdatum
                  FLEXDAG.START = regstart
                  FLEXDAG.SLUT = regslut
                  FLEXDAG.TOTALT = nytid
                  FLEXDAG.PLUS = 0
                  FLEXDAG.FELMED = ""
                  FLEXDAG.FELOK = TRUE.
                  IF varaonr = "155" THEN ASSIGN FLEXDAG.FLARB = 0 - nytid.
                  ELSE ASSIGN FLEXDAG.FLARB = 0.
               END.                                   
            END.                         
         END.          
      END.
      IF musz = TRUE THEN DO TRANSACTION:
         DELETE TIDREGITAB.
         musz = FALSE.
      END.
      ELSE DO:         
         RELEASE FLEXDAG NO-ERROR.
         RELEASE FLEXTID NO-ERROR.
         RELEASE TIDREGITAB NO-ERROR.            
         RUN nytolk_UI.         
      END.
   END.
   regdatum = regdatum + 1.
   IF regdatum > FILL-IN-STOPPDAT THEN DO: 
      /*SKIFT 0-24*/
      IF Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "GKAL" OR Guru.Konstanter:globforetag = "elpa" THEN DO:                       
         RUN REGVEC.P.
         RUN SLUTARB.P.
         RUN REGDAG.P.   
         IF regstart = 0 AND regdatum = ( FILL-IN-STOPPDAT + 1 )  THEN DO TRANSACTION:                  
            IF regslut NE 0 THEN DO:            
               CREATE TIDREGITAB.                      
               tidtabrec = RECID(TIDREGITAB).
               ASSIGN 
               SUBSTRING(TIDREGITAB.PROGRAM,1,158) = "PERTID" + STRING(TODAY) + STRING(TIME,"HH:MM") + Guru.Konstanter:globanv
               TIDREGITAB.PERSONALKOD = pkod      
               TIDREGITAB.DAG = regdagnamn
               TIDREGITAB.VECKONUMMER = regvnr
               TIDREGITAB.START = regstart 
               TIDREGITAB.SLUT = lunchstarten            
               TIDREGITAB.TRAKTAMENTE = CMB_TRAK
               TIDREGITAB.OVERTIDUTTAG = PERSONALTAB.OVERTIDUTTAG 
               TIDREGITAB.UTRYCKNING = FALSE
               TIDREGITAB.NODF = FALSE
               TIDREGITAB.PRISTYP = AONRTAB.PRISTYP
               TIDREGITAB.PRIS = TIMKOSTNADSTAB.PRISA
               TIDREGITAB.DATUM = regdatum
               TIDREGITAB.AONR = AONRTAB.AONR 
               TIDREGITAB.DELNR = AONRTAB.DELNR
               TIDREGITAB.RESMAL = FILL-IN_RESMAL.                                    
               IF regslut = 24 THEN TIDREGITAB.SLUT = lunchstarten.
               ELSE TIDREGITAB.SLUT = regslut.
            
               RUN pris_UI.
               IF TIDREGITAB.PRISTYP = "FRÅNVARO." THEN ASSIGN TIDREGITAB.TRAKTAMENTE = 0.
               nytid = TIDREGITAB.START.
               RUN TIMSEK.P.
               regstartsek = sekunder.
               nytid = TIDREGITAB.SLUT.
               RUN TIMSEK.P.          
               ASSIGN
               regdatumspar = regdatum
               regslutsek = sekunder
               regdatum = TIDREGITAB.DATUM.
               RUN TOTTID.P.
               ASSIGN TIDREGITAB.TOTALT = nytid.  /* obs används till FLEXDAG sedan*/         
               IF musz = TRUE THEN DO:
                  DELETE TIDREGITAB.
                  musz = FALSE.
               END.
               ELSE DO:                        
                  RELEASE TIDREGITAB NO-ERROR.            
                  RUN nytolk_UI.         
               END.         
            END.
         END.
      END.      
      LEAVE.
   END.
END.   
OPEN QUERY felq FOR EACH FELTEXT WHERE FELTEXT.ANVANDARE = Guru.Konstanter:globanv AND 
FELTEXT.EMOTAGET = FALSE
USE-INDEX FELTEXT NO-LOCK.
DO TRANSACTION:
   GET FIRST felq EXCLUSIVE-LOCK.
   IF NOT AVAILABLE FELTEXT THEN RETURN.
   CREATE felmeddtemp.
   felmeddtemp.FELMEDD = FELTEXT.
   ASSIGN FELTEXT.EMOTAGET = TRUE.
END.    
REPEAT:
   DO TRANSACTION:
      GET NEXT felq EXCLUSIVE-LOCK.
      IF NOT AVAILABLE FELTEXT THEN LEAVE.
      CREATE felmeddtemp.
      felmeddtemp.FELMEDD = FELTEXT.
      ASSIGN FELTEXT.EMOTAGET = TRUE.
   END.    
END.
RELEASE FLEXDAG NO-ERROR.
RELEASE FLEXTID NO-ERROR.
RELEASE TIDREGITAB NO-ERROR. 
RELEASE FELTEXT NO-ERROR.

PROCEDURE nytolk_UI :
   EMPTY TEMP-TABLE tidapptemp NO-ERROR.    
   CREATE tidapptemp.
   ASSIGN
   tidapptemp.FORETAG = Guru.Konstanter:globforetag
   tidapptemp.ANVANDARE = Guru.Konstanter:globanv
   tidapptemp.RECPERS = persrec
   tidapptemp.RECTID = tidtabrec
   tidapptemp.DATUM = regdatum.              
   
   {TIDUPPIN.I}
   musz = FALSE.   
   
END PROCEDURE.
PROCEDURE pris_UI :
   IF Guru.Konstanter:varforetypval[4] = 1 THEN DO:         
      FIND FIRST BEFATTNINGSTAB WHERE BEFATTNINGSTAB.NAMN = CMB_BEF
      NO-LOCK NO-ERROR.
      TIDREGITAB.OVERTIDTILL = BEFATTNINGSTAB.BEFATTNING.
      IF TIDREGITAB.PRISTYP = "FRÅNVARO." OR TIDREGITAB.PRISTYP = "RESTID..." THEN DO:
         FIND LAST PERSONALPRIS WHERE PERSONALPRIS.PERSONALKOD = TIDREGITAB.PERSONALKOD AND
         PERSONALPRIS.BEFATTNING = TIDREGITAB.PRISTYP AND 
         PERSONALPRIS.STARTDATUM <= TIDREGITAB.DATUM 
         AND PERSONALPRIS.SLUTDATUM >= TIDREGITAB.DATUM 
         NO-LOCK NO-ERROR.
         IF AVAILABLE PERSONALPRIS THEN TIDREGITAB.PRIS = PERSONALPRIS.PRIS.  
         ELSE DO:
            {SOKSTART.I}
            ASSIGN
            soktemp.SOKVAL = 1
            soktemp.SOKINT[1] = Guru.Konstanter:varforetypval[4]
            soktemp.SOKCHAR[2] = TIDREGITAB.PERSONALKOD
            soktemp.SOKCHAR[3] = TIDREGITAB.PRISTYP
            soktemp.SOKCHAR[4] = TIDREGITAB.OVERTIDTILL 
            soktemp.SOKDATE[1] = TIDREGITAB.DATUM.
            {SOKANROP.I}
            TIDREGITAB.PRIS = soktemp.SOKDECI[1].
         END.         
      END.
      ELSE IF (Guru.Konstanter:globforetag = "sund" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV") AND TIDREGITAB.PRISTYP = "EJ.KOSTN."  THEN DO:
         FIND LAST PERSONALPRIS WHERE PERSONALPRIS.PERSONALKOD = TIDREGITAB.PERSONALKOD AND
         PERSONALPRIS.BEFATTNING = TIDREGITAB.PRISTYP AND 
         PERSONALPRIS.STARTDATUM <= TIDREGITAB.DATUM 
         AND PERSONALPRIS.SLUTDATUM >= TIDREGITAB.DATUM 
         NO-LOCK NO-ERROR.
         IF AVAILABLE PERSONALPRIS THEN TIDREGITAB.PRIS = PERSONALPRIS.PRIS.  
         ELSE DO:
            {SOKSTART.I}
            ASSIGN
            soktemp.SOKVAL = 1
            soktemp.SOKINT[1] = Guru.Konstanter:varforetypval[4]
            soktemp.SOKCHAR[2] = TIDREGITAB.PERSONALKOD
            soktemp.SOKCHAR[3] = TIDREGITAB.PRISTYP
            soktemp.SOKCHAR[4] = TIDREGITAB.OVERTIDTILL 
            soktemp.SOKDATE[1] = TIDREGITAB.DATUM.
            {SOKANROP.I}
            TIDREGITAB.PRIS = soktemp.SOKDECI[1].
         END.
      END.
      ELSE IF Guru.Konstanter:globforetag = "ELPA" AND TIDREGITAB.PRISTYP = "EJ.KOSTN."  THEN DO:
         FIND LAST PERSONALPRIS WHERE PERSONALPRIS.PERSONALKOD = TIDREGITAB.PERSONALKOD AND
         PERSONALPRIS.BEFATTNING = TIDREGITAB.PRISTYP AND 
         PERSONALPRIS.STARTDATUM <= TIDREGITAB.DATUM 
         AND PERSONALPRIS.SLUTDATUM >= TIDREGITAB.DATUM 
         NO-LOCK NO-ERROR.
         IF AVAILABLE PERSONALPRIS THEN TIDREGITAB.PRIS = PERSONALPRIS.PRIS.  
         ELSE DO:
            {SOKSTART.I}
            ASSIGN
            soktemp.SOKVAL = 1
            soktemp.SOKINT[1] = Guru.Konstanter:varforetypval[4]
            soktemp.SOKCHAR[2] = TIDREGITAB.PERSONALKOD
            soktemp.SOKCHAR[3] = TIDREGITAB.PRISTYP
            soktemp.SOKCHAR[4] = TIDREGITAB.OVERTIDTILL 
            soktemp.SOKDATE[1] = TIDREGITAB.DATUM.
            {SOKANROP.I}
            TIDREGITAB.PRIS = soktemp.SOKDECI[1].
         END.
      END.
      ELSE IF (Guru.Konstanter:globforetag = "sund" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV") AND TIDREGITAB.PRISTYP = "FASTPRIS1"  THEN DO:
         FIND LAST PERSONALPRIS WHERE PERSONALPRIS.PERSONALKOD = TIDREGITAB.PERSONALKOD AND
         PERSONALPRIS.BEFATTNING = TIDREGITAB.PRISTYP AND 
         PERSONALPRIS.STARTDATUM <= TIDREGITAB.DATUM 
         AND PERSONALPRIS.SLUTDATUM >= TIDREGITAB.DATUM 
         NO-LOCK NO-ERROR.
         IF AVAILABLE PERSONALPRIS THEN TIDREGITAB.PRIS = PERSONALPRIS.PRIS.  
         ELSE DO:
            {SOKSTART.I}
            ASSIGN
            soktemp.SOKVAL = 1
            soktemp.SOKINT[1] = Guru.Konstanter:varforetypval[4]
            soktemp.SOKCHAR[2] = TIDREGITAB.PERSONALKOD
            soktemp.SOKCHAR[3] = TIDREGITAB.PRISTYP
            soktemp.SOKCHAR[4] = TIDREGITAB.OVERTIDTILL 
            soktemp.SOKDATE[1] = TIDREGITAB.DATUM.
            {SOKANROP.I}
            TIDREGITAB.PRIS = soktemp.SOKDECI[1].
         END.
      END.
      ELSE IF Guru.Konstanter:globforetag = "ELPA" AND TIDREGITAB.PRISTYP = "FASTPRIS1"  THEN DO:
         FIND LAST PERSONALPRIS WHERE PERSONALPRIS.PERSONALKOD = TIDREGITAB.PERSONALKOD AND
         PERSONALPRIS.BEFATTNING = TIDREGITAB.PRISTYP AND 
         PERSONALPRIS.STARTDATUM <= TIDREGITAB.DATUM 
         AND PERSONALPRIS.SLUTDATUM >= TIDREGITAB.DATUM 
         NO-LOCK NO-ERROR.
         IF AVAILABLE PERSONALPRIS THEN TIDREGITAB.PRIS = PERSONALPRIS.PRIS.  
         ELSE DO:
            {SOKSTART.I}
            ASSIGN
            soktemp.SOKVAL = 1
            soktemp.SOKINT[1] = Guru.Konstanter:varforetypval[4]
            soktemp.SOKCHAR[2] = TIDREGITAB.PERSONALKOD
            soktemp.SOKCHAR[3] = TIDREGITAB.PRISTYP
            soktemp.SOKCHAR[4] = TIDREGITAB.OVERTIDTILL 
            soktemp.SOKDATE[1] = TIDREGITAB.DATUM.
            {SOKANROP.I}
            TIDREGITAB.PRIS = soktemp.SOKDECI[1].
         END.
      END.
      ELSE DO:
         FIND LAST PERSONALPRIS WHERE PERSONALPRIS.PERSONALKOD = TIDREGITAB.PERSONALKOD AND
         PERSONALPRIS.BEFATTNING = BEFATTNINGSTAB.BEFATTNING AND 
         PERSONALPRIS.STARTDATUM <= TIDREGITAB.DATUM 
         AND PERSONALPRIS.SLUTDATUM >= TIDREGITAB.DATUM 
         NO-LOCK NO-ERROR.
         IF AVAILABLE PERSONALPRIS THEN TIDREGITAB.PRIS = PERSONALPRIS.PRIS.  
         ELSE DO:
            {SOKSTART.I}
            ASSIGN
            soktemp.SOKVAL = 1
            soktemp.SOKINT[1] = Guru.Konstanter:varforetypval[4]
            soktemp.SOKCHAR[2] = TIDREGITAB.PERSONALKOD
            soktemp.SOKCHAR[3] = TIDREGITAB.PRISTYP
            soktemp.SOKCHAR[4] = TIDREGITAB.OVERTIDTILL 
            soktemp.SOKDATE[1] = TIDREGITAB.DATUM.
            {SOKANROP.I}
            TIDREGITAB.PRIS = soktemp.SOKDECI[1].
         END.
      END.
   END.
   
END PROCEDURE.


