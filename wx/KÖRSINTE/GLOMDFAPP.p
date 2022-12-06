/*GLOMDFAPP.P*/
&Scoped-define NEW NEW
{GLOBVAR2DEL1.I}

{SOKDEF.I}
DEFINE INPUT PARAMETER vart AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER pkod AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER inregdatum AS DATE NO-UNDO.
DEFINE INPUT PARAMETER CMB_KNAPP AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER varaonr AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER vardelnr AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER FILL-IN_TID AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER flexkvst AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER flexkvsl AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER inglobanv AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER sregstart AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER sregslut AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER sregdagnamn AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER sregvnr AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER TABLE FOR felmeddtemp.
DEFINE NEW SHARED VARIABLE flrec AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE fldrec AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE persrec AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE regstart LIKE TIDREGITAB.START NO-UNDO. 
DEFINE NEW SHARED VARIABLE regslut LIKE TIDREGITAB.SLUT NO-UNDO.
DEFINE NEW SHARED VARIABLE regvnr AS INTEGER FORMAT "999" NO-UNDO.
DEFINE NEW SHARED VARIABLE regdagnamn AS CHARACTER FORMAT "X(3)" NO-UNDO.        
DEFINE NEW SHARED VARIABLE regdatum AS DATE NO-UNDO.
DEFINE NEW SHARED VARIABLE regtotalt LIKE TIDREGITAB.TOTALT NO-UNDO.
DEFINE NEW SHARED VARIABLE frustarten AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE fruslutet AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE kaffestart AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE kaffeslut AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE lunchstarten AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE lunchslutet AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE nytid AS DECIMAL FORMAT "99.99" NO-UNDO.
DEFINE NEW SHARED VARIABLE sekunder AS INTEGER NO-UNDO.    
DEFINE NEW SHARED VARIABLE regstartsek AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE regslutsek AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE flexstart AS DECIMAL NO-UNDO. 
DEFINE VARIABLE fstart AS DECIMAL NO-UNDO.     
DEFINE VARIABLE fslut AS DECIMAL NO-UNDO.     
DEFINE VARIABLE flkoll AS DECIMAL NO-UNDO.     
DEFINE VARIABLE maxarbkort AS DECIMAL NO-UNDO.     
DEFINE VARIABLE flexmost AS DECIMAL NO-UNDO.     
DEFINE VARIABLE flexmosl AS DECIMAL NO-UNDO.     
DEFINE VARIABLE seku AS INTEGER NO-UNDO.
DEFINE VARIABLE spslut AS DECIMAL NO-UNDO.
DEFINE VARIABLE foremaxarbkort AS DECIMAL NO-UNDO.
ASSIGN
globanv = inglobanv
regdatum = inregdatum
regstart = sregstart
regslut = sregslut
regdagnamn = sregdagnamn
regvnr = sregvnr. 
FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = pkod NO-LOCK NO-ERROR.
FIND FIRST AONRTAB WHERE AONRTAB.AONR = varaonr AND AONRTAB.DELNR = vardelnr
NO-LOCK NO-ERROR.
FIND FIRST TIMKOSTNADSTAB WHERE TIMKOSTNADSTAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
TIMKOSTNADSTAB.PRISTYP = AONRTAB.PRISTYP USE-INDEX PRISPERS NO-LOCK NO-ERROR.          
persrec = RECID(PERSONALTAB).
FIND FIRST FORETAG NO-LOCK NO-ERROR.
globforetag = FORETAG.FORETAG.
{FORESTYR.I}
FUNCTION klock100 RETURNS DECIMAL
  ( INPUT ber60 AS DECIMAL ):
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN  (TRUNCATE(ber60,0) * 3600 + (ber60 - TRUNCATE(ber60,0)) * 100 * 60) / 3600.

END FUNCTION.

DEFINE BUFFER flexbuff FOR FLEXTID.
DEFINE BUFFER tidbuff FOR TIDREGITAB.
IF vart = "nya" THEN DO:
   FIND FIRST FLEXTID WHERE FLEXTID.PERSONALKOD = PERSONALTAB.PERSONALKOD
   AND FLEXTID.DATUM = regdatum AND FLEXTID.KNAPP = CMB_KNAPP USE-INDEX FLEX
   NO-LOCK NO-ERROR.
   IF AVAILABLE FLEXTID THEN DO:
      IF CMB_KNAPP = "FLEX IN" OR CMB_KNAPP = "FLEX UT" THEN vart = vart.
      ELSE DO:
         CREATE felmeddtemp. 
         felmeddtemp.felmedd = "Det finns redan en " + CMB_KNAPP + " registrering.".
         RETURN.
      END.
   END.   
   IF CMB_KNAPP = "IN" THEN DO:     
      FIND FIRST flexbuff WHERE flexbuff.PERSONALKOD = PERSONALTAB.PERSONALKOD
      AND flexbuff.DATUM = regdatum AND flexbuff.KOM = TRUE USE-INDEX FLEX NO-LOCK NO-ERROR.
      IF AVAILABLE flexbuff THEN DO:
         CREATE felmeddtemp. 
         felmeddtemp.felmedd = "Det finns redan en in-registrering.". 
         RETURN.
      END.   
   END.   
   IF CMB_KNAPP = "UT" THEN DO:     
      FIND LAST flexbuff WHERE flexbuff.PERSONALKOD = PERSONALTAB.PERSONALKOD
      AND flexbuff.DATUM = regdatum  USE-INDEX FLEX NO-LOCK NO-ERROR.
      IF AVAILABLE flexbuff AND flexbuff.GICK = TRUE THEN DO:
         CREATE felmeddtemp. 
         felmeddtemp.felmedd = "Det finns redan en ut-registrering.".
         RETURN.
      END.   
   END.   
   
   /* arbetstidsförkortning*/
   IF globforetag = "SUND" OR globforetag = "SNAT" OR globforetag = "MISV" OR globforetag = "LULE" OR globforetag = "ELPA" THEN DO:            
      DEFINE BUFFER tidbuff10 FOR TIDREGITAB.
      DEFINE VARIABLE totarbkort AS DECIMAL NO-UNDO.
      DEFINE VARIABLE totarbkorti AS DECIMAL NO-UNDO.
      DEFINE VARIABLE avarfor AS INTEGER NO-UNDO.
      IF AVAILABLE AONRTAB AND AONRTAB.AONR = "160" THEN DO:              
         totarbkort = 0.                       
         FOR EACH tidbuff10 WHERE tidbuff10.PERSONALKOD = PERSONALTAB.PERSONALKOD AND tidbuff10.AONR = "160"
         AND YEAR(tidbuff10.DATUM) = YEAR(regdatum) AND tidbuff10.TIDLOG = TRUE NO-LOCK:
            totarbkort = totarbkort + klock100(tidbuff10.TOTALT).
         END.         
         totarbkorti = totarbkort.         
         IF CMB_KNAPP = "ANNAT IN" THEN DO:
            ASSIGN 
            nytid = regstart.
            RUN TIMSEK.P.
            regstartsek = sekunder.
            nytid = FILL-IN_TID.
            RUN TIMSEK.P.
            regslutsek = sekunder. 
            RUN TOTTID.P.            
            totarbkort = totarbkort +  klock100(nytid).
         END.
         IF CMB_KNAPP = "ANNAT UT" OR CMB_KNAPP = "IN" THEN DO:
            ASSIGN 
            nytid = FILL-IN_TID.
            RUN TIMSEK.P.
            regstartsek = sekunder.
            nytid = regslut.
            RUN TIMSEK.P.
            regslutsek = sekunder. 
            RUN TOTTID.P.            
            totarbkort = totarbkort +  klock100(nytid).
         END.      
         /*atkgräns*/
         IF globforetag = "gkal"  THEN foremaxarbkort = 63.
         ELSE IF globforetag = "SNAT"  THEN foremaxarbkort = 63.
         ELSE IF globforetag = "SUND"  THEN foremaxarbkort = 54.
         ELSE IF globforetag = "LULE"  THEN foremaxarbkort = 63.            
         ELSE foremaxarbkort = 63.
         avarfor = 0.
         IF globforetag = "gkal" OR globforetag = "SUND" OR globforetag = "SNAT" OR globforetag = "MISV" OR globforetag = "elpa" THEN DO:   
            RUN havafor (INPUT PERSONALTAB.PERSONALKOD, OUTPUT avarfor).                        
         END.         
         IF avarfor > 0 THEN maxarbkort = avarfor.
         ELSE maxarbkort = foremaxarbkort.
         IF totarbkort > maxarbkort THEN DO:            
            CREATE felmeddtemp.  
            felmeddtemp.felmedd = "Max " + STRING(maxarbkort) + " timmar per år får skrivas på arbetstidförkortning, du har tidigare skrivit "  + STRING(totarbkorti) + "timmar".
            RETURN.                  
         END.            
      END.  
      IF AVAILABLE AONRTAB AND AONRTAB.AONR = "161" THEN DO:              
         totarbkort = 0.                       
         FOR EACH tidbuff10 WHERE tidbuff10.PERSONALKOD = PERSONALTAB.PERSONALKOD AND tidbuff10.AONR = "161"
         AND YEAR(tidbuff10.DATUM) = YEAR(regdatum) AND tidbuff10.TIDLOG = TRUE NO-LOCK:
            totarbkort = totarbkort + klock100(tidbuff10.TOTALT).
         END.         
         totarbkorti = totarbkort.
         IF CMB_KNAPP = "ANNAT IN" THEN DO:
            totarbkort = totarbkort +  klock100(FILL-IN_TID) - klock100(regstart).
         END.
         IF CMB_KNAPP = "ANNAT UT" THEN DO:
            totarbkort = totarbkort +  klock100(regslut) - klock100(FILL-IN_TID).
         END.
         IF CMB_KNAPP = "IN" THEN DO:
            totarbkort = totarbkort +  klock100(regslut) - klock100(FILL-IN_TID).
         END.        
         /*atkgräns*/          
         maxarbkort = 27.
         /*IF regdatum GE 01/01/2004 THEN maxarbkort = 27.
         ELSE maxarbkort = 18.*/
         IF totarbkort > maxarbkort THEN DO:            
            CREATE felmeddtemp.  
            felmeddtemp.felmedd = "Max " + STRING(maxarbkort) + " timmar per år får skrivas på arbetstidförkortning,du har skrivit"  + STRING(totarbkorti) + "timmar".
            RETURN.                  
         END.            
      END.    
      IF AVAILABLE AONRTAB AND AONRTAB.AONR = "135" THEN DO:              
         totarbkort = 0.                       
         FOR EACH tidbuff10 WHERE tidbuff10.PERSONALKOD = PERSONALTAB.PERSONALKOD AND tidbuff10.AONR = "135"
         AND YEAR(tidbuff10.DATUM) = YEAR(regdatum) AND tidbuff10.TIDLOG = TRUE NO-LOCK:
            totarbkort = totarbkort + klock100(tidbuff10.TOTALT).
         END.         
         totarbkort = totarbkorti.
         IF CMB_KNAPP = "ANNAT IN" THEN DO:
            totarbkort = totarbkort +  klock100(FILL-IN_TID) - klock100(regstart).
         END.
         IF CMB_KNAPP = "ANNAT UT" THEN DO:
            totarbkort = totarbkort +  klock100(regslut) - klock100(FILL-IN_TID).
         END.
         IF CMB_KNAPP = "IN" THEN DO:
            totarbkort = totarbkort +  klock100(regslut) - klock100(FILL-IN_TID).
         END.                  
         /*maxarbkort = 27.
         /*IF regdatum GE 01/01/2004 THEN maxarbkort = 27.
         ELSE maxarbkort = 18.*/
         IF totarbkort > maxarbkort THEN DO:            
            CREATE felmeddtemp.  
            felmeddtemp.felmedd = "Max " + STRING(maxarbkort) + " timmar per år får skrivas på arbetstidförkortning,du har skrivit"  + STRING(totarbkorti) + "timmar".
            RETURN.                  
         END.            */
      END.    
   END.
END.
RUN btnreg_UI.

PROCEDURE btnreg_UI :
   FIND FIRST FLEXAVT WHERE FLEXAVT.PERSONALKOD = PERSONALTAB.PERSONALKOD NO-LOCK NO-ERROR.
   FIND FIRST FLEXREG WHERE FLEXREG.KOD = FLEXAVT.FLEXKOD USE-INDEX FLEXREG NO-LOCK NO-ERROR.
   DO TRANSACTION:   
      CREATE FLEXTID. 
      ASSIGN       
      FLEXTID.PERSONALKOD = PERSONALTAB.PERSONALKOD
      FLEXTID.KNAPP = CMB_KNAPP 
      FLEXTID.DATUM = regdatum 
      FLEXTID.TID = FILL-IN_TID
      FLEXTID.AONR = AONRTAB.AONR
      FLEXTID.DELNR = AONRTAB.DELNR
      FLEXTID.ORSAK = "Glömd registrering"
      FLEXTID.AUTO = "GLOM".   
      IF vart = "NYA" THEN DO:
         flexstart = FLEXTID.TID. 
      END.   
      flrec = RECID(FLEXTID).
      RUN SLUTARB.P.
      IF FLEXTID.KNAPP = "In" THEN ASSIGN FLEXTID.KOM = TRUE FLEXTID.GICK = FALSE.
      IF FLEXTID.KNAPP = "Ut" THEN ASSIGN FLEXTID.KOM = FALSE FLEXTID.GICK = TRUE. 
      FIND FIRST FLEXDAG WHERE FLEXDAG.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
      FLEXDAG.DATUM = FLEXTID.DATUM USE-INDEX FLEX EXCLUSIVE-LOCK NO-ERROR.
      IF NOT AVAILABLE FLEXDAG THEN DO:
         CREATE FLEXDAG.
         ASSIGN FLEXDAG.PERSONALKOD = PERSONALTAB.PERSONALKOD
         FLEXDAG.DATUM = FLEXTID.DATUM
         FLEXDAG.KONTROLL = "Ejkontroll"
         FLEXDAG.START = regstart
         FLEXDAG.SLUT = regslut
         FLEXDAG.FELOK = FALSE.
      END.  
      ASSIGN
      fldrec = RECID(FLEXDAG)      
      regdatum = FLEXDAG.DATUM.
      IF FLEXDAG.KONTROLL = "Kontroll" THEN DO:
         ASSIGN FLEXDAG.KONTROLL = "Ejkontroll" FLEXDAG.FELOK = FALSE.
      END.   
      IF FLEXTID.KNAPP = "In" THEN DO:      
         IF AVAILABLE FLEXREG THEN DO:
            ASSIGN
            flexmost = FLEXREG.MOSTART.
            flexmosl = FLEXREG.MOSLUT.
            /* sommartid*/
            /*k- morgonflex till 9 både sommar och vinter , övriga sommar 8.30 vinter 9 -ändras manuellt i FLEXREG*/         
            IF MONTH(regdatum) > MONTH(FLEXREG.SOMMARST) AND MONTH(regdatum) < MONTH(FLEXREG.SOMMARSL) THEN DO:
               IF FLEXAVT.FLEXKOD = "K" THEN regdatum = regdatum.               
               ELSE IF flexmosl = 9 THEN ASSIGN flexmosl = 8.30.
               /*ELSE IF FLEXAVT.FLEXKOD = "KV" AND flexmosl = 8 THEN ASSIGN flexmosl = 7.30.*/                       
            END.
            ELSE IF MONTH(regdatum) = MONTH(FLEXREG.SOMMARSL) AND DAY(regdatum) <= DAY(FLEXREG.SOMMARSL) THEN DO:
               IF FLEXAVT.FLEXKOD = "K" THEN regdatum = regdatum.
               ELSE IF flexmosl = 9 THEN ASSIGN flexmosl = 8.30.
               /*ELSE IF FLEXAVT.FLEXKOD = "KV" AND flexmosl = 8 THEN ASSIGN flexmosl = 7.30.*/
                          
            END.
            ELSE IF MONTH(regdatum) = MONTH(FLEXREG.SOMMARST) AND DAY(regdatum) >= DAY(FLEXREG.SOMMARST) THEN DO: 
               IF FLEXAVT.FLEXKOD = "K" THEN regdatum = regdatum.
               ELSE IF flexmosl = 9 THEN ASSIGN flexmosl = 8.30.
               /*ELSE IF FLEXAVT.FLEXKOD = "KV" AND flexmosl = 8 THEN ASSIGN flexmosl = 7.30.*/                           
            END.
            ELSE DO:
               IF FLEXAVT.FLEXKOD = "K" THEN regdatum = regdatum.
               ELSE  IF flexmosl = 8.30 THEN ASSIGN flexmosl = 9.00.                       
               /*ELSE IF FLEXAVT.FLEXKOD = "KV" AND flexmosl = 7.3 THEN ASSIGN flexmosl = 8.*/  
            END.
            IF PERSONALTAB.DELTID = TRUE THEN DO:
               FIND FIRST ORDARB WHERE ORDARB.ANSTALLNING = PERSONALTAB.ANSTALLNING NO-LOCK NO-ERROR.    
               IF AVAILABLE ORDARB THEN DO:
                  sekunder = ORDARB.START1.
                  RUN SEKTIM.P.               
                  IF nytid < regstart THEN DO:                   
                     nytid = flexmosl.
                     RUN TIMSEK.P.
                     seku = sekunder.
                     nytid = regstart.
                     RUN TIMSEK.P.
                     sekunder = seku - ORDARB.START1 + sekunder.
                     RUN SEKTIM.P.
                     flexmosl = nytid.
                     nytid = flexmost.
                     RUN TIMSEK.P.
                     seku = sekunder.
                     nytid = regstart.
                     RUN TIMSEK.P.
                     sekunder = seku - ORDARB.START1 + sekunder.
                     RUN SEKTIM.P.
                     flexmost = nytid.
                  END.               
               END.
            END.
            IF flexmost > FLEXTID.TID THEN ASSIGN FLEXDAG.START = flexmost.            
            ELSE ASSIGN FLEXDAG.START = FLEXTID.TID.                       
         END.
         
         IF vart = "NYA" THEN DO:
            IF AVAILABLE GODKOLL AND GODKOLL.DATUM GE regdatum THEN regdatum = regdatum.
            ELSE DO:
               FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD
               AND TIDREGITAB.DATUM = regdatum AND TIDLOG = TRUE 
               AND TIDREGITAB.OVERTIDUTTAG = "F" AND TIDREGITAB.GODKAND = "" EXCLUSIVE-LOCK NO-ERROR.
               IF NOT AVAILABLE TIDREGITAB THEN DO:         
                  ASSIGN
                  fstart = FLEXDAG.START
                  fslut = FLEXDAG.SLUT.
                  RUN nytid_UI.                  
               END.
               ELSE DO:         
                  IF FLEXTID.TID < TIDREGITAB.SLUT THEN DO:                   
                     ASSIGN
                     TIDREGITAB.START = FLEXDAG.START.
                     RUN andtid_UI.                     
                     IF AVAILABLE AONRTAB THEN DO:                  
                        ASSIGN
                        TIDREGITAB.AONR = AONRTAB.AONR
                        TIDREGITAB.DELNR = AONRTAB.DELNR.
                        TIDREGITAB.PRISTYP = AONRTAB.PRISTYP.
                        IF AVAILABLE TIMKOSTNADSTAB THEN DO:                     
                           TIDREGITAB.PRIS = TIMKOSTNADSTAB.PRISA.                  
                        END.
                     END.
                  END.
               END.   
            END.
         END.   
      END.   
      ELSE IF FLEXTID.KNAPP = "Ut" THEN DO: 
         IF AVAILABLE FLEXREG THEN DO:
            IF flexkvsl < FLEXTID.TID THEN ASSIGN FLEXDAG.SLUT = flexkvsl.
            ELSE ASSIGN FLEXDAG.SLUT = FLEXTID.TID.
         END.   
         IF vart = "NYA" THEN DO:
            IF AVAILABLE GODKOLL AND GODKOLL.DATUM GE regdatum THEN regdatum = regdatum.
            ELSE DO:
               FIND LAST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD
               AND TIDREGITAB.DATUM = regdatum AND TIDLOG = TRUE 
               AND TIDREGITAB.OVERTIDUTTAG = "F" AND TIDREGITAB.GODKAND = "" EXCLUSIVE-LOCK NO-ERROR.
               IF NOT AVAILABLE TIDREGITAB THEN DO:         
                  ASSIGN
                  fstart = FLEXDAG.START
                  fslut = FLEXDAG.SLUT.
                  RUN nytid_UI.                  
               END.
               ELSE DO:                          
                  ASSIGN TIDREGITAB.SLUT = FLEXDAG.SLUT.
                  RUN andtid_UI. 
                  IF AVAILABLE AONRTAB THEN DO:                  
                     ASSIGN
                     TIDREGITAB.AONR = AONRTAB.AONR
                     TIDREGITAB.DELNR = AONRTAB.DELNR.
                     TIDREGITAB.PRISTYP = AONRTAB.PRISTYP.
                     IF AVAILABLE TIMKOSTNADSTAB THEN DO:                     
                        TIDREGITAB.PRIS = TIMKOSTNADSTAB.PRISA.                  
                     END.
                  END.
               END.   
            END.
         END.   
      END.   
      IF FLEXTID.KNAPP = "Flex in" THEN DO:
         FIND flexbuff WHERE RECID(flexbuff) = flrec NO-LOCK NO-ERROR.   
         FIND PREV flexbuff USE-INDEX FLEX EXCLUSIVE-LOCK NO-ERROR.
         IF AVAILABLE flexbuff THEN DO:
            IF flexbuff.PERSONALKOD = FLEXTID.PERSONALKOD AND 
            flexbuff.DATUM = FLEXTID.DATUM AND flexbuff.KNAPP = "Flex ut" THEN DO:
               ASSIGN flexbuff.GICK = FALSE
               nytid = flexbuff.TID.
               RUN TIMSEK.P.
               regstartsek = sekunder.
               nytid = FLEXTID.TID.
               RUN TIMSEK.P.
               regslutsek = sekunder. 
               RUN TOTTID.P.
               ASSIGN FLEXDAG.FLARB = 0 - nytid.
            END.
            ELSE IF flexbuff.PERSONALKOD = FLEXTID.PERSONALKOD AND 
            flexbuff.DATUM = FLEXTID.DATUM AND flexbuff.KNAPP = "Lunch ut" THEN DO:
               ASSIGN flexbuff.GICK = FALSE
               nytid = flexbuff.TID.
               RUN TIMSEK.P.
               regstartsek = sekunder.
               nytid = FLEXTID.TID.
               RUN TIMSEK.P.
               regslutsek = sekunder. 
               RUN TOTTID.P.
               ASSIGN FLEXDAG.FLARB = 0 - nytid.
            END.
            ELSE IF flexbuff.PERSONALKOD = FLEXTID.PERSONALKOD AND 
            flexbuff.DATUM NE FLEXTID.DATUM THEN DO:
               ASSIGN FLEXTID.KOM = TRUE
               nytid = regstart.
               RUN TIMSEK.P.
               regstartsek = sekunder.
               nytid = FLEXTID.TID.
               RUN TIMSEK.P.
               regslutsek = sekunder. 
               RUN TOTTID.P.
               ASSIGN FLEXDAG.FLARB = 0 - nytid.
            END.
            ELSE IF flexbuff.PERSONALKOD NE FLEXTID.PERSONALKOD  THEN DO:
               ASSIGN FLEXTID.KOM = TRUE
               nytid = regstart.
               RUN TIMSEK.P.
               regstartsek = sekunder.
               nytid = FLEXTID.TID.
               RUN TIMSEK.P.
               regslutsek = sekunder. 
               RUN TOTTID.P.
               ASSIGN FLEXDAG.FLARB = 0 - nytid.
            END.
         END.          
         ELSE DO:
            ASSIGN FLEXTID.KOM = TRUE
            nytid = regstart.
            RUN TIMSEK.P.
            regstartsek = sekunder.
            nytid = FLEXTID.TID.
            RUN TIMSEK.P.
            regslutsek = sekunder. 
            RUN TOTTID.P.
            ASSIGN FLEXDAG.FLARB = 0 - nytid.
         END.
         IF vart = "NYA" THEN DO:
            IF AVAILABLE GODKOLL AND GODKOLL.DATUM GE regdatum THEN regdatum = regdatum.
            ELSE DO:
               FIND LAST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD
               AND TIDREGITAB.DATUM = regdatum AND TIDREGITAB.TIDLOG = TRUE 
               AND TIDREGITAB.OVERTIDUTTAG = "F" AND TIDREGITAB.GODKAND = "" EXCLUSIVE-LOCK NO-ERROR.
               IF NOT AVAILABLE TIDREGITAB THEN DO:         
                  ASSIGN
                  fstart = FLEXDAG.START
                  fslut = FLEXTID.TID.
                  RUN flnytid_UI.                  
                  CREATE tidbuff.
                  ASSIGN tidbuff.PERSONALKOD = PERSONALTAB.PERSONALKOD
                  tidbuff.START = FLEXTID.TID     
                  tidbuff.PROGRAM = 'GLOMDF' + STRING(TODAY) + STRING(TIME,"HH:MM") + Guru.Konstanter:globanv
                  tidbuff.DATUM = TIDREGITAB.DATUM
                  tidbuff.DAG = TIDREGITAB.DAG 
                  tidbuff.VECKONUMMER = TIDREGITAB.VECKONUMMER
                  tidbuff.TRAKTAMENTE = AONRTAB.TRAKTAMENTE 
                  tidbuff.OVERTIDUTTAG = 'F'
                  tidbuff.AONR = AONRTAB.AONR 
                  tidbuff.DELNR = AONRTAB.DELNR 
                  tidbuff.PRISTYP = AONRTAB.PRISTYP
                  tidbuff.PRIS = TIMKOSTNADSTAB.PRISA.                  
                  RUN pris2_UI.
                  ASSIGN tidbuff.SLUT = regslut.
                  ASSIGN                  
                  nytid = tidbuff.START.
                  RUN TIMSEK.P.
                  regstartsek = sekunder.
                  nytid = tidbuff.SLUT.
                  RUN TIMSEK.P.
                  regslutsek = sekunder.
                  regdatum = tidbuff.DATUM.                 
                  RUN TOTTID.P.
                  ASSIGN tidbuff.TOTALT = nytid.
                  VALIDATE tidbuff.
               END.
               ELSE DO:  
                  IF FLEXTID.TID > TIDREGITAB.START THEN DO:                  
                     ASSIGN 
                     flkoll =  TIDREGITAB.SLUT
                     TIDREGITAB.SLUT = FLEXTID.TID.
                     RUN flandtid_UI.                                      
                     CREATE tidbuff.
                     ASSIGN tidbuff.PERSONALKOD = PERSONALTAB.PERSONALKOD
                     tidbuff.START = FLEXTID.TID     
                     tidbuff.PROGRAM = 'GLOMDF' + STRING(TODAY) + STRING(TIME,"HH:MM") + Guru.Konstanter:globanv
                     tidbuff.DATUM = TIDREGITAB.DATUM
                     tidbuff.DAG = TIDREGITAB.DAG 
                     tidbuff.VECKONUMMER = TIDREGITAB.VECKONUMMER
                     tidbuff.TRAKTAMENTE = AONRTAB.TRAKTAMENTE 
                     tidbuff.OVERTIDUTTAG = 'F'
                     tidbuff.AONR = AONRTAB.AONR 
                     tidbuff.DELNR = AONRTAB.DELNR 
                     tidbuff.PRISTYP = AONRTAB.PRISTYP
                     tidbuff.PRIS = TIMKOSTNADSTAB.PRISA.                  
                     ASSIGN tidbuff.SLUT = flkoll.
                     RUN pris2_UI.
                     ASSIGN                  
                     nytid = tidbuff.START.
                     RUN TIMSEK.P.
                     regstartsek = sekunder.
                     nytid = tidbuff.SLUT.
                     RUN TIMSEK.P.
                     regslutsek = sekunder.
                     regdatum = tidbuff.DATUM.                 
                     RUN TOTTID.P.
                     ASSIGN tidbuff.TOTALT = nytid.
                     VALIDATE tidbuff.
                  END.
               END.   
            END.
         END.   
      END.      
      ELSE IF FLEXTID.KNAPP = "Flex ut" THEN DO:         
         FIND FIRST flexbuff WHERE flexbuff.PERSONALKOD = PERSONALTAB.PERSONALKOD
         AND flexbuff.DATUM = regdatum AND flexbuff.GICK = TRUE USE-INDEX FLEX NO-LOCK NO-ERROR.
         IF AVAILABLE flexbuff THEN ASSIGN FLEXTID.GICK = FALSE.            
         ELSE DO:   
            ASSIGN FLEXTID.GICK = TRUE.
            nytid = FLEXTID.TID.
            RUN TIMSEK.P.
            regstartsek = sekunder.
            nytid = regslut.
            RUN TIMSEK.P.
            regslutsek = sekunder. 
            RUN TOTTID.P.
            ASSIGN FLEXDAG.FLARB = 0 - nytid.   
         END.     
         IF vart = "NYA" THEN DO:
            IF AVAILABLE GODKOLL AND GODKOLL.DATUM GE regdatum THEN regdatum = regdatum.
            ELSE DO:
               FIND LAST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD
               AND TIDREGITAB.DATUM = regdatum AND TIDREGITAB.TIDLOG = TRUE 
               AND TIDREGITAB.OVERTIDUTTAG = "F" AND TIDREGITAB.GODKAND = "" USE-INDEX PSTART EXCLUSIVE-LOCK NO-ERROR.
               IF NOT AVAILABLE TIDREGITAB THEN DO:         
                  ASSIGN
                  fstart = FLEXDAG.START
                  fslut = FLEXTID.TID.
                  /*flkoll = TIDREGITAB.SLUT.*/
                  RUN nytid_UI.                   
                  CREATE tidbuff.
                  ASSIGN 
                  tidbuff.PERSONALKOD = PERSONALTAB.PERSONALKOD
                  tidbuff.START = FLEXTID.TID     
                  tidbuff.PROGRAM = 'GLOMDF' + STRING(TODAY) + STRING(TIME,"HH:MM") + Guru.Konstanter:globanv
                  tidbuff.DATUM = TIDREGITAB.DATUM
                  tidbuff.DAG = TIDREGITAB.DAG 
                  tidbuff.VECKONUMMER = TIDREGITAB.VECKONUMMER
                  tidbuff.TRAKTAMENTE = 0 
                  tidbuff.OVERTIDUTTAG = 'F'
                  tidbuff.AONR = "155" 
                  tidbuff.DELNR = 0 
                  tidbuff.PRISTYP = "FRÅNVARO."
                  tidbuff.PRIS = 0.                    
                  ASSIGN tidbuff.SLUT = regslut.
                  RUN pris2_UI.
                  ASSIGN                  
                  nytid = tidbuff.START.
                  RUN TIMSEK.P.
                  regstartsek = sekunder.
                  nytid = tidbuff.SLUT.
                  RUN TIMSEK.P.
                  regslutsek = sekunder.
                  regdatum = tidbuff.DATUM.                 
                  RUN TOTTID.P.
                  ASSIGN tidbuff.TOTALT = nytid.
                  VALIDATE tidbuff.
               END.
               ELSE DO:                           
                  IF FLEXTID.TID > TIDREGITAB.START THEN DO:                  
                     ASSIGN
                     spslut = TIDREGITAB.SLUT.
                     TIDREGITAB.SLUT = FLEXTID.TID.
                     RUN andtid_UI.                     
                     CREATE tidbuff.
                     ASSIGN tidbuff.PERSONALKOD = PERSONALTAB.PERSONALKOD
                     tidbuff.START = FLEXTID.TID     
                     tidbuff.PROGRAM = 'GLOMDF' + STRING(TODAY) + STRING(TIME,"HH:MM") + Guru.Konstanter:globanv
                     tidbuff.DATUM = TIDREGITAB.DATUM
                     tidbuff.DAG = TIDREGITAB.DAG 
                     tidbuff.VECKONUMMER = TIDREGITAB.VECKONUMMER
                     tidbuff.TRAKTAMENTE = 0 
                     tidbuff.OVERTIDUTTAG = 'F'
                     tidbuff.AONR = "155" 
                     tidbuff.DELNR = 0 
                     tidbuff.PRISTYP = "FRÅNVARO."
                     tidbuff.PRIS = 0.                  
                     ASSIGN tidbuff.SLUT = spslut.
                     RUN pris2_UI.
                     ASSIGN                  
                     nytid = tidbuff.START.
                     RUN TIMSEK.P.
                     regstartsek = sekunder.
                     nytid = tidbuff.SLUT.
                     RUN TIMSEK.P.
                     regslutsek = sekunder.
                     regdatum = tidbuff.DATUM.                 
                     RUN TOTTID.P.
                     ASSIGN tidbuff.TOTALT = nytid.
                     VALIDATE tidbuff.
                  END.
               END.   
            END.
         END.   
      END. 
      ELSE IF FLEXTID.KNAPP = "Annat in" THEN DO:
         FIND flexbuff WHERE RECID(flexbuff) = flrec NO-LOCK NO-ERROR.   
         FIND PREV flexbuff USE-INDEX FLEX EXCLUSIVE-LOCK NO-ERROR.
         IF AVAILABLE flexbuff THEN DO:
            IF flexbuff.PERSONALKOD = FLEXTID.PERSONALKOD AND 
            flexbuff.DATUM = FLEXTID.DATUM AND flexbuff.KNAPP = "Annat ut" THEN ASSIGN flexbuff.GICK = FALSE.                           
            ELSE IF flexbuff.PERSONALKOD = FLEXTID.PERSONALKOD AND 
            flexbuff.DATUM = FLEXTID.DATUM AND flexbuff.KNAPP = "Lunch ut" THEN ASSIGN flexbuff.GICK = FALSE.                          
            ELSE IF flexbuff.PERSONALKOD = FLEXTID.PERSONALKOD AND 
            flexbuff.DATUM NE FLEXTID.DATUM THEN ASSIGN FLEXTID.KOM = TRUE.                           
            ELSE IF flexbuff.PERSONALKOD NE FLEXTID.PERSONALKOD THEN ASSIGN FLEXTID.KOM = TRUE.                           
         END.  
         ELSE ASSIGN FLEXTID.KOM = TRUE.         
         IF vart = "AND" THEN DO:
            IF AVAILABLE GODKOLL AND GODKOLL.DATUM GE regdatum THEN regdatum = regdatum.
            ELSE DO:  
               FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD
               AND TIDREGITAB.DATUM = regdatum AND TIDREGITAB.TIDLOG = TRUE 
               AND TIDREGITAB.OVERTIDUTTAG = "F" AND TIDREGITAB.SLUT = flexstart
               AND TIDREGITAB.GODKAND = ""
               EXCLUSIVE-LOCK NO-ERROR.
               IF AVAILABLE TIDREGITAB THEN DO:         
                  FIND FIRST tidbuff WHERE tidbuff.PERSONALKOD = PERSONALTAB.PERSONALKOD
                  AND tidbuff.DATUM = regdatum AND tidbuff.TIDLOG = TRUE 
                  AND tidbuff.OVERTIDUTTAG = "F" AND tidbuff.START = flexstart 
                  AND tidbuff.GODKAND = ""
                  EXCLUSIVE-LOCK NO-ERROR.
                  IF AVAILABLE tidbuff THEN DO:        
                     ASSIGN
                     TIDREGITAB.SLUT = FLEXTID.TID.
                     RUN andtid_UI.                     
                     ASSIGN
                     tidbuff.START = FLEXTID.TID.
                     RUN andtid_UI.                     
                  END.                  
               END.   
            END.
         END.   
         IF vart = "NYA" THEN DO:
            IF AVAILABLE GODKOLL AND GODKOLL.DATUM GE regdatum THEN regdatum = regdatum.
            ELSE DO:  
               FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD
               AND TIDREGITAB.DATUM = regdatum AND TIDREGITAB.TIDLOG = TRUE 
               AND TIDREGITAB.START LE FLEXTID.TID AND TIDREGITAB.SLUT GE FLEXTID.TID
               AND TIDREGITAB.OVERTIDUTTAG = "F" AND TIDREGITAB.GODKAND = "" EXCLUSIVE-LOCK NO-ERROR.
               IF NOT AVAILABLE TIDREGITAB THEN DO:         
                  ASSIGN
                  fstart = FLEXDAG.START
                  fslut = FLEXTID.TID.
                  RUN nytid_UI.     
                  ASSIGN
                  fstart = FLEXTID.TID
                  fslut = FLEXDAG.SLUT.
                  RUN nytid_UI.                  
               END.
               ELSE DO:         
                 ASSIGN
                 flkoll = TIDREGITAB.SLUT
                 TIDREGITAB.SLUT = FLEXTID.TID.                     
                 RUN andtid_UI.                     
                 CREATE tidbuff.
                 ASSIGN tidbuff.PERSONALKOD = PERSONALTAB.PERSONALKOD
                 tidbuff.START = FLEXTID.TID     
                 tidbuff.PROGRAM = 'GLOMDF' + STRING(TODAY) + STRING(TIME,"HH:MM") + Guru.Konstanter:globanv
                 tidbuff.DATUM = TIDREGITAB.DATUM
                 tidbuff.DAG = TIDREGITAB.DAG 
                 tidbuff.VECKONUMMER = TIDREGITAB.VECKONUMMER
                 tidbuff.TRAKTAMENTE = AONRTAB.TRAKTAMENTE 
                 tidbuff.OVERTIDUTTAG = 'F'
                 tidbuff.AONR = AONRTAB.AONR 
                 tidbuff.DELNR = AONRTAB.DELNR 
                 tidbuff.PRISTYP = AONRTAB.PRISTYP
                 tidbuff.PRIS = TIMKOSTNADSTAB.PRISA.                         
                 ASSIGN tidbuff.SLUT = flkoll.
                 RUN pris2_UI.
                 ASSIGN                  
                 nytid = tidbuff.START.
                 RUN TIMSEK.P.
                 regstartsek = sekunder.
                 nytid = tidbuff.SLUT.
                 RUN TIMSEK.P.
                 regslutsek = sekunder.
                 regdatum = tidbuff.DATUM.                 
                 RUN TOTTID.P.
                 ASSIGN tidbuff.TOTALT = nytid.
                 VALIDATE tidbuff.
               END.   
            END.
         END.   
      END.      
      ELSE IF FLEXTID.KNAPP = "Annat ut" THEN DO:         
         IF vart = "nya" THEN DO:
            FIND FIRST flexbuff WHERE flexbuff.PERSONALKOD = PERSONALTAB.PERSONALKOD
            AND flexbuff.DATUM = regdatum AND flexbuff.GICK = TRUE USE-INDEX FLEX NO-LOCK NO-ERROR.
            IF AVAILABLE flexbuff THEN DO:
               ASSIGN FLEXTID.GICK = FALSE.   
            END.
            ELSE DO:   
               ASSIGN FLEXTID.GICK = TRUE.
            END.                     
            FIND LAST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD
            AND TIDREGITAB.DATUM = regdatum AND TIDREGITAB.TIDLOG = TRUE 
            AND TIDREGITAB.OVERTIDUTTAG = "F" AND TIDREGITAB.GODKAND = "" USE-INDEX PSTART EXCLUSIVE-LOCK NO-ERROR.
            IF NOT AVAILABLE TIDREGITAB THEN DO:         
               ASSIGN
               fstart = regstart
               fslut =  regslut.
               RUN nytid_UI.               
            END.
            ELSE DO:         
               IF FLEXTID.TID > TIDREGITAB.START THEN DO:                  
                  IF TIDREGITAB.AONR = AONRTAB.AONR AND TIDREGITAB.DELNR = AONRTAB.DELNR  THEN musz = musz.
                  ELSE DO:                       
                     ASSIGN
                     TIDREGITAB.SLUT = FLEXTID.TID.
                     RUN andtid_UI.                     
                     CREATE tidbuff.
                     ASSIGN tidbuff.PERSONALKOD = PERSONALTAB.PERSONALKOD
                     tidbuff.START = FLEXTID.TID     
                     tidbuff.PROGRAM = 'GLOMDF' + STRING(TODAY) + STRING(TIME,"HH:MM") + Guru.Konstanter:globanv
                     tidbuff.DATUM = TIDREGITAB.DATUM
                     tidbuff.DAG = TIDREGITAB.DAG 
                     tidbuff.VECKONUMMER = TIDREGITAB.VECKONUMMER
                     tidbuff.TRAKTAMENTE = AONRTAB.TRAKTAMENTE 
                     tidbuff.OVERTIDUTTAG = 'F'
                     tidbuff.AONR = AONRTAB.AONR 
                     tidbuff.DELNR = AONRTAB.DELNR 
                     tidbuff.PRISTYP = AONRTAB.PRISTYP
                     tidbuff.PRIS = TIMKOSTNADSTAB.PRISA.                         
                     ASSIGN tidbuff.SLUT = regslut.
                     RUN pris2_UI.
                     ASSIGN                  
                     nytid = tidbuff.START.
                     RUN TIMSEK.P.
                     regstartsek = sekunder.
                     nytid = tidbuff.SLUT.
                     RUN TIMSEK.P.
                     regslutsek = sekunder.
                     regdatum = tidbuff.DATUM.                 
                     RUN TOTTID.P.
                     ASSIGN tidbuff.TOTALT = nytid.
                     VALIDATE tidbuff.
                  END.
               END.
            END.                         
         END.
      END.
      IF  CMB_KNAPP = "Annat in" THEN DO:         
         FIND LAST flexbuff WHERE flexbuff.PERSONALKOD = PERSONALTAB.PERSONALKOD
         AND flexbuff.DATUM = regdatum  USE-INDEX FLEX EXCLUSIVE-LOCK NO-ERROR.
         IF AVAILABLE flexbuff AND flexbuff.KNAPP = "Annat ut" THEN DO:
            ASSIGN flexbuff.GICK = FALSE.
         END.         
         FIND LAST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD
         AND TIDREGITAB.DATUM = regdatum AND TIDREGITAB.TIDLOG = TRUE 
         AND TIDREGITAB.OVERTIDUTTAG = "F" AND TIDREGITAB.GODKAND = "" USE-INDEX PSTART EXCLUSIVE-LOCK NO-ERROR.
         IF NOT AVAILABLE TIDREGITAB THEN DO:         
            ASSIGN
            fstart = FLEXDAG.START
            fslut = FLEXDAG.SLUT.
            RUN nytid_UI.            
         END.
      END.
      ELSE IF FLEXTID.KNAPP = "Lunch in" THEN DO:
         FIND flexbuff WHERE RECID(flexbuff) = flrec NO-LOCK NO-ERROR.   
         FIND PREV flexbuff USE-INDEX FLEX EXCLUSIVE-LOCK NO-ERROR.
         IF AVAILABLE flexbuff THEN DO:
            IF flexbuff.PERSONALKOD = FLEXTID.PERSONALKOD AND 
            flexbuff.DATUM = FLEXTID.DATUM AND flexbuff.KNAPP = "Flex ut" THEN DO:
               ASSIGN flexbuff.GICK = FALSE
               nytid = flexbuff.TID.
               RUN TIMSEK.P.
               regstartsek = sekunder.
               nytid = FLEXTID.TID.
               RUN TIMSEK.P.
               regslutsek = sekunder. 
               RUN TOTTID.P.
               ASSIGN FLEXDAG.FLARB = 0 - nytid.
            END.
         END.
         IF vart = "AND" OR vart = "nya" THEN DO:
            IF AVAILABLE GODKOLL AND GODKOLL.DATUM GE regdatum THEN regdatum = regdatum.
            ELSE DO:
               FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD
               AND TIDREGITAB.DATUM = regdatum AND TIDREGITAB.TIDLOG = TRUE 
               AND TIDREGITAB.OVERTIDUTTAG = "F" AND TIDREGITAB.START < flexstart
               AND TIDREGITAB.SLUT > flexstart AND TIDREGITAB.GODKAND = ""
               EXCLUSIVE-LOCK NO-ERROR.
               IF AVAILABLE TIDREGITAB THEN DO:         
                  RUN andtid_UI.                  
               END.   
            END.
         END.   
      END.    
      ELSE IF FLEXTID.KNAPP = "Lunch ut" THEN DO:
         IF vart = "AND" OR vart = "nya" THEN DO:
            IF AVAILABLE GODKOLL AND GODKOLL.DATUM GE regdatum THEN regdatum = regdatum.
            ELSE DO:
               FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD
               AND TIDREGITAB.DATUM = regdatum AND TIDREGITAB.TIDLOG = TRUE 
               AND TIDREGITAB.OVERTIDUTTAG = "F" AND TIDREGITAB.START < flexstart
               AND TIDREGITAB.SLUT > flexstart AND TIDREGITAB.GODKAND = ""
               EXCLUSIVE-LOCK NO-ERROR.
               IF AVAILABLE TIDREGITAB THEN DO:         
                  RUN andtid_UI.
                  
               END.   
            END.
         END.   
      END.    
      RUN SLUTARB.P.
   END.
END PROCEDURE.
PROCEDURE nytid_UI :   
   CREATE TIDREGITAB.
   ASSIGN TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD
   TIDREGITAB.START = fstart     
   TIDREGITAB.SLUT = fslut   
   TIDREGITAB.PROGRAM = 'GLOMDF' + STRING(TODAY) + STRING(TIME,"HH:MM") + Guru.Konstanter:globanv
   TIDREGITAB.DATUM = regdatum
   TIDREGITAB.DAG = regdagnamn
   TIDREGITAB.VECKONUMMER = regvnr
   TIDREGITAB.TRAKTAMENTE = AONRTAB.TRAKTAMENTE
   TIDREGITAB.OVERTIDUTTAG = 'F'
   TIDREGITAB.AONR = AONRTAB.AONR
   TIDREGITAB.DELNR = AONRTAB.DELNR
   TIDREGITAB.PRISTYP = AONRTAB.PRISTYP
   TIDREGITAB.PRIS = TIMKOSTNADSTAB.PRISA.                         
   RUN pris_UI.
   ASSIGN            
   nytid = TIDREGITAB.START.
   RUN TIMSEK.P.
   regstartsek = sekunder.
   nytid = TIDREGITAB.SLUT.
   RUN TIMSEK.P.
   regslutsek = sekunder.
   regdatum = TIDREGITAB.DATUM.               
   RUN TOTTID.P.
   ASSIGN TIDREGITAB.TOTALT = nytid.
   VALIDATE TIDREGITAB.
END PROCEDURE.

PROCEDURE andtid_UI :
   ASSIGN
   TIDREGITAB.PROGRAM = 'GLOMDF' + STRING(TODAY) + STRING(TIME,"HH:MM") + Guru.Konstanter:globanv
   nytid = TIDREGITAB.START.
   RUN TIMSEK.P.
   regstartsek = sekunder.
   nytid = TIDREGITAB.SLUT.
   RUN TIMSEK.P.
   regslutsek = sekunder.
   regdatum = TIDREGITAB.DATUM.                  
   RUN TOTTID.P.
   ASSIGN TIDREGITAB.TOTALT = nytid.
   VALIDATE TIDREGITAB.
END PROCEDURE.

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE flnytid_UI DIALOG-1 
PROCEDURE flnytid_UI :   
   CREATE TIDREGITAB.
   ASSIGN 
   TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD
   TIDREGITAB.START = fstart     
   TIDREGITAB.SLUT = fslut   
   TIDREGITAB.PROGRAM = 'GLOMDF' + STRING(TODAY) + STRING(TIME,"HH:MM") + Guru.Konstanter:globanv
   TIDREGITAB.DATUM = regdatum
   TIDREGITAB.DAG = regdagnamn 
   TIDREGITAB.VECKONUMMER = regvnr
   TIDREGITAB.TRAKTAMENTE = 0 
   TIDREGITAB.OVERTIDUTTAG = 'F'
   TIDREGITAB.AONR = "155" 
   TIDREGITAB.DELNR = 0 
   TIDREGITAB.PRISTYP = "FRÅNVARO."
   TIDREGITAB.PRIS = 0.                         
   RUN pris_UI.
   ASSIGN            
   nytid = TIDREGITAB.START.
   RUN TIMSEK.P.
   regstartsek = sekunder.
   nytid = TIDREGITAB.SLUT.
   RUN TIMSEK.P.
   regslutsek = sekunder.
   regdatum = TIDREGITAB.DATUM.               
   RUN TOTTID.P.
   ASSIGN TIDREGITAB.TOTALT = nytid.
   VALIDATE TIDREGITAB.
END PROCEDURE.

PROCEDURE flandtid_UI :  
  ASSIGN
  TIDREGITAB.PROGRAM = 'GLOMDF' + STRING(TODAY) + STRING(TIME,"HH:MM") + Guru.Konstanter:globanv
  TIDREGITAB.DATUM = regdatum
  TIDREGITAB.DAG = regdagnamn 
  TIDREGITAB.VECKONUMMER = regvnr
  TIDREGITAB.TRAKTAMENTE = 0 
  TIDREGITAB.OVERTIDUTTAG = 'F'
  TIDREGITAB.AONR = "155" 
  TIDREGITAB.DELNR = 0 
  TIDREGITAB.PRISTYP = "FRÅNVARO."
  TIDREGITAB.PRIS = 0.                         
  ASSIGN            
  nytid = TIDREGITAB.START.
  RUN TIMSEK.P.
  regstartsek = sekunder.
  nytid = TIDREGITAB.SLUT.
  RUN TIMSEK.P.
  regslutsek = sekunder.
  regdatum = TIDREGITAB.DATUM.               
  RUN TOTTID.P.
  ASSIGN TIDREGITAB.TOTALT = nytid.
  VALIDATE TIDREGITAB.
END PROCEDURE.
PROCEDURE pris_UI :
   /*PRISFOR*/
   {PRISBEFTYP.I}   
END PROCEDURE.
PROCEDURE pris2_UI :
   IF Guru.Konstanter:varforetypval[4] = 1 THEN DO:         
      FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = tidbuff.PERSONALKOD
      NO-LOCK NO-ERROR.
      FIND FIRST BEFATTNINGSTAB WHERE BEFATTNINGSTAB.BEFATTNING = PERSONALTAB.BEFATTNING
      NO-LOCK NO-ERROR.
      tidbuff.OVERTIDTILL = BEFATTNINGSTAB.BEFATTNING.
      IF tidbuff.PRISTYP = "FRÅNVARO." OR tidbuff.PRISTYP = "RESTID..." THEN DO:
         FIND LAST PERSONALPRIS WHERE PERSONALPRIS.PERSONALKOD = tidbuff.PERSONALKOD AND
         PERSONALPRIS.BEFATTNING = tidbuff.PRISTYP AND 
         PERSONALPRIS.STARTDATUM <= tidbuff.DATUM AND 
         PERSONALPRIS.SLUTDATUM >= tidbuff.DATUM     
         NO-LOCK NO-ERROR.
         IF AVAILABLE PERSONALPRIS THEN tidbuff.PRIS = PERSONALPRIS.PRIS.  
         ELSE DO:
            {SOKSTART.I}
            ASSIGN
            soktemp.SOKVAL = 1
            soktemp.SOKINT[1] = Guru.Konstanter:varforetypval[4]
            soktemp.SOKCHAR[2] = tidbuff.PERSONALKOD
            soktemp.SOKCHAR[3] = tidbuff.PRISTYP
            soktemp.SOKCHAR[4] = tidbuff.OVERTIDTILL 
            soktemp.SOKDATE[1] = tidbuff.DATUM.
            {SOKANROP.I}
            tidbuff.PRIS = soktemp.SOKDECI[1].
         END.
      END.
      ELSE IF (globforetag = "sund" OR globforetag = "SNAT" OR globforetag = "MISV") AND tidbuff.PRISTYP = "EJ.KOSTN."  THEN DO:
         FIND LAST PERSONALPRIS WHERE PERSONALPRIS.PERSONALKOD = tidbuff.PERSONALKOD AND
         PERSONALPRIS.BEFATTNING = tidbuff.PRISTYP AND 
         PERSONALPRIS.STARTDATUM <= tidbuff.DATUM AND 
         PERSONALPRIS.SLUTDATUM >= tidbuff.DATUM     
         NO-LOCK NO-ERROR.
         IF AVAILABLE PERSONALPRIS THEN tidbuff.PRIS = PERSONALPRIS.PRIS.  
         ELSE DO:
            {SOKSTART.I}
            ASSIGN
            soktemp.SOKVAL = 1
            soktemp.SOKINT[1] = Guru.Konstanter:varforetypval[4]
            soktemp.SOKCHAR[2] = tidbuff.PERSONALKOD
            soktemp.SOKCHAR[3] = tidbuff.PRISTYP
            soktemp.SOKCHAR[4] = tidbuff.OVERTIDTILL 
            soktemp.SOKDATE[1] = tidbuff.DATUM.
            {SOKANROP.I}
            tidbuff.PRIS = soktemp.SOKDECI[1].
         END.
      END.
      ELSE IF globforetag = "ELPA" AND tidbuff.PRISTYP = "EJ.KOSTN."  THEN DO:
         FIND LAST PERSONALPRIS WHERE PERSONALPRIS.PERSONALKOD = tidbuff.PERSONALKOD AND
         PERSONALPRIS.BEFATTNING = tidbuff.PRISTYP AND 
         PERSONALPRIS.STARTDATUM <= tidbuff.DATUM AND 
         PERSONALPRIS.SLUTDATUM >= tidbuff.DATUM     
         NO-LOCK NO-ERROR.
         IF AVAILABLE PERSONALPRIS THEN tidbuff.PRIS = PERSONALPRIS.PRIS.  
         ELSE DO:
            {SOKSTART.I}
            ASSIGN
            soktemp.SOKVAL = 1
            soktemp.SOKINT[1] = Guru.Konstanter:varforetypval[4]
            soktemp.SOKCHAR[2] = tidbuff.PERSONALKOD
            soktemp.SOKCHAR[3] = tidbuff.PRISTYP
            soktemp.SOKCHAR[4] = tidbuff.OVERTIDTILL 
            soktemp.SOKDATE[1] = tidbuff.DATUM.
            {SOKANROP.I}
            tidbuff.PRIS = soktemp.SOKDECI[1].
         END.
      END.
      ELSE IF (globforetag = "sund" OR globforetag = "SNAT" OR globforetag = "MISV") AND tidbuff.PRISTYP = "FASTPRIS1"  THEN DO:
         FIND LAST PERSONALPRIS WHERE PERSONALPRIS.PERSONALKOD = tidbuff.PERSONALKOD AND
         PERSONALPRIS.BEFATTNING = tidbuff.PRISTYP AND 
         PERSONALPRIS.STARTDATUM <= tidbuff.DATUM AND 
         PERSONALPRIS.SLUTDATUM >= tidbuff.DATUM     
         NO-LOCK NO-ERROR.
         IF AVAILABLE PERSONALPRIS THEN tidbuff.PRIS = PERSONALPRIS.PRIS.  
         ELSE DO:
            {SOKSTART.I}
            ASSIGN
            soktemp.SOKVAL = 1
            soktemp.SOKINT[1] = Guru.Konstanter:varforetypval[4]
            soktemp.SOKCHAR[2] = tidbuff.PERSONALKOD
            soktemp.SOKCHAR[3] = tidbuff.PRISTYP
            soktemp.SOKCHAR[4] = tidbuff.OVERTIDTILL 
            soktemp.SOKDATE[1] = tidbuff.DATUM.
            {SOKANROP.I}
            tidbuff.PRIS = soktemp.SOKDECI[1].
         END.
      END.
      ELSE IF globforetag = "ELPA" AND tidbuff.PRISTYP = "FASTPRIS1"  THEN DO:
         FIND LAST PERSONALPRIS WHERE PERSONALPRIS.PERSONALKOD = tidbuff.PERSONALKOD AND
         PERSONALPRIS.BEFATTNING = tidbuff.PRISTYP AND 
         PERSONALPRIS.STARTDATUM <= tidbuff.DATUM AND 
         PERSONALPRIS.SLUTDATUM >= tidbuff.DATUM     
         NO-LOCK NO-ERROR.
         IF AVAILABLE PERSONALPRIS THEN tidbuff.PRIS = PERSONALPRIS.PRIS.  
         ELSE DO:
            {SOKSTART.I}
            ASSIGN
            soktemp.SOKVAL = 1
            soktemp.SOKINT[1] = Guru.Konstanter:varforetypval[4]
            soktemp.SOKCHAR[2] = tidbuff.PERSONALKOD
            soktemp.SOKCHAR[3] = tidbuff.PRISTYP
            soktemp.SOKCHAR[4] = tidbuff.OVERTIDTILL 
            soktemp.SOKDATE[1] = tidbuff.DATUM.
            {SOKANROP.I}
            tidbuff.PRIS = soktemp.SOKDECI[1].
         END.
      END.
      ELSE DO:
         FIND LAST PERSONALPRIS WHERE PERSONALPRIS.PERSONALKOD = tidbuff.PERSONALKOD AND
         PERSONALPRIS.BEFATTNING = BEFATTNINGSTAB.BEFATTNING AND 
         PERSONALPRIS.STARTDATUM <= tidbuff.DATUM 
         AND PERSONALPRIS.SLUTDATUM >= tidbuff.DATUM 
         NO-LOCK NO-ERROR.
         IF AVAILABLE PERSONALPRIS THEN tidbuff.PRIS = PERSONALPRIS.PRIS.  
         ELSE DO:
            {SOKSTART.I}
            ASSIGN
            soktemp.SOKVAL = 1
            soktemp.SOKINT[1] = Guru.Konstanter:varforetypval[4]
            soktemp.SOKCHAR[2] = tidbuff.PERSONALKOD
            soktemp.SOKCHAR[3] = tidbuff.PRISTYP
            soktemp.SOKCHAR[4] = tidbuff.OVERTIDTILL 
            soktemp.SOKDATE[1] = tidbuff.DATUM.
            {SOKANROP.I}
            tidbuff.PRIS = soktemp.SOKDECI[1].
         END.
      END.
   END.  
END PROCEDURE.

{AVAFOR.I}
