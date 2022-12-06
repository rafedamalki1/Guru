/*TIDBORT.P*/
{APP.I}
&Scoped-define NEW NEW
{TIDALLT.I}
DEFINE INPUT PARAMETER ganv AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER vadgora AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER dbrwbdatum AS DATE NO-UNDO.
DEFINE INPUT PARAMETER dbrwavdatum AS DATE NO-UNDO.
DEFINE INPUT PARAMETER TABLE FOR extratidallt.
DEFINE OUTPUT PARAMETER placerarec AS RECID.
DEFINE OUTPUT PARAMETER TABLE FOR tidallt.
DEFINE NEW SHARED VARIABLE brwbdatum AS DATE NO-UNDO.
DEFINE NEW SHARED VARIABLE brwavdatum AS DATE NO-UNDO.
DEFINE VARIABLE energiavt AS LOGICAL NO-UNDO.
DEFINE VARIABLE regdatumspar AS DATE NO-UNDO.  
DEFINE VARIABLE regdatumhj AS DATE NO-UNDO. 
DEFINE VARIABLE tidtabrecspar AS RECID NO-UNDO.
DEFINE VARIABLE nyber AS INTEGER NO-UNDO.
DEFINE VARIABLE ostart AS DECIMAL NO-UNDO. 
DEFINE VARIABLE krav5 AS INTEGER NO-UNDO. 
DEFINE VARIABLE krav6 AS INTEGER NO-UNDO.
DEFINE VARIABLE buffvar AS RECID NO-UNDO.
DEFINE VARIABLE spextraaonr AS CHARACTER NO-UNDO.
DEFINE VARIABLE spextradnr AS INTEGER NO-UNDO. 
DEFINE VARIABLE arbnr AS CHARACTER NO-UNDO.
DEFINE VARIABLE hjdat AS DATE NO-UNDO.
DEFINE NEW SHARED VARIABLE extraaonr LIKE TIDREGITAB.AONR NO-UNDO.
DEFINE NEW SHARED VARIABLE extradnr LIKE TIDREGITAB.DELNR NO-UNDO.
FIND FIRST FORETAG NO-LOCK NO-ERROR.
ASSIGN
brwbdatum = dbrwbdatum 
brwavdatum = dbrwavdatum
Guru.Konstanter:globforetag = FORETAG.FORETAG.
FIND FIRST extratidallt NO-ERROR.
FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = extratidallt.PERSONALKOD NO-LOCK NO-ERROR.
persrec = RECID(PERSONALTAB).
FIND FIRST TIDREGITAB WHERE RECID(TIDREGITAB) = extratidallt.RECTIDVIS NO-ERROR.
IF NOT AVAILABLE TIDREGITAB THEN RETURN.
tidtabrec = RECID(TIDREGITAB).
arbnr = TIDREGITAB.AONR.

IF vadgora = 1  THEN DO:
   RUN borttid_UI.
END.
IF vadgora = 11 THEN DO:
   RUN borttidflex_UI.
END.
IF vadgora = 2 THEN DO:
   RUN bortlon_UI.
END.
IF vadgora = 3 THEN DO:
   RUN bortber_UI.
END.
IF vadgora = 31 THEN DO:
   RUN bortberp_UI.
END.
IF vadgora = 4 THEN DO:
   RUN borttra_UI.
END.
RUN coptider_UI.
PROCEDURE borttra_UI:
   DO TRANSACTION:
      FIND TIDREGITAB WHERE RECID(TIDREGITAB) = tidtabrec NO-LOCK NO-ERROR.
      FIND NEXT TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
      TIDREGITAB.DATUM >= brwbdatum AND TIDREGITAB.DATUM <= brwavdatum AND 
      TIDREGITAB.TRAKTKOD NE '' USE-INDEX PSTART NO-LOCK NO-ERROR.  
      IF AVAILABLE TIDREGITAB THEN tidtabrec2 = RECID(TIDREGITAB).
      ELSE DO:
         FIND TIDREGITAB WHERE RECID(TIDREGITAB) = tidtabrec NO-LOCK NO-ERROR.
         FIND PREV TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
         TIDREGITAB.DATUM >= brwbdatum AND TIDREGITAB.DATUM <= brwavdatum AND 
         TIDREGITAB.TRAKTKOD NE '' USE-INDEX PSTART NO-LOCK NO-ERROR.  
         IF AVAILABLE TIDREGITAB THEN tidtabrec2 = RECID(TIDREGITAB).     
         ELSE tidtabrec2 = tidtabrec.    
      END.
      FIND TIDREGITAB WHERE RECID(TIDREGITAB) = tidtabrec EXCLUSIVE-LOCK NO-ERROR. 
      regdatum = TIDREGITAB.DATUM.     
      regdatumspar = TIDREGITAB.DATUM.     
      IF TIDREGITAB.TIDLOG = FALSE THEN DO:      
         DELETE TIDREGITAB.
      END.
      ELSE DO:
         ASSIGN 
         TIDREGITAB.TRAKTKOD = "" 
         TIDREGITAB.TRAKTANTAL = 0
         TIDREGITAB.TRAKTAUTO = FALSE
         SUBSTRING(TIDREGITAB.PROGRAM,1,158) = "BORTTRAK" + STRING(TODAY) + STRING(TIME,"HH:MM") + Guru.Konstanter:globanv.
         
      END.         
      FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
      TIDREGITAB.DATUM = regdatum AND TIDREGITAB.TRAKTKOD NE '' 
      USE-INDEX PSTART NO-LOCK NO-ERROR.
      IF NOT AVAILABLE TIDREGITAB THEN DO:
         FOR EACH TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
         TIDREGITAB.DATUM = regdatum USE-INDEX PSTART EXCLUSIVE-LOCK:
            ASSIGN TIDREGITAB.TRAKTAUTO = TRUE.
         END.   
         FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
         TIDREGITAB.DATUM = regdatum AND TIDREGITAB.TIDLOG = TRUE     
         USE-INDEX PSTART NO-LOCK NO-ERROR.
         IF NOT AVAILABLE TIDREGITAB THEN persrec = persrec.
         ELSE DO:
            tidtabrecspar = tidtabrec.
            tidtabrec = RECID(TIDREGITAB).                        
            RUN TRAKTBER.P.
            IF TIDREGITAB.TRAKTKOD = "" THEN tidtabrec = tidtabrecspar.
            ELSE DO:
               tidtabrec2 = tidtabrec.
               tidtabrec = 0.
            END.
            REPEAT:
               FIND NEXT TIDREGITAB WHERE 
               TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
               TIDREGITAB.DATUM = regdatum AND TIDREGITAB.TIDLOG = TRUE
               USE-INDEX PSTART NO-LOCK NO-ERROR.
               IF NOT AVAILABLE TIDREGITAB THEN LEAVE.
               ELSE DO:
                  tidtabrecspar = tidtabrec.
                  tidtabrec = RECID(TIDREGITAB).
                  RUN TRAKTBER.P.
                  IF TIDREGITAB.TRAKTKOD = "" THEN tidtabrec = tidtabrecspar.
               END.
            END.
         END. 
         FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
         TIDREGITAB.DATUM = regdatumspar AND TIDREGITAB.TRAKTKOD NE '' 
         USE-INDEX PSTART NO-LOCK NO-ERROR.
         IF AVAILABLE TIDREGITAB THEN DO:
            tidtabrec2 = RECID(TIDREGITAB).
            tidtabrec = 0.
         END.    
      END.      
      placerarec = tidtabrec2.            
   END.
END PROCEDURE.
PROCEDURE bortlon_UI:
   DO TRANSACTION:
      FIND TIDREGITAB WHERE RECID(TIDREGITAB) = tidtabrec NO-LOCK NO-ERROR.
      regdatum = TIDREGITAB.DATUM.     
      FIND NEXT TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
      TIDREGITAB.DATUM >= brwbdatum AND TIDREGITAB.DATUM <= brwavdatum AND 
      TIDREGITAB.LONTILLAGG NE '' USE-INDEX PSTART NO-LOCK NO-ERROR.  
      IF AVAILABLE TIDREGITAB THEN tidtabrec2 = RECID(TIDREGITAB).
      ELSE DO:
         FIND TIDREGITAB WHERE RECID(TIDREGITAB) = tidtabrec NO-LOCK NO-ERROR.
         FIND PREV TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
         TIDREGITAB.DATUM >= brwbdatum AND TIDREGITAB.DATUM <= brwavdatum AND 
         TIDREGITAB.LONTILLAGG NE '' USE-INDEX PSTART NO-LOCK NO-ERROR.  
         IF AVAILABLE TIDREGITAB THEN tidtabrec2 = RECID(TIDREGITAB).     
         ELSE tidtabrec2 = tidtabrec.    
      END.    
      FIND TIDREGITAB WHERE RECID(TIDREGITAB) = tidtabrec EXCLUSIVE-LOCK NO-ERROR. 
      IF TIDREGITAB.TIDLOG = FALSE THEN DO:
         DELETE TIDREGITAB.
         FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
         TIDREGITAB.DATUM = regdatum AND TIDREGITAB.LONAUTO = FALSE
         USE-INDEX PSTART NO-LOCK NO-ERROR.
         IF NOT AVAILABLE TIDREGITAB THEN DO:
            persrec = persrec.
         END.
         ELSE DO:
            FIND FIRST LONKORT WHERE LONKORT.LONTILLAGG = LONTILL.LONTILLAGG AND
            LONKORT.KORTLON = TIDREGITAB.LONTILLAGG USE-INDEX LONKORT NO-LOCK NO-ERROR.
            IF NOT AVAILABLE LONKORT THEN DO:
               persrec = persrec.
            END.
         END.
      END.
      ELSE DO:
         ASSIGN 
         TIDREGITAB.LONTILLAGG = ''
         TIDREGITAB.LONTILLANTAL = 0
         TIDREGITAB.LONAUTO = FALSE 
         SUBSTRING(TIDREGITAB.PROGRAM,1,158) = "BORTLON" + STRING(TODAY) + STRING(TIME,"HH:MM") + Guru.Konstanter:globanv.
      END.                      
   END.
   placerarec = tidtabrec2.      
   
END PROCEDURE.
PROCEDURE bortber_UI:
   FIND TIDREGITAB WHERE RECID(TIDREGITAB) = tidtabrec NO-LOCK NO-ERROR.
   DO TRANSACTION:  
      FIND NEXT TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
      TIDREGITAB.DATUM >= brwbdatum AND TIDREGITAB.DATUM <= brwavdatum AND  
      TIDREGITAB.BEREDSKAP NE '' NO-LOCK NO-ERROR.  
      IF AVAILABLE TIDREGITAB THEN tidtabrec2 = RECID(TIDREGITAB).
      ELSE DO:
         FIND TIDREGITAB WHERE RECID(TIDREGITAB) = tidtabrec NO-LOCK NO-ERROR.
         FIND PREV TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
         TIDREGITAB.DATUM >= brwbdatum AND TIDREGITAB.DATUM <= brwavdatum AND 
         TIDREGITAB.BEREDSKAP NE ''
         NO-LOCK NO-ERROR.  
         IF AVAILABLE TIDREGITAB THEN tidtabrec2 = RECID(TIDREGITAB).     
         ELSE tidtabrec2 = tidtabrec.    
      END. 
      FIND TIDREGITAB WHERE RECID(TIDREGITAB) = tidtabrec EXCLUSIVE-LOCK NO-ERROR.
      IF TIDREGITAB.TIDLOG = FALSE THEN DO:
         DELETE TIDREGITAB.
      END.
      ELSE DO:
         ASSIGN TIDREGITAB.BEREDSKAP = "" TIDREGITAB.BERANTAL = 0.
      END.
      placerarec = tidtabrec2.
      ASSIGN
      regdatum = brwbdatum
      bdatum = brwbdatum
      avdatum = bdatum.
      RUN BEROVER.P.                    
   END.
END PROCEDURE.
PROCEDURE bortberp_UI:
   FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
   YEAR(TIDREGITAB.DATUM) = YEAR(brwbdatum) AND
   MONTH(TIDREGITAB.DATUM) = MONTH(brwbdatum) AND
   TIDREGITAB.DATUM > brwavdatum AND 
   TIDREGITAB.BEREDSKAP NE '' 
   USE-INDEX PKOD NO-LOCK NO-ERROR.  
   IF AVAILABLE TIDREGITAB THEN DO:
      ASSIGN
      tidtabrec2 = RECID(TIDREGITAB)
      tidtabrec = 0.            
   END.
   ELSE DO:
      FIND LAST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
      YEAR(TIDREGITAB.DATUM) = YEAR(brwbdatum) AND
      MONTH(TIDREGITAB.DATUM) = MONTH(brwbdatum) AND
      TIDREGITAB.DATUM < brwbdatum AND 
      TIDREGITAB.BEREDSKAP NE '' 
      USE-INDEX PKOD NO-LOCK NO-ERROR.  
      IF AVAILABLE TIDREGITAB THEN DO: 
         ASSIGN
         tidtabrec2 = RECID(TIDREGITAB)
         tidtabrec = 0.               
      END.     
      ELSE tidtabrec2 = tidtabrec.    
   END.
   placerarec = tidtabrec2.
   OPEN QUERY tidq FOR EACH TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
   TIDREGITAB.DATUM >= brwbdatum AND TIDREGITAB.DATUM <= brwavdatum AND
   TIDREGITAB.BEREDSKAP NE '' USE-INDEX PKOD NO-LOCK.
   GET FIRST tidq NO-LOCK.   
   DO WHILE AVAILABLE(TIDREGITAB):
      DO TRANSACTION:             
         GET CURRENT tidq EXCLUSIVE-LOCK.
         IF TIDREGITAB.TIDLOG = FALSE THEN DO:
            DELETE TIDREGITAB.
         END.
         ELSE DO:
            ASSIGN TIDREGITAB.BEREDSKAP = "" TIDREGITAB.BERANTAL = 0.
         END.          
      END.
      GET NEXT tidq NO-LOCK.   
   END.
   ASSIGN
   regdatum = brwbdatum
   bdatum = brwbdatum
   avdatum = brwavdatum.
   RUN BEROVER.P.   
END PROCEDURE.
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
PROCEDURE borttid_UI:
   energiavt = FALSE.
   FIND FIRST ANSTFORMTAB WHERE ANSTFORMTAB.ANSTALLNING = PERSONALTAB.ANSTALLNING
   USE-INDEX ANSTF NO-LOCK NO-ERROR.
   IF (Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV") THEN ASSIGN energiavt = TRUE.
   IF Guru.Konstanter:globforetag = "GKAL" THEN ASSIGN energiavt = TRUE.
   IF Guru.Konstanter:globforetag = "LULE" THEN ASSIGN energiavt = TRUE.
   FIND FIRST UTRYCKNING WHERE UTRYCKNING.KOD = ANSTFORMTAB.KOD
   USE-INDEX UT NO-LOCK NO-ERROR.
   FIND TIDREGITAB WHERE RECID(TIDREGITAB) = tidtabrec NO-LOCK NO-ERROR.
   DO TRANSACTION:                   
      FIND NEXT TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
      TIDREGITAB.DATUM >= brwbdatum AND TIDREGITAB.DATUM <= brwavdatum AND 
      TIDREGITAB.TIDLOG = TRUE
      USE-INDEX PSTART NO-LOCK NO-ERROR. 
      IF AVAILABLE TIDREGITAB THEN tidtabrec2 = RECID(TIDREGITAB).
      ELSE DO:
         FIND TIDREGITAB WHERE RECID(TIDREGITAB) = tidtabrec NO-LOCK NO-ERROR.
         FIND PREV TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
         TIDREGITAB.DATUM >= brwbdatum AND TIDREGITAB.DATUM <= brwavdatum AND 
         TIDREGITAB.TIDLOG = TRUE
         USE-INDEX PSTART NO-LOCK NO-ERROR.  
         IF AVAILABLE TIDREGITAB THEN tidtabrec2 = RECID(TIDREGITAB).     
         ELSE tidtabrec2 = tidtabrec.    
      END.     
      placerarec = tidtabrec2.
      FIND TIDREGITAB WHERE RECID(TIDREGITAB) = tidtabrec EXCLUSIVE-LOCK NO-ERROR.
      ASSIGN
      tidtabrecspar = tidtabrec
      sekunder = 0
      regdatumspar = TIDREGITAB.DATUM
      regdatum = TIDREGITAB.DATUM
      regvnr = TIDREGITAB.VECKONUMMER
      extraaonr = TIDREGITAB.AONR
      extradnr = TIDREGITAB.DELNR.      
      RUN SLUTARB.P.            
      ASSIGN
      ostart = TIDREGITAB.START
      krav5 = 0   
      krav6 = 0.
  
      IF TIDREGITAB.OSL1 = 24.00 AND TIDREGITAB.OSL2 > 0 THEN krav5 = 1.
      IF TIDREGITAB.OKOD1 NE "" THEN DO:
         nytid = TIDREGITAB.SLUT.  
         buffvar = TIDREGITAB.RECTIDVIS.
         DELETE TIDREGITAB.        
         IF buffvar NE ? THEN DO:         
            FIND FIRST TIDREGITAB WHERE TIDREGITAB.RECTIDVIS = buffvar
            EXCLUSIVE-LOCK NO-ERROR.
            IF AVAILABLE TIDREGITAB THEN DELETE TIDREGITAB.
         END.
      END. 
      ELSE DELETE TIDREGITAB.            
      IF Guru.Konstanter:varforetypval[30] = 1 THEN DO:
         /*ob-sjuk*/
         IF arbnr = "110" THEN RUN OBBERSJ.P.
         ELSE RUN OBBER.P.
      END.
      ELSE RUN OBBER.P.
         /*dagen före*/
      FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND 
      TIDREGITAB.DATUM = regdatum - 1 AND TIDREGITAB.TIDLOG = TRUE
      USE-INDEX PSTART NO-LOCK NO-ERROR.
      IF AVAILABLE TIDREGITAB THEN DO:
         ASSIGN
         spextraaonr =  extraaonr
         spextradnr =   extradnr  
         extraaonr = TIDREGITAB.AONR
         extradnr = TIDREGITAB.DELNR
         regdatum = regdatum - 1
         bustart3 = TIDREGITAB.START.         
         IF Guru.Konstanter:varforetypval[30] = 1 THEN DO:
            /*ob-sjuk*/
            IF arbnr = "110" THEN RUN OBBERSJ.P.
            ELSE RUN OBBER.P.
         END.
         ELSE RUN OBBER.P.
         ASSIGN
         regdatum = regdatumspar
         extraaonr =  spextraaonr
         extradnr =  spextradnr.  
      END.   
      IF ostart < regstart THEN DO:
         FIND LAST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND 
         TIDREGITAB.DATUM = regdatum - 1 AND TIDREGITAB.TIDLOG = TRUE AND
         TIDREGITAB.START > regslut USE-INDEX PSTART NO-LOCK NO-ERROR.
         IF AVAILABLE TIDREGITAB THEN DO:
            IF TIDREGITAB.OSL1 = 24.00 AND TIDREGITAB.OSL2 > 0 THEN krav6 = 1.
         END.  
      END.  
      /*dagen */                                        
      regdatumhj = regdatum.
      IF ostart < regstart THEN DO:
         FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND 
         TIDREGITAB.DATUM = regdatum AND TIDREGITAB.TIDLOG = TRUE AND
         TIDREGITAB.START < regstart USE-INDEX PSTART NO-LOCK NO-ERROR.
         IF krav6 = 1 THEN DO:
           IF NOT AVAILABLE TIDREGITAB THEN DO: 
             krav6 = 0.
             FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND 
             TIDREGITAB.DATUM = regdatum - 1 AND TIDREGITAB.TIDLOG = TRUE AND
             TIDREGITAB.OSL1 = 24.00 USE-INDEX PSTART NO-LOCK NO-ERROR.
             IF AVAILABLE TIDREGITAB THEN regdatumhj = regdatum - 1.
           END.
         END.    
      END.  
      regdatum = regdatumspar.
      RUN SLUTARB.P.     
      FIND FIRST BEREDSKAPTAB WHERE 
      BEREDSKAPTAB.BEREDSKAPSAVTAL = PERSONALTAB.BEREDSKAPSAVTAL
      USE-INDEX BERED NO-LOCK NO-ERROR. 
      IF AVAILABLE BEREDSKAPTAB THEN DO:
         IF BEREDSKAPTAB.BERANTAL > 0 THEN persrec = persrec.
         ELSE IF UTRYCKNING.AVBE = FALSE THEN persrec = persrec.
         ELSE DO:                                   
            IF ostart < regstart THEN DO:
               FOR EACH TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND 
               TIDREGITAB.DATUM = regdatum AND TIDREGITAB.BEREDSKAP NE ''
               AND TIDREGITAB.BEREDSKAPSLUT < regslut
               USE-INDEX PSTART EXCLUSIVE-LOCK:
                  nytid = TIDREGITAB.BEREDSKAPSLUT.
                  RUN TIMSEK.P.
                  ASSIGN
                  nyber = sekunder
                  nytid = TIDREGITAB.BEREDSKAPSTART.
                  RUN TIMSEK.P.
                  sekunder = nyber - sekunder.
                  RUN SEKTIM.P.
                  ASSIGN TIDREGITAB.BERANTAL = nytid.
                  VALIDATE TIDREGITAB.
               END. 
            END.     
            ELSE IF ostart GE regslut THEN DO:
               FOR EACH TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND 
               TIDREGITAB.DATUM = regdatum AND TIDREGITAB.BEREDSKAP NE ''
               AND TIDREGITAB.BEREDSKAPSLUT GE regslut
               USE-INDEX PSTART EXCLUSIVE-LOCK:
                  nytid = TIDREGITAB.BEREDSKAPSLUT.
                  RUN TIMSEK.P.   
                  ASSIGN
                  nyber = sekunder
                  nytid = TIDREGITAB.BEREDSKAPSTART.
                  RUN TIMSEK.P.
                  sekunder = nyber - sekunder.
                  RUN SEKTIM.P.
                  ASSIGN TIDREGITAB.BERANTAL = nytid.
                  VALIDATE TIDREGITAB.
               END.   
            END.
         END.    
      END.      
      IF ostart GE regslut THEN DO:
         IF PERSONALTAB.DELTID = TRUE THEN DO:
            FIND FIRST ORDARB WHERE ORDARB.ANSTALLNING = PERSONALTAB.ANSTALLNING 
            USE-INDEX ORDARB NO-LOCK NO-ERROR.
            sekunder = ORDARB.STOPP1.
            RUN SEKTIM.P.
            FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND 
            TIDREGITAB.DATUM = regdatum AND TIDREGITAB.TIDLOG = TRUE AND
            TIDREGITAB.START GE nytid USE-INDEX PSTART NO-LOCK NO-ERROR.           
         END.
         ELSE DO:         
            FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND 
            TIDREGITAB.DATUM = regdatum AND TIDREGITAB.TIDLOG = TRUE AND
            TIDREGITAB.START GE regslut USE-INDEX PSTART NO-LOCK NO-ERROR.
         END.
      END.    
      ELSE IF energiavt = TRUE THEN DO:         
      /* OM DET FINNS TID EFTER ORDINARIE ARBETSTID TOLKA OM TILL ENKEL ÖVERTID*/   
         IF ostart LE regstart AND ostart ge ( regstart - 2 ) THEN DO:
            FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND 
            TIDREGITAB.DATUM = regdatum AND TIDREGITAB.TIDLOG = TRUE AND
            TIDREGITAB.START GE regslut AND TIDREGITAB.START < ( regslut + 2 ) USE-INDEX PSTART NO-LOCK NO-ERROR.
         END.
      END.  
      IF AVAILABLE TIDREGITAB THEN DO:
         bustart3 = TIDREGITAB.START.         
         regdatum = TIDREGITAB.DATUM.
         IF Guru.Konstanter:varforetypval[30] = 1 THEN DO:
            /*ob-sjuk*/
            IF arbnr = "110" THEN RUN OBBERSJ.P.
            ELSE RUN OBBER.P.
         END.
         ELSE RUN OBBER.P.  
         RUN OTOLKPR.P.
      END.   
      regdatum = regdatumspar.
        /*dagen efter*/
      FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND 
      TIDREGITAB.DATUM = regdatum + 1 AND TIDREGITAB.TIDLOG = TRUE
      USE-INDEX PSTART NO-LOCK NO-ERROR.
      IF AVAILABLE TIDREGITAB THEN DO:          
         ASSIGN 
         spextraaonr =  extraaonr
         spextradnr =   extradnr  
         extraaonr = TIDREGITAB.AONR
         extradnr = TIDREGITAB.DELNR
         bustart3 = TIDREGITAB.START
         regdatum = regdatum + 1.         
         IF Guru.Konstanter:varforetypval[30] = 1 THEN DO:
            /*ob-sjuk*/
            IF arbnr = "110" THEN RUN OBBERSJ.P.
            ELSE RUN OBBER.P.
         END.
         ELSE RUN OBBER.P.
         ASSIGN
         regdatum = regdatumspar
         extraaonr =  spextraaonr
         extradnr =  spextradnr.  
      END.      
      FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
      TIDREGITAB.DATUM = regdatum - 1 AND TIDREGITAB.TIDLOG = TRUE  
      USE-INDEX PSTART NO-LOCK NO-ERROR.
      IF AVAILABLE TIDREGITAB THEN DO:          
         ASSIGN
         regdatum = TIDREGITAB.DATUM
         tidtabrec = RECID(TIDREGITAB).
         RUN TRAKTBER.P.            
         ASSIGN
         tidtabrec = tidtabrecspar
         regdatum = regdatumspar.
      END.
      FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
      TIDREGITAB.DATUM = regdatum AND TIDREGITAB.TIDLOG = TRUE
      USE-INDEX PSTART NO-LOCK NO-ERROR.
      IF AVAILABLE TIDREGITAB THEN DO:         
         ASSIGN
         regdatum = TIDREGITAB.DATUM
         tidtabrec = RECID(TIDREGITAB).
         RUN TRAKTBER.P.            
         ASSIGN
         tidtabrec = tidtabrecspar
         regdatum = regdatumspar.
      END.
      FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
      TIDREGITAB.DATUM = regdatum + 1 AND TIDREGITAB.TIDLOG = TRUE
      USE-INDEX PSTART NO-LOCK NO-ERROR.
      IF AVAILABLE TIDREGITAB THEN DO:        
         ASSIGN
         regdatum = TIDREGITAB.DATUM
         tidtabrec = RECID(TIDREGITAB).
         RUN TRAKTBER.P.            
         ASSIGN
         tidtabrec = tidtabrecspar
         regdatum = regdatumspar.
      END.      
      IF krav5 = 1 THEN DO:
         krav5 = 0.
         FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND 
         TIDREGITAB.DATUM = regdatum + 1 AND TIDREGITAB.TIDLOG = TRUE AND
         TIDREGITAB.START < regstart USE-INDEX PSTART NO-LOCK NO-ERROR.
         IF AVAILABLE TIDREGITAB THEN DO:
            bustart3 = TIDREGITAB.START.            
            IF Guru.Konstanter:varforetypval[30] = 1 THEN DO:
               /*ob-sjuk*/
               IF arbnr = "110" THEN RUN OBBERSJ.P.
               ELSE RUN OBBER.P.
            END.
            ELSE RUN OBBER.P.
            regdatum = regdatumspar + 1.
            RUN OTOLKPR.P.
         END.
         ELSE DO: 
            IF AVAILABLE BEREDSKAPTAB THEN DO:
               IF BEREDSKAPTAB.BERANTAL > 0 THEN persrec = persrec.
               ELSE IF UTRYCKNING.AVBE = FALSE THEN persrec = persrec.
               ELSE DO: 
                  FOR EACH TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND 
                  TIDREGITAB.DATUM = regdatum + 1 AND TIDREGITAB.BEREDSKAP NE ''
                  AND TIDREGITAB.BEREDSKAPSLUT < regslut USE-INDEX PSTART EXCLUSIVE-LOCK:
                     nytid = TIDREGITAB.BEREDSKAPSLUT.
                     RUN TIMSEK.P.
                     ASSIGN
                     nyber = sekunder
                     nytid = TIDREGITAB.BEREDSKAPSTART.
                     RUN TIMSEK.P.
                     sekunder = nyber - sekunder.
                     RUN SEKTIM.P.
                     ASSIGN TIDREGITAB.BERANTAL = nytid.
                     VALIDATE TIDREGITAB.
                  END.
               END.
            END.  
         END. 
      END.     
      RETURN.
   END.
END PROCEDURE.

PROCEDURE borttidflex_UI:
   
   FIND TIDREGITAB WHERE RECID(TIDREGITAB) = tidtabrec NO-LOCK NO-ERROR.
   hjdat = TIDREGITAB.DATUM.
   FIND NEXT TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
   TIDREGITAB.DATUM >= hjdat AND TIDREGITAB.TIDLOG = TRUE   USE-INDEX PSTART NO-LOCK NO-ERROR. 
   IF AVAILABLE TIDREGITAB THEN tidtabrec2 = RECID(TIDREGITAB).
   ELSE DO:
      FIND TIDREGITAB WHERE RECID(TIDREGITAB) = tidtabrec NO-LOCK NO-ERROR.
      FIND PREV TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
      TIDREGITAB.DATUM < HJDAT AND TIDREGITAB.TIDLOG = TRUE  USE-INDEX PSTART NO-LOCK NO-ERROR.  
      IF AVAILABLE TIDREGITAB THEN tidtabrec2 = RECID(TIDREGITAB).     
      ELSE tidtabrec2 = tidtabrec.    
   END.     
   placerarec = tidtabrec2.   
   DO TRANSACTION:   
      FOR EACH TIDREGITAB WHERE  TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
      TIDREGITAB.DATUM =  hjdat AND  TIDREGITAB.TIDLOG = TRUE  AND  TIDREGITAB.OVERTIDUTTAG = "F" EXCLUSIVE-LOCK:
         DELETE TIDREGITAB.
      END.
      FOR EACH FLEXTID WHERE  FLEXTID.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
      FLEXTID.DATUM =  hjdat EXCLUSIVE-LOCK:
         DELETE FLEXTID.
      END.
      FOR EACH FLEXDAG WHERE  FLEXDAG.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
      FLEXDAG.DATUM =  hjdat EXCLUSIVE-LOCK:
         DELETE FLEXDAG.
      END.
    END.     
    
      
   FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
   TIDREGITAB.DATUM = regdatum AND TIDREGITAB.TIDLOG = TRUE
   USE-INDEX PSTART NO-LOCK NO-ERROR.
   IF AVAILABLE TIDREGITAB THEN DO:         
      ASSIGN
      regdatum = TIDREGITAB.DATUM
      tidtabrec = RECID(TIDREGITAB).
      RUN TRAKTBER.P.            
      ASSIGN
      tidtabrec = tidtabrecspar
      regdatum = regdatumspar.
   END.
   
END PROCEDURE.
