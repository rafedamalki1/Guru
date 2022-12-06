/*OVERUTR4.P AV OVERTID SPARAS INNAN  UPPDELNING 24.00 PAR8*/ 
/*DEFINE SHARED VARIABLE globanv LIKE ANVANDARE.ANVANDARE NO-UNDO.*/
DEFINE SHARED VARIABLE globforetag LIKE FORETAG.FORETAG NO-UNDO.
DEFINE SHARED VARIABLE persrec AS RECID  NO-UNDO.
DEFINE SHARED VARIABLE regdatum AS DATE NO-UNDO.
DEFINE SHARED VARIABLE stop1 LIKE OVERTIDTAB.STOPP1 NO-UNDO.
DEFINE SHARED VARIABLE stop2 LIKE OVERTIDTAB.STOPP1 NO-UNDO.
DEFINE SHARED VARIABLE stop3 LIKE OVERTIDTAB.STOPP1 NO-UNDO.
DEFINE SHARED VARIABLE stop4 LIKE OVERTIDTAB.STOPP1 NO-UNDO.
DEFINE SHARED VARIABLE slut4 AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE slut5 AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE start4 AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE start5 AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE starta1 LIKE OVERTIDTAB.START1 NO-UNDO.
DEFINE SHARED VARIABLE starta2 LIKE OVERTIDTAB.START1 NO-UNDO.
DEFINE SHARED VARIABLE starta3 LIKE OVERTIDTAB.START1 NO-UNDO.
DEFINE SHARED VARIABLE kod1 LIKE OVERTIDTAB.OVERTIDTILL NO-UNDO.
DEFINE SHARED VARIABLE kod2 LIKE OVERTIDTAB.OVERTIDTILL NO-UNDO.
DEFINE SHARED VARIABLE kod3 LIKE OVERTIDTAB.OVERTIDTILL NO-UNDO.
DEFINE SHARED VARIABLE kod4 LIKE OVERTIDTAB.OVERTIDTILL NO-UNDO.
DEFINE SHARED VARIABLE kod5 LIKE OVERTIDTAB.OVERTIDTILL NO-UNDO.
DEFINE SHARED VARIABLE tiddiff1 LIKE OVERTIDTAB.START1 NO-UNDO.
DEFINE SHARED VARIABLE tiddiff2 LIKE OVERTIDTAB.START1 NO-UNDO.
DEFINE SHARED VARIABLE komp LIKE OVERTIDTAB.OVERTIDUTTAG NO-UNDO.
DEFINE SHARED VARIABLE tiddi1 LIKE TIDREGITAB.OANT1 NO-UNDO.
DEFINE SHARED VARIABLE tiddi2 LIKE TIDREGITAB.OANT1 NO-UNDO.
DEFINE SHARED VARIABLE tiddi3 LIKE TIDREGITAB.OANT1 NO-UNDO.
DEFINE SHARED VARIABLE btid2 LIKE OVERTIDTAB.START1 NO-UNDO.
DEFINE SHARED VARIABLE bslut LIKE TIDREGITAB.SLUT NO-UNDO.
DEFINE SHARED VARIABLE buslut4 AS INTEGER FORMAT "99999" NO-UNDO.
DEFINE SHARED VARIABLE regstart LIKE TIDREGITAB.START NO-UNDO.
DEFINE SHARED VARIABLE regslut LIKE TIDREGITAB.SLUT NO-UNDO.
DEFINE SHARED VARIABLE krav1 AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE sekunder AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE nytid AS DECIMAL NO-UNDO.
DEFINE SHARED VARIABLE sta AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE slu AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE ber1 LIKE UTRYCKNING.UTRYCKNBER NO-UNDO.
DEFINE SHARED VARIABLE reco AS RECID NO-UNDO.
DEFINE VARIABLE startar AS INTEGER NO-UNDO.
DEFINE VARIABLE ejbe LIKE TIDREGITAB.OANT1 NO-UNDO.
DEFINE VARIABLE tid3 LIKE TIDREGITAB.OANT1 NO-UNDO.
DEFINE VARIABLE buslut3 LIKE TIDREGITAB.SLUT NO-UNDO.
DEFINE VARIABLE seku AS INTEGER NO-UNDO.
DEFINE VARIABLE sluta AS INTEGER NO-UNDO.

DEFINE VARIABLE tott LIKE TIDREGITAB.TOTALT NO-UNDO.
DEFINE VARIABLE tott1 LIKE TIDREGITAB.TOTALT NO-UNDO.
DEFINE SHARED TEMP-TABLE ohjalp
 FIELD ODATUM LIKE TIDREGITAB.DATUM FIELD OSTART LIKE TIDREGITAB.START
 FIELD OSLUT LIKE TIDREGITAB.SLUT 
 /*FIELD OOVERTIDTILL LIKE TIDREGITAB.OVERTIDTILL*/
 FIELD OKOD1 LIKE TIDREGITAB.OKOD1 FIELD OANT1 LIKE TIDREGITAB.OANT1
 FIELD OST1 LIKE TIDREGITAB.START FIELD OSL1 LIKE TIDREGITAB.SLUT
 FIELD OKOD2 LIKE TIDREGITAB.OKOD2 FIELD OANT2 LIKE TIDREGITAB.OANT2
 FIELD OST2 LIKE TIDREGITAB.START FIELD OSL2 LIKE TIDREGITAB.SLUT
 FIELD OKOD3 LIKE TIDREGITAB.OKOD3 FIELD OANT3 LIKE TIDREGITAB.OANT3
 FIELD OST3 LIKE TIDREGITAB.START FIELD OSL3 LIKE TIDREGITAB.SLUT
 /*FIELD OOVERANTAL LIKE TIDREGITAB.OVERANTAL */ 
 FIELD OTOTALT LIKE TIDREGITAB.TOTALT
 FIELD OOVERAUTO LIKE TIDREGITAB.OVERAUTO FIELD OENKE LIKE OVERKOD.ENKEL
 /*FIELD OOVERKV LIKE TIDREGITAB.OVERTIDTILL FIELD OOVERAN LIKE TIDREGITAB.OVERANTAL*/ 
 FIELD OAONR LIKE TIDREGITAB.AONR FIELD ODELNR LIKE
 TIDREGITAB.DELNR FIELD OETOT LIKE TIDREGITAB.TOTALT FIELD OUTR LIKE
 TIDREGITAB.UTRYCKNING FIELD RECTIDVIS AS RECID
 INDEX ODATUM IS PRIMARY ODATUM ASCENDING OSTART ASCENDING.

DEFINE SHARED TEMP-TABLE ovavtab
   FIELD KOD LIKE OVERAVTAB.KOD 
   FIELD OVERTIDTILL LIKE OVERAVTAB.OVERTIDTILL
   FIELD START1 LIKE OVERAVTAB.START1
   FIELD STOPP1 LIKE OVERAVTAB.STOPP1
   FIELD START2 LIKE OVERAVTAB.START2
   FIELD STOPP2 LIKE OVERAVTAB.STOPP2
   FIELD DATUM LIKE OVERAVTAB.DATUM
   FIELD OVERTIDUTTAG LIKE OVERAVTAB.OVERTIDUTTAG
   FIELD DAGEQ LIKE OVERAVTAB.DAGEQ
   FIELD EQDAG LIKE OVERAVTAB.EQDAG 
   INDEX ODATUM IS PRIMARY DATUM ASCENDING 
   INDEX OSTART1 KOD DATUM START1 STOPP1 ASCENDING
   INDEX OSTART2 KOD DATUM START2 STOPP2 ASCENDING.
DEFINE SHARED TEMP-TABLE otidtab
   FIELD KOD LIKE OVERTIDTAB.KOD 
   FIELD OVERTIDTILL LIKE OVERTIDTAB.OVERTIDTILL
   FIELD START1 LIKE OVERTIDTAB.START1
   FIELD STOPP1 LIKE OVERTIDTAB.STOPP1
   FIELD START2 LIKE OVERTIDTAB.START2
   FIELD STOPP2 LIKE OVERTIDTAB.STOPP2
   FIELD DAGNR LIKE OVERTIDTAB.DAGNR
   FIELD OVERTIDUTTAG LIKE OVERTIDTAB.OVERTIDUTTAG
   FIELD FORKL LIKE OVERTIDTAB.FORKL
   FIELD ARBSLUT LIKE OVERTIDTAB.ARBSLUT
   FIELD ARBSTART LIKE OVERTIDTAB.ARBSTART
   INDEX OVERTILL IS PRIMARY DAGNR OVERTIDTILL ASCENDING    
   INDEX OVERT KOD DAGNR ARBSLUT ASCENDING   
   INDEX OVERSTART KOD DAGNR START1 STOPP1 ASCENDING 
   INDEX OVERKOD OVERTIDTILL ASCENDING
   INDEX OVER KOD DAGNR OVERTIDTILL OVERTIDUTTAG ASCENDING
   INDEX OSTART2 KOD DAGNR START2 STOPP2 ASCENDING.      

/* jamfor med overtidstabell  */
FIND PERSONALTAB WHERE RECID(PERSONALTAB) = persrec NO-LOCK NO-ERROR.
FIND FIRST ANSTFORMTAB WHERE ANSTFORMTAB.ANSTALLNING = PERSONALTAB.ANSTALLNING
USE-INDEX ANSTF NO-LOCK NO-ERROR.
FIND FIRST UTRYCKNING WHERE UTRYCKNING.KOD = ANSTFORMTAB.KOD
USE-INDEX UT NO-LOCK NO-ERROR.
FIND FIRST ohjalp WHERE ohjalp.RECTIDVIS = reco NO-LOCK NO-ERROR.
ASSIGN regdatum = ohjalp.ODATUM nytid = ohjalp.OSTART.
RUN TIMSEK.P.
ASSIGN sta = sekunder nytid = ohjalp.OSLUT.
RUN TIMSEK.P.
ASSIGN slu = sekunder
sekunder = UTRYCKNING.UTRYCKNBER.  /*NYTT ANDRAT FRAN EJBER TILL BER NORDKRAFT*/
RUN SEKTIM.P.
ejbe = nytid.
RUN SLUTARB.P. 
RUN REGVEC.P.
nytid = regslut.
RUN TIMSEK.P.
ASSIGN slut5 = sekunder nytid = regstart.
RUN TIMSEK.P.
ASSIGN start5 = sekunder buslut3 = 0
start4 = 0 slut4 = 0.
IF UTRYCKNING.EXTID = TRUE THEN DO:
   IF WEEKDAY(ohjalp.ODATUM) = 1 OR WEEKDAY(ohjalp.ODATUM) = 7 THEN  slut4 = 0.
   ELSE  slut4 = slut5 - 57600.
   IF WEEKDAY(ohjalp.ODATUM) = 2 OR WEEKDAY(ohjalp.ODATUM) = 3
   OR WEEKDAY(ohjalp.ODATUM) = 4 OR WEEKDAY(ohjalp.ODATUM) = 5 
   OR WEEKDAY(ohjalp.ODATUM) = 6 THEN DO:
      FIND FIRST otidtab WHERE otidtab.ARBSLUT = TRUE AND
      otidtab.DAGNR = WEEKDAY(ohjalp.ODATUM) AND
      otidtab.KOD = ANSTFORMTAB.KOD USE-INDEX OVER NO-LOCK NO-ERROR.
      IF NOT AVAILABLE otidtab THEN DO TRANSACTION:
         CREATE FELTEXT.
         ASSIGN 
         FELTEXT.ANVANDARE = globanv         
         FELTEXT.FELTEXT = 'DET ÄR NÅGOT FEL PÅ DIN ÖVERTIDSDATABAS1 KONTAKTA ANSVARIG!'
         FELTEXT.PROGRAM = "OVERUTR4" + STRING(TODAY) + globanv.  
         RETURN.
      END.  
      nytid = ohjalp.OSTART.
      RUN TIMSEK.P.
      startar = sekunder.
      IF startar < ( otidtab.STOPP1 + slut4 ) AND startar > start5 THEN slut4 = slut4.
   END.
END.
IF globforetag = "MSK " AND ANSTFORMTAB.KOD = "K" THEN slut4 = 0.
IF UTRYCKNING.EXAKTM = TRUE THEN DO:
   IF WEEKDAY(ohjalp.ODATUM) = 1 OR WEEKDAY(ohjalp.ODATUM) = 7 THEN start4 = 0.
   ELSE  start4 = start5 - 25200.    /*25200*/
   IF WEEKDAY(ohjalp.ODATUM) = 2 OR WEEKDAY(ohjalp.ODATUM) = 3
   OR WEEKDAY(ohjalp.ODATUM) = 4 OR WEEKDAY(ohjalp.ODATUM) = 5 
   OR WEEKDAY(ohjalp.ODATUM) = 6 THEN DO:
      FIND FIRST otidtab WHERE otidtab.ARBSTART = TRUE AND
      otidtab.DAGNR = WEEKDAY(ohjalp.ODATUM) AND
      otidtab.KOD = ANSTFORMTAB.KOD USE-INDEX OVER NO-LOCK NO-ERROR.
      IF NOT AVAILABLE otidtab THEN DO TRANSACTION:
         CREATE FELTEXT.
         ASSIGN 
         FELTEXT.ANVANDARE = globanv         
         FELTEXT.FELTEXT = 'DET ÄR NÅGOT FEL PÅ DIN ÖVERTIDSDATABAS2 KONTAKTA ANSVARIG!'
         FELTEXT.PROGRAM = "OVERUTR4" + STRING(TODAY) + globanv.
         RETURN.
      END.                    
      nytid = ohjalp.OSLUT.
      RUN TIMSEK.P.
      startar = sekunder.
      IF startar < ( otidtab.START1 + start4 ) AND startar < slut5 THEN start4 = start4.
   END.
END.
FIND FIRST ovavtab WHERE ovavtab.DATUM = regdatum
AND ovavtab.KOD = ANSTFORMTAB.KOD USE-INDEX ODATUM NO-LOCK NO-ERROR.
IF NOT AVAILABLE ovavtab THEN DO:
   FIND FIRST otidtab WHERE otidtab.DAG = WEEKDAY(ohjalp.ODATUM) AND
   otidtab.KOD = ANSTFORMTAB.KOD AND otidtab.OVERTIDUTTAG = komp AND
   sta GE  otidtab.START1 AND sta < otidtab.STOPP1 + slut4 - start4 AND
   otidtab.START1 NE otidtab.STOPP1 USE-INDEX OVERSTART NO-LOCK NO-ERROR.
   IF NOT AVAILABLE otidtab THEN DO:
      FIND FIRST otidtab WHERE otidtab.DAG = WEEKDAY(ohjalp.ODATUM) AND
      otidtab.KOD = ANSTFORMTAB.KOD AND otidtab.OVERTIDUTTAG = komp AND
      sta GE otidtab.START2 AND sta < otidtab.STOPP2 + slut4 - start4 AND
      otidtab.START2 NE otidtab.STOPP2 USE-INDEX OSTART2 NO-LOCK NO-ERROR.
      IF NOT AVAILABLE otidtab THEN DO TRANSACTION:
         CREATE FELTEXT.
         ASSIGN 
         FELTEXT.ANVANDARE = globanv         
         FELTEXT.FELTEXT = 'DET ÄR NÅGOT FEL PÅ DIN ÖVERTIDSDATABAS33 KONTAKTA ANSVARIG!'
         FELTEXT.PROGRAM = "OVERUTR4" + STRING(TODAY) + globanv.
         RETURN.
      END.
      ASSIGN stop1 = otidtab.STOPP2 + slut4 - start4
      kod1 = otidtab.OVERTIDTILL.
   END.
   ELSE DO:
      ASSIGN stop1 = otidtab.STOPP1 + slut4 - start4
      kod1 = otidtab.OVERTIDTILL.
   END.
END.
ELSE DO:
   FIND FIRST ovavtab WHERE ovavtab.DATUM = regdatum AND
   ovavtab.KOD = ANSTFORMTAB.KOD AND ovavtab.OVERTIDUTTAG = komp AND
   sta GE  ovavtab.START1 AND sta < ovavtab.STOPP1 + slut4 - start4 AND
   ovavtab.START1 NE ovavtab.STOPP1 USE-INDEX OSTART1 NO-LOCK NO-ERROR.
   IF NOT AVAILABLE ovavtab THEN DO:
      FIND FIRST ovavtab WHERE ovavtab.DATUM = regdatum AND
      ovavtab.KOD = ANSTFORMTAB.KOD AND ovavtab.OVERTIDUTTAG = komp AND
      sta GE ovavtab.START2 AND sta < ovavtab.STOPP2 + slut4 - start4 AND
      ovavtab.START2 NE ovavtab.STOPP2 USE-INDEX OSTART2 NO-LOCK NO-ERROR.
      IF NOT AVAILABLE ovavtab THEN DO TRANSACTION:
         CREATE FELTEXT.
         ASSIGN 
         FELTEXT.ANVANDARE = globanv         
         FELTEXT.FELTEXT = 'DET ÄR NÅGOT FEL PÅ DIN ÖVERTIDSDATABAS4 KONTAKTA ANSVARIG!'
         FELTEXT.PROGRAM = "OVERUTR4" + STRING(TODAY) + globanv.
         RETURN.
      END.
      ASSIGN stop1 = ovavtab.STOPP2 + slut4 - start4
      kod1 = ovavtab.OVERTIDTILL.
   END.
   ELSE DO:
      ASSIGN stop1 = ovavtab.STOPP1 + slut4 - start4
      kod1 = ovavtab.OVERTIDTILL.
   END.
END.
ASSIGN sluta = slu + btid2
slu = sluta.
IF sluta > 86400 THEN DO:
   sekunder = sluta - 86400.
   RUN SEKTIM.P.
   buslut3 = nytid.
END.
ELSE sluta = slu.
IF buslut4 > 0  AND buslut4 < sluta  THEN DO:
   buslut4 = 0.       
   IF ohjalp.OSTART < regstart  AND ohjalp.OSTART > 0 THEN nytid = nytid.      
   ELSE IF regstart = regslut THEN nytid = nytid.
   ELSE RETURN.
END. 
sekunder = 86400 - UTRYCKNING.UTRYCKNBER.
RUN SEKTIM.P.
IF ohjalp.OSLUT = 24.00 AND ohjalp.OSTART GE nytid THEN DO:
   nytid = buslut3.
   RUN TIMSEK.P.
   buslut4 = sekunder.
END.      
FIND FIRST UTRYCKNING WHERE UTRYCKNING.KOD = ANSTFORMTAB.KOD
USE-INDEX UT NO-LOCK NO-ERROR. /* MSK SPE*/
IF sluta LE stop1 THEN DO:
   sekunder = ber1.
   RUN SEKTIM.P.
   tid3 = nytid.
   FIND FIRST ohjalp WHERE ohjalp.RECTIDVIS = reco  NO-ERROR.
   IF NOT AVAILABLE ohjalp THEN regdatum = regdatum.
   ELSE IF ohjalp.OANT1 > ejbe THEN regdatum = regdatum.    /*?? OOVERANTAL*/
   ELSE DO:                                                
      sekunder = sluta.
      RUN SEKTIM.P.
      ASSIGN ohjalp.OKOD1 = kod1 ohjalp.OANT1 = tid3
      ohjalp.OST1 = ohjalp.OSTART ohjalp.OSL1 = nytid.
   END.
END.
/* UTFYLLNAD TILL 07.30 PÅ MORGONEN FÖR MSK */
ELSE IF sluta > stop1 AND sluta > start5 AND sluta < slut5 AND stop1 = 27000
AND UTRYCKNING.LUFT = TRUE THEN do:
   nytid = ohjalp.OSTART.
   RUN TIMSEK.P.
   sekunder = start5 - sekunder.
   RUN SEKTIM.P.
   tid3 = nytid.
   FIND FIRST ohjalp WHERE ohjalp.RECTIDVIS = reco  NO-ERROR.
   IF NOT AVAILABLE ohjalp THEN regdatum = regdatum.
   ELSE IF ohjalp.OANT1 > ejbe THEN regdatum = regdatum.
   ELSE DO:                                             
      sekunder = sluta.
      RUN SEKTIM.P.
      ASSIGN ohjalp.OKOD1 = kod1 ohjalp.OANT1 = tid3
      ohjalp.OST1 = ohjalp.OSTART ohjalp.OSL1 = nytid.
   END.
END.
ELSE IF sluta = 86400 AND buslut3 = 0 THEN do:
   sekunder = ber1.
   RUN SEKTIM.P.
   tid3 = nytid.
   FIND FIRST ohjalp WHERE ohjalp.RECTIDVIS = reco NO-ERROR.
   IF NOT AVAILABLE ohjalp THEN regdatum = regdatum.
   ELSE IF ohjalp.OANT1 > ejbe THEN regdatum = regdatum.
   ELSE DO:                                            
      sekunder = sluta.
      RUN SEKTIM.P.
      ASSIGN ohjalp.OKOD1 = kod1 ohjalp.OANT1 = tid3
      ohjalp.OST1 = ohjalp.OSTART ohjalp.OSL1 = nytid.
   END.
END.
ELSE DO:
   IF buslut3 > 0 THEN DO:
      FIND FIRST ovavtab WHERE ovavtab.DATUM = regdatum + 1
      AND ovavtab.KOD = ANSTFORMTAB.KOD USE-INDEX ODATUM
      NO-LOCK NO-ERROR.
      IF NOT AVAILABLE ovavtab THEN DO:
         FIND FIRST otidtab WHERE otidtab.DAG = WEEKDAY(regdatum + 1) AND
         otidtab.KOD = ANSTFORMTAB.KOD AND otidtab.OVERTIDUTTAG = komp AND
         otidtab.START1 = 00.00 AND
         otidtab.STOPP1 NE otidtab.START1 USE-INDEX OVERSTART
         NO-LOCK NO-ERROR.
         IF NOT AVAILABLE otidtab THEN DO:
	     FIND FIRST otidtab WHERE otidtab.DAG = WEEKDAY(regdatum + 1) AND
   	     otidtab.KOD = ANSTFORMTAB.KOD AND otidtab.OVERTIDUTTAG = komp AND
            otidtab.START2 = 00.00 AND
	     otidtab.STOPP2 NE otidtab.START2 USE-INDEX OSTART2
            NO-LOCK NO-ERROR.
	     IF NOT AVAILABLE otidtab THEN DO TRANSACTION:
	        CREATE FELTEXT.
               ASSIGN 
               FELTEXT.ANVANDARE = globanv         
               FELTEXT.FELTEXT = 'DET ÄR NÅGOT FEL PÅ DIN ÖVERTIDSDATABAS5 KONTAKTA ANSVARIG!'
               FELTEXT.PROGRAM = "OVERUTR4" + STRING(TODAY) + globanv.
	        RETURN.
     	     END.
	     ASSIGN starta1 = otidtab.START2
	     stop2 = otidtab.STOPP2
   	     kod2 = otidtab.OVERTIDTILL.
         END.
         ELSE DO:
	     ASSIGN starta1 = otidtab.START1
	     stop2 = otidtab.STOPP1
	     kod2 = otidtab.OVERTIDTILL.
         END.   
      END.
      ELSE DO:
         FIND FIRST ovavtab WHERE ovavtab.DATUM = regdatum + 1 AND
         ovavtab.KOD = ANSTFORMTAB.KOD AND ovavtab.OVERTIDUTTAG = komp AND
         ovavtab.START1 = 0 AND
         ovavtab.STOPP1 NE ovavtab.START1 USE-INDEX OSTART1 NO-LOCK NO-ERROR.
         IF NOT AVAILABLE ovavtab THEN DO:
	     FIND FIRST ovavtab WHERE ovavtab.DATUM = regdatum + 1 AND
	     ovavtab.KOD = ANSTFORMTAB.KOD AND ovavtab.OVERTIDUTTAG = komp AND
	     ovavtab.START2 = 0 AND
	     ovavtab.STOPP2 NE ovavtab.START2 USE-INDEX OSTART2 NO-LOCK NO-ERROR.
 	     IF NOT AVAILABLE ovavtab THEN DO TRANSACTION:
	        CREATE FELTEXT.
               ASSIGN 
               FELTEXT.ANVANDARE = globanv         
               FELTEXT.FELTEXT = 'DET ÄR NÅGOT FEL PÅ DIN ÖVERTIDSDATABAS6 KONTAKTA ANSVARIG!'
               FELTEXT.PROGRAM = "OVERUTR4" + STRING(TODAY) + globanv.
	        RETURN.
	     END.
  	     ASSIGN starta1 = ovavtab.START2
	     stop2 = ovavtab.STOPP2
	     kod2 = ovavtab.OVERTIDTILL.
         END.
         ELSE DO:
	     ASSIGN starta1 = ovavtab.START1
  	     stop2 = ovavtab.STOPP1
	     kod2 = ovavtab.OVERTIDTILL.
         END.
      END.
   END.
   ELSE DO:
      IF sluta GE stop1 THEN DO:
         FIND FIRST ovavtab WHERE ovavtab.DATUM = regdatum
         AND ovavtab.KOD = ANSTFORMTAB.KOD USE-INDEX ODATUM
         NO-LOCK NO-ERROR.
         IF NOT AVAILABLE ovavtab THEN DO:
 	     FIND FIRST otidtab WHERE otidtab.DAG = WEEKDAY(ohjalp.ODATUM) AND
	     otidtab.KOD = ANSTFORMTAB.KOD AND otidtab.OVERTIDUTTAG = komp AND
	     otidtab.START1 = stop1 - slut4 + start4 AND
	     otidtab.STOPP1 NE otidtab.START1
	     USE-INDEX OVERSTART NO-LOCK NO-ERROR.
	     IF NOT AVAILABLE otidtab THEN DO:
	        FIND FIRST otidtab WHERE otidtab.DAG = WEEKDAY(ohjalp.ODATUM) AND
	        otidtab.KOD = ANSTFORMTAB.KOD AND otidtab.OVERTIDUTTAG = komp
   	        AND otidtab.START2 = stop1 - slut4 + start4 AND
	        otidtab.STOPP2 NE otidtab.START2
	        USE-INDEX OSTART2 NO-LOCK NO-ERROR.
	        IF NOT AVAILABLE otidtab THEN DO:
	           ASSIGN starta1 = 0 stop2 = 0 kod2 = ''.
	        END.
	        ELSE DO:
	           ASSIGN starta1 = otidtab.START2 + slut4 - start4
	           stop2 = otidtab.STOPP2
	           kod2 = otidtab.OVERTIDTILL.
	        END.
	     END.
	     ELSE DO:
	        ASSIGN starta1 = otidtab.START1 + slut4 - start4
	        stop2 = otidtab.STOPP1
	        kod2 = otidtab.OVERTIDTILL.
  	     END.
         END.
         ELSE DO: 
	     FIND FIRST ovavtab WHERE ovavtab.DATUM = regdatum AND
	     ovavtab.KOD = ANSTFORMTAB.KOD AND ovavtab.OVERTIDUTTAG = komp AND
	     ovavtab.START1 = stop1 - slut4 - start4 AND
	     ovavtab.STOPP1 NE ovavtab.START1
	     USE-INDEX OSTART1 NO-LOCK NO-ERROR.
	     IF NOT AVAILABLE ovavtab THEN DO:
	        FIND FIRST ovavtab WHERE ovavtab.DATUM = regdatum AND
	        ovavtab.KOD = ANSTFORMTAB.KOD AND ovavtab.OVERTIDUTTAG = komp AND
	        ovavtab.START2 = stop1 - slut4 + start4 AND
	        ovavtab.STOPP2 NE ovavtab.START2 USE-INDEX OSTART2
	        NO-LOCK NO-ERROR.
	        IF NOT AVAILABLE ovavtab THEN DO TRANSACTION:
	           CREATE FELTEXT.
                  ASSIGN 
                  FELTEXT.ANVANDARE = globanv         
                  FELTEXT.FELTEXT = 'DET ÄR NÅGOT FEL PÅ DIN ÖVERTIDSDATABAS7 KONTAKTA ANSVARIG!'
                  FELTEXT.PROGRAM = "OVERUTR4" + STRING(TODAY) + globanv.
	           RETURN.
	        END.
	        ASSIGN starta1 = ovavtab.START2 + slut4 - start4
	        stop2 = ovavtab.STOPP2
	        kod2 = ovavtab.OVERTIDTILL.
	     END.
	     ELSE DO:
	        ASSIGN starta1 = ovavtab.START1 + slut4 - start4
	        stop2 = ovavtab.STOPP1
	        kod2 = ovavtab.OVERTIDTILL.
	     END.
         END.
      END.
   END.
END.
FIND FIRST UTRYCKNING WHERE UTRYCKNING.KOD = ANSTFORMTAB.KOD
USE-INDEX UT NO-LOCK NO-ERROR. /* MSK SPE*/
IF sluta > stop1 AND sluta > start5 AND sluta < slut5 AND stop1 = start5
AND UTRYCKNING.LUFT = TRUE THEN regdatum = regdatum.
ELSE IF sluta LE stop1 THEN regdatum = regdatum.
ELSE DO:
   IF buslut3 > 0 AND kod1 = kod2 THEN DO:
      FIND FIRST ohjalp WHERE ohjalp.RECTIDVIS = reco NO-ERROR.
      IF NOT AVAILABLE ohjalp THEN regdatum = regdatum.
  
      IF ohjalp.OSLUT = 24.00 THEN DO:                 
         nytid = ohjalp.OSLUT.
         RUN TIMSEK.P.
         ASSIGN seku = sekunder
         nytid = ohjalp.OSTART.
         RUN TIMSEK.P.
         sekunder = seku - sekunder.
         RUN SEKTIM.P.
         tott = nytid.
         /*reco3 = RECID(TIDREGITAB).*/
         FIND NEXT ohjalp WHERE ohjalp.ODATUM = regdatum + 1
         NO-ERROR.
         IF NOT AVAILABLE ohjalp THEN DO:
  	     sekunder = sluta.
	     RUN SEKTIM.P.
	     FIND FIRST ohjalp WHERE ohjalp.RECTIDVIS = reco NO-ERROR.	
	     ASSIGN ohjalp.OKOD1 = kod1 ohjalp.OANT1 = ejbe
	     ohjalp.OST1 = ohjalp.OSTART ohjalp.OSL1 = nytid.
         END.
         ELSE IF ohjalp.OSTART NE 00.00 THEN DO:
	     sekunder = sluta.
	     RUN SEKTIM.P.
	     FIND FIRST ohjalp WHERE ohjalp.RECTIDVIS = reco NO-ERROR.	
  	     ASSIGN ohjalp.OKOD1 = kod1 ohjalp.OANT1 = ejbe
	     ohjalp.OST1 = ohjalp.OSTART ohjalp.OSL1 = nytid.
         END.
         ELSE IF ohjalp.OSTART = 00.00 AND ohjalp.OSLUT NE 00.00 THEN DO:
 	     IF ohjalp.OSLUT > buslut3 THEN DO:
	        /*ASSIGN ohjalp.OKOD1 = kod1 ohjalp.OANT1 = ohjalp.OSLUT
	        ohjalp.OST1 = ohjalp.OSTART ohjalp.OSL1 = ohjalp.OSLUT.
	        FIND FIRST ohjalp WHERE ohjalp.RECTIDVIS = reco NO-ERROR.
	        ASSIGN ohjalp.OKOD1 = kod1 ohjalp.OANT1 = tott
	        ohjalp.OST1 = ohjalp.OSTART ohjalp.OSL1 = 24.00.*/
	     END.
	     ELSE DO:
	        nytid = ejbe.
	        RUN TIMSEK.P.
	        ASSIGN seku = sekunder
	        nytid = tott.
	        RUN TIMSEK.P.
	        sekunder = seku - sekunder.
	        RUN SEKTIM.P.
	        tott1 = nytid.
	        ASSIGN ohjalp.OKOD1 = kod1 ohjalp.OANT1 = tott1
	        ohjalp.OST1 = ohjalp.OSTART ohjalp.OSL1 = tott1.
	        FIND FIRST ohjalp WHERE ohjalp.RECTIDVIS = reco NO-ERROR.	  
   	        ASSIGN ohjalp.OKOD1 = kod1 ohjalp.OANT1 = tott
	        ohjalp.OST1 = ohjalp.OSTART ohjalp.OSL1 = 24.00.
	        krav1 = TRUE.
	     END.
         END.
      END.    
      ELSE DO:            /*overlapp??*/
         sekunder = sluta.
         RUN SEKTIM.P.
         ASSIGN
         tott = nytid
         sekunder = (24 * 3600) - sta.
         RUN SEKTIM.P.
         ASSIGN tott1 = nytid
         sekunder = (ejbe * 3600) - sekunder.
         RUN SEKTIM.P.
         ASSIGN ohjalp.OKOD1 = kod1 ohjalp.OANT1 = tott1
         ohjalp.OST1 = ohjalp.OSTART ohjalp.OSL1 = 24.00
         ohjalp.OKOD2 = kod1 ohjalp.OANT2 = nytid
         ohjalp.OST2 = 00.00 ohjalp.OSL2 = nytid.
         /*FIND NEXT ohjalp WHERE ohjalp.ODATUM = atum + 1 EXCLUSIVE-LOCK  NO-ERROR.
         IF NOT AVAILABLE ohjalp THEN LEAVE.
         IF ohjalp.OSTART = 00.00 AND ohjalp.OSLUT NE 00.00 THEN DO:
	     IF ohjalp.SLUT > buslut3 THEN DO:
	        nytid = TIDREGITAB.SLUT.
	        RUN TIMSEK.P.
	        ASSIGN seku = sekunder
	        nytid = buslut3.
	        RUN TIMSEK.P.
	        sekunder = seku - sekunder.
	        RUN SEKTIM.P.
	        ASSIGN ohjalp.OKOD1 = kod1 ohjalp.OANT1 = nytid
	        ohjalp.OST1 = ohjalp.OSTART ohjalp.OSL1 = nytid.    /*??*/
	        krav1 = TRUE.
	     END.
	     ELSE DO:
	        ASSIGN ohjalp.OKOD1 = '' ohjalp.OANT1 = 0.
	        krav1 = TRUE.
	     END.
         END. */
      END.     
   END. 
   ELSE DO:
      IF buslut3 > 0 AND kod1 NE kod2 THEN DO:
         FIND FIRST ohjalp WHERE ohjalp.RECTIDVIS = reco NO-ERROR.
         /*sekunder = sluta.
         RUN SEKTIM.P.
         tott = nytid.  */
         nytid = ohjalp.OSTART.
         RUN TIMSEK.P.
         sekunder = (24 * 3600) -  sekunder.
         RUN SEKTIM.P.
         tott1 = nytid.      
         sekunder = (ejbe * 3600) - sekunder.
         RUN SEKTIM.P.
         ASSIGN ohjalp.OKOD1 = kod1 ohjalp.OANT1 = tott1
         ohjalp.OST1 = ohjalp.OSTART ohjalp.OSL1 = 24.00
         ohjalp.OKOD2 = kod2 ohjalp.OANT2 = nytid
         ohjalp.OST2 = 00.00 ohjalp.OSL2 = buslut3.
      END. 
   END.
END.
FIND FIRST UTRYCKNING WHERE UTRYCKNING.KOD = ANSTFORMTAB.KOD
USE-INDEX UT NO-LOCK NO-ERROR. /*  MSK SPE*/
IF sluta > stop1 AND sluta > start5 AND sluta < slut5 AND stop1 = start5
AND UTRYCKNING.LUFT = TRUE THEN regdatum = regdatum.
ELSE IF sluta > stop1 AND sluta le stop2 AND buslut3=0 THEN DO:
   ASSIGN tiddiff1 =  stop1 - sta
   tiddiff2 = sluta - starta1
   sekunder = tiddiff1.
   RUN SEKTIM.P.
   ASSIGN tiddi1 = nytid
   sekunder = tiddiff2.
   RUN SEKTIM.P.
   ASSIGN tiddi2 = nytid
   sekunder = sluta.
   RUN SEKTIM.P.
   ASSIGN tiddi3 = nytid
   sekunder = stop1.
   RUN SEKTIM.P.
   FIND FIRST ohjalp WHERE ohjalp.RECTIDVIS = reco NO-ERROR.
   ASSIGN ohjalp.OKOD1 = kod1 ohjalp.OANT1 = tiddi1
   ohjalp.OST1 = ohjalp.OSTART ohjalp.OSL1 = nytid
   ohjalp.OKOD2 = kod2 ohjalp.OANT2 = tiddi2
   ohjalp.OST2 = nytid ohjalp.OSL2 = tiddi3.
END.
ELSE IF sluta > stop1 AND sluta > stop2 AND buslut3=0
AND stop2 = start5 THEN DO:
   /* MORGON UTFYLLNAD TILL 7.30 BRYTNING 06.00 MSK TJÄNSTE */
   ASSIGN tiddiff1 =  stop1 - sta
   tiddiff2 = stop2 - starta1
   sekunder = tiddiff1.
   RUN SEKTIM.P.
   ASSIGN tiddi1 = nytid
   sekunder = tiddiff2.
   RUN SEKTIM.P.
   ASSIGN tiddi2 = nytid
   sekunder = sluta.
   RUN SEKTIM.P.
   ASSIGN tiddi3 = nytid
   sekunder = stop1.
   RUN SEKTIM.P.
   FIND FIRST ohjalp WHERE ohjalp.RECTIDVIS = reco NO-ERROR.
   ASSIGN ohjalp.OKOD1 = kod1 ohjalp.OANT1 = tiddi1
   ohjalp.OST1 = ohjalp.OSTART ohjalp.OSL1 = nytid
   ohjalp.OKOD2 = kod2 ohjalp.OANT2 = tiddi2
   ohjalp.OST2 = nytid ohjalp.OSL2 = tiddi3.
END.
