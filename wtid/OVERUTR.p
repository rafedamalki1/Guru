/* OVERUTR.P */ 
DEFINE NEW SHARED VARIABLE btid1 LIKE OVERTIDTAB.START1 NO-UNDO.
DEFINE NEW SHARED VARIABLE btid2 LIKE OVERTIDTAB.START1 NO-UNDO.
DEFINE NEW SHARED VARIABLE buslut4 AS INTEGER FORMAT "99999" INITIAL 0 NO-UNDO.
DEFINE NEW SHARED VARIABLE slut3 AS INTEGER FORMAT '9999' NO-UNDO.
DEFINE NEW SHARED VARIABLE autryck LIKE TIDREGITAB.UTRYCKNING NO-UNDO.
DEFINE NEW SHARED VARIABLE krav1 AS LOGICAL NO-UNDO.

DEFINE SHARED VARIABLE persrec AS RECID  NO-UNDO.
DEFINE SHARED VARIABLE regstart LIKE TIDREGITAB.START NO-UNDO.
DEFINE SHARED VARIABLE regslut LIKE TIDREGITAB.SLUT NO-UNDO.
DEFINE SHARED VARIABLE bdatum AS DATE NO-UNDO.
DEFINE SHARED VARIABLE avdatum AS DATE NO-UNDO.
DEFINE SHARED VARIABLE regdatum AS DATE NO-UNDO.
DEFINE SHARED VARIABLE ber1 LIKE UTRYCKNING.UTRYCKNBER NO-UNDO.
DEFINE SHARED VARIABLE bustart3 LIKE TIDREGITAB.START NO-UNDO. 
DEFINE SHARED VARIABLE slut2 LIKE TIDREGITAB.SLUT NO-UNDO.
DEFINE SHARED VARIABLE krav2 AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE nytid AS DECIMAL NO-UNDO.
DEFINE SHARED VARIABLE sekunder AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE sta AS INTEGER FORMAT '99999' NO-UNDO.
DEFINE SHARED VARIABLE slu AS INTEGER FORMAT '99999' NO-UNDO.
DEFINE SHARED VARIABLE reco AS RECID NO-UNDO.
DEFINE VARIABLE res LIKE AUTOMREG.PRISTYP NO-UNDO.
DEFINE VARIABLE seku AS INTEGER NO-UNDO.
DEFINE VARIABLE sekun AS INTEGER NO-UNDO.
DEFINE VARIABLE hjadatum LIKE TIDREGITAB.DATUM NO-UNDO.
DEFINE VARIABLE hjbdatum LIKE TIDREGITAB.DATUM NO-UNDO.
DEFINE VARIABLE tiden LIKE TIDREGITAB.START NO-UNDO.   
DEFINE VARIABLE tiden2 LIKE TIDREGITAB.START NO-UNDO.
DEFINE VARIABLE nydag AS LOGICAL NO-UNDO.
DEFINE VARIABLE ost LIKE TIDREGITAB.START NO-UNDO.
DEFINE VARIABLE avberrec AS RECID NO-UNDO.
DEFINE VARIABLE istrans AS LOGICAL INITIAL YES.
DEFINE VARIABLE odat1 LIKE TIDREGITAB.DATUM NO-UNDO.
DEFINE VARIABLE tid2 LIKE TIDREGITAB.START NO-UNDO.
DEFINE VARIABLE utrber LIKE UTRYCKNING.UTRYCKNBER NO-UNDO.
DEFINE VARIABLE utrejber LIKE UTRYCKNING.UTRYCKNEJBER NO-UNDO.
DEFINE VARIABLE berhj AS DECIMAL NO-UNDO.
DEFINE BUFFER tidbuff FOR TIDREGITAB.
DEFINE SHARED TEMP-TABLE ohjalp
   FIELD ODATUM LIKE TIDREGITAB.DATUM FIELD OSTART LIKE TIDREGITAB.START
   FIELD OSLUT LIKE TIDREGITAB.SLUT 
   FIELD OKOD1 LIKE TIDREGITAB.OKOD1 FIELD OANT1 LIKE TIDREGITAB.OANT1
   FIELD OST1 LIKE TIDREGITAB.START FIELD OSL1 LIKE TIDREGITAB.SLUT
   FIELD OKOD2 LIKE TIDREGITAB.OKOD2 FIELD OANT2 LIKE TIDREGITAB.OANT2
   FIELD OST2 LIKE TIDREGITAB.START FIELD OSL2 LIKE TIDREGITAB.SLUT
   FIELD OKOD3 LIKE TIDREGITAB.OKOD3 FIELD OANT3 LIKE TIDREGITAB.OANT3
   FIELD OST3 LIKE TIDREGITAB.START FIELD OSL3 LIKE TIDREGITAB.SLUT
   FIELD OTOTALT LIKE TIDREGITAB.TOTALT
   FIELD OOVERAUTO LIKE TIDREGITAB.OVERAUTO FIELD OENKE LIKE OVERKOD.ENKEL
   FIELD OAONR LIKE TIDREGITAB.AONR FIELD ODELNR LIKE TIDREGITAB.DELNR 
   FIELD OETOT LIKE TIDREGITAB.TOTALT 
   FIELD OUTR LIKE TIDREGITAB.UTRYCKNING FIELD RECTIDVIS AS RECID
   FIELD OVERTIDUTTAG LIKE TIDREGITAB.OVERTIDUTTAG
   FIELD OAVBE LIKE TIDREGITAB.LAGANTAL
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

DEFINE BUFFER obuff FOR ohjalp.      
FIND PERSONALTAB WHERE RECID(PERSONALTAB) = persrec NO-LOCK NO-ERROR.
FIND FIRST ANSTFORMTAB WHERE ANSTFORMTAB.ANSTALLNING = PERSONALTAB.ANSTALLNING
USE-INDEX ANSTF NO-LOCK NO-ERROR.
FIND FIRST UTRYCKNING WHERE UTRYCKNING.KOD = ANSTFORMTAB.KOD
USE-INDEX UT NO-LOCK NO-ERROR.                            
ASSIGN 
utrber = UTRYCKNING.UTRYCKNBER
utrejber = UTRYCKNING.UTRYCKNEJBER.
FIND FIRST AUTOMREG WHERE AUTOMREG.RESTIDREG = TRUE
USE-INDEX PRISTYPER NO-LOCK NO-ERROR.
ASSIGN
res = AUTOMREG.PRISTYP
krav1 = false    /* TRUE OM REG UPPDELAD TILL 2 */
krav2 = false. 
FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
TIDREGITAB.DATUM = regdatum AND TIDREGITAB.OVERAUTO = FALSE
USE-INDEX PSTART NO-LOCK NO-ERROR.
IF NOT AVAILABLE TIDREGITAB THEN DO: 
   /* INGA MAN*/   
   FIND FIRST ohjalp NO-LOCK NO-ERROR.
   ASSIGN autryck = ohjalp.OUTR
   reco = ohjalp.RECTIDVIS
   nytid = regslut.           
   RUN TIMSEK.P.
   slut3 = sekunder.  
   IF regstart = regslut THEN slut3 = slut3.   /*EJ LÖR SÖN*/   
   ELSE slut3 = slut3 + UTRYCKNING.EXTRAARBPASS.
   sekunder = slut3.
   RUN SEKTIM.P.
   slut2= nytid.      
   IF ohjalp.OSTART GE regslut AND ohjalp.OSTART LE slut2 
   AND regstart NE regslut THEN DO:      
      FIND FIRST ohjalp WHERE ohjalp.RECTIDVIS = reco NO-ERROR.
      nytid = regslut.
      RUN TIMSEK.P.
      ASSIGN sta = sekunder
      nytid = ohjalp.OSTART.
      RUN TIMSEK.P.
      seku = sekunder. 
      FIND FIRST BEREDSKAPTAB WHERE BEREDSKAPTAB.BEREDSKAPSAVTAL =
      PERSONALTAB.BEREDSKAPSAVTAL USE-INDEX BERED NO-LOCK NO-ERROR.
      IF BEREDSKAPTAB.BERANTAL > 0 THEN regdatum = regdatum.
      ELSE IF UTRYCKNING.AVBE = FALSE THEN regdatum = regdatum.
      ELSE DO:         
    	   FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONAL =
	      PERSONALTAB.PERSONALKOD AND TIDREGITAB.DATUM = ohjalp.ODATUM AND
	      TIDREGITAB.BEREDSKAPSTART LE ohjalp.OSTART AND
	      TIDREGITAB.BEREDSKAPSLUT > ohjalp.OSTART USE-INDEX PSTART NO-LOCK NO-ERROR.
         IF AVAILABLE TIDREGITAB THEN DO TRANSACTION:   
            avberrec = RECID(TIDREGITAB).
            FIND TIDREGITAB WHERE RECID(TIDREGITAB) = avberrec EXCLUSIVE-LOCK NO-ERROR.
            nytid = TIDREGITAB.BEREDSKAPSLUT.
            RUN TIMSEK.P.
            ASSIGN sekun = sekunder
            nytid = TIDREGITAB.BEREDSKAPSTART.
            RUN TIMSEK.P.
            ASSIGN seku = sekunder
            sekunder = sekun - seku.
            RUN SEKTIM.P.
            ASSIGN TIDREGITAB.BERANTAL = nytid.	     
            /*om övertids-registreringen täcker flera beredskapsregistreringar
            -nollställ båda*/
            berhj = ohjalp.OSLUT.
            IF ohjalp.OSL1 > berhj THEN berhj = ohjalp.OSL1.
            IF ohjalp.OSL2 > berhj THEN berhj = ohjalp.OSL2.
            IF ohjalp.OSL3 > berhj THEN berhj = ohjalp.OSL3.
            IF berhj > TIDREGITAB.BEREDSKAPSLUT THEN DO:
               FIND FIRST tidbuff WHERE tidbuff.PERSONAL =
      	      PERSONALTAB.PERSONALKOD AND tidbuff.DATUM = ohjalp.ODATUM AND
      	      tidbuff.BEREDSKAPSTART = TIDREGITAB.BEREDSKAPSLUT 
      	      USE-INDEX PSTART NO-LOCK NO-ERROR.
               IF AVAILABLE tidbuff THEN DO:   
                  avberrec = RECID(tidbuff).
                  FIND tidbuff WHERE RECID(tidbuff) = avberrec EXCLUSIVE-LOCK NO-ERROR.
                  nytid = tidbuff.BEREDSKAPSLUT.
                  RUN TIMSEK.P.
                  ASSIGN sekun = sekunder
                  nytid = tidbuff.BEREDSKAPSTART.
                  RUN TIMSEK.P.
                  ASSIGN seku = sekunder
                  sekunder = sekun - seku.
                  RUN SEKTIM.P.
                  ASSIGN tidbuff.BERANTAL = nytid.
               END.
            END.
         END.                         
      END.     
      IF autryck = FALSE THEN DO:
         nytid = ohjalp.OANT1.
         RUN TIMSEK.P.
         sekunder = sekunder + seku - sta.
         RUN SEKTIM.P.
         ASSIGN ohjalp.OANT1 = nytid.
      END.
   END.
   ELSE DO:   
      FOR EACH TIDREGITAB WHERE TIDREGITAB.PERSONAL = PERSONALTAB.PERSONALKOD AND
      TIDREGITAB.DATUM = ohjalp.ODATUM USE-INDEX PSTART NO-LOCK:
         IF ohjalp.OSTART GE TIDREGITAB.BEREDSKAPSTART AND
         ohjalp.OSTART < TIDREGITAB.BEREDSKAPSLUT THEN  krav2 = TRUE.
      END.   
         
      IF krav2 = true THEN ber1 = utrber.
      ELSE ber1 = utrejber.      
      nytid = ohjalp.OSLUT.
      RUN TIMSEK.P.
      ASSIGN slu = sekunder
      nytid = ohjalp.OSTART.
      RUN TIMSEK.P.
      sta = sekunder.
      IF ohjalp.OSLUT < ohjalp.OSTART THEN btid1 = 86400 + slu - sta.
      ELSE btid1 = slu - sta.      
      IF autryck = FALSE  OR btid1 > ber1 THEN DO:
         IF btid1 > ber1 AND ohjalp.OSLUT = 24.00 THEN DO:
            FIND NEXT ohjalp NO-LOCK NO-ERROR.
            IF AVAILABLE ohjalp THEN DO:
               IF ohjalp.OSTART = 00.00 THEN krav1 = TRUE.
            END.
            FIND FIRST ohjalp WHERE ohjalp.RECTIDVIS = reco NO-LOCK NO-ERROR.
         END.      
      END.  
      ELSE DO:                  
         btid2 = ber1 - btid1.                                  
         RUN OVERUTR4.P. /* JAMFOR MED OVERTABELL*/                                    
         FIND FIRST BEREDSKAPTAB WHERE BEREDSKAPTAB.BEREDSKAPSAVTAL =
         PERSONALTAB.BEREDSKAPSAVTAL USE-INDEX BERED NO-LOCK NO-ERROR.
         IF BEREDSKAPTAB.BERANTAL > 0 THEN regdatum = regdatum.
         ELSE IF UTRYCKNING.AVBE = FALSE THEN regdatum = regdatum.
         ELSE DO:          
            IF ber1 = utrber THEN DO:
    	        FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONAL =
	           PERSONALTAB.PERSONALKOD AND TIDREGITAB.DATUM = ohjalp.ODATUM AND
	           TIDREGITAB.BEREDSKAPSTART LE ohjalp.OSTART AND
	           TIDREGITAB.BEREDSKAPSLUT > ohjalp.OSTART USE-INDEX PSTART
	           NO-LOCK NO-ERROR.
              IF AVAILABLE TIDREGITAB THEN DO TRANSACTION:   
	              avberrec = RECID(TIDREGITAB).
                 FIND TIDREGITAB WHERE RECID(TIDREGITAB) = avberrec EXCLUSIVE-LOCK NO-ERROR.
   	           nytid = TIDREGITAB.BEREDSKAPSLUT.
   	           RUN TIMSEK.P.
   	           ASSIGN sekun = sekunder
   	           nytid = TIDREGITAB.BEREDSKAPSTART.
   	           RUN TIMSEK.P.
   	           ASSIGN seku = sekunder
   	           sekunder = sekun - seku.
   	           RUN SEKTIM.P.
   	           ASSIGN TIDREGITAB.BERANTAL = nytid.	              
                 /*om övertids-registreringen täcker flera beredskapsregistreringar
                 -nollställ båda*/
                 berhj = ohjalp.OSLUT.                 
                 IF ohjalp.OSL1 > berhj THEN berhj = ohjalp.OSL1.
                 IF ohjalp.OSL2 > berhj THEN berhj = ohjalp.OSL2.
                 IF ohjalp.OSL3 > berhj THEN berhj = ohjalp.OSL3.
                 IF berhj > TIDREGITAB.BEREDSKAPSLUT THEN DO:
                    FIND FIRST tidbuff WHERE tidbuff.PERSONAL =
            	     PERSONALTAB.PERSONALKOD AND tidbuff.DATUM = ohjalp.ODATUM AND
            	     tidbuff.BEREDSKAPSTART = TIDREGITAB.BEREDSKAPSLUT 
            	     USE-INDEX PSTART NO-LOCK NO-ERROR.
                    IF AVAILABLE tidbuff THEN DO:   
                       avberrec = RECID(tidbuff).
                       FIND tidbuff WHERE RECID(tidbuff) = avberrec EXCLUSIVE-LOCK NO-ERROR.
                       nytid = tidbuff.BEREDSKAPSLUT.
                       RUN TIMSEK.P.
                       ASSIGN sekun = sekunder
                       nytid = tidbuff.BEREDSKAPSTART.
                       RUN TIMSEK.P.
                       ASSIGN seku = sekunder
                       sekunder = sekun - seku.
                       RUN SEKTIM.P.
                       ASSIGN tidbuff.BERANTAL = nytid.
                    END.
                 END.
   	           IF TIDREGITAB.BEREDSKAPSTART = 00.00 THEN DO:
   	              FIND FIRST obuff WHERE obuff.ODATUM = TIDREGITAB.DATUM - 1 NO-LOCK NO-ERROR.
   	              IF NOT AVAILABLE obuff THEN DO:
   	                 FIND LAST tidbuff WHERE tidbuff.PERSONALKOD = TIDREGITAB.PERSONALKOD AND
   	                 tidbuff.DATUM = TIDREGITAB.DATUM - 1 USE-INDEX PSTART NO-LOCK NO-ERROR.
   	                 IF AVAILABLE tidbuff THEN DO:
   	                    IF tidbuff.OSL1 = 24.00 AND tidbuff.OSL2 > 0 THEN DO:
   	                       nytid = tidbuff.OSL2.
   	                       RUN TIMSEK.P.
   	                       ASSIGN sekun = sekunder
   	                       nytid = TIDREGITAB.BERANTAL.
   	                       RUN TIMSEK.P.
   	                       ASSIGN seku = sekunder
   	                       sekunder = seku - sekun.
   	                       RUN SEKTIM.P.
                             ASSIGN TIDREGITAB.BERANTAL = nytid.
                          END.
                          ELSE IF tidbuff.OSL2 = 24.00 AND tidbuff.OSL3 > 0 THEN DO:
   	                       nytid = tidbuff.OSL3.
   	                       RUN TIMSEK.P.
   	                       ASSIGN sekun = sekunder
   	                       nytid = TIDREGITAB.BERANTAL.
   	                       RUN TIMSEK.P.
   	                       ASSIGN seku = sekunder
   	                       sekunder = seku - sekun.
   	                       RUN SEKTIM.P.
                             ASSIGN TIDREGITAB.BERANTAL = nytid.
                          END.
                       END.      
                    END.   
                 END.   
 	        END. 
	        IF ohjalp.OSL1 = 24.00 AND ohjalp.OSL2 > 0 THEN DO:
                  FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONAL =
                  PERSONALTAB.PERSONALKOD AND TIDREGITAB.DATUM = ohjalp.ODATUM + 1 AND
                  TIDREGITAB.BEREDSKAPSTART = 00.00 AND TIDREGITAB.BEREDSKAPSLUT > 0
                  USE-INDEX PSTART NO-LOCK NO-ERROR.
                  IF AVAILABLE TIDREGITAB THEN DO TRANSACTION: 
                     avberrec = RECID(TIDREGITAB).
                     FIND TIDREGITAB WHERE RECID(TIDREGITAB) = avberrec EXCLUSIVE-LOCK NO-ERROR.
                     nytid = TIDREGITAB.BEREDSKAPSLUT.
                     RUN TIMSEK.P.
                     ASSIGN sekun = sekunder
                     nytid = TIDREGITAB.BEREDSKAPSTART.
                     RUN TIMSEK.P.
                     ASSIGN seku = sekunder
                     sekunder = sekun - seku.
                     RUN SEKTIM.P.
                     ASSIGN TIDREGITAB.BERANTAL = nytid.              
                  END.
               END.        
    	      END.
         END.
      END.
   END.  
   slinga:
   REPEAT:
      FIND NEXT ohjalp NO-LOCK NO-ERROR.
      IF NOT AVAILABLE ohjalp THEN LEAVE slinga.
      IF krav1 =  TRUE THEN DO:
         krav1= FALSE.
         NEXT slinga.
      END.
      ASSIGN autryck = ohjalp.OUTR
      reco = ohjalp.RECTIDVIS
      nytid = regslut.
      RUN TIMSEK.P.
      slut3 = sekunder.    
      IF regstart = regslut THEN slut3 = slut3.   /*EJ LÖR SÖN*/   
      ELSE slut3 = slut3 + UTRYCKNING.EXTRAARBPASS.
      sekunder = slut3.
      RUN SEKTIM.P.
      slut2= nytid.             
      IF ohjalp.OSTART GE regslut AND ohjalp.OSTART LE slut2 
      AND regstart NE regslut THEN DO:        
         nytid = regslut.
         RUN TIMSEK.P.
         ASSIGN sta = sekunder
         nytid = ohjalp.OSTART.
         RUN TIMSEK.P.
         seku = sekunder.
         FIND FIRST BEREDSKAPTAB WHERE BEREDSKAPTAB.BEREDSKAPSAVTAL =
         PERSONALTAB.BEREDSKAPSAVTAL USE-INDEX BERED NO-LOCK NO-ERROR.
         IF BEREDSKAPTAB.BERANTAL > 0 THEN regdatum = regdatum.
         ELSE IF UTRYCKNING.AVBE = FALSE THEN regdatum = regdatum.
         ELSE DO:            
    	      FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONAL =
	         PERSONALTAB.PERSONALKOD AND TIDREGITAB.DATUM = ohjalp.ODATUM AND
	         TIDREGITAB.BEREDSKAPSTART LE ohjalp.OSTART AND
	         TIDREGITAB.BEREDSKAPSLUT > ohjalp.OSTART 
	         USE-INDEX PSTART NO-LOCK NO-ERROR.
            IF AVAILABLE TIDREGITAB THEN DO TRANSACTION:   
	            avberrec = RECID(TIDREGITAB).
               FIND TIDREGITAB WHERE RECID(TIDREGITAB) = avberrec EXCLUSIVE-LOCK NO-ERROR.
   	         nytid = TIDREGITAB.BEREDSKAPSLUT.
   	         RUN TIMSEK.P.
   	         ASSIGN sekun = sekunder
   	         nytid = TIDREGITAB.BEREDSKAPSTART.
   	         RUN TIMSEK.P.
   	         ASSIGN seku = sekunder
   	         sekunder = sekun - seku.
   	         RUN SEKTIM.P.
   	         ASSIGN TIDREGITAB.BERANTAL = nytid.	     
               /*om övertids-registreringen täcker flera beredskapsregistreringar
               -nollställ båda*/
               berhj = ohjalp.OSLUT.
               IF ohjalp.OSL1 > berhj THEN berhj = ohjalp.OSL1.
               IF ohjalp.OSL2 > berhj THEN berhj = ohjalp.OSL2.
               IF ohjalp.OSL3 > berhj THEN berhj = ohjalp.OSL3.
               IF berhj > TIDREGITAB.BEREDSKAPSLUT THEN DO:
                  FIND FIRST tidbuff WHERE tidbuff.PERSONAL =
         	      PERSONALTAB.PERSONALKOD AND tidbuff.DATUM = ohjalp.ODATUM AND
         	      tidbuff.BEREDSKAPSTART = TIDREGITAB.BEREDSKAPSLUT 
         	      USE-INDEX PSTART NO-LOCK NO-ERROR.
                  IF AVAILABLE tidbuff THEN DO:   
                     avberrec = RECID(tidbuff).
                     FIND tidbuff WHERE RECID(tidbuff) = avberrec EXCLUSIVE-LOCK NO-ERROR.
                     nytid = tidbuff.BEREDSKAPSLUT.
                     RUN TIMSEK.P.
                     ASSIGN sekun = sekunder
                     nytid = tidbuff.BEREDSKAPSTART.
                     RUN TIMSEK.P.
                     ASSIGN seku = sekunder
                     sekunder = sekun - seku.
                     RUN SEKTIM.P.
                     ASSIGN tidbuff.BERANTAL = nytid.
                  END.
               END.
    	      END.            
         END.           
         IF autryck = FALSE THEN DO:  
            FIND FIRST ohjalp WHERE ohjalp.RECTIDVIS = reco NO-ERROR.
	     nytid = ohjalp.OANT1.
	     RUN TIMSEK.P.
	     sekunder = sekunder + seku - sta.
	     RUN SEKTIM.P.
            ASSIGN ohjalp.OANT1 = nytid.
         END.
      END.
      ELSE DO: 
         FOR EACH TIDREGITAB WHERE TIDREGITAB.PERSONAL = PERSONALTAB.PERSONALKOD
         AND TIDREGITAB.DATUM = ohjalp.ODATUM USE-INDEX PSTART NO-LOCK: 
            IF ohjalp.OSTART GE TIDREGITAB.BEREDSKAPSTART AND
	     ohjalp.OSTART < TIDREGITAB.BEREDSKAPSLUT THEN krav2 = TRUE.
         END.
           
         IF krav2 = true THEN ber1 = utrber.
	      ELSE ber1 = utrejber.                
         nytid = ohjalp.OSLUT.
         RUN TIMSEK.P.
         ASSIGN slu = sekunder
         nytid = ohjalp.OSTART.
         RUN TIMSEK.P.
         sta = sekunder.
         IF ohjalp.OSLUT < ohjalp.OSTART THEN btid1 = 86400 + slu - sta.
         ELSE btid1 = slu - sta.         
         IF autryck = FALSE  OR btid1 > ber1 THEN DO:
            IF btid1 > ber1 AND ohjalp.OSLUT = 24.00 THEN DO:
               FIND NEXT ohjalp NO-LOCK NO-ERROR.
               IF AVAILABLE ohjalp THEN DO:
                  IF ohjalp.OSTART = 00.00 THEN krav1 = TRUE.
               END.
               FIND FIRST ohjalp WHERE ohjalp.RECTIDVIS = reco NO-LOCK NO-ERROR.
            END.       
         END.  
         ELSE DO:
	     btid2 = ber1 - btid1.                         
	     RUN OVERUTR4.P. /* JAMFOR MED OVERTABELL*/         
         END.                     
         FIND FIRST BEREDSKAPTAB WHERE BEREDSKAPTAB.BEREDSKAPSAVTAL =
         PERSONALTAB.BEREDSKAPSAVTAL USE-INDEX BERED NO-LOCK NO-ERROR.
         IF BEREDSKAPTAB.BERANTAL > 0 THEN regdatum = regdatum.
         ELSE IF UTRYCKNING.AVBE = FALSE THEN regdatum = regdatum.
         ELSE DO:
            IF ber1 = utrber THEN DO:
	            FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONAL =
	            PERSONALTAB.PERSONALKOD AND TIDREGITAB.DATUM = ohjalp.ODATUM AND
	            TIDREGITAB.BEREDSKAPSTART LE ohjalp.OSTART AND
	            TIDREGITAB.BEREDSKAPSLUT > ohjalp.OSTART USE-INDEX PSTART
	            NO-LOCK NO-ERROR.
	            IF AVAILABLE TIDREGITAB THEN DO TRANSACTION:
	               avberrec = RECID(TIDREGITAB).
                  FIND TIDREGITAB WHERE RECID(TIDREGITAB) = avberrec 
                  EXCLUSIVE-LOCK NO-ERROR.
	               nytid = TIDREGITAB.BEREDSKAPSLUT.
	               RUN TIMSEK.P.
	               ASSIGN sekun = sekunder
	               nytid = TIDREGITAB.BEREDSKAPSTART.
	               RUN TIMSEK.P.
	               ASSIGN seku = sekunder
	               sekunder = sekun - seku.
	               RUN SEKTIM.P.
	               ASSIGN TIDREGITAB.BERANTAL = nytid.	     
                  /*om övertids-registreringen täcker flera beredskapsregistreringar
                  -nollställ båda*/
                  berhj = ohjalp.OSLUT.                  
                  IF ohjalp.OSL1 > berhj THEN berhj = ohjalp.OSL1.
                  IF ohjalp.OSL2 > berhj THEN berhj = ohjalp.OSL2.
                  IF ohjalp.OSL3 > berhj THEN berhj = ohjalp.OSL3.
                  IF berhj > TIDREGITAB.BEREDSKAPSLUT THEN DO:
                     FIND FIRST tidbuff WHERE tidbuff.PERSONAL =
            	      PERSONALTAB.PERSONALKOD AND tidbuff.DATUM = ohjalp.ODATUM AND
            	      tidbuff.BEREDSKAPSTART = TIDREGITAB.BEREDSKAPSLUT 
            	      USE-INDEX PSTART NO-LOCK NO-ERROR.
                     IF AVAILABLE tidbuff THEN DO:   
                        avberrec = RECID(tidbuff).
                        FIND tidbuff WHERE RECID(tidbuff) = avberrec EXCLUSIVE-LOCK NO-ERROR.
                        nytid = tidbuff.BEREDSKAPSLUT.
                        RUN TIMSEK.P.
                        ASSIGN sekun = sekunder
                        nytid = tidbuff.BEREDSKAPSTART.
                        RUN TIMSEK.P.
                        ASSIGN seku = sekunder
                        sekunder = sekun - seku.
                        RUN SEKTIM.P.
                        ASSIGN tidbuff.BERANTAL = nytid.
                     END.
                  END.
	               IF TIDREGITAB.BEREDSKAPSTART = 00.00 THEN DO:
   	              FIND FIRST obuff WHERE obuff.ODATUM = TIDREGITAB.DATUM - 1 NO-LOCK NO-ERROR.
   	              IF NOT AVAILABLE obuff THEN DO:
     	                 FIND LAST tidbuff WHERE tidbuff.PERSONALKOD = TIDREGITAB.PERSONALKOD AND
   	                 tidbuff.DATUM = TIDREGITAB.DATUM - 1 USE-INDEX PSTART NO-LOCK NO-ERROR.
   	                 IF AVAILABLE tidbuff THEN DO:
   	                    IF tidbuff.OSL1 = 24.00 AND tidbuff.OSL2 > 0 THEN DO:
   	                       nytid = tidbuff.OSL2.
   	                       RUN TIMSEK.P.
   	                       ASSIGN sekun = sekunder
   	                       nytid = TIDREGITAB.BERANTAL.
   	                       RUN TIMSEK.P.
   	                       ASSIGN seku = sekunder
   	                       sekunder = seku - sekun.
   	                       RUN SEKTIM.P.
                             ASSIGN TIDREGITAB.BERANTAL = nytid.
                          END.
                          ELSE IF tidbuff.OSL2 = 24.00 AND tidbuff.OSL3 > 0 THEN DO:
   	                       nytid = tidbuff.OSL3.
   	                       RUN TIMSEK.P.
   	                       ASSIGN sekun = sekunder
   	                       nytid = TIDREGITAB.BERANTAL.
   	                       RUN TIMSEK.P.
   	                       ASSIGN seku = sekunder
   	                       sekunder = seku - sekun.
   	                       RUN SEKTIM.P.
                             ASSIGN TIDREGITAB.BERANTAL = nytid.
                          END.
                       END.                             
 	               END. 
 	           END.   
	        END.	     	        
	        IF ohjalp.OSL1 = 24.00 AND ohjalp.OSL2 > 0 THEN DO:
                  FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONAL =
                  PERSONALTAB.PERSONALKOD AND TIDREGITAB.DATUM = ohjalp.ODATUM + 1 AND
                  TIDREGITAB.BEREDSKAPSTART = 00.00 AND TIDREGITAB.BEREDSKAPSLUT > 0
                  USE-INDEX PSTART NO-LOCK NO-ERROR.
                  IF AVAILABLE TIDREGITAB THEN DO TRANSACTION: 
                     avberrec = RECID(TIDREGITAB).
                     FIND TIDREGITAB WHERE RECID(TIDREGITAB) = avberrec EXCLUSIVE-LOCK NO-ERROR.
                     nytid = TIDREGITAB.BEREDSKAPSLUT.
                     RUN TIMSEK.P.
                     ASSIGN sekun = sekunder
                     nytid = TIDREGITAB.BEREDSKAPSTART.
                     RUN TIMSEK.P.
                     ASSIGN seku = sekunder
                     sekunder = sekun - seku.
                     RUN SEKTIM.P.
                     ASSIGN TIDREGITAB.BERANTAL = nytid.              
                  END.
               END.        
 	     END.
         END.
      END.
   END.
   /*TEST SAMMA TID*/          
   ost = 0.
   FIND FIRST ohjalp NO-LOCK NO-ERROR.
   tiden2 = ohjalp.OSLUT.
   IF ohjalp.OSL1 > 0 THEN tiden = ohjalp.OSL1.
   IF ohjalp.OSL2 > 0 THEN tiden = ohjalp.OSL2.
   IF ohjalp.OSL3 > 0 THEN tiden = ohjalp.OSL3.
   IF ohjalp.OSL3 > 0 AND ohjalp.OSL3 < ohjalp.OSL1 THEN DO: /*??*/
      ASSIGN nydag = TRUE
      ost = ohjalp.OST1.
   END.
   IF ohjalp.OSL2 > 0 AND ohjalp.OSL2 < ohjalp.OSL1 THEN DO: /*??*/
      ASSIGN nydag = TRUE
      ost = ohjalp.OST1.
   END.                  
   lapp2:
   REPEAT:
      odat1 = ohjalp.ODATUM.
      FIND NEXT ohjalp NO-ERROR.
      IF NOT AVAILABLE ohjalp THEN LEAVE lapp2. 
      IF nydag = FALSE AND  ohjalp.ODATUM > odat1 THEN DO:  /*NYTT*/
         IF ohjalp.OSL1 > 0 THEN tiden = ohjalp.OSL1.
         IF ohjalp.OSL2 > 0 THEN tiden = ohjalp.OSL2.
         IF ohjalp.OSL3 > 0 THEN tiden = ohjalp.OSL3.
         IF ohjalp.OSL3 > 0 AND ohjalp.OSL3 < ohjalp.OSL1 THEN DO: /*??*/
            ASSIGN nydag = TRUE
            ost = ohjalp.OST1.
         END.
         IF ohjalp.OSL2 > 0 AND ohjalp.OSL2 < ohjalp.OSL1 THEN DO: /*??*/
            ASSIGN nydag = TRUE
            ost = ohjalp.OST1.
         END.                  
      END.         
      IF tiden2 ne ohjalp.OSTART THEN DO:
         tiden2 = ohjalp.OSLUT.
         NEXT lapp2.
      END.   
      IF tiden = 24.00 AND ohjalp.OST1 = 00.00 THEN DO:
         IF ohjalp.OSL1 > ohjalp.OSLUT THEN DO:            
            ASSIGN ohjalp.OSL1 = ohjalp.OSLUT ohjalp.OANT1 = ohjalp.OSLUT.
         END.
      END.
      ELSE IF nydag = TRUE AND ohjalp.OSTART > ost THEN DO:  /*??*/                 
         ASSIGN ohjalp.OST1 = 0 ohjalp.OSL1 = 0 ohjalp.OANT1 = 0.
         nydag = FALSE.
      END.
      ELSE IF ohjalp.OST1 < tiden THEN DO:  /*överlappning*/         
         IF ohjalp.OSLUT > tiden THEN DO: /* VERKLIG TID ÖVERLAPPAR*/            
            IF ohjalp.OST2 > ohjalp.OST1 AND ohjalp.OST2 LE tiden THEN DO:               
               /*FÖRSTA KODEN ÖVERLAPPAS HELT. ANDRA FLYTTAS TILL FÖRSTA*/
               ASSIGN ohjalp.OKOD1 = ohjalp.OKOD2
               ohjalp.OST2 = 0 ohjalp.OSL2 = 0 ohjalp.OKOD2 = "".             
            END.               
            ASSIGN ohjalp.OST1 = tiden
   	      nytid = ohjalp.OSLUT.
   	      RUN TIMSEK.P.
   	      ASSIGN seku = sekunder
   	      nytid = ohjalp.OST1.
   	      RUN TIMSEK.P.
   	      sekunder = seku - sekunder.
   	      RUN SEKTIM.P.        
            ASSIGN ohjalp.OSL1 = ohjalp.OSLUT ohjalp.OANT1 = nytid.              
         END.
         ELSE IF ohjalp.OSL2 = 0 THEN DO:                       
	        ASSIGN ohjalp.OST1 = 0 ohjalp.OSL1 = 0 ohjalp.OANT1 = 0.
         END.
         ELSE IF ohjalp.OSL2 > 0 AND ohjalp.OSL2 > tiden THEN DO:
  	     ASSIGN ohjalp.OST1 = tiden
	     nytid = ohjalp.OSL1.
	     RUN TIMSEK.P.
	     ASSIGN seku = sekunder
	     nytid = ohjalp.OST2.
	     RUN TIMSEK.P.
	     sekunder = seku - sekunder.
	     RUN SEKTIM.P.       
	     ASSIGN ohjalp.OSL1 = ohjalp.OSL2 ohjalp.OANT1 = nytid
	     ohjalp.OSL2 = ohjalp.OSL3 ohjalp.OSL2 = ohjalp.OSL3
	     ohjalp.OANT2 = ohjalp.OANT3.
         END.
      END.
      tiden2 = ohjalp.OSLUT.
      IF ohjalp.OSL1 > 0 THEN tiden = ohjalp.OSL1.
      IF ohjalp.OSL2 > 0 THEN tiden = ohjalp.OSL2.
      IF ohjalp.OSL3 > 0 THEN tiden = ohjalp.OSL3.
      IF ohjalp.OSL3 > 0 AND ohjalp.OSL3 < ohjalp.OSL1 THEN DO: /*??*/
         ASSIGN nydag = TRUE
         ost = ohjalp.OST1.
      END.
      IF ohjalp.OSL2 > 0 AND ohjalp.OSL2 < ohjalp.OSL1 THEN DO: /*??*/
         ASSIGN nydag = TRUE
         ost = ohjalp.OST1.
      END.
   END.               
   IF UTRYCKNING.OVLAPP = TRUE THEN DO:
      ost = 0.                  
      FIND FIRST ohjalp NO-LOCK NO-ERROR.         
      IF AVAILABLE ohjalp THEN DO:         
         ASSIGN odat1 = ohjalp.ODATUM tid2 = ohjalp.OST1.
         IF ohjalp.OSL1 > 0 THEN tiden = ohjalp.OSL1.
         IF ohjalp.OSL2 > 0 THEN tiden = ohjalp.OSL2.
         IF ohjalp.OSL3 > 0 THEN tiden = ohjalp.OSL3.         
         IF ohjalp.OSL3 > 0 AND ohjalp.OSL3 < ohjalp.OSL1 THEN DO: /*??*/
            ASSIGN nydag = TRUE
            ost = ohjalp.OST1.
         END.
         IF ohjalp.OSL2 > 0 AND ohjalp.OSL2 < ohjalp.OSL1 THEN DO: /*??*/
            ASSIGN nydag = TRUE
            ost = ohjalp.OST1.
         END.      
      END.   
      lapp:
      REPEAT:            
         FIND NEXT ohjalp NO-ERROR.            
         IF NOT AVAILABLE ohjalp THEN LEAVE lapp.            
         IF tiden = 24.00 AND ohjalp.OST1 = 00.00 THEN DO:                           
            sekunder = 86400 - utrber.
            RUN SEKTIM.P.            
	         IF ohjalp.OSL1 > ohjalp.OSLUT AND tid2 LE nytid THEN DO:               
	            ASSIGN ohjalp.OSL1 = ohjalp.OSLUT ohjalp.OANT1 = ohjalp.OSLUT.               
	         END.
         END.
         ELSE IF tiden = 24.00 AND ohjalp.OSTART > ost AND nydag = TRUE THEN DO:                                    
            ASSIGN ohjalp.OST1 = 0 ohjalp.OSL1 = 0 ohjalp.OANT1 = 0.
	         ASSIGN ohjalp.OST2 = 0 ohjalp.OSL2 = 0 ohjalp.OANT2 = 0.
	      END.
         ELSE IF nydag = TRUE AND ohjalp.ODATUM = odat1 AND ohjalp.OSTART > ost THEN DO:                                    
            /*ny överlappning graninge*/
            IF ohjalp.OSL2 > tiden THEN DO:            
               ASSIGN
               ohjalp.OST1 = tiden ohjalp.OSL1 = ohjalp.OSL2 ohjalp.OKOD1 = ohjalp.OKOD2
               ohjalp.OST2 = 0 ohjalp.OSL2 = 0 ohjalp.OANT2 = 0.	
               nytid = ohjalp.OST1.
               RUN TIMSEK.P.
               seku = sekunder.
               nytid = ohjalp.OSL1.
               RUN TIMSEK.P.
               sekunder = sekunder - seku.
               RUN SEKTIM.P.
               ASSIGN ohjalp.OANT1 = nytid.
            END.
         END.
         ELSE IF ohjalp.OST1 < tiden AND ohjalp.ODATUM = odat1 THEN DO:                                               
            IF ohjalp.OSL2 > ohjalp.OSLUT THEN DO:                            
               nytid = ohjalp.OSL2.
	            RUN TIMSEK.P.
	            ASSIGN seku = sekunder
	            nytid = ohjalp.OST2.
	            RUN TIMSEK.P.
	            sekunder = seku - sekunder.
	            RUN SEKTIM.P.
               ASSIGN ohjalp.OANT2 = nytid ohjalp.OST3 = 0
               ohjalp.OSL3 = 0 ohjalp.OANT3 = 0.
            END.                                       
            IF nydag = FALSE AND ohjalp.ODATUM = odat1 AND
            ohjalp.OSL1 > tid2 THEN DO:                              
	            IF ohjalp.OSL1 > tiden THEN DO:                  
                  ASSIGN ohjalp.OST1 = tiden.                  
	               IF ohjalp.OST2 > ohjalp.OST1 AND ohjalp.OST2 LE tiden THEN DO:	              
                     ASSIGN ohjalp.OKOD1 = ohjalp.OKOD2
                     ohjalp.OST2 = 0 ohjalp.OSL2 = 0 ohjalp.OKOD2 = ""
                     ohjalp.OANT2 = 0.    
                     nytid = ohjalp.OSLUT.                     
                  END. 
                  ELSE IF ohjalp.OST2 > 0 AND ohjalp.OST2 < ohjalp.OST1 AND ohjalp.OST2 LE tiden THEN DO:	              
                     ASSIGN ohjalp.OKOD1 = ohjalp.OKOD2
                     ohjalp.OST2 = 0 ohjalp.OSL2 = 0 ohjalp.OKOD2 = ""
                     ohjalp.OANT2 = 0.    
                     nytid = ohjalp.OSLUT.                     
                  END. 
	               ELSE nytid = ohjalp.OSL1.	               
	               RUN TIMSEK.P.
	               ASSIGN seku = sekunder
	               nytid = ohjalp.OST1.
	               RUN TIMSEK.P.
	               sekunder = seku - sekunder.
	               RUN SEKTIM.P.                  	             
                  ASSIGN ohjalp.OANT1 = nytid.                  
   	         END.
   	         ELSE IF ohjalp.OSL2 = 0 THEN DO:                                    
                  /*tolka så att det blir utryckning med överlappning*/
                  IF tiden LE ohjalp.OSL1 THEN DO:                 
                     ohjalp.OST1 = tiden.
                     nytid = ohjalp.OST1.
   	               RUN TIMSEK.P.
   	               ASSIGN seku = sekunder
   	               nytid = ohjalp.OSL1.
   	               RUN TIMSEK.P.
   	               sekunder = sekunder - seku.
   	               RUN SEKTIM.P.                  	               
                     ASSIGN ohjalp.OANT1 = nytid.
                  END.                  
   	         END.
   	         ELSE IF ohjalp.OSL2 > 0 AND ohjalp.OSL2 > tiden THEN DO:                                             
                  ASSIGN ohjalp.OST1 = tiden.
                  nytid = ohjalp.OST1.
                  RUN TIMSEK.P.
                  ASSIGN seku = sekunder
                  nytid = ohjalp.OSL2.
                  RUN TIMSEK.P.
                  sekunder = sekunder - seku.
                  RUN SEKTIM.P.                  
                  ASSIGN ohjalp.OSL1 = ohjalp.OSL2 ohjalp.OANT1 = nytid ohjalp.OKOD1 = ohjalp.OKOD2
                  ohjalp.OST2 = ohjalp.OST3 ohjalp.OSL2 = ohjalp.OSL3
                  ohjalp.OANT2 = ohjalp.OANT3 ohjalp.OKOD2 = ohjalp.OKOD3.                  
   	         END.                                            
   	      END.  
   	      ELSE IF nydag = TRUE THEN DO:                    
   	         nydag = FALSE.
   	         IF ohjalp.OSLUT > tiden THEN DO:                  
   	            ASSIGN ohjalp.OST1 = tiden.
   	            nytid = ohjalp.OSLUT.
   	            RUN TIMSEK.P.
   	            ASSIGN seku = sekunder
   	            nytid = ohjalp.OST1.
   	            RUN TIMSEK.P.
   	            sekunder = seku - sekunder.
   	            RUN SEKTIM.P.                  
   	            ASSIGN ohjalp.OSL1 = ohjalp.OSLUT ohjalp.OANT1 = nytid.
   	         END.
   	         ELSE IF ohjalp.OSL2 = 0 THEN DO:                                                      
   	            ASSIGN ohjalp.OST1 = 0 ohjalp.OSL1 = 0 ohjalp.OANT1 = 0.
   	         END.	     
   	         ELSE IF ohjalp.OSL2 > 0 AND ohjalp.OSL2 > tiden THEN DO:                  
   	            ASSIGN ohjalp.OST1 = tiden.
   	            nytid = ohjalp.OSL1.
   	            RUN TIMSEK.P.
   	            ASSIGN seku = sekunder
   	            nytid = ohjalp.OST2.
   	            RUN TIMSEK.P.
   	            sekunder = seku - sekunder.
   	            RUN SEKTIM.P.                  
   	            ASSIGN ohjalp.OSL1 = ohjalp.OSL2 ohjalp.OANT1 = nytid
   	            ohjalp.OSL2 = ohjalp.OSL3 ohjalp.OSL2 = ohjalp.OSL3
   	            ohjalp.OANT2 = ohjalp.OANT3.
   	         END.                                            
	         END.
         END.              
         ELSE IF ohjalp.OST1 < tiden AND nydag = TRUE AND ohjalp.ODATUM = odat1 + 1 THEN DO:                        
            /*tex 23.45-24 0-0.30 skall ej ge något*/
            IF tiden GE ohjalp.OSL1 THEN DO:                              
               ASSIGN ohjalp.OST1 = 0 ohjalp.OSL1 = 0 ohjalp.OANT1 = 0.
               ASSIGN ohjalp.OST2 = 0 ohjalp.OSL2 = 0 ohjalp.OANT2 = 0.	
            END.
            ELSE IF tiden GE ohjalp.OST1 AND tiden < ohjalp.OSL1 THEN DO:
            /* 23-24  0-4 skall ej ge 3 + 4 timmar utan 3 + 2*/
               nytid = ohjalp.OSL1.
               RUN TIMSEK.P.
               seku = sekunder.
               nytid = tiden.
               RUN TIMSEK.P.
               sekunder = seku - sekunder.
               RUN SEKTIM.P.
               ASSIGN ohjalp.OST1 = tiden ohjalp.OANT1 = nytid.
            END.
         END.
         IF ohjalp.OSL1 > 0 THEN tiden = ohjalp.OSL1.
         IF ohjalp.OSL2 > 0 THEN tiden = ohjalp.OSL2.
         IF ohjalp.OSL3 > 0 THEN tiden = ohjalp.OSL3.
         odat1 = ohjalp.ODATUM.  
         tid2 = ohjalp.OST1.
         IF ohjalp.OSL3 > 0 AND ohjalp.OSL3 < ohjalp.OSL1 THEN DO: /*??*/
            ASSIGN nydag = TRUE
	     ost = ohjalp.OST1.
         END.
         IF ohjalp.OSL2 > 0 AND ohjalp.OSL2 < ohjalp.OSL1 THEN DO: /*??*/
            ASSIGN nydag = TRUE
	     ost = ohjalp.OST1.
         END.
      END.
   END.
   ELSE DO:  
      FIND FIRST obuff WHERE obuff.OSLUT = 24.00 NO-LOCK NO-ERROR.   
      IF AVAILABLE obuff THEN DO:         
         FIND FIRST ohjalp WHERE ohjalp.OSTART = 00.00 AND
         ohjalp.OSLUT > 00.00 NO-LOCK NO-ERROR.   
         IF AVAILABLE ohjalp THEN DO: 
            IF obuff.OSL1 = 24 AND obuff.OSL2 > 0 THEN DO:
               IF ohjalp.OSLUT < obuff.OSL2 THEN DO:                  
                  ASSIGN ohjalp.OST1 = 0 ohjalp.OSL1 = 0 ohjalp.OANT1 = 0.
               END.
               ELSE IF ohjalp.OSLUT > obuff.OSL2 THEN DO:
                  ASSIGN ohjalp.OST1 = obuff.OSL2.
                  nytid = ohjalp.OSL1.
                  RUN TIMSEK.P.
                  ASSIGN
                  seku = sekunder
                  nytid = ohjalp.OST1.
                  RUN TIMSEK.P.
                  sekunder = seku - sekunder.
                  RUN SEKTIM.P.
                  ASSIGN ohjalp.OANT1 = nytid.
               END.
            END.     
            IF obuff.OSL2 = 24 AND obuff.OSL3 > 0 THEN DO:
               IF ohjalp.OSLUT < obuff.OSL3 THEN DO:                  
                  ASSIGN ohjalp.OST1 = 0 ohjalp.OSL1 = 0 ohjalp.OANT1 = 0.
               END.
               ELSE IF ohjalp.OSLUT > obuff.OSL3 THEN DO:
                  ASSIGN ohjalp.OST1 = obuff.OSL3.
                  nytid = ohjalp.OSL1.
                  RUN TIMSEK.P.
                  ASSIGN
                  seku = sekunder
                  nytid = ohjalp.OST1.
                  RUN TIMSEK.P.
                  sekunder = seku - sekunder.
                  RUN SEKTIM.P.
                  ASSIGN ohjalp.OANT1 = nytid.
               END.
            END.
            /*OBS NYTT 981222 LENA UTRYCKNING 21-02 SKALL EJ GE 6 TIMMAR*/ 
            sekunder = 86400 - utrber.
            RUN SEKTIM.P.
            IF obuff.OSL1 = 24 AND obuff.OST1 = nytid THEN DO:
               IF ohjalp.OST1 = 0 AND ohjalp.OSL1 > 0 
               AND ohjalp.OSL1 > ohjalp.OSLUT THEN DO:
                  ASSIGN ohjalp.OSL1 = ohjalp.OSLUT
                  nytid = ohjalp.OSL1.
                  RUN TIMSEK.P.
                  ASSIGN
                  seku = sekunder
                  nytid = ohjalp.OST1.
                  RUN TIMSEK.P.
                  sekunder = seku - sekunder.
                  RUN SEKTIM.P.
                  ASSIGN ohjalp.OANT1 = nytid.
               END.   
            END.  
         END. 
      END.                         
   END.
     
   FOR EACH ohjalp NO-LOCK:         
      DO TRANSACTION:  
         FIND FIRST TIDREGITAB WHERE RECID(TIDREGITAB) = ohjalp.RECTIDVIS
         EXCLUSIVE-LOCK NO-ERROR.
         IF AVAILABLE TIDREGITAB THEN DO :
            ASSIGN TIDREGITAB.OST1 = ohjalp.OST1 TIDREGITAB.OSL1 = ohjalp.OSL1
            TIDREGITAB.OKOD1 = ohjalp.OKOD1 TIDREGITAB.OANT1 = ohjalp.OANT1
            TIDREGITAB.OST2 = ohjalp.OST2 TIDREGITAB.OSL2 = ohjalp.OSL2
            TIDREGITAB.OKOD2 = ohjalp.OKOD2 TIDREGITAB.OANT2 = ohjalp.OANT2
            TIDREGITAB.OST3 = ohjalp.OST3 TIDREGITAB.OSL3 = ohjalp.OSL3
            TIDREGITAB.OKOD3 = ohjalp.OKOD3 TIDREGITAB.OANT3 = ohjalp.OANT3.                           
         END. 
      END.       
   END.  
   /*   ENDAST OM BEREDSKAP REDUCERAR OVERTID  */
   FIND FIRST BEREDSKAPTAB WHERE BEREDSKAPTAB.BEREDSKAPSAVTAL =
   PERSONALTAB.BEREDSKAPSAVTAL USE-INDEX BERED NO-LOCK NO-ERROR.
   IF BEREDSKAPTAB.BERANTAL > 0 THEN regdatum = regdatum.  
   ELSE IF UTRYCKNING.AVBE = FALSE THEN regdatum = regdatum.
   ELSE DO:   
      FIND FIRST ohjalp NO-LOCK NO-ERROR.
      reco = ohjalp.RECTIDVIS.  
      FOR EACH TIDREGITAB WHERE TIDREGITAB.PERSONAL = PERSONALTAB.PERSONALKOD
      AND TIDREGITAB.DATUM = ohjalp.ODATUM USE-INDEX PSTART NO-LOCK: 
         IF ohjalp.OSTART GE TIDREGITAB.BEREDSKAPSTART AND
	  ohjalp.OSTART < TIDREGITAB.BEREDSKAPSLUT THEN  krav2 = TRUE.
      END.            
      IF krav2 = true THEN ber1 = utrber.
      ELSE ber1 = utrejber.              
      IF krav2 = TRUE THEN DO:         
         RUN AVBER.P.
      END.
      avdra:
      REPEAT:
 	  FIND NEXT ohjalp NO-LOCK NO-ERROR.
 	  IF NOT AVAILABLE ohjalp THEN LEAVE avdra.
 	  reco = ohjalp.RECTIDVIS.
	  IF UTRYCKNING.AVBE = FALSE THEN krav2 = FALSE.
	  ELSE DO:	    
	     FOR EACH TIDREGITAB WHERE TIDREGITAB.PERSONAL = PERSONALTAB.PERSONALKOD
	     AND TIDREGITAB.DATUM = ohjalp.ODATUM USE-INDEX PSTART NO-LOCK:
	        IF ohjalp.OSTART GE TIDREGITAB.BEREDSKAPSTART AND
	        ohjalp.OSTART < TIDREGITAB.BEREDSKAPSLUT THEN krav2 = TRUE.
  	     END.
  	  END.    	     	 
     IF krav2 = true THEN ber1 = utrber.
	  ELSE ber1 = utrejber.		  
	  IF krav2 = TRUE THEN DO:	     
        RUN AVBER.P.
	  END.
      END.     
   END.            
END.
