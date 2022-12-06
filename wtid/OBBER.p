 /*OBBER.P BER AV OBEKVAM ARBETSTID */
&Scoped-define NEW NEW

&Scoped-define NEW 
{TIDALLT.I}
{REGVAR.I}
DEFINE SHARED VARIABLE persrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE tidtabrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE extraaonr LIKE TIDREGITAB.AONR NO-UNDO.
DEFINE SHARED VARIABLE extradnr LIKE TIDREGITAB.DELNR NO-UNDO.
/*DEFINE SHARED VARIABLE globanv LIKE ANVANDARE.ANVANDARE NO-UNDO.*/

DEFINE NEW SHARED VARIABLE seku AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE slutar1 LIKE TIDREGITAB.SLUT NO-UNDO.
DEFINE NEW SHARED VARIABLE startar1 LIKE TIDREGITAB.START NO-UNDO.
DEFINE NEW SHARED VARIABLE slutar2 LIKE TIDREGITAB.SLUT NO-UNDO.
DEFINE NEW SHARED VARIABLE startar2 LIKE TIDREGITAB.START NO-UNDO.
DEFINE NEW SHARED VARIABLE slutar3 LIKE TIDREGITAB.SLUT NO-UNDO.
DEFINE NEW SHARED VARIABLE startar3 LIKE TIDREGITAB.START NO-UNDO.
DEFINE NEW SHARED VARIABLE obkod1 LIKE TIDREGITAB.LONTILLAGG NO-UNDO.
DEFINE NEW SHARED VARIABLE obkod2 LIKE TIDREGITAB.LONTILLAGG NO-UNDO.
DEFINE NEW SHARED VARIABLE obkod3 LIKE TIDREGITAB.LONTILLAGG NO-UNDO.
DEFINE VARIABLE obkodsj1 LIKE TIDREGITAB.LONTILLAGG NO-UNDO.
DEFINE VARIABLE obkodsj2 LIKE TIDREGITAB.LONTILLAGG NO-UNDO.
DEFINE VARIABLE obkodsj3 LIKE TIDREGITAB.LONTILLAGG NO-UNDO.
DEFINE VARIABLE sl1 AS DECIMAL NO-UNDO.
DEFINE VARIABLE st1 AS DECIMAL NO-UNDO.
DEFINE VARIABLE regdatum2 AS DATE NO-UNDO.
DEFINE VARIABLE regdahj1 AS DATE NO-UNDO.
DEFINE VARIABLE odatum AS DATE NO-UNDO.
DEFINE VARIABLE oblon1 AS DECIMAL NO-UNDO.
DEFINE VARIABLE oblon2 AS DECIMAL NO-UNDO.
DEFINE VARIABLE oblon3 AS DECIMAL NO-UNDO.
DEFINE VARIABLE antal AS INTEGER NO-UNDO.
DEFINE VARIABLE obhjalp AS INTEGER NO-UNDO. 
DEFINE VARIABLE onr AS CHARACTER NO-UNDO.
DEFINE VARIABLE dnr AS INTEGER NO-UNDO.
DEFINE VARIABLE regdatumspar AS DATE NO-UNDO.
DEFINE VARIABLE ejob AS LOGICAL NO-UNDO.
DEFINE VARIABLE oblonsj1 AS DECIMAL NO-UNDO.
DEFINE VARIABLE oblonsj2 AS DECIMAL NO-UNDO.
DEFINE VARIABLE oblonsj3 AS DECIMAL NO-UNDO.
DEFINE VARIABLE hjdat AS DATE NO-UNDO.
DEFINE TEMP-TABLE obfil
   FIELD OBSTART LIKE TIDREGITAB.START
   FIELD OBSLUT LIKE TIDREGITAB.SLUT
   FIELD OBST1 LIKE TIDREGITAB.START
   FIELD OBSL1 LIKE TIDREGITAB.SLUT
   FIELD OLONKOD LIKE TIDREGITAB.LONTILLAGG
   FIELD OLONANT LIKE TIDREGITAB.LONTILLANTAL
   FIELD AONR AS CHARACTER
   FIELD DELNR AS INTEGER
   FIELD KARENS AS LOGICAL
   FIELD OBDATUM LIKE TIDREGITAB.DATUM.

DEFINE TEMP-TABLE obsum   
   FIELD OLONKOD LIKE TIDREGITAB.LONTILLAGG
   FIELD OLONANT LIKE TIDREGITAB.LONTILLANTAL
   FIELD AONR AS CHARACTER
   FIELD DELNR AS INTEGER
   FIELD OBDATUM LIKE TIDREGITAB.DATUM.
    
DEFINE BUFFER tidbuff FOR TIDREGITAB.
DEFINE BUFFER obbuff FOR obfil.   
DEFINE QUERY traktq FOR TIDREGITAB.   


FUNCTION klock100 RETURNS DECIMAL
   ( INPUT ber60 AS DECIMAL ):
   RETURN  (TRUNCATE(ber60,0) * 3600 + (ber60 - TRUNCATE(ber60,0)) * 100 * 60) / 3600.
END FUNCTION.

FUNCTION klock60 RETURNS DECIMAL
   ( INPUT ber100 AS DECIMAL ) :
   RETURN TRUNCATE(ber100,0) + ((ber100 - TRUNCATE(ber100,0)) * 60 / 100 ).   /* Function return value. */
END FUNCTION.
   
FIND FIRST FORETAG NO-LOCK NO-ERROR.
   Guru.Konstanter:globforetag = FORETAG.FORETAG.
&Scoped-define NEW NEW
{FORESTYR.I}
EMPTY TEMP-TABLE obfil NO-ERROR. 
FIND TIDREGITAB WHERE RECID(TIDREGITAB) = tidtabrec NO-LOCK NO-ERROR.
IF AVAILABLE TIDREGITAB THEN DO:
   ASSIGN
   onr = TIDREGITAB.AONR
   dnr = TIDREGITAB.DELNR.
   IF TIDREGITAB.PRISTYP = "FRÅNVARO." THEN DO: 
      FIND PERSONALTAB WHERE RECID(PERSONALTAB) = persrec NO-LOCK NO-ERROR.
      FIND FIRST tidbuff WHERE tidbuff.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
      tidbuff.DATUM = regdatum AND tidbuff.TIDLOG = TRUE AND tidbuff.PRISTYP NE "FRÅNVARO." NO-LOCK NO-ERROR.
      IF AVAILABLE tidbuff THEN DO:
         ASSIGN
         onr = tidbuff.AONR
         dnr = tidbuff.DELNR.
      END.
   END.
END.   
ELSE DO:
   /*FRÅN BORTTID.W*/  
   ASSIGN
   onr = extraaonr
   dnr = extradnr.
   FIND FIRST AONRTAB WHERE AONRTAB.AONR = onr AND AONRTAB.DELNR = dnr NO-LOCK NO-ERROR.
   IF AVAILABLE AONRTAB THEN DO:
      IF AONRTAB.PRISTYP =  "FRÅNVARO." THEN onr = "".
   END.
END.   
FIND PERSONALTAB WHERE RECID(PERSONALTAB) = persrec NO-LOCK NO-ERROR.
FIND FIRST ANSTFORMTAB WHERE ANSTFORMTAB.ANSTALLNING = PERSONALTAB.ANSTALLNING
USE-INDEX ANSTF NO-LOCK NO-ERROR.
FIND FIRST UTRYCKNING WHERE UTRYCKNING.KOD = ANSTFORMTAB.KOD USE-INDEX UT NO-LOCK NO-ERROR.
IF NOT AVAILABLE UTRYCKNING THEN DO TRANSACTION:
   CREATE FELTEXT.
   ASSIGN 
   FELTEXT.ANVANDARE = Guru.Konstanter:globanv                      
   FELTEXT.FELTEXT = 'Kontakta Elpool! För nu är det något fel! Ange läge 23.'
   FELTEXT.PROGRAM = "OBBER" + STRING(TODAY) + STRING(TIME,"HH:MM") + Guru.Konstanter:globanv. 
   RETURN.
END.
IF UTRYCKNING.OB = FALSE THEN RETURN.
ELSE DO:
   ASSIGN
   obhjalp = 0
   regdatum2 = regdatum
   regdahj1= regdatum.
   RUN REGVEC.P.
   RUN REGDAG.P.
   RUN SLUTARB.P.
   RUN OBTID.P.      
   ASSIGN
   obkodsj1 = ""
   obkodsj2 = ""
   obkodsj3 = "".
   IF Guru.Konstanter:globforetag = "GKAL" OR Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT"  OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "elpa" THEN DO:   
      IF Guru.Konstanter:globforetag = "elpa" THEN DO:   
         IF obkod1 = "414" THEN obkodsj1 = "417". 
         IF obkod1 = "415" THEN obkodsj1 = "418". 
         IF obkod1 = "416" THEN obkodsj1 = "419". 
         IF obkod2 = "414" THEN obkodsj2 = "417". 
         IF obkod2 = "415" THEN obkodsj2 = "418". 
         IF obkod2 = "416" THEN obkodsj2 = "419". 
         IF obkod3 = "414" THEN obkodsj3 = "417". 
         IF obkod3 = "415" THEN obkodsj3 = "418". 
         IF obkod3 = "416" THEN obkodsj3 = "419". 
      END.
      IF Guru.Konstanter:globforetag = "GKAL"  THEN DO:   
         IF obkod1 = "242" THEN obkodsj1 = "VASJ". 
         IF obkod1 = "243" THEN obkodsj1 = "VESJ". 
         IF obkod1 = "244" THEN obkodsj1 = "STSJ". 
         IF obkod2 = "242" THEN obkodsj2 = "VASJ". 
         IF obkod2 = "243" THEN obkodsj2 = "VESJ". 
         IF obkod2 = "244" THEN obkodsj2 = "STSJ". 
         IF obkod3 = "242" THEN obkodsj3 = "VASJ". 
         IF obkod3 = "243" THEN obkodsj3 = "VESJ". 
         IF obkod3 = "244" THEN obkodsj3 = "STSJ". 
      END.
      IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT"   OR Guru.Konstanter:globforetag = "MISV" THEN DO:   
         IF obkod1 = "4105" THEN obkodsj1 = "314". 
         IF obkod1 = "4106" THEN obkodsj1 = "315". 
         IF obkod1 = "4107" THEN obkodsj1 = "316". 
         IF obkod2 = "4105" THEN obkodsj2 = "314". 
         IF obkod2 = "4106" THEN obkodsj2 = "315". 
         IF obkod2 = "4107" THEN obkodsj2 = "316". 
         IF obkod3 = "4105" THEN obkodsj3 = "314". 
         IF obkod3 = "4106" THEN obkodsj3 = "315". 
         IF obkod3 = "4107" THEN obkodsj3 = "316". 
      END.
      IF obkodsj1 NE "" THEN DO:              
         FOR EACH TIDREGITAB WHERE	        
         TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
         TIDREGITAB.DATUM = regdatum AND TIDREGITAB.LONTILLAGG = obkodsj1 AND
         TIDREGITAB.LONAUTO = TRUE AND TIDREGITAB.GODKAND = "" /*AND TIDREGITAB.TIDLOG = FALSE*/ EXCLUSIVE-LOCK:                  
            IF TIDREGITAB.TIDLOG = FALSE THEN DELETE TIDREGITAB.
            ELSE DO:
               ASSIGN
               TIDREGITAB.LONTILLAGG = ""
               TIDREGITAB.LONTILLANTAL = 0.
            END.
         END.               
      END.
      IF obkodsj2 NE "" THEN DO:              
         FOR EACH TIDREGITAB WHERE	        
         TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
         TIDREGITAB.DATUM = regdatum AND TIDREGITAB.LONTILLAGG = obkodsj2 AND
         TIDREGITAB.LONAUTO = TRUE AND TIDREGITAB.GODKAND = "" /*AND TIDREGITAB.TIDLOG = FALSE*/ EXCLUSIVE-LOCK:                  
            IF TIDREGITAB.TIDLOG = FALSE THEN DELETE TIDREGITAB.
            ELSE DO:
               ASSIGN
               TIDREGITAB.LONTILLAGG = ""
               TIDREGITAB.LONTILLANTAL = 0.
            END.
         END.               
      END.
      IF obkodsj3 NE "" THEN DO:              
         FOR EACH TIDREGITAB WHERE	        
         TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
         TIDREGITAB.DATUM = regdatum AND TIDREGITAB.LONTILLAGG = obkodsj3 AND
         TIDREGITAB.LONAUTO = TRUE AND TIDREGITAB.GODKAND = "" /*AND TIDREGITAB.TIDLOG = FALSE*/ EXCLUSIVE-LOCK:                  
            IF TIDREGITAB.TIDLOG = FALSE THEN DELETE TIDREGITAB.
            ELSE DO:
               ASSIGN
               TIDREGITAB.LONTILLAGG = ""
               TIDREGITAB.LONTILLANTAL = 0.
            END.
         END.               
      END.
   END.   
   
   IF startar1 > 0 OR slutar1 > 0 THEN DO:      
      IF regslut LE slutar1 AND regslut > startar1 OR 
      regstart GE startar1 AND regstart < slutar1 OR 
      regstart < startar1 AND regslut > slutar1 THEN DO:         
         OPEN QUERY traktq
         FOR EACH TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
         TIDREGITAB.DATUM = regdatum2 AND TIDREGITAB.START GE regstart AND
         TIDREGITAB.SLUT LE regslut AND TIDREGITAB.TIDLOG = TRUE /*AND TIDREGITAB.PRISTYP NE "FRÅNVARO."*/
         USE-INDEX PSTART NO-LOCK.
         GET FIRST traktq NO-LOCK. 
         IF NOT AVAILABLE TIDREGITAB THEN DO:              
            IF obkod1 NE "" THEN DO TRANSACTION:	        
               FOR EACH TIDREGITAB WHERE	        
	            TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
               TIDREGITAB.DATUM = regdatum AND TIDREGITAB.LONTILLAGG = obkod1 AND
               TIDREGITAB.LONAUTO = TRUE AND TIDREGITAB.TIDLOG = FALSE AND TIDREGITAB.GODKAND = "" EXCLUSIVE-LOCK:                  
                  DELETE TIDREGITAB.
               END.
               IF obkodsj1 NE "" THEN DO:              
                  FOR EACH TIDREGITAB WHERE	        
   	            TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
                  TIDREGITAB.DATUM = regdatum AND TIDREGITAB.LONTILLAGG = obkodsj1 AND
                  TIDREGITAB.LONAUTO = TRUE AND TIDREGITAB.TIDLOG = FALSE AND TIDREGITAB.GODKAND = "" EXCLUSIVE-LOCK:                  
                     DELETE TIDREGITAB.
                  END.               
               END.
               /*Tillägg pga PROBLEM MED ATT TA BORT OLIKA LÖNEARTER*/
               IF obkod2 NE "" THEN DO:               
                  FOR EACH TIDREGITAB WHERE	        
   	            TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
                  TIDREGITAB.DATUM = regdatum AND TIDREGITAB.LONTILLAGG = obkod2 AND
                  TIDREGITAB.LONAUTO = TRUE AND TIDREGITAB.TIDLOG = FALSE AND TIDREGITAB.GODKAND = "" EXCLUSIVE-LOCK:                     
                     DELETE TIDREGITAB.                     
                  END.
                  IF obkodsj2 NE "" THEN DO:               
                    FOR EACH TIDREGITAB WHERE	        
      	            TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
                     TIDREGITAB.DATUM = regdatum AND TIDREGITAB.LONTILLAGG = obkodsj2 AND
                     TIDREGITAB.LONAUTO = TRUE AND TIDREGITAB.TIDLOG = FALSE AND TIDREGITAB.GODKAND = "" EXCLUSIVE-LOCK:                     
                        DELETE TIDREGITAB.                     
                     END.
                  END.
               END.
            END.
            RETURN.
         END.         
         DO WHILE AVAILABLE(TIDREGITAB): 
           IF Guru.Konstanter:varforetypval[30] = 0 AND TIDREGITAB.PRISTYP = "FRÅNVARO."  THEN .
           ELSE IF TIDREGITAB.PRISTYP = "FRÅNVARO." AND TIDREGITAB.AONR NE "110" THEN.
           ELSE DO:           
              ejob = FALSE.
              /*skift ej ob på lunchen*/
              IF regstart = 0 AND regslut = 24 THEN DO:
                 IF TIDREGITAB.START GE lunchstarten AND  TIDREGITAB.SLUT LE lunchslutet THEN ejob = TRUE.
              END.
              IF ejob = TRUE THEN ejob = FALSE.
              ELSE DO:       
         	    IF TIDREGITAB.START > startar1 THEN st1 = TIDREGITAB.START.
         	    ELSE st1 = startar1.
                /*20100512 AND TIDREGITAB.SLUT GE startar1*/     
         	    IF TIDREGITAB.SLUT < slutar1 THEN sl1 = TIDREGITAB.SLUT.                
             	 ELSE sl1 = slutar1.
                IF Guru.Konstanter:globforetag = "GKAL" OR Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:
                   IF TIDREGITAB.SLUT > startar2 AND TIDREGITAB.SLUT LE slutar2 
                   AND slutar2 NE startar2 THEN DO:
                      IF obkod1 = obkod2 THEN DO:           
                         IF TIDREGITAB.SLUT < slutar2 THEN sl1 = TIDREGITAB.SLUT.
                         ELSE sl1 = slutar2.              
                      END.
                      ELSE DO:              
                          CREATE obfil.
                          ASSIGN obfil.OBSTART = TIDREGITAB.START 
                          obfil.OBSLUT = TIDREGITAB.SLUT
                          obfil.OBDATUM = TIDREGITAB.DATUM       	     
                          obfil.OLONKOD = obkod2
                          obfil.AONR = TIDREGITAB.AONR
                          obfil.DELNR = TIDREGITAB.DELNR.
                          IF TIDREGITAB.AONR = "110" THEN  obfil.OLONKOD = obkodsj2.                                                                                        
                          st1 = startar2.
                          IF TIDREGITAB.SLUT < slutar2 THEN sl1 = TIDREGITAB.SLUT.
                          ELSE sl1 = slutar2.                 
                          ASSIGN
                          obfil.OBST1 = st1 obfil.OBSL1 = sl1.                    
                          IF TIDREGITAB.START > startar1 THEN st1 = TIDREGITAB.START.
                          ELSE st1 = startar1.
                          sl1 = slutar1.
                      END.
                       
                   END.
                END.          
                IF TIDREGITAB.SLUT < st1  THEN st1 = st1.
                ELSE DO:          
                   IF sl1 > st1 THEN DO:                   
                      CREATE obfil.
                      ASSIGN obfil.OBSTART = TIDREGITAB.START 
                      obfil.OBSLUT = TIDREGITAB.SLUT
                      obfil.OBDATUM = TIDREGITAB.DATUM 
                      obfil.OBST1 = st1 obfil.OBSL1 = sl1
                      obfil.OLONKOD = obkod1
                      obfil.AONR = TIDREGITAB.AONR
                      obfil.DELNR = TIDREGITAB.DELNR.                    
                      IF TIDREGITAB.AONR = "110" THEN  obfil.OLONKOD = obkodsj1.                
                   END.
                END.
             END.
           END.
          GET NEXT traktq NO-LOCK. 
         END.
         CLOSE QUERY traktq.         
      END.
   END.   
   IF startar2 > 0 OR slutar2 > 0 THEN DO:      
      IF regslut LE slutar2 AND regslut > startar2 OR 
      regstart GE startar2 AND regstart < slutar2 OR 
      regstart < startar2 AND regslut > slutar2 THEN DO:     
         OPEN QUERY traktq
         FOR EACH TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
         TIDREGITAB.DATUM = regdatum2 AND TIDREGITAB.START GE regstart AND
         TIDREGITAB.SLUT LE regslut AND TIDREGITAB.TIDLOG = TRUE /*AND TIDREGITAB.PRISTYP NE "FRÅNVARO."*/
         USE-INDEX PSTART NO-LOCK.
         GET FIRST traktq NO-LOCK. 
         IF NOT AVAILABLE TIDREGITAB THEN DO:
   	     IF obkod2 NE "" THEN DO TRANSACTION:   	        
              FOR EACH TIDREGITAB WHERE 
   	        TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
   	        TIDREGITAB.DATUM = regdatum AND TIDREGITAB.LONTILLAGG = obkod2 AND
   	        TIDREGITAB.LONAUTO = TRUE AND TIDREGITAB.TIDLOG = FALSE AND TIDREGITAB.GODKAND = "" EXCLUSIVE-LOCK:
                 DELETE TIDREGITAB.
              END.
              IF obkodsj2 NE "" THEN DO:              
                 FOR EACH TIDREGITAB WHERE 
      	        TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
      	        TIDREGITAB.DATUM = regdatum AND TIDREGITAB.LONTILLAGG = obkodsj2 AND
      	        TIDREGITAB.LONAUTO = TRUE AND TIDREGITAB.TIDLOG = FALSE AND TIDREGITAB.GODKAND = "" EXCLUSIVE-LOCK:
                    DELETE TIDREGITAB.
                 END.
              END.
   	     END.
   	     RETURN.
        END.
        DO WHILE AVAILABLE(TIDREGITAB):   
        IF Guru.Konstanter:varforetypval[30] = 0 AND TIDREGITAB.PRISTYP = "FRÅNVARO."  THEN .
        ELSE IF TIDREGITAB.PRISTYP = "FRÅNVARO." AND TIDREGITAB.AONR NE "110" THEN.
        ELSE DO:        
           ejob = FALSE.
           /*skift ej ob på lunchen*/
           IF regstart = 0 AND regslut = 24 THEN DO:
              IF TIDREGITAB.START GE lunchstarten AND  TIDREGITAB.SLUT LE lunchslutet THEN ejob = TRUE.
           END.
           IF ejob = TRUE THEN ejob = FALSE.
           ELSE DO:       
      	     IF TIDREGITAB.START > startar2 THEN st1 = TIDREGITAB.START.
      	     ELSE st1 = startar2.
              /*20100512 AND TIDREGITAB.SLUT GE startar1*/     
      	     IF TIDREGITAB.SLUT < slutar2 /*AND TIDREGITAB.SLUT GE startar2*/ THEN sl1 = TIDREGITAB.SLUT.
      	     ELSE sl1 = slutar2.
              IF Guru.Konstanter:globforetag = "GKAL" OR Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT"  OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:
                 IF TIDREGITAB.SLUT > startar3 AND TIDREGITAB.SLUT LE slutar3
                 AND slutar3 NE startar3 THEN DO:
                    IF TIDREGITAB.SLUT < slutar3 THEN sl1 = TIDREGITAB.SLUT.
          	        ELSE sl1 = slutar3.              
                 END.           
              END.
              /*20100808 fredagar med schema 0-24 finns en obfil med obst1 = 18 och en med obst1 22- det får bara finnas 1*/
              FIND FIRST obfil WHERE obfil.OBSTART = TIDREGITAB.START AND obfil.OBSLUT = TIDREGITAB.SLUT
              AND obfil.OBDATUM = TIDREGITAB.DATUM  /*AND obfil.OBST1 = st1*/ AND obfil.OBSL1 = sl1 
              AND obfil.OLONKOD = obkod2 NO-LOCK NO-ERROR.
              IF NOT AVAILABLE obfil THEN DO:        
                 IF  TIDREGITAB.SLUT LE  startar2 THEN.  /*för att inte få dubletter 0-24 0-6 22-24*/
                 ELSE DO:
            	     CREATE obfil.
            	     ASSIGN obfil.OBSTART = TIDREGITAB.START 
            	     obfil.OBSLUT = TIDREGITAB.SLUT
            	     obfil.OBDATUM = TIDREGITAB.DATUM
            	     obfil.OBST1 = st1 obfil.OBSL1 = sl1
            	     obfil.OLONKOD = obkod2
            	     obfil.AONR = TIDREGITAB.AONR
            	     obfil.DELNR = TIDREGITAB.DELNR.             
                    IF TIDREGITAB.AONR = "110" THEN  obfil.OLONKOD = obkodsj2.
                 END.                   
              END.
           END.
        END.
	     GET NEXT traktq NO-LOCK. 
         END.
         CLOSE QUERY traktq.         
      END.
   END.
   IF startar3 > 0 OR slutar3 > 0 THEN DO:
      IF regslut LE slutar3 AND regslut > startar3 OR 
      regstart GE startar3 AND regstart < slutar3 OR 
      regstart < startar3 AND regslut > slutar3 THEN DO:
         OPEN QUERY traktq
         FOR EACH TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
         TIDREGITAB.DATUM = regdatum2 AND TIDREGITAB.START GE regstart AND
         TIDREGITAB.SLUT LE regslut AND TIDREGITAB.TIDLOG = TRUE /*AND TIDREGITAB.PRISTYP NE "FRÅNVARO." */
         USE-INDEX PSTART NO-LOCK.
         GET FIRST traktq NO-LOCK. 
         IF NOT AVAILABLE TIDREGITAB THEN DO:
   	     IF obkod3 NE "" THEN DO TRANSACTION:   	        
              FOR EACH TIDREGITAB WHERE 
   	        TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
   	        TIDREGITAB.DATUM = regdatum AND TIDREGITAB.LONTILLAGG = obkod3 AND
   	        TIDREGITAB.LONAUTO = TRUE AND TIDREGITAB.TIDLOG = FALSE AND TIDREGITAB.GODKAND = "" EXCLUSIVE-LOCK:
                 DELETE TIDREGITAB.
              END.
              IF obkodsj3 NE "" THEN DO:              
                 FOR EACH TIDREGITAB WHERE 
      	        TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
      	        TIDREGITAB.DATUM = regdatum AND TIDREGITAB.LONTILLAGG = obkodsj3 AND
      	        TIDREGITAB.LONAUTO = TRUE AND TIDREGITAB.TIDLOG = FALSE AND TIDREGITAB.GODKAND = "" EXCLUSIVE-LOCK:
                    DELETE TIDREGITAB.
                 END.
              END.
   
   	     END.
   	     RETURN.
         END.
         DO WHILE AVAILABLE(TIDREGITAB):
           IF Guru.Konstanter:varforetypval[30] = 0 AND TIDREGITAB.PRISTYP = "FRÅNVARO."  THEN .
           ELSE IF TIDREGITAB.PRISTYP = "FRÅNVARO." AND TIDREGITAB.AONR NE "110" THEN.
           ELSE DO:           
              ejob = FALSE.
              /*skift ej ob på lunchen*/
              IF regstart = 0 AND regslut = 24 THEN DO:
                 IF TIDREGITAB.START GE lunchstarten AND  TIDREGITAB.SLUT LE lunchslutet THEN ejob = TRUE.
              END.
              IF ejob = TRUE THEN ejob = FALSE.
              ELSE DO:       
         	     IF TIDREGITAB.START > startar3 THEN st1 = TIDREGITAB.START.
         	     ELSE st1 = startar3.
                 /*20100512 AND TIDREGITAB.SLUT GE startar3*/     
         	     IF TIDREGITAB.SLUT < slutar3 /*AND TIDREGITAB.SLUT GE startar3*/ THEN sl1 = TIDREGITAB.SLUT.
         	     ELSE sl1 = slutar3.
         	     CREATE obfil.
         	     ASSIGN obfil.OBSTART = TIDREGITAB.START
         	     obfil.OBSLUT = TIDREGITAB.SLUT
         	     obfil.OBDATUM = TIDREGITAB.DATUM 
         	     obfil.OBST1 = st1 obfil.OBSL1 = sl1
         	     obfil.OLONKOD = obkod3
         	     obfil.AONR = TIDREGITAB.AONR
         	     obfil.DELNR = TIDREGITAB.DELNR.        
                 IF TIDREGITAB.AONR = "110" THEN  obfil.OLONKOD = obkodsj3.                
              END.
              GET NEXT traktq NO-LOCK. 
           END.
         END.
         CLOSE QUERY traktq.             
      END.
   END.      
   /*FÖR ATT FIXA 22-22.30 22.30-24*/
   FOR EACH obfil WHERE NO-LOCK:      
      FIND FIRST obbuff WHERE obbuff.OBSLUT = obfil.OBSTART NO-ERROR.
      IF AVAILABLE obbuff  THEN DO:
         obfil.OBST1 = obfil.OBSTART.
      END.         
   END.     
   /*ny 20120227*/     
   EMPTY TEMP-TABLE obsum NO-ERROR. 
   FOR EACH obfil WHERE obfil.OBSL1 > obfil.OBST1:
      FIND FIRST obsum WHERE obsum.OLONKOD = obfil.OLONKOD AND obsum.AONR = obfil.AONR AND OBSUM.DELNR = obfil.DELNR NO-ERROR.
      IF NOT AVAILABLE obsum THEN DO: 
         CREATE obsum.
         ASSIGN obsum.OLONKOD = obfil.OLONKOD obsum.OBDATUM = obfil.OBDATUM obsum.AONR = obfil.AONR OBSUM.DELNR = obfil.DELNR.
      END.   
      nytid = obfil.OBST1.
      RUN TIMSEK.P.
      regstartsek = sekunder.
      nytid = obfil.OBSL1.
      RUN TIMSEK.P.          
      ASSIGN
      regdatumspar = regdatum
      regslutsek = sekunder
      regdatum = obfil.OBDATUM.
      RUN REGVEC.P.
      RUN TOTTID.P.         
      RUN TIMSEK.P.
      obsum.OLONANT = obsum.OLONANT + klock100(nytid).
   END.
   FOR EACH obsum:
      obsum.OLONANT = klock60(obsum.OLONANT).
   END. 
   DEBUGGER:SET-BREAK().
   FOR EACH obsum,           
   EACH TIDREGITAB WHERE          
   TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
   TIDREGITAB.DATUM = obsum.OBDATUM AND TIDREGITAB.LONTILLAGG = obsum.OLONKOD AND
   TIDREGITAB.LONAUTO = TRUE AND TIDREGITAB.TIDLOG = FALSE AND TIDREGITAB.GODKAND = "" EXCLUSIVE-LOCK:                  
      DELETE TIDREGITAB.
   END.   
   FIND FIRST obsum NO-ERROR.
   IF NOT AVAILABLE obsum  THEN DO:
      /*20120314 frånvaro ej sjuk rensa*/
      IF obkod1 NE "" THEN DO:
         FOR EACH TIDREGITAB WHERE          
         TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
         TIDREGITAB.DATUM = regdatum2 AND TIDREGITAB.LONTILLAGG = obkod1 AND
         TIDREGITAB.LONAUTO = TRUE AND TIDREGITAB.TIDLOG = FALSE AND TIDREGITAB.GODKAND = "" EXCLUSIVE-LOCK:                  
            DELETE TIDREGITAB.
         END.
      END.   
      IF obkod2 NE "" THEN DO:
         FOR EACH TIDREGITAB WHERE          
         TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
         TIDREGITAB.DATUM = regdatum2 AND TIDREGITAB.LONTILLAGG = obkod2 AND
         TIDREGITAB.LONAUTO = TRUE AND TIDREGITAB.TIDLOG = FALSE AND TIDREGITAB.GODKAND = "" EXCLUSIVE-LOCK:                  
            DELETE TIDREGITAB.
         END.
      END.
      IF obkod3 NE "" THEN DO:
         FOR EACH TIDREGITAB WHERE          
         TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
         TIDREGITAB.DATUM = regdatum2 AND TIDREGITAB.LONTILLAGG = obkod3 AND
         TIDREGITAB.LONAUTO = TRUE AND TIDREGITAB.TIDLOG = FALSE AND TIDREGITAB.GODKAND = "" EXCLUSIVE-LOCK:                  
            DELETE TIDREGITAB.
         END.
      END.            
   END.
                    
   FOR EACH obsum WHERE obsum.OLONANT > 0:
      FIND FIRST TIDREGITAB WHERE 
      TIDREGITAB.PERSONAL = PERSONALTAB.PERSONALKOD AND
      TIDREGITAB.DATUM = regdatum2 AND
      TIDREGITAB.AONR = obsum.AONR AND TIDREGITAB.DELNR = obsum.DELNR AND      
      TIDREGITAB.LONTILLAGG = obsum.OLONKOD AND TIDREGITAB.PRISTYP NE "FRÅNVARO."  EXCLUSIVE-LOCK NO-ERROR.
      IF AVAILABLE TIDREGITAB THEN DO:
         IF  TIDREGITAB.GODKAND NE "" THEN .                  
         ELSE IF obsum.OLONANT = 0 THEN DELETE TIDREGITAB.
         ELSE DO:
            ASSIGN  TIDREGITAB.LONTILLANTAL = obsum.OLONANT.                       
         END.   
      END.         
      ELSE DO:                                                  
         FIND FIRST tidbuff WHERE tidbuff.PERSONAL = PERSONALTAB.PERSONALKOD AND
         tidbuff.DATUM = regdatum2 AND  tidbuff.GODKAND NE ""  NO-LOCK NO-ERROR.
         IF NOT AVAILABLE tidbuff  THEN DO:         
            CREATE TIDREGITAB.
            CREATE tidallt.
            tidallt.RECTIDVIS = RECID(TIDREGITAB).
            ASSIGN TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD 
            TIDREGITAB.VECKONUMMER = regvnr
            SUBSTRING(TIDREGITAB.PROGRAM,1,158) = "OBBER" + STRING(TODAY) + STRING(TIME,"HH:MM") + Guru.Konstanter:globanv            
            TIDREGITAB.DATUM = regdatum2 TIDREGITAB.DAG = regdagnamn
            TIDREGITAB.AONR = obsum.AONR TIDREGITAB.DELNR = obsum.DELNR 
            TIDREGITAB.START = 7.00
            TIDREGITAB.SLUT = 7.00 
            TIDREGITAB.TIDLOG = FALSE
            TIDREGITAB.LONTILLAGG = obsum.OLONKOD TIDREGITAB.LONTILLANTAL = obsum.OLONANT.
            TIDREGITAB.LONAUTO = TRUE.
         END.   
      END.   
   END.                          
   FOR EACH obfil WHERE obfil.AONR = "110":
      FIND LAST tidbuff WHERE  tidbuff.PERSONAL = PERSONALTAB.PERSONALKOD AND
      tidbuff.DATUM < obfil.OBDATUM AND tidbuff.TIDLOG = TRUE USE-INDEX PKOD  NO-LOCK NO-ERROR.
      IF AVAILABLE tidbuff THEN DO:
         IF tidbuff.AONR NE "110" THEN DO:            
            obfil.KARENS = TRUE.            
         END.   
         ELSE DO:
            IF obfil.OBSLUT = 24 THEN.
            ELSE DO:
               /*  kolla karens 22-24 0-7 delat på två dagar*/
               IF tidbuff.SLUT = 24 THEN DO:
                  hjdat = tidbuff.DATUM.
                  FIND FIRST tidbuff WHERE 
                  tidbuff.PERSONAL = PERSONALTAB.PERSONALKOD AND
                  tidbuff.DATUM = hjdat AND tidbuff.TIDLOG = TRUE AND tidbuff.AONR NE "110" USE-INDEX PKOD  NO-LOCK NO-ERROR.
                  IF AVAILABLE tidbuff THEN DO:                     
                     obfil.KARENS = TRUE.                            
                  END.   
               END.
            END.                  
         END.   
      END.
   END.      
  
   IF Guru.Konstanter:globforetag = "GKAL" OR Guru.Konstanter:globforetag = "SUND"  OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "elpa" THEN DO:
      antal = 0.
      IF obkodsj1 NE "" THEN DO:
         FOR EACH obfil WHERE obfil.OLONKOD = obkodsj1 AND obfil.OBSL1 > obfil.OBST1 AND obfil.KARENS = FALSE:         
           /* totttidw för att räkna bort raster tex arbetstid 0-7 23-24*/          
            nytid = obfil.OBST1.
            RUN TIMSEK.P.
            regstartsek = sekunder.
            nytid = obfil.OBSL1.
            RUN TIMSEK.P.          
            ASSIGN
            regdatumspar = regdatum
            regslutsek = sekunder
            regdatum = obfil.OBDATUM.
            RUN REGVEC.P.
            RUN TOTTID.P.         
            RUN TIMSEK.P.      
            antal = antal + sekunder.                     
         END.
      END.
      sekunder = antal.
      RUN SEKTIM.P.
      oblonsj1 = nytid.
      antal = 0.
      IF obkodsj2 ne "" THEN DO:
         FOR EACH obfil WHERE obfil.OLONKOD = obkodsj2 AND obfil.OBSL1 > obfil.OBST1 AND obfil.KARENS = FALSE:
            /* totttidw för att räkna bort raster tex arbetstid 0-7 23-24*/
            nytid = obfil.OBST1.
            RUN TIMSEK.P.
            regstartsek = sekunder.
            nytid = obfil.OBSL1.
            RUN TIMSEK.P.          
            ASSIGN
            regdatumspar = regdatum
            regslutsek = sekunder
            regdatum = obfil.OBDATUM.
            RUN REGVEC.P.
            RUN TOTTID.P.         
            RUN TIMSEK.P.      
            antal = antal + sekunder.         
            
         END.
      END.
      sekunder = antal.
      RUN SEKTIM.P.
      ASSIGN
      oblonsj2 = nytid   
      antal = 0.
      
      IF obkodsj3 ne "" THEN DO:
         FOR EACH obfil WHERE obfil.OLONKOD = obkodsj3 AND obfil.OBSL1 > obfil.OBST1 AND obfil.KARENS = FALSE:
            /* totttidw för att räkna bort raster tex arbetstid 0-7 23-24*/
            nytid = obfil.OBST1.
            RUN TIMSEK.P.
            regstartsek = sekunder.
            nytid = obfil.OBSL1.
            RUN TIMSEK.P.          
            ASSIGN
            regdatumspar = regdatum
            regslutsek = sekunder
            regdatum = obfil.OBDATUM.
            RUN REGVEC.P.
            RUN TOTTID.P.         
            RUN TIMSEK.P.      
            antal = antal + sekunder.
            
         END.
      END.
      sekunder = antal.
      RUN SEKTIM.P.
      oblonsj3 = nytid.         
      IF obkodsj1 NE "" THEN DO TRANSACTION:
         FIND FIRST TIDREGITAB WHERE 
         TIDREGITAB.PERSONAL = PERSONALTAB.PERSONALKOD AND
         TIDREGITAB.DATUM = regdatum2 AND
         TIDREGITAB.LONTILLAGG = obkodsj1 AND TIDREGITAB.AONR = "110"  EXCLUSIVE-LOCK NO-ERROR.
         IF AVAILABLE TIDREGITAB THEN DO:    
            IF TIDREGITAB.GODKAND NE "" THEN.     
            ELSE IF oblonsj1 = 0 THEN DELETE TIDREGITAB.
            ELSE DO:
               ASSIGN  TIDREGITAB.LONTILLANTAL = oblonsj1.               
            END.   
         END.
         ELSE IF oblonsj1 > 0 THEN DO:                            
            FIND FIRST tidbuff WHERE tidbuff.PERSONAL = PERSONALTAB.PERSONALKOD AND
            tidbuff.DATUM = regdatum2 AND  tidbuff.GODKAND NE ""  NO-LOCK NO-ERROR.
            IF NOT AVAILABLE tidbuff  THEN DO:         
               CREATE TIDREGITAB.
               CREATE tidallt.
               tidallt.RECTIDVIS = RECID(TIDREGITAB).
               ASSIGN TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD 
               TIDREGITAB.VECKONUMMER = regvnr
               SUBSTRING(TIDREGITAB.PROGRAM,1,158) = "OBBER" + STRING(TODAY) + STRING(TIME,"HH:MM") + Guru.Konstanter:globanv
               TIDREGITAB.DATUM = regdatum2 TIDREGITAB.DAG = regdagnamn
               TIDREGITAB.AONR = "110" TIDREGITAB.DELNR = 0 
               TIDREGITAB.START = 7.00
               TIDREGITAB.SLUT = 7.00 
               TIDREGITAB.TIDLOG = FALSE
               TIDREGITAB.LONTILLAGG = obkodsj1 TIDREGITAB.LONTILLANTAL = oblonsj1
               TIDREGITAB.LONAUTO = TRUE.
             END.           
         END.
      END.   
      IF obkodsj2 NE "" THEN DO TRANSACTION:
         FIND FIRST TIDREGITAB WHERE 
         TIDREGITAB.PERSONAL = PERSONALTAB.PERSONALKOD AND
         TIDREGITAB.DATUM = regdatum2 AND
         TIDREGITAB.LONTILLAGG = obkodsj2 AND TIDREGITAB.AONR = "110"  EXCLUSIVE-LOCK NO-ERROR.
         IF AVAILABLE TIDREGITAB THEN DO:    
            IF TIDREGITAB.GODKAND NE "" THEN.     
            ELSE IF oblonsj2 = 0 THEN DELETE TIDREGITAB.
            ELSE DO:
               ASSIGN TIDREGITAB.LONTILLANTAL = oblonsj2.               
             END.
         END.
         ELSE IF oblonsj2 > 0 THEN DO:            
            FIND FIRST tidbuff WHERE tidbuff.PERSONAL = PERSONALTAB.PERSONALKOD AND
            tidbuff.DATUM = regdatum2 AND  tidbuff.GODKAND NE ""  NO-LOCK NO-ERROR.
            IF NOT AVAILABLE tidbuff  THEN DO:         
               CREATE TIDREGITAB.
               CREATE tidallt.
               tidallt.RECTIDVIS = RECID(TIDREGITAB).
               ASSIGN TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD 
               TIDREGITAB.VECKONUMMER = regvnr
               SUBSTRING(TIDREGITAB.PROGRAM,1,158) = "OBBER" + STRING(TODAY) + STRING(TIME,"HH:MM") + Guru.Konstanter:globanv
               TIDREGITAB.DATUM = regdatum2 TIDREGITAB.DAG = regdagnamn
               TIDREGITAB.AONR = "110" TIDREGITAB.DELNR = 0 
               TIDREGITAB.START = 7.00
               TIDREGITAB.SLUT = 7.00 TIDREGITAB.TIDLOG = FALSE
               TIDREGITAB.LONTILLAGG = obkodsj2 TIDREGITAB.LONTILLANTAL = oblonsj2
               TIDREGITAB.LONAUTO = TRUE.
            END.            
         END.
      END.      
      IF obkodsj3 NE "" THEN DO TRANSACTION:
         FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONAL = PERSONALTAB.PERSONALKOD AND 
         TIDREGITAB.DATUM = regdatum2 AND
         TIDREGITAB.LONTILLAGG = obkodsj3 AND TIDREGITAB.AONR = "110" EXCLUSIVE-LOCK NO-ERROR.
         IF AVAILABLE TIDREGITAB THEN DO:
            IF TIDREGITAB.GODKAND NE "" THEN.
            ELSE IF oblonsj3 = 0 THEN DELETE TIDREGITAB.
            ELSE DO:
               ASSIGN TIDREGITAB.LONTILLANTAL = oblonsj3.               
            END.   
         END.
         ELSE IF oblonsj3 > 0 THEN DO:
            FIND FIRST tidbuff WHERE tidbuff.PERSONAL = PERSONALTAB.PERSONALKOD AND
            tidbuff.DATUM = regdatum2 AND  tidbuff.GODKAND NE ""  NO-LOCK NO-ERROR.
            IF NOT AVAILABLE tidbuff  THEN DO:                     
               CREATE TIDREGITAB.
               CREATE tidallt.
               tidallt.RECTIDVIS = RECID(TIDREGITAB).
               ASSIGN TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD 
               TIDREGITAB.VECKONUMMER = regvnr
               SUBSTRING(TIDREGITAB.PROGRAM,1,158) = "OBBER" + STRING(TODAY) + STRING(TIME,"HH:MM") + Guru.Konstanter:globanv
               TIDREGITAB.DATUM = regdatum2 TIDREGITAB.DAG = regdagnamn
               TIDREGITAB.AONR = "110" TIDREGITAB.DELNR = 0 TIDREGITAB.START = 7.00
               TIDREGITAB.SLUT = 7.00 TIDREGITAB.TIDLOG = FALSE
               TIDREGITAB.LONTILLAGG = obkodsj3 TIDREGITAB.LONTILLANTAL = oblonsj3
               TIDREGITAB.LONAUTO = TRUE.
            END.            
         END.
      END.      
   END.   
   RELEASE TIDREGITAB.
END.
 
