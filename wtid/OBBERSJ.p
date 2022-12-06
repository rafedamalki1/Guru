 /*OBBERSJ.P BER AV OBEKVAM ARBETSTID */
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
DEFINE VARIABLE oblonsj1 AS DECIMAL NO-UNDO.
DEFINE VARIABLE oblonsj2 AS DECIMAL NO-UNDO.
DEFINE VARIABLE oblonsj3 AS DECIMAL NO-UNDO.
DEFINE VARIABLE antal AS INTEGER NO-UNDO.
DEFINE VARIABLE obhjalp AS INTEGER NO-UNDO. 
DEFINE VARIABLE onr AS CHARACTER NO-UNDO.
DEFINE VARIABLE dnr AS INTEGER NO-UNDO.
DEFINE VARIABLE onrsj AS CHARACTER NO-UNDO.
DEFINE VARIABLE dnrsj AS INTEGER NO-UNDO.
DEFINE VARIABLE regdatumspar AS DATE NO-UNDO.
DEFINE VARIABLE slsjdat AS DATE NO-UNDO.
DEFINE VARIABLE stsjdat AS DATE NO-UNDO.
DEFINE VARIABLE kaldag AS DATE NO-UNDO. 
DEFINE VARIABLE ejob AS LOGICAL NO-UNDO.
DEFINE VARIABLE karens AS LOGICAL NO-UNDO.
DEFINE VARIABLE hjdat AS DATE NO-UNDO.
DEFINE VARIABLE fjorton AS LOGICAL NO-UNDO.
DEFINE TEMP-TABLE obfil
   FIELD OBSTART LIKE TIDREGITAB.START
   FIELD OBSLUT LIKE TIDREGITAB.SLUT
   FIELD OBST1 LIKE TIDREGITAB.START
   FIELD OBSL1 LIKE TIDREGITAB.SLUT
   FIELD OLONKOD LIKE TIDREGITAB.LONTILLAGG
   FIELD OLONANT LIKE TIDREGITAB.LONTILLANTAL
   FIELD KARENS AS LOGICAL
   FIELD AONR AS CHARACTER
   FIELD DELNR AS INTEGER
   FIELD OBDATUM LIKE TIDREGITAB.DATUM. 
DEFINE BUFFER tidbuff FOR TIDREGITAB.   
DEFINE QUERY traktq FOR TIDREGITAB.   

EMPTY TEMP-TABLE obfil NO-ERROR. 
FIND TIDREGITAB WHERE RECID(TIDREGITAB) = tidtabrec NO-LOCK NO-ERROR.
IF AVAILABLE TIDREGITAB THEN DO:
   ASSIGN
   onr = TIDREGITAB.AONR
   dnr = TIDREGITAB.DELNR.   
END.   
ELSE DO:
   /*FRÅN BORTTID.W*/  
   ASSIGN
   onr = extraaonr
   dnr = extradnr.
   FIND FIRST AONRTAB WHERE AONRTAB.AONR = onr AND AONRTAB.DELNR = dnr NO-LOCK NO-ERROR.
   IF AVAILABLE AONRTAB THEN DO:
      IF AONRTAB.AONR NE "" THEN onr = "".
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
   IF Guru.Konstanter:globforetag = "elpa" THEN DO:   
      IF obkod1 = "414" THEN ASSIGN obkod1 = "417" obkodsj1 = "414". 
      IF obkod1 = "415" THEN ASSIGN obkod1 = "418" obkodsj1 = "415". 
      IF obkod1 = "416" THEN ASSIGN obkod1 = "419" obkodsj1 = "416". 
      IF obkod2 = "414" THEN ASSIGN obkod2 = "417" obkodsj2 = "414". 
      IF obkod2 = "415" THEN ASSIGN obkod2 = "418" obkodsj2 = "415". 
      IF obkod2 = "416" THEN ASSIGN obkod2 = "419" obkodsj2 = "416". 
      IF obkod3 = "414" THEN ASSIGN obkod3 = "417" obkodsj3 = "414". 
      IF obkod3 = "415" THEN ASSIGN obkod3 = "418" obkodsj3 = "415". 
      IF obkod3 = "416" THEN ASSIGN obkod3 = "419" obkodsj3 = "416". 
   END.     
   IF Guru.Konstanter:globforetag = "GKAL" THEN DO:   
      IF obkod1 = "242" THEN ASSIGN obkod1 = "VASJ" obkodsj1 = "242". 
      IF obkod1 = "243" THEN ASSIGN obkod1 = "VESJ" obkodsj1 = "243". 
      IF obkod1 = "244" THEN ASSIGN obkod1 = "STSJ" obkodsj1 = "244". 
      IF obkod2 = "242" THEN ASSIGN obkod2 = "VASJ" obkodsj2 = "242". 
      IF obkod2 = "243" THEN ASSIGN obkod2 = "VESJ" obkodsj2 = "243". 
      IF obkod2 = "244" THEN ASSIGN obkod2 = "STSJ" obkodsj2 = "244". 
      IF obkod3 = "242" THEN ASSIGN obkod3 = "VASJ" obkodsj3 = "242". 
      IF obkod3 = "243" THEN ASSIGN obkod3 = "VESJ" obkodsj3 = "243". 
      IF obkod3 = "244" THEN ASSIGN obkod3 = "STSJ" obkodsj3 = "244". 
   END.     
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT"  OR Guru.Konstanter:globforetag = "MISV" THEN DO:   
      IF obkod1 = "4105" THEN ASSIGN obkod1 = "314" obkodsj1 = "4105". 
      IF obkod1 = "4106" THEN ASSIGN obkod1 = "315" obkodsj1 = "4106". 
      IF obkod1 = "4107" THEN ASSIGN obkod1 = "316" obkodsj1 = "4107". 
      IF obkod2 = "4105" THEN ASSIGN obkod2 = "314" obkodsj2 = "4105". 
      IF obkod2 = "4106" THEN ASSIGN obkod2 = "315" obkodsj2 = "4106". 
      IF obkod2 = "4107" THEN ASSIGN obkod2 = "316" obkodsj2 = "4107". 
      IF obkod3 = "4105" THEN ASSIGN obkod3 = "314" obkodsj3 = "4105". 
      IF obkod3 = "4106" THEN ASSIGN obkod3 = "315" obkodsj3 = "4106". 
       IF obkod3 = "4107" THEN ASSIGN obkod3 = "316" obkodsj3 = "4107". 
   END.    
   
   IF obkodsj1 NE "" THEN DO:              
      FOR EACH TIDREGITAB WHERE	        
      TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
      TIDREGITAB.DATUM = regdatum AND TIDREGITAB.LONTILLAGG = obkodsj1 AND
      TIDREGITAB.LONAUTO = TRUE AND TIDREGITAB.GODKAND = ""  EXCLUSIVE-LOCK:                  
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
      TIDREGITAB.LONAUTO = TRUE AND TIDREGITAB.GODKAND = ""  EXCLUSIVE-LOCK:                  
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
      TIDREGITAB.LONAUTO = TRUE AND TIDREGITAB.GODKAND = ""  EXCLUSIVE-LOCK:                  
         IF TIDREGITAB.TIDLOG = FALSE THEN DELETE TIDREGITAB.
         ELSE DO:
            ASSIGN
            TIDREGITAB.LONTILLAGG = ""
            TIDREGITAB.LONTILLANTAL = 0.
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
         TIDREGITAB.SLUT LE regslut AND TIDREGITAB.TIDLOG = TRUE /*AND TIDREGITAB.AONR = "110" */
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
      	    IF TIDREGITAB.SLUT < slutar1  THEN sl1 = TIDREGITAB.SLUT.
          	 ELSE sl1 = slutar1.
             IF Guru.Konstanter:globforetag = "GKAL" OR Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT"  OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:
                IF TIDREGITAB.SLUT > startar2 AND TIDREGITAB.SLUT LE slutar2 
                AND slutar2 NE startar2 THEN DO:
                   IF obkod1 = obkod2 THEN DO:           
                      IF TIDREGITAB.SLUT < slutar2 THEN sl1 = TIDREGITAB.SLUT.
                      ELSE sl1 = slutar2.              
                   END.
                   ELSE DO:         
                       FIND FIRST obfil WHERE obfil.OBSTART =  TIDREGITAB.START
                       AND obfil.OBSLUT = TIDREGITAB.SLUT AND obfil.OBDATUM = TIDREGITAB.DATUM 
                       AND obfil.OLONKOD = obkod2 NO-ERROR.
                       IF NOT AVAILABLE obfil THEN DO:
                          FIND FIRST obfil WHERE obfil.OBSTART =  TIDREGITAB.START
                          AND obfil.OBSLUT = TIDREGITAB.SLUT AND obfil.OBDATUM = TIDREGITAB.DATUM 
                          AND obfil.OLONKOD = obkodsj2 NO-ERROR.
                       END.
                       IF NOT AVAILABLE obfil THEN DO:
                          CREATE obfil.
                       END.                       
                       ASSIGN obfil.OBSTART = TIDREGITAB.START 
                       obfil.OBSLUT = TIDREGITAB.SLUT
                       obfil.OBDATUM = TIDREGITAB.DATUM       	     
                       obfil.AONR = TIDREGITAB.AONR       	     
                       obfil.DELNR = TIDREGITAB.DELNR       	     
                       obfil.OLONKOD = obkod2.                       
                       IF TIDREGITAB.AONR = "110" THEN obfil.OLONKOD = obkod2.
                       ELSE DO:
                          ASSIGN
                          obfil.OLONKOD = obkodsj2
                          onrsj = obfil.AONR 
                          dnrsj = obfil.DELNR.
                       END.
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
                   FIND FIRST obfil WHERE obfil.OBSTART =  TIDREGITAB.START
                   AND obfil.OBSLUT = TIDREGITAB.SLUT AND obfil.OBDATUM = TIDREGITAB.DATUM 
                   AND obfil.OLONKOD = obkod1 NO-ERROR.
                   IF NOT AVAILABLE obfil THEN DO:
                      FIND FIRST obfil WHERE obfil.OBSTART =  TIDREGITAB.START
                      AND obfil.OBSLUT = TIDREGITAB.SLUT AND obfil.OBDATUM = TIDREGITAB.DATUM 
                      AND obfil.OLONKOD = obkodsj1 NO-ERROR.
                   END.
                   IF NOT AVAILABLE obfil THEN DO:
                      CREATE obfil.
                   END.                                       
                   ASSIGN obfil.OBSTART = TIDREGITAB.START 
                   obfil.OBSLUT = TIDREGITAB.SLUT
                   obfil.OBDATUM = TIDREGITAB.DATUM 
                   obfil.AONR = TIDREGITAB.AONR       	     
                   obfil.DELNR = TIDREGITAB.DELNR       	     
                   obfil.OBST1 = st1 obfil.OBSL1 = sl1
                   obfil.OLONKOD = obkod1.                
                   IF TIDREGITAB.AONR = "110" THEN  obfil.OLONKOD = obkod1.                
                   ELSE DO: 
                      ASSIGN
                      obfil.OLONKOD = obkodsj1
                      onrsj = obfil.AONR 
                      dnrsj = obfil.DELNR.
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
         TIDREGITAB.SLUT LE regslut AND TIDREGITAB.TIDLOG = TRUE /*AND TIDREGITAB.AONR = "110"*/
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
        ejob = FALSE.
        /*skift ej ob på lunchen*/
        IF regstart = 0 AND regslut = 24 THEN DO:
           IF TIDREGITAB.START GE lunchstarten AND  TIDREGITAB.SLUT LE lunchslutet THEN ejob = TRUE.
        END.
        IF ejob = TRUE THEN ejob = FALSE.
        ELSE DO:       
   	     IF TIDREGITAB.START > startar2 THEN st1 = TIDREGITAB.START.
   	     ELSE st1 = startar2.
           /*20100512 AND TIDREGITAB.SLUT GE startar2*/     
   	     IF TIDREGITAB.SLUT < slutar2  THEN sl1 = TIDREGITAB.SLUT.
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
              FIND FIRST obfil WHERE obfil.OBSTART =  TIDREGITAB.START
              AND obfil.OBSLUT = TIDREGITAB.SLUT AND obfil.OBDATUM = TIDREGITAB.DATUM 
              AND obfil.OLONKOD = obkod2 NO-ERROR.
              IF AVAILABLE obfil THEN DO:
                 IF st1 < sl1 THEN DO:                 
                    /*tillaggt så att inte arbetstid 0-7 23-24 ställer till det förn sätter start 18 slut 7*/
                    ASSIGN 
                    obfil.AONR = TIDREGITAB.AONR       	     
                    obfil.DELNR = TIDREGITAB.DELNR       	     
            	     obfil.OBST1 = st1 obfil.OBSL1 = sl1
                    obfil.OLONKOD = obkod2.              
                 END.
              END.
              IF NOT AVAILABLE obfil THEN DO:
                 FIND FIRST obfil WHERE obfil.OBSTART =  TIDREGITAB.START
                 AND obfil.OBSLUT = TIDREGITAB.SLUT AND obfil.OBDATUM = TIDREGITAB.DATUM 
                 AND obfil.OLONKOD = obkodsj2 NO-ERROR.
                 IF AVAILABLE obfil THEN DO:
                    IF st1 < sl1 THEN DO:                 
                       /*tillaggt så att inte arbetstid 0-7 23-24 ställer till det förn sätter start 18 slut 7*/
                       ASSIGN 
                       obfil.AONR = TIDREGITAB.AONR       	     
                       obfil.DELNR = TIDREGITAB.DELNR       	     
               	     obfil.OBST1 = st1 obfil.OBSL1 = sl1
                       obfil.OLONKOD = obkod2.              
                    END.
                 END.
              END.
              IF NOT AVAILABLE obfil THEN DO:
                 CREATE obfil.
                 ASSIGN obfil.OBSTART = TIDREGITAB.START 
         	     obfil.OBSLUT = TIDREGITAB.SLUT
         	     obfil.OBDATUM = TIDREGITAB.DATUM
                 obfil.AONR = TIDREGITAB.AONR       	     
                 obfil.DELNR = TIDREGITAB.DELNR       	     
         	     obfil.OBST1 = st1 obfil.OBSL1 = sl1
         	     obfil.OLONKOD = obkod2.              
              END.                             	           	     
              IF TIDREGITAB.AONR = "110" THEN obfil.OLONKOD = obkod2.
              ELSE DO: 
                 ASSIGN
                 obfil.OLONKOD = obkodsj2                       
                 onrsj = obfil.AONR 
                 dnrsj = obfil.DELNR.
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
         TIDREGITAB.SLUT LE regslut AND TIDREGITAB.TIDLOG = TRUE /*AND TIDREGITAB.AONR = "110"*/
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
      	     IF TIDREGITAB.SLUT < slutar3 THEN sl1 = TIDREGITAB.SLUT.
      	     ELSE sl1 = slutar3.
              FIND FIRST obfil WHERE obfil.OBSTART =  TIDREGITAB.START
              AND obfil.OBSLUT = TIDREGITAB.SLUT AND obfil.OBDATUM = TIDREGITAB.DATUM 
              AND obfil.OLONKOD = obkod3 NO-ERROR.
              IF NOT AVAILABLE obfil THEN DO:
                 FIND FIRST obfil WHERE obfil.OBSTART =  TIDREGITAB.START
                 AND obfil.OBSLUT = TIDREGITAB.SLUT AND obfil.OBDATUM = TIDREGITAB.DATUM 
                 AND obfil.OLONKOD = obkodsj3 NO-ERROR.
              END.
              IF NOT AVAILABLE obfil THEN DO:
                 CREATE obfil.
              END.                             	     
      	     ASSIGN obfil.OBSTART = TIDREGITAB.START
      	     obfil.OBSLUT = TIDREGITAB.SLUT
      	     obfil.OBDATUM = TIDREGITAB.DATUM 
              obfil.AONR = TIDREGITAB.AONR       	     
              obfil.DELNR = TIDREGITAB.DELNR       	     
      	     obfil.OBST1 = st1 obfil.OBSL1 = sl1
      	     obfil.OLONKOD = obkod3.              
              IF TIDREGITAB.AONR = "110" THEN obfil.OLONKOD = obkod3.
              ELSE DO: 
                 ASSIGN
                 obfil.OLONKOD = obkodsj3
                 onrsj = obfil.AONR 
                 dnrsj = obfil.DELNR.
              END.
           END.
           GET NEXT traktq NO-LOCK. 
         END.
         CLOSE QUERY traktq.             
      END.
   END.               
   /*avgör om det är karensdagen. För karensdagen utbetalas ej obsjuk    */
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
   /* obsjuk endast 14 kalenderdagar sedan ingen tolkning*/
   FOR EACH obfil WHERE obfil.AONR = "110":        
      slsjdat = obfil.OBDATUM.
      stsjdat = obfil.OBDATUM - 14.        
      kaldag = slsjdat.
      regdatum = slsjdat.
      fjorton = FALSE.
      REPEAT:
         IF regdatum LE stsjdat THEN LEAVE.
         RUN REGVEC.P.
         RUN REGDAG.P.
         RUN SLUTARB.P.
         IF regstart NE regslut THEN DO:
            FIND FIRST tidbuff WHERE tidbuff.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
            tidbuff.TIDLOG = TRUE AND tidbuff.AONR = "110" AND tidbuff.datum = regdatum  NO-LOCK NO-ERROR.
            IF AVAILABLE tidbuff THEN DO:
               kaldag = regdatum.
               fjorton = TRUE.
            END.
            ELSE DO:
                fjorton = FALSE.
                LEAVE.
            END.    
         END.
         ELSE DO:
            
         END.         
         regdatum = regdatum - 1.
      END.   
      /*kaldag- första dagen inom 14-dagarsintervallen som har sjuk.
      Om den föregås av ett antal arbetsfria dagar och tidreg innan inte är sju = ingen karens*/
      IF fjorton = TRUE AND kaldag > stsjdat THEN DO:
         FIND LAST tidbuff WHERE tidbuff.PERSONALKOD = PERSONALTAB.PERSONALKOD AND tidbuff.datum LE stsjdat 
         AND tidbuff.TIDLOG = TRUE  USE-INDEX PSTART  NO-LOCK NO-ERROR.
         IF AVAILABLE tidbuff THEN DO:
            IF tidbuff.AONR NE "110" THEN fjorton = FALSE.
         END.   
      END.
      IF fjorton = TRUE THEN obfil.KARENS = TRUE.      
   END.         
   antal = 0.
   IF obkod1 ne "" THEN DO:      
      FOR EACH obfil WHERE obfil.OLONKOD = obkod1 AND obfil.OBSL1 > obfil.OBST1 AND obfil.KARENS = FALSE:         
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
   oblon1 = nytid.
   antal = 0.
   IF obkod2 ne "" THEN DO:
      FOR EACH obfil WHERE obfil.OLONKOD = obkod2 AND 
      obfil.OBSL1 > obfil.OBST1 AND obfil.KARENS = FALSE:
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
   oblon2 = nytid   
   antal = 0.   
   IF obkod3 ne "" THEN DO:
      FOR EACH obfil WHERE obfil.OLONKOD = obkod3 AND 
      obfil.OBSL1 > obfil.OBST1 AND obfil.KARENS = FALSE:
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
   oblon3 = nytid.            
   IF obkod1 NE "" THEN DO TRANSACTION:               
      FIND FIRST TIDREGITAB WHERE 
      TIDREGITAB.PERSONAL = PERSONALTAB.PERSONALKOD AND
      TIDREGITAB.DATUM = regdatum2 AND
      TIDREGITAB.LONTILLAGG = obkod1 AND TIDREGITAB.AONR = "110" EXCLUSIVE-LOCK NO-ERROR.
      IF AVAILABLE TIDREGITAB THEN DO:       
         IF TIDREGITAB.GODKAND NE "" THEN.  
         ELSE IF oblon1 = 0 THEN DELETE TIDREGITAB.
         ELSE DO:
            ASSIGN  TIDREGITAB.LONTILLANTAL = oblon1.
            IF onr NE "" THEN ASSIGN TIDREGITAB.AONR = onr TIDREGITAB.DELNR = dnr.
         END.   
      END.
      ELSE IF oblon1 > 0 THEN DO:        
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
            TIDREGITAB.AONR = onr TIDREGITAB.DELNR = dnr 
            TIDREGITAB.START = 7.00
            TIDREGITAB.SLUT = 7.00 
            TIDREGITAB.TIDLOG = FALSE
            TIDREGITAB.LONTILLAGG = obkod1 TIDREGITAB.LONTILLANTAL = oblon1
            TIDREGITAB.LONAUTO = TRUE.
         END.            
      END.
   END.

   IF obkod2 NE "" THEN DO TRANSACTION:
      FIND FIRST TIDREGITAB WHERE 
      TIDREGITAB.PERSONAL = PERSONALTAB.PERSONALKOD AND
      TIDREGITAB.DATUM = regdatum2 AND
      TIDREGITAB.LONTILLAGG = obkod2 AND TIDREGITAB.AONR = "110" EXCLUSIVE-LOCK NO-ERROR.
      IF AVAILABLE TIDREGITAB THEN DO:    
         IF TIDREGITAB.GODKAND NE "" THEN.     
         ELSE IF oblon2 = 0 THEN DELETE TIDREGITAB.
         ELSE DO:
            ASSIGN TIDREGITAB.LONTILLANTAL = oblon2.
            IF onr NE "" THEN ASSIGN TIDREGITAB.AONR = onr TIDREGITAB.DELNR = dnr.
          END.
      END.
      ELSE IF oblon2 > 0 THEN DO:
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
            TIDREGITAB.AONR = onr TIDREGITAB.DELNR = dnr 
            TIDREGITAB.START = 7.00
            TIDREGITAB.SLUT = 7.00 TIDREGITAB.TIDLOG = FALSE
            TIDREGITAB.LONTILLAGG = obkod2 TIDREGITAB.LONTILLANTAL = oblon2
            TIDREGITAB.LONAUTO = TRUE.
         END.            
      END.
   END.
   
   IF obkod3 NE "" THEN DO TRANSACTION:
      FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONAL = PERSONALTAB.PERSONALKOD AND 
      TIDREGITAB.DATUM = regdatum2 AND
      TIDREGITAB.LONTILLAGG = obkod3 AND TIDREGITAB.AONR = "110" EXCLUSIVE-LOCK NO-ERROR.
      IF AVAILABLE TIDREGITAB THEN DO:
         IF TIDREGITAB.GODKAND NE "" THEN.
         ELSE IF oblon3 = 0 THEN DELETE TIDREGITAB.
         ELSE DO:
            ASSIGN TIDREGITAB.LONTILLANTAL = oblon3.
            IF onr NE "" THEN ASSIGN TIDREGITAB.AONR = onr TIDREGITAB.DELNR = dnr.
         END.   
      END.
      ELSE IF oblon3 > 0 THEN DO:
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
            TIDREGITAB.AONR = onr TIDREGITAB.DELNR = dnr TIDREGITAB.START = 7.00
            TIDREGITAB.SLUT = 7.00 TIDREGITAB.TIDLOG = FALSE
            TIDREGITAB.LONTILLAGG = obkod3 TIDREGITAB.LONTILLANTAL = oblon3
            TIDREGITAB.LONAUTO = TRUE.
         END.            
      END.
   END.
   antal = 0.
   IF obkodsj1 ne "" THEN DO:
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
      FOR EACH obfil WHERE obfil.OLONKOD = obkodsj2 AND 
      obfil.OBSL1 > obfil.OBST1 AND obfil.KARENS = FALSE:
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
      FOR EACH obfil WHERE obfil.OLONKOD = obkodsj3 AND 
      obfil.OBSL1 > obfil.OBST1 AND obfil.KARENS = FALSE:
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
      TIDREGITAB.LONTILLAGG = obkodsj1 AND TIDREGITAB.AONR NE "110" EXCLUSIVE-LOCK NO-ERROR.
      IF AVAILABLE TIDREGITAB THEN DO:
         IF TIDREGITAB.GODKAND NE "" THEN.         
         ELSE IF oblonsj1 = 0 THEN DELETE TIDREGITAB.
         ELSE DO:
            ASSIGN  TIDREGITAB.LONTILLANTAL = oblonsj1.
            IF onrsj NE "" THEN ASSIGN TIDREGITAB.AONR = onrsj TIDREGITAB.DELNR = dnrsj.
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
            TIDREGITAB.AONR = onrsj TIDREGITAB.DELNR = dnrsj 
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
      TIDREGITAB.LONTILLAGG = obkodsj2 AND TIDREGITAB.AONR NE "110" EXCLUSIVE-LOCK NO-ERROR.
      IF AVAILABLE TIDREGITAB THEN DO:
         IF TIDREGITAB.GODKAND NE "" THEN.         
         ELSE IF oblonsj2 = 0 THEN DELETE TIDREGITAB.
         ELSE DO:
            ASSIGN TIDREGITAB.LONTILLANTAL = oblonsj2.
            IF onrsj NE "" THEN ASSIGN TIDREGITAB.AONR = onrsj TIDREGITAB.DELNR = dnrsj.
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
            TIDREGITAB.AONR = onrsj TIDREGITAB.DELNR = dnrsj 
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
      TIDREGITAB.LONTILLAGG = obkodsj3 AND TIDREGITAB.AONR NE "110" EXCLUSIVE-LOCK NO-ERROR.
      IF AVAILABLE TIDREGITAB THEN DO:
         IF TIDREGITAB.GODKAND NE "" THEN.
         ELSE IF oblonsj3 = 0 THEN DELETE TIDREGITAB.
         ELSE DO:
            ASSIGN TIDREGITAB.LONTILLANTAL = oblonsj3.
            IF onrsj NE "" THEN ASSIGN TIDREGITAB.AONR = onrsj TIDREGITAB.DELNR = dnrsj.
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
            TIDREGITAB.AONR = onrsj TIDREGITAB.DELNR = dnrsj TIDREGITAB.START = 7.00
            TIDREGITAB.SLUT = 7.00 TIDREGITAB.TIDLOG = FALSE
            TIDREGITAB.LONTILLAGG = obkodsj3 TIDREGITAB.LONTILLANTAL = oblonsj3
            TIDREGITAB.LONAUTO = TRUE.
         END.            
      END.
   END.
   RELEASE TIDREGITAB.   
   
END.

