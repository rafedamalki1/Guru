/*AVBER.P  */
DEFINE SHARED VARIABLE persrec AS RECID  NO-UNDO.
DEFINE SHARED VARIABLE sekunder AS INTEGER FORMAT '99999' NO-UNDO. 
DEFINE SHARED VARIABLE nytid AS DECIMAL NO-UNDO.
DEFINE SHARED VARIABLE reco AS RECID NO-UNDO.
DEFINE SHARED VARIABLE regstart LIKE TIDREGITAB.START NO-UNDO.
DEFINE SHARED VARIABLE regslut LIKE TIDREGITAB.SLUT NO-UNDO.

DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE seku AS INTEGER FORMAT '99999' NO-UNDO.
DEFINE VARIABLE bsluta LIKE TIDREGITAB.SLUT NO-UNDO.
DEFINE VARIABLE bstarta LIKE TIDREGITAB.START NO-UNDO.
DEFINE VARIABLE beslut LIKE TIDREGITAB.SLUT NO-UNDO.
DEFINE VARIABLE bestart LIKE TIDREGITAB.START NO-UNDO.
DEFINE VARIABLE beant LIKE TIDREGITAB.START NO-UNDO.
DEFINE VARIABLE nyber1 AS DECIMAL FORMAT "99.99" NO-UNDO.
DEFINE VARIABLE orec3 AS RECID NO-UNDO.
DEFINE VARIABLE hjdat LIKE TIDREGITAB.DATUM NO-UNDO.
DEFINE VARIABLE bereslut AS DECIMAL NO-UNDO.
DEFINE SHARED TEMP-TABLE ohjalp
 FIELD ODATUM LIKE TIDREGITAB.DATUM 
 FIELD OSTART LIKE TIDREGITAB.START
 FIELD OSLUT LIKE TIDREGITAB.SLUT 
 FIELD OKOD1 LIKE TIDREGITAB.OKOD1 
 FIELD OANT1 LIKE TIDREGITAB.OANT1
 FIELD OST1 LIKE TIDREGITAB.START 
 FIELD OSL1 LIKE TIDREGITAB.SLUT
 FIELD OKOD2 LIKE TIDREGITAB.OKOD2 
 FIELD OANT2 LIKE TIDREGITAB.OANT2
 FIELD OST2 LIKE TIDREGITAB.START 
 FIELD OSL2 LIKE TIDREGITAB.SLUT
 FIELD OKOD3 LIKE TIDREGITAB.OKOD3 
 FIELD OANT3 LIKE TIDREGITAB.OANT3
 FIELD OST3 LIKE TIDREGITAB.START 
 FIELD OSL3 LIKE TIDREGITAB.SLUT
 FIELD OTOTALT LIKE TIDREGITAB.TOTALT 
 FIELD OOVERAUTO LIKE TIDREGITAB.OVERAUTO 
 FIELD OENKE LIKE OVERKOD.ENKEL
 FIELD OAONR LIKE TIDREGITAB.AONR 
 FIELD ODELNR LIKE TIDREGITAB.DELNR 
 FIELD OETOT LIKE TIDREGITAB.TOTALT 
 FIELD OUTR LIKE TIDREGITAB.UTRYCKNING 
 FIELD RECTIDVIS AS RECID
 FIELD OVERTIDUTTAG LIKE TIDREGITAB.OVERTIDUTTAG
 FIELD OAVBE LIKE TIDREGITAB.LAGANTAL
 INDEX ODATUM IS PRIMARY ODATUM ASCENDING OSTART ASCENDING.
 
DEFINE VARIABLE avberrec AS RECID  NO-UNDO. 
FIND PERSONALTAB WHERE RECID(PERSONALTAB) = persrec NO-LOCK NO-ERROR.
FIND FIRST ANSTFORMTAB WHERE ANSTFORMTAB.ANSTALLNING = PERSONALTAB.ANSTALLNING 
USE-INDEX ANSTF NO-LOCK NO-ERROR. 

FIND FIRST ohjalp WHERE ohjalp.RECTIDVIS = reco NO-LOCK NO-ERROR.
FIND FIRST UTRYCKNING WHERE UTRYCKNING.KOD = ANSTFORMTAB.KOD
USE-INDEX UT NO-LOCK NO-ERROR.

ASSIGN bestart = ohjalp.OST1  
beslut = ohjalp.OSL1
beant = ohjalp.OANT1.
   
IF Guru.Konstanter:globforetag = "sund" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" THEN DO:
   FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
   TIDREGITAB.DATUM = ohjalp.ODATUM AND TIDREGITAB.BEREDSKAP NE '' AND
   TIDREGITAB.BEREDSKAPSTART LE bestart AND TIDREGITAB.BEREDSKAPSLUT GE bestart
   AND TIDREGITAB.TIDLOG = FALSE AND TIDREGITAB.BEREDSKAP NE "260" USE-INDEX PKOD NO-LOCK NO-ERROR. 
   IF NOT AVAILABLE TIDREGITAB THEN DO:  
      FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
      TIDREGITAB.DATUM = ohjalp.ODATUM AND TIDREGITAB.BEREDSKAP NE '' AND
      TIDREGITAB.BEREDSKAPSTART > bestart AND TIDREGITAB.BEREDSKAPSLUT GE beslut
      AND TIDREGITAB.TIDLOG = FALSE AND TIDREGITAB.BEREDSKAP NE "260" USE-INDEX PKOD NO-LOCK NO-ERROR. 
   END.
END.
ELSE DO:
   FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
   TIDREGITAB.DATUM = ohjalp.ODATUM AND TIDREGITAB.BEREDSKAP NE '' AND
   TIDREGITAB.BEREDSKAPSTART LE bestart AND TIDREGITAB.BEREDSKAPSLUT GE bestart
   AND TIDREGITAB.TIDLOG = FALSE USE-INDEX PKOD NO-LOCK NO-ERROR. 
   IF NOT AVAILABLE TIDREGITAB THEN DO:  
      FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
      TIDREGITAB.DATUM = ohjalp.ODATUM AND TIDREGITAB.BEREDSKAP NE '' AND
      TIDREGITAB.BEREDSKAPSTART > bestart AND TIDREGITAB.BEREDSKAPSLUT GE beslut
      AND TIDREGITAB.TIDLOG = FALSE USE-INDEX PKOD NO-LOCK NO-ERROR. 
   END.
END.
IF AVAILABLE TIDREGITAB THEN DO TRANSACTION:         
   avberrec = RECID(TIDREGITAB).
   FIND TIDREGITAB WHERE RECID(TIDREGITAB) = avberrec EXCLUSIVE-LOCK NO-ERROR.
   ASSIGN
   orec3 = RECID(TIDREGITAB)
   bstarta = TIDREGITAB.BEREDSKAPSTART
   bsluta = TIDREGITAB.BEREDSKAPSLUT.   
   IF beslut LE TIDREGITAB.BEREDSKAPSLUT THEN DO:     
      musz = FALSE.
      IF Guru.Konstanter:globforetag = "gran" THEN DO:            
         FIND FIRST OVERKOD WHERE OVERKOD.OVERTIDTILL = TIDREGITAB.BEREDSKAP USE-INDEX OVER NO-LOCK NO-ERROR.
         IF AVAILABLE OVERKOD THEN musz = TRUE.
      END.
      IF musz = TRUE THEN DO:
         musz = FALSE.
      END.      
      ELSE DO:
         nytid = beant.
         RUN TIMSEK.P.   
         ASSIGN
         seku = sekunder
         nytid = TIDREGITAB.BERANTAL.
         RUN TIMSEK.P.
         sekunder = sekunder - seku.
         IF sekunder < 0 THEN sekunder = 0.
         RUN SEKTIM.P.
         nyber1 = nytid.
         ASSIGN TIDREGITAB.BERANTAL = nyber1.     
      END.   
   END.   
   ELSE IF beslut > TIDREGITAB.BEREDSKAPSLUT AND
   beslut > regstart AND beslut < regslut THEN DO:
      nytid = beant.
      RUN TIMSEK.P.  
      ASSIGN 
      seku = sekunder
      nytid = TIDREGITAB.BERANTAL.
      RUN TIMSEK.P.
      sekunder = sekunder - seku.  
      IF sekunder < 0 THEN sekunder = 0.
      RUN SEKTIM.P.
      nyber1 = nytid.
      ASSIGN TIDREGITAB.BERANTAL = nyber1.
   END.
   ELSE IF beslut > TIDREGITAB.BEREDSKAPSLUT THEN DO: 
      musz = FALSE.
      IF Guru.Konstanter:globforetag = "gran" THEN DO:            
         FIND FIRST OVERKOD WHERE OVERKOD.OVERTIDTILL = TIDREGITAB.BEREDSKAP USE-INDEX OVER NO-LOCK NO-ERROR.
         IF AVAILABLE OVERKOD THEN musz = TRUE.
      END.
      IF musz = TRUE THEN DO:
         musz = FALSE.
      END.      
      ELSE DO:
         nytid = bestart.
         RUN TIMSEK.P.   
         ASSIGN
         seku = sekunder
         nytid = TIDREGITAB.BEREDSKAPSLUT.
         RUN TIMSEK.P.
         ASSIGN
         seku = sekunder - seku 
         nytid = TIDREGITAB.BERANTAL.
         RUN TIMSEK.P.
         sekunder = sekunder - seku.
         IF sekunder < 0 THEN sekunder = 0.
         RUN SEKTIM.P.      
         nyber1 = nytid.
         ASSIGN TIDREGITAB.BERANTAL = nyber1.   
      END.   
      IF Guru.Konstanter:globforetag = "sund" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" THEN DO:
         FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
         TIDREGITAB.DATUM = ohjalp.ODATUM AND TIDREGITAB.BEREDSKAP NE '' AND
         TIDREGITAB.BEREDSKAPSTART = bsluta AND TIDREGITAB.TIDLOG = FALSE AND TIDREGITAB.BEREDSKAP NE "260" USE-INDEX PKOD NO-LOCK NO-ERROR.
      END.
      ELSE DO:      
         FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
         TIDREGITAB.DATUM = ohjalp.ODATUM AND TIDREGITAB.BEREDSKAP NE '' AND
         TIDREGITAB.BEREDSKAPSTART = bsluta AND TIDREGITAB.TIDLOG = FALSE USE-INDEX PKOD NO-LOCK NO-ERROR.
      END.
      IF AVAILABLE TIDREGITAB THEN DO:             
         avberrec = RECID(TIDREGITAB).
         FIND TIDREGITAB WHERE RECID(TIDREGITAB) = avberrec EXCLUSIVE-LOCK NO-ERROR.
         IF beslut LE TIDREGITAB.BEREDSKAPSLUT THEN DO:
            nytid = beslut.
            RUN TIMSEK.P.   
            ASSIGN
            seku = sekunder
            nytid = TIDREGITAB.BEREDSKAPSTART.
            RUN TIMSEK.P.
            ASSIGN
            seku = seku - sekunder
            nytid = TIDREGITAB.BERANTAL.
            RUN TIMSEK.P.
            sekunder = sekunder - seku.
            IF sekunder < 0 THEN sekunder = 0.
            RUN SEKTIM.P.
            ASSIGN TIDREGITAB.BERANTAL = nytid.
         END.
      END.        
   END.        
END.         
musz = FALSE.      
IF Guru.Konstanter:globforetag = "ELPA" THEN DO:
   IF UTRYCKNING.KLOCKAN = FALSE THEN musz = TRUE.   
END.
ELSE musz = TRUE.
IF musz = TRUE THEN DO:
   musz = FALSE.   
   IF ohjalp.OST2 = 0 AND ohjalp.OSL2 > 0 THEN hjdat = ohjalp.ODATUM + 1.
   ELSE hjdat = ohjalp.ODATUM. 
   IF ohjalp.OST2 > 0  OR ohjalp.OSL2 > 0 THEN DO:
      IF Guru.Konstanter:globforetag = "sund" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" THEN DO:
         FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
         TIDREGITAB.DATUM = hjdat AND TIDREGITAB.BEREDSKAP NE '' AND
         TIDREGITAB.BEREDSKAPSTART LE ohjalp.OST2 AND TIDREGITAB.BEREDSKAPSLUT GE ohjalp.OST2
         AND TIDREGITAB.TIDLOG = FALSE AND TIDREGITAB.BEREDSKAP NE "260" USE-INDEX PKOD NO-LOCK NO-ERROR.
      END.
      ELSE DO:      
         FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
         TIDREGITAB.DATUM = hjdat AND TIDREGITAB.BEREDSKAP NE '' AND
         TIDREGITAB.BEREDSKAPSTART LE ohjalp.OST2 AND TIDREGITAB.BEREDSKAPSLUT GE ohjalp.OST2
         AND TIDREGITAB.TIDLOG = FALSE USE-INDEX PKOD NO-LOCK NO-ERROR.
      END.
      IF AVAILABLE TIDREGITAB THEN DO TRANSACTION:         
         avberrec = RECID(TIDREGITAB).
         FIND TIDREGITAB WHERE RECID(TIDREGITAB) = avberrec EXCLUSIVE-LOCK NO-ERROR.     
         IF ohjalp.OSL2 LE TIDREGITAB.BEREDSKAPSLUT THEN DO:
            nytid = ohjalp.OANT2.
            RUN TIMSEK.P.   
            ASSIGN
            seku = sekunder
            nytid = TIDREGITAB.BERANTAL.
            RUN TIMSEK.P.
            sekunder = sekunder - seku.
            IF sekunder < 0 THEN sekunder = 0.
            RUN SEKTIM.P.
            nyber1 = nytid.
            ASSIGN TIDREGITAB.BERANTAL = nyber1.        
         END.
         ELSE IF ohjalp.OSL2 > TIDREGITAB.BEREDSKAPSLUT AND
         ohjalp.OSL2 > regstart AND ohjalp.OSL2 < regslut THEN DO:
            nytid = ohjalp.OANT2.
            RUN TIMSEK.P.  
            ASSIGN 
            seku = sekunder
            nytid = TIDREGITAB.BERANTAL.
            RUN TIMSEK.P.
            sekunder = sekunder - seku.
            IF sekunder < 0 THEN sekunder = 0.
            RUN SEKTIM.P.
            nyber1 = nytid.
            ASSIGN TIDREGITAB.BERANTAL = nyber1.
         END.
         ELSE IF ohjalp.OSL2 > TIDREGITAB.BEREDSKAPSLUT THEN DO: 
            nytid = ohjalp.OST2.  
            RUN TIMSEK.P.  
            ASSIGN 
            seku = sekunder
            nytid = TIDREGITAB.BEREDSKAPSLUT.  
            RUN TIMSEK.P.
            ASSIGN
            seku = sekunder - seku
            nytid = TIDREGITAB.BERANTAL.
            RUN TIMSEK.P.
            sekunder = sekunder - seku.
            IF sekunder < 0 THEN sekunder = 0.
            RUN SEKTIM.P.
            nyber1 = nytid.
            ASSIGN TIDREGITAB.BERANTAL = nyber1.        
            IF Guru.Konstanter:globforetag = "sund" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" THEN DO:
               FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
               TIDREGITAB.DATUM = hjdat AND TIDREGITAB.BEREDSKAP NE '' AND
               TIDREGITAB.BEREDSKAPSTART = bsluta AND
               TIDREGITAB.TIDLOG = FALSE AND TIDREGITAB.BEREDSKAP NE "260" USE-INDEX PKOD NO-LOCK NO-ERROR.
            END.
            ELSE DO:            
               FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
               TIDREGITAB.DATUM = hjdat AND TIDREGITAB.BEREDSKAP NE '' AND
               TIDREGITAB.BEREDSKAPSTART = bsluta AND
               TIDREGITAB.TIDLOG = FALSE USE-INDEX PKOD NO-LOCK NO-ERROR.
            END.
            IF AVAILABLE TIDREGITAB THEN DO:            
               avberrec = RECID(TIDREGITAB).
               FIND TIDREGITAB WHERE RECID(TIDREGITAB) = avberrec EXCLUSIVE-LOCK NO-ERROR.
               IF ohjalp.OSL2 LE TIDREGITAB.BEREDSKAPSLUT THEN DO:
                  nytid = ohjalp.OSL2.
                  RUN TIMSEK.P.   
                  ASSIGN
                  seku = sekunder
                  nytid = TIDREGITAB.BEREDSKAPSTART.
                  RUN TIMSEK.P.
                  ASSIGN
                  seku = seku - sekunder
                  nytid = TIDREGITAB.BERANTAL.
                  RUN TIMSEK.P.
                  sekunder = sekunder - seku.
                  IF sekunder < 0 THEN sekunder = 0.
                  RUN SEKTIM.P.
                  ASSIGN TIDREGITAB.BERANTAL = nytid.          
               END.
            END.
         END.                     
      END. 
   END.                        
   IF ohjalp.OST3 = 0 AND ohjalp.OSL3 > 0 THEN hjdat = ohjalp.ODATUM + 1.
   ELSE hjdat = ohjalp.ODATUM. 
   IF ohjalp.OST3 > 0  OR ohjalp.OSL3 > 0 THEN DO TRANSACTION:
     IF Guru.Konstanter:globforetag = "sund" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" THEN DO:
        FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
        TIDREGITAB.DATUM = hjdat AND TIDREGITAB.BEREDSKAP NE '' AND
        TIDREGITAB.BEREDSKAPSTART LE ohjalp.OST3 AND TIDREGITAB.BEREDSKAPSLUT GE ohjalp.OST3
        AND TIDREGITAB.TIDLOG = FALSE AND TIDREGITAB.BEREDSKAP NE "260" USE-INDEX PKOD NO-LOCK NO-ERROR.
     END.
     ELSE DO:     
        FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
        TIDREGITAB.DATUM = hjdat AND TIDREGITAB.BEREDSKAP NE '' AND
        TIDREGITAB.BEREDSKAPSTART LE ohjalp.OST3 AND TIDREGITAB.BEREDSKAPSLUT GE ohjalp.OST3
        AND TIDREGITAB.TIDLOG = FALSE USE-INDEX PKOD NO-LOCK NO-ERROR.
     END.
     IF AVAILABLE TIDREGITAB THEN DO:                 
        avberrec = RECID(TIDREGITAB).
        FIND TIDREGITAB WHERE RECID(TIDREGITAB) = avberrec EXCLUSIVE-LOCK NO-ERROR.
        IF ohjalp.OSL3 LE TIDREGITAB.BEREDSKAPSLUT THEN DO:
           nytid = ohjalp.OANT3.
           RUN TIMSEK.P.   
           ASSIGN
           seku = sekunder
           nytid = TIDREGITAB.BERANTAL.
           RUN TIMSEK.P.
           sekunder = sekunder - seku.
           IF sekunder < 0 THEN sekunder = 0.
           RUN SEKTIM.P.
           nyber1 = nytid.
           ASSIGN TIDREGITAB.BERANTAL = nyber1.        
        END.                   
        ELSE IF ohjalp.OSL1 > TIDREGITAB.BEREDSKAPSLUT AND
        ohjalp.OSL1 > regstart AND ohjalp.OSL1 < regslut THEN DO:
           nytid = ohjalp.OANT1.
           RUN TIMSEK.P.  
           ASSIGN 
           seku = sekunder
           nytid = TIDREGITAB.BERANTAL.
           RUN TIMSEK.P.
           sekunder = sekunder - seku. 
           IF sekunder < 0 THEN sekunder = 0.
           RUN SEKTIM.P.
           nyber1 = nytid.
           ASSIGN TIDREGITAB.BERANTAL = nyber1.
        END.
        ELSE IF ohjalp.OSL3 > TIDREGITAB.BEREDSKAPSLUT THEN DO: 
           nytid = ohjalp.OST3.  
           RUN TIMSEK.P.  
           ASSIGN 
           seku = sekunder
           nytid = TIDREGITAB.BEREDSKAPSLUT.  
           RUN TIMSEK.P.
           ASSIGN
           seku = sekunder - seku
           nytid = TIDREGITAB.BERANTAL.
           RUN TIMSEK.P.
           sekunder = sekunder - seku.
           IF sekunder < 0 THEN sekunder = 0.
           RUN SEKTIM.P.
           nyber1 = nytid.
           ASSIGN TIDREGITAB.BERANTAL = nyber1.       
           IF Guru.Konstanter:globforetag = "sund" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" THEN DO:
              FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
              TIDREGITAB.DATUM = hjdat AND TIDREGITAB.BEREDSKAP NE '' AND
              TIDREGITAB.BEREDSKAPSTART = bsluta AND
              TIDREGITAB.TIDLOG = FALSE AND TIDREGITAB.BEREDSKAP NE "260" USE-INDEX PKOD NO-LOCK NO-ERROR.
           END.
           ELSE DO:           
              FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
              TIDREGITAB.DATUM = hjdat AND TIDREGITAB.BEREDSKAP NE '' AND
              TIDREGITAB.BEREDSKAPSTART = bsluta AND
              TIDREGITAB.TIDLOG = FALSE USE-INDEX PKOD NO-LOCK NO-ERROR.
           END.
           IF AVAILABLE TIDREGITAB THEN DO:             
              avberrec = RECID(TIDREGITAB).
              FIND TIDREGITAB WHERE RECID(TIDREGITAB) = avberrec EXCLUSIVE-LOCK NO-ERROR. 
              IF ohjalp.OSL3 LE TIDREGITAB.BEREDSKAPSLUT THEN DO:
                 nytid = ohjalp.OSL3.
                 RUN TIMSEK.P.   
                 ASSIGN
                 seku = sekunder
                 nytid = TIDREGITAB.BEREDSKAPSTART.
                 RUN TIMSEK.P.
                 ASSIGN
                 seku = seku - sekunder
                 nytid = TIDREGITAB.BERANTAL.
                 RUN TIMSEK.P.
                 sekunder = sekunder - seku. 
                 IF sekunder < 0 THEN sekunder = 0.
                 RUN SEKTIM.P.
                 ASSIGN TIDREGITAB.BERANTAL = nytid.           
              END.
           END.
        END.             
     END. 
   END.                            
END.
IF Guru.Konstanter:globforetag = "sund" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "cELPA" THEN DO:
   
   /*Om det ligger fler än en beredskapslöneart för samma tid, som också skall göras avdrag för.
   Förutom beredskap läggs komptimmar  ut klämdag för  sundsvall
   beredskapkomptid mellan 8-16  övertid 0-8-17-24*/
   DO TRANSACTION:   
      FOR EACH TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND TIDREGITAB.DATUM = ohjalp.ODATUM 
      AND TIDREGITAB.BEREDSKAP NE '' USE-INDEX PKOD EXCLUSIVE-LOCK:
         FIND FIRST OVERKOD WHERE OVERKOD.OVERTIDTILL = TIDREGITAB.BEREDSKAP USE-INDEX OVER NO-LOCK NO-ERROR.
         IF AVAILABLE OVERKOD THEN DO:                  
            nytid = TIDREGITAB.BEREDSKAPSTART.
            RUN TIMSEK.P.
            seku = sekunder.             
            nytid = TIDREGITAB.BEREDSKAPSLUT .
            RUN TIMSEK.P.
            sekunder = sekunder - seku.
            RUN SEKTIM.P.
            TIDREGITAB.BERANTAL = nytid.            
            IF ohjalp.OSTART GE TIDREGITAB.BEREDSKAPSTART AND ohjalp.OSTART < TIDREGITAB.BEREDSKAPSLUT THEN DO: 
            /* Övertid  startar mellan 8-16 */
               IF ohjalp.OSL1 LE TIDREGITAB.BEREDSKAPSLUT THEN DO: 
                  nytid = ohjalp.OSL1.
                  RUN TIMSEK.P.  
                  ASSIGN 
                  seku = sekunder
                  nytid = ohjalp.OST1.
                  RUN TIMSEK.P.  
                  ASSIGN 
                  seku = seku - sekunder.
                  nytid = TIDREGITAB.BERANTAL.
                  RUN TIMSEK.P.
                  sekunder = sekunder - seku.  
                  IF sekunder < 0 THEN sekunder = 0.
                  RUN SEKTIM.P.
                  nyber1 = nytid.
                  ASSIGN TIDREGITAB.BERANTAL = nyber1.                  
                  IF ohjalp.OST2 NE ohjalp.OSL2 AND ohjalp.OSL2 LE TIDREGITAB.BEREDSKAPSLUT THEN DO: 
                     nytid = ohjalp.OSL2.
                     RUN TIMSEK.P.  
                     ASSIGN 
                     seku = sekunder
                     nytid = ohjalp.OST2.
                     RUN TIMSEK.P.  
                     ASSIGN 
                     seku = seku - sekunder.
                     nytid = TIDREGITAB.BERANTAL.
                     RUN TIMSEK.P.
                     sekunder = sekunder - seku.  
                     IF sekunder < 0 THEN sekunder = 0.
                     RUN SEKTIM.P.
                     nyber1 = nytid.
                     ASSIGN TIDREGITAB.BERANTAL = nyber1.
                  END.
                  ELSE IF ohjalp.OST2 NE ohjalp.OSL2 AND ohjalp.OSL2 > TIDREGITAB.BEREDSKAPSLUT THEN DO:
                     nytid = TIDREGITAB.BEREDSKAPSLUT.
                     RUN TIMSEK.P.  
                     ASSIGN 
                     seku = sekunder
                     nytid = ohjalp.OST1.
                     RUN TIMSEK.P.  
                     ASSIGN 
                     seku = seku - sekunder.
                     nytid = TIDREGITAB.BERANTAL.
                     RUN TIMSEK.P.
                     sekunder = sekunder - seku.  
                     IF sekunder < 0 THEN sekunder = 0.
                     RUN SEKTIM.P.
                     nyber1 = nytid.
                     ASSIGN TIDREGITAB.BERANTAL = nyber1.
                  END.
               END.
               ELSE DO:
                  nytid = TIDREGITAB.BEREDSKAPSLUT.
                  RUN TIMSEK.P.  
                  ASSIGN 
                  seku = sekunder
                  nytid = ohjalp.OST1.
                  RUN TIMSEK.P.  
                  ASSIGN 
                  seku = seku - sekunder.
                  nytid = TIDREGITAB.BERANTAL.
                  RUN TIMSEK.P.
                  sekunder = sekunder - seku.  
                  IF sekunder < 0 THEN sekunder = 0.
                  RUN SEKTIM.P.
                  nyber1 = nytid.
                  ASSIGN TIDREGITAB.BERANTAL = nyber1.
               END.
            END.
            ELSE IF ohjalp.OSLUT GE TIDREGITAB.BEREDSKAPSTART AND ohjalp.OSLUT LE TIDREGITAB.BEREDSKAPSLUT THEN DO: 
            /* Övertid  slutar mellan 8-16*/
               IF ohjalp.OSL1 GE TIDREGITAB.BEREDSKAPSTART  THEN DO:
                  IF ohjalp.OST1 GE TIDREGITAB.BEREDSKAPSTART  THEN DO:               
                     nytid = ohjalp.OSL1.
                     RUN TIMSEK.P.  
                     ASSIGN 
                     seku = sekunder
                     nytid = ohjalp.OST1.
                     RUN TIMSEK.P.  
                     ASSIGN 
                     seku = seku - sekunder.
                     nytid = TIDREGITAB.BERANTAL.
                     RUN TIMSEK.P.
                     sekunder = sekunder - seku.  
                     IF sekunder < 0 THEN sekunder = 0.
                     RUN SEKTIM.P.
                     nyber1 = nytid.
                     ASSIGN TIDREGITAB.BERANTAL = nyber1.
                  END.
                  ELSE IF ohjalp.OST1 < TIDREGITAB.BEREDSKAPSTART  THEN DO:               
                     nytid = ohjalp.OSL1.
                     RUN TIMSEK.P.  
                     ASSIGN 
                     seku = sekunder
                     nytid = TIDREGITAB.BEREDSKAPSTART.
                     RUN TIMSEK.P.  
                     ASSIGN 
                     seku = seku - sekunder.
                     nytid = TIDREGITAB.BERANTAL.
                     RUN TIMSEK.P.
                     sekunder = sekunder - seku.  
                     IF sekunder < 0 THEN sekunder = 0.
                     RUN SEKTIM.P.
                     nyber1 = nytid.
                     ASSIGN TIDREGITAB.BERANTAL = nyber1.
                  END.
                     
               END.
               ELSE IF ohjalp.OSL1 < TIDREGITAB.BEREDSKAPSTART  THEN DO:               
                  IF ohjalp.OSL2 > TIDREGITAB.BEREDSKAPSTART  THEN DO:                            
                     nytid = ohjalp.OSL2.
                     RUN TIMSEK.P.  
                     ASSIGN 
                     seku = sekunder
                     nytid = TIDREGITAB.BEREDSKAPSTART.
                     RUN TIMSEK.P.  
                     ASSIGN 
                     seku = seku - sekunder.
                     nytid = TIDREGITAB.BERANTAL.
                     RUN TIMSEK.P.
                     sekunder = sekunder - seku.  
                     IF sekunder < 0 THEN sekunder = 0.
                     RUN SEKTIM.P.
                     nyber1 = nytid.
                     ASSIGN TIDREGITAB.BERANTAL = nyber1.
                  END.
               END.               
            END.
            ELSE IF ohjalp.OSTART < TIDREGITAB.BEREDSKAPSTART AND ohjalp.OSLUT GE TIDREGITAB.BEREDSKAPSLUT THEN DO: 
               /* Övertid  startar före 8 och slutar efter 16*/
               ASSIGN TIDREGITAB.BERANTAL = 0.
            END.            
         END.
      END.
   END.
END. 
