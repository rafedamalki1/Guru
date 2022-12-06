/*berover2.P  MED AVDRAG BER*/
DEFINE SHARED VARIABLE regstart LIKE TIDREGITAB.START NO-UNDO. 
DEFINE SHARED VARIABLE regslut LIKE TIDREGITAB.SLUT NO-UNDO.      
DEFINE SHARED VARIABLE regdatum AS DATE NO-UNDO.
DEFINE SHARED VARIABLE bdatum AS DATE NO-UNDO.
DEFINE SHARED VARIABLE avdatum AS DATE NO-UNDO. 
DEFINE SHARED VARIABLE nytid AS DECIMAL FORMAT "99.99" NO-UNDO.
DEFINE SHARED VARIABLE sekunder AS INTEGER FORMAT "-9999999" NO-UNDO.
DEFINE SHARED VARIABLE persrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE persrec2 AS RECID NO-UNDO.
DEFINE SHARED VARIABLE vart AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.

DEFINE NEW SHARED VARIABLE reco AS RECID NO-UNDO. 
DEFINE NEW SHARED VARIABLE bustart3 LIKE TIDREGITAB.START NO-UNDO.  
DEFINE VARIABLE datb AS DATE NO-UNDO.
DEFINE VARIABLE data AS DATE NO-UNDO.
DEFINE VARIABLE krav AS LOGICAL NO-UNDO.
DEFINE VARIABLE regdat1 AS DATE NO-UNDO.
DEFINE VARIABLE regdat2 AS DATE NO-UNDO.
DEFINE VARIABLE dat9 AS DATE NO-UNDO.
DEFINE VARIABLE busl3 LIKE TIDREGITAB.SLUT NO-UNDO.
DEFINE NEW SHARED TEMP-TABLE ohjalp
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
FIND PERSONALTAB WHERE RECID(PERSONALTAB) = persrec NO-LOCK NO-ERROR.
FIND FIRST ANSTFORMTAB WHERE ANSTFORM.ANSTALLNING = PERSONALTAB.ANSTALLNING
USE-INDEX ANSTF NO-LOCK NO-ERROR.
FIND FIRST BEREDSKAPTAB WHERE BEREDSKAPTAB.BEREDSKAPSAVTAL =
PERSONALTAB.BEREDSKAPSAVTAL USE-INDEX BERED NO-LOCK NO-ERROR.
DO:
   FIND FIRST UTRYCKNING WHERE UTRYCKNING.KOD = ANSTFORMTAB.KOD
   USE-INDEX UT NO-LOCK NO-ERROR.  
   FIND FIRST UTRTAB WHERE UTRTAB.KOD = ANSTFORMTAB.KOD USE-INDEX UTRSTART NO-LOCK
   NO-ERROR.                
   IF AVAILABLE UTRTAB THEN krav = TRUE.
   IF UTRYCKNING.AVBE = TRUE THEN krav = TRUE.
   IF krav = TRUE THEN DO: 
      dat9 = regdatum.
      datb = bdatum.
      data = avdatum.
      regdatum = datb. 
      regdat1 = regdatum.                              
      busl3 = 0.
      over1:
      REPEAT:                   
         IF regdat1 > data THEN LEAVE over1.
         FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
         TIDREGITAB.DATUM = regdat1 AND TIDREGITAB.OVERAUTO = FALSE 
         USE-INDEX PSTART NO-LOCK NO-ERROR.
         IF NOT AVAILABLE TIDREGITAB THEN DO:
            RUN SLUTARB.P.
            FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
            TIDREGITAB.DATUM = regdat1 AND TIDREGITAB.OVERAUTO = TRUE AND
            TIDREGITAB.TIDLOG = TRUE AND 
            (TIDREGITAB.START GE regslut OR TIDREGITAB.START < regstart) 
            USE-INDEX PSTART NO-LOCK NO-ERROR.
            IF NOT AVAILABLE TIDREGITAB THEN persrec = persrec.
            ELSE IF busl3 = 24.00 AND TIDREGITAB.START = 00.00 AND TIDREGITAB.DATUM = 
            regdat2 + 1 THEN persrec = persrec.
            ELSE DO:                 
               busl3 = TIDREGITAB.SLUT.      
               regdat2 = TIDREGITAB.DATUM.
               bustart3 = TIDREGITAB.START.
               RUN OTOLKPR.P.
            END.    
            over2:
            REPEAT:
               FIND NEXT TIDREGITAB WHERE 
               TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
               TIDREGITAB.DATUM = regdat1 AND TIDREGITAB.OVERAUTO = TRUE AND
               TIDREGITAB.TIDLOG = TRUE AND 
               (TIDREGITAB.START GE regslut OR TIDREGITAB.START < regstart) 
               USE-INDEX PSTART NO-LOCK NO-ERROR.
               IF NOT AVAILABLE TIDREGITAB THEN LEAVE over2.                 
               ELSE DO:                  
                  bustart3 = TIDREGITAB.START.
                  busl3 = TIDREGITAB.SLUT.      
                  regdat2 = TIDREGITAB.DATUM.
                  RUN OTOLKPR.P.   
               END.
            END.    
            regdat1 = regdat1 + 1.
            regdatum = regdat1.
         END.  
         ELSE IF UTRYCKNING.AVBE = TRUE THEN DO:
            IF BEREDSKAPTAB.BERANTAL > 0 THEN regdatum = regdatum.
            ELSE DO:    
               FOR EACH TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
               TIDREGITAB.DATUM = regdatum AND TIDREGITAB.TIDLOG = FALSE
               AND TIDREGITAB.OANT1 > 0 AND TIDREGITAB.OVERAUTO = FALSE USE-INDEX PSTART NO-LOCK:
                  CREATE ohjalp.
                  ASSIGN ohjalp.ODATUM = TIDREGITAB.DATUM ohjalp.OSLUT = TIDREGITAB.SLUT
                  ohjalp.OSTART = TIDREGITAB.START
                  ohjalp.OKOD1 = TIDREGITAB.OKOD1 ohjalp.OANT1 = TIDREGITAB.OANT1
                  ohjalp.OST1 = TIDREGITAB.OST1 ohjalp.OSL1 = TIDREGITAB.OSL1
                  ohjalp.OKOD2 = TIDREGITAB.OKOD2 ohjalp.OANT2 = TIDREGITAB.OANT2
                  ohjalp.OST2 = TIDREGITAB.OST2 ohjalp.OSL2 = TIDREGITAB.OSL2
                  ohjalp.OKOD3 = TIDREGITAB.OKOD3 ohjalp.OANT3 = TIDREGITAB.OANT3
                  ohjalp.OST3 = TIDREGITAB.OST3 ohjalp.OSL3 = TIDREGITAB.OSL3
                  ohjalp.OTOTALT = TIDREGITAB.TOTALT
                  ohjalp.OOVERAUTO = TIDREGITAB.OVERAUTO
                  ohjalp.OAONR = TIDREGITAB.AONR ohjalp.ODELNR = TIDREGITAB.DELNR
                  ohjalp.OUTR = TIDREGITAB.UTRYCKNING ohjalp.RECTIDVIS = RECID(TIDREGITAB)
                  ohjalp.OAVBE = TIDREGITAB.LAGANTAL.
               END.  
               FIND FIRST ohjalp NO-LOCK NO-ERROR.
               IF AVAILABLE ohjalp THEN DO:
                  reco = ohjalp.RECTIDVIS.                  
                  RUN AVBER.P.
               END.   
               over3:
               REPEAT:                                                   
                 FIND NEXT ohjalp NO-LOCK NO-ERROR.
                 IF NOT AVAILABLE ohjalp THEN LEAVE over3.
                 ELSE DO:   
                    reco = ohjalp.RECTIDVIS.                  
                    RUN AVBER.P.
                 END.
               END.     
            END. 
         END.   
      END. 
      regdatum = dat9.  
   END.   
END. 