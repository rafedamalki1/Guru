/*berover.P  UTAN AVDRAG BEREDSKAP*/
DEFINE SHARED VARIABLE regstart LIKE TIDREGITAB.START NO-UNDO. 
DEFINE SHARED VARIABLE regslut LIKE TIDREGITAB.SLUT NO-UNDO.      
DEFINE SHARED VARIABLE regdatum AS DATE NO-UNDO.
DEFINE SHARED VARIABLE bdatum AS DATE NO-UNDO.
DEFINE SHARED VARIABLE avdatum AS DATE NO-UNDO. 

DEFINE VARIABLE datb AS DATE NO-UNDO.
DEFINE VARIABLE data AS DATE NO-UNDO.
DEFINE SHARED VARIABLE nytid AS DECIMAL FORMAT "99.99" NO-UNDO.
DEFINE SHARED VARIABLE sekunder AS INTEGER FORMAT "-9999999" NO-UNDO.
DEFINE  SHARED VARIABLE persrec AS RECID NO-UNDO.
DEFINE  SHARED VARIABLE persrec2 AS RECID NO-UNDO.
DEFINE SHARED VARIABLE vart AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE  SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE reco AS RECID NO-UNDO. 
DEFINE NEW SHARED VARIABLE bustart3 LIKE TIDREGITAB.START NO-UNDO.      
DEFINE VARIABLE krav AS LOGICAL NO-UNDO.
DEFINE VARIABLE regdat1 AS DATE NO-UNDO.
DEFINE VARIABLE regdat2 AS DATE NO-UNDO.  
DEFINE VARIABLE dat9 AS DATE NO-UNDO.
DEFINE VARIABLE busl3 LIKE TIDREGITAB.SLUT NO-UNDO.
DEFINE BUFFER tidbuff FOR TIDREGITAB.
FIND PERSONALTAB WHERE RECID(PERSONALTAB) = persrec NO-LOCK NO-ERROR.
FIND FIRST ANSTFORMTAB WHERE ANSTFORM.ANSTALLNING = PERSONALTAB.ANSTALLNING
USE-INDEX ANSTF NO-LOCK NO-ERROR.
DO:
   FIND FIRST UTRYCKNING WHERE UTRYCKNING.KOD = ANSTFORMTAB.KOD
   USE-INDEX UT NO-LOCK NO-ERROR.
   FIND FIRST UTRTAB WHERE UTRTAB.KOD = ANSTFORMTAB.KOD USE-INDEX UTRSTART NO-LOCK
   NO-ERROR.                
   IF AVAILABLE UTRTAB THEN krav = TRUE.
   IF UTRYCKNING.AVBE = TRUE THEN krav = TRUE.
   IF UTRYCKNING.UTRYCKNBER NE UTRYCKNING.UTRYCKNEJBER THEN krav = TRUE.
   IF krav = TRUE THEN DO: 
  /* IF UTRYCKNING.AVBE = TRUE THEN DO: */
      dat9 = regdatum.
      datb = bdatum.
      data = avdatum.
      regdatum = datb. 
      regdat1 = regdatum.                    
      busl3 = 0.
      over1:
      REPEAT:                   
         IF regdat1 > data THEN LEAVE over1.
         FIND FIRST tidbuff WHERE tidbuff.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
         tidbuff.DATUM = regdat1 AND tidbuff.OVERAUTO = FALSE 
         USE-INDEX PSTART NO-LOCK NO-ERROR.
         IF NOT AVAILABLE tidbuff THEN DO:
            RUN SLUTARB.P.
            FIND FIRST tidbuff WHERE tidbuff.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
            tidbuff.DATUM = regdat1 AND tidbuff.OVERAUTO = TRUE AND
            tidbuff.TIDLOG = TRUE AND 
            (tidbuff.START GE regslut OR tidbuff.START < regstart) 
            USE-INDEX PSTART NO-LOCK NO-ERROR.
            IF NOT AVAILABLE tidbuff THEN persrec = persrec.
            ELSE IF busl3 = 24.00 AND tidbuff.START = 00.00 AND tidbuff.DATUM = 
            regdat2 + 1 THEN persrec = persrec.
            ELSE DO:
               ASSIGN                        
               bustart3 = tidbuff.START
               busl3 = tidbuff.SLUT      
               regdat2 = tidbuff.DATUM.
               IF Guru.Konstanter:globforetag = "GRAN" 
                 OR Guru.Konstanter:globforetag = "ELPA"   THEN DO:
                  IF ANSTFORMTAB.KOD BEGINS "K" THEN DO TRANSACTION :                        
                      FIND CURRENT tidbuff EXCLUSIVE-LOCK.
                      ASSIGN tidbuff.UTRYCKN = FALSE.                                                 
                  END.                     
               END.                 
               RUN OTOLKPR.P.
               regdatum = regdat1.               
            END.    
            over2:
            REPEAT:
               FIND NEXT tidbuff WHERE 
               tidbuff.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
               tidbuff.DATUM = regdat1 AND tidbuff.OVERAUTO = TRUE AND
               tidbuff.TIDLOG = TRUE AND 
               (tidbuff.START GE regslut OR tidbuff.START < regstart) 
               USE-INDEX PSTART NO-LOCK NO-ERROR.
               IF NOT AVAILABLE tidbuff THEN LEAVE over2.                 
               ELSE DO:                  
                  bustart3 = tidbuff.START.
                  busl3 = tidbuff.SLUT.      
                  regdat2 = tidbuff.DATUM.
                  IF Guru.Konstanter:globforetag = "GRAN" 
                    OR Guru.Konstanter:globforetag = "ELPA"   THEN DO:
                     IF ANSTFORMTAB.KOD BEGINS "K" THEN DO TRANSACTION :                        
                        FIND CURRENT tidbuff EXCLUSIVE-LOCK.
                        ASSIGN tidbuff.UTRYCKN = FALSE.                                                 
                     END.                     
                  END.                  
                  RUN OTOLKPR.P.  
                  regdatum = regdat1.                  
               END.
            END.   
            regdat1 = regdat1 + 1.
            regdatum = regdat1.
         END. 
      END.   
      regdatum = dat9.
   END.
END. 
