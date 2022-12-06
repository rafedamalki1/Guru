/*OVERKOM.P LAGG TILLBAKA */

DEFINE SHARED VARIABLE persrec AS RECID  NO-UNDO.
DEFINE SHARED VARIABLE regdatum AS DATE NO-UNDO.
DEFINE SHARED VARIABLE bdatum AS DATE NO-UNDO.
DEFINE SHARED VARIABLE avdatum AS DATE NO-UNDO.
DEFINE SHARED VARIABLE regstart AS DECIMAL NO-UNDO.
DEFINE SHARED VARIABLE regslut AS DECIMAL NO-UNDO.    
DEFINE SHARED VARIABLE lunchstarten AS DECIMAL NO-UNDO.
DEFINE SHARED VARIABLE lunchslutet AS DECIMAL NO-UNDO.
DEFINE SHARED VARIABLE nytid AS DECIMAL NO-UNDO.
DEFINE SHARED VARIABLE sekunder AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE reco AS RECID NO-UNDO.

DEFINE SHARED VARIABLE start4 AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE komp LIKE OVERTIDTAB.OVERTIDUTTAG NO-UNDO.
DEFINE SHARED VARIABLE bslut LIKE TIDREGITAB.SLUT NO-UNDO.
DEFINE SHARED VARIABLE bstart LIKE TIDREGITAB.START NO-UNDO.
DEFINE SHARED VARIABLE avslut LIKE TIDREGITAB.SLUT NO-UNDO.
DEFINE SHARED VARIABLE avstart LIKE TIDREGITAB.START NO-UNDO.
DEFINE SHARED VARIABLE reco2 AS RECID NO-UNDO.     
/*DEFINE SHARED VARIABLE globanv LIKE ANVANDARE.ANVANDARE NO-UNDO.*/
DEFINE VARIABLE recbuff AS RECID NO-UNDO.       
DEFINE VARIABLE seku AS INTEGER NO-UNDO.
DEFINE VARIABLE ovhalv AS INTEGER NO-UNDO.
DEFINE VARIABLE tothalv AS INTEGER NO-UNDO.
DEFINE VARIABLE ovstart AS DECIMAL NO-UNDO.
DEFINE VARIABLE ovslut AS DECIMAL NO-UNDO.
DEFINE VARIABLE enkel AS DECIMAL FORMAT "99.99" NO-UNDO.
DEFINE VARIABLE overant AS INTEGER FORMAT "-9999999" NO-UNDO.
DEFINE VARIABLE otim5 AS DECIMAL FORMAT "99.99" NO-UNDO. 
DEFINE VARIABLE otim6 AS DECIMAL FORMAT "99.99" NO-UNDO. 
DEFINE VARIABLE otim AS INTEGER FORMAT "-9999999" NO-UNDO.  
DEFINE VARIABLE otim1 AS INTEGER FORMAT "-9999999" NO-UNDO. 
DEFINE VARIABLE otim2 AS INTEGER FORMAT "-9999999" NO-UNDO.              
DEFINE VARIABLE otim3 AS INTEGER FORMAT "-9999999" NO-UNDO. 

DEFINE VARIABLE otim4 AS DECIMAL NO-UNDO. 
DEFINE VARIABLE start10 LIKE TIDREGITAB.SLUT NO-UNDO.
DEFINE VARIABLE kvalgrans AS DECIMAL NO-UNDO.
DEFINE VARIABLE slut11 LIKE TIDREGITAB.SLUT NO-UNDO.
DEFINE VARIABLE odatum AS DATE NO-UNDO.
DEFINE VARIABLE okod LIKE OVERTIDTAB.OVERTIDTILL NO-UNDO.    
DEFINE VARIABLE recko AS RECID NO-UNDO.
DEFINE VARIABLE otid LIKE TIDREGITAB.OVERTIDUTTAG NO-UNDO.
DEFINE VARIABLE htim AS INTEGER NO-UNDO.
DEFINE VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE enketid AS DECIMAL NO-UNDO.
DEFINE VARIABLE omenkel AS INTEGER NO-UNDO.
DEFINE VARIABLE okoden AS INTEGER NO-UNDO.
DEFINE BUFFER tidbuff FOR TIDREGITAB.
DEFINE BUFFER tidbuff2 FOR TIDREGITAB.
DEFINE BUFFER tidbuff3 FOR TIDREGITAB.
DEFINE BUFFER okodbuff FOR OVERKOD.
DEFINE QUERY traktq FOR TIDREGITAB.


FIND PERSONALTAB WHERE RECID(PERSONALTAB) = persrec NO-LOCK NO-ERROR.
FIND FIRST ANSTFORMTAB WHERE ANSTFORMTAB.ANSTALLNING = PERSONALTAB.ANSTALLNING 
USE-INDEX ANSTF NO-LOCK NO-ERROR.

sekunder = 18000 + start4.
/* enkel 5-7 eller 6-8 SUND 5.3-7.3 SOMMAR 6-8 VINTER*/
IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" THEN DO: 
    IF regstart = 7 THEN sekunder = 18000 + start4.
    IF regstart = 7.3 THEN sekunder = 19800 + start4.
    IF regstart = 8 THEN sekunder = 21600 + start4.
END.
IF Guru.Konstanter:globforetag = "GKAL" THEN DO: 
    IF regstart = 7 THEN sekunder = 18000 + start4.
    IF regstart = 7.3 THEN sekunder = 19800 + start4.    
    IF regstart = 14 THEN sekunder = 43200.    
    IF regstart = 6 THEN sekunder = 14400 .    
END.
IF Guru.Konstanter:globforetag = "LULE" THEN DO:     
   /*leab*/
    IF regstart = 7.3 THEN sekunder = 19800 + start4.    
    IF regstart = 14.3 THEN sekunder = 45000.    
    IF regstart = 6 THEN sekunder = 14400 .    
    /*bio*/
    IF regstart = 7 THEN sekunder = 18000 + start4.    
    IF regstart = 14 THEN sekunder = 43200.    
END.
RUN SEKTIM.P.
start10 = nytid.
/* de som har arbetstid 6-14 skall ha kval om de går ut mellan 4-5*/
IF start10 < 5 THEN ASSIGN kvalgrans = start10.
ELSE kvalgrans = 5.
omenkel = 0.
/*omenkel- tid som arbetas mellan 5-7 men ger kval (start före 5), kvalificerar till kval övertid på aftonen,
arbtid 7-16 öv 4.3-7 och 16-18 ger alla timmar kval öv 4-6 och 16-18 kval morgon och 1 t enkel kväll 1 t kval kväll.*/
FOR EACH tidbuff2 WHERE tidbuff2.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
tidbuff2.DATUM = bdatum AND tidbuff2.TIDLOG = TRUE AND tidbuff2.OKOD1 NE "" USE-INDEX PSTART NO-LOCK:
   IF tidbuff2.START LE kvalgrans AND tidbuff2.SLUT > start10 THEN DO:
      IF tidbuff2.OSL1 = start10 AND tidbuff2.OST2 = start10 THEN DO:         
         nytid = tidbuff2.OANT2.
         RUN TIMSEK.P.
         omenkel =  omenkel + sekunder.
         IF tidbuff2.OSL2 = tidbuff2.OST3 AND tidbuff2.OKOD3 NE "" THEN DO:            
            nytid = tidbuff2.OANT3.
            RUN TIMSEK.P.
            omenkel =  omenkel + sekunder.
         END.
      END.
      ELSE IF tidbuff2.OST1 GE start10 THEN DO:
         nytid = tidbuff2.OANT1.
         RUN TIMSEK.P.
         omenkel =  omenkel + sekunder.
      END.
   END.
   ELSE IF start10 = 4 THEN DO:
      
      IF tidbuff2.START < 5 AND tidbuff2.SLUT > 5 THEN DO:
         IF tidbuff2.OSL1 = start10 AND tidbuff2.OST2 = start10 THEN DO:         
            nytid = tidbuff2.OANT2.
            RUN TIMSEK.P.
            omenkel =  omenkel + sekunder.
            IF tidbuff2.OSL2 = tidbuff2.OST3 AND tidbuff2.OKOD3 NE "" THEN DO:            
               nytid = tidbuff2.OANT3.
               RUN TIMSEK.P.
               omenkel =  omenkel + sekunder.
            END.
         END.
         ELSE IF tidbuff2.OST1 GE start10 THEN DO:
            nytid = tidbuff2.OANT1.
            RUN TIMSEK.P.
            omenkel =  omenkel + sekunder.
         END.
      END.
   END.   
   ELSE IF tidbuff2.START > kvalgrans AND tidbuff2.SLUT > start10 THEN DO:
      /*Om registreringen är uppdelad i två registreringar tex 3-6 6-7.3 ska de tolkas ihop*/
      FIND FIRST tidbuff3 WHERE tidbuff3.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
      tidbuff3.DATUM = bdatum AND tidbuff3.TIDLOG = TRUE AND tidbuff3.SLUT = tidbuff2.START AND tidbuff3.START LE kvalgrans USE-INDEX PSTART NO-LOCK NO-ERROR.
      IF AVAILABLE tidbuff3 THEN DO:
         IF tidbuff2.OSL1 = start10 AND tidbuff2.OST2 = start10 THEN DO:         
            nytid = tidbuff2.OANT2.
            RUN TIMSEK.P.
            omenkel =  omenkel + sekunder.
            IF tidbuff2.OSL2 = tidbuff2.OST3 AND tidbuff2.OKOD3 NE "" THEN DO:            
               nytid = tidbuff2.OANT3.
               RUN TIMSEK.P.
               omenkel =  omenkel + sekunder.
            END.
         END.
         ELSE IF tidbuff2.OST1 GE start10 THEN DO:
            nytid = tidbuff2.OANT1.
            RUN TIMSEK.P.
            omenkel =  omenkel + sekunder.
         END.            
      END.   
   END.   
END.
/*MISV nyttavtal 20220101 ej komptimmar för veckovila hal eller klämdag Lena Jönsson  */
IF Guru.Konstanter:globforetag = "MISV" THEN.
ELSE IF bstart < 5.00 AND bslut > kvalgrans THEN DO:
   
   DO TRANSACTION:                
      FIND FIRST TIDREGITAB WHERE RECID(TIDREGITAB) = reco EXCLUSIVE-LOCK NO-ERROR. 
      IF TIDREGITAB.OSL1 = start10 AND TIDREGITAB.OST2 = start10 THEN DO:         
         ASSIGN TIDREGITAB.OKOD2 = TIDREGITAB.OKOD1.                          
         IF TIDREGITAB.OSL2 = TIDREGITAB.OST3 AND TIDREGITAB.OKOD3 NE "" THEN DO:            
            ASSIGN TIDREGITAB.OKOD3 = TIDREGITAB.OKOD2.            
         END.   
      END.  
      ELSE IF TIDREGITAB.OSL2 = start10 AND TIDREGITAB.OST3 = start10 THEN DO:                  
         ASSIGN TIDREGITAB.OKOD3 = TIDREGITAB.OKOD2.         
      END.  
      ELSE IF start10 = 4 THEN DO:
         IF TIDREGITAB.OST1 GE 4  THEN DO:         
            FIND FIRST OVERKOD WHERE OVERKOD.KOD = ANSTFORMTAB.KOD AND OVERKOD.ENKEL = "KVAL"
            AND OVERKOD.OVERTIDUTTAG = TIDREGITAB.OVERTIDUTTAG  NO-LOCK NO-ERROR.
            IF AVAILABLE OVERKOD THEN DO:               
               ASSIGN TIDREGITAB.OKOD1 = OVERKOD.OVERTIDTILL.               
               IF TIDREGITAB.OKOD2 NE "" THEN DO:
                  ASSIGN TIDREGITAB.OKOD2 = OVERKOD.OVERTIDTILL.                  
               END.
            END.
         END.
      END.                              
   END.   
END.
ELSE IF avstart < 5.00 AND avslut > kvalgrans THEN DO:
      
   DO TRANSACTION:
      FIND FIRST TIDREGITAB WHERE RECID(TIDREGITAB) = reco2 EXCLUSIVE-LOCK NO-ERROR. 
      IF TIDREGITAB.OSL1 = start10 AND TIDREGITAB.OST2 = start10 THEN DO:
         ASSIGN TIDREGITAB.OKOD2 = TIDREGITAB.OKOD1.   
         IF TIDREGITAB.OSL2 = TIDREGITAB.OST3 AND TIDREGITAB.OKOD3 NE "" THEN DO:
            ASSIGN TIDREGITAB.OKOD3 = TIDREGITAB.OKOD2.            
         END. 
      END.  
      ELSE IF TIDREGITAB.OSL2 = start10 AND TIDREGITAB.OST3 = start10 THEN DO:
         ASSIGN TIDREGITAB.OKOD3 = TIDREGITAB.OKOD2.         
      END.  
   END.   
END. 

IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "GKAL" OR Guru.Konstanter:globforetag = "LULE" THEN DO:
   FIND FIRST TIDREGITAB WHERE RECID(TIDREGITAB) = reco NO-LOCK NO-ERROR.   
   otid = TIDREGITAB.OVERTIDUTTAG.   
   FIND LAST OVERKOD WHERE OVERKOD.KOD = ANSTFORMTAB.KOD AND OVERKOD.ENKEL = "ENKE"
   AND OVERKOD.OVERTIDUTTAG = otid NO-LOCK NO-ERROR.
   FIND FIRST okodbuff WHERE okodbuff.KOD = ANSTFORMTAB.KOD AND
   okodbuff.ENKEL = "KVAL" AND okodbuff.OVERTIDUTTAG = otid  NO-LOCK NO-ERROR.
   IF AVAILABLE OVERKOD THEN DO:   
      IF TIDREGITAB.START GE regslut AND TIDREGITAB.OKOD1 = OVERKOD.OVERTIDTILL  THEN DO TRANSACTION:      
         overant = 0.                  
         FIND FIRST tidbuff WHERE tidbuff.PERSONALKOD = PERSONALTAB.PERSONALKOD
         AND tidbuff.DATUM = bdatum AND tidbuff.SLUT LE regstart AND 
         tidbuff.SLUT GE ( regstart - 2 ) NO-LOCK NO-ERROR.          
         IF AVAILABLE tidbuff THEN DO:                          
            FIND LAST OVERKOD WHERE OVERKOD.KOD = ANSTFORMTAB.KOD AND OVERKOD.ENKEL = "ENKE"
            AND OVERKOD.OVERTIDTILL = tidbuff.OKOD1 NO-LOCK NO-ERROR. 
            IF NOT AVAILABLE OVERKOD THEN DO:
               IF tidbuff.OKOD2 NE "" THEN DO:            
                  FIND LAST OVERKOD WHERE OVERKOD.KOD = ANSTFORMTAB.KOD AND OVERKOD.ENKEL = "ENKE"
                  AND OVERKOD.OVERTIDTILL = tidbuff.OKOD2 NO-LOCK NO-ERROR. 
               END.
            END.         
            IF AVAILABLE OVERKOD THEN DO:                                              
               IF tidbuff.OKOD1 = OVERKOD.OVERTIDTILL THEN DO:               
                  nytid = tidbuff.OANT1.
                  RUN TIMSEK.P.
                  otim1 = sekunder.
                  nytid = TIDREGITAB.OANT1.
                  RUN TIMSEK.P.
                  otim2 = sekunder.
                  overant = otim2 + otim1.                                                
                  IF overant >  7200  THEN DO:
                     FIND FIRST TIDREGITAB WHERE RECID(TIDREGITAB) = reco EXCLUSIVE-LOCK NO-ERROR.
                     otim = overant - 7200.
                     sekunder = otim.
                     RUN SEKTIM.P.
                     otim4 = nytid.                  
                     IF otim4 GE TIDREGITAB.OANT1 THEN DO:                                        
                        IF AVAILABLE okodbuff THEN DO:                  
                           ASSIGN  TIDREGITAB.OKOD1 = okodbuff.OVERTIDTILL.
                        END.
                     END.      
                     ELSE IF AVAILABLE okodbuff THEN DO:                     
                        nytid = TIDREGITAB.OANT1.
                        RUN TIMSEK.P.                    
                        sekunder = sekunder - otim.
                        seku = sekunder.
                        RUN SEKTIM.P.
                        ASSIGN TIDREGITAB.OANT1 = nytid.
                        nytid = TIDREGITAB.OST1.
                        RUN TIMSEK.P.
                        sekunder = sekunder + seku.
                        seku = sekunder.
                        RUN SEKTIM.P.
                        IF TIDREGITAB.OSL2 = 0 THEN ASSIGN TIDREGITAB.OSL2 = TIDREGITAB.OSL1.
                        ASSIGN TIDREGITAB.OSL1 = nytid
                        TIDREGITAB.OST2 =  nytid.
                        nytid = TIDREGITAB.OSL2.
                        RUN TIMSEK.P.
                        sekunder = sekunder  - seku.
                        RUN SEKTIM.P.
                        ASSIGN TIDREGITAB.OANT2 = nytid
                        TIDREGITAB.OKOD2 = okodbuff.OVERTIDTILL.               
                     END.                        
                  END.
               END.      
               ELSE IF tidbuff.OKOD2 = OVERKOD.OVERTIDTILL THEN DO:               
                  nytid = tidbuff.OANT2.
                  RUN TIMSEK.P.
                  otim1 = sekunder.
                  nytid = TIDREGITAB.OANT1.
                  RUN TIMSEK.P.
                  otim2 = sekunder.
                  overant = otim2 + otim1.                                 
                  IF overant >  7200  THEN DO:
                     FIND FIRST TIDREGITAB WHERE RECID(TIDREGITAB) = reco EXCLUSIVE-LOCK NO-ERROR.
                     otim = overant - 7200.
                     sekunder = otim.
                     RUN SEKTIM.P.
                     otim4 = nytid.                  
                     IF otim4 GE TIDREGITAB.OANT1 THEN DO:                                        
                        IF AVAILABLE okodbuff THEN DO:                  
                           ASSIGN  TIDREGITAB.OKOD1 = okodbuff.OVERTIDTILL.
                        END.
                     END.      
                     ELSE IF AVAILABLE okodbuff THEN DO:            
                        nytid = TIDREGITAB.OANT1.
                        RUN TIMSEK.P.                    
                        sekunder = sekunder - otim.
                        seku = sekunder.
                        RUN SEKTIM.P.
                        ASSIGN TIDREGITAB.OANT1 = nytid.
                        nytid = TIDREGITAB.OST1.
                        RUN TIMSEK.P.
                        sekunder = sekunder + seku.
                        seku = sekunder.
                        RUN SEKTIM.P.
                        IF TIDREGITAB.OSL2 = 0 THEN ASSIGN TIDREGITAB.OSL2 = TIDREGITAB.OSL1.
                        ASSIGN TIDREGITAB.OSL1 = nytid
                        TIDREGITAB.OST2 =  nytid.
                        nytid = TIDREGITAB.OSL2.
                        RUN TIMSEK.P.
                        sekunder = sekunder  - seku.
                        RUN SEKTIM.P.
                        ASSIGN TIDREGITAB.OANT2 = nytid
                        TIDREGITAB.OKOD2 = okodbuff.OVERTIDTILL.               
                     END.                        
                  END.
               END.      
            END.        
            ELSE DO:                                     
               /* omenkel- kval på morgonen mellan 5-7 som ända skall kvalificera till kval på eftermiddagen*/            
               IF omenkel > 0 THEN DO:                                            
                  nytid = TIDREGITAB.OANT1.
                  RUN TIMSEK.P.
                  otim2 = sekunder.
                  overant = otim2 + otim1.                  
                  overant = overant + omenkel.               
                  IF overant >  7200  THEN DO:
                     FIND FIRST TIDREGITAB WHERE RECID(TIDREGITAB) = reco EXCLUSIVE-LOCK NO-ERROR.
                     otim = overant - 7200.
                     sekunder = otim.
                     RUN SEKTIM.P.
                     otim4 = nytid.                  
                     IF otim4 GE TIDREGITAB.OANT1 THEN DO:                                        
                        IF AVAILABLE okodbuff THEN DO:                  
                           ASSIGN  TIDREGITAB.OKOD1 = okodbuff.OVERTIDTILL.
                        END.
                     END.      
                     ELSE IF AVAILABLE okodbuff THEN DO:                     
                        nytid = TIDREGITAB.OANT1.
                        RUN TIMSEK.P.                    
                        sekunder = sekunder - otim.
                        seku = sekunder.
                        RUN SEKTIM.P.
                        ASSIGN TIDREGITAB.OANT1 = nytid.
                        nytid = TIDREGITAB.OST1.
                        RUN TIMSEK.P.
                        sekunder = sekunder + seku.
                        seku = sekunder.
                        RUN SEKTIM.P.
                        IF TIDREGITAB.OSL2 = 0 THEN ASSIGN TIDREGITAB.OSL2 = TIDREGITAB.OSL1.
                        ASSIGN TIDREGITAB.OSL1 = nytid
                        TIDREGITAB.OST2 =  nytid.
                        nytid = TIDREGITAB.OSL2.
                        RUN TIMSEK.P.
                        sekunder = sekunder  - seku.
                        RUN SEKTIM.P.
                        ASSIGN TIDREGITAB.OANT2 = nytid
                        TIDREGITAB.OKOD2 = okodbuff.OVERTIDTILL.               
                     END.                        
                  END.               
                  
               END.
            END.
            
         END.
      END.
      ELSE IF TIDREGITAB.SLUT LE regstart THEN DO TRANSACTION:             
         /*FINNS DET ENKEL ÖVERTID FÖRE ARBETSTID*/      
         IF TIDREGITAB.OKOD1 = OVERKOD.OVERTIDTILL OR TIDREGITAB.OKOD2 = OVERKOD.OVERTIDTILL THEN DO:      
            IF TIDREGITAB.OKOD1 = OVERKOD.OVERTIDTILL THEN enketid = TIDREGITAB.OANT1.
            IF TIDREGITAB.OKOD2 = OVERKOD.OVERTIDTILL THEN enketid = TIDREGITAB.OANT2.         
            IF TIDREGITAB.START GE 5 THEN DO:
               /*uppdelad sammanhängande tid som startar före 5*/
               FIND FIRST tidbuff WHERE tidbuff.PERSONALKOD = PERSONALTAB.PERSONALKOD
               AND tidbuff.DATUM = TIDREGITAB.DATUM AND tidbuff.SLUT = TIDREGITAB.START AND 
               tidbuff.START < 5 NO-LOCK NO-ERROR.              
               IF AVAILABLE tidbuff THEN DO:
                  IF TIDREGITAB.OKOD1 = OVERKOD.OVERTIDTILL THEN TIDREGITAB.OKOD1 = okodbuff.OVERTIDTILL.
                  IF TIDREGITAB.OKOD2 = OVERKOD.OVERTIDTILL THEN TIDREGITAB.OKOD2 = okodbuff.OVERTIDTILL.
               END.
            END.                                       
            overant = 0.         
            FIND FIRST tidbuff WHERE tidbuff.PERSONALKOD = PERSONALTAB.PERSONALKOD
            AND tidbuff.DATUM = bdatum AND tidbuff.START GE regslut AND 
            tidbuff.START < ( regslut + 2 ) NO-LOCK NO-ERROR.              
            IF AVAILABLE tidbuff THEN DO:            
               FIND FIRST okodbuff WHERE okodbuff.KOD = ANSTFORMTAB.KOD AND
               okodbuff.ENKEL = "KVAL" AND okodbuff.OVERTIDUTTAG = tidbuff.OVERTIDUTTAG  NO-LOCK NO-ERROR.
               recbuff = RECID(tidbuff).
                nytid = enketid.   
                RUN TIMSEK.P.
                otim1 = sekunder.
                nytid = tidbuff.OANT1.
                RUN TIMSEK.P.
                otim2 = sekunder.
                overant = otim2 + otim1.                
                overant = overant + omenkel.             
                IF overant > 7200  THEN DO:                
                   FIND FIRST tidbuff WHERE RECID(tidbuff) = recbuff EXCLUSIVE-LOCK NO-ERROR.
                   otim = overant - 7200.
                   sekunder = otim.
                   RUN SEKTIM.P.
                   otim4 = nytid.
                   IF otim4 GE tidbuff.OANT1 THEN DO:                                                           
                      IF AVAILABLE okodbuff THEN DO:                  
                         ASSIGN  tidbuff.OKOD1 = okodbuff.OVERTIDTILL.
                      END.
                   END.      
                   ELSE IF AVAILABLE okodbuff THEN DO:                   
                      nytid = tidbuff.OANT1.
                      RUN TIMSEK.P.                    
                      sekunder = sekunder - otim.
                      seku = sekunder.
                      RUN SEKTIM.P.
                      ASSIGN tidbuff.OANT1 = nytid.
                      nytid = tidbuff.OST1.
                      RUN TIMSEK.P.
                      sekunder = sekunder + seku.
                      seku = sekunder.
                      RUN SEKTIM.P.
                      IF tidbuff.OSL2 = 0 THEN ASSIGN tidbuff.OSL2 = tidbuff.OSL1.
                      ASSIGN tidbuff.OSL1 = nytid
                      tidbuff.OST2 =  nytid. 
                      nytid = tidbuff.OSL2.
                      RUN TIMSEK.P.
                      sekunder = sekunder  - seku.
                      RUN SEKTIM.P.
                      ASSIGN tidbuff.OANT2 = nytid
                      tidbuff.OKOD2 = okodbuff.OVERTIDTILL.               
                  END.                                   
               END.               
            END.
            ELSE DO:                         
               /*ex arbetstid 7.30-16.30 reg 5.20-7.3  ger 5.2-5.3 kval 5.3-.7.3 enkel 7.3-7.5 kval max 2tim enkel*/
               IF TIDREGITAB.OKOD1 NE "" AND TIDREGITAB.OKOD2 NE "" AND TIDREGITAB.OKOD3 NE "" THEN DO:
                  FIND FIRST okodbuff WHERE okodbuff.KOD = ANSTFORMTAB.KOD AND
                  okodbuff.ENKEL = "KVAL" AND okodbuff.OVERTIDUTTAG = TIDREGITAB.OVERTIDUTTAG  NO-LOCK NO-ERROR.
                  IF TIDREGITAB.OANT2 = 2 AND TIDREGITAB.OKOD2 = TIDREGITAB.OKOD3 THEN DO:
                     IF TIDREGITAB.OKOD3 NE okodbuff.OVERTIDTILL THEN DO:
                        TIDREGITAB.OKOD3 = okodbuff.OVERTIDTILL.
                     END.     
                  END.
               END.
            END.
         END.
      END.
          
      IF regstart = 0 AND regslut = 24 THEN DO:             
         /*tolka max 2 timmar enkel övertid dag med schema 0-6 22-24*/
         IF TIDREGITAB.SLUT > (lunchslutet - 2) AND TIDREGITAB.SLUT LE lunchslutet THEN DO TRANSACTION:         
            /*övertid 20-22*/         
            IF TIDREGITAB.OKOD1 = OVERKOD.OVERTIDTILL OR TIDREGITAB.OKOD2 = OVERKOD.OVERTIDTILL THEN DO:                  
               IF TIDREGITAB.OKOD1 = OVERKOD.OVERTIDTILL THEN DO: 
                  okoden = 1.
                  enketid = TIDREGITAB.OANT1.
               END.
               IF TIDREGITAB.OKOD2 = OVERKOD.OVERTIDTILL THEN DO: 
                  okoden = 2.
                  enketid = TIDREGITAB.OANT2.                     
               END.            
               overant = 0.                       
               FIND FIRST tidbuff WHERE tidbuff.PERSONALKOD = PERSONALTAB.PERSONALKOD
               AND tidbuff.DATUM = bdatum AND tidbuff.START GE lunchstarten AND 
               tidbuff.START < ( lunchstarten + 2 ) NO-LOCK NO-ERROR.             
               IF AVAILABLE tidbuff THEN DO:                                
                  FIND LAST OVERKOD WHERE OVERKOD.KOD = ANSTFORMTAB.KOD AND OVERKOD.ENKEL = "ENKE"
                  AND OVERKOD.OVERTIDTILL = tidbuff.OKOD1 NO-LOCK NO-ERROR. 
                  IF NOT AVAILABLE OVERKOD THEN DO:
                     IF tidbuff.OKOD2 NE "" THEN DO:            
                        FIND LAST OVERKOD WHERE OVERKOD.KOD = ANSTFORMTAB.KOD AND OVERKOD.ENKEL = "ENKE"
                        AND OVERKOD.OVERTIDTILL = tidbuff.OKOD2 NO-LOCK NO-ERROR. 
                     END.
                  END.         
                  IF AVAILABLE OVERKOD THEN DO:                           
                     IF tidbuff.OKOD1 = OVERKOD.OVERTIDTILL THEN DO:               
                        nytid = tidbuff.OANT1.
                        RUN TIMSEK.P.
                        otim1 = sekunder.
                        nytid = enketid.
                        RUN TIMSEK.P.
                        otim2 = sekunder.
                        overant = otim2 + otim1.                                                      
                        IF overant >  7200  THEN DO:
                           FIND FIRST TIDREGITAB WHERE RECID(TIDREGITAB) = reco EXCLUSIVE-LOCK NO-ERROR.
                           otim = overant - 7200.
                           sekunder = otim.
                           RUN SEKTIM.P.
                           otim4 = nytid.                  
                           IF otim4 GE enketid THEN DO:                                        
                              IF AVAILABLE okodbuff THEN DO:                  
                                 IF okoden = 1 THEN ASSIGN  TIDREGITAB.OKOD1 = okodbuff.OVERTIDTILL.
                                 IF okoden = 2 THEN ASSIGN  TIDREGITAB.OKOD2 = okodbuff.OVERTIDTILL.
                              END.
                           END.      
                           ELSE IF AVAILABLE okodbuff THEN DO:                     
                              nytid = enketid.
                              RUN TIMSEK.P.                    
                              sekunder = sekunder - otim.
                              seku = sekunder.
                              RUN SEKTIM.P.
                              IF okoden = 1 THEN DO:                                                         
                                 ASSIGN TIDREGITAB.OANT2 = nytid.                              
                                 IF TIDREGITAB.OSL2 = 0 THEN DO: 
                                    ASSIGN TIDREGITAB.OSL2 = TIDREGITAB.OSL1
                                    TIDREGITAB.OKOD2 = TIDREGITAB.OKOD1.
                                 END.
                                 nytid = TIDREGITAB.OSL2.
                                 RUN TIMSEK.P.
                                 sekunder = sekunder - seku.
                                 seku = sekunder.
                                 RUN SEKTIM.P.                              
                                 ASSIGN TIDREGITAB.OST2 = nytid
                                 TIDREGITAB.OSL1 =  nytid.
                                 nytid = TIDREGITAB.OST1.
                                 RUN TIMSEK.P.
                                 sekunder = seku - sekunder.
                                 RUN SEKTIM.P.
                                 ASSIGN TIDREGITAB.OANT1 = nytid
                                 TIDREGITAB.OKOD1 = okodbuff.OVERTIDTILL.      
                              END.
                              ELSE DO:                                    
                                 ASSIGN TIDREGITAB.OANT2 = nytid.
                                 nytid = TIDREGITAB.OSL2.
                                 RUN TIMSEK.P.
                                 sekunder = sekunder - seku.
                                 seku = sekunder.
                                 RUN SEKTIM.P.                              
                                 ASSIGN TIDREGITAB.OST2 = nytid
                                 TIDREGITAB.OSL1 =  nytid.
                                 nytid = TIDREGITAB.OST1.
                                 RUN TIMSEK.P.
                                 sekunder = seku - sekunder.
                                 RUN SEKTIM.P.
                                 ASSIGN TIDREGITAB.OANT1 = nytid
                                 TIDREGITAB.OKOD1 = okodbuff.OVERTIDTILL.                                    
                              END.
                           END.                        
                        END.
                     END.                           
                  END.                          
               END.
            END.
         END.
         ELSE IF TIDREGITAB.START < (lunchstarten + 2) AND TIDREGITAB.START GE lunchstarten AND TIDREGITAB.SLUT LE (lunchslutet - 2) THEN DO TRANSACTION:                   
            /*arbtid 0-6 22-24 övertid mellan 6-8*/
            /*FINNS DET ENKEL ÖVERTID FÖRE ARBETSTID*/                  
            overant = 0.            
            FIND FIRST tidbuff WHERE tidbuff.PERSONALKOD = PERSONALTAB.PERSONALKOD
            AND tidbuff.DATUM = bdatum AND tidbuff.SLUT LE lunchslutet AND 
            tidbuff.SLUT > ( lunchslutet -  2 ) NO-LOCK NO-ERROR.                            
            IF AVAILABLE tidbuff THEN DO:                           
               IF tidbuff.OKOD1 = OVERKOD.OVERTIDTILL OR tidbuff.OKOD2 = OVERKOD.OVERTIDTILL THEN DO:                        
                  IF tidbuff.OKOD1 = OVERKOD.OVERTIDTILL THEN DO: 
                     okoden = 1.
                     enketid = tidbuff.OANT1.
                  END.
                  IF tidbuff.OKOD2 = OVERKOD.OVERTIDTILL THEN DO: 
                     okoden = 2.
                     enketid = tidbuff.OANT2.         
                  END.
                  FIND FIRST okodbuff WHERE okodbuff.KOD = ANSTFORMTAB.KOD AND
                  okodbuff.ENKEL = "KVAL" AND okodbuff.OVERTIDUTTAG = tidbuff.OVERTIDUTTAG  NO-LOCK NO-ERROR.
                  recbuff = RECID(tidbuff).               
                   nytid = TIDREGITAB.OANT1.   /*TIDREGITAB.OANT1.*/
                   RUN TIMSEK.P.
                   otim1 = sekunder.
                   nytid = enketid.
                   RUN TIMSEK.P.
                   otim2 = sekunder.
                   overant = otim2 + otim1.                
                   overant = overant + omenkel.                
                   IF overant > 7200  THEN DO:
                      FIND FIRST tidbuff WHERE RECID(tidbuff) = recbuff EXCLUSIVE-LOCK NO-ERROR.
                      otim = overant - 7200.
                      sekunder = otim.
                      RUN SEKTIM.P.
                      otim4 = nytid.
                      IF otim4 GE enketid THEN DO:                                          
                         IF AVAILABLE okodbuff THEN DO:                  
                            IF okoden = 1 THEN ASSIGN  tidbuff.OKOD1 = okodbuff.OVERTIDTILL.
                            IF okoden = 2 THEN ASSIGN  tidbuff.OKOD2 = okodbuff.OVERTIDTILL.                         
                         END.
                      END.      
                      ELSE IF AVAILABLE okodbuff THEN DO:                      
                         nytid = enketid.
                        RUN TIMSEK.P.                    
                        sekunder = sekunder - otim.
                        seku = sekunder.
                        RUN SEKTIM.P.
                        IF okoden = 1 THEN DO:                           
                           
                           ASSIGN tidbuff.OANT2 = nytid.                              
                           IF tidbuff.OSL2 = 0 THEN DO: 
                              ASSIGN tidbuff.OSL2 = tidbuff.OSL1
                              tidbuff.OKOD2 = OVERKOD.OVERTIDTILL.
                           END.
                           nytid = tidbuff.OSL2.
                           RUN TIMSEK.P.
                           sekunder = sekunder - seku.
                           seku = sekunder.
                           RUN SEKTIM.P.                              
                           ASSIGN tidbuff.OST2 = nytid
                           tidbuff.OSL1 =  nytid.
                           nytid = tidbuff.OST1.
                           RUN TIMSEK.P.
                           sekunder = seku - sekunder.
                           RUN SEKTIM.P.
                           ASSIGN tidbuff.OANT1 = nytid
                           tidbuff.OKOD1 = okodbuff.OVERTIDTILL.      
                        END.
                        ELSE DO:                                                                      
                           ASSIGN tidbuff.OANT2 = nytid.
                           nytid = tidbuff.OSL2.
                           RUN TIMSEK.P.
                           sekunder = sekunder - seku.
                           seku = sekunder.
                           RUN SEKTIM.P.                              
                           ASSIGN tidbuff.OST2 = nytid
                           tidbuff.OSL1 =  nytid.
                           nytid = tidbuff.OST1.
                           RUN TIMSEK.P.
                           sekunder = seku - sekunder.
                           RUN SEKTIM.P.
                           ASSIGN tidbuff.OANT1 = nytid
                           tidbuff.OKOD1 = okodbuff.OVERTIDTILL.                              
                        END.                                            
                     END.                                   
                  END.               
               END.
               ELSE DO:
                  /*ex arbetstid 7.30-16.30 reg 5.20-7.3  ger 5.2-5.3 kval 5.3-.7.3 enkel 7.3-7.5 kval max 2tim enkel*/
                  IF TIDREGITAB.OKOD1 NE "" AND TIDREGITAB.OKOD2 NE "" AND TIDREGITAB.OKOD3 NE "" THEN DO:
                     FIND FIRST okodbuff WHERE okodbuff.KOD = ANSTFORMTAB.KOD AND
                     okodbuff.ENKEL = "KVAL" AND okodbuff.OVERTIDUTTAG = TIDREGITAB.OVERTIDUTTAG  NO-LOCK NO-ERROR.
                     IF TIDREGITAB.OANT2 = 2 AND TIDREGITAB.OKOD2 = TIDREGITAB.OKOD3 THEN DO:
                        IF TIDREGITAB.OKOD3 NE okodbuff.OVERTIDTILL THEN DO:
                           TIDREGITAB.OKOD3 = okodbuff.OVERTIDTILL.
                        END.
         
                     END.
                  END.
               END.
            END.
         END.      
      END.
      ELSE DO:
         FIND FIRST OVERAVTAB WHERE OVERAVTAB.DATUM = bdatum AND OVERAVTAB.KOD =  ANSTFORMTAB.KOD  USE-INDEX ODATUM NO-LOCK NO-ERROR.
         IF AVAILABLE OVERAVTAB THEN DO: 
            IF OVERAVTAB.DAGEQ = "KLA" THEN DO:             
            END.
            IF OVERAVTAB.DAGEQ = "HAL" THEN DO:               
            END.
         END.           
      END.
   END.      
END.

FIND FIRST TIDREGITAB WHERE RECID(TIDREGITAB) = reco NO-LOCK NO-ERROR.
/* TOLKNING JÄMNA HALVTIMMAR PÅ UPPDELADE på flera registreringar*/         
IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "GKAL" OR Guru.Konstanter:globforetag = "LULE" THEN DO:
   FIND FIRST TIDREGITAB WHERE RECID(TIDREGITAB) = reco NO-LOCK NO-ERROR.
   IF TIDREGITAB.START > regslut OR TIDREGITAB.START < regstart THEN DO:       
      /*Om det finns ytterligare en registrering före som nnu ej ska ha jämna halvtimmar*/
      FIND FIRST tidbuff WHERE tidbuff.PERSONALKOD = TIDREGITAB.PERSONALKOD AND
      tidbuff.DATUM = TIDREGITAB.DATUM AND tidbuff.SLUT = TIDREGITAB.START NO-LOCK NO-ERROR.
      IF AVAILABLE tidbuff THEN DO TRANSACTION:
         nytid = tidbuff.OANT1.
         RUN TIMSEK.P.
         seku = sekunder.
         nytid = tidbuff.OANT2.
         RUN TIMSEK.P.
         seku = seku + sekunder.
         nytid = tidbuff.OANT3.
         RUN TIMSEK.P.
         sekunder = seku + sekunder.
         RUN SEKTIM.P.         
         IF tidbuff.TOTALT < nytid THEN DO:
            FIND FIRST tidbuff WHERE tidbuff.PERSONALKOD = TIDREGITAB.PERSONALKOD AND
            tidbuff.DATUM = TIDREGITAB.DATUM AND tidbuff.SLUT = TIDREGITAB.START EXCLUSIVE-LOCK NO-ERROR.
            IF tidbuff.OANT3 = 0 AND tidbuff.OANT2 = 0 THEN DO:
               ASSIGN tidbuff.OSL1 = tidbuff.SLUT
               tidbuff.OANT1 = tidbuff.TOTALT.
            END.
            ELSE IF tidbuff.OANT3 = 0 THEN DO:  
               IF tidbuff.SLUT > tidbuff.OSL1 AND tidbuff.SLUT < tidbuff.OSL2 THEN DO:
                  ASSIGN tidbuff.OSL2 = tidbuff.SLUT
                  nytid = tidbuff.OSL2.
                  RUN TIMSEK.P.
                  ASSIGN
                  seku = sekunder
                  nytid = tidbuff.OST2.
                  RUN TIMSEK.P.
                  sekunder = seku - sekunder.
                  RUN SEKTIM.P.
                  ASSIGN tidbuff.OANT2 = nytid.
               END.
               ELSE IF tidbuff.SLUT > tidbuff.OST1 AND tidbuff.SLUT LE tidbuff.OSL1 AND tidbuff.OANT2 > 0 THEN DO:
                  ASSIGN tidbuff.OSL1 = tidbuff.SLUT
                  nytid = tidbuff.OSL1.
                  RUN TIMSEK.P.
                  ASSIGN
                  seku = sekunder
                  nytid = tidbuff.OST1.
                  RUN TIMSEK.P.
                  sekunder = seku - sekunder.
                  RUN SEKTIM.P.
                  ASSIGN tidbuff.OANT1 = nytid
                  tidbuff.OANT2 = 0 tidbuff.OST2 = 0 tidbuff.OSL2=0.                  
               END.   
            END.      
         END.       
      END.   
   END.
   IF TIDREGITAB.START GE regslut THEN DO:      
      ASSIGN
      ovstart = TIDREGITAB.START
      ovslut = TIDREGITAB.SLUT
      tothalv = 0
      ovhalv = 0.
      /*Om det finns ytterligare en registrering efter som skall tolkas ihop till jämna halvtimmar*/
      FIND FIRST tidbuff WHERE tidbuff.PERSONALKOD = TIDREGITAB.PERSONALKOD AND
      tidbuff.DATUM = TIDREGITAB.DATUM AND tidbuff.START = TIDREGITAB.SLUT NO-LOCK NO-ERROR.
      IF AVAILABLE tidbuff THEN DO TRANSACTION:
         nytid = tidbuff.OANT1.
         RUN TIMSEK.P.
         ovhalv = sekunder.
         nytid = tidbuff.OANT2.
         RUN TIMSEK.P.
         ovhalv = ovhalv + sekunder.
         nytid = tidbuff.OANT3.
         RUN TIMSEK.P.
         ovhalv = ovhalv + sekunder.          
         nytid = TIDREGITAB.OANT1.
         RUN TIMSEK.P.
         ASSIGN
         ovhalv = ovhalv + sekunder
         nytid = TIDREGITAB.OANT2.
         RUN TIMSEK.P.
         ASSIGN
         ovhalv = ovhalv + sekunder
         nytid = TIDREGITAB.OANT3.
         RUN TIMSEK.P.
         ovhalv = ovhalv + sekunder.
         nytid = tidbuff.TOTALT.
         RUN TIMSEK.P.
         tothalv = sekunder.
         nytid = TIDREGITAB.TOTALT.
         RUN TIMSEK.P.
         tothalv = tothalv + sekunder.
         ovslut = tidbuff.SLUT.
         REPEAT:
            FIND NEXT tidbuff WHERE tidbuff.PERSONALKOD = TIDREGITAB.PERSONALKOD AND
            tidbuff.DATUM = TIDREGITAB.DATUM AND tidbuff.START = ovslut NO-LOCK NO-ERROR.
            IF NOT AVAILABLE tidbuff THEN LEAVE.
            ovslut = tidbuff.SLUT.
            nytid = tidbuff.OANT1.
            RUN TIMSEK.P.
            ovhalv = ovhalv + sekunder.
            nytid = tidbuff.OANT2.
            RUN TIMSEK.P.
            ovhalv = ovhalv + sekunder.
            nytid = tidbuff.OANT3.
            RUN TIMSEK.P.
            ovhalv = ovhalv + sekunder.
            nytid = tidbuff.TOTALT.
            RUN TIMSEK.P.      
            tothalv = tothalv + sekunder.
         END.
         FIND FIRST tidbuff WHERE tidbuff.PERSONALKOD = TIDREGITAB.PERSONALKOD AND
         tidbuff.DATUM = TIDREGITAB.DATUM AND tidbuff.SLUT = TIDREGITAB.START AND 
         tidbuff.OVERTIDUTTAG NE "F" NO-LOCK NO-ERROR.
         IF AVAILABLE tidbuff  THEN DO:
            ovstart = tidbuff.START.
            nytid = tidbuff.OANT1.
            RUN TIMSEK.P.
            ovhalv = ovhalv + sekunder.
            nytid = tidbuff.OANT2.
            RUN TIMSEK.P.
            ovhalv = ovhalv + sekunder.
            nytid = tidbuff.OANT3.
            RUN TIMSEK.P.
            ovhalv = ovhalv + sekunder.
            nytid = tidbuff.TOTALT.
            RUN TIMSEK.P.      
            tothalv = tothalv + sekunder.
            REPEAT:
               FIND PREV tidbuff WHERE tidbuff.PERSONALKOD = TIDREGITAB.PERSONALKOD AND
               tidbuff.DATUM = TIDREGITAB.DATUM AND tidbuff.SLUT = ovstart AND 
               tidbuff.OVERTIDUTTAG NE "F" NO-LOCK NO-ERROR.
               IF NOT AVAILABLE tidbuff THEN LEAVE.
               ovstart = tidbuff.START.
               nytid = tidbuff.OANT1.
               RUN TIMSEK.P.
               ovhalv = ovhalv + sekunder.
               nytid = tidbuff.OANT2.
               RUN TIMSEK.P.
               ovhalv = ovhalv + sekunder.
               nytid = tidbuff.OANT3.
               RUN TIMSEK.P.
               ovhalv = ovhalv + sekunder.
               nytid = tidbuff.TOTALT.
               RUN TIMSEK.P.      
               tothalv = tothalv + sekunder.
            END.
         END.
         /* Om tolkningen på båda inte är jämna halvtimmar*/
         htim = TRUNCATE( ovhalv / 1800 ,0).
         sekunder =  ovhalv - ( htim * 1800 ).
         IF sekunder > 0 THEN DO:
            htim = TRUNCATE( tothalv / 1800 ,0).
            tothalv = 1800 - tothalv + ( htim * 1800 ).
            IF tothalv = 1800 THEN tothalv = 0.      
            FIND FIRST tidbuff WHERE tidbuff.PERSONALKOD = TIDREGITAB.PERSONALKOD AND
            tidbuff.DATUM = TIDREGITAB.DATUM AND tidbuff.SLUT = ovslut EXCLUSIVE-LOCK NO-ERROR.
            nytid = tidbuff.SLUT.
            RUN TIMSEK.P.
            seku = sekunder.
            nytid = tidbuff.START.
            RUN TIMSEK.P.
            sekunder = seku - sekunder + tothalv.
            RUN SEKTIM.P.
            IF tidbuff.OANT3 = 0 AND tidbuff.OANT2 = 0 THEN DO:               
               tidbuff.OANT1 = nytid.
               sekunder = seku + tothalv.
               RUN SEKTIM.P.
               tidbuff.OSL1 = nytid.
            END.
            ELSE IF tidbuff.OANT3 = 0 THEN DO:           
               sekunder = seku + tothalv.
               RUN SEKTIM.P.
               tidbuff.OSL2 = nytid.
               nytid = tidbuff.OSL2.
               RUN TIMSEK.P.
               seku = sekunder.
               nytid = tidbuff.OST2.
               RUN TIMSEK.P.
               sekunder = seku - sekunder.
               RUN SEKTIM.P.
               tidbuff.OANT2 = nytid.
            END.            
         END.         
      END.      
   END.
   FIND TIDREGITAB WHERE RECID(TIDREGITAB) = reco NO-LOCK NO-ERROR.
END.         
                       
IF Guru.Konstanter:globforetag = "gkal" THEN DO:
   /* Vid kompetensutveckling på 255 ska arbetsfridag ska det endast betala enkel övertid 201503/01 Mikaela Lundh*/
   IF regstart = regslut THEN DO:
      IF WEEKDAY(regdatum) = 1 OR WEEKDAY(regdatum) = 7 THEN.
      ELSE IF regdatum > 03/01/2015 THEN DO: 
         OPEN QUERY traktq
         FOR EACH TIDREGITAB WHERE TIDREGITAB.AONR = "255" AND TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
         TIDREGITAB.DATUM = bdatum AND TIDREGITAB.OKOD1 NE "" USE-INDEX PSTART NO-LOCK.   
         DO TRANSACTION:  
            GET FIRST traktq EXCLUSIVE-LOCK.   
            DO WHILE AVAILABLE(TIDREGITAB):      
               ASSIGN
               recko = RECID(TIDREGITAB)
               komp = TIDREGITAB.OVERTIDUTTAG.
               FIND FIRST OVERKOD WHERE OVERKOD.KOD = ANSTFORMTAB.KOD AND  OVERKOD.OVERTIDUTTAG = komp AND
               OVERKOD.OVERTIDTILL = TIDREGITAB.OKOD1 USE-INDEX OVER NO-LOCK NO-ERROR.
               IF OVERKOD.ENKE = "KVAL" THEN DO:
                  FIND FIRST OVERKOD WHERE OVERKOD.KOD = ANSTFORMTAB.KOD AND
                  OVERKOD.OVERTIDUTTAG = komp AND
                  OVERKOD.ENKE = "ENKE" AND OVERKOD.MULTIP > 0 USE-INDEX OVER NO-LOCK NO-ERROR.
                  IF AVAILABLE OVERKOD THEN DO:                        
                     ASSIGN TIDREGITAB.OKOD1 = OVERKOD.OVERTIDTILL.
                  END.                             
               END.
               GET NEXT traktq EXCLUSIVE-LOCK.   
            END.
            
         END.  
      END.
   END.          
            
END.                                                                
IF regstart = regslut AND ANSTFORMTAB.KOD = "KF" AND bdatum = avdatum 
THEN DO:     
   OPEN QUERY traktq   
   FOR EACH TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
   TIDREGITAB.DATUM = bdatum AND TIDREGITAB.OKOD1 NE "" 
   USE-INDEX PSTART NO-LOCK.   
   DO TRANSACTION:  
      GET FIRST traktq EXCLUSIVE-LOCK.   
      DO WHILE AVAILABLE(TIDREGITAB):      
         ASSIGN
         recko = RECID(TIDREGITAB)
         komp = TIDREGITAB.OVERTIDUTTAG.
         FIND FIRST OVERKOD WHERE OVERKOD.KOD = ANSTFORMTAB.KOD AND
         OVERKOD.OVERTIDUTTAG = komp AND
         OVERKOD.OVERTIDTILL = TIDREGITAB.OKOD1 
         USE-INDEX OVER NO-LOCK NO-ERROR.
         IF OVERKOD.ENKE = "ENKE" THEN DO:
            FIND FIRST OVERKOD WHERE OVERKOD.KOD = ANSTFORMTAB.KOD AND
 	         OVERKOD.OVERTIDUTTAG = komp AND
	         OVERKOD.ENKE = "KVAL" USE-INDEX OVER NO-LOCK NO-ERROR.
	         okod = OVERKOD.OVERTIDTILL.	    
            ASSIGN TIDREGITAB.OKOD1 = okod.         
         END. 
         IF TIDREGITAB.OKOD2 NE "" THEN DO:
            FIND FIRST OVERKOD WHERE OVERKOD.KOD = ANSTFORMTAB.KOD AND
            OVERKOD.OVERTIDUTTAG = komp AND
            OVERKOD.OVERTIDTILL = TIDREGITAB.OKOD2 
            USE-INDEX OVER NO-LOCK NO-ERROR.
            IF OVERKOD.ENKE = "ENKE" THEN DO:
               FIND FIRST OVERKOD WHERE OVERKOD.KOD = ANSTFORMTAB.KOD AND
	            OVERKOD.OVERTIDUTTAG = komp AND
	            OVERKOD.ENKE = "KVAL" USE-INDEX OVER NO-LOCK NO-ERROR.
	            okod = OVERKOD.OVERTIDTILL.	    
               ASSIGN TIDREGITAB.OKOD2 = okod.         
            END. 
         END.      
         IF TIDREGITAB.OKOD3 NE "" THEN DO:
            FIND FIRST OVERKOD WHERE OVERKOD.KOD = ANSTFORMTAB.KOD AND
            OVERKOD.OVERTIDUTTAG = komp AND
            OVERKOD.OVERTIDTILL = TIDREGITAB.OKOD3
            USE-INDEX OVER NO-LOCK NO-ERROR.
            IF OVERKOD.ENKE = "ENKE" THEN DO:
               FIND FIRST OVERKOD WHERE OVERKOD.KOD = ANSTFORMTAB.KOD AND
	            OVERKOD.OVERTIDUTTAG = komp AND
	            OVERKOD.ENKE = "KVAL" USE-INDEX OVER NO-LOCK NO-ERROR.
	            okod = OVERKOD.OVERTIDTILL.	    
               ASSIGN TIDREGITAB.OKOD3 = okod.         
            END. 
         END.   
         GET NEXT traktq EXCLUSIVE-LOCK.     
      END.  
      IF bdatum NE avdatum THEN DO:     
         GET FIRST traktq EXCLUSIVE-LOCK.   
         DO WHILE AVAILABLE(TIDREGITAB):  
	     ASSIGN
	     recko = RECID(TIDREGITAB)
	     komp = TIDREGITAB.OVERTIDUTTAG.
	     FIND FIRST OVERKOD WHERE OVERKOD.KOD = ANSTFORMTAB.KOD AND
	     OVERKOD.OVERTIDUTTAG = komp AND
	     OVERKOD.OVERTIDTILL = TIDREGITAB.OKOD1 
	     USE-INDEX OVER NO-LOCK NO-ERROR.
            IF OVERKOD.ENKE = "ENKE" THEN DO:
   	         FIND FIRST OVERKOD WHERE OVERKOD.KOD = ANSTFORMTAB.KOD AND
	            OVERKOD.OVERTIDUTTAG = komp AND
               OVERKOD.ENKE = "KVAL" USE-INDEX OVER NO-LOCK NO-ERROR.
               okod = OVERKOD.OVERTIDTILL.	    
               ASSIGN TIDREGITAB.OKOD1 = okod.	        
            END.
            IF TIDREGITAB.OKOD2 NE "" THEN DO:
               FIND FIRST OVERKOD WHERE OVERKOD.KOD = ANSTFORMTAB.KOD AND
               OVERKOD.OVERTIDUTTAG = komp AND
               OVERKOD.OVERTIDTILL = TIDREGITAB.OKOD2 
               USE-INDEX OVER NO-LOCK NO-ERROR.
               IF OVERKOD.ENKE = "ENKE" THEN DO:
                  FIND FIRST OVERKOD WHERE OVERKOD.KOD = ANSTFORMTAB.KOD AND
	               OVERKOD.OVERTIDUTTAG = komp AND
	               OVERKOD.ENKE = "KVAL" USE-INDEX OVER NO-LOCK NO-ERROR.
	               okod = OVERKOD.OVERTIDTILL.	    
                  ASSIGN TIDREGITAB.OKOD2 = okod.         
               END. 
            END.      
            IF TIDREGITAB.OKOD3 NE "" THEN DO:
               FIND FIRST OVERKOD WHERE OVERKOD.KOD = ANSTFORMTAB.KOD AND
               OVERKOD.OVERTIDUTTAG = komp AND
               OVERKOD.OVERTIDTILL = TIDREGITAB.OKOD3
               USE-INDEX OVER NO-LOCK NO-ERROR.
               IF OVERKOD.ENKE = "ENKE" THEN DO:
                  FIND FIRST OVERKOD WHERE OVERKOD.KOD = ANSTFORMTAB.KOD AND
	               OVERKOD.OVERTIDUTTAG = komp AND
	               OVERKOD.ENKE = "KVAL" USE-INDEX OVER NO-LOCK NO-ERROR.
	               okod = OVERKOD.OVERTIDTILL.	    
                  ASSIGN TIDREGITAB.OKOD3 = okod.         
               END. 
            END.   
         END.
      END.
   END.
   CLOSE QUERY traktq.   
END.
IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" THEN DO:   
   IF regstart = regslut THEN okod = okod.
   ELSE IF  bstart > regslut  AND bslut LE regslut + 2 THEN DO:
      FIND FIRST TIDREGITAB WHERE RECID(TIDREGITAB) = reco NO-LOCK NO-ERROR. 
      IF TIDREGITAB.OKOD2 NE "" AND TIDREGITAB.OKOD2 NE TIDREGITAB.OKOD1 AND TIDREGITAB.OSL2 > bslut THEN DO TRANSACTION:      
         FIND FIRST TIDREGITAB WHERE RECID(TIDREGITAB) = reco EXCLUSIVE-LOCK NO-ERROR. 
         ASSIGN TIDREGITAB.OKOD2 = TIDREGITAB.OKOD1.
      END.         
   END.
END.


