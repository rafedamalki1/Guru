/*fdagkollA.P*/ 
/*kontrollerar den dag och person du har uppe i flexand.p */
DEFINE INPUT PARAMETER pkod AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER indat AS DATE NO-UNDO.
&Scoped-define NEW NEW
{GLOBVAR2DEL1.I}
{REGVAR.I}
DEFINE NEW SHARED VARIABLE fnytid AS DECIMAL FORMAT "-99.99" NO-UNDO.


DEFINE NEW SHARED VARIABLE persrec AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE fldrec AS RECID NO-UNDO. 

DEFINE VARIABLE minlu AS INTEGER NO-UNDO.    
DEFINE VARIABLE lusta AS INTEGER NO-UNDO.
DEFINE VARIABLE luslu AS INTEGER NO-UNDO.
DEFINE VARIABLE total AS INTEGER NO-UNDO. 
DEFINE VARIABLE plus AS INTEGER NO-UNDO. 
DEFINE VARIABLE flexrec AS RECID NO-UNDO. 
DEFINE VARIABLE flexkvst LIKE TIDREGITAB.START NO-UNDO.     
DEFINE VARIABLE flexmosl LIKE TIDREGITAB.START NO-UNDO.     
DEFINE VARIABLE fltid AS INTEGER NO-UNDO. 
DEFINE VARIABLE flrec3 AS RECID NO-UNDO. 
DEFINE VARIABLE seku AS INTEGER NO-UNDO.
DEFINE VARIABLE autoin AS DECIMAL NO-UNDO.
DEFINE VARIABLE autout AS DECIMAL NO-UNDO.
DEFINE VARIABLE fnytidkoll AS DECIMAL NO-UNDO.
FUNCTION klock60 RETURNS DECIMAL
   ( INPUT ber100 AS DECIMAL ) :
   RETURN TRUNCATE(ber100,0) + ((ber100 - TRUNCATE(ber100,0)) * 60 / 100 ).   /* Function return value. */
END FUNCTION.
FUNCTION klock100 RETURNS DECIMAL
  ( INPUT ber60 AS DECIMAL ):
  RETURN  (TRUNCATE(ber60,0) * 3600 + (ber60 - TRUNCATE(ber60,0)) * 100 * 60) / 3600.

END FUNCTION.
regdatum = indat.
DEFINE BUFFER flexbuff FOR FLEXTID.         
DEFINE QUERY persfq FOR PERSONALTAB.   
FIND PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = pkod NO-LOCK NO-ERROR.
persrec =  RECID(PERSONALTAB).
FIND FIRST FORETAG NO-LOCK NO-ERROR.
IF AVAILABLE FORETAG THEN Guru.Konstanter:globforetag = FORETAG.FORETAG.
RUN REGVEC.P.
RUN SLFLARB.P.
FIND FIRST ANSTFORMTAB WHERE ANSTFORMTAB.ANSTALLNING = PERSONALTAB.ANSTALLNING
USE-INDEX ANSTF NO-LOCK NO-ERROR.   
FIND FIRST FLEXAVT WHERE FLEXAVT.PERSONALKOD = PERSONALTAB.PERSONALKOD 
USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.
IF NOT AVAILABLE FLEXAVT THEN regdatum = regdatum.
ELSE IF FLEXAVT.FLEXTID = TRUE AND regstart NE regslut THEN DO:       
   FIND FIRST FLEXREG WHERE FLEXREG.KOD = FLEXAVT.FLEXKOD USE-INDEX FLEXREG NO-LOCK NO-ERROR.
   IF AVAILABLE FLEXREG THEN DO:
      ASSIGN
      flexmosl = FLEXREG.MOSLUT
      flexkvst = FLEXREG.KVSTART.
      IF MONTH(regdatum) > MONTH(FLEXREG.SOMMARST) AND MONTH(regdatum) < MONTH(FLEXREG.SOMMARSL) THEN DO:
         flexkvst = FLEXREG.KVSOST. 
         IF FLEXAVT.FLEXKOD = "K" THEN regdatum = regdatum.               
         ELSE IF flexmosl = 9 THEN ASSIGN flexmosl = 8.30.                    
      END.
      ELSE IF MONTH(regdatum) = MONTH(FLEXREG.SOMMARSL) AND DAY(regdatum) <= DAY(FLEXREG.SOMMARSL) THEN DO:
         flexkvst = FLEXREG.KVSOST.           
         IF FLEXAVT.FLEXKOD = "K" THEN regdatum = regdatum.               
         ELSE IF flexmosl = 9 THEN ASSIGN flexmosl = 8.30.         
      END.
      ELSE IF MONTH(regdatum) = MONTH(FLEXREG.SOMMARST) AND DAY(regdatum) >= DAY(FLEXREG.SOMMARST) THEN DO:
         flexkvst = FLEXREG.KVSOST.           
         IF FLEXAVT.FLEXKOD = "K" THEN regdatum = regdatum.               
         ELSE IF flexmosl = 9 THEN ASSIGN flexmosl = 8.30.             
      END.    
      ELSE DO:
         IF FLEXAVT.FLEXKOD = "K" THEN regdatum = regdatum.
         ELSE  IF flexmosl = 8.30 THEN ASSIGN flexmosl = 9.00.                                 
      END.    
      IF PERSONALTAB.DELTID = TRUE THEN DO:
         FIND FIRST ORDARB WHERE ORDARB.ANSTALLNING = PERSONALTAB.ANSTALLNING NO-LOCK NO-ERROR.
         IF AVAILABLE ORDARB THEN DO:
            sekunder = ORDARB.STOPP1.
            RUN SEKTIM.P.
            IF nytid > regslut THEN DO:                               
               nytid = flexkvst.
               RUN TIMSEK.P.
               seku = sekunder.
               nytid = regslut.
               RUN TIMSEK.P.
               sekunder = seku - ORDARB.STOPP1 + sekunder.
               RUN SEKTIM.P.
               flexkvst = nytid.               
            END.

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
            END.                              

         END.        
      END.
      FIND FIRST OVERAVTAB WHERE OVERAVTAB.DATUM = regdatum AND 
      OVERAVTAB.KOD = ANSTFORMTAB.KOD USE-INDEX ODATUM NO-LOCK NO-ERROR.
      IF AVAILABLE OVERAVTAB AND OVERAVTAB.DAGEQ = "HAL"  THEN DO:
         flexkvst = regslut.
      END.    
   END. 
   /* När ska den automatiska in och utstämplingen göras "NATT" */
   ASSIGN
   autout = flexkvst
   autoin = flexmosl.
   FIND FIRST FLEXDAG WHERE FLEXDAG.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
   FLEXDAG.DATUM = regdatum  USE-INDEX FLEX NO-LOCK NO-ERROR.
   IF NOT AVAILABLE FLEXDAG THEN DO TRANSACTION:
      IF AVAILABLE FLEXREG THEN DO:      
         IF regdatum > FLEXREG.SALDOKORD THEN  DO:                  
            CREATE FLEXDAG.
            ASSIGN FLEXDAG.PERSONALKOD = PERSONALTAB.PERSONALKOD
            FLEXDAG.KONTROLL = "Ejkontroll"
            FLEXDAG.DATUM = regdatum
            FLEXDAG.START = regstart
            FLEXDAG.SLUT = regslut.            
         END.
      END.
   END.
   IF AVAILABLE FLEXDAG THEN DO:   
      DO TRANSACTION: 
         fldrec = RECID(FLEXDAG).
         
         FIND FLEXDAG WHERE RECID(FLEXDAG) = fldrec EXCLUSIVE-LOCK NO-ERROR.
         /* flexen ska alltid sättas till noll. Ibland har de gjort en periodreg som de bara ändrar slutet på Lena 20220207
         FIND FIRST FLEXTID WHERE FLEXTID.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
         FLEXTID.DATUM = regdatum AND FLEXTID.AUTO BEGINS "PER" USE-INDEX FLEX NO-LOCK NO-ERROR.
         IF AVAILABLE FLEXTID THEN regstart = regstart.
         ELSE DO:*/
            ASSIGN FLEXDAG.FLARB = 0
            FLEXDAG.FELMED = "". 
         /*END.   */
      END.  
   END.
   IF Guru.Konstanter:globforetag = "SUFL" OR Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" THEN regstart = regstart. /*ingen lunchstämpling*/
   ELSE DO:
      {FLLUNCHK.I}
      
   END. 
   FIND FIRST flexbuff WHERE flexbuff.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
   flexbuff.DATUM = regdatum AND flexbuff.KOM = TRUE USE-INDEX FLEX NO-LOCK NO-ERROR.
   IF AVAILABLE flexbuff THEN DO TRANSACTION:       
      FIND FLEXDAG WHERE RECID(FLEXDAG) = fldrec EXCLUSIVE-LOCK NO-ERROR.     
      IF AVAILABLE FLEXDAG THEN DO: 
         IF flexbuff.KNAPP = "ÖVERTID IN" THEN DO:
            total = 0.
            plus = 0.
            nytid = flexbuff.TID.
            RUN TIMSEK.P.
            total = sekunder.
            nytid = regstart.
            RUN TIMSEK.P.
            total = sekunder - total.
            sekunder = total.
            RUN FSEKTIM.P.                        
            FIND FLEXDAG WHERE RECID(FLEXDAG) = fldrec EXCLUSIVE-LOCK NO-ERROR.          
            ASSIGN FLEXDAG.OVINPLUS = fnytid.  /*plustid före arbetstid  */
         END.
         ELSE IF flexbuff.KNAPP = "FLEX IN" THEN DO:
            ASSIGN 
            nytid = regstart.
            RUN TIMSEK.P.
            regstartsek = sekunder.
            nytid = flexbuff.TID.
            RUN TIMSEK.P.
            regslutsek = sekunder. 
            RUN TOTTID.P.
            FIND FLEXDAG WHERE RECID(FLEXDAG) = fldrec EXCLUSIVE-LOCK NO-ERROR.
            ASSIGN FLEXDAG.FLARB = 0 - nytid.
         END. 
         ELSE DO:
            IF flexbuff.KNAPP = "IN" AND flexbuff.TID > flexmosl THEN DO:
               FIND FLEXDAG WHERE RECID(FLEXDAG) = fldrec EXCLUSIVE-LOCK NO-ERROR.
               ASSIGN FLEXDAG.FELMED = "Inreg efter tillåten flex".
            END.
         END.
      END.                         
   END.                  
   ELSE DO TRANSACTION:                             
      FIND FLEXDAG WHERE RECID(FLEXDAG) = fldrec EXCLUSIVE-LOCK NO-ERROR.
      IF AVAILABLE FLEXDAG THEN DO:
         IF FLEXDAG.FELMED NE "" THEN ASSIGN FLEXDAG.FELMED = "Flera reg saknas".
         ELSE ASSIGN FLEXDAG.FELMED = "Kom saknas".
      END.   
      /*automatisk in-stämpling  vid morgonflexgränsen om det finns en ut-stämpling*/
      IF Guru.Konstanter:globforetag = "elpa" THEN DO:               
         IF FLEXREG.AUTUT = TRUE THEN DO:
            FIND FIRST flexbuff WHERE flexbuff.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
            flexbuff.DATUM = regdatum AND flexbuff.GICK = TRUE USE-INDEX FLEX NO-LOCK NO-ERROR.
            IF AVAILABLE flexbuff THEN DO:             
               CREATE flexbuff.
               ASSIGN flexbuff.PERSONALKOD = PERSONALTAB.PERSONALKOD flexbuff.DATUM = regdatum
               flexbuff.KOM = TRUE flexbuff.GICK = FALSE flexbuff.KNAPP = "In" flexbuff.TID = autoin
               flexbuff.AUTO = "NATT".    
               IF FLEXDAG.FELMED = "Kom saknas" THEN ASSIGN FLEXDAG.FELMED = "".
               ASSIGN FLEXDAG.START = autoin.
               FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD
               AND TIDREGITAB.DATUM = regdatum AND TIDLOG = TRUE 
               AND TIDREGITAB.OVERTIDUTTAG = "F" AND TIDREGITAB.START = regstart
               AND TIDREGITAB.Slut GE autoin
               AND TIDREGITAB.GODKAND = "" EXCLUSIVE-LOCK NO-ERROR.
               IF AVAILABLE TIDREGITAB THEN DO:         
                  ASSIGN
                  TIDREGITAB.START = FLEXDAG.START.
                  RUN andtid_UI.               
               END. 
               DEBUGGER:SET-BREAK().  
               RUN dagflex_UI.
               
               ASSIGN FLEXDAG.TOTALT = fnytid.               
               nytid = regtotalt.
               RUN TIMSEK.P.
               ASSIGN
               plus = total - sekunder
               sekunder = plus.
               RUN FSEKTIM.P.              
               FLEXDAG.PLUS = fnytid.
            END.
         END.
      END.      
   END.   
   FIND FLEXDAG WHERE RECID(FLEXDAG) = fldrec NO-LOCK NO-ERROR.
   IF AVAILABLE FLEXDAG THEN RUN dagflex_UI.
   
   DO TRANSACTION:
      FIND FLEXDAG WHERE RECID(FLEXDAG) = fldrec EXCLUSIVE-LOCK NO-ERROR.
      IF AVAILABLE FLEXDAG THEN DO:
         ASSIGN FLEXDAG.TOTALT = fnytid.
      END.   
   END.   
   nytid = regtotalt.
   RUN TIMSEK.P.
   ASSIGN
   plus = total - sekunder
   sekunder = plus.
   RUN FSEKTIM.P.              
   FIND FIRST FLEXTID WHERE FLEXTID.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
   FLEXTID.DATUM = regdatum AND FLEXTID.GICK = TRUE USE-INDEX FLEX NO-LOCK NO-ERROR.
   IF AVAILABLE FLEXTID THEN DO TRANSACTION:             
      FIND FLEXDAG WHERE RECID(FLEXDAG) = fldrec EXCLUSIVE-LOCK NO-ERROR.
      IF AVAILABLE FLEXDAG THEN DO:
         IF FLEXTID.KNAPP = "ÖVERTID UT" THEN DO:
            FIND FLEXDAG WHERE RECID(FLEXDAG) = fldrec EXCLUSIVE-LOCK NO-ERROR.
            ASSIGN FLEXDAG.OVUTPLUS = fnytid.   
         END.   
         ELSE IF FLEXTID.KNAPP = "FLEX UT" THEN DO:
            nytid = FLEXTID.TID.
            RUN TIMSEK.P.
            regstartsek = sekunder.
            nytid = regslut.
            RUN TIMSEK.P.
            regslutsek = sekunder. 
            RUN TOTTID.P.
            IF FLEXDAG.FLARB NE 0 THEN DO:
               RUN TIMSEK.P.
               fltid = sekunder.
               nytid = FLEXDAG.FLARB.
               RUN TIMSEK.P.
               sekunder = sekunder - fltid.
               RUN FSEKTIM.P.
               ASSIGN FLEXDAG.FLARB = fnytid.
            END.   
            ELSE DO:
               ASSIGN FLEXDAG.FLARB = 0 - nytid.
            END.   
         END.   
         ELSE DO:
            FIND FLEXDAG WHERE RECID(FLEXDAG) = fldrec EXCLUSIVE-LOCK NO-ERROR.
            ASSIGN FLEXDAG.PLUS = fnytid.                
            IF FLEXTID.KNAPP = "UT" AND FLEXTID.TID  < flexkvst THEN DO:
               ASSIGN FLEXDAG.FELMED = "Utreg innan tillåten flex".
            END.                                 
         END.
      END.         
   END.
   ELSE DO TRANSACTION:                      
      FIND FLEXDAG WHERE RECID(FLEXDAG) = fldrec EXCLUSIVE-LOCK NO-ERROR.
      IF AVAILABLE FLEXDAG THEN DO:
         ASSIGN FLEXDAG.PLUS = fnytid.
         IF FLEXDAG.FELMED NE "" THEN ASSIGN FLEXDAG.FELMED = "Flera reg saknas".
         ELSE ASSIGN FLEXDAG.FELMED = "Gick saknas".     
         /*automatisk ut-stämpling  vid kvällsflexgränsen om det finns en in-stämpling*/
         IF Guru.Konstanter:globforetag = "elpa" THEN DO:               
            IF FLEXREG.AUTUT = TRUE THEN DO:
               FIND FIRST flexbuff WHERE flexbuff.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
               flexbuff.DATUM = regdatum AND flexbuff.KOM = TRUE USE-INDEX FLEX NO-LOCK NO-ERROR.
               IF AVAILABLE flexbuff THEN DO:             
                  CREATE flexbuff.
                  ASSIGN flexbuff.PERSONALKOD = PERSONALTAB.PERSONALKOD flexbuff.DATUM = regdatum
                  flexbuff.KOM = FALSE flexbuff.GICK = TRUE flexbuff.KNAPP = "Ut" flexbuff.TID = autout
                  flexbuff.AUTO = "NATT".    
                  IF FLEXDAG.FELMED = "Gick saknas" THEN ASSIGN FLEXDAG.FELMED = "".    
                  ASSIGN FLEXDAG.SLUT = autout.
                  FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD
                  AND TIDREGITAB.DATUM = regdatum AND TIDLOG = TRUE 
                  AND TIDREGITAB.OVERTIDUTTAG = "F" AND TIDREGITAB.SLUT = regslut
                  AND TIDREGITAB.START LE autout
                  AND TIDREGITAB.GODKAND = "" EXCLUSIVE-LOCK NO-ERROR.
                  IF AVAILABLE TIDREGITAB THEN DO:         
                     ASSIGN
                     TIDREGITAB.SLUT = FLEXDAG.SLUT.
                     RUN andtid_UI.               
                  END.   
                  RUN dagflex_UI.
                  
                  ASSIGN FLEXDAG.TOTALT = fnytid.               
                  nytid = regtotalt.
                  RUN TIMSEK.P.
                  ASSIGN
                  plus = total - sekunder
                  sekunder = plus.
                  RUN FSEKTIM.P.              
                  FLEXDAG.PLUS = fnytid.
               END.
            END.
         END.
      END.   
   END. 
   IF FLEXREG.FLEXARB = TRUE THEN DO TRANSACTION:                                        
      FIND FIRST FLEXTID WHERE FLEXTID.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
      FLEXTID.DATUM = regdatum AND FLEXTID.KNAPP = "FLEX UT" AND
      FLEXTID.GICK = FALSE AND FLEXTID.KOM = FALSE USE-INDEX FLEX NO-LOCK NO-ERROR.
      IF AVAILABLE FLEXTID THEN DO:  
         FIND FIRST flexbuff WHERE flexbuff.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
         flexbuff.DATUM = regdatum AND flexbuff.KNAPP = "FLEX IN" AND
         flexbuff.GICK = FALSE AND flexbuff.KOM = FALSE USE-INDEX FLEX NO-LOCK NO-ERROR.
         IF NOT AVAILABLE flexbuff THEN DO:
            FIND FLEXDAG WHERE RECID(FLEXDAG) = fldrec EXCLUSIVE-LOCK NO-ERROR.       
            IF AVAILABLE FLEXDAG THEN DO:    
               IF FLEXDAG.FELMED NE "" THEN ASSIGN FLEXDAG.FELMED = "Flera reg saknas".
               ELSE ASSIGN FLEXDAG.FELMED = "Flex in saknas".
            END.     
         END.   
         ELSE DO:
            nytid = FLEXTID.TID.
            RUN TIMSEK.P.
            regstartsek = sekunder.
            nytid = flexbuff.TID.
            RUN TIMSEK.P.
            regslutsek = sekunder. 
            RUN TOTTID.P.
            FIND FLEXDAG WHERE RECID(FLEXDAG) = fldrec EXCLUSIVE-LOCK NO-ERROR.
            IF AVAILABLE FLEXDAG THEN DO:           
               IF FLEXDAG.FLARB NE 0 THEN DO:
                  RUN TIMSEK.P.
                  fltid = sekunder.
                  nytid = FLEXDAG.FLARB.
                  RUN TIMSEK.P.
                  sekunder = sekunder - fltid.
                  RUN FSEKTIM.P.
                  ASSIGN FLEXDAG.FLARB = fnytid.
               END.   
               ELSE DO:
                  ASSIGN FLEXDAG.FLARB = 0 - nytid.
               END.
            END.   
            flarb:
            REPEAT:  
               FIND NEXT FLEXTID WHERE FLEXTID.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
               FLEXTID.DATUM = regdatum AND FLEXTID.KNAPP = "FLEX UT" AND
               FLEXTID.GICK = FALSE AND FLEXTID.KOM = FALSE USE-INDEX FLEX NO-LOCK NO-ERROR.
               IF AVAILABLE FLEXTID THEN DO:                    
                  FIND NEXT flexbuff WHERE flexbuff.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
                  flexbuff.DATUM = regdatum AND flexbuff.KNAPP = "FLEX IN" AND
                  flexbuff.GICK = FALSE AND flexbuff.KOM = FALSE USE-INDEX FLEX NO-LOCK NO-ERROR.
                  IF NOT AVAILABLE flexbuff THEN DO:
                     FIND FLEXDAG WHERE RECID(FLEXDAG) = fldrec EXCLUSIVE-LOCK NO-ERROR.    
                     IF AVAILABLE FLEXDAG THEN DO:       
                        IF FLEXDAG.FELMED NE "" THEN ASSIGN FLEXDAG.FELMED = "Flera reg saknas".
                        ELSE ASSIGN FLEXDAG.FELMED = "Flex in saknas".
                     END.     
                     LEAVE flarb.
                  END.   
                  ELSE DO:
                     nytid = FLEXTID.TID.
                     RUN TIMSEK.P.
                     regstartsek = sekunder.
                     nytid = flexbuff.TID.
                     RUN TIMSEK.P.
                     regslutsek = sekunder. 
                     RUN TOTTID.P.
                     FIND FLEXDAG WHERE RECID(FLEXDAG) = fldrec EXCLUSIVE-LOCK NO-ERROR.    
                     IF AVAILABLE FLEXDAG THEN DO:       
                        IF FLEXDAG.FLARB NE 0 THEN DO:
                           RUN TIMSEK.P.
                           fltid = sekunder.
                           nytid = FLEXDAG.FLARB.
                           RUN TIMSEK.P.
                           sekunder = sekunder - fltid.
                           RUN FSEKTIM.P.
                           ASSIGN FLEXDAG.FLARB = fnytid.
                        END.   
                        ELSE DO:
                           ASSIGN FLEXDAG.FLARB = 0 - nytid.
                        END.
                     END.     
                  END.
               END.   
               ELSE DO:
                  FIND NEXT flexbuff WHERE flexbuff.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
                  flexbuff.DATUM = regdatum AND flexbuff.KNAPP = "FLEX IN" AND
                  flexbuff.GICK = FALSE AND flexbuff.KOM = FALSE USE-INDEX FLEX NO-LOCK NO-ERROR.
                  IF AVAILABLE flexbuff THEN DO:
                     FIND FLEXDAG WHERE RECID(FLEXDAG) = fldrec EXCLUSIVE-LOCK NO-ERROR.    
                     IF AVAILABLE FLEXDAG THEN DO:       
                        IF FLEXDAG.FELMED NE "" THEN ASSIGN FLEXDAG.FELMED = "Flera reg saknas".
                        ELSE ASSIGN FLEXDAG.FELMED = "Flex ut saknas".
                     END.     
                  END.   
                  LEAVE flarb.                       
               END.         
            END. /*repeat*/  
         END.   
      END.   
      ELSE DO:
         FIND FIRST flexbuff WHERE flexbuff.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
         flexbuff.DATUM = regdatum AND flexbuff.KNAPP = "FLEX IN" AND
         flexbuff.GICK = FALSE AND flexbuff.KOM = FALSE USE-INDEX FLEX NO-LOCK NO-ERROR.
         IF AVAILABLE flexbuff THEN DO:
            FIND FLEXDAG WHERE RECID(FLEXDAG) = fldrec EXCLUSIVE-LOCK NO-ERROR.
            IF AVAILABLE FLEXDAG THEN DO:           
               IF FLEXDAG.FELMED NE "" THEN ASSIGN FLEXDAG.FELMED = "Flera reg saknas".
               ELSE ASSIGN FLEXDAG.FELMED = "Flex ut saknas".
            END.     
         END.   
      END.
   END.              
   DO TRANSACTION:
      FIND FLEXDAG WHERE RECID(FLEXDAG) = fldrec EXCLUSIVE-LOCK NO-ERROR.
      IF AVAILABLE FLEXDAG THEN DO:
         IF FLEXDAG.FELMED = "" THEN ASSIGN FLEXDAG.FELOK = TRUE.
         ELSE IF FLEXDAG.KONTROLL = "KONTROLL" AND FLEXDAG.FELOK = TRUE AND
         FLEXDAG.FELMED NE "" THEN nytid = nytid.
         ELSE ASSIGN FLEXDAG.FELOK = FALSE.
         ASSIGN FLEXDAG.KONTROLL = "Kontroll".
         FIND FIRST flexbuff WHERE flexbuff.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
         flexbuff.DATUM = regdatum  USE-INDEX FLEX NO-LOCK NO-ERROR.
         IF NOT AVAILABLE flexbuff THEN DO:
            ASSIGN FLEXDAG.KONTROLL = "Kontroll"
            FLEXDAG.FELMED = "Ingen registrering gjord"
            FLEXDAG.FELOK = FALSE. 
         END.
      END.      
   END.   
END.    
ELSE IF FLEXAVT.FLEXTID = TRUE AND PERSONALTAB.DELTID = TRUE AND regstart = regslut THEN DO:
   DO TRANSACTION:   
      FIND FIRST FLEXDAG WHERE FLEXDAG.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
      FLEXDAG.DATUM = regdatum  USE-INDEX FLEX EXCLUSIVE-LOCK NO-ERROR.
      IF AVAILABLE FLEXDAG THEN DO:      
         ASSIGN FLEXDAG.KONTROLL = "Kontroll"
         FLEXDAG.FELMED = "Deltid har ej arbetstid idag"
         FLEXDAG.FELOK = FALSE.       
      END.   
   END.
END.
/*IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph NO-ERROR.
edataapph = ?.*/
 
PROCEDURE dagflex_UI :
   ASSIGN  total = 0
   plus = 0.        
   /*om någon har frukost eller kafferast ska detta inte generera plusflex*/
   
   fnytid = klock60(klock100(FLEXDAG.SLUT) - klock100(FLEXDAG.START)  - klock100(fruslutet)  + klock100(frustarten)
    - klock100(lunchslutet)  + klock100(lunchstarten)  - klock100(kaffeslut)  + klock100(kaffestart)).
   
   nytid = fnytid.      
   RUN TIMSEK.P.
   total = sekunder.   
END PROCEDURE.

PROCEDURE andtid_UI :
   ASSIGN
   TIDREGITAB.PROGRAM = 'FDAGKOLLA.P' + STRING(TODAY) + STRING(TIME,"HH:MM")  
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
