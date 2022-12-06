/*SLUTARBW.P*/ 
&Scoped-define NEW NEW
{GLOBVAR2DEL1.I}
DEFINE INPUT-OUTPUT PARAMETER pkod AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER regstart AS DECIMAL NO-UNDO. 
DEFINE INPUT-OUTPUT PARAMETER regslut AS DECIMAL NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER regvnr AS INTEGER FORMAT "999" NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER regdagnamn AS CHARACTER FORMAT "X(3)" NO-UNDO.        
DEFINE INPUT-OUTPUT PARAMETER regdatum AS DATE NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER regtotalt AS DECIMAL NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER frustarten AS DECIMAL NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER fruslutet AS DECIMAL NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER kaffestart AS DECIMAL NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER kaffeslut AS DECIMAL NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER lunchstarten AS DECIMAL NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER lunchslutet AS DECIMAL NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER nytidI AS DECIMAL FORMAT "99.99" NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER sekunderI AS INTEGER NO-UNDO.    
DEFINE NEW SHARED VARIABLE fnytid AS DECIMAL FORMAT "-99.99" NO-UNDO.
DEFINE NEW SHARED VARIABLE nytid AS DECIMAL FORMAT "99.99" NO-UNDO.
DEFINE NEW SHARED VARIABLE sekunder AS INTEGER FORMAT "-9999999" NO-UNDO.
ASSIGN
nytid =  nytidI  
sekunder = sekunderI.
DEFINE VARIABLE dagnr AS INTEGER NO-UNDO. 
DEFINE VARIABLE vschemat AS INTEGER NO-UNDO.  
DEFINE VARIABLE arregvnr AS INTEGER NO-UNDO.
DEFINE VARIABLE minlu AS INTEGER NO-UNDO.    
DEFINE VARIABLE lusta AS INTEGER NO-UNDO.
DEFINE VARIABLE luslu AS INTEGER NO-UNDO.

DEFINE VARIABLE total AS INTEGER NO-UNDO. 
DEFINE VARIABLE plus AS INTEGER NO-UNDO. 
DEFINE VARIABLE flrec AS RECID NO-UNDO. 
DEFINE VARIABLE fldrec AS RECID NO-UNDO. 
DEFINE VARIABLE lunorm AS INTEGER NO-UNDO.
DEFINE VARIABLE seku AS INTEGER NO-UNDO.
DEFINE BUFFER flexbuff FOR FLEXTID.
FUNCTION klock60 RETURNS DECIMAL
   ( INPUT ber100 AS DECIMAL ) :
   RETURN TRUNCATE(ber100,0) + ((ber100 - TRUNCATE(ber100,0)) * 60 / 100 ).   /* Function return value. */
END FUNCTION.
FUNCTION klock100 RETURNS DECIMAL
  ( INPUT ber60 AS DECIMAL ):
  RETURN  (TRUNCATE(ber60,0) * 3600 + (ber60 - TRUNCATE(ber60,0)) * 100 * 60) / 3600.

END FUNCTION.
FIND FIRST FORETAG NO-LOCK NO-ERROR.
Guru.Konstanter:globforetag = FORETAG.FORETAG.
FIND PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = pkod NO-LOCK NO-ERROR.


{ARBETSTIDER.I}



IF Guru.Konstanter:globforetag = "ELPA" THEN DO:
   FIND PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = pkod NO-LOCK NO-ERROR.
   FIND FIRST ANSTFORMTAB WHERE ANSTFORMTAB.ANSTALLNING = PERSONALTAB.ANSTALLNING
   USE-INDEX ANSTF NO-LOCK NO-ERROR.   
   IF ANSTFORMTAB.KOD = "T" THEN RUN slutflex_UI.         
END. 
IF Guru.Konstanter:globforetag = "SUFL" OR Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" THEN RUN slutflex_UI.
IF Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "ELPA" THEN RUN lunchflex_UI.
ASSIGN
nytidI =  nytid
sekunderI = sekunder.

RELEASE FLEXDAG NO-ERROR.
RELEASE FLEXTID NO-ERROR.
RELEASE TIDREGITAB NO-ERROR. 
RELEASE FELTEXT NO-ERROR.


PROCEDURE lunchflex_UI:
   FIND FIRST FLEXAVT WHERE FLEXAVT.PERSONALKOD = PERSONALTAB.PERSONALKOD 
   USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.
   IF AVAILABLE FLEXAVT THEN DO:
      IF FLEXAVT.FLEXTID = TRUE THEN DO:
         nytid = lunchslutet.
         RUN TIMSEK.P.
         luslu = sekunder.
         nytid = lunchstarten.
         RUN TIMSEK.P.
         lusta = sekunder.
         sekunder = luslu - lusta.   
         lunorm = sekunder / 60.
         FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
         TIDREGITAB.DATUM = regdatum AND TIDREGITAB.TIDLOG = TRUE USE-INDEX PSTART NO-LOCK NO-ERROR.
         IF AVAILABLE TIDREGITAB THEN DO: 
            IF TIDREGITAB.LAGANTAL > 0 AND TIDREGITAB.LAGANTAL NE lunorm THEN DO:
               sekunder = lusta + (TIDREGITAB.LAGANTAL * 60).
               RUN SEKTIM.P.
               lunchslutet = nytid.     
            END.
         END.
      END.
   END.
END.
PROCEDURE slutflex_UI:
   FIND PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = pkod NO-LOCK NO-ERROR.
   FIND FIRST ANSTFORMTAB WHERE ANSTFORMTAB.ANSTALLNING = PERSONALTAB.ANSTALLNING
   USE-INDEX ANSTF NO-LOCK NO-ERROR.   
   FIND FIRST FLEXAVT WHERE FLEXAVT.PERSONALKOD = PERSONALTAB.PERSONALKOD 
   USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.
   IF NOT AVAILABLE FLEXAVT THEN RETURN.
   FIND FIRST FLEXREG WHERE FLEXREG.KOD = FLEXAVT.FLEXKOD USE-INDEX FLEXREG NO-LOCK NO-ERROR.
   IF NOT AVAILABLE FLEXREG THEN RETURN.
   IF FLEXAVT.FLEXTID = TRUE AND regstart NE regslut THEN DO:            
      IF Guru.Konstanter:globforetag = "SUFL" OR Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" THEN regstart = regstart. /*ingen lunchstämpling*/
      ELSE IF Guru.Konstanter:globforetag ="ELPA" AND PERSONALTAB.PERSONALKOD = "120" THEN regstart = regstart. /*ingen lunchstämpling*/
      ELSE DO:
         FIND FIRST FLEXTID WHERE FLEXTID.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
         FLEXTID.DATUM = regdatum AND FLEXTID.KNAPP = "LUNCH IN" USE-INDEX FLEX NO-LOCK NO-ERROR.
         IF AVAILABLE FLEXTID THEN DO: 
            flrec = RECID(FLEXTID).
            FIND flexbuff WHERE RECID(flexbuff) = flrec NO-LOCK NO-ERROR.   
            FIND PREV flexbuff USE-INDEX FLEX NO-LOCK NO-ERROR.      
            IF AVAILABLE flexbuff AND flexbuff.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
            flexbuff.DATUM = regdatum AND flexbuff.KNAPP = "LUNCH UT" THEN DO:
               ASSIGN lunchslutet = FLEXTID.TID
               lunchstarten = flexbuff.TID
               nytid = lunchslutet.
               RUN TIMSEK.P.
               ASSIGN luslu = sekunder
               nytid = lunchstarten.
               RUN TIMSEK.P.
               ASSIGN lusta = sekunder
               total = luslu - lusta
               nytid = FLEXREG.MINLU.
               RUN TIMSEK.P.
               IF sekunder > total THEN DO:
                  luslu = lusta + sekunder.
                  sekunder = luslu.
                  RUN SEKTIM.P.
                  lunchslutet = nytid.
               END.                 
            END.                                  
            ELSE DO:  
               lunchslutet = FLEXTID.TID.
               nytid = FLEXREG.MEDLU.
               RUN TIMSEK.P.
               ASSIGN minlu = sekunder
               nytid = lunchslutet.
               RUN TIMSEK.P.
               sekunder = sekunder - minlu.
               RUN SEKTIM.P.
               lunchstarten = nytid.
            END.            
         END.  
         ELSE DO:
            FIND FIRST flexbuff WHERE flexbuff.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
            flexbuff.DATUM = regdatum AND flexbuff.KNAPP = "LUNCH UT" USE-INDEX FLEX NO-LOCK NO-ERROR.
            IF AVAILABLE flexbuff THEN DO:
               lunchstarten = flexbuff.TID.
               nytid = FLEXREG.MEDLU.
               RUN TIMSEK.P.
               ASSIGN minlu = sekunder
               nytid = lunchstarten.
               RUN TIMSEK.P.
               sekunder = sekunder + minlu.
               RUN SEKTIM.P.
               lunchslutet = nytid.
            END.
            ELSE DO:  /*INGEN LUNCH STÄMPLING MEN FLEX UNDER LUNCH = LUNCH EFTER FLEX*/          
               FIND FIRST flexbuff WHERE flexbuff.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
               flexbuff.DATUM = regdatum AND flexbuff.KNAPP = "FLEX IN" AND
               flexbuff.KOM = FALSE AND flexbuff.TID GE FLEXREG.LUSTART AND
               flexbuff.TID < FLEXREG.LUSLUT USE-INDEX FLEX NO-LOCK NO-ERROR.
               IF AVAILABLE flexbuff THEN DO:
                  ASSIGN lunchstarten = flexbuff.TID.
                  nytid = FLEXREG.MEDLU.
                  RUN TIMSEK.P.
                  ASSIGN minlu = sekunder
                  nytid = lunchstarten.
                  RUN TIMSEK.P.
                  sekunder =  sekunder + minlu.
                  RUN SEKTIM.P.
                  lunchslutet = nytid.
               END.   
               IF NOT AVAILABLE flexbuff THEN DO:
                  FIND FIRST flexbuff WHERE flexbuff.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
                  flexbuff.DATUM = regdatum AND flexbuff.KNAPP = "FLEX UT" AND
                  flexbuff.GICK = FALSE AND flexbuff.TID GE FLEXREG.LUSTART AND
                  flexbuff.TID < FLEXREG.LUSLUT USE-INDEX FLEX NO-LOCK NO-ERROR.
                  IF AVAILABLE flexbuff THEN DO:
                     ASSIGN lunchslutet = flexbuff.TID.
                     nytid = FLEXREG.MEDLU.
                     RUN TIMSEK.P.
                     ASSIGN minlu = sekunder
                     nytid = lunchslutet.
                     RUN TIMSEK.P.
                     sekunder =  sekunder - minlu.
                     RUN SEKTIM.P.
                     lunchstarten = nytid.
                  END.   
               END.   
               IF NOT AVAILABLE flexbuff THEN DO:       
                  IF FLEXREG.AUTOLUNCH = TRUE THEN DO:
                     ASSIGN lunchstarten = FLEXREG.LUSTART
                     nytid = FLEXREG.MEDLU.
                     RUN TIMSEK.P.
                     ASSIGN minlu = sekunder
                     nytid = FLEXREG.LUSTART.
                     RUN TIMSEK.P.
                     sekunder = minlu + sekunder.
                     RUN SEKTIM.P.
                     lunchslutet = nytid.
                  END.   
                  ELSE DO:
                     ASSIGN lunchstarten = FLEXREG.LUSTART
                     lunchslutet = lunchstarten.
                  END.   
               END.   
            END.   
         END.                                 
      END.
             
      FIND FIRST FLEXDAG WHERE FLEXDAG.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
      FLEXDAG.DATUM = regdatum  USE-INDEX FLEX NO-LOCK NO-ERROR.
      IF NOT AVAILABLE FLEXDAG THEN DO:
         IF AVAILABLE FLEXREG THEN DO:               
            IF regdatum > FLEXREG.SALDOKORD THEN DO:               
               CREATE FLEXDAG.
               ASSIGN 
               FLEXDAG.KONTROLL = "Ejkontroll"
               FLEXDAG.PERSONALKOD = PERSONALTAB.PERSONALKOD
               FLEXDAG.DATUM = regdatum
               FLEXDAG.START = regstart
               FLEXDAG.SLUT = regslut.               
            END.
         END.
      END.
      IF AVAILABLE FLEXDAG THEN DO:      
         fldrec = RECID(FLEXDAG).  
         FIND FIRST flexbuff WHERE flexbuff.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
         flexbuff.DATUM = regdatum AND flexbuff.KOM = TRUE USE-INDEX FLEX NO-LOCK NO-ERROR.
         IF AVAILABLE flexbuff THEN DO:       
            IF flexbuff.KNAPP = "ÖVERTID IN" THEN DO:
               ASSIGN
               total = 0
               plus = 0
               nytid = flexbuff.TID.
               RUN TIMSEK.P.
               ASSIGN
               total = sekunder
               nytid = regstart.
               RUN TIMSEK.P.   
               ASSIGN
               total = sekunder - total
               sekunder = total.
               RUN SEKTIM.P.                        
               FIND FLEXDAG WHERE RECID(FLEXDAG) = fldrec EXCLUSIVE-LOCK NO-ERROR.
               ASSIGN FLEXDAG.OVINPLUS = nytid.  /*plustid före arbetstid  */
            END.
         END.                   
         FIND FIRST FLEXTID WHERE FLEXTID.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
         FLEXTID.DATUM = regdatum AND FLEXTID.GICK = TRUE USE-INDEX FLEX NO-LOCK NO-ERROR.
         IF AVAILABLE FLEXTID THEN DO: 
            ASSIGN  total = 0
            plus = 0.   
            FIND FIRST flexbuff WHERE flexbuff.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
            flexbuff.DATUM = regdatum AND flexbuff.KOM = TRUE USE-INDEX FLEX NO-LOCK NO-ERROR.
            IF AVAILABLE flexbuff THEN DO:                   
               /*om någon har frukost eller kafferast ska detta inte generera plusflex*/
               nytid = klock60(klock100(FLEXDAG.SLUT) - klock100(FLEXDAG.START)  - klock100(fruslutet)  + klock100(frustarten)
                - klock100(lunchslutet)  + klock100(lunchstarten)  - klock100(kaffeslut)  + klock100(kaffestart)).                     
               RUN TIMSEK.P.
               total = sekunder.            
               FIND FLEXDAG WHERE RECID(FLEXDAG) = fldrec EXCLUSIVE-LOCK NO-ERROR.
               ASSIGN FLEXDAG.TOTALT = nytid.
               nytid = regtotalt.
               RUN TIMSEK.P.
               ASSIGN
               plus = total - sekunder
               sekunder = plus.
               RUN FSEKTIM.P.                 
               IF FLEXTID.KNAPP = "ÖVERTID UT" THEN DO:
                  ASSIGN 
                  FLEXDAG.OVUTPLUS = fnytid
                  FLEXDAG.PLUS = 0.
               END.      
               ELSE  DO:
                  ASSIGN
                  FLEXDAG.PLUS = fnytid
                  FLEXDAG.OVUTPLUS = 0.
               END.   
            END.          
         END.                                
      END.
   END.

END PROCEDURE. 


