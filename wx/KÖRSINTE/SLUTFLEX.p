/*SLUTFLEX.P*/
DEFINE NEW SHARED VARIABLE fnytid AS DECIMAL FORMAT "-99.99" NO-UNDO.
DEFINE SHARED VARIABLE globforetag LIKE FORETAG.FORETAG NO-UNDO.
DEFINE SHARED VARIABLE persrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE regstart LIKE TIDREGITAB.START NO-UNDO. 
DEFINE SHARED VARIABLE regslut LIKE TIDREGITAB.SLUT NO-UNDO.
DEFINE SHARED VARIABLE regvnr AS INTEGER FORMAT "999" NO-UNDO.
DEFINE SHARED VARIABLE regdagnamn AS CHARACTER FORMAT "X(3)" NO-UNDO.        
DEFINE SHARED VARIABLE regdatum AS DATE NO-UNDO.
DEFINE SHARED VARIABLE regtotalt LIKE TIDREGITAB.TOTALT NO-UNDO.
DEFINE SHARED VARIABLE frustarten AS DECIMAL NO-UNDO.
DEFINE SHARED VARIABLE fruslutet AS DECIMAL NO-UNDO.
DEFINE SHARED VARIABLE kaffestart AS DECIMAL NO-UNDO.
DEFINE SHARED VARIABLE kaffeslut AS DECIMAL NO-UNDO.
DEFINE SHARED VARIABLE lunchstarten AS DECIMAL NO-UNDO.
DEFINE SHARED VARIABLE lunchslutet AS DECIMAL NO-UNDO.
DEFINE SHARED VARIABLE nytid AS DECIMAL FORMAT "99.99" NO-UNDO.
DEFINE SHARED VARIABLE sekunder AS INTEGER NO-UNDO.    
DEFINE VARIABLE minlu AS INTEGER NO-UNDO.    
DEFINE VARIABLE lusta AS INTEGER NO-UNDO.
DEFINE VARIABLE luslu AS INTEGER NO-UNDO.
DEFINE VARIABLE dagnr AS INTEGER NO-UNDO. 
DEFINE VARIABLE vschemat LIKE VECKOARBAV.VECKOSCHEMA NO-UNDO.  
DEFINE VARIABLE total AS INTEGER NO-UNDO. 
DEFINE VARIABLE plus AS INTEGER NO-UNDO. 
DEFINE VARIABLE flrec3 AS RECID NO-UNDO. 
DEFINE VARIABLE fldrec AS RECID NO-UNDO. 
DEFINE BUFFER flexbuff FOR FLEXTID.
FUNCTION klock60 RETURNS DECIMAL
   ( INPUT ber100 AS DECIMAL ) :
   RETURN TRUNCATE(ber100,0) + ((ber100 - TRUNCATE(ber100,0)) * 60 / 100 ).   /* Function return value. */
END FUNCTION.
FUNCTION klock100 RETURNS DECIMAL
  ( INPUT ber60 AS DECIMAL ):
  RETURN  (TRUNCATE(ber60,0) * 3600 + (ber60 - TRUNCATE(ber60,0)) * 100 * 60) / 3600.

END FUNCTION.
FIND PERSONALTAB WHERE RECID(PERSONALTAB) = persrec NO-LOCK NO-ERROR.
FIND FIRST ANSTFORMTAB WHERE ANSTFORMTAB.ANSTALLNING = PERSONALTAB.ANSTALLNING
USE-INDEX ANSTF NO-LOCK NO-ERROR.   
FIND FIRST FLEXAVT WHERE FLEXAVT.PERSONALKOD = PERSONALTAB.PERSONALKOD 
USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.
IF NOT AVAILABLE FLEXAVT THEN RETURN.
FIND FIRST FLEXREG WHERE FLEXREG.KOD = FLEXAVT.FLEXKOD USE-INDEX FLEXREG NO-LOCK NO-ERROR.
IF NOT AVAILABLE FLEXREG THEN RETURN.
IF FLEXAVT.FLEXTID = TRUE AND regstart NE regslut THEN DO:
   IF globforetag = "SUFL" OR globforetag = "SUND" OR globforetag = "SNAT" OR globforetag = "MISV" THEN regstart = regstart. /*ingen lunchstämpling*/
   ELSE DO:
      FIND FIRST FLEXTID WHERE FLEXTID.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
      FLEXTID.DATUM = regdatum AND FLEXTID.KNAPP = "LUNCH IN" USE-INDEX FLEX NO-LOCK NO-ERROR.
      IF AVAILABLE FLEXTID THEN DO: 
         flrec3 = RECID(FLEXTID).
         FIND flexbuff WHERE RECID(flexbuff) = flrec3 NO-LOCK NO-ERROR.   
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
            FLEXDAG.PERSONALKOD = PERSONALTAB.PERSONALKOD
            FLEXDAG.KONTROLL = "Ejkontroll"
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
            /*nytid = lunchstarten. 
            RUN TIMSEK.P.
            ASSIGN lusta = sekunder
            nytid = lunchslutet.
            RUN TIMSEK.P.
            ASSIGN luslu = sekunder
            nytid = FLEXDAG.SLUT.     /*EJ ÖVERTID*/
            RUN TIMSEK.P.           
            ASSIGN
            total = sekunder    
            nytid = FLEXDAG.START.      /*EJ ÖVERTID*/
            RUN TIMSEK.P.   
            ASSIGN
            total = total - sekunder - luslu + lusta
            sekunder = total.
            RUN SEKTIM.P.*/ 
         
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
