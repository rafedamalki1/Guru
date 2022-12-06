/*FLLUNCHK.I*/
FIND FIRST FLEXTID WHERE FLEXTID.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
FLEXTID.DATUM = regdatum AND FLEXTID.KNAPP = "Lunch in" USE-INDEX FLEX NO-LOCK NO-ERROR.
IF AVAILABLE FLEXTID THEN DO : 
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
         ASSIGN luslu = lusta + sekunder
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
      IF AVAILABLE flexbuff AND flexbuff.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
      flexbuff.DATUM = regdatum AND flexbuff.KNAPP = "FLEX UT" THEN nytid = nytid.
      ELSE DO:
         IF FLEXREG.MEDLU = 0 THEN nytid = nytid.
         ELSE DO TRANSACTION:
            FIND FLEXDAG WHERE RECID(FLEXDAG) = fldrec EXCLUSIVE-LOCK NO-ERROR.
            ASSIGN FLEXDAG.FELMED = "Lunch ut saknas".                 
         END.   
      END.   
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
      FIND FIRST flexbuff WHERE flexbuff.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
      flexbuff.DATUM = regdatum AND flexbuff.KNAPP = "FLEX IN" USE-INDEX FLEX NO-LOCK NO-ERROR.
      IF NOT AVAILABLE flexbuff THEN DO:
         IF FLEXREG.MEDLU = 0 THEN nytid = nytid.
         ELSE DO TRANSACTION:
            FIND FLEXDAG WHERE RECID(FLEXDAG) = fldrec EXCLUSIVE-LOCK NO-ERROR.
            ASSIGN FLEXDAG.FELMED = "Lunch in saknas".     
         END.   
      END.   
   END.
   ELSE DO:  /*INGEN LUNCH STÄMPLING MEN FLEX UNDER LUNCH = LUNCH EFTER FLEX*/          
      FIND FIRST FLEXTID WHERE FLEXTID.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
      FLEXTID.DATUM = regdatum AND FLEXTID.KNAPP = "FLEX IN" AND
      FLEXTID.KOM = FALSE AND FLEXTID.TID GE FLEXREG.LUSTART AND
      FLEXTID.TID < FLEXREG.LUSLUT USE-INDEX FLEX NO-LOCK NO-ERROR.
      IF AVAILABLE FLEXTID THEN DO:
         ASSIGN lunchstarten = FLEXTID.TID.
         nytid = FLEXREG.MEDLU.
         RUN TIMSEK.P.
         ASSIGN minlu = sekunder
         nytid = lunchstarten.
         RUN TIMSEK.P.
         sekunder =  sekunder + minlu.
         RUN SEKTIM.P.
         lunchslutet = nytid.
      END.   
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
      IF NOT AVAILABLE flexbuff THEN DO:       
         IF NOT AVAILABLE FLEXTID THEN DO:  
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
               IF Guru.Konstanter:globforetag = "ELPA" AND PERSONALTAB.PERSONALKOD = "140" THEN nytid = nytid.
               ELSE DO:               
                  ASSIGN lunchstarten = FLEXREG.LUSTART
                  lunchslutet = lunchstarten.
               END.
            END.   
         END.   
      END.
      IF FLEXREG.MEDLU = 0 THEN nytid = nytid. /*ELPOOL BEHÖVER EJ STÄMPLA LUNCH*/        
      ELSE DO TRANSACTION:
         FIND FLEXDAG WHERE RECID(FLEXDAG) = fldrec EXCLUSIVE-LOCK NO-ERROR.
         ASSIGN FLEXDAG.FELMED = "Lunch ej registrerad".   
      END.   
   END.
END.                                                           
