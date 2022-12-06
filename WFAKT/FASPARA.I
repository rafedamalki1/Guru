/*FASPARA.I*/
FIND FIRST FAKTREGLER WHERE FAKTREGLER.FAKTNR = FAKTPLAN.FAKTNR 
USE-INDEX FAKTREGLER NO-LOCK NO-ERROR.
RUN spara_UI.
PROCEDURE spara_UI:
  IF (vartyp = 3 AND FILL-IN_SLUTFAKT = FALSE) OR vartyp = 1 OR vartyp = 2 OR vartyp = 52
   THEN DO:
      ASSIGN
      hamttid = FALSE
      hamtkost = FALSE.
   END.
   ELSE DO:
      ASSIGN
      hamttid = TRUE
      hamtkost = TRUE.
   END.   
   IF hamttid = TRUE THEN DO:
      OPEN QUERY faktidq FOR EACH FAKTTID WHERE FAKTTID.FAKTNR = FAKTPLAN.FAKTNR AND                  
      FAKTTID.FDELNR = FAKTURERAD.FDELNR AND FAKTTID.VFAKTNR = 0 NO-LOCK.
      DO TRANSACTION:
         GET FIRST faktidq EXCLUSIVE-LOCK.
         DO WHILE AVAILABLE(FAKTTID):
            DELETE FAKTTID.
            GET NEXT faktidq EXCLUSIVE-LOCK.
         END.
      END.
      FOR EACH sumtidtemp:            
         CREATE FAKTTID.         
         ASSIGN     
         FAKTTID.VFAKTNR = skarp         
         FAKTTID.FAKTNR = FAKTPLAN.FAKTNR            
         FAKTTID.FDELNR = FAKTURERAD.FDELNR 
         FAKTTID.SENASTTID = FAKTPLAN.SENASTTID 
         FAKTTID.SENASTFAK = FAKTPLAN.SENASTFAK
         SUBSTRING(FAKTTID.VECKOKORD,1) = sumtidtemp.VECKOKORD 
         SUBSTRING(FAKTTID.VECKOKORD,30) = sumtidtemp.FELKORD 
         FAKTTID.PERSONALKOD = sumtidtemp.PERSONALKOD
         FAKTTID.NAMN = sumtidtemp.NAMN 
         FAKTTID.AONR = sumtidtemp.AONR
         FAKTTID.DELNR = sumtidtemp.DELNR
         FAKTTID.TIMMAR = sumtidtemp.TIMMAR
         FAKTTID.BELOPP = sumtidtemp.BELOPP        
         FAKTTID.OBELOPP = sumtidtemp.OBELOPP 
         FAKTTID.TBELOPP = sumtidtemp.TBELOPP             
         FAKTTID.OTIMMAR = sumtidtemp.OTIMMAR 
         FAKTTID.RESPRIS = sumtidtemp.RESPRIS
         FAKTTID.OPRIS = sumtidtemp.OPRIS
         FAKTTID.LONKOST = sumtidtemp.LONKOST
         FAKTTID.MTRLKOST = 0
         FAKTTID.OVRKOST = 0
         FAKTTID.KOSTBELOPP = 0         
         FAKTTID.BEFATTNING = sumtidtemp.BEFATTNING 
         FAKTTID.PERSMASK = sumtidtemp.PERSMASK
         FAKTTID.TRAKTKOD = sumtidtemp.TRAKTKOD
         FAKTTID.TRAKTANTAL = sumtidtemp.TRAKTANTAL  
         FAKTTID.LONTILLAGG = sumtidtemp.LONTILLAGG      
         FAKTTID.LONTILLANTAL = sumtidtemp.LONTILLANTAL 
         FAKTTID.PRISA = sumtidtemp.PRISA 
         FAKTTID.ENDAGS = sumtidtemp.ENDAGS       
         FAKTTID.MED = sumtidtemp.MED      
         FAKTTID.PRISTYP = sumtidtemp.PRISTYP
         FAKTTID.DECRESTID = sumtidtemp.RESTIM
         FAKTTID.RESKOSTDEC = sumtidtemp.RESKOSTDEC
         FAKTTID.OTEXTID = sumtidtemp.OTEXTID
         FAKTTID.DATUM = sumtidtemp.DATUM
         FAKTTID.START = sumtidtemp.START 
         FAKTTID.SLUT = sumtidtemp.SLUT
         FAKTTID.GSTART = sumtidtemp.GSTART 
         FAKTTID.GSLUT = sumtidtemp.GSLUT
         FAKTTID.LUNCH = sumtidtemp.LUNCH          
         FAKTTID.OANT1 = sumtidtemp.OANT1.
         IF FAKTTID.MED = ? THEN DO:
            ASSIGN
            FAKTTID.SENASTFAK = ?
            FAKTTID.VFAKTNR = 0.
         END.
         IF FAKTTID.VFAKTNR = 0 THEN FAKTTID.SENASTFAK = ?.
      END.
   END.
   IF hamtkost = TRUE THEN DO:  
      OPEN QUERY fakkostq FOR EACH FAKTKOST WHERE 
      FAKTKOST.FAKTNR = FAKTPLAN.FAKTNR AND                  
      FAKTKOST.FDELNR = FAKTURERAD.FDELNR AND FAKTKOST.VFAKTNR = 0 NO-LOCK.
      DO TRANSACTION:
         GET FIRST fakkostq EXCLUSIVE-LOCK.
         DO WHILE AVAILABLE(FAKTKOST):            
            DELETE FAKTKOST.
            GET NEXT fakkostq EXCLUSIVE-LOCK.
         END.
      END.    
      FOR EACH kosttemp NO-LOCK:
         DO TRANSACTION:
            CREATE FAKTKOST.               
            ASSIGN         
            FAKTKOST.VERNR = kosttemp.FAKTNR   
            FAKTKOST.MED = kosttemp.MED
            FAKTKOST.VFAKTNR = skarp
            FAKTKOST.FAKTNR = FAKTPLAN.FAKTNR            
            FAKTKOST.FDELNR = FAKTURERAD.FDELNR 
            FAKTKOST.AONR = kosttemp.AONR
            FAKTKOST.DELNR = kosttemp.DELNR                 
            FAKTKOST.RADNR = kosttemp.RADNR
            FAKTKOST.MTRLPA = FAKTREGLER.MTRLPA
            FAKTKOST.FRTJPA = FAKTREGLER.FRTJPA             
            FAKTKOST.MASKKOST = kosttemp.MASKKOST 
            FAKTKOST.PERSKOST = kosttemp.PERSKOST
            FAKTKOST.TRAKTKOST = kosttemp.TRAKTKOST
            FAKTKOST.MTRL = kosttemp.MTRL
            FAKTKOST.OVRKR = kosttemp.OVRKR                                      
            FAKTKOST.BENAMNING = kosttemp.BENAMNING
            FAKTKOST.RADNR = kosttemp.RADNR
            FAKTKOST.MOMSEXTERNT = kosttemp.MOMSEXTERNT 
            FAKTKOST.MOMSID = kosttemp.MOMSID.           
            IF FAKTKOST.MED = ? THEN DO:
               FAKTKOST.VFAKTNR = 0.
            END.   
         END.         
      END.
   END.
END PROCEDURE.   
