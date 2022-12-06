/*STIDKAPP.P*/
&Scoped-define NEW NEW
{FAKTTEMP.I}

DEFINE INPUT PARAMETER fnr LIKE FAKTPLAN.FAKTNR NO-UNDO.
DEFINE INPUT PARAMETER fakkredrec AS RECID NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR sumtidtemp.
FIND FAKTKRED WHERE RECID(FAKTKRED) = fakkredrec NO-LOCK NO-ERROR.
IF FAKTKRED.SPARAD = FALSE THEN DO:
   OPEN QUERY stidq FOR EACH FAKTTID WHERE FAKTTID.FAKTNR = fnr AND
   FAKTTID.VFAKTNR = FAKTKRED.VFAKTNR AND  
   FAKTTID.MED = TRUE NO-LOCK.      
   GET FIRST stidq NO-LOCK.
   DO WHILE AVAILABLE(FAKTTID):
      CREATE sumtidtemp.        
      ASSIGN                                                  
      sumtidtemp.PERSONALKOD = FAKTTID.PERSONALKOD
      sumtidtemp.NAMN = FAKTTID.NAMN 
      sumtidtemp.AONR = FAKTTID.AONR
      sumtidtemp.DELNR = FAKTTID.DELNR
      sumtidtemp.TIMMAR = FAKTTID.TIMMAR
      sumtidtemp.BELOPP = FAKTTID.BELOPP        
      sumtidtemp.OBELOPP = FAKTTID.OBELOPP 
      sumtidtemp.TBELOPP = FAKTTID.TBELOPP             
      sumtidtemp.OTIMMAR = FAKTTID.OTIMMAR 
      sumtidtemp.LONKOST = FAKTTID.LONKOST                  
      sumtidtemp.BEFATTNING = FAKTTID.BEFATTNING      
      sumtidtemp.PERSMASK = FAKTTID.PERSMASK
      sumtidtemp.TRAKTKOD = FAKTTID.TRAKTKOD
      sumtidtemp.TRAKTANTAL = FAKTTID.TRAKTANTAL  
      sumtidtemp.LONTILLAGG = FAKTTID.LONTILLAGG      
      sumtidtemp.LONTILLANTAL = FAKTTID.LONTILLANTAL 
      sumtidtemp.PRISA = FAKTTID.PRISA 
      sumtidtemp.ENDAGS = FAKTTID.ENDAGS       
      sumtidtemp.MED = FAKTTID.MED      
      sumtidtemp.PRISTYP = FAKTTID.PRISTYP
      sumtidtemp.RESTIM = FAKTTID.DECRESTID
      sumtidtemp.RESPRIS = FAKTTID.RESPRIS
      sumtidtemp.OPRIS = FAKTTID.OPRIS
      sumtidtemp.RESKOSTDEC = FAKTTID.RESKOSTDEC
      sumtidtemp.OTEXTID = FAKTTID.OTEXTID
      sumtidtemp.DATUM = FAKTTID.DATUM
      sumtidtemp.START = FAKTTID.START 
      sumtidtemp.SLUT = FAKTTID.SLUT
      sumtidtemp.GSTART = FAKTTID.GSTART 
      sumtidtemp.GSLUT = FAKTTID.GSLUT
      sumtidtemp.LUNCH = FAKTTID.LUNCH
      sumtidtemp.OANT1 = FAKTTID.OANT1.
      GET NEXT stidq NO-LOCK.
   END.
   CLOSE QUERY stidq.
END.
ELSE DO:
   OPEN QUERY kstidq FOR EACH FAKTTIDKRED WHERE FAKTTIDKRED.FAKTNR = fnr AND 
   FAKTTIDKRED.FDELNR = FAKTKRED.FDELNR 
   NO-LOCK.      
   GET FIRST kstidq NO-LOCK.
   DO WHILE AVAILABLE(FAKTTIDKRED):
      CREATE sumtidtemp.        
      ASSIGN                                                  
      sumtidtemp.PERSONALKOD = FAKTTIDKRED.PERSONALKOD
      sumtidtemp.NAMN = FAKTTIDKRED.NAMN 
      sumtidtemp.AONR = FAKTTIDKRED.AONR
      sumtidtemp.DELNR = FAKTTIDKRED.DELNR
      sumtidtemp.TIMMAR = FAKTTIDKRED.TIMMAR
      sumtidtemp.BELOPP = FAKTTIDKRED.BELOPP        
      sumtidtemp.OBELOPP = FAKTTIDKRED.OBELOPP 
      sumtidtemp.TBELOPP = FAKTTIDKRED.TBELOPP             
      sumtidtemp.OTIMMAR = FAKTTIDKRED.OTIMMAR 
      sumtidtemp.LONKOST = FAKTTIDKRED.LONKOST                  
      sumtidtemp.BEFATTNING = FAKTTIDKRED.BEFATTNING      
      sumtidtemp.PERSMASK = FAKTTIDKRED.PERSMASK
      sumtidtemp.TRAKTKOD = FAKTTIDKRED.TRAKTKOD
      sumtidtemp.TRAKTANTAL = FAKTTIDKRED.TRAKTANTAL  
      sumtidtemp.LONTILLAGG = FAKTTIDKRED.LONTILLAGG      
      sumtidtemp.LONTILLANTAL = FAKTTIDKRED.LONTILLANTAL 
      sumtidtemp.PRISA = FAKTTIDKRED.PRISA 
      sumtidtemp.ENDAGS = FAKTTIDKRED.ENDAGS       
      sumtidtemp.MED = FAKTTIDKRED.MED      
      sumtidtemp.PRISTYP = FAKTTIDKRED.PRISTYP
      sumtidtemp.RESTIM = FAKTTIDKRED.DECRESTID
      sumtidtemp.RESPRIS = FAKTTIDKRED.RESPRIS
      sumtidtemp.OPRIS = FAKTTIDKRED.OPRIS
      sumtidtemp.RESKOSTDEC = FAKTTIDKRED.RESKOSTDEC
      sumtidtemp.OTEXTID = FAKTTIDKRED.OTEXTID
      sumtidtemp.DATUM = FAKTTIDKRED.DATUM
      sumtidtemp.START = FAKTTIDKRED.START 
      sumtidtemp.SLUT = FAKTTIDKRED.SLUT
      sumtidtemp.GSTART = FAKTTIDKRED.GSTART 
      sumtidtemp.GSLUT = FAKTTIDKRED.GSLUT
      sumtidtemp.LUNCH = FAKTTIDKRED.LUNCH
      sumtidtemp.OANT1 = FAKTTIDKRED.OANT1.
      GET NEXT kstidq NO-LOCK.
   END.
   CLOSE QUERY kstidq.    
END.
