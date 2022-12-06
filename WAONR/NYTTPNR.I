   /*NYTTPNR.I*/
   DEFINE VARIABLE spaonummer AS CHARACTER NO-UNDO.
   DEFINE VARIABLE spdelnummer AS INTEGER NO-UNDO.
   
   FIND FIRST BYTAONR WHERE BYTAONR.AONR = aonummer
   AND BYTAONR.DELNR = delnummer  NO-LOCK NO-ERROR.      
   IF AVAILABLE BYTAONR THEN DO:
      ASSIGN
      spaonummer = aonummer.
      spdelnummer = delnummer.
      aonummer = BYTAONR.NAONR. 
      delnummer = BYTAONR.NDELNR.
   END.
   IF NOT AVAILABLE BYTAONR THEN DO:
      FIND FIRST BYTAONR WHERE BYTAONR.NAONR = aonummer
      AND BYTAONR.NDELNR = delnummer  NO-LOCK NO-ERROR.
      IF AVAILABLE BYTAONR THEN DO:
         ASSIGN
         spaonummer = aonummer
         spdelnummer = delnummer
         aonummer = BYTAONR.AONR 
         delnummer = BYTAONR.DELNR.
      END.
   END.
   IF AVAILABLE BYTAONR THEN DO:
      EMPTY TEMP-TABLE uppfoltemp NO-ERROR.             
      CREATE uppfoltemp.
      ASSIGN
      uppfoltemp.ANVANDARE = Guru.Konstanter:globanv
      uppfoltemp.FORETAG = Guru.Konstanter:globforetag  
      uppfoltemp.PERIOD = RAD_PERIOD      
      uppfoltemp.AONR = aonummer
      uppfoltemp.DELNR = delnummer   
      uppfoltemp.INDATUM = bdatum
      uppfoltemp.UTDATUM = avdatum.                                                  
      {AOUTFIN.I}      
      ASSIGN
      aonummer = spaonummer
      delnummer = spdelnummer.
   
   END.   
