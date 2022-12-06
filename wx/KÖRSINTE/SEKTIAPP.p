/*SEKTIAPP.P körs inte*/
DEFINE TEMP-TABLE sektemp 
   FIELD tadmSEK AS LOGICAL EXTENT 50 
   FIELD tidaSEK AS LOGICAL EXTENT 50 
   FIELD tidbSEK AS LOGICAL EXTENT 50 
   FIELD tidoSEK AS LOGICAL EXTENT 50 
   FIELD tidrSEK AS LOGICAL EXTENT 50 
   FIELD tidSEK AS LOGICAL EXTENT 50 
   FIELD tidsSEK AS LOGICAL EXTENT 50 
   FIELD tidtSEK AS LOGICAL EXTENT 50 
   FIELD tadmLOG AS LOGICAL 
   FIELD tidaLOG AS LOGICAL 
   FIELD tidbLOG AS LOGICAL 
   FIELD tidoLOG AS LOGICAL 
   FIELD tidrLOG AS LOGICAL 
   FIELD tidLOG AS LOGICAL 
   FIELD tidsLOG AS LOGICAL 
   FIELD tidtLOG AS LOGICAL. 
DEFINE INPUT PARAMETER globniv LIKE ANVANDARE.AV-LEVEL NO-UNDO.
DEFINE OUTPUT PARAMETER TABLE FOR sektemp. 
CREATE sektemp.
RUN nextguru_UI.
PROCEDURE nextguru_UI:
   FIND FIRST XSEK WHERE XSEK.MENYVART = "TADM" AND
   XSEK.AV-LEVEL = Guru.Konstanter:globniv USE-INDEX XSEK NO-LOCK NO-ERROR. 
   IF AVAILABLE XSEK THEN DO:
      ASSIGN 
      sektemp.tadmLOG = TRUE
      sektemp.tadmSEK[1] = XSEK.SEK[1]
      sektemp.tadmSEK[2] = XSEK.SEK[2]
      sektemp.tadmSEK[3] = XSEK.SEK[3]
      sektemp.tadmSEK[4] = XSEK.SEK[4]
      sektemp.tadmSEK[5] = XSEK.SEK[5]
      sektemp.tadmSEK[6] = XSEK.SEK[6]
      sektemp.tadmSEK[7] = XSEK.SEK[7]
      sektemp.tadmSEK[8] = XSEK.SEK[8]
      sektemp.tadmSEK[9] = XSEK.SEK[9]
      sektemp.tadmSEK[10] = XSEK.SEK[10]
      sektemp.tadmSEK[11] = XSEK.SEK[11]
      sektemp.tadmSEK[12] = XSEK.SEK[12]
      sektemp.tadmSEK[13] = XSEK.SEK[13].
   END.
   FIND FIRST XSEK WHERE XSEK.MENYVART = "TID" AND
   XSEK.AV-LEVEL = Guru.Konstanter:globniv USE-INDEX XSEK NO-LOCK NO-ERROR.  
   IF AVAILABLE XSEK THEN DO:
      ASSIGN 
      sektemp.tidLOG = TRUE
      sektemp.tidSEK[1] = XSEK.SEK[1]
      sektemp.tidSEK[2] = XSEK.SEK[2]
      sektemp.tidSEK[3] = XSEK.SEK[3]
      sektemp.tidSEK[4] = XSEK.SEK[4]
      sektemp.tidSEK[5] = XSEK.SEK[5]
      sektemp.tidSEK[6] = XSEK.SEK[6]
      sektemp.tidSEK[7] = XSEK.SEK[7]
      sektemp.tidSEK[8] = XSEK.SEK[8]
      sektemp.tidSEK[9] = XSEK.SEK[9]
      sektemp.tidSEK[10] = XSEK.SEK[10]
      sektemp.tidSEK[11] = XSEK.SEK[11]
      sektemp.tidSEK[12] = XSEK.SEK[12]
      sektemp.tidSEK[13] = XSEK.SEK[13].
   END.
   FIND FIRST XSEK WHERE XSEK.MENYVART = "TIDA" AND
   XSEK.AV-LEVEL = Guru.Konstanter:globniv USE-INDEX XSEK NO-LOCK NO-ERROR. 
   IF AVAILABLE XSEK THEN DO:
      ASSIGN 
      sektemp.tidaLOG = TRUE
      sektemp.tidaSEK[1] = XSEK.SEK[1]
      sektemp.tidaSEK[2] = XSEK.SEK[2]
      sektemp.tidaSEK[3] = XSEK.SEK[3]
      sektemp.tidaSEK[4] = XSEK.SEK[4]
      sektemp.tidaSEK[5] = XSEK.SEK[5]
      sektemp.tidaSEK[6] = XSEK.SEK[6]
      sektemp.tidaSEK[7] = XSEK.SEK[7]
      sektemp.tidaSEK[8] = XSEK.SEK[8]
      sektemp.tidaSEK[9] = XSEK.SEK[9]
      sektemp.tidaSEK[10] = XSEK.SEK[10]
      sektemp.tidaSEK[11] = XSEK.SEK[11]
      sektemp.tidaSEK[12] = XSEK.SEK[12]
      sektemp.tidaSEK[13] = XSEK.SEK[13].
   END.
   FIND FIRST XSEK WHERE XSEK.MENYVART = "TIDB" AND
   XSEK.AV-LEVEL = Guru.Konstanter:globniv USE-INDEX XSEK NO-LOCK NO-ERROR.  
   IF AVAILABLE XSEK THEN DO:
      ASSIGN 
      sektemp.tidbLOG = TRUE
      sektemp.tidbSEK[1] = XSEK.SEK[1]
      sektemp.tidbSEK[2] = XSEK.SEK[2]
      sektemp.tidbSEK[3] = XSEK.SEK[3]
      sektemp.tidbSEK[4] = XSEK.SEK[4]
      sektemp.tidbSEK[5] = XSEK.SEK[5]
      sektemp.tidbSEK[6] = XSEK.SEK[6]
      sektemp.tidbSEK[7] = XSEK.SEK[7]
      sektemp.tidbSEK[8] = XSEK.SEK[8]
      sektemp.tidbSEK[9] = XSEK.SEK[9]
      sektemp.tidbSEK[10] = XSEK.SEK[10]
      sektemp.tidbSEK[11] = XSEK.SEK[11]
      sektemp.tidbSEK[12] = XSEK.SEK[12]
      sektemp.tidbSEK[13] = XSEK.SEK[13].
   END.
   FIND FIRST XSEK WHERE XSEK.MENYVART = "TIDO" AND
   XSEK.AV-LEVEL = Guru.Konstanter:globniv USE-INDEX XSEK NO-LOCK NO-ERROR.  
   IF AVAILABLE XSEK THEN DO:
      ASSIGN 
      sektemp.tidoLOG = TRUE
      sektemp.tidoSEK[1] = XSEK.SEK[1]
      sektemp.tidoSEK[2] = XSEK.SEK[2]
      sektemp.tidoSEK[3] = XSEK.SEK[3]
      sektemp.tidoSEK[4] = XSEK.SEK[4]
      sektemp.tidoSEK[5] = XSEK.SEK[5]
      sektemp.tidoSEK[6] = XSEK.SEK[6]
      sektemp.tidoSEK[7] = XSEK.SEK[7]
      sektemp.tidoSEK[8] = XSEK.SEK[8]
      sektemp.tidoSEK[9] = XSEK.SEK[9]
      sektemp.tidoSEK[10] = XSEK.SEK[10]
      sektemp.tidoSEK[11] = XSEK.SEK[11]
      sektemp.tidoSEK[12] = XSEK.SEK[12]
      sektemp.tidoSEK[13] = XSEK.SEK[13].
   END.
   FIND FIRST XSEK WHERE XSEK.MENYVART = "TIDR" AND
   XSEK.AV-LEVEL = Guru.Konstanter:globniv USE-INDEX XSEK NO-LOCK NO-ERROR.  
   IF AVAILABLE XSEK THEN DO:
      ASSIGN 
      sektemp.tidrLOG = TRUE
      sektemp.tidrSEK[1] = XSEK.SEK[1]
      sektemp.tidrSEK[2] = XSEK.SEK[2]
      sektemp.tidrSEK[3] = XSEK.SEK[3]
      sektemp.tidrSEK[4] = XSEK.SEK[4]
      sektemp.tidrSEK[5] = XSEK.SEK[5]
      sektemp.tidrSEK[6] = XSEK.SEK[6]
      sektemp.tidrSEK[7] = XSEK.SEK[7]
      sektemp.tidrSEK[8] = XSEK.SEK[8]
      sektemp.tidrSEK[9] = XSEK.SEK[9]
      sektemp.tidrSEK[10] = XSEK.SEK[10]
      sektemp.tidrSEK[11] = XSEK.SEK[11]
      sektemp.tidrSEK[12] = XSEK.SEK[12]
      sektemp.tidrSEK[13] = XSEK.SEK[13].
   END.
   FIND FIRST XSEK WHERE XSEK.MENYVART = "TIDS" AND
   XSEK.AV-LEVEL = Guru.Konstanter:globniv USE-INDEX XSEK NO-LOCK NO-ERROR.  
   IF AVAILABLE XSEK THEN DO:
      ASSIGN 
      sektemp.tidsLOG = TRUE
      sektemp.tidsSEK[1] = XSEK.SEK[1]
      sektemp.tidsSEK[2] = XSEK.SEK[2]
      sektemp.tidsSEK[3] = XSEK.SEK[3]
      sektemp.tidsSEK[4] = XSEK.SEK[4]
      sektemp.tidsSEK[5] = XSEK.SEK[5]
      sektemp.tidsSEK[6] = XSEK.SEK[6]
      sektemp.tidsSEK[7] = XSEK.SEK[7]
      sektemp.tidsSEK[8] = XSEK.SEK[8]
      sektemp.tidsSEK[9] = XSEK.SEK[9]
      sektemp.tidsSEK[10] = XSEK.SEK[10]
      sektemp.tidsSEK[11] = XSEK.SEK[11]
      sektemp.tidsSEK[12] = XSEK.SEK[12]
      sektemp.tidsSEK[13] = XSEK.SEK[13].
   END.
   FIND FIRST XSEK WHERE XSEK.MENYVART = "TIDT" AND
   XSEK.AV-LEVEL = Guru.Konstanter:globniv USE-INDEX XSEK NO-LOCK NO-ERROR.  
   IF AVAILABLE XSEK THEN DO:
      ASSIGN 
      sektemp.tidtLOG = TRUE   
      sektemp.tidtSEK[1] = XSEK.SEK[1]
      sektemp.tidtSEK[2] = XSEK.SEK[2]
      sektemp.tidtSEK[3] = XSEK.SEK[3]
      sektemp.tidtSEK[4] = XSEK.SEK[4]
      sektemp.tidtSEK[5] = XSEK.SEK[5]
      sektemp.tidtSEK[6] = XSEK.SEK[6]
      sektemp.tidtSEK[7] = XSEK.SEK[7]
      sektemp.tidtSEK[8] = XSEK.SEK[8]
      sektemp.tidtSEK[9] = XSEK.SEK[9]
      sektemp.tidtSEK[10] = XSEK.SEK[10]
      sektemp.tidtSEK[11] = XSEK.SEK[11]
      sektemp.tidtSEK[12] = XSEK.SEK[12]
      sektemp.tidtSEK[13] = XSEK.SEK[13].
   END.
END PROCEDURE. 
