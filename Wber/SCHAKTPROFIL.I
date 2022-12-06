
/*------------------------------------------------------------------------
    File        : SCHAKTPROFIL.I
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Thu Nov 14 08:27:38 CET 2019
    Notes       :
  ----------------------------------------------------------------------*/
{SCHAKTPROFILTT.I}

PROCEDURE Styrprofil_UI :
   /*Anders Olsson Elpool i Umeå AB  10 okt 2022 09:37:13 
   vilka koder ska användas vid packning och utbyte massor 
   */
   DEFINE OUTPUT PARAMETER TABLE FOR schackprofilstyrtt.
   DEFINE OUTPUT PARAMETER TABLE FOR tempforlagg.
   DEFINE OUTPUT PARAMETER TABLE FOR temphandelse.
   
   EMPTY TEMP-TABLE temphandelse NO-ERROR. 
   EMPTY TEMP-TABLE tempforlagg NO-ERROR. 
   EMPTY TEMP-TABLE schackprofiltt NO-ERROR. 
   EMPTY TEMP-TABLE schackprofilstyrtt NO-ERROR.
   
   DEFINE VARIABLE hjforlagg AS CHARACTER NO-UNDO.
   DEFINE VARIABLE arbvar AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lopnrvar AS INTEGER NO-UNDO.
   
   IF Guru.Konstanter:globforetag = "GRAN" OR Guru.Konstanter:globforetag = "VAST" THEN DO:
      CREATE schackprofilstyrtt.
      ASSIGN
      schackprofilstyrtt.ARBKOD = "137"
      schackprofilstyrtt.LOPSTART = 01
      schackprofilstyrtt.LOPSLUT = 19
      schackprofilstyrtt.PACKKOD = "13762"
      schackprofilstyrtt.UTBKOD = "13758".
   END. 
   ELSE IF Guru.Konstanter:globforetag = "AKEA"  THEN DO:
      CREATE schackprofilstyrtt.
      ASSIGN
      schackprofilstyrtt.ARBKOD = "137"
      schackprofilstyrtt.LOPSTART = 01
      schackprofilstyrtt.LOPSLUT = 19
      schackprofilstyrtt.PACKKOD = "13761"
      schackprofilstyrtt.UTBKOD = "13758".
   END.     
   ELSE DO:
      
      CREATE schackprofilstyrtt.
      ASSIGN
      schackprofilstyrtt.ARBKOD = "137"
      schackprofilstyrtt.LOPSTART = 01
      schackprofilstyrtt.LOPSLUT = 19
      schackprofilstyrtt.PACKKOD = "13762"
      schackprofilstyrtt.UTBKOD = "13758".
   END.      
   
   FOR EACH FORLAGG WHERE FORLAGG.BORT = FALSE NO-LOCK.
      hjforlagg = FORLAGG.FORLAGG.
      hjforlagg = REPLACE(hjforlagg,"G","").
      CREATE tempforlagg.
      BUFFER-COPY FORLAGG TO tempforlagg.
      tempforlagg.ARBKOD = SUBSTRING(hjforlagg,1,3).
      tempforlagg.LOPNR = INTEGER(TRIM(SUBSTRING(hjforlagg,4,3))) NO-ERROR.
   END.
   FIND FIRST tempforlagg WHERE NO-LOCK NO-ERROR.
   IF NOT AVAILABLE tempforlagg THEN DO:
      FOR EACH schackprofilstyrtt WHERE NO-LOCK:
         arbvar = schackprofilstyrtt.ARBKOD.
         lopnrvar = schackprofilstyrtt.LOPSTART.
         REPEAT :
            IF lopnrvar >  schackprofilstyrtt.LOPSLUT THEN LEAVE.
            FIND FIRST tempforlagg WHERE tempforlagg.ARBKOD = arbvar AND  tempforlagg.LOPNR = lopnrvar   NO-LOCK NO-ERROR.
            IF NOT AVAILABLE tempforlagg THEN DO:
               CREATE tempforlagg.
               ASSIGN
               tempforlagg.ARBKOD = arbvar 
               tempforlagg.LOPNR = lopnrvar.
               lopnrvar = lopnrvar + 1.
            END.     
         END.   
      END.
      FOR EACH tempforlagg  WHERE NO-LOCK:
          FIND FIRST HDHANDELSE  WHERE HDHANDELSE.BENAMNING BEGINS tempforlagg.ARBKOD + STRING(tempforlagg.LOPNR,"99") NO-LOCK NO-ERROR.
          IF AVAILABLE HDHANDELSE THEN DO:
             tempforlagg.ID = HDHANDELSE.ID.
          END.
      END.
   END.   
   FOR EACH schackprofilstyrtt WHERE NO-LOCK:
      FIND FIRST temphandelse WHERE temphandelse.BENAMNING BEGINS schackprofilstyrtt.PACKKOD NO-LOCK NO-ERROR.
      IF NOT AVAILABLE temphandelse THEN DO:
         FIND FIRST HDHANDELSE  WHERE HDHANDELSE.BENAMNING BEGINS schackprofilstyrtt.PACKKOD NO-LOCK NO-ERROR.
         IF NOT AVAILABLE HDHANDELSE THEN DO:
            RUN NyPack_UI (INPUT SUBSTRING(schackprofilstyrtt.PACKKOD,1,5)).
            FIND FIRST HDHANDELSE  WHERE HDHANDELSE.BENAMNING BEGINS schackprofilstyrtt.PACKKOD NO-LOCK NO-ERROR.
         END.
         IF AVAILABLE HDHANDELSE THEN DO:
            CREATE temphandelse.
            BUFFER-COPY HDHANDELSE TO temphandelse.
            temphandelse.TYP = temphandelse.SORTCHAR.
         END.    
      END.
      FIND FIRST temphandelse WHERE temphandelse.BENAMNING BEGINS schackprofilstyrtt.UTBKOD NO-LOCK NO-ERROR.
      IF NOT AVAILABLE temphandelse THEN DO:
         FIND FIRST HDHANDELSE WHERE HDHANDELSE.BENAMNING BEGINS schackprofilstyrtt.UTBKOD NO-LOCK NO-ERROR.
         IF NOT AVAILABLE HDHANDELSE THEN DO:
            RUN NyUmass_UI (INPUT SUBSTRING(schackprofilstyrtt.UTBKOD,1,5)).
            FIND FIRST HDHANDELSE  WHERE HDHANDELSE.BENAMNING BEGINS schackprofilstyrtt.PACKKOD NO-LOCK NO-ERROR.
         END.
         IF AVAILABLE HDHANDELSE THEN DO:
            CREATE temphandelse.
            BUFFER-COPY HDHANDELSE TO temphandelse.
            temphandelse.TYP = temphandelse.SORTCHAR.
         END.    
      END.
   END.
END PROCEDURE.    
PROCEDURE SkapSprofil_UI :
   DEFINE OUTPUT PARAMETER TABLE FOR schackprofiltt.
   DEFINE VARIABLE onr AS INTEGER NO-UNDO.
   /*Anders Olsson Elpool i Umeå AB  5 okt 2022 14:19:09 
   DEFAULT PROFILER SKAPAS 
   */
   
   FIND FIRST schackprofiltt WHERE NO-LOCK NO-ERROR.
   IF NOT AVAILABLE schackprofiltt THEN DO:
     
      RUN sprphuv_UI (INPUT-OUTPU onr,0,0,0,0).
      IF Guru.Konstanter:globforetag = "AKEA" THEN DO:
         RUN sprphuv_UI (INPUT-OUTPU onr,40,70,65,45).
         RUN sprphuv_UI (INPUT-OUTPU onr,40,70,65,30).
         RUN sprphuv_UI (INPUT-OUTPU onr,40,70,65,15).
         RUN sprphuv_UI (INPUT-OUTPU onr,40,70,71,51).
         RUN sprphuv_UI (INPUT-OUTPU onr,40,70,71,30).
         RUN sprphuv_UI (INPUT-OUTPU onr,40,70,71,15).
         RUN sprphuv_UI (INPUT-OUTPU onr,40,70,85,65).
         RUN sprphuv_UI (INPUT-OUTPU onr,40,70,85,30).
         RUN sprphuv_UI (INPUT-OUTPU onr,40,70,85,15).
         RUN sprphuv_UI (INPUT-OUTPU onr,60,90,65,45).
         RUN sprphuv_UI (INPUT-OUTPU onr,60,90,65,30).
         RUN sprphuv_UI (INPUT-OUTPU onr,60,90,65,15).
         RUN sprphuv_UI (INPUT-OUTPU onr,60,90,71,51).
         RUN sprphuv_UI (INPUT-OUTPU onr,60,90,71,30).
         RUN sprphuv_UI (INPUT-OUTPU onr,60,90,71,15).
         RUN sprphuv_UI (INPUT-OUTPU onr,60,90,85,65).
         RUN sprphuv_UI (INPUT-OUTPU onr,60,90,85,30).
         RUN sprphuv_UI (INPUT-OUTPU onr,60,90,85,15).
         RUN sprphuv_UI (INPUT-OUTPU onr,80,110,65,45).
         RUN sprphuv_UI (INPUT-OUTPU onr,80,110,65,30).
         RUN sprphuv_UI (INPUT-OUTPU onr,80,110,65,15).
         RUN sprphuv_UI (INPUT-OUTPU onr,80,110,71,51).
         RUN sprphuv_UI (INPUT-OUTPU onr,80,110,71,30).
         RUN sprphuv_UI (INPUT-OUTPU onr,80,110,71,15).
         RUN sprphuv_UI (INPUT-OUTPU onr,80,110,85,65).
         RUN sprphuv_UI (INPUT-OUTPU onr,80,110,85,30).
         RUN sprphuv_UI (INPUT-OUTPU onr,80,110,85,15).
         RUN sprphuv_UI (INPUT-OUTPU onr,100,130,65,45).
         RUN sprphuv_UI (INPUT-OUTPU onr,100,130,65,30).
         RUN sprphuv_UI (INPUT-OUTPU onr,100,130,65,15).
         RUN sprphuv_UI (INPUT-OUTPU onr,100,130,71,51).
         RUN sprphuv_UI (INPUT-OUTPU onr,100,130,71,30).
         RUN sprphuv_UI (INPUT-OUTPU onr,100,130,71,15).
         RUN sprphuv_UI (INPUT-OUTPU onr,100,130,85,65).
         RUN sprphuv_UI (INPUT-OUTPU onr,100,130,85,30).
         RUN sprphuv_UI (INPUT-OUTPU onr,100,130,85,15).
      END.
     
      ELSE IF Guru.Konstanter:globforetag = "WSP" THEN DO:
         RUN sprphuv_UI (INPUT-OUTPU onr,40,70,65,45).
         RUN sprphuv_UI (INPUT-OUTPU onr,100,130,85,15).
      END.
      ELSE IF Guru.Konstanter:globforetag = "ELPA" THEN DO:
         RUN sprphuv_UI (INPUT-OUTPU onr,40,70,65,45).
         RUN sprphuv_UI (INPUT-OUTPU onr,100,130,85,15).
      END.
      ELSE DO:
         RUN sprphuv_UI (INPUT-OUTPU onr,40,70,65,45).
      END.  
   END.   
END PROCEDURE.



PROCEDURE sprphuv_UI :
   DEFINE INPUT-OUTPUT PARAMETER onr AS INTEGER NO-UNDO.
   DEFINE INPUT  PARAMETER bbin AS INTEGER NO-UNDO.    /*BOTTENBREDD*/
   DEFINE INPUT  PARAMETER tbin AS INTEGER NO-UNDO.    /*TOPPBRED*/ 
   DEFINE INPUT  PARAMETER djpin AS INTEGER NO-UNDO.  /*DJUP PACKNING*/
   DEFINE INPUT  PARAMETER djmin AS INTEGER NO-UNDO.  /*DJUP FÖR UTBYTE AV MASSOR %*/ 
   /*
   Man kommer att kunna välja Schaktprofil BB*TB*DJ-DJM%  i cm för automatisk beräkning av Utbyte massor (kod 137 58) samt Packning (kod 136 62).
BB = BOTTENBREDD TB  = TOPPBREDD DJ = DJUP DJM% = DJUP FÖR UTBYTE AV MASSOR.
   */
   
    
   CREATE schackprofiltt.
   schackprofiltt.ORDNING = onr.
   ASSIGN 
   schackprofiltt.BB = bbin
   schackprofiltt.TB = tbin
   schackprofiltt.DJP = djpin
   schackprofiltt.DJM = djmin.
   schackprofiltt.PACK   = (schackprofiltt.BB + schackprofiltt.TB) / 2 * 0.01 * schackprofiltt.DJP * 0.01.
   schackprofiltt.UTBYTM = (schackprofiltt.BB + schackprofiltt.TB) / 2 * 0.01 * schackprofiltt.DJM * 0.01.  
   IF bbin = 0 THEN schackprofiltt.SPROFIL = "".
   ELSE schackprofiltt.SPROFIL = STRING(schackprofiltt.BB) + "x" +  STRING(schackprofiltt.TB) + "x" +  STRING(schackprofiltt.DJP)  + "-" +  STRING(schackprofiltt.DJM) + "%".
   onr = onr + 1. 
END PROCEDURE.

PROCEDURE NyPack_UI :
   DEFINE INPUT  PARAMETER benvar AS CHARACTER NO-UNDO.
   DEFINE VARIABLE idvar AS INTEGER NO-UNDO.
   DEFINE VARIABLE ovar AS INTEGER NO-UNDO.
   DO TRANSACTION:       
      FIND LAST HDHANDELSE USE-INDEX ID NO-LOCK NO-ERROR.
      IF AVAILABLE HDHANDELSE THEN idvar = HDHANDELSE.ID.
      idvar = idvar + 1.
      FOR EACH  HDHANDELSE NO-LOCK BY ORDNING :
         ovar =  HDHANDELSE .ORDNING.
      END.
      ovar = ovar + 1.
      CREATE HDHANDELSE.
      ASSIGN 
      HDHANDELSE.ID = idvar
      HDHANDELSE.ENHET = "m" 
      HDHANDELSE.SORTCHAR = "FH"
      HDHANDELSE.BENAMNING = benvar + " Pack sch vibro"  
      HDHANDELSE.ORDNING = ovar.  
   END.
     
END PROCEDURE.

PROCEDURE NyUmass_UI :
   DEFINE INPUT  PARAMETER benvar AS CHARACTER NO-UNDO.
   DEFINE VARIABLE idvar AS INTEGER NO-UNDO.
   DEFINE VARIABLE ovar AS INTEGER NO-UNDO.
   DO TRANSACTION:       
      FIND LAST HDHANDELSE USE-INDEX ID NO-LOCK NO-ERROR.
      IF AVAILABLE HDHANDELSE THEN idvar = HDHANDELSE.ID.
      idvar = idvar + 1.
      FOR EACH  HDHANDELSE NO-LOCK BY ORDNING :
         ovar =  HDHANDELSE .ORDNING.
      END.
      ovar = ovar + 1.
      CREATE HDHANDELSE.
      ASSIGN 
      HDHANDELSE.ID = idvar
      HDHANDELSE.ENHET = "m³" 
      HDHANDELSE.SORTCHAR = "FH"
      HDHANDELSE.BENAMNING = benvar + " Utb massor"  
      HDHANDELSE.ORDNING = ovar.  
   END.
     
END PROCEDURE.

