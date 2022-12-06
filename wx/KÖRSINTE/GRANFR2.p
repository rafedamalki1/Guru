/*GRANFR2.P SKAPAR PA90FIL FRANVARO.*/  
DEFINE STREAM frloko. 
DEFINE STREAM frlotj. 
DEFINE STREAM frlosy.
DEFINE STREAM franko. 
DEFINE STREAM frantj.  
/*DEFINE STREAM fransy.*/
{LESAMMAN.I}
DEFINE INPUT PARAMETER invkdatum AS DATE NO-UNDO.
DEFINE INPUT PARAMETER ingvisatidpermanad AS LOGICAL NO-UNDO.
DEFINE INPUT PARAMETER inglobforetag LIKE FORETAG.FORETAG NO-UNDO.
DEFINE INPUT PARAMETER inman AS INTEGER FORMAT "99" NO-UNDO.
RUN sammut_UI (INPUT 1).
DEFINE NEW SHARED VARIABLE vkdatum AS DATE NO-UNDO.
DEFINE NEW SHARED VARIABLE gvisatidpermanad AS LOGICAL NO-UNDO.

DEFINE NEW SHARED VARIABLE man AS INTEGER FORMAT "99" NO-UNDO.
ASSIGN
vkdatum     = invkdatum  
gvisatidpermanad   = ingvisatidpermanad 
Guru.Konstanter:globforetag = inglobforetag
man         = inman.
DEFINE NEW SHARED VARIABLE nytid AS DECIMAL FORMAT "99.99" NO-UNDO.
DEFINE NEW SHARED VARIABLE sekunder AS INTEGER FORMAT "-9999999" NO-UNDO.
DEFINE VARIABLE rapphj1 AS CHARACTER FORMAT "X(9)" NO-UNDO.      /*HJALP ANTAL*/
DEFINE VARIABLE rapphj2 AS CHARACTER FORMAT "X(11)" NO-UNDO.
DEFINE VARIABLE rapphj3 AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE VARIABLE overrapp1 AS CHARACTER FORMAT "X(100)" NO-UNDO.      /*LON*/
DEFINE VARIABLE regdat1 AS DATE NO-UNDO.
DEFINE VARIABLE regdat2 AS DATE NO-UNDO.  
DEFINE VARIABLE onr2 LIKE TIDREGITAB.AONR NO-UNDO.
DEFINE VARIABLE dnr2 LIKE TIDREGITAB.DELNR NO-UNDO.
DEFINE VARIABLE antal LIKE TIDREGITAB.LONTILLANTAL NO-UNDO.   /*LON*/
DEFINE VARIABLE proc1 AS INTEGER FORMAT "999" NO-UNDO.
DEFINE VARIABLE pnr LIKE PERSONALTAB.PERSONALKOD NO-UNDO.      /*LON*/
DEFINE VARIABLE anst LIKE ANSTFORM.KOD NO-UNDO.
DEFINE VARIABLE fkod LIKE FVARO.FRKOD NO-UNDO.
DEFINE VARIABLE reco AS RECID NO-UNDO.
DEFINE VARIABLE ftag AS CHARACTER NO-UNDO.
DEFINE VARIABLE kolle AS CHARACTER NO-UNDO.
DEFINE VARIABLE kolldatum AS DATE NO-UNDO.
DEFINE VARIABLE prognamn AS CHARACTER FORMAT "X(41)" NO-UNDO.
DEFINE TEMP-TABLE franvaro
   FIELD PERSONALKOD LIKE PERSONALTAB.PERSONALKOD
   FIELD AONR LIKE TIDREGITAB.AONR
   FIELD DELNR LIKE TIDREGITAB.DELNR
   FIELD FRAN LIKE TIDREGITAB.DATUM
   FIELD TILL LIKE TIDREGITAB.DATUM
   FIELD PROCENT AS INTEGER FORMAT "999"
   FIELD TIMMAR AS DECIMAL FORMAT "99.99"
   FIELD LART AS CHARACTER FORMAT "X(4)"   
   FIELD PKOD LIKE ANSTFORMTAB.KOD
   FIELD VIJUDID AS CHARACTER
   INDEX FRANVARO IS PRIMARY  PERSONALKOD FRAN LART ASCENDING
   INDEX KALMAR VIJUDID PERSONALKOD FRAN LART ASCENDING.
DEFINE TEMP-TABLE fvarokop
   FIELD PERSONALKOD LIKE PERSONALTAB.PERSONALKOD
   FIELD AONR LIKE TIDREGITAB.AONR
   FIELD DELNR LIKE TIDREGITAB.DELNR
   FIELD FRAN LIKE TIDREGITAB.DATUM
   FIELD TILL LIKE TIDREGITAB.DATUM
   FIELD PROCENT AS INTEGER FORMAT "999"
   FIELD TIMMAR AS DECIMAL FORMAT "99.99"
   FIELD LART AS CHARACTER FORMAT "X(4)"   
   FIELD PKOD LIKE ANSTFORMTAB.KOD
   FIELD VIJUDID AS CHARACTER
   INDEX FRANVARO IS PRIMARY  PERSONALKOD FRAN LART ASCENDING
   INDEX KALMAR VIJUDID PERSONALKOD FRAN LART ASCENDING.

DEFINE TEMP-TABLE frsum
   FIELD PERSONALKOD AS CHARACTER
   FIELD DATUM AS DATE
   FIELD TIMMAR AS DECIMAL FORMAT "99.99".
{AMERICANEUROPEAN.I}   
IF Guru.Konstanter:globforetag = "GRAN" THEN DO:
   INPUT FROM \\GRANGURU\guru_ser\server\PRO9S\gran\fransu.d NO-ECHO.
END. 
ELSE IF Guru.Konstanter:globforetag = "GADM" THEN DO:
   INPUT FROM \\GRANGURU\guru_ser\server\PRO9S\gadm\fransu.d NO-ECHO.
END.  
ELSE IF Guru.Konstanter:globforetag = "GSYD" THEN DO:
   INPUT FROM \\GRANGURU\guru_ser\server\PRO9S\gsyd\fransu.d NO-ECHO.
END. 
ELSE IF Guru.Konstanter:globforetag = "GRIT" THEN DO:
   INPUT FROM \\GRANGURU\guru_ser\server\PRO9S\grit\fransu.d NO-ECHO.
END. 
ELSE IF Guru.Konstanter:globforetag = "GKAL" THEN DO:
   INPUT FROM d:\DELAD\server\PRO9S\gkal\fransu.d NO-ECHO.
END. 
ELSE IF Guru.Konstanter:globforetag = "GKRVA" THEN DO:
   INPUT FROM \\GRANGURU\guru_ser\server\PRO9S\gkrva\fransu.d NO-ECHO.
END. 
ELSE IF Guru.Konstanter:globforetag = "ELPA" THEN DO:
   INPUT FROM \\pc112\delad\pro9s\korning\fransu.d NO-ECHO.
END.   
REPEAT TRANSACTION:
  CREATE franvaro.
  ASSIGN.
  IMPORT franvaro.
END.   
REPEAT TRANSACTION:
  FIND NEXT franvaro USE-INDEX franvaro EXCLUSIVE-LOCK NO-ERROR.
  IF NOT AVAILABLE franvaro THEN LEAVE.  
  IF gvisatidpermanad = FALSE AND man = 12  THEN DO:
    IF MONTH(franvaro.FRAN) < 10 AND MONTH(franvaro.TILL) < 10 THEN NEXT.
    IF MONTH(franvaro.FRAN) > 10  AND MONTH(franvaro.TILL) > 10 THEN NEXT.
    IF MONTH(franvaro.FRAN) > 10 AND MONTH(franvaro.TILL) < 10 THEN DO:
      pnr = franvaro.PERSONALKOD.
      onr2 = franvaro.AONR.
      dnr2 = franvaro.DELNR.
      fkod = franvaro.LART.
      regdat1 = franvaro.TILL.
      regdat2 = regdat1.
      proc1 = franvaro.PROCENT.
      antal = franvaro.TIMMAR.
      anst = franvaro.PKOD.
      repeat:
	regdat2 = regdat2 - 1.
	IF MONTH(regdat2) > MONTH(regdat1) THEN LEAVE.
      END.
      ASSIGN franvaro.TILL = regdat2.
      regdat2 = regdat2 + 1.
      CREATE franvaro.
      ASSIGN franvaro.PERSONALKOD = pnr
      franvaro.AONR = onr2
      franvaro.DELNR = dnr2
      franvaro.LART = fkod
      franvaro.FRAN = regdat2
      franvaro.TILL = regdat1
      franvaro.PROCENT = proc1
      franvaro.TIMMAR = antal
      franvaro.PKOD = anst.
    END.
  END.
  ELSE DO:
     IF gvisatidpermanad = FALSE THEN DO: 
        IF MONTH(franvaro.FRAN) > man AND MONTH(franvaro.TILL) > man THEN NEXT.
        IF MONTH(franvaro.FRAN) LE man AND MONTH(franvaro.TILL) LE man THEN NEXT.
        IF MONTH(franvaro.FRAN) LE man AND MONTH(franvaro.TILL) > man THEN DO:
           pnr = franvaro.PERSONALKOD.
           onr2 = franvaro.AONR.
           dnr2 = franvaro.DELNR.
           fkod = franvaro.LART.
           regdat1 = franvaro.TILL.
           regdat2 = regdat1.
           proc1 = franvaro.PROCENT.
           antal = franvaro.TIMMAR.
           anst = franvaro.PKOD.
           repeat:
	       regdat2 = regdat2 - 1.
	       IF MONTH(regdat2) < MONTH(regdat1) THEN LEAVE.
           END.
           ASSIGN franvaro.TILL = regdat2.
           regdat2 = regdat2 + 1.
           CREATE franvaro.
           ASSIGN franvaro.PERSONALKOD = pnr
           franvaro.AONR = onr2
           franvaro.DELNR = dnr2
           franvaro.LART = fkod
           franvaro.FRAN = regdat2
           franvaro.TILL = regdat1
           franvaro.PROCENT = proc1
           franvaro.TIMMAR = antal
           franvaro.PKOD = anst.
        END.
     END.   
     ELSE DO: 
        IF MONTH(franvaro.FRAN) > MONTH(vkdatum) AND MONTH(franvaro.TILL) > MONTH(vkdatum) THEN NEXT.
        IF MONTH(franvaro.FRAN) LE MONTH(vkdatum) AND MONTH(franvaro.TILL) LE MONTH(vkdatum) THEN NEXT.
        IF MONTH(franvaro.FRAN) LE MONTH(vkdatum) AND MONTH(franvaro.TILL) > MONTH(vkdatum) THEN DO:              
           pnr = franvaro.PERSONALKOD.
           onr2 = franvaro.AONR.
           dnr2 = franvaro.DELNR.
           fkod = franvaro.LART.
           regdat1 = franvaro.TILL.
           regdat2 = regdat1.
           proc1 = franvaro.PROCENT.
           antal = franvaro.TIMMAR.
           anst = franvaro.PKOD.
           repeat:
              regdat2 = regdat2 - 1.
              IF MONTH(regdat2) < MONTH(regdat1) THEN LEAVE.
           END.
           ASSIGN franvaro.TILL = regdat2.
           regdat2 = regdat2 + 1.
           CREATE franvaro.
           ASSIGN franvaro.PERSONALKOD = pnr
           franvaro.AONR = onr2
           franvaro.DELNR = dnr2
           franvaro.LART = fkod
           franvaro.FRAN = regdat2
           franvaro.TILL = regdat1
           franvaro.PROCENT = proc1
           franvaro.TIMMAR = antal
           franvaro.PKOD = anst.
        END.
     END.
  END.
END.
IF Guru.Konstanter:globforetag = "GKAL"    THEN DO:      
   FOR EACH franvaro:
      IF Guru.Konstanter:globforetag = "GADM"  THEN DO:
         FIND FIRST PERSONALTAB WHERE PERSONALTAB.ANSTNR = franvaro.PERSONALKOD NO-LOCK NO-ERROR.
      END.
      ELSE DO:      
         FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = franvaro.PERSONALKOD NO-LOCK NO-ERROR.
      END.
      FIND FIRST OMRADETAB WHERE OMRADETAB.OMRADE = PERSONALTAB.OMRADE NO-LOCK NO-ERROR.
      FIND FIRST AVDELNING WHERE AVDELNING.AVDELNINGNR = OMRADETAB.AVDELNINGNR NO-LOCK NO-ERROR.
      FIND FIRST JURPERS WHERE JURPERS.JUDID = AVDELNING.POSTANST NO-LOCK NO-ERROR.      
      IF AVAILABLE JURPERS THEN DO:
         ASSIGN franvaro.VIJUDID = JURPERS.VIJUDID.
      END.
   END.
END.
IF Guru.Konstanter:globforetag = "GRAN"    OR Guru.Konstanter:globforetag = "ELPA" THEN DO:      
   FOR EACH franvaro WHERE franvaro.TIMMAR > 0 AND franvaro.FRAN = franvaro.TILL: 
      CREATE fvarokop.
      BUFFER-COPY franvaro TO fvarokop.
   END.
   FOR EACH fvarokop:
      nytid =  fvarokop.TIMMAR.
      RUN TIMSEK.P.
      ASSIGN fvarokop.TIMMAR = sekunder / 3600.
   END.
   FOR EACH fvarokop  BREAK BY fvarokop.PERSONALKOD BY fvarokop.FRAN: 
      ACCUMULATE fvarokop.TIMMAR (TOTAL BY fvarokop.PERSONALKOD BY fvarokop.FRAN).
      IF LAST-OF(fvarokop.FRAN) THEN DO: 
         CREATE frsum.         
         ASSIGN      
         frsum.PERSONALKOD = fvarokop.PERSONALKOD 
         frsum.DATUM = fvarokop.FRAN            
         frsum.TIMMAR = (ACCUM TOTAL BY fvarokop.FRAN fvarokop.TIMMAR).              
      END.
   END.
   FOR EACH frsum WHERE frsum.TIMMAR = 8.25:
      FIND FIRST franvaro WHERE franvaro.PERSONALKOD = frsum.PERSONALKOD AND
      franvaro.FRAN = franvaro.TILL AND franvaro.FRAN = frsum.DATUM
      AND (franvaro.TIMMAR - INTEGER(franvaro.TIMMAR)) =  0.15 NO-ERROR.
      IF NOT AVAILABLE franvaro THEN DO:
         FIND LAST franvaro WHERE franvaro.PERSONALKOD = frsum.PERSONALKOD AND
         franvaro.FRAN = franvaro.TILL AND franvaro.FRAN = frsum.DATUM NO-ERROR.
      END.
      IF AVAILABLE franvaro THEN DO:
         nytid = franvaro.TIMMAR.
         RUN TIMSEK.P.
         sekunder = sekunder - 900.
         RUN SEKTIM.P.
         franvaro.TIMMAR = nytid.
      END.
   END.
END.


IF Guru.Konstanter:globforetag = "GRAN" THEN DO:
   OUTPUT  TO \\GRANGURU\guru_ser\server\PRO9S\gran\felman.d NO-ECHO.
END.  
ELSE IF Guru.Konstanter:globforetag = "GADM" THEN DO:
   OUTPUT  TO \\GRANGURU\guru_ser\server\PRO9S\gadm\felman.d NO-ECHO.
END.  
ELSE IF Guru.Konstanter:globforetag = "GRIT" THEN DO:
   OUTPUT  TO \\GRANGURU\guru_ser\server\PRO9S\grit\felman.d NO-ECHO.
END. 
ELSE IF Guru.Konstanter:globforetag = "GKAL" THEN DO:
   OUTPUT  TO d:\DELAD\server\PRO9S\gkal\felman.d NO-ECHO.
END. 
ELSE IF Guru.Konstanter:globforetag = "GKRVA" THEN DO:
   OUTPUT  TO \\GRANGURU\guru_ser\server\PRO9S\gkrva\felman.d NO-ECHO.
END. 
ELSE IF Guru.Konstanter:globforetag = "ELPA" THEN DO:
   OUTPUT TO \\pc112\delad\pro9s\korning\felman.d NO-ECHO.
END.   

kolldatum = DATE(MONTH(vkdatum),01,YEAR(vkdatum)).
FOR EACH franvaro WHERE franvaro.FRAN < kolldatum USE-INDEX franvaro  NO-LOCK:   
  EXPORT franvaro.
END.
OUTPUT CLOSE.


/*HUR SKALL MAN GÖRA HÄR MED GSYD*/
DO TRANSACTION:
IF Guru.Konstanter:globforetag = "GRAN" THEN DO:
   OUTPUT STREAM franko TO \\GRANGURU\guru_ser\server\PRO9S\gran\frkoll.d NO-ECHO.   
   OUTPUT STREAM frloko TO \\GRANGURU\guru_ser\server\PRO9S\gran\lokoll.d APPEND.
   ftag = "8888".   /*2222*/
   kolle = "05".    /*04*/
END.  
ELSE IF Guru.Konstanter:globforetag = "GADM" THEN DO:
   OUTPUT STREAM franko TO \\GRANGURU\guru_ser\server\PRO9S\gadm\frkoll.d NO-ECHO.   
   OUTPUT STREAM frloko TO \\GRANGURU\guru_ser\server\PRO9S\gadm\lokoll.d APPEND.
   ftag = "8888".    /*2000*/
   kolle = "03".     /*04*/
END.  
ELSE IF Guru.Konstanter:globforetag = "GSYD" THEN DO:
   OUTPUT STREAM franko TO \\GRANGURU\guru_ser\server\PRO9S\gsyd\frkoll.d NO-ECHO.   
   OUTPUT STREAM frloko TO \\GRANGURU\guru_ser\server\PRO9S\gsyd\lokoll.d APPEND.
   ftag = "4444".
   kolle = "01". 
END.      
ELSE IF Guru.Konstanter:globforetag = "GRIT" THEN DO:
   OUTPUT STREAM franko TO \\GRANGURU\guru_ser\server\PRO9S\grit\frkoll.d NO-ECHO.   
   OUTPUT STREAM frloko TO \\GRANGURU\guru_ser\server\PRO9S\grit\lokoll.d APPEND.
   /* tidigare 2000 04*/
   ftag = "8888".
   kolle = "08". 
END.   
ELSE IF Guru.Konstanter:globforetag = "GKAL" THEN DO:
   OUTPUT STREAM franko TO d:\DELAD\server\PRO9S\gkal\frkoll.d NO-ECHO.   
   OUTPUT STREAM frloko TO d:\DELAD\server\PRO9S\gkal\lokoll.d APPEND.
   ftag = "2003".
   kolle = "01". 
END.       
ELSE IF Guru.Konstanter:globforetag = "GKRVA" THEN DO:
   OUTPUT STREAM franko TO \\GRANGURU\guru_ser\server\PRO9S\gkrva\frkoll.d NO-ECHO.   
   OUTPUT STREAM frloko TO \\GRANGURU\guru_ser\server\PRO9S\gkrva\lokoll.d APPEND.
   ftag = "8888".
   kolle = "06". 
END.  

ELSE IF Guru.Konstanter:globforetag = "ELPA" THEN DO:
   OUTPUT STREAM frloko TO \\pc112\delad\pro9s\korning\lokoll.d APPEND.  
   OUTPUT STREAM franko TO \\pc112\delad\pro9s\korning\frkoll.d.  
   ftag = "2000".
   kolle = "04". 
END.                                
IF Guru.Konstanter:globforetag = "GRAN" OR Guru.Konstanter:globforetag = "cELPA"  THEN DO:
   FIND LAST franvaro WHERE franvaro.PERSONALKOD BEGINS "35" USE-INDEX franvaro NO-LOCK NO-ERROR.
END.  
ELSE IF Guru.Konstanter:globforetag = "GADM" THEN DO:
   FIND LAST franvaro WHERE franvaro.VIJUDID = "GRA" USE-INDEX franvaro NO-LOCK NO-ERROR.
END.  
ELSE IF Guru.Konstanter:globforetag = "GKRVA" THEN DO:
   FIND LAST franvaro WHERE franvaro.VIJUDID = "GVAOST" USE-INDEX franvaro NO-LOCK NO-ERROR.
   IF franvaro.VIJUDID = "GVAOST" THEN kolle = "06".
END.  
ELSE DO:
   FIND LAST franvaro USE-INDEX franvaro NO-LOCK NO-ERROR.
END.  
reco = RECID(franvaro).  
IF Guru.Konstanter:globforetag = "GRAN" OR Guru.Konstanter:globforetag = "cELPA" THEN DO:                   
   FIND FIRST franvaro  WHERE franvaro.PERSONALKOD BEGINS "35" USE-INDEX franvaro EXCLUSIVE-LOCK NO-ERROR.
END.  
ELSE IF Guru.Konstanter:globforetag = "GADM" THEN DO:
   FIND FIRST franvaro WHERE franvaro.VIJUDID = "GRA" USE-INDEX franvaro EXCLUSIVE-LOCK NO-ERROR.
END.  
ELSE IF Guru.Konstanter:globforetag = "GKRVA" THEN DO:
   FIND FIRST franvaro WHERE franvaro.VIJUDID = "GVAOST" USE-INDEX franvaro EXCLUSIVE-LOCK NO-ERROR.
   IF franvaro.VIJUDID = "GVAOST" THEN kolle = "06".
END. 
ELSE DO:                   
   FIND FIRST franvaro USE-INDEX franvaro EXCLUSIVE-LOCK NO-ERROR.
END.
/*IF NOT AVAILABLE franvaro THEN LEAVE.*/
IF AVAILABLE franvaro THEN DO:
  IF Guru.Konstanter:globforetag = "GKAL" THEN DO:
     IF franvaro.VIJUDID = "GKEAB" THEN kolle = "01".
     ELSE IF franvaro.VIJUDID = "GSEAB" THEN kolle = "02".
  END.    
  IF Guru.Konstanter:globforetag = "GRIT" THEN DO:
      IF franvaro.VIJUDID = "Gra" THEN ASSIGN kolle = "08".
      ELSE IF franvaro.VIJUDID = "Fors" THEN ASSIGN kolle = "03".      
      ELSE IF franvaro.VIJUDID = "Serv" THEN ASSIGN kolle = "02".     
      ELSE IF franvaro.VIJUDID = "FEKA" THEN ASSIGN kolle = "09".     
      ELSE ASSIGN kolle = "08".      
  END.    
  IF gvisatidpermanad = FALSE THEN DO:     
     IF man = 12 AND MONTH(franvaro.FRAN) < 10  
     AND MONTH(franvaro.TILL) < 10 THEN  antal = antal.
     ELSE IF man = 1 AND MONTH(franvaro.FRAN) = 2
     AND MONTH(franvaro.TILL) = 2 THEN  antal = antal.
     ELSE IF MONTH(franvaro.FRAN) > man AND MONTH(franvaro.TILL) > man THEN  antal = antal.
     ELSE DO:
       nytid = franvaro.TIMMAR.
       RUN TIMSEK.P.
       franvaro.TIMMAR = sekunder / 36.  
       rapphj1 = STRING(franvaro.TIMMAR,"999999"). 
       overrapp1 = ftag + " " + kolle + " " + SUBSTRING(franvaro.PERSONALKOD,1 ,5) + " " +  
       SUBSTRING(franvaro.LART,1,3) +   
       " " + SUBSTRING(rapphj1,1 ,6)  +  
       " " + "000000" + " " + "0000000" +
       " " + "000000000" + " " + 
       SUBSTRING(STRING(franvaro.FRAN),1 ,2) +
       SUBSTRING(STRING(franvaro.FRAN),4 ,2) +
       SUBSTRING(STRING(franvaro.FRAN),7 ,2) + " " +
       SUBSTRING(STRING(franvaro.TILL),1 ,2) +
       SUBSTRING(STRING(franvaro.TILL),4 ,2) +
       SUBSTRING(STRING(franvaro.TILL),7 ,2) + " " +
       "000" + " " + "0000" + " " + "      " + " " + "EL".
       IF  franvaro.LART = "530" THEN DO:
          PUT STREAM franko overrapp1 SKIP. 
       END.   
       PUT STREAM frloko overrapp1 SKIP.
       DELETE franvaro.
    END.   
  END.
  ELSE DO:
     nytid = franvaro.TIMMAR.
     RUN TIMSEK.P.
     franvaro.TIMMAR = sekunder / 36.  
     rapphj1 = STRING(franvaro.TIMMAR,"999999"). 
     overrapp1 = ftag + " " + kolle + " " + SUBSTRING(franvaro.PERSONALKOD,1 ,5) + " " +  
     SUBSTRING(franvaro.LART,1,3) +   
     " " + SUBSTRING(rapphj1,1 ,6)  +  
     " " + "000000" + " " + "0000000" +
     " " + "000000000" + " " + 
     SUBSTRING(STRING(franvaro.FRAN),1 ,2) +
     SUBSTRING(STRING(franvaro.FRAN),4 ,2) +
     SUBSTRING(STRING(franvaro.FRAN),7 ,2) + " " +
     SUBSTRING(STRING(franvaro.TILL),1 ,2) +
     SUBSTRING(STRING(franvaro.TILL),4 ,2) +
     SUBSTRING(STRING(franvaro.TILL),7 ,2) + " " +
     "000" + " " + "0000" + " " + "      " + " " + "EL".
     IF  franvaro.LART = "530" THEN DO:
        PUT STREAM franko overrapp1 SKIP. 
     END.   
     PUT STREAM frloko overrapp1 SKIP.
     DELETE franvaro.
  END.   
END.
END.
REPEAT TRANSACTION:    
   IF Guru.Konstanter:globforetag = "GRAN" OR Guru.Konstanter:globforetag = "cELPA" THEN DO:                   
     FIND NEXT franvaro  WHERE franvaro.PERSONALKOD BEGINS "35" USE-INDEX franvaro EXCLUSIVE-LOCK NO-ERROR.
   END.
   ELSE IF Guru.Konstanter:globforetag = "GADM" THEN DO:
      FIND NEXT franvaro WHERE franvaro.VIJUDID = "GRA" USE-INDEX franvaro EXCLUSIVE-LOCK NO-ERROR.
   END. 
   ELSE IF Guru.Konstanter:globforetag = "GKRVA" THEN DO:
      FIND NEXT franvaro WHERE franvaro.VIJUDID = "GVAOST" USE-INDEX franvaro EXCLUSIVE-LOCK NO-ERROR.
      IF franvaro.VIJUDID = "GVAOST" THEN kolle = "06".
   END. 
   ELSE DO:                   
      FIND NEXT franvaro USE-INDEX franvaro EXCLUSIVE-LOCK NO-ERROR.
   END.  
  IF NOT AVAILABLE franvaro THEN LEAVE. 
  IF AVAILABLE franvaro THEN DO:
    IF Guru.Konstanter:globforetag = "GKAL" THEN DO:
       IF franvaro.VIJUDID = "GKEAB" THEN kolle = "01".
       ELSE IF franvaro.VIJUDID = "GSEAB" THEN kolle = "02".
    END.
    IF Guru.Konstanter:globforetag = "GRIT" THEN DO:
      IF franvaro.VIJUDID = "Gra" THEN ASSIGN kolle = "08".
      ELSE IF franvaro.VIJUDID = "Fors" THEN ASSIGN kolle = "03".      
      ELSE IF franvaro.VIJUDID = "Serv" THEN ASSIGN kolle = "02".     
      ELSE IF franvaro.VIJUDID = "FEKA" THEN ASSIGN kolle = "09".     
      ELSE ASSIGN kolle = "08".      
  END.    
    IF gvisatidpermanad = FALSE THEN DO:
       IF man = 12 AND MONTH(franvaro.FRAN) < 6
       AND MONTH(franvaro.TILL) < 6 THEN NEXT.
       ELSE IF man = 1 AND MONTH(franvaro.FRAN) = 2
       AND MONTH(franvaro.TILL) = 2 THEN NEXT.
       ELSE IF MONTH(franvaro.FRAN) > man AND MONTH(franvaro.TILL) > man THEN NEXT.
       ELSE DO:
         nytid = franvaro.TIMMAR.
         RUN TIMSEK.P.
         franvaro.TIMMAR = sekunder / 36.
         rapphj1 = STRING(franvaro.TIMMAR,"999999").  
         overrapp1 = ftag + " " + kolle + " " + SUBSTRING(franvaro.PERSONALKOD,1 ,5) + " " + 
         SUBSTRING(franvaro.LART,1,3) + 
         " " + SUBSTRING(rapphj1,1 ,6) +  
         " " + "000000"  + " " + "0000000" +
         " " + "000000000" + " " +
         SUBSTRING(STRING(franvaro.FRAN),1 ,2) +
         SUBSTRING(STRING(franvaro.FRAN),4 ,2) +
         SUBSTRING(STRING(franvaro.FRAN),7 ,2) + " " +
         SUBSTRING(STRING(franvaro.TILL),1 ,2) +
         SUBSTRING(STRING(franvaro.TILL),4 ,2) +
         SUBSTRING(STRING(franvaro.TILL),7 ,2) + " " +
         "000" + " " + "0000" + " " + "      " + " " + "EL".  
         IF  franvaro.LART = "530" THEN DO:
            PUT STREAM franko overrapp1 SKIP. 
         END.   
         PUT STREAM frloko overrapp1 SKIP.                                               
         DELETE franvaro.
       END.
    END.  
    ELSE DO:
       nytid = franvaro.TIMMAR.
       RUN TIMSEK.P.
       franvaro.TIMMAR = sekunder / 36.
       rapphj1 = STRING(franvaro.TIMMAR,"999999").  
       overrapp1 = ftag + " " + kolle + " " + SUBSTRING(franvaro.PERSONALKOD,1 ,5) + " " + 
       SUBSTRING(franvaro.LART,1,3) + 
       " " + SUBSTRING(rapphj1,1 ,6) +  
       " " + "000000"  + " " + "0000000" +
       " " + "000000000" + " " +
       SUBSTRING(STRING(franvaro.FRAN),1 ,2) +
       SUBSTRING(STRING(franvaro.FRAN),4 ,2) +
       SUBSTRING(STRING(franvaro.FRAN),7 ,2) + " " +
       SUBSTRING(STRING(franvaro.TILL),1 ,2) +
       SUBSTRING(STRING(franvaro.TILL),4 ,2) +
       SUBSTRING(STRING(franvaro.TILL),7 ,2) + " " +
       "000" + " " + "0000" + " " + "      " + " " + "EL".  
       IF  franvaro.LART = "530" THEN DO:
          PUT STREAM franko overrapp1 SKIP. 
       END.   
       PUT STREAM frloko overrapp1 SKIP.                                                
       DELETE franvaro.         
    END.
  END.
END.
OUTPUT STREAM franko CLOSE.      
OUTPUT STREAM frloko CLOSE.
IF Guru.Konstanter:globforetag = "GRAN" THEN DO:
   OUTPUT STREAM frantj TO \\GRANGURU\guru_ser\server\PRO9S\gran\frtjan.d NO-ECHO.  
   OUTPUT STREAM frlotj TO \\GRANGURU\guru_ser\server\PRO9S\gran\lotjan.d APPEND.
END.                                             
ELSE IF Guru.Konstanter:globforetag = "GADM" THEN DO:
   OUTPUT STREAM frantj TO \\GRANGURU\guru_ser\server\PRO9S\gadm\frtjan.d NO-ECHO.  
   OUTPUT STREAM frlotj TO \\GRANGURU\guru_ser\server\PRO9S\gadm\lotjan.d APPEND.
END.                                             
ELSE IF Guru.Konstanter:globforetag = "GKRVA" THEN DO:
   OUTPUT STREAM frantj TO \\GRANGURU\guru_ser\server\PRO9S\gkrva\frtjan.d NO-ECHO.  
   OUTPUT STREAM frlotj TO \\GRANGURU\guru_ser\server\PRO9S\gkrva\lotjan.d APPEND.
END.                                             
ELSE IF Guru.Konstanter:globforetag = "GRIT" THEN nytid = nytid.
ELSE IF Guru.Konstanter:globforetag = "GKAL" THEN nytid = nytid. 
ELSE IF Guru.Konstanter:globforetag = "GSYD" THEN nytid = nytid.
ELSE IF Guru.Konstanter:globforetag = "cELPA" THEN DO:
   OUTPUT STREAM frantj TO \\pc112\delad\pro9s\korning\frtjan.d NO-ECHO.
   OUTPUT STREAM frlotj TO \\pc112\delad\pro9s\korning\lotjan.d APPEND.
END.  
IF Guru.Konstanter:globforetag = "GRAN"    THEN DO: 
   FOR EACH franvaro WHERE franvaro.LART = "585" AND franvaro.TIMMAR > 0:
      /*föräldrapenning del av dag på ny löneart 20040908*/
      ASSIGN franvaro.LART = "588".
   END.
END.
DO TRANSACTION:
   IF Guru.Konstanter:globforetag = "GRAN"  OR Guru.Konstanter:globforetag = "cELPA"  THEN DO:
      IF Guru.Konstanter:globforetag = "GRAN" THEN DO:      
         FIND LAST franvaro WHERE franvaro.PERSONALKOD BEGINS "45" USE-INDEX franvaro NO-LOCK NO-ERROR.
         reco = RECID(franvaro).                       
         FIND FIRST franvaro  WHERE franvaro.PERSONALKOD BEGINS "45" USE-INDEX franvaro EXCLUSIVE-LOCK NO-ERROR.
         kolle = "05".
      END.      
      IF Guru.Konstanter:globforetag = "GADM" THEN DO:      
         FIND LAST franvaro WHERE franvaro.VIJUDID = "GRAEM" USE-INDEX franvaro NO-LOCK NO-ERROR.         
         reco = RECID(franvaro).                       
         FIND FIRST franvaro WHERE franvaro.VIJUDID = "GRAEM" USE-INDEX franvaro EXCLUSIVE-LOCK NO-ERROR.                  
         IF franvaro.VIJUDID = "GRAEM" THEN kolle = "03".
      END.
      IF Guru.Konstanter:globforetag = "GKRVA" THEN DO:      
         FIND LAST franvaro WHERE franvaro.VIJUDID = "GVARME" USE-INDEX franvaro NO-LOCK NO-ERROR.         
         reco = RECID(franvaro).                       
         FIND FIRST franvaro WHERE franvaro.VIJUDID = "GVARME" USE-INDEX franvaro EXCLUSIVE-LOCK NO-ERROR.                  
         IF franvaro.VIJUDID = "GVARME" THEN kolle = "07".
      END.
      /*IF NOT AVAILABLE franvaro THEN LEAVE.*/
      IF AVAILABLE franvaro THEN DO:
        IF franvaro.PERSONALKOD = "45086" THEN antal = antal.
        ELSE DO:        
           IF gvisatidpermanad = FALSE THEN DO:
              IF man = 12 AND MONTH(franvaro.FRAN) < 10
              AND MONTH(franvaro.TILL) < 10 THEN  antal = antal.
              ELSE IF man = 1 AND MONTH(franvaro.FRAN) = 2
              AND MONTH(franvaro.TILL) = 2 THEN  antal = antal.
              ELSE IF MONTH(franvaro.FRAN) > man AND MONTH(franvaro.TILL) > man THEN  antal = antal.
              ELSE DO:
                nytid = franvaro.TIMMAR.
                RUN TIMSEK.P.
                franvaro.TIMMAR = sekunder / 36.
                rapphj1 = STRING(franvaro.TIMMAR,"999999").  
                overrapp1 = ftag + " " + kolle +  " " + SUBSTRING(franvaro.PERSONALKOD,1 ,5) +  " " +
                SUBSTRING(franvaro.LART,1,3) +  
                " " + SUBSTRING(rapphj1,1 ,6)  +  
                " " + "000000"  + " " + "0000000" +
                " " + "000000000" + " " +
                SUBSTRING(STRING(franvaro.FRAN),1 ,2) +
                SUBSTRING(STRING(franvaro.FRAN),4 ,2) +
                SUBSTRING(STRING(franvaro.FRAN),7 ,2) + " " +
                SUBSTRING(STRING(franvaro.TILL),1 ,2) +
                SUBSTRING(STRING(franvaro.TILL),4 ,2) +  
                SUBSTRING(STRING(franvaro.TILL),7 ,2) + " " +
                "000" + " " + "0000" + " " + "      " + " " + "EL".
                IF franvaro.LART = "530" THEN DO:
                   PUT STREAM frantj overrapp1 SKIP.  
                END.   
                PUT STREAM frlotj overrapp1 SKIP.             
                DELETE franvaro. 
              END.
           END.
           ELSE DO:   
              nytid = franvaro.TIMMAR.
              RUN TIMSEK.P.
              franvaro.TIMMAR = sekunder / 36.
              rapphj1 = STRING(franvaro.TIMMAR,"999999").  
              overrapp1 = ftag + " " + kolle +  " " + SUBSTRING(franvaro.PERSONALKOD,1 ,5) +  " " +
              SUBSTRING(franvaro.LART,1,3) +  
              " " + SUBSTRING(rapphj1,1 ,6)  +  
              " " + "000000"  + " " + "0000000" +
              " " + "000000000" + " " +
              SUBSTRING(STRING(franvaro.FRAN),1 ,2) +
              SUBSTRING(STRING(franvaro.FRAN),4 ,2) +
              SUBSTRING(STRING(franvaro.FRAN),7 ,2) + " " +
              SUBSTRING(STRING(franvaro.TILL),1 ,2) +
              SUBSTRING(STRING(franvaro.TILL),4 ,2) +  
              SUBSTRING(STRING(franvaro.TILL),7 ,2) + " " +
              "000" + " " + "0000" + " " + "      " + " " + "EL".
              IF franvaro.LART = "530" THEN DO:
                 PUT STREAM frantj overrapp1 SKIP.  
              END.   
              PUT STREAM frlotj overrapp1 SKIP.             
              DELETE franvaro.       
           END.
        END.
      END.
      REPEAT:
        IF Guru.Konstanter:globforetag = "GRAN" THEN DO:               
           FIND NEXT franvaro WHERE franvaro.PERSONALKOD BEGINS "45" USE-INDEX franvaro EXCLUSIVE-LOCK NO-ERROR.
           IF franvaro.PERSONALKOD = "45086" THEN NEXT.
           kolle = "05".
        END.
        IF Guru.Konstanter:globforetag = "GADM" THEN DO:               
           FIND NEXT franvaro WHERE franvaro.VIJUDID = "GRAEM" USE-INDEX franvaro EXCLUSIVE-LOCK NO-ERROR.                  
           IF franvaro.VIJUDID = "GRAEM" THEN kolle = "03".
        END.
        IF Guru.Konstanter:globforetag = "GKRVA" THEN DO:               
           FIND NEXT franvaro WHERE franvaro.VIJUDID = "GVARME" USE-INDEX franvaro EXCLUSIVE-LOCK NO-ERROR.                  
           IF franvaro.VIJUDID = "GVARME" THEN kolle = "07".
        END.
        IF NOT AVAILABLE franvaro THEN LEAVE.
        IF AVAILABLE franvaro THEN DO:
          IF gvisatidpermanad = FALSE THEN DO:
             IF man = 12 AND MONTH(franvaro.FRAN) < 6
             AND MONTH(franvaro.TILL) < 6 THEN NEXT.
             ELSE IF man = 1 AND MONTH(franvaro.FRAN) = 2
             AND MONTH(franvaro.TILL) = 2 THEN NEXT.
             ELSE IF MONTH(franvaro.FRAN) > man AND MONTH(franvaro.TILL) > man THEN NEXT.
             ELSE DO:
               nytid = franvaro.TIMMAR.
               RUN TIMSEK.P.
               franvaro.TIMMAR = sekunder / 36.
               rapphj1 = STRING(franvaro.TIMMAR,"999999").  
               overrapp1 = ftag + " " + kolle + " " + SUBSTRING(franvaro.PERSONALKOD,1 ,5) + " " +
               SUBSTRING(franvaro.LART,1,3) +   
               " " + SUBSTRING(rapphj1,1 ,6) +  
               " " + "000000" + " "  + "0000000" +
               " " + "000000000" + " " +
               SUBSTRING(STRING(franvaro.FRAN),1 ,2) +
               SUBSTRING(STRING(franvaro.FRAN),4 ,2) +
               SUBSTRING(STRING(franvaro.FRAN),7 ,2) + " " +
               SUBSTRING(STRING(franvaro.TILL),1 ,2) +
               SUBSTRING(STRING(franvaro.TILL),4 ,2) +
               SUBSTRING(STRING(franvaro.TILL),7 ,2) + " " +
               "000" + " " + "0000" + " " + "      " + " " + "EL".  
               IF franvaro.LART = "530" THEN DO:
                  PUT STREAM frantj overrapp1 SKIP. 
               END.   
               PUT STREAM frlotj overrapp1 SKIP.           
               DELETE franvaro.
             END.
          END.
          ELSE DO:   
             nytid = franvaro.TIMMAR.
             RUN TIMSEK.P.
             franvaro.TIMMAR = sekunder / 36.
             rapphj1 = STRING(franvaro.TIMMAR,"999999").  
             overrapp1 = ftag + " " + kolle + " " + SUBSTRING(franvaro.PERSONALKOD,1 ,5) + " " +
             SUBSTRING(franvaro.LART,1,3) +   
             " " + SUBSTRING(rapphj1,1 ,6) +  
             " " + "000000" + " "  + "0000000" +
             " " + "000000000" + " " +
             SUBSTRING(STRING(franvaro.FRAN),1 ,2) +
             SUBSTRING(STRING(franvaro.FRAN),4 ,2) +
             SUBSTRING(STRING(franvaro.FRAN),7 ,2) + " " +
             SUBSTRING(STRING(franvaro.TILL),1 ,2) +
             SUBSTRING(STRING(franvaro.TILL),4 ,2) +
             SUBSTRING(STRING(franvaro.TILL),7 ,2) + " " +
             "000" + " " + "0000" + " " + "      " + " " + "EL".  
             IF franvaro.LART = "530" THEN DO:
                PUT STREAM frantj overrapp1 SKIP. 
             END.   
             PUT STREAM frlotj overrapp1 SKIP.           
             DELETE franvaro.            
          END.
        END.
      END.         
   END.   
END.                        
IF Guru.Konstanter:globforetag = "GRAN"  OR Guru.Konstanter:globforetag = "cELPA"  THEN DO:
   OUTPUT STREAM frantj CLOSE.
   OUTPUT STREAM frlotj CLOSE.
END.

IF Guru.Konstanter:globforetag = "GRAN"  THEN DO:
   OUTPUT STREAM frantj TO \\GRANGURU\guru_ser\server\PRO9S\gran\frsyd.d NO-ECHO.  
   OUTPUT STREAM frlosy TO \\GRANGURU\guru_ser\server\PRO9S\gran\losyd.d APPEND.
END.                                             
ELSE IF Guru.Konstanter:globforetag = "GADM" THEN DO:
   OUTPUT STREAM frantj TO \\GRANGURU\guru_ser\server\PRO9S\gadm\frtrad.d NO-ECHO.  
   OUTPUT STREAM frlosy TO \\GRANGURU\guru_ser\server\PRO9S\gadm\lotrad.d APPEND.
END.
ELSE IF Guru.Konstanter:globforetag = "GRIT" THEN nytid = nytid.
ELSE IF Guru.Konstanter:globforetag = "GKAL" THEN nytid = nytid. 
ELSE IF Guru.Konstanter:globforetag = "GSYD" THEN nytid = nytid.
ELSE IF Guru.Konstanter:globforetag = "GKRVA" THEN nytid = nytid. 
ELSE IF Guru.Konstanter:globforetag = "cELPA" THEN DO:
   OUTPUT STREAM frantj TO \\pc112\delad\pro9s\korning\frsyd.d NO-ECHO.
   OUTPUT STREAM frlosy TO \\pc12\delad\pro9s\korning\losyd.d APPEND.
END.  
DO TRANSACTION:
   IF Guru.Konstanter:globforetag = "GRAN" OR Guru.Konstanter:globforetag = "cELPA" THEN DO:
      FIND LAST franvaro WHERE franvaro.PERSONALKOD BEGINS "31" USE-INDEX franvaro NO-LOCK NO-ERROR.
      reco = RECID(franvaro).                       
      FIND FIRST franvaro  WHERE franvaro.PERSONALKOD BEGINS "31" USE-INDEX franvaro EXCLUSIVE-LOCK NO-ERROR.
      /*IF NOT AVAILABLE franvaro THEN LEAVE.*/
      IF AVAILABLE franvaro THEN DO:
        IF gvisatidpermanad = FALSE THEN DO:
           IF man = 12 AND MONTH(franvaro.FRAN) < 10
           AND MONTH(franvaro.TILL) < 10 THEN  antal = antal.
           ELSE IF man = 1 AND MONTH(franvaro.FRAN) = 2
           AND MONTH(franvaro.TILL) = 2 THEN  antal = antal.
           ELSE IF MONTH(franvaro.FRAN) > man AND MONTH(franvaro.TILL) > man THEN  antal = antal.
           ELSE DO:
             nytid = franvaro.TIMMAR.
             RUN TIMSEK.P.
             franvaro.TIMMAR = sekunder / 36.
             rapphj1 = STRING(franvaro.TIMMAR,"999999").  
             /*tidigare 4444 01*/
             overrapp1 = "8888" + " " + "04" +  " " + SUBSTRING(franvaro.PERSONALKOD,1 ,5) +  " " +
             SUBSTRING(franvaro.LART,1,3) +  
             " " + SUBSTRING(rapphj1,1 ,6)  +  
             " " + "000000"  + " " + "0000000" +
             " " + "000000000" + " " +
             SUBSTRING(STRING(franvaro.FRAN),1 ,2) +
             SUBSTRING(STRING(franvaro.FRAN),4 ,2) +
             SUBSTRING(STRING(franvaro.FRAN),7 ,2) + " " +
             SUBSTRING(STRING(franvaro.TILL),1 ,2) +
             SUBSTRING(STRING(franvaro.TILL),4 ,2) +  
             SUBSTRING(STRING(franvaro.TILL),7 ,2) + " " +
             "000" + " " + "0000" + " " + "      " + " " + "EL".
             IF franvaro.LART = "530" THEN DO:
                PUT STREAM frantj overrapp1 SKIP.  
             END.   
             PUT STREAM frlosy overrapp1 SKIP.             
             DELETE franvaro. 
           END.
        END.
        ELSE DO:   
           nytid = franvaro.TIMMAR.
           RUN TIMSEK.P.
           franvaro.TIMMAR = sekunder / 36.
           rapphj1 = STRING(franvaro.TIMMAR,"999999").  
           /*tidigare 4444 01*/
           overrapp1 = "8888" + " " + "04" +  " " + SUBSTRING(franvaro.PERSONALKOD,1 ,5) +  " " +
           SUBSTRING(franvaro.LART,1,3) +  
           " " + SUBSTRING(rapphj1,1 ,6)  +  
           " " + "000000"  + " " + "0000000" +
           " " + "000000000" + " " +
           SUBSTRING(STRING(franvaro.FRAN),1 ,2) +
           SUBSTRING(STRING(franvaro.FRAN),4 ,2) +
           SUBSTRING(STRING(franvaro.FRAN),7 ,2) + " " +
           SUBSTRING(STRING(franvaro.TILL),1 ,2) +
           SUBSTRING(STRING(franvaro.TILL),4 ,2) +  
           SUBSTRING(STRING(franvaro.TILL),7 ,2) + " " +
           "000" + " " + "0000" + " " + "      " + " " + "EL".
           IF franvaro.LART = "530" THEN DO:
              PUT STREAM frantj overrapp1 SKIP.  
           END.   
           PUT STREAM frlosy overrapp1 SKIP.             
           DELETE franvaro.       
        END.
      END.
      REPEAT:
        FIND NEXT franvaro WHERE franvaro.PERSONALKOD BEGINS "31" USE-INDEX franvaro EXCLUSIVE-LOCK NO-ERROR.
        IF NOT AVAILABLE franvaro THEN LEAVE.
        IF AVAILABLE franvaro THEN DO:
          IF gvisatidpermanad = FALSE THEN DO:
             IF man = 12 AND MONTH(franvaro.FRAN) < 6
             AND MONTH(franvaro.TILL) < 6 THEN NEXT.
             ELSE IF man = 1 AND MONTH(franvaro.FRAN) = 2
             AND MONTH(franvaro.TILL) = 2 THEN NEXT.
             ELSE IF MONTH(franvaro.FRAN) > man AND MONTH(franvaro.TILL) > man THEN NEXT.
             ELSE DO:
               nytid = franvaro.TIMMAR.
               RUN TIMSEK.P.
               franvaro.TIMMAR = sekunder / 36.
               rapphj1 = STRING(franvaro.TIMMAR,"999999").  
               /*tidigare 4444 01*/
               overrapp1 = "8888" + " " + "04" + " " + SUBSTRING(franvaro.PERSONALKOD,1 ,5) + " " +
               SUBSTRING(franvaro.LART,1,3) +   
               " " + SUBSTRING(rapphj1,1 ,6) +  
               " " + "000000" + " "  + "0000000" +
               " " + "000000000" + " " +
               SUBSTRING(STRING(franvaro.FRAN),1 ,2) +
               SUBSTRING(STRING(franvaro.FRAN),4 ,2) +
               SUBSTRING(STRING(franvaro.FRAN),7 ,2) + " " +
               SUBSTRING(STRING(franvaro.TILL),1 ,2) +
               SUBSTRING(STRING(franvaro.TILL),4 ,2) +
               SUBSTRING(STRING(franvaro.TILL),7 ,2) + " " +
               "000" + " " + "0000" + " " + "      " + " " + "EL".  
               IF franvaro.LART = "530" THEN DO:
                  PUT STREAM frantj overrapp1 SKIP. 
               END.   
               PUT STREAM frlosy overrapp1 SKIP.           
               DELETE franvaro.
             END.
          END.
          ELSE DO:   
             nytid = franvaro.TIMMAR.
             RUN TIMSEK.P.
             franvaro.TIMMAR = sekunder / 36.
             rapphj1 = STRING(franvaro.TIMMAR,"999999").  
             /*tidigare 4444 01*/
             overrapp1 = "8888" + " " + "04" + " " + SUBSTRING(franvaro.PERSONALKOD,1 ,5) + " " +
             SUBSTRING(franvaro.LART,1,3) +   
             " " + SUBSTRING(rapphj1,1 ,6) +  
             " " + "000000" + " "  + "0000000" +
             " " + "000000000" + " " +
             SUBSTRING(STRING(franvaro.FRAN),1 ,2) +
             SUBSTRING(STRING(franvaro.FRAN),4 ,2) +
             SUBSTRING(STRING(franvaro.FRAN),7 ,2) + " " +
             SUBSTRING(STRING(franvaro.TILL),1 ,2) +
             SUBSTRING(STRING(franvaro.TILL),4 ,2) +
             SUBSTRING(STRING(franvaro.TILL),7 ,2) + " " +
             "000" + " " + "0000" + " " + "      " + " " + "EL".  
             IF franvaro.LART = "530" THEN DO:
                PUT STREAM frantj overrapp1 SKIP. 
             END.   
             PUT STREAM frlosy overrapp1 SKIP.           
             DELETE franvaro.            
          END.
        END.
      END.         
   END.   
   IF Guru.Konstanter:globforetag = "GRAN" OR Guru.Konstanter:globforetag = "cELPA" THEN DO:
      FIND LAST franvaro WHERE franvaro.PERSONALKOD BEGINS "41" USE-INDEX franvaro NO-LOCK NO-ERROR.
      reco = RECID(franvaro).                       
      FIND FIRST franvaro  WHERE franvaro.PERSONALKOD BEGINS "41" USE-INDEX franvaro EXCLUSIVE-LOCK NO-ERROR.
      /*IF NOT AVAILABLE franvaro THEN LEAVE.*/
      IF AVAILABLE franvaro THEN DO:
        IF gvisatidpermanad = FALSE THEN DO:
           IF man = 12 AND MONTH(franvaro.FRAN) < 10
           AND MONTH(franvaro.TILL) < 10 THEN  antal = antal.
           ELSE IF man = 1 AND MONTH(franvaro.FRAN) = 2
           AND MONTH(franvaro.TILL) = 2 THEN  antal = antal.
           ELSE IF MONTH(franvaro.FRAN) > man AND MONTH(franvaro.TILL) > man THEN  antal = antal.
           ELSE DO:
             nytid = franvaro.TIMMAR.
             RUN TIMSEK.P.
             franvaro.TIMMAR = sekunder / 36.
             rapphj1 = STRING(franvaro.TIMMAR,"999999").  
             /*tidigare 4444 01*/
             overrapp1 = "8888" + " " + "04" +  " " + SUBSTRING(franvaro.PERSONALKOD,1 ,5) +  " " +
             SUBSTRING(franvaro.LART,1,3) +  
             " " + SUBSTRING(rapphj1,1 ,6)  +  
             " " + "000000"  + " " + "0000000" +
             " " + "000000000" + " " +
             SUBSTRING(STRING(franvaro.FRAN),1 ,2) +
             SUBSTRING(STRING(franvaro.FRAN),4 ,2) +
             SUBSTRING(STRING(franvaro.FRAN),7 ,2) + " " +
             SUBSTRING(STRING(franvaro.TILL),1 ,2) +
             SUBSTRING(STRING(franvaro.TILL),4 ,2) +  
             SUBSTRING(STRING(franvaro.TILL),7 ,2) + " " +
             "000" + " " + "0000" + " " + "      " + " " + "EL".
             IF franvaro.LART = "530" THEN DO:
                PUT STREAM frantj overrapp1 SKIP.  
             END.   
             PUT STREAM frlosy overrapp1 SKIP.             
             DELETE franvaro. 
           END.
        END.
        ELSE DO:   
           nytid = franvaro.TIMMAR.
           RUN TIMSEK.P.
           franvaro.TIMMAR = sekunder / 36.
           rapphj1 = STRING(franvaro.TIMMAR,"999999").  
           /*tidigare 4444 01*/
           overrapp1 = "8888" + " " + "04" +  " " + SUBSTRING(franvaro.PERSONALKOD,1 ,5) +  " " +
           SUBSTRING(franvaro.LART,1,3) +  
           " " + SUBSTRING(rapphj1,1 ,6)  +  
           " " + "000000"  + " " + "0000000" +
           " " + "000000000" + " " +
           SUBSTRING(STRING(franvaro.FRAN),1 ,2) +
           SUBSTRING(STRING(franvaro.FRAN),4 ,2) +
           SUBSTRING(STRING(franvaro.FRAN),7 ,2) + " " +
           SUBSTRING(STRING(franvaro.TILL),1 ,2) +
           SUBSTRING(STRING(franvaro.TILL),4 ,2) +  
           SUBSTRING(STRING(franvaro.TILL),7 ,2) + " " +
           "000" + " " + "0000" + " " + "      " + " " + "EL".
           IF franvaro.LART = "530" THEN DO:
              PUT STREAM frantj overrapp1 SKIP.  
           END.   
           PUT STREAM frlosy overrapp1 SKIP.             
           DELETE franvaro.       
        END.
      END.
      REPEAT:
        FIND NEXT franvaro WHERE franvaro.PERSONALKOD BEGINS "41" USE-INDEX franvaro EXCLUSIVE-LOCK NO-ERROR.
        IF NOT AVAILABLE franvaro THEN LEAVE.
        IF AVAILABLE franvaro THEN DO:
          IF gvisatidpermanad = FALSE THEN DO:
             IF man = 12 AND MONTH(franvaro.FRAN) < 6
             AND MONTH(franvaro.TILL) < 6 THEN NEXT.
             ELSE IF man = 1 AND MONTH(franvaro.FRAN) = 2
             AND MONTH(franvaro.TILL) = 2 THEN NEXT.
             ELSE IF MONTH(franvaro.FRAN) > man AND MONTH(franvaro.TILL) > man THEN NEXT.
             ELSE DO:
               nytid = franvaro.TIMMAR.
               RUN TIMSEK.P.
               franvaro.TIMMAR = sekunder / 36.
               rapphj1 = STRING(franvaro.TIMMAR,"999999").  
               /*tidigare 4444 01*/
               overrapp1 = "8888" + " " + "04" + " " + SUBSTRING(franvaro.PERSONALKOD,1 ,5) + " " +
               SUBSTRING(franvaro.LART,1,3) +   
               " " + SUBSTRING(rapphj1,1 ,6) +  
               " " + "000000" + " "  + "0000000" +
               " " + "000000000" + " " +
               SUBSTRING(STRING(franvaro.FRAN),1 ,2) +
               SUBSTRING(STRING(franvaro.FRAN),4 ,2) +
               SUBSTRING(STRING(franvaro.FRAN),7 ,2) + " " +
               SUBSTRING(STRING(franvaro.TILL),1 ,2) +
               SUBSTRING(STRING(franvaro.TILL),4 ,2) +
               SUBSTRING(STRING(franvaro.TILL),7 ,2) + " " +
               "000" + " " + "0000" + " " + "      " + " " + "EL".  
               IF franvaro.LART = "530" THEN DO:
                  PUT STREAM frantj overrapp1 SKIP. 
               END.   
               PUT STREAM frlosy overrapp1 SKIP.           
               DELETE franvaro.
             END.
          END.
          ELSE DO:   
             nytid = franvaro.TIMMAR.
             RUN TIMSEK.P.
             franvaro.TIMMAR = sekunder / 36.
             rapphj1 = STRING(franvaro.TIMMAR,"999999").  
             /*tidigare 4444 01*/
             overrapp1 = "8888" + " " + "04" + " " + SUBSTRING(franvaro.PERSONALKOD,1 ,5) + " " +
             SUBSTRING(franvaro.LART,1,3) +   
             " " + SUBSTRING(rapphj1,1 ,6) +  
             " " + "000000" + " "  + "0000000" +
             " " + "000000000" + " " +
             SUBSTRING(STRING(franvaro.FRAN),1 ,2) +
             SUBSTRING(STRING(franvaro.FRAN),4 ,2) +
             SUBSTRING(STRING(franvaro.FRAN),7 ,2) + " " +
             SUBSTRING(STRING(franvaro.TILL),1 ,2) +
             SUBSTRING(STRING(franvaro.TILL),4 ,2) +
             SUBSTRING(STRING(franvaro.TILL),7 ,2) + " " +
             "000" + " " + "0000" + " " + "      " + " " + "EL".  
             IF franvaro.LART = "530" THEN DO:
                PUT STREAM frantj overrapp1 SKIP. 
             END.   
             PUT STREAM frlosy overrapp1 SKIP.           
             DELETE franvaro.            
          END.
        END.
      END.         
   END.   

   IF Guru.Konstanter:globforetag = "GRAN" OR Guru.Konstanter:globforetag = "cELPA" THEN DO:
      FIND LAST franvaro WHERE franvaro.PERSONALKOD BEGINS "24" USE-INDEX franvaro NO-LOCK NO-ERROR.
      reco = RECID(franvaro).                       
      FIND FIRST franvaro  WHERE franvaro.PERSONALKOD BEGINS "24" USE-INDEX franvaro EXCLUSIVE-LOCK NO-ERROR.
      /*IF NOT AVAILABLE franvaro THEN LEAVE.*/
      IF AVAILABLE franvaro THEN DO:
        IF gvisatidpermanad = FALSE THEN DO:
           IF man = 12 AND MONTH(franvaro.FRAN) < 10
           AND MONTH(franvaro.TILL) < 10 THEN  antal = antal.
           ELSE IF man = 1 AND MONTH(franvaro.FRAN) = 2
           AND MONTH(franvaro.TILL) = 2 THEN  antal = antal.
           ELSE IF MONTH(franvaro.FRAN) > man AND MONTH(franvaro.TILL) > man THEN  antal = antal.
           ELSE DO:
             nytid = franvaro.TIMMAR.
             RUN TIMSEK.P.
             franvaro.TIMMAR = sekunder / 36.
             rapphj1 = STRING(franvaro.TIMMAR,"999999").  
             /*tidigare 4444 01*/
             overrapp1 = "8888" + " " + "04" +  " " + SUBSTRING(franvaro.PERSONALKOD,1 ,5) +  " " +
             SUBSTRING(franvaro.LART,1,3) +  
             " " + SUBSTRING(rapphj1,1 ,6)  +  
             " " + "000000"  + " " + "0000000" +
             " " + "000000000" + " " +
             SUBSTRING(STRING(franvaro.FRAN),1 ,2) +
             SUBSTRING(STRING(franvaro.FRAN),4 ,2) +
             SUBSTRING(STRING(franvaro.FRAN),7 ,2) + " " +
             SUBSTRING(STRING(franvaro.TILL),1 ,2) +
             SUBSTRING(STRING(franvaro.TILL),4 ,2) +  
             SUBSTRING(STRING(franvaro.TILL),7 ,2) + " " +
             "000" + " " + "0000" + " " + "      " + " " + "EL".
             IF franvaro.LART = "530" THEN DO:
                PUT STREAM frantj overrapp1 SKIP.  
             END.   
             PUT STREAM frlosy overrapp1 SKIP.             
             DELETE franvaro. 
           END.
        END.
        ELSE DO:   
           nytid = franvaro.TIMMAR.
           RUN TIMSEK.P.
           franvaro.TIMMAR = sekunder / 36.
           rapphj1 = STRING(franvaro.TIMMAR,"999999").  
           /*tidigare 4444 01*/
           overrapp1 = "8888" + " " + "04" +  " " + SUBSTRING(franvaro.PERSONALKOD,1 ,5) +  " " +
           SUBSTRING(franvaro.LART,1,3) +  
           " " + SUBSTRING(rapphj1,1 ,6)  +  
           " " + "000000"  + " " + "0000000" +
           " " + "000000000" + " " +
           SUBSTRING(STRING(franvaro.FRAN),1 ,2) +
           SUBSTRING(STRING(franvaro.FRAN),4 ,2) +
           SUBSTRING(STRING(franvaro.FRAN),7 ,2) + " " +
           SUBSTRING(STRING(franvaro.TILL),1 ,2) +
           SUBSTRING(STRING(franvaro.TILL),4 ,2) +  
           SUBSTRING(STRING(franvaro.TILL),7 ,2) + " " +
           "000" + " " + "0000" + " " + "      " + " " + "EL".
           IF franvaro.LART = "530" THEN DO:
              PUT STREAM frantj overrapp1 SKIP.  
           END.   
           PUT STREAM frlosy overrapp1 SKIP.             
           DELETE franvaro.       
        END.
      END.
      REPEAT:
        FIND NEXT franvaro WHERE franvaro.PERSONALKOD BEGINS "24" USE-INDEX franvaro EXCLUSIVE-LOCK NO-ERROR.
        IF NOT AVAILABLE franvaro THEN LEAVE.
        IF AVAILABLE franvaro THEN DO:
          IF gvisatidpermanad = FALSE THEN DO:
             IF man = 12 AND MONTH(franvaro.FRAN) < 6
             AND MONTH(franvaro.TILL) < 6 THEN NEXT.
             ELSE IF man = 1 AND MONTH(franvaro.FRAN) = 2
             AND MONTH(franvaro.TILL) = 2 THEN NEXT.
             ELSE IF MONTH(franvaro.FRAN) > man AND MONTH(franvaro.TILL) > man THEN NEXT.
             ELSE DO:
               nytid = franvaro.TIMMAR.
               RUN TIMSEK.P.
               franvaro.TIMMAR = sekunder / 36.
               rapphj1 = STRING(franvaro.TIMMAR,"999999").  
               /*tidigare 4444 01*/
               overrapp1 = "8888" + " " + "04" + " " + SUBSTRING(franvaro.PERSONALKOD,1 ,5) + " " +
               SUBSTRING(franvaro.LART,1,3) +   
               " " + SUBSTRING(rapphj1,1 ,6) +  
               " " + "000000" + " "  + "0000000" +
               " " + "000000000" + " " +
               SUBSTRING(STRING(franvaro.FRAN),1 ,2) +
               SUBSTRING(STRING(franvaro.FRAN),4 ,2) +
               SUBSTRING(STRING(franvaro.FRAN),7 ,2) + " " +
               SUBSTRING(STRING(franvaro.TILL),1 ,2) +
               SUBSTRING(STRING(franvaro.TILL),4 ,2) +
               SUBSTRING(STRING(franvaro.TILL),7 ,2) + " " +
               "000" + " " + "0000" + " " + "      " + " " + "EL".  
               IF franvaro.LART = "530" THEN DO:
                  PUT STREAM frantj overrapp1 SKIP. 
               END.   
               PUT STREAM frlosy overrapp1 SKIP.           
               DELETE franvaro.
             END.
          END.
          ELSE DO:   
             nytid = franvaro.TIMMAR.
             RUN TIMSEK.P.
             franvaro.TIMMAR = sekunder / 36.
             rapphj1 = STRING(franvaro.TIMMAR,"999999").  
             /*tidigare 4444 01*/
             overrapp1 = "8888" + " " + "04" + " " + SUBSTRING(franvaro.PERSONALKOD,1 ,5) + " " +
             SUBSTRING(franvaro.LART,1,3) +   
             " " + SUBSTRING(rapphj1,1 ,6) +  
             " " + "000000" + " "  + "0000000" +
             " " + "000000000" + " " +
             SUBSTRING(STRING(franvaro.FRAN),1 ,2) +
             SUBSTRING(STRING(franvaro.FRAN),4 ,2) +
             SUBSTRING(STRING(franvaro.FRAN),7 ,2) + " " +
             SUBSTRING(STRING(franvaro.TILL),1 ,2) +
             SUBSTRING(STRING(franvaro.TILL),4 ,2) +
             SUBSTRING(STRING(franvaro.TILL),7 ,2) + " " +
             "000" + " " + "0000" + " " + "      " + " " + "EL".  
             IF franvaro.LART = "530" THEN DO:
                PUT STREAM frantj overrapp1 SKIP. 
             END.   
             PUT STREAM frlosy overrapp1 SKIP.           
             DELETE franvaro.            
          END.
        END.
      END.         
   END.   
   
   IF Guru.Konstanter:globforetag = "GRAN" OR Guru.Konstanter:globforetag = "cELPA" THEN DO:
      /*special för Kenneth Gebing 45086*/
      FIND LAST franvaro WHERE franvaro.PERSONALKOD = "45086"  USE-INDEX franvaro NO-LOCK NO-ERROR.
      reco = RECID(franvaro).                       
      FIND FIRST franvaro  WHERE franvaro.PERSONALKOD = "45086" USE-INDEX franvaro EXCLUSIVE-LOCK NO-ERROR.
      /*IF NOT AVAILABLE franvaro THEN LEAVE.*/
      IF AVAILABLE franvaro THEN DO:
        IF gvisatidpermanad = FALSE THEN DO:
           IF man = 12 AND MONTH(franvaro.FRAN) < 10
           AND MONTH(franvaro.TILL) < 10 THEN  antal = antal.
           ELSE IF man = 1 AND MONTH(franvaro.FRAN) = 2
           AND MONTH(franvaro.TILL) = 2 THEN  antal = antal.
           ELSE IF MONTH(franvaro.FRAN) > man AND MONTH(franvaro.TILL) > man THEN  antal = antal.
           ELSE DO:
             nytid = franvaro.TIMMAR.
             RUN TIMSEK.P.
             franvaro.TIMMAR = sekunder / 36.
             rapphj1 = STRING(franvaro.TIMMAR,"999999").  
             /*tidigare 4444 01*/
             overrapp1 = "8888" + " " + "04" +  " " + SUBSTRING(franvaro.PERSONALKOD,1 ,5) +  " " +
             SUBSTRING(franvaro.LART,1,3) +  
             " " + SUBSTRING(rapphj1,1 ,6)  +  
             " " + "000000"  + " " + "0000000" +
             " " + "000000000" + " " +
             SUBSTRING(STRING(franvaro.FRAN),1 ,2) +
             SUBSTRING(STRING(franvaro.FRAN),4 ,2) +
             SUBSTRING(STRING(franvaro.FRAN),7 ,2) + " " +
             SUBSTRING(STRING(franvaro.TILL),1 ,2) +
             SUBSTRING(STRING(franvaro.TILL),4 ,2) +  
             SUBSTRING(STRING(franvaro.TILL),7 ,2) + " " +
             "000" + " " + "0000" + " " + "      " + " " + "EL".
             IF franvaro.LART = "530" THEN DO:
                PUT STREAM frantj overrapp1 SKIP.  
             END.   
             PUT STREAM frlosy overrapp1 SKIP.             
             DELETE franvaro. 
           END.
        END.
        ELSE DO:   
           nytid = franvaro.TIMMAR.
           RUN TIMSEK.P.
           franvaro.TIMMAR = sekunder / 36.
           rapphj1 = STRING(franvaro.TIMMAR,"999999").  
           /*tidigare 4444 01*/
           overrapp1 = "8888" + " " + "04" +  " " + SUBSTRING(franvaro.PERSONALKOD,1 ,5) +  " " +
           SUBSTRING(franvaro.LART,1,3) +  
           " " + SUBSTRING(rapphj1,1 ,6)  +  
           " " + "000000"  + " " + "0000000" +
           " " + "000000000" + " " +
           SUBSTRING(STRING(franvaro.FRAN),1 ,2) +
           SUBSTRING(STRING(franvaro.FRAN),4 ,2) +
           SUBSTRING(STRING(franvaro.FRAN),7 ,2) + " " +
           SUBSTRING(STRING(franvaro.TILL),1 ,2) +
           SUBSTRING(STRING(franvaro.TILL),4 ,2) +  
           SUBSTRING(STRING(franvaro.TILL),7 ,2) + " " +
           "000" + " " + "0000" + " " + "      " + " " + "EL".
           IF franvaro.LART = "530" THEN DO:
              PUT STREAM frantj overrapp1 SKIP.  
           END.   
           PUT STREAM frlosy overrapp1 SKIP.             
           DELETE franvaro.       
        END.
      END.
      REPEAT:
        FIND NEXT franvaro WHERE franvaro.PERSONALKOD = "45086" USE-INDEX franvaro EXCLUSIVE-LOCK NO-ERROR.
        IF NOT AVAILABLE franvaro THEN LEAVE.
        IF AVAILABLE franvaro THEN DO:
          IF gvisatidpermanad = FALSE THEN DO:
             IF man = 12 AND MONTH(franvaro.FRAN) < 6
             AND MONTH(franvaro.TILL) < 6 THEN NEXT.
             ELSE IF man = 1 AND MONTH(franvaro.FRAN) = 2
             AND MONTH(franvaro.TILL) = 2 THEN NEXT.
             ELSE IF MONTH(franvaro.FRAN) > man AND MONTH(franvaro.TILL) > man THEN NEXT.
             ELSE DO:
               nytid = franvaro.TIMMAR.
               RUN TIMSEK.P.
               franvaro.TIMMAR = sekunder / 36.
               rapphj1 = STRING(franvaro.TIMMAR,"999999").  
               /*tidigare 4444 01*/
               overrapp1 = "8888" + " " + "04" + " " + SUBSTRING(franvaro.PERSONALKOD,1 ,5) + " " +
               SUBSTRING(franvaro.LART,1,3) +   
               " " + SUBSTRING(rapphj1,1 ,6) +  
               " " + "000000" + " "  + "0000000" +
               " " + "000000000" + " " +
               SUBSTRING(STRING(franvaro.FRAN),1 ,2) +
               SUBSTRING(STRING(franvaro.FRAN),4 ,2) +
               SUBSTRING(STRING(franvaro.FRAN),7 ,2) + " " +
               SUBSTRING(STRING(franvaro.TILL),1 ,2) +
               SUBSTRING(STRING(franvaro.TILL),4 ,2) +
               SUBSTRING(STRING(franvaro.TILL),7 ,2) + " " +
               "000" + " " + "0000" + " " + "      " + " " + "EL".  
               IF franvaro.LART = "530" THEN DO:
                  PUT STREAM frantj overrapp1 SKIP. 
               END.   
               PUT STREAM frlosy overrapp1 SKIP.           
               DELETE franvaro.
             END.
          END.
          ELSE DO:   
             nytid = franvaro.TIMMAR.
             RUN TIMSEK.P.
             franvaro.TIMMAR = sekunder / 36.
             rapphj1 = STRING(franvaro.TIMMAR,"999999").  
             /*tidigare 4444 01*/
             overrapp1 = "8888" + " " + "04" + " " + SUBSTRING(franvaro.PERSONALKOD,1 ,5) + " " +
             SUBSTRING(franvaro.LART,1,3) +   
             " " + SUBSTRING(rapphj1,1 ,6) +  
             " " + "000000" + " "  + "0000000" +
             " " + "000000000" + " " +
             SUBSTRING(STRING(franvaro.FRAN),1 ,2) +
             SUBSTRING(STRING(franvaro.FRAN),4 ,2) +
             SUBSTRING(STRING(franvaro.FRAN),7 ,2) + " " +
             SUBSTRING(STRING(franvaro.TILL),1 ,2) +
             SUBSTRING(STRING(franvaro.TILL),4 ,2) +
             SUBSTRING(STRING(franvaro.TILL),7 ,2) + " " +
             "000" + " " + "0000" + " " + "      " + " " + "EL".  
             IF franvaro.LART = "530" THEN DO:
                PUT STREAM frantj overrapp1 SKIP. 
             END.   
             PUT STREAM frlosy overrapp1 SKIP.           
             DELETE franvaro.            
          END.
        END.
      END.         
   END.   

   IF Guru.Konstanter:globforetag = "GADM" OR Guru.Konstanter:globforetag = "cELPA" THEN DO:      
      /* GRATR utgår, ny SYDSP*/
      FIND LAST franvaro WHERE franvaro.VIJUDID = "SYDSP" USE-INDEX franvaro NO-LOCK NO-ERROR.
      reco = RECID(franvaro).                       
      FIND FIRST franvaro  WHERE franvaro.VIJUDID = "SYDSP" USE-INDEX franvaro EXCLUSIVE-LOCK NO-ERROR.
      /*IF NOT AVAILABLE franvaro THEN LEAVE.*/
      IF AVAILABLE franvaro THEN DO:
        IF gvisatidpermanad = FALSE THEN DO:
           IF man = 12 AND MONTH(franvaro.FRAN) < 10
           AND MONTH(franvaro.TILL) < 10 THEN  antal = antal.
           ELSE IF man = 1 AND MONTH(franvaro.FRAN) = 2
           AND MONTH(franvaro.TILL) = 2 THEN  antal = antal.
           ELSE IF MONTH(franvaro.FRAN) > man AND MONTH(franvaro.TILL) > man THEN  antal = antal.
           ELSE DO:
             nytid = franvaro.TIMMAR.
             RUN TIMSEK.P.
             franvaro.TIMMAR = sekunder / 36.
             rapphj1 = STRING(franvaro.TIMMAR,"999999").  
             /* tidigare 5555 01*/
             overrapp1 = "8888" + " " + "02" +  " " + SUBSTRING(franvaro.PERSONALKOD,1 ,5) +  " " +
             SUBSTRING(franvaro.LART,1,3) +  
             " " + SUBSTRING(rapphj1,1 ,6)  +  
             " " + "000000"  + " " + "0000000" +
             " " + "000000000" + " " +
             SUBSTRING(STRING(franvaro.FRAN),1 ,2) +
             SUBSTRING(STRING(franvaro.FRAN),4 ,2) +
             SUBSTRING(STRING(franvaro.FRAN),7 ,2) + " " +
             SUBSTRING(STRING(franvaro.TILL),1 ,2) +
             SUBSTRING(STRING(franvaro.TILL),4 ,2) +  
             SUBSTRING(STRING(franvaro.TILL),7 ,2) + " " +
             "000" + " " + "0000" + " " + "      " + " " + "EL".
             IF franvaro.LART = "530" THEN DO:
                PUT STREAM frantj overrapp1 SKIP.  
             END.   
             PUT STREAM frlosy overrapp1 SKIP.             
             DELETE franvaro. 
           END.
        END.
        ELSE DO:   
           nytid = franvaro.TIMMAR.
           RUN TIMSEK.P.
           franvaro.TIMMAR = sekunder / 36.
           rapphj1 = STRING(franvaro.TIMMAR,"999999").  
           /* tidigare 5555 01*/
           overrapp1 = "8888" + " " + "02" +  " " + SUBSTRING(franvaro.PERSONALKOD,1 ,5) +  " " +
           SUBSTRING(franvaro.LART,1,3) +  
           " " + SUBSTRING(rapphj1,1 ,6)  +  
           " " + "000000"  + " " + "0000000" +
           " " + "000000000" + " " +
           SUBSTRING(STRING(franvaro.FRAN),1 ,2) +
           SUBSTRING(STRING(franvaro.FRAN),4 ,2) +
           SUBSTRING(STRING(franvaro.FRAN),7 ,2) + " " +
           SUBSTRING(STRING(franvaro.TILL),1 ,2) +
           SUBSTRING(STRING(franvaro.TILL),4 ,2) +  
           SUBSTRING(STRING(franvaro.TILL),7 ,2) + " " +
           "000" + " " + "0000" + " " + "      " + " " + "EL".
           IF franvaro.LART = "530" THEN DO:
              PUT STREAM frantj overrapp1 SKIP.  
           END.   
           PUT STREAM frlosy overrapp1 SKIP.             
           DELETE franvaro.       
        END.
      END.
      REPEAT:
        FIND NEXT franvaro WHERE franvaro.VIJUDID = "SYDSP" USE-INDEX franvaro EXCLUSIVE-LOCK NO-ERROR.
        IF NOT AVAILABLE franvaro THEN LEAVE.
        IF AVAILABLE franvaro THEN DO:
          IF gvisatidpermanad = FALSE THEN DO:
             IF man = 12 AND MONTH(franvaro.FRAN) < 6
             AND MONTH(franvaro.TILL) < 6 THEN NEXT.
             ELSE IF man = 1 AND MONTH(franvaro.FRAN) = 2
             AND MONTH(franvaro.TILL) = 2 THEN NEXT.
             ELSE IF MONTH(franvaro.FRAN) > man AND MONTH(franvaro.TILL) > man THEN NEXT.
             ELSE DO:
               nytid = franvaro.TIMMAR.
               RUN TIMSEK.P.
               franvaro.TIMMAR = sekunder / 36.
               rapphj1 = STRING(franvaro.TIMMAR,"999999").  
               /* tidigare 5555 01*/
               overrapp1 = "8888" + " " + "02" + " " + SUBSTRING(franvaro.PERSONALKOD,1 ,5) + " " +
               SUBSTRING(franvaro.LART,1,3) +   
               " " + SUBSTRING(rapphj1,1 ,6) +  
               " " + "000000" + " "  + "0000000" +
               " " + "000000000" + " " +
               SUBSTRING(STRING(franvaro.FRAN),1 ,2) +
               SUBSTRING(STRING(franvaro.FRAN),4 ,2) +
               SUBSTRING(STRING(franvaro.FRAN),7 ,2) + " " +
               SUBSTRING(STRING(franvaro.TILL),1 ,2) +
               SUBSTRING(STRING(franvaro.TILL),4 ,2) +
               SUBSTRING(STRING(franvaro.TILL),7 ,2) + " " +
               "000" + " " + "0000" + " " + "      " + " " + "EL".  
               IF franvaro.LART = "530" THEN DO:
                  PUT STREAM frantj overrapp1 SKIP. 
               END.   
               PUT STREAM frlosy overrapp1 SKIP.           
               DELETE franvaro.
             END.
          END.
          ELSE DO:   
             nytid = franvaro.TIMMAR.
             RUN TIMSEK.P.
             franvaro.TIMMAR = sekunder / 36.
             rapphj1 = STRING(franvaro.TIMMAR,"999999").  
             /* tidigare 5555 01*/
             overrapp1 = "8888" + " " + "02" + " " + SUBSTRING(franvaro.PERSONALKOD,1 ,5) + " " +
             SUBSTRING(franvaro.LART,1,3) +   
             " " + SUBSTRING(rapphj1,1 ,6) +  
             " " + "000000" + " "  + "0000000" +
             " " + "000000000" + " " +
             SUBSTRING(STRING(franvaro.FRAN),1 ,2) +
             SUBSTRING(STRING(franvaro.FRAN),4 ,2) +
             SUBSTRING(STRING(franvaro.FRAN),7 ,2) + " " +
             SUBSTRING(STRING(franvaro.TILL),1 ,2) +
             SUBSTRING(STRING(franvaro.TILL),4 ,2) +
             SUBSTRING(STRING(franvaro.TILL),7 ,2) + " " +
             "000" + " " + "0000" + " " + "      " + " " + "EL".  
             IF franvaro.LART = "530" THEN DO:
                PUT STREAM frantj overrapp1 SKIP. 
             END.   
             PUT STREAM frlosy overrapp1 SKIP.           
             DELETE franvaro.            
          END.
        END.
      END.         
   END.   

END.                        
IF Guru.Konstanter:globforetag = "GRAN"  OR Guru.Konstanter:globforetag = "cELPA" THEN DO:
   OUTPUT STREAM frantj CLOSE.
   OUTPUT STREAM frlosy CLOSE.
END.

   
IF Guru.Konstanter:globforetag = "GRAN" THEN DO:
   OUTPUT  TO \\GRANGURU\guru_ser\server\PRO9S\gran\fransu.d NO-ECHO.
END.  
ELSE IF Guru.Konstanter:globforetag = "GADM" THEN DO:
   OUTPUT  TO \\GRANGURU\guru_ser\server\PRO9S\gadm\fransu.d NO-ECHO.
END.  
ELSE IF Guru.Konstanter:globforetag = "GSYD" THEN DO:
   OUTPUT  TO \\GRANGURU\guru_ser\server\PRO9S\gsyd\fransu.d NO-ECHO.
END. 
ELSE IF Guru.Konstanter:globforetag = "GRIT" THEN DO:
   OUTPUT  TO \\GRANGURU\guru_ser\server\PRO9S\grit\fransu.d NO-ECHO.
END. 
ELSE IF Guru.Konstanter:globforetag = "GKAL" THEN DO:
   OUTPUT  TO d:\DELAD\server\PRO9S\gkal\fransu.d NO-ECHO.
END. 
ELSE IF Guru.Konstanter:globforetag = "GKRVA" THEN DO:
   OUTPUT  TO \\GRANGURU\guru_ser\server\PRO9S\gkrva\fransu.d NO-ECHO.
END. 
ELSE IF Guru.Konstanter:globforetag = "ELPA" THEN DO:
   OUTPUT TO \\pc112\delad\pro9s\korning\fransu.d NO-ECHO.
END.   
FOR EACH franvaro USE-INDEX franvaro  NO-LOCK:
  EXPORT franvaro.
END.
OUTPUT CLOSE.

IF Guru.Konstanter:globforetag = "GRAN" THEN DO:
   OUTPUT  TO \\GRANGURU\guru_ser\server\PRO9S\gran\felpers.d APPEND.
END.  
ELSE IF Guru.Konstanter:globforetag = "GADM" THEN DO:
   OUTPUT  TO \\GRANGURU\guru_ser\server\PRO9S\gadm\felpers.d APPEND.
END.  
ELSE IF Guru.Konstanter:globforetag = "GSYD" THEN DO:
   OUTPUT  TO \\GRANGURU\guru_ser\server\PRO9S\gsyd\felpers.d APPEND.
END. 
ELSE IF Guru.Konstanter:globforetag = "GRIT" THEN DO:
   OUTPUT  TO \\GRANGURU\guru_ser\server\PRO9S\grit\felpers.d APPEND.
END. 
ELSE IF Guru.Konstanter:globforetag = "GKAL" THEN DO:
   OUTPUT  TO d:\DELAD\server\PRO9S\gkal\felpers.d APPEND.
END. 
ELSE IF Guru.Konstanter:globforetag = "GKRVA" THEN DO:
   OUTPUT  TO \\GRANGURU\guru_ser\server\PRO9S\gkrva\felpers.d APPEND.
END. 
ELSE IF Guru.Konstanter:globforetag = "ELPA" THEN DO:
   OUTPUT TO \\pc112\delad\pro9s\korning\felpers.d APPEND.
END.   
FOR EACH franvaro USE-INDEX franvaro  NO-LOCK:
  EXPORT franvaro.
END.


IF Guru.Konstanter:globforetag = "GRAN" THEN DO:
   prognamn = "\\GRANGURU\guru_ser\server\PRO9S\gran\losyd".
   prognamn = prognamn  + STRING(TODAY,"999999") + ".d".        
   OS-copy \\GRANGURU\guru_ser\server\PRO9S\gran\losyd.d VALUE(prognamn).       

   prognamn = "\\GRANGURU\guru_ser\server\PRO9S\gran\lotjan".
   prognamn = prognamn  + STRING(TODAY,"999999") + ".d".        
   OS-copy \\GRANGURU\guru_ser\server\PRO9S\gran\lotjan.d VALUE(prognamn).       
END.  
ELSE IF Guru.Konstanter:globforetag = "GADM" THEN DO:   
   prognamn = "\\GRANGURU\guru_ser\server\PRO9S\gadm\lotrad".
   prognamn = prognamn  + STRING(TODAY,"999999") + ".d".        
   OS-copy \\GRANGURU\guru_ser\server\PRO9S\gadm\lotrad.d VALUE(prognamn).       

   prognamn = "\\GRANGURU\guru_ser\server\PRO9S\gadm\lokoll".
   prognamn = prognamn  + STRING(TODAY,"999999") + ".d".        
   OS-copy \\GRANGURU\guru_ser\server\PRO9S\gadm\lokoll.d VALUE(prognamn).       
END.  
ELSE IF Guru.Konstanter:globforetag = "GRIT" THEN DO:   
   prognamn = "\\GRANGURU\guru_ser\server\PRO9S\grit\lokoll".
   prognamn = prognamn  + STRING(TODAY,"999999") + ".d".        
   OS-copy \\GRANGURU\guru_ser\server\PRO9S\grit\lokoll.d VALUE(prognamn).       
END.   
ELSE IF Guru.Konstanter:globforetag = "GKAL" THEN DO:
   prognamn = "d:\DELAD\server\PRO9S\gkal\lokoll".
   prognamn = prognamn  + STRING(TODAY,"999999") + ".d".        
   OS-copy d:\DELAD\server\PRO9S\gkal\lokoll.d VALUE(prognamn).         
   OS-copy d:\DELAD\server\PRO9S\gkal\lokoll.d d:\guru\gkal\gurukalmar.gkal.         
END.       
ELSE IF Guru.Konstanter:globforetag = "GKRVA" THEN DO:   
   prognamn = "\\GRANGURU\guru_ser\server\PRO9S\gkrva\lokoll".
   prognamn = prognamn  + STRING(TODAY,"999999") + ".d".        
   OS-copy \\GRANGURU\guru_ser\server\PRO9S\gkrva\lokoll.d VALUE(prognamn).         

   prognamn = "\\GRANGURU\guru_ser\server\PRO9S\gkrva\lotjan".
   prognamn = prognamn  + STRING(TODAY,"999999") + ".d".        
   OS-copy \\GRANGURU\guru_ser\server\PRO9S\gkrva\lotjan.d VALUE(prognamn).         
END.  



OUTPUT CLOSE.
RUN sammut_UI (INPUT 2).
{EUROPEANAMERICAN.I}