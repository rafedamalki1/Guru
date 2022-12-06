/*NORDFR2.P SKAPAR PA90FIL FRANVARO.*/  

DEFINE SHARED VARIABLE regdatum AS DATE NO-UNDO.
DEFINE SHARED VARIABLE regvnr AS INTEGER FORMAT "999" NO-UNDO.
DEFINE SHARED VARIABLE vkdatum AS DATE NO-UNDO.
DEFINE SHARED VARIABLE gvisatidpermanad AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE nytid AS DECIMAL FORMAT "99.99" NO-UNDO.
DEFINE SHARED VARIABLE sekunder AS INTEGER FORMAT "-9999999" NO-UNDO.
DEFINE SHARED VARIABLE man AS INTEGER FORMAT "99" NO-UNDO.
DEFINE VARIABLE rapphj1 AS CHARACTER FORMAT "X(9)" NO-UNDO.      /*HJALP ANTAL*/
DEFINE VARIABLE rapphj2 AS CHARACTER FORMAT "X(11)" NO-UNDO.
DEFINE VARIABLE rapphj3 AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE VARIABLE overrapp1 AS CHARACTER FORMAT "X(100)" NO-UNDO.      /*LON*/
DEFINE VARIABLE regdat1 AS DATE NO-UNDO.
DEFINE VARIABLE regdat2 AS DATE NO-UNDO.  
DEFINE VARIABLE regdat3 AS DATE NO-UNDO.
DEFINE VARIABLE onr2 LIKE TIDREGITAB.AONR NO-UNDO.
DEFINE VARIABLE dnr2 LIKE TIDREGITAB.DELNR NO-UNDO.
DEFINE VARIABLE antal LIKE TIDREGITAB.LONTILLANTAL NO-UNDO.   /*LON*/
DEFINE VARIABLE proc1 AS INTEGER FORMAT "999" NO-UNDO.
DEFINE VARIABLE pnr LIKE PERSONALTAB.PERSONNUMMER NO-UNDO.      /*LON*/
DEFINE VARIABLE fkod LIKE FVARO.FRKOD NO-UNDO.
DEFINE VARIABLE tperiod AS INTEGER FORMAT "999" NO-UNDO.

 
DEFINE VARIABLE ar AS INTEGER FORMAT "9999" NO-UNDO. 
DEFINE TEMP-TABLE franvaro
   FIELD PERSONNUMMER LIKE PERSONALTAB.PERSONNUMMER
   FIELD AONR LIKE TIDREGITAB.AONR
   FIELD DELNR LIKE TIDREGITAB.DELNR
   FIELD FRAN LIKE TIDREGITAB.DATUM
   FIELD TILL LIKE TIDREGITAB.DATUM
   FIELD PROCENT AS INTEGER FORMAT "999"
   FIELD TIMMAR AS DECIMAL FORMAT "99.99"
   FIELD LART AS CHARACTER FORMAT "X(4)"
   INDEX FRANVARO IS PRIMARY  PERSONNUMMER FRAN LART ASCENDING.  
/*IF globforetag = "NORD" THEN DO:     
   DOS SILENT F:\data\progress\fran.bat.  
END.*/              
/*IF globforetag = "NORD" THEN DO:                                              
   DOS SILENT copy F:\data\progress\frfelma.d F:\data\progress\frfelko.d NO-ECHO.
   DOS SILENT copy F:\data\progress\franfel.d F:\data\progress\frfelma.d NO-ECHO.
   DOS SILENT copy F:\data\progress\tom.d F:\data\progress\franfel.d NO-ECHO.
   DOS SILENT copy F:\data\progress\fransu.d F:\data\progress\kofrsu.d NO-ECHO.
   DOS SILENT copy F:\data\progress\fran1.d F:\data\progress\frankop.d NO-ECHO.
END.*/
{AMERICANEUROPEAN.I}         
regdatum = TODAY.
RUN REGVEC.P.
ASSIGN
tperiod = regvnr + 19.
IF OPSYS = "MSDOS" OR OPSYS = "WIN32" THEN DO:
  IF globforetag = "NORD" THEN DO:
     INPUT FROM P:\progress\fransu.d NO-ECHO.
  END.   
  ELSE DO:
     INPUT FROM C:\GURU\fransu.d NO-ECHO.
  END.   
END.
REPEAT TRANSACTION:
  CREATE franvaro.
  ASSIGN.
  IMPORT franvaro.
END.   
REPEAT TRANSACTION:
  FIND NEXT franvaro USE-INDEX franvaro EXCLUSIVE-LOCK NO-ERROR.
  IF NOT AVAILABLE franvaro THEN LEAVE.
  IF gvisatidpermanad = FALSE AND  man = 12 THEN DO:
    IF MONTH(franvaro.FRAN) < 10 AND MONTH(franvaro.TILL) < 10 THEN NEXT.
    IF MONTH(franvaro.FRAN) > 10  AND MONTH(franvaro.TILL) > 10 THEN NEXT.
    IF MONTH(franvaro.FRAN) > 10 AND MONTH(franvaro.TILL) < 10 THEN DO:
      pnr = franvaro.PERSONNUMMER.
      onr2 = franvaro.AONR.
      dnr2 = franvaro.DELNR.
      fkod = franvaro.LART.
      regdat1 = franvaro.TILL.
      regdat2 = regdat1.
      proc1 = franvaro.PROCENT.
      antal = franvaro.TIMMAR.
      repeat:
	regdat2 = regdat2 - 1.
	IF MONTH(regdat2) > MONTH(regdat1) THEN LEAVE.
      END.
      ASSIGN franvaro.TILL = regdat2.
      regdat2 = regdat2 + 1.
      CREATE franvaro.
      ASSIGN franvaro.PERSONNUMMER = pnr
      franvaro.AONR = onr2
      franvaro.DELNR = dnr2
      franvaro.LART = fkod
      franvaro.FRAN = regdat2
      franvaro.TILL = regdat1
      franvaro.PROCENT = proc1
      franvaro.TIMMAR = antal.
    END.
  END.
  ELSE DO:
     IF gvisatidpermanad = FALSE THEN DO:
        IF MONTH(franvaro.FRAN) > man AND MONTH(franvaro.TILL) > man THEN NEXT.
        IF MONTH(franvaro.FRAN) LE man AND MONTH(franvaro.TILL) LE man THEN NEXT.
        IF MONTH(franvaro.FRAN) LE man AND MONTH(franvaro.TILL) > man THEN DO:
           pnr = franvaro.PERSONNUMMER.
           onr2 = franvaro.AONR.
           dnr2 = franvaro.DELNR.
           fkod = franvaro.LART.
           regdat1 = franvaro.TILL.
           regdat2 = regdat1.
           proc1 = franvaro.PROCENT.
           antal = franvaro.TIMMAR.
           repeat:
             regdat2 = regdat2 - 1.
             IF MONTH(regdat2) < MONTH(regdat1) THEN LEAVE.
           END.
           ASSIGN franvaro.TILL = regdat2.
           regdat2 = regdat2 + 1.
           CREATE franvaro.
           ASSIGN franvaro.PERSONNUMMER = pnr
           franvaro.AONR = onr2
           franvaro.DELNR = dnr2
           franvaro.LART = fkod
           franvaro.FRAN = regdat2
           franvaro.TILL = regdat1
           franvaro.PROCENT = proc1
           franvaro.TIMMAR = antal.        
        END.   
     END.   
     ELSE DO:
        IF MONTH(franvaro.FRAN) > MONTH(vkdatum) AND MONTH(franvaro.TILL) > MONTH(vkdatum) THEN NEXT.
        IF MONTH(franvaro.FRAN) LE MONTH(vkdatum) AND MONTH(franvaro.TILL) LE MONTH(vkdatum) THEN NEXT.
        IF MONTH(franvaro.FRAN) LE MONTH(vkdatum) AND MONTH(franvaro.TILL) > MONTH(vkdatum) THEN DO:
           pnr = franvaro.PERSONNUMMER.
           onr2 = franvaro.AONR.
           dnr2 = franvaro.DELNR.
           fkod = franvaro.LART.
           regdat1 = franvaro.TILL.
           regdat2 = regdat1.
           proc1 = franvaro.PROCENT.
           antal = franvaro.TIMMAR.
           repeat:
              regdat2 = regdat2 - 1.
              IF MONTH(regdat2) < MONTH(regdat1) THEN LEAVE.
           END.
           ASSIGN franvaro.TILL = regdat2.
           regdat2 = regdat2 + 1.
           CREATE franvaro.
           ASSIGN franvaro.PERSONNUMMER = pnr
           franvaro.AONR = onr2
           franvaro.DELNR = dnr2
           franvaro.LART = fkod
           franvaro.FRAN = regdat2
           franvaro.TILL = regdat1
           franvaro.PROCENT = proc1
           franvaro.TIMMAR = antal.        
        END.   
     END.   
  END.
END.

IF OPSYS = "MSDOS" OR OPSYS = "WIN32" THEN DO: 
  IF globforetag = "NORD" THEN DO:
     OUTPUT TO P:\progress\frkop.d NO-ECHO.
  END.   
  ELSE DO:
     OUTPUT TO C:\GURU\frkop.d NO-ECHO.
  END.   
END.
FOR EACH franvaro USE-INDEX franvaro NO-LOCK:
  EXPORT franvaro.
END.
OUTPUT CLOSE.
/*IF OPSYS = "MSDOS" OR OPSYS = "WIN32" THEN DO:
  IF globforetag = "NORD" THEN DO:
     DOS SILENT copy F:\data\progress\fran1.d F:\data\progress\frankop.d NO-ECHO.
  END.   
  ELSE DO:
     DOS SILENT copy C:\GURU\fran1.d C:\GURU\frankop.d NO-ECHO.
  END.   
END. */   
DO TRANSACTION:
IF OPSYS = "MSDOS" OR OPSYS = "WIN32" THEN DO: 
  IF globforetag = "NORD" THEN DO:
     OUTPUT TO P:\progress\fran1.d NO-ECHO.
  END.   
  ELSE DO:
     OUTPUT TO C:\GURU\fran1.d NO-ECHO.
  END.   
END.   
FIND FIRST franvaro USE-INDEX franvaro EXCLUSIVE-LOCK NO-ERROR.
IF NOT AVAILABLE franvaro THEN LEAVE.
IF AVAILABLE franvaro THEN DO:
   IF gvisatidpermanad = FALSE THEN DO:
      IF man = 12 AND MONTH(franvaro.FRAN) < 10
      AND MONTH(franvaro.TILL) < 10 THEN antal = antal.   
      ELSE IF man = 1 AND MONTH(franvaro.FRAN) = 2
      AND MONTH(franvaro.TILL) = 2 THEN antal = antal. 
      ELSE IF MONTH(franvaro.FRAN) > man AND MONTH(franvaro.TILL) > man THEN antal = antal. 
      ELSE DO:
        nytid = franvaro.TIMMAR.
        RUN TIMSEK.P.
        franvaro.TIMMAR = sekunder / 36.
        rapphj1 = STRING(franvaro.TIMMAR,">>>>>").
        overrapp1 = "4845" + STRING(tperiod) + SUBSTRING(franvaro.PERSONNUMMER,1 ,10) + 
        SUBSTRING(STRING(franvaro.FRAN),1 ,2) +
        SUBSTRING(STRING(franvaro.FRAN),4 ,2) +
        SUBSTRING(STRING(franvaro.FRAN),7 ,2) +  
        SUBSTRING(franvaro.LART,1,2) +  
        SUBSTRING(rapphj1,1 , 5) + "     " +
        SUBSTRING(STRING(franvaro.TILL),1 ,2) +
        SUBSTRING(STRING(franvaro.TILL),4 ,2) +
        SUBSTRING(STRING(franvaro.TILL),7 ,2).  
        PUT overrapp1.
        PUT SKIP.
        DELETE franvaro.
      END.
   END.
   ELSE DO:
      nytid = franvaro.TIMMAR.
      RUN TIMSEK.P.
      franvaro.TIMMAR = sekunder / 36.
      rapphj1 = STRING(franvaro.TIMMAR,">>>>>").
      overrapp1 = "4845" + STRING(tperiod) + SUBSTRING(franvaro.PERSONNUMMER,1 ,10) + 
      SUBSTRING(STRING(franvaro.FRAN),1 ,2) +
      SUBSTRING(STRING(franvaro.FRAN),4 ,2) +
      SUBSTRING(STRING(franvaro.FRAN),7 ,2) +  
      SUBSTRING(franvaro.LART,1,2) +  
      SUBSTRING(rapphj1,1 , 5) + "     " +
      SUBSTRING(STRING(franvaro.TILL),1 ,2) +
      SUBSTRING(STRING(franvaro.TILL),4 ,2) +
      SUBSTRING(STRING(franvaro.TILL),7 ,2).  
      PUT overrapp1.
      PUT SKIP.
      DELETE franvaro.
   END. 
END.   
END.
REPEAT TRANSACTION:
  FIND NEXT franvaro USE-INDEX franvaro EXCLUSIVE-LOCK NO-ERROR.
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
          rapphj1 = STRING(franvaro.TIMMAR,">>>>>").
          overrapp1 = "4845" + STRING(tperiod) + SUBSTRING(franvaro.PERSONNUMMER,1 ,10) + 
          SUBSTRING(STRING(franvaro.FRAN),1 ,2) +
          SUBSTRING(STRING(franvaro.FRAN),4 ,2) +
          SUBSTRING(STRING(franvaro.FRAN),7 ,2) +  
          SUBSTRING(franvaro.LART,1,2) +  
          SUBSTRING(rapphj1,1 , 5) + "     " +
          SUBSTRING(STRING(franvaro.TILL),1 ,2) +
          SUBSTRING(STRING(franvaro.TILL),4 ,2) +
          SUBSTRING(STRING(franvaro.TILL),7 ,2).      
          PUT overrapp1.
          PUT SKIP.
          DELETE franvaro.
        END.
     END.
     ELSE DO:
        nytid = franvaro.TIMMAR.
        RUN TIMSEK.P.
        franvaro.TIMMAR = sekunder / 36.
        rapphj1 = STRING(franvaro.TIMMAR,">>>>>").
        overrapp1 = "4845" + STRING(tperiod) + SUBSTRING(franvaro.PERSONNUMMER,1 ,10) + 
        SUBSTRING(STRING(franvaro.FRAN),1 ,2) +
        SUBSTRING(STRING(franvaro.FRAN),4 ,2) +
        SUBSTRING(STRING(franvaro.FRAN),7 ,2) +  
        SUBSTRING(franvaro.LART,1,2) +  
        SUBSTRING(rapphj1,1 , 5) + "     " +
        SUBSTRING(STRING(franvaro.TILL),1 ,2) +
        SUBSTRING(STRING(franvaro.TILL),4 ,2) +
        SUBSTRING(STRING(franvaro.TILL),7 ,2).      
        PUT overrapp1.
        PUT SKIP.
        DELETE franvaro.
     END.   
  END.
END.
OUTPUT CLOSE.
IF OPSYS = "MSDOS" OR OPSYS = "WIN32" THEN DO:
  IF globforetag = "NORD" THEN DO:
     OUTPUT TO P:\progress\fransu.d NO-ECHO.
  END.   
  ELSE DO:
     OUTPUT TO C:\GURU\fransu.d NO-ECHO.
  END.   
END.
FOR EACH franvaro USE-INDEX franvaro  NO-LOCK:
  EXPORT franvaro.
END.
{EUROPEANAMERICAN.I}