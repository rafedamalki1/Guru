/*GRANFR2.P SKAPAR PA90FIL FRANVARO.*/  
DEFINE STREAM frloko. 
DEFINE STREAM frlotj. 
DEFINE STREAM franko. 
DEFINE STREAM frantj.   
DEFINE NEW SHARED VARIABLE vkdatum AS DATE NO-UNDO.
DEFINE NEW SHARED VARIABLE gvisatidpermanad AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE man AS INTEGER FORMAT "99" NO-UNDO. 
DEFINE NEW SHARED VARIABLE nytid AS DECIMAL FORMAT "99.99" NO-UNDO.
DEFINE NEW SHARED VARIABLE sekunder AS INTEGER FORMAT "-9999999" NO-UNDO.
DEFINE NEW SHARED VARIABLE globforetag LIKE FORETAG.FORETAG NO-UNDO.
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
   INDEX FRANVARO IS PRIMARY  PERSONALKOD FRAN LART ASCENDING.
globforetag = "gadm".
vkdatum = 10/31/98.
gvisatidpermanad = true.   
   
IF globforetag = "GRAN" THEN DO:
   INPUT FROM \\GRANGURU\guru_ser\PRO8S\gran\fransu.d NO-ECHO.
END. 
ELSE IF globforetag = "GADM" THEN DO:
   INPUT FROM \\GRANGURU\guru_ser\PRO8S\gadm\fransu.d NO-ECHO.
END.  
ELSE IF globforetag = "GSYD" THEN DO:
   INPUT FROM \\GRANGURU\guru_ser\PRO8S\gsyd\fransu.d NO-ECHO.
END. 
ELSE DO:
   INPUT FROM C:\GURU\fransu.d NO-ECHO.
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
/*HUR SKALL MAN GÖRA HÄR MED GSYD*/
DO TRANSACTION:
IF globforetag = "GRAN" THEN DO:
   OUTPUT STREAM franko TO \\GRANGURU\guru_ser\PRO8S\gran\Tfrkoll.d NO-ECHO.   
   OUTPUT STREAM frloko TO \\GRANGURU\guru_ser\PRO8S\gran\Tlokoll.d APPEND.
   ftag = "2222".
   kolle = "03". 
END.  
ELSE IF globforetag = "GADM" THEN DO:
   OUTPUT STREAM franko TO \\GRANGURU\guru_ser\PRO8S\gadm\Tfrkoll.d NO-ECHO.   
   OUTPUT STREAM frloko TO \\GRANGURU\guru_ser\PRO8S\gadm\Tlokoll.d APPEND.    
   ftag = "1111".
   kolle = "01". 
END.  
ELSE IF globforetag = "GSYD" THEN DO:
   OUTPUT STREAM franko TO \\GRANGURU\guru_ser\PRO8S\gsyd\Tfrkoll.d NO-ECHO.   
   OUTPUT STREAM frloko TO \\GRANGURU\guru_ser\PRO8S\gsyd\Tlokoll.d APPEND.
   ftag = "4444".
   kolle = "01". 
END.          
ELSE DO:
  /* OUTPUT STREAM frloko TO C:\GURU\lokoll.d APPEND.  */
   OUTPUT STREAM franko TO C:\GURU\frkoll.d.  
   ftag = "2222".
   kolle = "03". 
END.                                
IF globforetag = "GRAN" OR globforetag = "ELPA" THEN DO:
   FIND LAST franvaro WHERE franvaro.PKOD = "K" USE-INDEX franvaro NO-LOCK NO-ERROR.
END.  
ELSE DO:
   FIND LAST franvaro USE-INDEX franvaro NO-LOCK NO-ERROR.
END.  
reco = RECID(franvaro).  
IF globforetag = "GRAN" OR globforetag = "ELPA" THEN DO:                   
   FIND FIRST franvaro  WHERE franvaro.PKOD = "K" USE-INDEX franvaro EXCLUSIVE-LOCK NO-ERROR.
END.     
ELSE DO:                   
   FIND FIRST franvaro USE-INDEX franvaro EXCLUSIVE-LOCK NO-ERROR.
END.
IF NOT AVAILABLE franvaro THEN LEAVE.
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
   IF globforetag = "GRAN" OR globforetag = "ELPA" THEN DO:                   
     FIND NEXT franvaro  WHERE franvaro.PKOD = "K" USE-INDEX franvaro EXCLUSIVE-LOCK NO-ERROR.
   END.
   ELSE DO:                   
      FIND NEXT franvaro USE-INDEX franvaro EXCLUSIVE-LOCK NO-ERROR.
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
IF globforetag = "GRAN" THEN DO:
   OUTPUT STREAM frantj TO \\GRANGURU\guru_ser\PRO8S\gran\Tfrtjan.d NO-ECHO.  
   OUTPUT STREAM frlotj TO \\GRANGURU\guru_ser\PRO8S\gran\Tlotjan.d APPEND.
END.                                             
ELSE IF globforetag = "GADM" THEN nytid = nytid.
ELSE IF globforetag = "ROSL" THEN nytid = nytid.
ELSE IF globforetag = "MALA" THEN nytid = nytid. 
ELSE IF globforetag = "GSYD" THEN nytid = nytid.
ELSE DO:
   OUTPUT STREAM frantj TO C:\GURU\frtjan.d NO-ECHO.
   OUTPUT STREAM frlotj TO C:\GURU\lotjan.d APPEND.
END.  
DO TRANSACTION:
   IF globforetag = "GRAN" OR globforetag = "ELPA" THEN DO:
      FIND LAST franvaro WHERE franvaro.PKOD BEGINS "T" USE-INDEX franvaro NO-LOCK NO-ERROR.
      reco = RECID(franvaro).                       
      FIND FIRST franvaro  WHERE franvaro.PKOD BEGINS "T" USE-INDEX franvaro EXCLUSIVE-LOCK NO-ERROR.
      IF NOT AVAILABLE franvaro THEN LEAVE.
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
             overrapp1 = "2222" + " " + "04" +  " " + SUBSTRING(franvaro.PERSONALKOD,1 ,5) +  " " +
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
           overrapp1 = "2222" + " " + "04" +  " " + SUBSTRING(franvaro.PERSONALKOD,1 ,5) +  " " +
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
      REPEAT:
        FIND NEXT franvaro WHERE franvaro.PKOD BEGINS "T" USE-INDEX franvaro EXCLUSIVE-LOCK NO-ERROR.
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
               overrapp1 = "2222" + " " + "04" + " " + SUBSTRING(franvaro.PERSONALKOD,1 ,5) + " " +
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
             overrapp1 = "2222" + " " + "04" + " " + SUBSTRING(franvaro.PERSONALKOD,1 ,5) + " " +
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
IF globforetag = "GRAN" THEN DO:
   OUTPUT STREAM frantj CLOSE.
   OUTPUT STREAM frlotj CLOSE.
END.   
IF globforetag = "GRAN" THEN DO:
   OUTPUT  TO \\GRANGURU\guru_ser\PRO8S\gran\fransu.d NO-ECHO.
END.  
ELSE IF globforetag = "GADM" THEN DO:
   OUTPUT  TO \\GRANGURU\guru_ser\PRO8S\gadm\fransu.d NO-ECHO.
END.  
ELSE IF globforetag = "GSYD" THEN DO:
   OUTPUT  TO \\GRANGURU\guru_ser\PRO8S\gsyd\fransu.d NO-ECHO.
END. 
ELSE DO:
   OUTPUT TO C:\GURU\fransu.d NO-ECHO.
END.   
FOR EACH franvaro USE-INDEX franvaro  NO-LOCK:
  EXPORT franvaro.
END.
OUTPUT CLOSE.
