/*XGRFR2.P SKAPAR PA90FIL FRANVARO.*/  
DEFINE STREAM frloko. 
DEFINE STREAM frlotj. 
DEFINE STREAM frlosy.
DEFINE STREAM franko. 
DEFINE STREAM frantj.  
/*DEFINE STREAM fransy.*/
/*{LESAMMAN.I}*/
DEFINE INPUT PARAMETER invkdatum AS DATE NO-UNDO.
DEFINE INPUT PARAMETER ingvisatidpermanad AS LOGICAL NO-UNDO.
DEFINE INPUT PARAMETER inglobforetag LIKE FORETAG.FORETAG NO-UNDO.
DEFINE INPUT PARAMETER inman AS INTEGER FORMAT "99" NO-UNDO.
/*RUN sammut_UI (INPUT 1).*/
DEFINE NEW SHARED VARIABLE vkdatum AS DATE NO-UNDO.
DEFINE NEW SHARED VARIABLE gvisatidpermanad AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE globforetag LIKE FORETAG.FORETAG NO-UNDO.
DEFINE NEW SHARED VARIABLE man AS INTEGER FORMAT "99" NO-UNDO.
ASSIGN
vkdatum     = invkdatum  
gvisatidpermanad   = ingvisatidpermanad 
globforetag = inglobforetag
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
IF globforetag = "CGRAN" THEN DO:
   INPUT FROM \\GRANGURU\guru_ser\server\PRO9S\GRAN\fransu.d NO-ECHO.
END. 
ELSE IF globforetag = "CGADM" THEN DO:
   INPUT FROM \\GRANGURU\guru_ser\server\PRO9S\gadm\fransu.d NO-ECHO.
END.  
ELSE IF globforetag = "GSYD" THEN DO:
   INPUT FROM \\GRANGURU\guru_ser\server\PRO9S\gsyd\fransu.d NO-ECHO.
END. 
ELSE IF globforetag = "CGRIT" THEN DO:
   INPUT FROM \\GRANGURU\guru_ser\server\PRO9S\GRIT\fransu.d NO-ECHO.
END. 
ELSE IF globforetag = "CGKAL" THEN DO:
   INPUT FROM \\GRANGURU\guru_ser\server\PRO9S\GKAL\fransu.d NO-ECHO.
END. 
ELSE IF globforetag = "ELPA" THEN DO:
   INPUT FROM \\pc012\d\delad\pro9s\korning\fransu.d NO-ECHO.
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
IF globforetag = "CGKAL" THEN DO:      
   FOR EACH franvaro:
      FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = franvaro.PERSONALKOD NO-LOCK NO-ERROR.
      FIND FIRST OMRADETAB WHERE OMRADETAB.OMRADE = PERSONALTAB.OMRADE NO-LOCK NO-ERROR.
      FIND FIRST AVDELNING WHERE AVDELNING.AVDELNINGNR = OMRADETAB.AVDELNINGNR NO-LOCK NO-ERROR.
      FIND FIRST JURPERS WHERE JURPERS.JUDID = AVDELNING.POSTANST NO-LOCK NO-ERROR.      
      IF AVAILABLE JURPERS THEN DO:
         ASSIGN franvaro.VIJUDID = JURPERS.VIJUDID.
      END.
   END.
END.
IF globforetag = "CGRAN" OR globforetag = "CGRIT" OR globforetag = "CGADM" OR globforetag = "ELPA" THEN DO:      
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



/*HUR SKALL MAN GÖRA HÄR MED GSYD*/
DO TRANSACTION:
IF globforetag = "CGRAN" THEN DO:
   OUTPUT STREAM franko TO \\GRANGURU\guru_ser\server\PRO9S\GRAN\frkoll.d NO-ECHO.   
   OUTPUT STREAM frloko TO \\GRANGURU\guru_ser\server\PRO9S\GRAN\lokoll.d APPEND.
   ftag = "2222".
   kolle = "03". 
END.  
ELSE IF globforetag = "CGADM" THEN DO:
   OUTPUT STREAM franko TO \\GRANGURU\guru_ser\server\PRO9S\gadm\frkoll.d NO-ECHO.   
   OUTPUT STREAM frloko TO \\GRANGURU\guru_ser\server\PRO9S\gadm\lokoll.d APPEND.
   ftag = "2000".
   kolle = "04". 
END.  
ELSE IF globforetag = "GSYD" THEN DO:
   OUTPUT STREAM franko TO \\GRANGURU\guru_ser\server\PRO9S\gsyd\frkoll.d NO-ECHO.   
   OUTPUT STREAM frloko TO \\GRANGURU\guru_ser\server\PRO9S\gsyd\lokoll.d APPEND.
   ftag = "4444".
   kolle = "01". 
END.      
ELSE IF globforetag = "CGRIT" THEN DO:
   OUTPUT STREAM franko TO \\GRANGURU\guru_ser\server\PRO9S\GRIT\frkoll.d NO-ECHO.   
   OUTPUT STREAM frloko TO \\GRANGURU\guru_ser\server\PRO9S\GRIT\lokoll.d APPEND.
   ftag = "2000".
   kolle = "04". 
END.   
ELSE IF globforetag = "CGKAL" THEN DO:
   OUTPUT STREAM franko TO \\GRANGURU\guru_ser\server\PRO9S\GKAL\frkoll.d NO-ECHO.   
   OUTPUT STREAM frloko TO \\GRANGURU\guru_ser\server\PRO9S\GKAL\lokoll.d APPEND.
   ftag = "2003".
   kolle = "01". 
END.       
ELSE IF globforetag = "ELPA" THEN DO:
   OUTPUT STREAM frloko TO \\pc012\d\delad\pro9s\korning\lokoll.d APPEND.  
   OUTPUT STREAM franko TO \\pc012\d\delad\pro9s\korning\frkoll.d.  
   ftag = "2000".
   kolle = "04". 
END.                                
IF globforetag = "CGRAN" OR globforetag = "cELPA"  THEN DO:
   FIND LAST franvaro WHERE franvaro.PERSONALKOD BEGINS "35" USE-INDEX franvaro NO-LOCK NO-ERROR.
END.  
ELSE DO:
   FIND LAST franvaro USE-INDEX franvaro NO-LOCK NO-ERROR.
END.  
reco = RECID(franvaro).  
IF globforetag = "CGRAN" OR globforetag = "cELPA" THEN DO:                   
   FIND FIRST franvaro  WHERE franvaro.PERSONALKOD BEGINS "35" USE-INDEX franvaro EXCLUSIVE-LOCK NO-ERROR.
END.     
ELSE DO:                   
   FIND FIRST franvaro USE-INDEX franvaro EXCLUSIVE-LOCK NO-ERROR.
END.
IF NOT AVAILABLE franvaro THEN LEAVE.
IF AVAILABLE franvaro THEN DO:
  IF globforetag = "CGKAL" THEN DO:
     IF franvaro.VIJUDID = "GKEAB" THEN kolle = "01".
     ELSE IF franvaro.VIJUDID = "GSEAB" THEN kolle = "02".
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
   IF globforetag = "CGRAN" OR globforetag = "cELPA" THEN DO:                   
     FIND NEXT franvaro  WHERE franvaro.PERSONALKOD BEGINS "35" USE-INDEX franvaro EXCLUSIVE-LOCK NO-ERROR.
   END.
   ELSE DO:                   
      FIND NEXT franvaro USE-INDEX franvaro EXCLUSIVE-LOCK NO-ERROR.
   END.  
  IF NOT AVAILABLE franvaro THEN LEAVE. 
  IF AVAILABLE franvaro THEN DO:
    IF globforetag = "CGKAL" THEN DO:
       IF franvaro.VIJUDID = "GKEAB" THEN kolle = "01".
       ELSE IF franvaro.VIJUDID = "GSEAB" THEN kolle = "02".
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
IF globforetag = "CGRAN" THEN DO:
   OUTPUT STREAM frantj TO \\GRANGURU\guru_ser\server\PRO9S\GRAN\frtjan.d NO-ECHO.  
   OUTPUT STREAM frlotj TO \\GRANGURU\guru_ser\server\PRO9S\GRAN\lotjan.d APPEND.
END.                                             
ELSE IF globforetag = "CGADM" THEN nytid = nytid.
ELSE IF globforetag = "CGRIT" THEN nytid = nytid.
ELSE IF globforetag = "CGKAL" THEN nytid = nytid. 
ELSE IF globforetag = "GSYD" THEN nytid = nytid.
ELSE IF globforetag = "ELPA" THEN nytid = nytid.
ELSE IF globforetag = "cELPA" THEN DO:
   OUTPUT STREAM frantj TO \\pc012\d\delad\pro9s\korning\frtjan.d NO-ECHO.
   OUTPUT STREAM frlotj TO \\pc012\d\delad\pro9s\korning\lotjan.d APPEND.
END.  
DO TRANSACTION:
   IF globforetag = "CGRAN" OR globforetag = "cELPA" THEN DO:
      FIND LAST franvaro WHERE franvaro.PERSONALKOD BEGINS "45" USE-INDEX franvaro NO-LOCK NO-ERROR.
      reco = RECID(franvaro).                       
      FIND FIRST franvaro  WHERE franvaro.PERSONALKOD BEGINS "45" USE-INDEX franvaro EXCLUSIVE-LOCK NO-ERROR.
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
        FIND NEXT franvaro WHERE franvaro.PERSONALKOD BEGINS "45" USE-INDEX franvaro EXCLUSIVE-LOCK NO-ERROR.
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
IF globforetag = "CGRAN" OR globforetag = "cELPA" THEN DO:
   OUTPUT STREAM frantj CLOSE.
   OUTPUT STREAM frlotj CLOSE.
END.

IF globforetag = "CGRAN"  THEN DO:
   OUTPUT STREAM frantj TO \\GRANGURU\guru_ser\server\PRO9S\GRAN\frsyd.d NO-ECHO.  
   OUTPUT STREAM frlosy TO \\GRANGURU\guru_ser\server\PRO9S\GRAN\losyd.d APPEND.
END.                                             
ELSE IF globforetag = "CGADM" THEN nytid = nytid.
ELSE IF globforetag = "CGRIT" THEN nytid = nytid.
ELSE IF globforetag = "CGKAL" THEN nytid = nytid. 
ELSE IF globforetag = "GSYD" THEN nytid = nytid.
ELSE IF globforetag = "ELPA" THEN nytid = nytid.
ELSE IF globforetag = "cELPA" THEN DO:
   OUTPUT STREAM frantj TO \\pc012\d\delad\pro9s\korning\frsyd.d NO-ECHO.
   OUTPUT STREAM frlosy TO \\pc012\d\delad\pro9s\korning\losyd.d APPEND.
END.  
DO TRANSACTION:
   IF globforetag = "CGRAN" OR globforetag = "cELPA" THEN DO:
      FIND LAST franvaro WHERE franvaro.PERSONALKOD BEGINS "31" USE-INDEX franvaro NO-LOCK NO-ERROR.
      reco = RECID(franvaro).                       
      FIND FIRST franvaro  WHERE franvaro.PERSONALKOD BEGINS "31" USE-INDEX franvaro EXCLUSIVE-LOCK NO-ERROR.
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
             overrapp1 = "4444" + " " + "01" +  " " + SUBSTRING(franvaro.PERSONALKOD,1 ,5) +  " " +
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
           overrapp1 = "4444" + " " + "01" +  " " + SUBSTRING(franvaro.PERSONALKOD,1 ,5) +  " " +
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
               overrapp1 = "4444" + " " + "01" + " " + SUBSTRING(franvaro.PERSONALKOD,1 ,5) + " " +
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
             overrapp1 = "4444" + " " + "01" + " " + SUBSTRING(franvaro.PERSONALKOD,1 ,5) + " " +
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
   IF globforetag = "CGRAN" OR globforetag = "cELPA" THEN DO:
      FIND LAST franvaro WHERE franvaro.PERSONALKOD BEGINS "41" USE-INDEX franvaro NO-LOCK NO-ERROR.
      reco = RECID(franvaro).                       
      FIND FIRST franvaro  WHERE franvaro.PERSONALKOD BEGINS "41" USE-INDEX franvaro EXCLUSIVE-LOCK NO-ERROR.
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
             overrapp1 = "4444" + " " + "01" +  " " + SUBSTRING(franvaro.PERSONALKOD,1 ,5) +  " " +
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
           overrapp1 = "4444" + " " + "01" +  " " + SUBSTRING(franvaro.PERSONALKOD,1 ,5) +  " " +
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
               overrapp1 = "4444" + " " + "01" + " " + SUBSTRING(franvaro.PERSONALKOD,1 ,5) + " " +
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
             overrapp1 = "4444" + " " + "01" + " " + SUBSTRING(franvaro.PERSONALKOD,1 ,5) + " " +
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
IF globforetag = "CGRAN" OR globforetag = "cELPA" THEN DO:
   OUTPUT STREAM frantj CLOSE.
   OUTPUT STREAM frlosy CLOSE.
END.

   
IF globforetag = "CGRAN" THEN DO:
   OUTPUT  TO \\GRANGURU\guru_ser\server\PRO9S\GRAN\fransu.d NO-ECHO.
END.  
ELSE IF globforetag = "CGADM" THEN DO:
   OUTPUT  TO \\GRANGURU\guru_ser\server\PRO9S\gadm\fransu.d NO-ECHO.
END.  
ELSE IF globforetag = "GSYD" THEN DO:
   OUTPUT  TO \\GRANGURU\guru_ser\server\PRO9S\gsyd\fransu.d NO-ECHO.
END. 
ELSE IF globforetag = "CGRIT" THEN DO:
   OUTPUT  TO \\GRANGURU\guru_ser\server\PRO9S\GRIT\fransu.d NO-ECHO.
END. 
ELSE IF globforetag = "CGKAL" THEN DO:
   OUTPUT  TO \\GRANGURU\guru_ser\server\PRO9S\GKAL\fransu.d NO-ECHO.
END. 
ELSE IF globforetag = "ELPA" THEN DO:
   OUTPUT TO \\pc012\d\delad\pro9s\korning\fransu.d NO-ECHO.
END.   
FOR EACH franvaro USE-INDEX franvaro  NO-LOCK:
  EXPORT franvaro.
END.
OUTPUT CLOSE.
RUN sammut_UI (INPUT 2).
