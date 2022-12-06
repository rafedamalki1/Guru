/*GRANFR1.P OMARBETAT OPEN QUERY  KRAV ATT AONR FINNS UPPLAGD I FVARO
(TIDIGARE PRISTYP FRANVARO) KOPIA FINNS I SPARA*/
/* skapar filen pa90hj som ar veckans korning .Denna lagges till pa90veck.d*/
/*SKAPAR FILEN PA90VECK.P */
/*ALLA TILLAGG SOM VECKOKORS FAR EN EGEN RAD I DENNA RAPPORT*/
/*RAPPORTEN LIGGER TILL GRUND FOR MANADSKORNIING */  
DEFINE NEW SHARED VARIABLE globforetag LIKE FORETAG.FORETAG NO-UNDO.
DEFINE NEW SHARED VARIABLE persrec AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE vkdatum AS DATE NO-UNDO.
DEFINE NEW SHARED VARIABLE gvisatidpermanad AS LOGICAL NO-UNDO.
DEFINE new SHARED VARIABLE lunchslutet AS DECIMAL NO-UNDO.
DEFINE new SHARED VARIABLE lunchstarten AS DECIMAL NO-UNDO.
DEFINE new SHARED VARIABLE frustarten AS DECIMAL NO-UNDO.
DEFINE new SHARED VARIABLE fruslutet AS DECIMAL NO-UNDO.
DEFINE new SHARED VARIABLE kaffestart AS DECIMAL NO-UNDO.
DEFINE new SHARED VARIABLE kaffeslut AS DECIMAL NO-UNDO.
DEFINE new SHARED VARIABLE globanv LIKE ANVANDARE.ANVANDARE NO-UNDO.
DEFINE new SHARED VARIABLE regdagnamn AS CHARACTER FORMAT "X(3)" NO-UNDO.        
DEFINE new SHARED VARIABLE regtotalt LIKE TIDREGITAB.TOTALT NO-UNDO.
   

DEFINE VARIABLE antal LIKE TIDREGITAB.LONTILLANTAL NO-UNDO.   /*LON*/
DEFINE VARIABLE antalet LIKE PHJALP.PPAR8 NO-UNDO.   /*LON*/
DEFINE VARIABLE personal LIKE PERSONALTAB.PERSONALKOD NO-UNDO.
DEFINE VARIABLE vecknr LIKE TIDREGITAB.VECKONUMMER NO-UNDO.      /*LON*/
DEFINE VARIABLE proc LIKE FVARO.PROC NO-UNDO.
DEFINE VARIABLE fkod LIKE FVARO.FRKOD NO-UNDO.
DEFINE VARIABLE del1 LIKE FVARO.DEL NO-UNDO.
DEFINE VARIABLE regdatum2 AS DATE NO-UNDO.  
DEFINE VARIABLE regdat3 AS DATE NO-UNDO.
DEFINE VARIABLE regdat4 AS DATE NO-UNDO. 
DEFINE VARIABLE regdat5 AS DATE NO-UNDO.
DEFINE VARIABLE rec AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE regvnr AS INTEGER FORMAT "999" NO-UNDO.
DEFINE NEW SHARED VARIABLE regdatum AS DATE NO-UNDO.
DEFINE NEW SHARED VARIABLE regstart LIKE TIDREGITAB.START NO-UNDO. 
DEFINE NEW SHARED VARIABLE regslut LIKE TIDREGITAB.SLUT NO-UNDO.
DEFINE VARIABLE onr LIKE TIDREGITAB.AONR NO-UNDO.
DEFINE VARIABLE onr2 LIKE TIDREGITAB.AONR NO-UNDO.
DEFINE VARIABLE dnr LIKE TIDREGITAB.DELNR NO-UNDO.
DEFINE VARIABLE dnr2 LIKE TIDREGITAB.DELNR NO-UNDO.
DEFINE VARIABLE starttiden AS INTEGER NO-UNDO.
DEFINE VARIABLE sluttiden AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE nytid AS DECIMAL FORMAT "99.99" NO-UNDO.
DEFINE VARIABLE tott1 AS DECIMAL FORMAT "99.99" NO-UNDO.
DEFINE VARIABLE tott2 AS INTEGER NO-UNDO.
DEFINE VARIABLE proc1 AS INTEGER FORMAT "999" NO-UNDO.
DEFINE NEW SHARED VARIABLE sekunder AS INTEGER FORMAT "-9999999" NO-UNDO.
DEFINE VARIABLE seku AS INTEGER NO-UNDO.
DEFINE VARIABLE anst LIKE ANSTFORM.KOD NO-UNDO.
DEFINE VARIABLE krav AS INTEGER FORMAT "9" NO-UNDO.
DEFINE BUFFER frbuff FOR TIDREGITAB.   
DEFINE QUERY tidq FOR TIDREGITAB.
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
DEFINE TEMP-TABLE frfel
   FIELD PERSONALKOD LIKE PERSONALTAB.PERSONALKOD
   FIELD AONR LIKE TIDREGITAB.AONR
   FIELD DELNR LIKE TIDREGITAB.DELNR
   FIELD FRAN LIKE TIDREGITAB.DATUM
   FIELD TILL LIKE TIDREGITAB.DATUM
   FIELD PROCENT AS INTEGER FORMAT "999"
   FIELD TIMMAR AS DECIMAL FORMAT "99.99"
   FIELD LART AS CHARACTER FORMAT "X(4)"
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
IF globforetag = "GRAN" THEN DO:
   INPUT FROM \\GRANGURU\guru_ser\PRO8S\gran\franfel.d NO-ECHO.
END.  
ELSE IF globforetag = "GADM" THEN DO:
   INPUT FROM \\GRANGURU\guru_ser\PRO8S\gadm\franfel.d NO-ECHO.
END.  
ELSE IF globforetag = "GSYD" THEN DO:
   INPUT FROM \\GRANGURU\guru_ser\PRO8S\gsyd\franfel.d NO-ECHO.
END.     
ELSE DO:
   INPUT FROM C:\GURU\franfel.d NO-ECHO.
END.   
REPEAT TRANSACTION:
  CREATE frfel.
  ASSIGN.
  IMPORT frfel.
END.
OPEN QUERY persq FOR EACH PERSONALTAB WHERE PERSONALTAB.PERSMASK = TRUE 
USE-INDEX PERSONALKOD NO-LOCK.
GET FIRST persq NO-LOCK.
DO WHILE AVAILABLE(PERSONALTAB):
   persrec = RECID(PERSONALTAB).
   personal = PERSONALTAB.PERSONALKOD.
   FIND FIRST ANSTFORMTAB WHERE ANSTFORMTAB.ANSTALLNING =
   PERSONALTAB.ANSTALLNING USE-INDEX ANSTF NO-LOCK NO-ERROR.
   anst = ANSTFORMTAB.KOD.
   IF gvisatidpermanad = TRUE THEN DO:
      OPEN QUERY tidq FOR EACH TIDREGITAB WHERE
      TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
      TIDREGITAB.GODKAND NE "  " AND TIDREGITAB.VECKOKORD = "v845" AND
      TIDREGITAB.DATUM <= vkdatum AND TIDREGITAB.TIDLOG = TRUE
      USE-INDEX PSTART NO-LOCK. 
   END.   
   ELSE DO:
      OPEN QUERY tidq FOR EACH TIDREGITAB WHERE
      TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
      TIDREGITAB.GODKAND NE "  " AND TIDREGITAB.VECKOKORD = "v845" 
      AND TIDREGITAB.TIDLOG = TRUE
      USE-INDEX PSTART NO-LOCK. 
   END. 
    
   GET FIRST tidq NO-LOCK.         
   DO WHILE AVAILABLE(TIDREGITAB):         
      krav = 0.
      IF TIDREGITAB.GODKAND = '' THEN krav = 1.
      IF gvisatidpermanad = TRUE THEN DO:
         IF TIDREGITAB.DATUM > vkdatum THEN krav = 1. 
      END.   
      FIND FIRST FRDEL WHERE FRDEL.PERSONALKOD = personal AND
      FRDEL.AONR = TIDREGITAB.AONR USE-INDEX FRDEL NO-LOCK NO-ERROR.
      IF AVAILABLE FRDEL THEN DO:
         onr = FRDEL.AONR.
         dnr = FRDEL.DELNR.
         krav = 1.
      END.   
      FIND FIRST FVARO WHERE FVARO.AONR = TIDREGITAB.AONR
      AND FVARO.DELNR = TIDREGITAB.DELNR USE-INDEX FVARO NO-LOCK NO-ERROR.
      IF NOT AVAILABLE FVARO THEN krav = 1. 
      IF krav = 0 THEN DO:  
         ASSIGN
         rec = RECID(TIDREGITAB)
         onr2 = TIDREGITAB.AONR
         dnr2 = TIDREGITAB.DELNR.
         FIND FIRST FVARO WHERE FVARO.AONR = onr2
         AND FVARO.DELNR = dnr2 USE-INDEX FVARO NO-LOCK NO-ERROR.
         IF AVAILABLE FVARO THEN DO TRANSACTION:
            fkod = FVARO.FRKOD.
	     proc = FVARO.PROC.
	     del1 = FVARO.DEL.   /* ti-timmar pr-procent he-heldag */
	     FIND LAST franvaro WHERE franvaro.PERSONALKOD = personal AND
	     franvaro.AONR = onr2 AND franvaro.DELNR = dnr2
	     USE-INDEX franvaro EXCLUSIVE-LOCK NO-ERROR.
	     IF AVAILABLE franvaro THEN DO:
	        IF franvaro.FRAN > TIDREGITAB.DATUM AND
	        franvaro.TILL > TIDREGITAB.DATUM THEN DO:
	           FIND PREV franvaro WHERE franvaro.PERSONALKOD = personal AND
	           franvaro.AONR = onr2 AND franvaro.DELNR = dnr2
	           USE-INDEX franvaro EXCLUSIVE-LOCK NO-ERROR.
	        END.
	     END.                  
            IF NOT AVAILABLE franvaro THEN DO:
	        regdatum = TIDREGITAB.DATUM.
	        regvnr = TIDREGITAB.VECKONUMMER.
	        RUN SLUTARB.P.
	        nytid = regstart.
	        RUN TIMSEK.P.              /* raknar ut totala tiden */
	        starttiden = sekunder.
	        nytid = regslut.
	        RUN TIMSEK.P.
	        sluttiden = sekunder.
	        regdatum = TIDREGITAB.DATUM.
               regvnr = TIDREGITAB.VECKONUMMER.
	        RUN NORDFTO.P.
	        tott1 = nytid.
	        RUN TIMSEK.P.
	        tott2 = sekunder.
	        IF TIDREGITAB.START NE regstart OR TIDREGITAB.SLUT NE regslut
	        THEN DO:
	           nytid = TIDREGITAB.TOTALT.
	           RUN TIMSEK.P.
	           IF tott2 LE 0 THEN proc1 = 0.
	           ELSE proc1 = 100 * sekunder / tott2.
	           IF proc = TRUE THEN  antal = TIDREGITAB.TOTALT.
	           IF proc NE TRUE THEN antal = TIDREGITAB.TOTALT.
	        END.
	        ELSE DO:
	           proc1 = 100.
	           antal = 0.
	        END.         
	        regdat3 = TIDREGITAB.DATUM.
	        IF proc1 = 100  AND DAY(TIDREGITAB.DATUM) LE 3 AND
	        DAY(TIDREGITAB.DATUM) > 1 THEN DO:
	           regdat3 = TIDREGITAB.DATUM - 1 .
	           regdat5 = regdat3.
	           IF WEEKDAY(regdat3) = 1 OR WEEKDAY(regdat3) = 7 THEN regdat3 = regdat3 - 1.
	           ELSE DO:
	              FIND FIRST OVERAVTAB WHERE OVERAVTAB.DATUM = regdat3 AND
	              OVERAVTAB.KOD = ANSTFORMTAB.KOD USE-INDEX ODATUM NO-LOCK NO-ERROR.
	              IF AVAILABLE OVERAVTAB THEN DO:
	                 IF OVERAVTAB.EQDAG = 1 OR OVERAVTAB.EQDAG = 7 THEN regdat3 = regdat3 - 1.  
	              END.
	           END.                                  
                  DAT:
	           REPEAT:
	              IF regdat5 = regdat3 THEN LEAVE DAT.
	              IF regdat5 > regdat3 THEN DO:    
	                 regdat5 = regdat3.
                        IF WEEKDAY(regdat3) = 1 OR WEEKDAY(regdat3) = 7 THEN regdat3 = regdat3 - 1.  
                        ELSE DO:
	                    FIND FIRST OVERAVTAB WHERE OVERAVTAB.DATUM = regdat3 AND
	                    OVERAVTAB.KOD = ANSTFORMTAB.KOD USE-INDEX ODATUM NO-LOCK NO-ERROR.
	                    IF AVAILABLE OVERAVTAB THEN DO:
	                       IF OVERAVTAB.EQDAG = 1 OR OVERAVTAB.EQDAG = 7 THEN regdat3 = regdat3 - 1.
	                    END.
	                 END.
	              END.
	           END.   
	           IF DAY(regdat3) GE 26 THEN DO:           
	              FIND FIRST frbuff WHERE frbuff.PERSONALKOD =  personal AND frbuff.DATUM =
	              regdat3 AND frbuff.TIDLOG = TRUE AND frbuff.AONR = onr2 NO-LOCK NO-ERROR.
	              IF AVAILABLE frbuff THEN regdat3 = 
	              DATE(MONTH(TIDREGITAB.DATUM),01,YEAR(TIDREGITAB.DATUM)).
	              ELSE regdat3 = TIDREGITAB.DATUM.              
	           END.  	     
	           ELSE regdat3 = TIDREGITAB.DATUM. 
	        END.              	         
	        regdat4 = TIDREGITAB.DATUM.
               IF proc1 = 100  AND DAY(TIDREGITAB.DATUM) GE 26 AND
	        DAY(TIDREGITAB.DATUM + 1) > 1 THEN DO:
	           regdat4 = TIDREGITAB.DATUM + 1 .
	           regdat5 = regdat4.
	           IF WEEKDAY(regdat4) = 1 OR WEEKDAY(regdat4) = 7 THEN regdat4 = regdat4 + 1.
	           ELSE DO:
	              FIND FIRST OVERAVTAB WHERE OVERAVTAB.DATUM = regdat4 AND
	              OVERAVTAB.KOD = ANSTFORMTAB.KOD USE-INDEX ODATUM NO-LOCK NO-ERROR.
	              IF AVAILABLE OVERAVTAB THEN DO:
	                 IF OVERAVTAB.EQDAG = 1 OR OVERAVTAB.EQDAG = 7 THEN regdat4 = regdat4 + 1.  
	              END.
	           END. 
	           IF regdat4 = regdat5 THEN DO:
	              regdat4 = regdat4 - 1.
	              regdat5 = regdat4.
	           END.     
                  DAT2:
	           REPEAT:
                     IF regdat4 = regdat5 THEN LEAVE DAT2. 
 	              IF regdat4 > regdat5 THEN DO:
	                 regdat5 = regdat4.
                        IF WEEKDAY(regdat4) = 1 OR WEEKDAY(regdat4) = 7 THEN regdat4 = regdat4 + 1. 
                        ELSE DO:
	                    FIND FIRST OVERAVTAB WHERE OVERAVTAB.DATUM = regdat4 AND
	                    OVERAVTAB.KOD = ANSTFORMTAB.KOD USE-INDEX ODATUM NO-LOCK NO-ERROR.
	                    IF AVAILABLE OVERAVTAB THEN DO:
	                       IF OVERAVTAB.EQDAG = 1 OR OVERAVTAB.EQDAG = 7 THEN regdat4 = regdat4 + 1.
	                    END.
	                 END.    
	              END.
	           END.   
	           IF DAY(regdat4) LE 3 THEN DO:           
	              FIND FIRST frbuff WHERE frbuff.PERSONALKOD =  personal AND frbuff.DATUM =
	              regdat4 AND frbuff.TIDLOG = TRUE AND frbuff.AONR = onr2 NO-LOCK NO-ERROR.
	              IF AVAILABLE frbuff THEN DO:
	                 IF MONTH(TIDREGITAB.DATUM) = 12 THEN DO:  
	                    regdat5 = DATE(01,01,YEAR(TIDREGITAB.DATUM) + 1).
	                    regdat4 = regdat5 - 1.
	                 END.
	                 ELSE DO:
	                    regdat5 = DATE(MONTH(TIDREGITAB.DATUM) + 1,01,YEAR(TIDREGITAB.DATUM)).
	                    regdat4 = regdat5 - 1.
	                 END.
	              END.  
	              ELSE regdat4 = TIDREGITAB.DATUM.              
	           END.                       
	           ELSE regdat4 = TIDREGITAB.DATUM.  
	        END.  	    
	        CREATE franvaro.
               ASSIGN franvaro.PERSONALKOD = personal franvaro.AONR = onr2
               franvaro.DELNR = dnr2 franvaro.LART = fkod
	        franvaro.FRAN = regdat3 franvaro.TILL = regdat4
               franvaro.PROCENT = proc1 franvaro.TIMMAR = antal franvaro.PKOD = anst.
	     END.
            ELSE DO:
               regdatum = TIDREGITAB.DATUM - 1.
	        RUN REGVEC.P.
	        RUN SLUTARB.P.
	        IF regstart = regslut THEN DO:
	           REPEAT:
                     IF regstart ne regslut THEN LEAVE.
	              regdatum = regdatum - 1.
                     RUN REGVEC.P.
	              RUN SLUTARB.P.
	           END.
	        END.
	        regdatum2 = regdatum.
	        regdatum = TIDREGITAB.DATUM.
	        regvnr = TIDREGITAB.VECKONUMMER.
	        RUN SLUTARB.P.
	        nytid = regstart.
	        RUN TIMSEK.P.              /* raknar ut totala tiden */
	        starttiden = sekunder.
	        nytid = regslut.
	        RUN TIMSEK.P.
	        sluttiden = sekunder. 
               regdatum = TIDREGITAB.DATUM.
               regvnr = TIDREGITAB.VECKONUMMER.
               RUN NORDFTO.P.
	        tott1 = nytid.
               RUN TIMSEK.P.
               tott2 = sekunder. 
	        IF franvaro.TILL < regdatum2
	        OR franvaro.PROCENT < 100
	        OR TIDREGITAB.TOTALT < tott1 THEN DO:
	           IF TIDREGITAB.START NE regstart OR TIDREGITAB.SLUT NE regslut THEN DO:
	              nytid = TIDREGITAB.TOTALT.
	              RUN TIMSEK.P.
	              IF tott2 LE 0 THEN proc1 = 0.
	              ELSE proc1 = 100 * sekunder / tott2.
	              IF proc = TRUE THEN antal = nytid.
	              IF proc NE TRUE THEN antal = nytid.
	           END.
	           ELSE DO:
	              proc1 = 100.
	              antal = 0.
	           END.        
                  regdat3 = TIDREGITAB.DATUM.
	           IF proc1 = 100  AND DAY(TIDREGITAB.DATUM) LE 3 AND
                  DAY(TIDREGITAB.DATUM) > 1 THEN DO:
                     regdat3 = TIDREGITAB.DATUM - 1 .
	              regdat5 = regdat3.
	              IF WEEKDAY(regdat3) = 1 OR WEEKDAY(regdat3) = 7 THEN regdat3 = regdat3 - 1.
	              ELSE DO:
	                 FIND FIRST OVERAVTAB WHERE OVERAVTAB.DATUM = regdat3 AND
	                 OVERAVTAB.KOD = ANSTFORMTAB.KOD USE-INDEX ODATUM NO-LOCK NO-ERROR.
	                 IF AVAILABLE OVERAVTAB THEN DO:
	                    IF OVERAVTAB.EQDAG = 1 OR OVERAVTAB.EQDAG = 7 THEN regdat3 = regdat3 - 1.  
	                 END.
	              END.                                  
	              DAT3:
  	              REPEAT:
  	                 IF regdat5 = regdat3 THEN LEAVE DAT3.
                        IF regdat5 > regdat3 THEN DO:    
	                    regdat5 = regdat3.
                           IF WEEKDAY(regdat3) = 1 OR WEEKDAY(regdat3) = 7 THEN regdat3 = regdat3 - 1.  
                           ELSE DO:
	                       FIND FIRST OVERAVTAB WHERE OVERAVTAB.DATUM = regdat3 AND
	                       OVERAVTAB.KOD = ANSTFORMTAB.KOD USE-INDEX ODATUM NO-LOCK NO-ERROR.
	                       IF AVAILABLE OVERAVTAB THEN DO:
	                          IF OVERAVTAB.EQDAG = 1 OR OVERAVTAB.EQDAG = 7 THEN regdat3 = regdat3 - 1.
	                       END.
	                    END.
	                 END.
	              END.   
	              IF DAY(regdat3) GE 26 THEN DO:           
	                 FIND FIRST frbuff WHERE frbuff.PERSONALKOD =  personal AND frbuff.DATUM =
	                 regdat3 AND frbuff.TIDLOG = TRUE AND frbuff.AONR = onr2 NO-LOCK NO-ERROR.
	                 IF AVAILABLE frbuff THEN regdat3 = 
	                 DATE(MONTH(TIDREGITAB.DATUM),01,YEAR(TIDREGITAB.DATUM)).
      	                 ELSE regdat3 = TIDREGITAB.DATUM.              
	              END.    	   
	              ELSE regdat3 = TIDREGITAB.DATUM.  
	           END.              	         
	           regdat4 = TIDREGITAB.DATUM.
                  IF proc1 = 100  AND DAY(TIDREGITAB.DATUM) GE 26 AND
	           DAY(TIDREGITAB.DATUM + 1) > 1 THEN DO:
	              regdat4 = TIDREGITAB.DATUM + 1 .
	              regdat5 = regdat4.
	              IF WEEKDAY(regdat4) = 1 OR WEEKDAY(regdat4) = 7 THEN regdat4 = regdat4 + 1.
	              ELSE DO:
	                 FIND FIRST OVERAVTAB WHERE OVERAVTAB.DATUM = regdat4 AND
	                 OVERAVTAB.KOD = ANSTFORMTAB.KOD USE-INDEX ODATUM NO-LOCK NO-ERROR.
                        IF AVAILABLE OVERAVTAB THEN DO:
 	                    IF OVERAVTAB.EQDAG = 1 OR OVERAVTAB.EQDAG = 7 THEN regdat4 = regdat4 + 1.  
	                 END.
	              END.   
	              IF regdat4 = regdat5 THEN DO:
	                 regdat4 = regdat4 - 1.
	                 regdat5 = regdat4.
	              END.     
	              DAT4:
	              REPEAT:
	                 IF regdat4 = regdat5 THEN LEAVE DAT4. 
 	                 IF regdat4 > regdat5 THEN DO:
	                    regdat5 = regdat4.
                           IF WEEKDAY(regdat4) = 1 OR WEEKDAY(regdat4) = 7 THEN regdat4 = regdat4 + 1. 
                           ELSE DO:
	                       FIND FIRST OVERAVTAB WHERE OVERAVTAB.DATUM = regdat4 AND
	                       OVERAVTAB.KOD = ANSTFORMTAB.KOD USE-INDEX ODATUM NO-LOCK NO-ERROR.
	                       IF AVAILABLE OVERAVTAB THEN DO:
	                          IF OVERAVTAB.EQDAG = 1 OR OVERAVTAB.EQDAG = 7 THEN regdat4 = regdat4 + 1.
	                       END.
  	                    END.      
	                 END.
	              END.   
	              IF DAY(regdat4) LE 3 THEN DO:           
	                 FIND FIRST frbuff WHERE frbuff.PERSONALKOD =  personal AND frbuff.DATUM =
	                 regdat4 AND frbuff.TIDLOG = TRUE AND frbuff.AONR = onr2 NO-LOCK NO-ERROR.
	                 IF AVAILABLE frbuff THEN DO: 
	                    IF MONTH(TIDREGITAB.DATUM) = 12 THEN DO:  
	                       regdat5 = DATE(01,01,YEAR(TIDREGITAB.DATUM) + 1).
	                       regdat4 = regdat5 - 1.
	                    END.
	                    ELSE DO:
	                       regdat5 = DATE(MONTH(TIDREGITAB.DATUM) + 1,01,YEAR(TIDREGITAB.DATUM)).
	                       regdat4 = regdat5 - 1.
	                    END.   
	                 END.  
	                 ELSE regdat4 = TIDREGITAB.DATUM.              
	              END.                              
	              ELSE regdat4 = TIDREGITAB.DATUM.  
	           END.  	    
                  CREATE franvaro.
	           ASSIGN franvaro.PERSONALKOD = personal franvaro.AONR = onr2
	           franvaro.DELNR = dnr2 franvaro.LART = fkod
	           franvaro.FRAN = regdat3 franvaro.TILL = regdat4
	           franvaro.PROCENT = proc1 franvaro.TIMMAR = antal franvaro.PKOD = anst.
	        END.
               ELSE DO:      
	           regdat4 = TIDREGITAB.DATUM.
                  IF proc1 = 100  AND DAY(TIDREGITAB.DATUM) GE 26 AND
	           DAY(TIDREGITAB.DATUM + 1) > 1 THEN DO:
	              regdat4 = TIDREGITAB.DATUM + 1 .
	              regdat5 = regdat4.
     	              IF WEEKDAY(regdat4) = 1 OR WEEKDAY(regdat4) = 7 THEN regdat4 = regdat4 + 1.
	              ELSE DO:
	                 FIND FIRST OVERAVTAB WHERE OVERAVTAB.DATUM = regdat4 AND
	                 OVERAVTAB.KOD = ANSTFORMTAB.KOD USE-INDEX ODATUM NO-LOCK NO-ERROR.
	                 IF AVAILABLE OVERAVTAB THEN DO:
	                    IF OVERAVTAB.EQDAG = 1 OR OVERAVTAB.EQDAG = 7 THEN regdat4 = regdat4 + 1.  
	                 END.
	              END.        
	              IF regdat4 = regdat5 THEN DO:
	                 regdat4 = regdat4 - 1.
	                 regdat5 = regdat4.
                     END.
	              DAT5:
                     REPEAT:
	                 IF regdat4 = regdat5 THEN LEAVE DAT5. 
 	                 IF regdat4 > regdat5 THEN DO:
	                    regdat5 = regdat4.
                           IF WEEKDAY(regdat4) = 1 OR WEEKDAY(regdat4) = 7 THEN regdat4 = regdat4 + 1. 
                           ELSE DO:
	                       FIND FIRST OVERAVTAB WHERE OVERAVTAB.DATUM = regdat4 AND
	                       OVERAVTAB.KOD = ANSTFORMTAB.KOD USE-INDEX ODATUM NO-LOCK NO-ERROR.
	                       IF AVAILABLE OVERAVTAB THEN DO:
	                          IF OVERAVTAB.EQDAG = 1 OR OVERAVTAB.EQDAG = 7 THEN regdat4 = regdat4 + 1.
	                       END.
	                    END.    
	                 END.
	              END.   
	              IF DAY(regdat4) LE 3 THEN DO:           
	                 FIND FIRST frbuff WHERE frbuff.PERSONALKOD =  personal AND frbuff.DATUM =
	                 regdat4 AND frbuff.TIDLOG = TRUE AND frbuff.AONR = onr2 NO-LOCK NO-ERROR.
	                 IF AVAILABLE frbuff THEN DO:
	                    IF MONTH(TIDREGITAB.DATUM) = 12 THEN DO:  
	                       regdat5 = DATE(01,01,YEAR(TIDREGITAB.DATUM) + 1).
	                       regdat4 = regdat5 - 1.
	                    END.
	                    ELSE DO:
	                       regdat5 = DATE(MONTH(TIDREGITAB.DATUM) + 1,01,YEAR(TIDREGITAB.DATUM)).
	                       regdat4 = regdat5 - 1.
	                    END.   
	                 END.  
	                 ELSE regdat4 = TIDREGITAB.DATUM.              
	              END.
	              ELSE regdat4 = TIDREGITAB.DATUM.   
	           END.  	    
                  ASSIGN franvaro.TILL = regdat4.
               END. 
            END.
            IF del1 = FALSE AND franvaro.TIMMAR > 0 THEN DO:
               CREATE frfel.
               ASSIGN frfel.PERSONALKOD = franvaro.PERSONALKOD frfel.AONR = franvaro.AONR
               frfel.DELNR = franvaro.DELNR frfel.LART = franvaro.LART 
               frfel.FRAN =   franvaro.FRAN frfel.TILL = franvaro.TILL 
               frfel.PROCENT = franvaro.PROCENT frfel.TIMMAR = franvaro.TIMMAR.
            END.      
         END.   	    
      END.
      GET NEXT tidq NO-LOCK.
   END.
   GET NEXT persq NO-LOCK.
END.
DO TRANSACTION:
   FIND FIRST franvaro WHERE franvaro.PERSONALKOD = " " OR franvaro.PERSONALKOD =
   "00000" USE-INDEX franvaro EXCLUSIVE-LOCK NO-ERROR.
   IF AVAILABLE franvaro THEN DELETE franvaro.
END.
REPEAT TRANSACTION:
   FIND NEXT franvaro WHERE franvaro.PERSONALKOD = " " OR franvaro.PERSONALKOD =
   "000000-0000"  USE-INDEX franvaro EXCLUSIVE-LOCK NO-ERROR.
   IF NOT AVAILABLE franvaro THEN LEAVE.
   ELSE DELETE franvaro.
END. 
IF globforetag = "GRAN" THEN DO:
   OUTPUT TO \\GRANGURU\guru_ser\PRO8S\gran\fransu.d NO-ECHO.
END. 
ELSE IF globforetag = "GADM" THEN DO:
   OUTPUT TO \\GRANGURU\guru_ser\PRO8S\gadm\fransu.d NO-ECHO.
END. 
ELSE IF globforetag = "GSYD" THEN DO:
   OUTPUT TO \\GRANGURU\guru_ser\PRO8S\gsyd\fransu.d NO-ECHO.
END. 
ELSE DO:
   OUTPUT TO C:\GURU\fransu.d NO-ECHO.
END.   
FOR EACH franvaro USE-INDEX franvaro  NO-LOCK:
   EXPORT franvaro.
END.
IF globforetag = "GRAN" THEN DO:
   OUTPUT TO \\GRANGURU\guru_ser\PRO8S\gran\franfel.d NO-ECHO.
END.    
ELSE IF globforetag = "GADM" THEN DO:
   OUTPUT TO \\GRANGURU\guru_ser\PRO8S\gadm\franfel.d NO-ECHO.
END.
ELSE IF globforetag = "GSYD" THEN DO:
   OUTPUT TO \\GRANGURU\guru_ser\PRO8S\gsyd\franfel.d NO-ECHO.
END.
ELSE DO:
   OUTPUT TO C:\GURU\franfel.d NO-ECHO.
END.   
FOR EACH frfel USE-INDEX franvaro  NO-LOCK:
   EXPORT frfel.
END.
