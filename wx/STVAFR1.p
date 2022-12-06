    /*STvaFR1.P körs ej*/
DEFINE INPUT PARAMETER kordatum AS DATE NO-UNDO.
DEFINE INPUT PARAMETER koranv LIKE ANVANDARE.ANVANDARE NO-UNDO.
/*DEFINE NEW SHARED VARIABLE globanv LIKE ANVANDARE.ANVANDARE NO-UNDO.*/
DEFINE NEW SHARED VARIABLE globforetag LIKE FORETAG.FORETAG NO-UNDO.
DEFINE NEW SHARED VARIABLE persrec AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE vkdatum AS DATE NO-UNDO.
DEFINE NEW SHARED VARIABLE gvisatidpermanad AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE regvnr AS INTEGER FORMAT "999" NO-UNDO.
DEFINE NEW SHARED VARIABLE regdatum AS DATE NO-UNDO.
DEFINE NEW SHARED VARIABLE regstart LIKE TIDREGITAB.START NO-UNDO. 
DEFINE NEW SHARED VARIABLE regslut LIKE TIDREGITAB.SLUT NO-UNDO.
DEFINE NEW SHARED VARIABLE nytid AS DECIMAL FORMAT "99.99" NO-UNDO.
DEFINE NEW SHARED VARIABLE sekunder AS INTEGER FORMAT "-9999999" NO-UNDO.
DEFINE NEW SHARED VARIABLE regdagnamn AS CHARACTER FORMAT "X(3)" NO-UNDO.        
DEFINE NEW SHARED VARIABLE regtotalt LIKE TIDREGITAB.TOTALT NO-UNDO.
DEFINE NEW SHARED VARIABLE frustarten AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE fruslutet AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE kaffestart AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE kaffeslut AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE lunchstarten AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE lunchslutet AS DECIMAL NO-UNDO.


DEFINE VARIABLE onr LIKE TIDREGITAB.AONR NO-UNDO.
DEFINE VARIABLE onr2 LIKE TIDREGITAB.AONR NO-UNDO.
DEFINE VARIABLE dnr LIKE TIDREGITAB.DELNR NO-UNDO.
DEFINE VARIABLE dnr2 LIKE TIDREGITAB.DELNR NO-UNDO.
DEFINE VARIABLE starttiden AS INTEGER NO-UNDO.
DEFINE VARIABLE sluttiden AS INTEGER NO-UNDO.
DEFINE VARIABLE antal LIKE TIDREGITAB.LONTILLANTAL NO-UNDO.   /*LON*/
DEFINE VARIABLE antalet LIKE PHJALP.PPAR8 NO-UNDO.   /*LON*/
DEFINE VARIABLE personal LIKE PERSONALTAB.PERSONALKOD NO-UNDO.
DEFINE VARIABLE pnr LIKE PERSONALTAB.ANSTNR NO-UNDO.      /*LON*/
DEFINE VARIABLE vecknr LIKE TIDREGITAB.VECKONUMMER NO-UNDO.      /*LON*/
DEFINE VARIABLE pc LIKE FVARO.PROC NO-UNDO.
DEFINE VARIABLE fkod LIKE FVARO.FRKOD NO-UNDO.
DEFINE VARIABLE del1 LIKE FVARO.DEL NO-UNDO.
DEFINE VARIABLE regdatum2 AS DATE NO-UNDO.  
DEFINE VARIABLE regdat3 AS DATE NO-UNDO.
DEFINE VARIABLE regdat4 AS DATE NO-UNDO. 
DEFINE VARIABLE regdat5 AS DATE NO-UNDO.
DEFINE VARIABLE rec AS RECID NO-UNDO.
DEFINE VARIABLE tott1 AS DECIMAL FORMAT "99.99" NO-UNDO.
DEFINE VARIABLE tott2 AS INTEGER NO-UNDO.
DEFINE VARIABLE proc1 AS INTEGER FORMAT "999" NO-UNDO.
DEFINE VARIABLE seku AS INTEGER NO-UNDO.
DEFINE VARIABLE anst LIKE ANSTFORM.KOD NO-UNDO.
DEFINE VARIABLE krav AS INTEGER FORMAT "9" NO-UNDO.
DEFINE VARIABLE frstart LIKE TIDREGITAB.START NO-UNDO.
DEFINE VARIABLE frslut LIKE TIDREGITAB.START NO-UNDO.


DEFINE BUFFER frbuff FOR TIDREGITAB.
DEFINE NEW SHARED TEMP-TABLE franvaro
   FIELD ANSTNR LIKE PERSONALTAB.ANSTNR
   FIELD AONR LIKE TIDREGITAB.AONR
   FIELD DELNR LIKE TIDREGITAB.DELNR
   FIELD FRAN LIKE TIDREGITAB.DATUM
   FIELD TILL LIKE TIDREGITAB.DATUM
   FIELD PROCENT AS INTEGER FORMAT "999"
   FIELD TIMMAR AS DECIMAL FORMAT "99999.99"
   FIELD LART AS CHARACTER FORMAT "X(4)"
   FIELD STARTKL LIKE TIDREGITAB.START
   FIELD SLUTKL LIKE TIDREGITAB.SLUT
   INDEX FRANVARO IS PRIMARY  ANSTNR FRAN LART ASCENDING.
DEFINE TEMP-TABLE frfel
   FIELD ANSTNR LIKE PERSONALTAB.ANSTNR
   FIELD AONR LIKE TIDREGITAB.AONR
   FIELD DELNR LIKE TIDREGITAB.DELNR
   FIELD FRAN LIKE TIDREGITAB.DATUM
   FIELD TILL LIKE TIDREGITAB.DATUM
   FIELD PROCENT AS INTEGER FORMAT "999"
   FIELD TIMMAR AS DECIMAL FORMAT "99.99"
   FIELD LART AS CHARACTER FORMAT "X(4)"
   INDEX FRANVARO IS PRIMARY  ANSTNR FRAN LART ASCENDING. 
DEFINE NEW SHARED TEMP-TABLE pa90fil
   FIELD PANSTNR LIKE PERSONALTAB.ANSTNR
   FIELD START LIKE TIDREGITAB.START
   FIELD SLUT LIKE TIDREGITAB.SLUT
   FIELD PLONTILLAGG LIKE TIDREGITAB.LONTILLAGG
   FIELD PLONTILLANTAL LIKE TIDREGITAB.LONTILLANTAL
   FIELD POVERTIDTILL LIKE TIDREGITAB.OVERTIDTILL
   FIELD POVERANTAL LIKE TIDREGITAB.OVERANTAL
   FIELD PTRAKTKOD LIKE TIDREGITAB.TRAKTKOD
   FIELD PTRAKTANTAL LIKE TIDREGITAB.TRAKTANTAL
   FIELD PBEREDSKAP LIKE TIDREGITAB.BEREDSKAP
   FIELD PBERANTAL LIKE TIDREGITAB.BERANTAL
   FIELD PDATUM LIKE TIDREGITAB.DATUM
   FIELD SLUTDATUM LIKE TIDREGITAB.DATUM INITIAL ?
   FIELD PVECKONUMMER LIKE TIDREGITAB.VECKONUMMER
   FIELD MANAD AS INTEGER
   FIELD PSORT AS CHARACTER FORMAT "XX"
   FIELD PPAR8 AS DECIMAL FORMAT "-99.99"
   FIELD AONR LIKE TIDREGITAB.AONR
   FIELD DELNR LIKE TIDREGITAB.DELNR
   FIELD PAVTAL AS CHARACTER
   FIELD TID AS LOGICAL
   FIELD BERKOLL AS LOGICAL INITIAL FALSE
   INDEX PANSTNR IS PRIMARY PANSTNR ASCENDING.
DEFINE INPUT PARAMETER TABLE FOR pa90fil.
FIND FIRST FORETAG USE-INDEX FORETAG NO-LOCK NO-ERROR.
globforetag = FORETAG.FORETAG.
globanv = koranv.
vkdatum = kordatum.
gvisatidpermanad = TRUE.    
{AMERICANEUROPEAN.I}  
OPEN QUERY persq FOR EACH PERSONALTAB WHERE PERSONALTAB.PERSMASK = TRUE 
USE-INDEX PERSONALKOD NO-LOCK.
GET FIRST persq NO-LOCK.
DO WHILE AVAILABLE(PERSONALTAB):
   persrec = RECID(PERSONALTAB).
   personal = PERSONALTAB.PERSONALKOD.
   pnr = PERSONALTAB.ANSTNR.
   FIND FIRST ANSTFORMTAB WHERE ANSTFORMTAB.ANSTALLNING =
   PERSONALTAB.ANSTALLNING USE-INDEX ANSTF NO-LOCK NO-ERROR.
   anst = ANSTFORMTAB.KOD.
   IF gvisatidpermanad = TRUE THEN DO:
      OPEN QUERY tidq FOR EACH TIDREGITAB WHERE
      TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
      TIDREGITAB.GODKAND BEGINS "G" AND TIDREGITAB.VECKOKORD = "" AND
      TIDREGITAB.DATUM <= vkdatum AND TIDREGITAB.TIDLOG = TRUE
      USE-INDEX PSTART NO-LOCK. 
   END.   
  
    
   GET FIRST tidq NO-LOCK.         
   DO WHILE AVAILABLE(TIDREGITAB):         
      krav = 0.             
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
      regdatum = TIDREGITAB.DATUM.
      regvnr = TIDREGITAB.VECKONUMMER.
      RUN SLUTARB.P.
      IF regstart = regslut THEN krav = 1.
      IF regstart NE regslut THEN DO:
         IF TIDREGITAB.START GE regslut AND TIDREGITAB.SLUT > regslut THEN krav = 1.
         IF TIDREGITAB.START < regstart AND TIDREGITAB.SLUT LE regstart THEN krav = 1.
      END.   
      IF krav = 0 THEN DO:  
         ASSIGN
         rec = RECID(TIDREGITAB)
         onr2 = TIDREGITAB.AONR
         dnr2 = TIDREGITAB.DELNR.
         FIND FIRST FVARO WHERE FVARO.AONR = onr2
         AND FVARO.DELNR = dnr2 USE-INDEX FVARO NO-LOCK NO-ERROR.
         IF AVAILABLE FVARO THEN DO TRANSACTION:	
            ASSIGN
            fkod = FVARO.FRKOD
	     pc = FVARO.PROC
	     del1 = FVARO.DEL   /* ti-timmar pr-procent he-heldag */
            frstart = TIDREGITAB.START
            frslut = TIDREGITAB.SLUT. 
            FIND LAST franvaro WHERE franvaro.ANSTNR = pnr AND
	     franvaro.AONR = onr2 AND franvaro.DELNR = dnr2
	     USE-INDEX franvaro EXCLUSIVE-LOCK NO-ERROR.
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
     	        IF regstart = regslut THEN DO:  /*FRÅNVAROREG  HELGDAG*/
     	           ASSIGN
     	           proc1 = 000
     	           antal = 0. 
     	        END.  
	        ELSE IF TIDREGITAB.START NE regstart OR TIDREGITAB.SLUT NE regslut
	        THEN DO:
	           nytid = TIDREGITAB.TOTALT.
	           RUN TIMSEK.P.
	           IF tott2 LE 0 THEN proc1 = 0.
	           ELSE DO:
	              proc1 = 100 * sekunder / tott2.
                    /* ASSIGN
	              frstart = TIDREGITAB.START
	              frslut = TIDREGITAB.SLUT.*/
	           END.   
	           IF pc = TRUE THEN DO:  /* TIMMAR SKALL UT*/
	              proc1 = 0.      
	              antal = TIDREGITAB.TOTALT.
	           END.
	           IF pc NE TRUE THEN DO:  /* PROCENT SKALL UT*/	           
	              antal = 0.
	           END.
	        END.
	        ELSE IF pc = TRUE THEN DO:
	           proc1 = 0.      
	           antal = TIDREGITAB.TOTALT.
	        END.
	        ELSE DO:
	           ASSIGN
	           proc1 = 100
                  antal = 0.
	        END.         
               regdat3 = TIDREGITAB.DATUM.
               regdat4 = TIDREGITAB.DATUM.
	        IF proc1 = 100  AND DAY(TIDREGITAB.DATUM) LE 3  AND
	        DAY(TIDREGITAB.DATUM) > 1 THEN DO:
	           ASSIGN
	           regdat3 = TIDREGITAB.DATUM - 1 
	           regdat5 = regdat3.
	           IF WEEKDAY(regdat3) = 1 OR WEEKDAY(regdat3) = 7 THEN regdat3 = regdat3 - 1.
	           ELSE DO:
	              FIND FIRST OVERAVTAB WHERE OVERAVTAB.DATUM = regdat3 AND
	              OVERAVTAB.KOD = ANSTFORMTAB.KOD USE-INDEX ODATUM NO-LOCK NO-ERROR.
	              IF AVAILABLE OVERAVTAB THEN DO:
	                 IF OVERAVTAB.EQDAG = 1 OR OVERAVTAB.EQDAG = 7 THEN regdat3 = regdat3 - 1.  
	              END.
	           END.                                  
	           dat:
	           REPEAT:
	              IF regdat5 = regdat3 THEN LEAVE dat.
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
               ELSE IF proc1 = 100  AND DAY(TIDREGITAB.DATUM) GE 26 AND
               DAY(TIDREGITAB.DATUM + 1) > 1 THEN DO:
	           ASSIGN
	           regdat4 = TIDREGITAB.DATUM + 1 
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
	              ASSIGN
	              regdat4 = regdat4 - 1
	              regdat5 = regdat4.
	           END.     
	           dat2:
	           REPEAT:
	              IF regdat4 = regdat5 THEN LEAVE dat2. 
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
	                    ASSIGN
	                    regdat5 = DATE(01,01,YEAR(TIDREGITAB.DATUM) + 1)
	                    regdat4 = regdat5 - 1.
	                 END.
	                 ELSE DO:
	                    ASSIGN
	                    regdat5 = DATE(MONTH(TIDREGITAB.DATUM) + 1,01,YEAR(TIDREGITAB.DATUM))
	                    regdat4 = regdat5 - 1.
	                 END.   
	              END.  
	              ELSE regdat4 = TIDREGITAB.DATUM.              
	           END.              
                  ELSE regdat4 = TIDREGITAB.DATUM.                    
                  IF proc1 = 100  AND WEEKDAY(TIDREGITAB.DATUM) = 2 THEN DO: /*HELG BAKÅT*/
	              ASSIGN
	              regdat3 = TIDREGITAB.DATUM - 1 
	              regdat5 = regdat3.
	              IF WEEKDAY(regdat3) = 1 OR WEEKDAY(regdat3) = 7 THEN regdat3 = regdat3 - 1.
	              ELSE DO:
	                 FIND FIRST OVERAVTAB WHERE OVERAVTAB.DATUM = regdat3 AND
	                 OVERAVTAB.KOD = ANSTFORMTAB.KOD USE-INDEX ODATUM NO-LOCK NO-ERROR.
	                 IF AVAILABLE OVERAVTAB THEN DO:
	                    IF OVERAVTAB.EQDAG = 1 OR OVERAVTAB.EQDAG = 7 THEN regdat3 = regdat3 - 1.  
	                 END.
	              END.                                  
	              dat:
	              REPEAT:
	                 IF regdat5 = regdat3 THEN LEAVE dat.
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
                     FIND FIRST frbuff WHERE frbuff.PERSONALKOD =  personal AND frbuff.DATUM =
	              regdat3 AND frbuff.TIDLOG = TRUE AND frbuff.AONR = onr2
	              AND frbuff.TOTALT = tott1 NO-LOCK NO-ERROR.
	              IF AVAILABLE frbuff THEN regdat3 = regdat3 + 1 .	              
                     ELSE regdat3 = TIDREGITAB.DATUM.              	         	     	      
	           END.                                  
	        END.    	    
	        ELSE IF proc1 = 100  AND WEEKDAY(TIDREGITAB.DATUM) = 2 THEN DO: /*HELG BAKÅT*/
	           ASSIGN
	           regdat3 = TIDREGITAB.DATUM - 1 
	           regdat5 = regdat3.
	           IF WEEKDAY(regdat3) = 1 OR WEEKDAY(regdat3) = 7 THEN regdat3 = regdat3 - 1.
	           ELSE DO:
	              FIND FIRST OVERAVTAB WHERE OVERAVTAB.DATUM = regdat3 AND
	              OVERAVTAB.KOD = ANSTFORMTAB.KOD USE-INDEX ODATUM NO-LOCK NO-ERROR.
	              IF AVAILABLE OVERAVTAB THEN DO:
	                 IF OVERAVTAB.EQDAG = 1 OR OVERAVTAB.EQDAG = 7 THEN regdat3 = regdat3 - 1.  
	              END.
	           END.                                  
	           dat:
	           REPEAT:
	              IF regdat5 = regdat3 THEN LEAVE dat.
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
                  FIND FIRST frbuff WHERE frbuff.PERSONALKOD =  personal AND frbuff.DATUM =
	           regdat3 AND frbuff.TIDLOG = TRUE AND frbuff.AONR = onr2
	           AND frbuff.TOTALT = tott1 NO-LOCK NO-ERROR.
	           IF AVAILABLE frbuff THEN regdat3 = regdat3 + 1 .	              
                  ELSE regdat3 = TIDREGITAB.DATUM.              	         	     	      
	        END.              	         	        	        
	        CREATE franvaro.
	        ASSIGN franvaro.ANSTNR = pnr franvaro.AONR = onr2
	        franvaro.DELNR = dnr2 franvaro.LART = fkod
	        franvaro.FRAN = regdat3 franvaro.TILL = regdat4
	        franvaro.PROCENT = proc1 franvaro.TIMMAR = antal.
	        IF proc1 > 0 AND proc1 < 100 THEN DO:
 	           ASSIGN
 	           franvaro.STARTKL = frstart 
	           franvaro.SLUTKL = frslut.
	           IF franvaro.LART = "3301" THEN ASSIGN franvaro.LART = "3300".
	        END.   
            ASSIGN
 	        franvaro.STARTKL = frstart 
	        franvaro.SLUTKL = frslut.  
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
	        ASSIGN
	        regdatum2 = regdatum
	        regdatum = TIDREGITAB.DATUM
	        regvnr = TIDREGITAB.VECKONUMMER.
	        RUN SLUTARB.P.	   
	        nytid = regstart.
	        RUN TIMSEK.P. 
	        ASSIGN             /* raknar ut totala tiden */
	        starttiden = sekunder
	        nytid = regslut.
	        RUN TIMSEK.P.
	        ASSIGN
	        sluttiden = sekunder
	        regdatum = TIDREGITAB.DATUM
	        regvnr = TIDREGITAB.VECKONUMMER.
	        RUN NORDFTO.P.
	        tott1 = nytid.
	        RUN TIMSEK.P.
	        tott2 = sekunder.	   
	        IF franvaro.TILL < regdatum2
	        OR franvaro.PROCENT < 100
	        OR TIDREGITAB.TOTALT < tott1 THEN DO:
	           IF TIDREGITAB.START NE regstart OR TIDREGITAB.SLUT NE regslut
	           THEN DO:
	              nytid = TIDREGITAB.TOTALT.
	              RUN TIMSEK.P.
	              IF tott2 LE 0 THEN proc1 = 0.
	              ELSE proc1 = 100 * sekunder / tott2.
	              IF pc = TRUE THEN DO:  /*TIMMAR */
		          proc1 = 0.
		          antal = nytid.
	              END.
	              IF pc NE TRUE THEN DO:  /*PROCENT*/
	                 /* proc1 = 0.
		          antal = nytid.*/
		          antal = 0.
	              END.
	           END.	    
	           ELSE IF pc = TRUE THEN DO:  /*TIMMAR */
		       proc1 = 0.
		       antal = nytid.
	           END.
	           ELSE DO:
	              ASSIGN
	              proc1 = 100
	              antal = 0.
	           END.        
	           regdat3 = TIDREGITAB.DATUM.
	           IF proc1 = 100  AND DAY(TIDREGITAB.DATUM) LE 3 AND
	           DAY(TIDREGITAB.DATUM) > 1 THEN DO:
	              ASSIGN
	              regdat3 = TIDREGITAB.DATUM - 1 
	              regdat5 = regdat3.
	              IF WEEKDAY(regdat3) = 1 OR WEEKDAY(regdat3) = 7 THEN regdat3 = regdat3 - 1.
	              ELSE DO:
	                 FIND FIRST OVERAVTAB WHERE OVERAVTAB.DATUM = regdat3 AND
	                 OVERAVTAB.KOD = ANSTFORMTAB.KOD USE-INDEX ODATUM NO-LOCK NO-ERROR.
	                 IF AVAILABLE OVERAVTAB THEN DO:
	                    IF OVERAVTAB.EQDAG = 1 OR OVERAVTAB.EQDAG = 7 THEN regdat3 = regdat3 - 1.  
	                 END.
	              END.                                  
	              dat:
  	              REPEAT:
	                 IF regdat5 = regdat3 THEN LEAVE dat.
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
	              ASSIGN
	              regdat4 = TIDREGITAB.DATUM + 1 
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
	                 ASSIGN
	                 regdat4 = regdat4 - 1
	                 regdat5 = regdat4.
	              END.     
	              dat2:
	              REPEAT:
	                 IF regdat4 = regdat5 THEN LEAVE dat2. 
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
	                       ASSIGN
	                       regdat5 = DATE(01,01,YEAR(TIDREGITAB.DATUM) + 1)
	                       regdat4 = regdat5 - 1.
	                    END.
	                    ELSE DO:
	                       ASSIGN
	                       regdat5 = DATE(MONTH(TIDREGITAB.DATUM) + 1,01,YEAR(TIDREGITAB.DATUM))
  	                       regdat4 = regdat5 - 1.
	                    END.   
	                 END.  
	                 ELSE regdat4 = TIDREGITAB.DATUM.              
	              END.                              
	              ELSE regdat4 = TIDREGITAB.DATUM.  
	           END.  	    
	           CREATE franvaro.
	           ASSIGN franvaro.ANSTNR = pnr franvaro.AONR = onr2
	           franvaro.DELNR = dnr2 franvaro.LART = fkod
	           franvaro.FRAN = regdat3 franvaro.TILL = regdat4
	           franvaro.PROCENT = proc1 franvaro.TIMMAR = antal.
	           IF proc1 > 0 AND proc1 < 100 THEN DO:
 	              ASSIGN
 	              franvaro.STARTKL = frstart 
	              franvaro.SLUTKL = frslut.
	              IF franvaro.LART = "3301" THEN ASSIGN franvaro.LART = "3300".
	           END.
               ASSIGN
 	           franvaro.STARTKL = frstart 
	           franvaro.SLUTKL = frslut.
	        END.
               ELSE DO:      
	           regdat4 = TIDREGITAB.DATUM.
                  IF proc1 = 100  AND DAY(TIDREGITAB.DATUM) GE 26 AND
	           DAY(TIDREGITAB.DATUM + 1) > 1 THEN DO:
	              ASSIGN
	              regdat4 = TIDREGITAB.DATUM + 1 
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
	                 ASSIGN
	                 regdat4 = regdat4 - 1
	                 regdat5 = regdat4.
	              END.
	              dat3:
	              REPEAT:
	                 IF regdat4 = regdat5 THEN LEAVE dat3. 
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
	                       ASSIGN
	                       regdat5 = DATE(01,01,YEAR(TIDREGITAB.DATUM) + 1)
	                       regdat4 = regdat5 - 1.
	                    END.
	                    ELSE DO:
	                       ASSIGN
	                       regdat5 = DATE(MONTH(TIDREGITAB.DATUM) + 1,01,YEAR(TIDREGITAB.DATUM)).
	                       regdat4 = regdat5 - 1.
	                    END.   
	                 END.  
	                 ELSE regdat4 = TIDREGITAB.DATUM.              
	              END.
	              ELSE regdat4 = TIDREGITAB.DATUM.   
	           END.  	    
                  ASSIGN franvaro.TILL = regdat4.
                  ASSIGN franvaro.SLUTKL = frslut.
               END. 
            END.
            IF del1 = FALSE AND franvaro.TIMMAR > 0 THEN DO:
               CREATE frfel.
               ASSIGN frfel.ANSTNR = franvaro.ANSTNR frfel.AONR = franvaro.AONR  
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
   FIND FIRST franvaro WHERE franvaro.ANSTNR = " " OR franvaro.ANSTNR =
   "000000000000" USE-INDEX franvaro EXCLUSIVE-LOCK NO-ERROR.
   IF AVAILABLE franvaro THEN DELETE franvaro.
END.
REPEAT TRANSACTION:
   FIND NEXT franvaro WHERE franvaro.ANSTNR = " " OR franvaro.ANSTNR =
    "000000000000"  USE-INDEX franvaro EXCLUSIVE-LOCK NO-ERROR.
   IF NOT AVAILABLE franvaro THEN LEAVE.
   ELSE DELETE franvaro.
END.
IF globforetag = "VATT" THEN DO:
   OUTPUT TO /guru/export/lon/fransu.d NO-ECHO.
END.
ELSE DO:
   OUTPUT TO D:\delad\pro8\GURU\apptemp\fransu.d NO-ECHO.
END.   
FOR EACH franvaro USE-INDEX franvaro  NO-LOCK:
  EXPORT franvaro.
END.
OUTPUT CLOSE.  
IF globforetag = "VATT" THEN DO:
   OUTPUT TO /guru/export/lon/frfelma.d NO-ECHO APPEND.
END. 
ELSE DO:
   OUTPUT TO D:\delad\pro8\GURU\apptemp\frfelma.d NO-ECHO APPEND.
END.   
FOR EACH frfel USE-INDEX franvaro  NO-LOCK:
  EXPORT frfel.
END.
RUN STVAFR2.P.
{EUROPEANAMERICAN.I}