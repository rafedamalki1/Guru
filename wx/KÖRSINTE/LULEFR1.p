/*LULEFR1.P           */

{LESAMMAN.I} 
DEFINE INPUT PARAMETER invkdatum AS DATE NO-UNDO.
DEFINE INPUT PARAMETER ingvisatidpermanad AS LOGICAL NO-UNDO.
DEFINE INPUT PARAMETER inglobforetag LIKE FORETAG.FORETAG NO-UNDO.
DEFINE INPUT PARAMETER korvar AS CHARACTER NO-UNDO.
RUN sammut_UI (INPUT 1).
DEFINE NEW SHARED VARIABLE vkdatum AS DATE NO-UNDO.
DEFINE NEW SHARED VARIABLE gvisatidpermanad AS LOGICAL NO-UNDO.

ASSIGN
vkdatum     = invkdatum  
gvisatidpermanad   = ingvisatidpermanad 
Guru.Konstanter:globforetag = inglobforetag.
DEFINE NEW SHARED VARIABLE persrec AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE regvnr AS INTEGER FORMAT "999" NO-UNDO.
DEFINE NEW SHARED VARIABLE regdatum AS DATE NO-UNDO.
DEFINE NEW SHARED VARIABLE regstart LIKE TIDREGITAB.START NO-UNDO. 
DEFINE NEW SHARED VARIABLE regslut LIKE TIDREGITAB.SLUT NO-UNDO.
DEFINE NEW SHARED VARIABLE nytid AS DECIMAL FORMAT "99.99" NO-UNDO.
DEFINE NEW SHARED VARIABLE sekunder AS INTEGER FORMAT "-9999999" NO-UNDO.
/*DEFINE NEW SHARED VARIABLE globanv LIKE ANVANDARE.ANVANDARE NO-UNDO.*/
DEFINE NEW SHARED VARIABLE regdagnamn AS CHARACTER FORMAT "X(3)" NO-UNDO.        
DEFINE NEW SHARED VARIABLE regtotalt LIKE TIDREGITAB.TOTALT NO-UNDO.
DEFINE NEW SHARED VARIABLE frustarten AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE fruslutet AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE kaffestart AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE kaffeslut AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE lunchstarten AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE lunchslutet AS DECIMAL NO-UNDO.

DEFINE VARIABLE antal LIKE TIDREGITAB.LONTILLANTAL NO-UNDO.   /*LON*/
DEFINE VARIABLE antalet LIKE PHJALP.PPAR8 NO-UNDO.   /*LON*/
DEFINE VARIABLE personal LIKE PERSONALTAB.PERSONALKOD NO-UNDO.
DEFINE VARIABLE pershj LIKE PERSONALTAB.PERSONALKOD NO-UNDO.
DEFINE VARIABLE vecknr LIKE TIDREGITAB.VECKONUMMER NO-UNDO.      /*LON*/
DEFINE VARIABLE proc LIKE FVARO.PROC NO-UNDO.
DEFINE VARIABLE fkod AS CHARACTER NO-UNDO.
DEFINE VARIABLE del1 LIKE FVARO.DEL NO-UNDO.
DEFINE VARIABLE regdatum2 AS DATE NO-UNDO.  
DEFINE VARIABLE regdat3 AS DATE NO-UNDO.
DEFINE VARIABLE regdat4 AS DATE NO-UNDO. 
DEFINE VARIABLE regdat5 AS DATE NO-UNDO.
DEFINE VARIABLE rec AS RECID NO-UNDO.

DEFINE VARIABLE regdat1 AS DATE NO-UNDO.
DEFINE VARIABLE regdat2 AS DATE NO-UNDO.  

DEFINE VARIABLE onr LIKE TIDREGITAB.AONR NO-UNDO.
DEFINE VARIABLE onr2 LIKE TIDREGITAB.AONR NO-UNDO.
DEFINE VARIABLE dnr LIKE TIDREGITAB.DELNR NO-UNDO.
DEFINE VARIABLE dnr2 LIKE TIDREGITAB.DELNR NO-UNDO.
DEFINE VARIABLE starttiden AS INTEGER NO-UNDO.
DEFINE VARIABLE sluttiden AS INTEGER NO-UNDO.

DEFINE VARIABLE tott1 AS DECIMAL FORMAT "99.99" NO-UNDO.
DEFINE VARIABLE tott2 AS INTEGER NO-UNDO.
DEFINE VARIABLE proc1 AS INTEGER FORMAT "999" NO-UNDO.

DEFINE VARIABLE seku AS INTEGER NO-UNDO.
DEFINE VARIABLE anst LIKE ANSTFORM.KOD NO-UNDO.
DEFINE VARIABLE krav AS INTEGER FORMAT "9" NO-UNDO.
DEFINE BUFFER frbuff FOR TIDREGITAB.   
DEFINE QUERY tidq FOR TIDREGITAB.
DEFINE VARIABLE prognamn5 AS CHARACTER  NO-UNDO.
DEFINE VARIABLE prognamn3 AS CHARACTER  NO-UNDO.
DEFINE VARIABLE prognamn4 AS CHARACTER  NO-UNDO.
DEFINE VARIABLE avdatum AS DATE NO-UNDO.
DEFINE VARIABLE komptid AS DECIMAL NO-UNDO.
DEFINE VARIABLE tiltid AS DECIMAL NO-UNDO.
DEFINE VARIABLE fratid AS DECIMAL NO-UNDO.

/*DEFINE TEMP-TABLE franvaro
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
   INDEX FRANVARO IS PRIMARY  PERSONALKOD FRAN LART ASCENDING.*/
DEFINE TEMP-TABLE franvarotemp
   FIELD PERSONALKOD LIKE PERSONALTAB.PERSONALKOD
   FIELD AONR LIKE TIDREGITAB.AONR
   FIELD DELNR LIKE TIDREGITAB.DELNR
   FIELD FRAN LIKE TIDREGITAB.DATUM
   FIELD FRTID AS DECIMAL FORMAT "99.99"
   FIELD TILL LIKE TIDREGITAB.DATUM
   FIELD TITID AS DECIMAL FORMAT "99.99"
   FIELD PROCENT AS INTEGER FORMAT "999"
   FIELD TIMMAR AS DECIMAL FORMAT "99.99"
   FIELD LART AS CHARACTER FORMAT "X(4)"   
   FIELD PKOD LIKE ANSTFORMTAB.KOD
   FIELD VIJUDID AS CHARACTER
   FIELD PNR AS CHARACTER
   FIELD TIMANSTALLD AS LOGICAL
   FIELD SJUK AS INTEGER EXTENT 31
   FIELD SJTIM AS DECIMAL EXTENT 31
   FIELD SJUKTIMMAR AS DECIMAL 
   FIELD ORSAK AS CHARACTER   
   FIELD ARBDAGAR AS INTEGER   
   FIELD TTID AS DECIMAL
   FIELD TOTTID AS DECIMAL
   INDEX FRANVARO IS PRIMARY PNR FRAN LART ASCENDING
   INDEX KALMAR VIJUDID PNR FRAN LART ASCENDING.

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
DEFINE TEMP-TABLE frfor
   FIELD PERSONALKOD LIKE PERSONALTAB.PERSONALKOD
   FIELD AONR LIKE TIDREGITAB.AONR
   FIELD DELNR LIKE TIDREGITAB.DELNR
   FIELD FRAN LIKE TIDREGITAB.DATUM
   FIELD TILL LIKE TIDREGITAB.DATUM 
   FIELD PROCENT AS INTEGER FORMAT "999"
   FIELD TIMMAR AS DECIMAL FORMAT "99.99"
   FIELD LART AS CHARACTER FORMAT "X(4)"
   INDEX FRANVARO IS PRIMARY  PERSONALKOD FRAN LART ASCENDING.          

DEFINE BUFFER franvbuff FOR franvarotemp.
FUNCTION klock100 RETURNS DECIMAL
  ( INPUT ber60 AS DECIMAL ):
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN  (TRUNCATE(ber60,0) * 3600 + (ber60 - TRUNCATE(ber60,0)) * 100 * 60) / 3600.

END FUNCTION.   
{AMERICANEUROPEAN.I}
IF Guru.Konstanter:globforetag = "LULE" THEN prognamn5 = "D:\elpool\DELAD\PRO9s\EXPORT\LON\Lonback\".   
ELSE IF Guru.Konstanter:globforetag = "ELPA" THEN prognamn5 = "\\pc112\delad\pro9s\korning\".

IF korvar = "" THEN DO:
   prognamn3 = prognamn5 + "fransu.d".
   INPUT FROM VALUE(prognamn3) NO-ECHO.
   
   REPEAT TRANSACTION:
     CREATE franvarotemp.
     ASSIGN.
     IMPORT franvarotemp.
   END.      
   prognamn3 = prognamn5 + "franfel.d".
   INPUT FROM VALUE(prognamn3) NO-ECHO.
   REPEAT TRANSACTION:
     CREATE frfel.
     ASSIGN.
     IMPORT frfel.
   END.
   prognamn3 = prognamn5 + "franfor.d".
   INPUT FROM VALUE(prognamn3) NO-ECHO.
   
   REPEAT TRANSACTION:
     CREATE frfor.
     ASSIGN.
     IMPORT frfor.
   END.
END.
OPEN QUERY persq FOR EACH PERSONALTAB WHERE PERSONALTAB.PERSMASK = TRUE AND 
PERSONALTAB.BRAVO = TRUE USE-INDEX PERSONALKOD NO-LOCK.
GET FIRST persq NO-LOCK.
DO WHILE AVAILABLE(PERSONALTAB):
   persrec = RECID(PERSONALTAB).
   personal = PERSONALTAB.PERSONALKOD.
   pershj = PERSONALTAB.PERSONALKOD.
   IF PERSONALTAB.ANSTALLNING = "Entrep.avtal" THEN .
   ELSE DO:
      FIND FIRST ANSTFORMTAB WHERE ANSTFORMTAB.ANSTALLNING =
      PERSONALTAB.ANSTALLNING USE-INDEX ANSTF NO-LOCK NO-ERROR.
      IF AVAILABLE ANSTFORMTAB THEN DO:
         anst = ANSTFORMTAB.KOD. 
         IF Guru.Konstanter:globforetag = "LULE" THEN DO:   
            IF TODAY < 05/01/2005 THEN DO:      
               OPEN QUERY tidq FOR EACH TIDREGITAB WHERE
               TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
               TIDREGITAB.GODKAND NE "" AND
               /*SUBSTRING(TIDREGITAB.VECKOKORD,1,9) NE "" AND*/ SUBSTRING(TIDREGITAB.VECKOKORD,10,9) = korvar AND
               TIDREGITAB.DATUM > 03/31/2005 AND TIDREGITAB.DATUM <= vkdatum AND TIDREGITAB.TIDLOG = TRUE
               USE-INDEX PSTART NO-LOCK. 
            END.
            ELSE DO:
               OPEN QUERY tidq FOR EACH TIDREGITAB WHERE
               TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
               TIDREGITAB.GODKAND BEGINS "G" AND
               SUBSTRING(TIDREGITAB.VECKOKORD,1,9) NE "" AND SUBSTRING(TIDREGITAB.VECKOKORD,10,9) = korvar AND
               TIDREGITAB.DATUM > 03/31/2005 AND TIDREGITAB.DATUM <= vkdatum AND TIDREGITAB.TIDLOG = TRUE
               USE-INDEX PSTART NO-LOCK. 
            END.
         END.
         IF Guru.Konstanter:globforetag = "ELPA" THEN DO:   
            OPEN QUERY tidq FOR EACH TIDREGITAB WHERE
            TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
            TIDREGITAB.GODKAND BEGINS "G" AND
            SUBSTRING(TIDREGITAB.VECKOKORD,1,9) NE "" AND SUBSTRING(TIDREGITAB.VECKOKORD,10,9) = korvar AND
            TIDREGITAB.DATUM > 12/31/2004 AND TIDREGITAB.DATUM <= vkdatum AND TIDREGITAB.TIDLOG = TRUE
            USE-INDEX PSTART NO-LOCK. 
         END.
          
         GET FIRST tidq NO-LOCK.         
         DO WHILE AVAILABLE(TIDREGITAB):         
            krav = 0.
            IF TIDREGITAB.GODKAND = '' THEN krav = 1.
            IF TIDREGITAB.START = TIDREGITAB.SLUT THEN krav = 1.       
            IF TIDREGITAB.DATUM > vkdatum THEN krav = 1. 
            
            FIND FIRST FRDEL WHERE FRDEL.PERSONALKOD = pershj AND
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
             	   FIND LAST franvarotemp WHERE franvarotemp.PERSONALKOD = personal AND
             	   franvarotemp.AONR = onr2 AND franvarotemp.DELNR = dnr2
             	   USE-INDEX franvaro EXCLUSIVE-LOCK NO-ERROR.
             	   IF AVAILABLE franvarotemp THEN DO:
             	      IF franvarotemp.FRAN > TIDREGITAB.DATUM AND
             	      franvarotemp.TILL > TIDREGITAB.DATUM THEN DO:
             	         FIND PREV franvarotemp WHERE franvarotemp.PERSONALKOD = personal AND
             	         franvarotemp.AONR = onr2 AND franvarotemp.DELNR = dnr2
             	         USE-INDEX franvaro EXCLUSIVE-LOCK NO-ERROR.
             	      END.
             	   END.                  
                  IF NOT AVAILABLE franvarotemp THEN DO:
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
                     ELSE IF TIDREGITAB.AONR = "141" OR TIDREGITAB.AONR = "143" 
                     OR TIDREGITAB.AONR = "160" OR TIDREGITAB.AONR = "170"   
                     OR TIDREGITAB.AONR = "171" OR TIDREGITAB.AONR = "172" OR TIDREGITAB.AONR = "250"
                     OR TIDREGITAB.AONR = "280" OR TIDREGITAB.AONR = "281" OR TIDREGITAB.AONR = "283" OR TIDREGITAB.AONR = "875"   THEN DO:                                  
                        /* timavdrag skall läggas ut med timmar i antal . Om det gäller flera dagar summeras timmarna                 
                        465 Läkarbesök 141
                        623- enskild angelägenhet 143
                        865- arbetstidsförkortning 160
                        860- kompledighet 170
                        440- veckovila 171                 
                        442- dygnsvila 172                 
                        692- utbildning 250
                        693- fackliga uppdrag med lön 280 281
                        697- Skyddsombudsarb 283
                        875- friskvård 875
                        franvarotemp.TOTTID -TOTALT ANTAL TIMMAR FÖR PERIOD
                        franvarotemp.TTID- TIMMAR DEL AV DAG
                        */
                         antal = TIDREGITAB.TOTALT.
                     END.
             	      ELSE DO:
             	         proc1 = 100.
             	         antal = 0.
             	      END.         
             	      regdat3 = TIDREGITAB.DATUM.
                     fratid = TIDREGITAB.START.
             	      IF proc1 = 100  AND DAY(TIDREGITAB.DATUM) LE 3 AND
             	      DAY(TIDREGITAB.DATUM) > 1 THEN DO:
                        ASSIGN
             	         regdat3 = TIDREGITAB.DATUM - 1 
             	         regdat5 = regdat3
                        fratid = 00.00.
             	         IF WEEKDAY(regdat3) = 1 OR WEEKDAY(regdat3) = 7 THEN DO:
                            regdat3 = regdat3 - 1.
                            fratid = 00.00.
                        END.
             	         ELSE DO:
             	            FIND FIRST OVERAVTAB WHERE OVERAVTAB.DATUM = regdat3 AND
             	            OVERAVTAB.KOD = ANSTFORMTAB.KOD USE-INDEX ODATUM NO-LOCK NO-ERROR.
             	            IF AVAILABLE OVERAVTAB THEN DO:
             	               IF OVERAVTAB.EQDAG = 1 OR OVERAVTAB.EQDAG = 7 THEN DO: 
                                 regdat3 = regdat3 - 1.  
                                 fratid = 00.00.
                              END.
             	            END.
             	         END.                                  
                        DAT:
             	         REPEAT:
             	            IF regdat5 = regdat3 THEN LEAVE DAT.
             	            IF regdat5 > regdat3 THEN DO:    
             	               regdat5 = regdat3.
                              IF WEEKDAY(regdat3) = 1 OR WEEKDAY(regdat3) = 7 THEN DO: 
                                 regdat3 = regdat3 - 1.  
                                 fratid = 00.00.
                              END.
                              ELSE DO:
             	                  FIND FIRST OVERAVTAB WHERE OVERAVTAB.DATUM = regdat3 AND
             	                  OVERAVTAB.KOD = ANSTFORMTAB.KOD USE-INDEX ODATUM NO-LOCK NO-ERROR.
             	                  IF AVAILABLE OVERAVTAB THEN DO:
             	                     IF OVERAVTAB.EQDAG = 1 OR OVERAVTAB.EQDAG = 7 THEN DO: 
                                       regdat3 = regdat3 - 1.
                                       fratid = 00.00.
                                    END.
             	                  END.
             	               END.
             	            END.
             	         END.               
                        
             	         IF DAY(regdat3) GE 25 THEN DO:           
             	            FIND FIRST frbuff WHERE frbuff.PERSONALKOD =  pershj AND frbuff.DATUM =
             	            regdat3 AND frbuff.TIDLOG = TRUE AND frbuff.AONR = onr2 NO-LOCK NO-ERROR.
             	            IF AVAILABLE frbuff THEN DO: 
                              ASSIGN
                              regdat3 = DATE(MONTH(TIDREGITAB.DATUM),01,YEAR(TIDREGITAB.DATUM))
                              fratid = 00.00.
                           END.       	            
             	            ELSE DO: 
                              ASSIGN
                              regdat3 = TIDREGITAB.DATUM
                              fratid = TIDREGITAB.START.
                           END.
             	         END.  	     
             	         ELSE DO: 
                           ASSIGN
                           regdat3 = TIDREGITAB.DATUM
                           fratid = TIDREGITAB.START.
                        END.
             	      END.                     
                     ASSIGN
             	      regdat4 = TIDREGITAB.DATUM.
                     tiltid = TIDREGITAB.SLUT.
                     IF proc1 = 100  AND DAY(TIDREGITAB.DATUM) GE 25 AND
             	      DAY(TIDREGITAB.DATUM + 1) > 1 THEN DO:
                        ASSIGN
             	         regdat4 = TIDREGITAB.DATUM + 1 
             	         regdat5 = regdat4
                        tiltid = 24.00.
             	         IF WEEKDAY(regdat4) = 1 OR WEEKDAY(regdat4) = 7 THEN DO:
                           ASSIGN
                           regdat4 = regdat4 + 1.
                           tiltid = 24.00.
                        END.
             	         ELSE DO:
             	            FIND FIRST OVERAVTAB WHERE OVERAVTAB.DATUM = regdat4 AND
             	            OVERAVTAB.KOD = ANSTFORMTAB.KOD USE-INDEX ODATUM NO-LOCK NO-ERROR.
             	            IF AVAILABLE OVERAVTAB THEN DO:
             	               IF OVERAVTAB.EQDAG = 1 OR OVERAVTAB.EQDAG = 7 THEN DO: 
                                 ASSIGN
                                 regdat4 = regdat4 + 1
                                 tiltid = 24.00.
                              END.
             	            END.
             	         END. 
             	         IF regdat4 = regdat5 THEN DO:
                           ASSIGN
             	            regdat4 = regdat4 - 1
             	            regdat5 = regdat4
                           tiltid = 24.00.
             	         END.     
                        DAT2:
             	         REPEAT:
                           IF regdat4 = regdat5 THEN LEAVE DAT2. 
                           IF regdat4 > regdat5 THEN DO:
                              regdat5 = regdat4.
                              IF WEEKDAY(regdat4) = 1 OR WEEKDAY(regdat4) = 7 THEN DO: 
                                 ASSIGN
                                 regdat4 = regdat4 + 1
                                 tiltid = 24.00.
                              END.
                              ELSE DO:
                                 FIND FIRST OVERAVTAB WHERE OVERAVTAB.DATUM = regdat4 AND
                                 OVERAVTAB.KOD = ANSTFORMTAB.KOD USE-INDEX ODATUM NO-LOCK NO-ERROR.
                                 IF AVAILABLE OVERAVTAB THEN DO:
                                    IF OVERAVTAB.EQDAG = 1 OR OVERAVTAB.EQDAG = 7 THEN DO: 
                                       ASSIGN
                                       regdat4 = regdat4 + 1
                                       tiltid = 24.00.
                                    END.
                                 END.
                              END.    
                           END.   
             	          END.   
             	          IF DAY(regdat4) LE 3 THEN DO:           
             	             FIND FIRST frbuff WHERE frbuff.PERSONALKOD =  pershj AND frbuff.DATUM =
             	             regdat4 AND frbuff.TIDLOG = TRUE AND frbuff.AONR = onr2 NO-LOCK NO-ERROR.
             	             IF AVAILABLE frbuff THEN DO:
             	                IF MONTH(TIDREGITAB.DATUM) = 12 THEN DO:  
                                  ASSIGN
             	                   regdat5 = DATE(01,01,YEAR(TIDREGITAB.DATUM) + 1)
             	                   regdat4 = regdat5 - 1
                                  tiltid = 24.00.
             	                END.
             	                ELSE DO:
             	                   regdat5 = DATE(MONTH(TIDREGITAB.DATUM) + 1,01,YEAR(TIDREGITAB.DATUM)).
                                  ASSIGN
             	                   regdat4 = regdat5 - 1
                                  tiltid = 24.00.
             	                END.
             	             END.  
             	             ELSE DO:
                               ASSIGN
                               regdat4 = TIDREGITAB.DATUM
                               tiltid = 24.00.
                            END.
             	          END.                       
             	          ELSE DO: 
                            ASSIGN
                            regdat4 = TIDREGITAB.DATUM
                            tiltid = 24.00.
                         END.
             	       END.  	    
             	       CREATE franvarotemp.
                      ASSIGN franvarotemp.PERSONALKOD = personal franvarotemp.AONR = onr2
                      franvarotemp.DELNR = dnr2 franvarotemp.LART = fkod
             	       franvarotemp.FRAN = regdat3 franvarotemp.TILL = regdat4
                      franvarotemp.PROCENT = proc1 franvarotemp.TIMMAR = antal franvarotemp.PKOD = anst
                      franvarotemp.FRTID = fratid franvarotemp.TITID = tiltid.                                                
                      FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = franvarotemp.PERSONALKOD NO-LOCK NO-ERROR.      
                      FIND FIRST OMRADETAB WHERE OMRADETAB.OMRADE = PERSONALTAB.OMRADE NO-LOCK NO-ERROR.
                      FIND FIRST AVDELNING WHERE AVDELNING.AVDELNINGNR = OMRADETAB.AVDELNINGNR NO-LOCK NO-ERROR.
                      FIND FIRST JURPERS WHERE JURPERS.JUDID = AVDELNING.POSTANST NO-LOCK NO-ERROR.      
                      IF AVAILABLE JURPERS THEN DO:
                         ASSIGN franvarotemp.VIJUDID = JURPERS.VIJUDID.
                      END.
             	    END.
                   ELSE DO:
                      regdatum = TIDREGITAB.DATUM - 1.
             	       RUN REGVEC.P.
             	       RUN SLUTARB.P.
             	       IF regstart = regslut THEN DO:
             	          REPEAT:
                            IF regstart ne regslut THEN LEAVE.
                            /*SÅ ATT DET INTE SKA BLI LOP OM PERSONEN STARTAR SIN ANSTÄLLNING MED FRÅNVARO*/
                            IF regdatum < ( TIDREGITAB.DATUM - 31 ) THEN LEAVE.
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
             	       IF franvarotemp.TILL < regdatum2
             	       OR franvarotemp.PROCENT < 100
             	       OR TIDREGITAB.TOTALT < tott1 THEN DO:
             	          IF TIDREGITAB.START NE regstart OR TIDREGITAB.SLUT NE regslut THEN DO:
             	             nytid = TIDREGITAB.TOTALT.
             	             RUN TIMSEK.P.
             	             IF tott2 LE 0 THEN proc1 = 0.
             	             ELSE proc1 = 100 * sekunder / tott2.
             	             IF proc = TRUE THEN antal = nytid.
             	             IF proc NE TRUE THEN antal = nytid.
             	          END.
                         ELSE IF TIDREGITAB.AONR = "141" OR TIDREGITAB.AONR = "143" 
                         OR TIDREGITAB.AONR = "160" OR TIDREGITAB.AONR = "170"   
                         OR TIDREGITAB.AONR = "171" OR TIDREGITAB.AONR = "172" OR TIDREGITAB.AONR = "250"
                         OR TIDREGITAB.AONR = "280" OR TIDREGITAB.AONR = "281" OR TIDREGITAB.AONR = "283" OR TIDREGITAB.AONR = "875"  THEN DO:                                  
                            /* timavdrag skall läggas ut med timmar i antal . Om det gäller flera dagar summeras timmarna                 
                            465 Läkarbesök 141
                            623- enskild angelägenhet 143
                            865- arbetstidsförkortning 160
                            860- kompledighet 170
                           440- veckovila 171                 
                           442- dygnsovila 172                 
                           692- utbildning 250
                            693- fackliga uppdrag med lön 280 281
                            697- Skyddsombudsarb 283
                            875- friskvård 875
                            franvarotemp.TOTTID -TOTALT ANTAL TIMMAR FÖR PERIOD
                            franvarotemp.TTID- TIMMAR DEL AV DAG
                            */
                            antal = TIDREGITAB.TOTALT.
                         END.
             	          ELSE DO:
             	             proc1 = 100.
             	             antal = 0.
             	          END.        
                         regdat3 = TIDREGITAB.DATUM.
                         fratid = TIDREGITAB.START.
             	          IF proc1 = 100  AND DAY(TIDREGITAB.DATUM) LE 3 AND
                         DAY(TIDREGITAB.DATUM) > 1 THEN DO:
                            ASSIGN
                            regdat3 = TIDREGITAB.DATUM - 1 
             	             regdat5 = regdat3
                            fratid = 00.00.
             	             IF WEEKDAY(regdat3) = 1 OR WEEKDAY(regdat3) = 7 THEN DO: 
                               ASSIGN
                               regdat3 = regdat3 - 1
                               fratid = 00.00.
                            END.
             	             ELSE DO:
             	                FIND FIRST OVERAVTAB WHERE OVERAVTAB.DATUM = regdat3 AND
             	                OVERAVTAB.KOD = ANSTFORMTAB.KOD USE-INDEX ODATUM NO-LOCK NO-ERROR.
             	                IF AVAILABLE OVERAVTAB THEN DO:
             	                   IF OVERAVTAB.EQDAG = 1 OR OVERAVTAB.EQDAG = 7 THEN DO: 
                                     ASSIGN
                                     regdat3 = regdat3 - 1
                                     fratid = 00.00.
                                  END.
             	                END.
             	             END.                                  
             	             DAT3:
               	          REPEAT:
               	             IF regdat5 = regdat3 THEN LEAVE DAT3.
                               IF regdat5 > regdat3 THEN DO:    
             	                   regdat5 = regdat3.
                                  IF WEEKDAY(regdat3) = 1 OR WEEKDAY(regdat3) = 7 THEN DO: 
                                     ASSIGN
                                     regdat3 = regdat3 - 1
                                     fratid = 00.00.
                                  END.
                                  ELSE DO:
             	                      FIND FIRST OVERAVTAB WHERE OVERAVTAB.DATUM = regdat3 AND
             	                      OVERAVTAB.KOD = ANSTFORMTAB.KOD USE-INDEX ODATUM NO-LOCK NO-ERROR.
             	                      IF AVAILABLE OVERAVTAB THEN DO:
             	                         IF OVERAVTAB.EQDAG = 1 OR OVERAVTAB.EQDAG = 7 THEN DO: 
                                           ASSIGN
                                           regdat3 = regdat3 - 1
                                           fratid = 00.00.
                                        END.
             	                      END.
             	                   END.
             	                END.
             	             END.   
             	             IF DAY(regdat3) GE 25 THEN DO:           
             	                FIND FIRST frbuff WHERE frbuff.PERSONALKOD =  pershj AND frbuff.DATUM =
             	                regdat3 AND frbuff.TIDLOG = TRUE AND frbuff.AONR = onr2 NO-LOCK NO-ERROR.
             	                IF AVAILABLE frbuff THEN DO: 
                                  ASSIGN
                                  regdat3 = DATE(MONTH(TIDREGITAB.DATUM),01,YEAR(TIDREGITAB.DATUM))
                                  fratid = 00.00.
                               END.       	                
                   	          ELSE DO: 
                                  ASSIGN
                                  regdat3 = TIDREGITAB.DATUM.              
                                  fratid = TIDREGITAB.START.
                               END.
             	             END.    	   
             	             ELSE DO: 
                               ASSIGN
                               regdat3 = TIDREGITAB.DATUM
                               fratid = TIDREGITAB.START.
                            END.
             	          END.              	   
                         ASSIGN
             	          regdat4 = TIDREGITAB.DATUM
                         tiltid = TIDREGITAB.SLUT.
                         IF proc1 = 100  AND DAY(TIDREGITAB.DATUM) GE 25 AND
             	          DAY(TIDREGITAB.DATUM + 1) > 1 THEN DO:
                            ASSIGN
             	             regdat4 = TIDREGITAB.DATUM + 1 
             	             regdat5 = regdat4
                            tiltid = 24.00.
             	             IF WEEKDAY(regdat4) = 1 OR WEEKDAY(regdat4) = 7 THEN DO:
                               ASSIGN
                               regdat4 = regdat4 + 1
                               tiltid = 24.00.
                            END.
             	             ELSE DO:
             	                FIND FIRST OVERAVTAB WHERE OVERAVTAB.DATUM = regdat4 AND
             	                OVERAVTAB.KOD = ANSTFORMTAB.KOD USE-INDEX ODATUM NO-LOCK NO-ERROR.
                               IF AVAILABLE OVERAVTAB THEN DO:
              	                   IF OVERAVTAB.EQDAG = 1 OR OVERAVTAB.EQDAG = 7 THEN DO: 
                                     ASSIGN
                                     regdat4 = regdat4 + 1
                                     tiltid = 24.00.
                                  END.
             	                END.
             	             END.   
             	             IF regdat4 = regdat5 THEN DO:
                               ASSIGN
             	                regdat4 = regdat4 - 1
             	                regdat5 = regdat4
                               tiltid = 24.00.
             	             END.     
             	             DAT4:
             	             REPEAT:
             	                IF regdat4 = regdat5 THEN LEAVE DAT4. 
              	                IF regdat4 > regdat5 THEN DO:
             	                   regdat5 = regdat4.
                                  IF WEEKDAY(regdat4) = 1 OR WEEKDAY(regdat4) = 7 THEN DO: 
                                     ASSIGN
                                     regdat4 = regdat4 + 1
                                     tiltid = 24.00.
                                  END.
                                  ELSE DO:
             	                      FIND FIRST OVERAVTAB WHERE OVERAVTAB.DATUM = regdat4 AND
             	                      OVERAVTAB.KOD = ANSTFORMTAB.KOD USE-INDEX ODATUM NO-LOCK NO-ERROR.
             	                      IF AVAILABLE OVERAVTAB THEN DO:
             	                         IF OVERAVTAB.EQDAG = 1 OR OVERAVTAB.EQDAG = 7 THEN DO: 
                                           ASSIGN
                                           regdat4 = regdat4 + 1
                                           tiltid = 24.00.
                                        END.
             	                      END.
               	                END.      
             	                END.
             	             END.   
             	             IF DAY(regdat4) LE 3 THEN DO:           
             	                FIND FIRST frbuff WHERE frbuff.PERSONALKOD =  pershj AND frbuff.DATUM =
             	                regdat4 AND frbuff.TIDLOG = TRUE AND frbuff.AONR = onr2 NO-LOCK NO-ERROR.
             	                IF AVAILABLE frbuff THEN DO: 
             	                   IF MONTH(TIDREGITAB.DATUM) = 12 THEN DO:  
                                     ASSIGN
             	                      regdat5 = DATE(01,01,YEAR(TIDREGITAB.DATUM) + 1)
             	                      regdat4 = regdat5 - 1
                                     tiltid = 24.00.
             	                   END.
             	                   ELSE DO:
                                     ASSIGN
             	                      regdat5 = DATE(MONTH(TIDREGITAB.DATUM) + 1,01,YEAR(TIDREGITAB.DATUM))
             	                      regdat4 = regdat5 - 1
                                     tiltid = 24.00.
             	                   END.   
             	                END.  
             	                ELSE DO: 
                                  ASSIGN
                                  regdat4 = TIDREGITAB.DATUM
                                  tiltid = TIDREGITAB.SLUT.
                               END.
             	             END.                              
             	             ELSE DO: 
                               ASSIGN
                               regdat4 = TIDREGITAB.DATUM
                               tiltid = TIDREGITAB.SLUT.
                            END.
             	          END.  	    
                         CREATE franvarotemp.
             	          ASSIGN franvarotemp.PERSONALKOD = personal franvarotemp.AONR = onr2
             	          franvarotemp.DELNR = dnr2 franvarotemp.LART = fkod
             	          franvarotemp.FRAN = regdat3 franvarotemp.TILL = regdat4
             	          franvarotemp.PROCENT = proc1 franvarotemp.TIMMAR = antal franvarotemp.PKOD = anst
                         franvarotemp.FRTID = fratid franvarotemp.TITID = tiltid.
                         FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = franvarotemp.PERSONALKOD NO-LOCK NO-ERROR.      
                          FIND FIRST OMRADETAB WHERE OMRADETAB.OMRADE = PERSONALTAB.OMRADE NO-LOCK NO-ERROR.
                          FIND FIRST AVDELNING WHERE AVDELNING.AVDELNINGNR = OMRADETAB.AVDELNINGNR NO-LOCK NO-ERROR.
                          FIND FIRST JURPERS WHERE JURPERS.JUDID = AVDELNING.POSTANST NO-LOCK NO-ERROR.      
                          IF AVAILABLE JURPERS THEN DO:
                             ASSIGN franvarotemp.VIJUDID = JURPERS.VIJUDID.
                          END.
             	       END.
                      ELSE DO:      
                         ASSIGN
             	          regdat4 = TIDREGITAB.DATUM
                         tiltid = TIDREGITAB.SLUT.
                         IF proc1 = 100  AND DAY(TIDREGITAB.DATUM) GE 25 AND
             	          DAY(TIDREGITAB.DATUM + 1) > 1 THEN DO:
                            ASSIGN
             	             regdat4 = TIDREGITAB.DATUM + 1 
             	             regdat5 = regdat4
                            tiltid = 24.00.
                  	       IF WEEKDAY(regdat4) = 1 OR WEEKDAY(regdat4) = 7 THEN DO:
                               ASSIGN
                               regdat4 = regdat4 + 1
                               tiltid = 24.00.
                            END.
             	             ELSE DO:
             	                FIND FIRST OVERAVTAB WHERE OVERAVTAB.DATUM = regdat4 AND
             	                OVERAVTAB.KOD = ANSTFORMTAB.KOD USE-INDEX ODATUM NO-LOCK NO-ERROR.
             	                IF AVAILABLE OVERAVTAB THEN DO:
             	                   IF OVERAVTAB.EQDAG = 1 OR OVERAVTAB.EQDAG = 7 THEN DO: 
                                     ASSIGN
                                     regdat4 = regdat4 + 1
                                     tiltid = 24.00.
                                  END.
             	                END.
             	             END.        
             	             IF regdat4 = regdat5 THEN DO:
                               ASSIGN
             	                regdat4 = regdat4 - 1
             	                regdat5 = regdat4
                               tiltid = 24.00.
                            END.
             	             DAT5:
                            REPEAT:
             	                IF regdat4 = regdat5 THEN LEAVE DAT5. 
              	                IF regdat4 > regdat5 THEN DO:
             	                   regdat5 = regdat4.
                                  IF WEEKDAY(regdat4) = 1 OR WEEKDAY(regdat4) = 7 THEN DO: 
                                     ASSIGN
                                     regdat4 = regdat4 + 1
                                     tiltid = 24.00.
                                  END.
                                  ELSE DO:
             	                      FIND FIRST OVERAVTAB WHERE OVERAVTAB.DATUM = regdat4 AND
             	                      OVERAVTAB.KOD = ANSTFORMTAB.KOD USE-INDEX ODATUM NO-LOCK NO-ERROR.
             	                      IF AVAILABLE OVERAVTAB THEN DO:
             	                         IF OVERAVTAB.EQDAG = 1 OR OVERAVTAB.EQDAG = 7 THEN DO: 
                                           ASSIGN
                                           regdat4 = regdat4 + 1.
                                           tiltid = 24.00.
                                        END.
             	                      END.
             	                   END.    
             	                END.
             	             END.   
             	             IF DAY(regdat4) LE 3 THEN DO:           
             	                FIND FIRST frbuff WHERE frbuff.PERSONALKOD =  pershj AND frbuff.DATUM =
             	                regdat4 AND frbuff.TIDLOG = TRUE AND frbuff.AONR = onr2 NO-LOCK NO-ERROR.
             	                IF AVAILABLE frbuff THEN DO:
             	                   IF MONTH(TIDREGITAB.DATUM) = 12 THEN DO:  
             	                      regdat5 = DATE(01,01,YEAR(TIDREGITAB.DATUM) + 1).
                                     ASSIGN
             	                      regdat4 = regdat5 - 1
                                     tiltid = 24.00.
             	                   END.
             	                   ELSE DO:
             	                      regdat5 = DATE(MONTH(TIDREGITAB.DATUM) + 1,01,YEAR(TIDREGITAB.DATUM)).
                                     ASSIGN
             	                      regdat4 = regdat5 - 1
                                     tiltid = 24.00.
             	                   END.   
             	                END.  
             	                ELSE DO: 
                                  ASSIGN
                                  regdat4 = TIDREGITAB.DATUM
                                  tiltid = TIDREGITAB.SLUT.
                               END.
             	             END.
             	             ELSE DO: 
                               ASSIGN
                               regdat4 = TIDREGITAB.DATUM
                               tiltid = TIDREGITAB.SLUT.
                            END.
             	          END.  	    
                         ASSIGN franvarotemp.TILL = regdat4 franvarotemp.TITID = tiltid.
                      END. 
                   END.           
                  
                   IF del1 = FALSE AND franvarotemp.TIMMAR > 0 THEN DO:
                      CREATE frfel.
                      ASSIGN frfel.PERSONALKOD = franvarotemp.PERSONALKOD frfel.AONR = franvarotemp.AONR
                      frfel.DELNR = franvarotemp.DELNR frfel.LART = franvarotemp.LART 
                      frfel.FRAN =   franvarotemp.FRAN frfel.TILL = franvarotemp.TILL 
                      frfel.PROCENT = franvarotemp.PROCENT frfel.TIMMAR = franvarotemp.TIMMAR.
                   END.      
                   
                END.   	    
             END.
             GET NEXT tidq NO-LOCK.
          END.
      END.
    END.  
    GET NEXT persq NO-LOCK.
 END.

prognamn3 = prognamn5 + "ftest"  + STRING(TODAY,"999999") + ".d".
OUTPUT TO VALUE(prognamn3).     
/* timavdrag skall läggas ut med timmar i antal . Om det gäller flera dagar summeras timmarna
440- veckovila
442- dygnsvila
465 Läkarbesök
623- enskild angelägenhet
692- utbildning
693- fackliga uppdrag med lön
697- Skyddsombudsarb 
860- kompledighet
865- arbetstidsförkortning
875- Friskvård
franvarotemp.TOTTID -TOTALT ANTAL TIMMAR FÖR PERIOD
franvarotemp.TTID- regtotalt DEL AV DAG
*/
FOR EACH franvarotemp WHERE franvarotemp.LART = "440" OR franvarotemp.LART = "442" OR franvarotemp.LART = "465" OR franvarotemp.LART = "623" OR franvarotemp.LART = "692"
OR franvarotemp.LART = "693" OR franvarotemp.LART = "697" OR franvarotemp.LART = "860" OR franvarotemp.LART = "865" OR franvarotemp.LART = "875" :
   IF franvarotemp.FRAN NE franvarotemp.TILL THEN DO:
      ASSIGN       
      regdatum = franvarotemp.FRAN 
      avdatum = franvarotemp.TILL.
      komptid = 0.
      FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = franvarotemp.PERSONALKOD USE-INDEX PERSONALKOD NO-LOCK.       
      persrec = RECID(PERSONALTAB).
      REPEAT:      
         RUN REGVEC.P.
         RUN SLUTARB.P.
         IF regstart NE regslut THEN DO:
            komptid = komptid + klock100(regtotalt).             
         END.                  
         regdatum = regdatum + 1.
         IF regdatum > avdatum THEN LEAVE.
      END.
      franvarotemp.TOTTID = komptid.
      EXPORT franvarotemp.
   END.   
END.

/*Se till att dela upp semester per månad eftersom den alltid delas upp i LULEFR2.p och då följer inte franvarotemp.TOTTID med*/
OPEN QUERY fq FOR EACH franvarotemp WHERE franvarotemp.LART = "502" AND franvarotemp.VIJUDID = "02".
GET FIRST fq NO-LOCK.
DO WHILE AVAILABLE(franvarotemp):      
   IF MONTH(franvarotemp.FRAN) < MONTH(franvarotemp.TILL) THEN DO:              
     pnr = franvarotemp.PERSONALKOD.
     onr2 = franvarotemp.AONR.
     dnr2 = franvarotemp.DELNR.
     fkod = franvarotemp.LART.
     regdat1 = franvarotemp.TILL.
     regdat2 = regdat1.
     proc1 = franvarotemp.PROCENT.
     antal = franvarotemp.TIMMAR.
     anst = franvarotemp.PKOD.
     REPEAT:
        regdat2 = regdat2 - 1.
        IF MONTH(regdat2) < MONTH(regdat1) THEN LEAVE.
     END.
     ASSIGN franvarotemp.TILL = regdat2.
     regdat2 = regdat2 + 1.
     CREATE franvarotemp.
     ASSIGN franvarotemp.PERSONALKOD = pnr
     franvarotemp.AONR = onr2
     franvarotemp.DELNR = dnr2
     franvarotemp.LART = fkod
     franvarotemp.FRAN = regdat2
     franvarotemp.TILL = regdat1
     franvarotemp.PROCENT = proc1
     franvarotemp.TIMMAR = antal
     franvarotemp.PKOD = anst.
     
  END.
  IF MONTH(franvarotemp.FRAN) LE MONTH(vkdatum) AND MONTH(franvarotemp.TILL) > MONTH(vkdatum) THEN DO:              
     pnr = franvarotemp.PERSONALKOD.
     onr2 = franvarotemp.AONR.
     dnr2 = franvarotemp.DELNR.
     fkod = franvarotemp.LART.
     regdat1 = franvarotemp.TILL.
     regdat2 = regdat1.
     proc1 = franvarotemp.PROCENT.
     antal = franvarotemp.TIMMAR.
     anst = franvarotemp.PKOD.
     REPEAT:
        regdat2 = regdat2 - 1.
        IF MONTH(regdat2) < MONTH(regdat1) THEN LEAVE.
     END.
     ASSIGN franvarotemp.TILL = regdat2.
     regdat2 = regdat2 + 1.
     CREATE franvarotemp.
     ASSIGN franvarotemp.PERSONALKOD = pnr
     franvarotemp.AONR = onr2
     franvarotemp.DELNR = dnr2
     franvarotemp.LART = fkod
     franvarotemp.FRAN = regdat2
     franvarotemp.TILL = regdat1
     franvarotemp.PROCENT = proc1
     franvarotemp.TIMMAR = antal
     franvarotemp.PKOD = anst.
     
  END.
  GET NEXT fq NO-LOCK.
END.      


          
FOR EACH franvarotemp WHERE franvarotemp.LART = "502" AND franvarotemp.VIJUDID = "02":
   /*SEMESTER 150 BIOENERGI*/
   franvarotemp.TOTTID = 0.
   IF franvarotemp.FRAN = franvarotemp.TILL THEN DO:
      FOR EACH TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = franvarotemp.PERSONALKOD AND
      TIDREGITAB.DATUM = franvarotemp.FRAN AND TIDREGITAB.START GE franvarotemp.FRTID AND TIDREGITAB.AONR = franvarotemp.AONR NO-LOCK:
         ASSIGN franvarotemp.TOTTID = franvarotemp.TOTTID + klock100(TIDREGITAB.TOTALT).
      END.
   END.
   ELSE IF franvarotemp.FRAN + 1 = franvarotemp.TILL  THEN DO:
      FOR EACH TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = franvarotemp.PERSONALKOD AND
      TIDREGITAB.DATUM = franvarotemp.FRAN AND TIDREGITAB.START GE franvarotemp.FRTID AND TIDREGITAB.AONR = franvarotemp.AONR NO-LOCK:
         ASSIGN franvarotemp.TOTTID = franvarotemp.TOTTID + klock100(TIDREGITAB.TOTALT).
      END.      
      FOR EACH TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = franvarotemp.PERSONALKOD AND
      TIDREGITAB.DATUM = franvarotemp.TILL AND TIDREGITAB.SLUT LE franvarotemp.TITID AND TIDREGITAB.AONR = franvarotemp.AONR NO-LOCK:
         ASSIGN franvarotemp.TOTTID = franvarotemp.TOTTID + klock100(TIDREGITAB.TOTALT).
      END.
      IF franvarotemp.TITID = 06 THEN DO:
         ASSIGN franvarotemp.TILL = franvarotemp.FRAN franvarotemp.TITID = 24.
      END.
   END.
   ELSE DO:   
      FOR EACH TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = franvarotemp.PERSONALKOD AND
      TIDREGITAB.DATUM = franvarotemp.FRAN AND TIDREGITAB.START GE franvarotemp.FRTID AND TIDREGITAB.AONR = franvarotemp.AONR NO-LOCK:
         ASSIGN franvarotemp.TOTTID = franvarotemp.TOTTID + klock100(TIDREGITAB.TOTALT).
      END.
      FOR EACH TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = franvarotemp.PERSONALKOD AND
      TIDREGITAB.DATUM > franvarotemp.FRAN AND TIDREGITAB.DATUM < franvarotemp.TILL AND TIDREGITAB.AONR = franvarotemp.AONR NO-LOCK:
         ASSIGN franvarotemp.TOTTID = franvarotemp.TOTTID + klock100(TIDREGITAB.TOTALT).
      END.
      FOR EACH TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = franvarotemp.PERSONALKOD AND
      TIDREGITAB.DATUM = franvarotemp.TILL AND TIDREGITAB.SLUT LE franvarotemp.TITID AND TIDREGITAB.AONR = franvarotemp.AONR NO-LOCK:
         ASSIGN franvarotemp.TOTTID = franvarotemp.TOTTID + klock100(TIDREGITAB.TOTALT).
      END.
      /* tiden skall läggas på första dygnet*/
      IF franvarotemp.TITID = 06 THEN DO:
         ASSIGN franvarotemp.TILL = (franvarotemp.TILL - 1 ) franvarotemp.TITID = 24.
      END.
   END.
   EXPORT franvarotemp.
END.

FOR EACH franvarotemp WHERE franvarotemp.VIJUDID = "02" AND franvarotemp.LART = "502" AND franvarotemp.FRAN = franvarotemp.TILL AND 
franvarotemp.TITID = 06 :
   FIND FIRST franvbuff WHERE franvbuff.PERSONALKOD = franvarotemp.PERSONALKOD AND franvbuff.LART = franvarotemp.LART 
   AND franvbuff.TILL = (franvarotemp.TILL - 1 )  NO-ERROR.
   IF AVAILABLE franvbuff THEN DO:
      ASSIGN franvbuff.TOTTID = franvbuff.TOTTID + franvarotemp.TOTTID    franvbuff.TITID = 24.
      DELETE franvarotemp.
   END.
END.

FOR EACH franvarotemp WHERE franvarotemp.FRAN = franvarotemp.TILL :   
   IF franvarotemp.TIMMAR > 0 THEN DO:   
      /*regtotalt DEL AV DAG*/
      FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = franvarotemp.PERSONALKOD USE-INDEX PERSONALKOD NO-LOCK.       
      persrec = RECID(PERSONALTAB).       
      regdatum = franvarotemp.FRAN. 
      RUN REGVEC.P.
      RUN SLUTARB.P.
      ASSIGN
      franvarotemp.TTID = regtotalt.         
   END.
END.
OUTPUT CLOSE.
/*special*/
IF Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "elpa" THEN DO:
  prognamn3 = prognamn5 + "franvarotemp1"  + STRING(TODAY,"999999") + ".d".
  OUTPUT TO VALUE(prognamn3).     
  FOR EACH franvarotemp BY franvarotemp.VIJUDID BY franvarotemp.PERSONALKOD BY franvarotemp.FRAN:
     EXPORT franvarotemp.
  END.
  OUTPUT CLOSE.
END.           


DO TRANSACTION:
   FIND FIRST franvarotemp WHERE franvarotemp.PERSONALKOD = " " OR franvarotemp.PERSONALKOD =
   "00000" USE-INDEX franvaro EXCLUSIVE-LOCK NO-ERROR.
   IF AVAILABLE franvarotemp THEN DELETE franvarotemp.
END.
REPEAT TRANSACTION:
   FIND NEXT franvarotemp WHERE franvarotemp.PERSONALKOD = " " OR franvarotemp.PERSONALKOD =
   "000000-0000"  USE-INDEX franvaro EXCLUSIVE-LOCK NO-ERROR.
   IF NOT AVAILABLE franvarotemp THEN LEAVE.
   ELSE DELETE franvarotemp.
END. 
IF korvar = "" THEN DO:
   prognamn3 = prognamn5 + "fransu.d".
   OUTPUT TO VALUE(prognamn3) NO-ECHO.   
END.
ELSE DO:
   prognamn3 = prognamn5 + "fransuomk.d".
   OUTPUT TO VALUE(prognamn3) NO-ECHO.
END.
FOR EACH franvarotemp USE-INDEX franvaro  NO-LOCK:
   EXPORT franvarotemp.
END.
IF korvar = "" THEN DO:
   prognamn3 = prognamn5 + "franfel.d".
   OUTPUT TO VALUE(prognamn3) NO-ECHO.
END.
ELSE DO:
   prognamn3 = prognamn5 + "franfelomk.d".
   OUTPUT TO VALUE(prognamn3) NO-ECHO.

END.
FOR EACH frfel USE-INDEX franvaro NO-LOCK:
   EXPORT frfel.
END.
IF korvar = "" THEN DO:
   prognamn3 = prognamn5 + "franfor.d".
   OUTPUT TO VALUE(prognamn3) NO-ECHO.

END.
ELSE DO:
   prognamn3 = prognamn5 + "franforomk.d".
   OUTPUT TO VALUE(prognamn3) NO-ECHO.
END.
FOR EACH frfor USE-INDEX franvaro  NO-LOCK:
   EXPORT frfor.
END.
RUN sammut_UI (INPUT 2).
{EUROPEANAMERICAN.I}