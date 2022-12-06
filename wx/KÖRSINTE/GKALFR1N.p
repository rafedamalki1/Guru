/*GKALFR1N.P 
OMARBETAT OPEN QUERY  KRAV ATT AONR FINNS UPPLAGD I FVARO
(TIDIGARE PRISTYP FRANVARO) KOPIA FINNS I SPARA*/
/* skapar filen pa90hj som ar veckans korning .Denna lagges till pa90veck.d*/
/*SKAPAR FILEN PA90VECK.P */
/*ALLA TILLAGG SOM VECKOKORS FAR EN EGEN RAD I DENNA RAPPORT*/
/*RAPPORTEN LIGGER TILL GRUND FOR MANADSKORNIING */  
/*web*/
/*
DEFINE /*NEW*/ SHARED VARIABLE vkdatum AS DATE NO-UNDO.
DEFINE /*NEW*/ SHARED VARIABLE gvisatidpermanad AS LOGICAL NO-UNDO.
 
*/
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
DEFINE VARIABLE kommentar AS CHARACTER NO-UNDO.

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
DEFINE VARIABLE prognamn2 AS CHARACTER  NO-UNDO.
DEFINE VARIABLE prognamn3 AS CHARACTER  NO-UNDO.
DEFINE VARIABLE prognamn4 AS CHARACTER  NO-UNDO.
DEFINE VARIABLE avdatum AS DATE NO-UNDO.
DEFINE VARIABLE komptid AS DECIMAL NO-UNDO.
DEFINE VARIABLE statid AS DECIMAL NO-UNDO.
DEFINE VARIABLE slutid AS DECIMAL NO-UNDO.
DEFINE VARIABLE antal2 LIKE TIDREGITAB.LONTILLANTAL NO-UNDO.   
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
   FIELD TILL LIKE TIDREGITAB.DATUM
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
   FIELD STTID AS DECIMAL
   FIELD SLTID AS DECIMAL
   FIELD BPNR AS CHARACTER
   FIELD FTIM AS DECIMAL
   FIELD LATTHELG AS INTEGER INITIAL 0
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

FUNCTION klock100 RETURNS DECIMAL
  ( INPUT ber60 AS DECIMAL ):
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN  (TRUNCATE(ber60,0) * 3600 + (ber60 - TRUNCATE(ber60,0)) * 100 * 60) / 3600.

END FUNCTION.   


IF Guru.Konstanter:globforetag = "GKAL" THEN prognamn2 = "d:\DELAD\server\PRO9S\gkal\".
/*IF Guru.Konstanter:globforetag = "GKAL" THEN prognamn2 = "d:\DELAD\server\PRO9S\gkal\".*/
ELSE IF Guru.Konstanter:globforetag = "ELPA" THEN prognamn2 = "\\pc112\delad\pro9s\korning\".
{AMERICANEUROPEAN.I}
IF korvar = "" THEN DO:
   
   prognamn3 = prognamn2 + "fransu.d".
   INPUT FROM VALUE(prognamn3) NO-ECHO.
   
   REPEAT TRANSACTION:
     CREATE franvarotemp.
     ASSIGN.
     IMPORT franvarotemp.
   END.      
   prognamn3 = prognamn2 + "franfel.d".
   INPUT FROM VALUE(prognamn3) NO-ECHO.
   REPEAT TRANSACTION:
     CREATE frfel.
     ASSIGN.
     IMPORT frfel.
   END.
   prognamn3 = prognamn2 + "franfor.d".
   INPUT FROM VALUE(prognamn3) NO-ECHO.
   
   REPEAT TRANSACTION:
     CREATE frfor.
     ASSIGN.
     IMPORT frfor.
   END.
   
END.
OPEN QUERY persq FOR EACH PERSONALTAB WHERE PERSONALTAB.PERSMASK = TRUE 
USE-INDEX PERSONALKOD NO-LOCK.
GET FIRST persq NO-LOCK.
DO WHILE AVAILABLE(PERSONALTAB):
   persrec = RECID(PERSONALTAB).
   personal = PERSONALTAB.PERSONALKOD.
   pershj = PERSONALTAB.PERSONALKOD.
   IF Guru.Konstanter:globforetag = "GRIT"  OR Guru.Konstanter:globforetag = "Celpa" THEN DO:
      personal = PERSONALTAB.ANSTNR.
   END.   

   FIND FIRST ANSTFORMTAB WHERE ANSTFORMTAB.ANSTALLNING =
   PERSONALTAB.ANSTALLNING USE-INDEX ANSTF NO-LOCK NO-ERROR.
   anst = ANSTFORMTAB.KOD.   
   OPEN QUERY tidq FOR EACH TIDREGITAB WHERE
   TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
   TIDREGITAB.GODKAND BEGINS "G" AND TIDREGITAB.VECKOKORD = korvar AND
   TIDREGITAB.DATUM <= vkdatum AND TIDREGITAB.TIDLOG = TRUE
   USE-INDEX PSTART NO-LOCK. 
    
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
      IF TIDREGITAB.PRISTYP = "RESTID..." THEN krav = 1.
      /*utbildning utanför ord arb ska med , men inte restid*/
      /*regdatum = TIDREGITAB.DATUM.
      regvnr = TIDREGITAB.VECKONUMMER.
      RUN SLUTARB.P.
      IF regstart = regslut THEN krav = 1.
      IF regstart NE regslut THEN DO:
         IF TIDREGITAB.START GE regslut AND TIDREGITAB.SLUT > regslut THEN krav = 1.
         IF TIDREGITAB.START < regstart AND TIDREGITAB.SLUT LE regstart THEN krav = 1.
      END.   */
      IF krav = 0 THEN DO:  
         ASSIGN
         rec = RECID(TIDREGITAB)
         onr2 = TIDREGITAB.AONR
         dnr2 = TIDREGITAB.DELNR.
         IF TIDREGITAB.AONR = "131" OR TIDREGITAB.AONR = "132" THEN DO:         
            kommentar = TIDREGITAB.RESMAL.
         END.
         ELSE kommentar = "".
         FIND FIRST FVARO WHERE FVARO.AONR = onr2
         AND FVARO.DELNR = dnr2 USE-INDEX FVARO NO-LOCK NO-ERROR.
         IF AVAILABLE FVARO THEN DO TRANSACTION:
            
            /* manuell översättning hogia-personec vid övergång 20111201 lena*/
            proc = TRUE.
            IF onr2 = "110" THEN ASSIGN fkod = "Sjuk".
            IF onr2 = "111" THEN ASSIGN fkod = "Sjuk".
            IF onr2 = "113" THEN ASSIGN fkod = "Sjuk".
            IF onr2 = "131" THEN ASSIGN fkod = "FPsem90%".
            IF onr2 = "132" THEN ASSIGN fkod = "Tillf fp".
            IF onr2 = "134" THEN ASSIGN fkod = "FPsem90%".
            IF onr2 = "140" THEN ASSIGN fkod = "Enskanl".
            IF onr2 = "141" THEN ASSIGN fkod = "Tjl ulön".
            IF onr2 = "143" THEN ASSIGN fkod = "Tjl lön".
            IF onr2 = "144" THEN ASSIGN fkod = "Tjl lön".
            IF onr2 = "145" THEN ASSIGN fkod = "Närstvår".
            IF onr2 = "150" THEN ASSIGN fkod = "Sembet".
            IF onr2 = "151" THEN ASSIGN fkod = "Semobet".
            IF onr2 = "160" THEN ASSIGN fkod = "Atidkont".
            IF onr2 = "162" THEN ASSIGN fkod = "Led6juni".
            IF onr2 = "170" THEN ASSIGN fkod = "Veckvila".
            IF onr2 = "171" THEN ASSIGN fkod = "Komp".
            IF onr2 = "172" THEN ASSIGN fkod = "LFKALM1".
            IF onr2 = "250" THEN ASSIGN fkod = "Utb i tj".
            IF onr2 = "280" THEN ASSIGN fkod = "Faverksh".
            IF onr2 = "282" THEN ASSIGN fkod = "Fauppdes".
            
            /*fkod = FVARO.FRKOD.
       	   proc = FVARO.PROC.*/
       	   del1 = FVARO.DEL.   /* ti-timmar pr-procent he-heldag */
       	   FIND LAST franvarotemp WHERE franvarotemp.PERSONALKOD = personal AND
       	   franvarotemp.AONR = onr2 AND franvarotemp.DELNR = dnr2 AND franvarotemp.BPNR = kommentar
       	   USE-INDEX franvaro NO-ERROR.
       	   IF AVAILABLE franvarotemp THEN DO:
       	      IF franvarotemp.FRAN > TIDREGITAB.DATUM AND
       	      franvarotemp.TILL > TIDREGITAB.DATUM THEN DO:
       	         FIND PREV franvarotemp WHERE franvarotemp.PERSONALKOD = personal AND
       	         franvarotemp.AONR = onr2 AND franvarotemp.DELNR = dnr2 AND franvarotemp.BPNR = kommentar
       	         USE-INDEX franvaro NO-ERROR.
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
               ASSIGN
               statid = 0
               slutid = 0.
       	      IF TIDREGITAB.START NE regstart OR TIDREGITAB.SLUT NE regslut
       	      THEN DO:
       	         nytid = TIDREGITAB.TOTALT.
       	         RUN TIMSEK.P.
       	         IF tott2 LE 0 THEN proc1 = 0.
       	         ELSE proc1 = 100 * sekunder / tott2.
       	         IF proc = TRUE THEN  antal = TIDREGITAB.TOTALT.
       	         IF proc NE TRUE THEN antal = TIDREGITAB.TOTALT.
                  ASSIGN
                  statid = TIDREGITAB.START
                  slutid = TIDREGITAB.SLUT.
       	      END.
               ELSE IF TIDREGITAB.AONR = "171" OR TIDREGITAB.AONR = "172" OR TIDREGITAB.AONR = "160" OR TIDREGITAB.AONR = "162" OR TIDREGITAB.AONR = "250" THEN DO:                
                  /*ORSAK = "Komp" ORSAK = "Atidkont" orsak = "dygnvila"*/
                   antal = TIDREGITAB.TOTALT.
               END.
       	      ELSE DO:
       	         proc1 = 100.
       	         antal = 0.
       	      END.         
               ASSIGN
       	      regdat3 = TIDREGITAB.DATUM
               statid = TIDREGITAB.START               
               antal2 =  klock100(antal).               
       	      IF proc1 = 100  AND DAY(TIDREGITAB.DATUM) LE 3 AND
       	      DAY(TIDREGITAB.DATUM) > 1 THEN DO:
       	         regdat3 = TIDREGITAB.DATUM - 1 .
       	         regdat5 = regdat3.
                  statid = 0.
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
       	         IF DAY(regdat3) GE 24 THEN DO:           
                     IF onr2 = "131" OR onr2 = "132" THEN DO:
                        FIND FIRST frbuff WHERE frbuff.PERSONALKOD =  pershj AND frbuff.DATUM =
          	            regdat3 AND frbuff.TIDLOG = TRUE AND frbuff.AONR = onr2 AND frbuff.RESMAL = kommentar NO-LOCK NO-ERROR.
                     END.
                     ELSE DO:                     
          	            FIND FIRST frbuff WHERE frbuff.PERSONALKOD =  pershj AND frbuff.DATUM =
          	            regdat3 AND frbuff.TIDLOG = TRUE AND frbuff.AONR = onr2 NO-LOCK NO-ERROR.
                     END.
       	            IF AVAILABLE frbuff THEN regdat3 =  DATE(MONTH(TIDREGITAB.DATUM),01,YEAR(TIDREGITAB.DATUM)).
       	            ELSE ASSIGN regdat3 = TIDREGITAB.DATUM statid = TIDREGITAB.START.              
       	         END.  	     
       	         ELSE ASSIGN regdat3 = TIDREGITAB.DATUM statid = TIDREGITAB.START. 
       	      END.              	         
               ASSIGN
       	      regdat4 = TIDREGITAB.DATUM
               slutid = TIDREGITAB.SLUT.
               IF proc1 = 100  AND DAY(TIDREGITAB.DATUM) GE 24 AND
       	      DAY(TIDREGITAB.DATUM + 1) > 1 THEN DO:
       	         regdat4 = TIDREGITAB.DATUM + 1 .
       	         regdat5 = regdat4.
                  slutid = 24.00.
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
                     IF onr2 = "131" OR onr2 = "132" THEN DO:
                        FIND FIRST frbuff WHERE frbuff.PERSONALKOD =  pershj AND frbuff.DATUM =
          	            regdat4 AND frbuff.TIDLOG = TRUE AND frbuff.AONR = onr2 AND frbuff.RESMAL = kommentar NO-LOCK NO-ERROR.                        
                     END.
                     ELSE DO:                     
          	            FIND FIRST frbuff WHERE frbuff.PERSONALKOD =  pershj AND frbuff.DATUM =
          	            regdat4 AND frbuff.TIDLOG = TRUE AND frbuff.AONR = onr2 NO-LOCK NO-ERROR.
                     END.
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
       	            ELSE ASSIGN regdat4 = TIDREGITAB.DATUM slutid = TIDREGITAB.SLUT.
       	         END.                       
       	         ELSE ASSIGN regdat4 = TIDREGITAB.DATUM slutid = TIDREGITAB.SLUT.
       	      END.  	    
       	      CREATE franvarotemp.
               ASSIGN franvarotemp.PERSONALKOD = personal franvarotemp.AONR = onr2
               franvarotemp.DELNR = dnr2 franvarotemp.LART = fkod
       	      franvarotemp.FRAN = regdat3 franvarotemp.TILL = regdat4
               franvarotemp.PROCENT = proc1 franvarotemp.TIMMAR = antal2 franvarotemp.PKOD = anst
               franvarotemp.STTID = statid franvarotemp.SLTID = slutid.
               IF franvarotemp.AONR = "131" OR franvarotemp.AONR = "132" THEN franvarotemp.BPNR = kommentar. 
               ELSE franvarotemp.BPNR = "".
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
               ASSIGN
               statid = 0
               slutid = 0.
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
                     ASSIGN
                     statid = TIDREGITAB.START
                     slutid = TIDREGITAB.SLUT.
       	         END.
                  ELSE IF TIDREGITAB.AONR = "171" OR TIDREGITAB.AONR = "172" OR TIDREGITAB.AONR = "160" OR TIDREGITAB.AONR = "162" OR TIDREGITAB.AONR = "250" THEN DO:                
                     /*  ORSAK = "Komp" ORSAK = "Atidkont" orsak = "dygnvila"*/
                     antal = nytid.
                  END.
       	         ELSE DO:
       	            proc1 = 100.
       	            antal = 0.
       	         END.        
                  ASSIGN
                  antal2 =  klock100(antal)
                  regdat3 = TIDREGITAB.DATUM
                  statid = TIDREGITAB.START.
       	         IF proc1 = 100  AND DAY(TIDREGITAB.DATUM) LE 3 AND
                  DAY(TIDREGITAB.DATUM) > 1 THEN DO:
                     regdat3 = TIDREGITAB.DATUM - 1 .
       	            regdat5 = regdat3.
                     statid = 0.
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
       	            IF DAY(regdat3) GE 24 THEN DO:           
                        IF onr2 = "131" OR onr2 = "132" THEN DO:
                           FIND FIRST frbuff WHERE frbuff.PERSONALKOD =  pershj AND frbuff.DATUM =
             	            regdat3 AND frbuff.TIDLOG = TRUE AND frbuff.AONR = onr2 AND frbuff.RESMAL = kommentar NO-LOCK NO-ERROR.                        
                        END.
                        ELSE DO:                     
          	               FIND FIRST frbuff WHERE frbuff.PERSONALKOD =  pershj AND frbuff.DATUM =
          	               regdat3 AND frbuff.TIDLOG = TRUE AND frbuff.AONR = onr2 NO-LOCK NO-ERROR.
                        END.
       	               IF AVAILABLE frbuff THEN regdat3 = DATE(MONTH(TIDREGITAB.DATUM),01,YEAR(TIDREGITAB.DATUM)).
             	         ELSE ASSIGN regdat3 = TIDREGITAB.DATUM statid = TIDREGITAB.START.              
       	            END.    	   
       	            ELSE ASSIGN  regdat3 = TIDREGITAB.DATUM statid = TIDREGITAB.START.  
       	         END.              	   
                  ASSIGN
       	         regdat4 = TIDREGITAB.DATUM
                  slutid = TIDREGITAB.SLUT.
                  IF proc1 = 100  AND DAY(TIDREGITAB.DATUM) GE 24 AND
       	         DAY(TIDREGITAB.DATUM + 1) > 1 THEN DO:
       	            regdat4 = TIDREGITAB.DATUM + 1 .
       	            regdat5 = regdat4.
                     slutid = 24.00.
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
                        IF onr2 = "131" OR onr2 = "132" THEN DO:
                           FIND FIRST frbuff WHERE frbuff.PERSONALKOD =  pershj AND frbuff.DATUM =
             	            regdat4 AND frbuff.TIDLOG = TRUE AND frbuff.AONR = onr2 AND frbuff.RESMAL = kommentar NO-LOCK NO-ERROR.                        
                        END.
                        ELSE DO:                     
          	               FIND FIRST frbuff WHERE frbuff.PERSONALKOD =  pershj AND frbuff.DATUM =
          	               regdat4 AND frbuff.TIDLOG = TRUE AND frbuff.AONR = onr2 NO-LOCK NO-ERROR.
                        END.
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
       	               ELSE ASSIGN regdat4 = TIDREGITAB.DATUM slutid = TIDREGITAB.SLUT.
       	            END.                              
       	            ELSE ASSIGN regdat4 = TIDREGITAB.DATUM slutid = TIDREGITAB.SLUT.  
       	         END.  	    
                  CREATE franvarotemp.
       	         ASSIGN franvarotemp.PERSONALKOD = personal franvarotemp.AONR = onr2
       	         franvarotemp.DELNR = dnr2 franvarotemp.LART = fkod
       	         franvarotemp.FRAN = regdat3 franvarotemp.TILL = regdat4
       	         franvarotemp.PROCENT = proc1 franvarotemp.TIMMAR = antal2 franvarotemp.PKOD = anst
                  franvarotemp.STTID = statid franvarotemp.SLTID = slutid.
                  IF franvarotemp.AONR = "131" OR franvarotemp.AONR = "132" THEN franvarotemp.BPNR = kommentar.
                  ELSE franvarotemp.BPNR = "".
       	      END.
               ELSE DO:      
                  ASSIGN
       	         regdat4 = TIDREGITAB.DATUM
                  slutid = TIDREGITAB.SLUT.
                  IF proc1 = 100  AND DAY(TIDREGITAB.DATUM) GE 24 AND
       	         DAY(TIDREGITAB.DATUM + 1) > 1 THEN DO:
       	            regdat4 = TIDREGITAB.DATUM + 1 .
       	            regdat5 = regdat4.
                     slutid = 24.00.
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
                       IF onr2 = "131" OR onr2 = "132" THEN DO:
                           FIND FIRST frbuff WHERE frbuff.PERSONALKOD =  pershj AND frbuff.DATUM =
             	            regdat4 AND frbuff.TIDLOG = TRUE AND frbuff.AONR = onr2 AND frbuff.RESMAL = kommentar NO-LOCK NO-ERROR.                        
                        END.
                        ELSE DO:                     
          	              FIND FIRST frbuff WHERE frbuff.PERSONALKOD =  pershj AND frbuff.DATUM =
          	              regdat4 AND frbuff.TIDLOG = TRUE AND frbuff.AONR = onr2 NO-LOCK NO-ERROR.
                        END.
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
       	              ELSE ASSIGN regdat4 = TIDREGITAB.DATUM slutid = TIDREGITAB.SLUT.              
       	           END.
       	           ELSE ASSIGN regdat4 = TIDREGITAB.DATUM slutid = TIDREGITAB.SLUT.   
       	        END.  	    
                 FIND CURRENT franvarotemp EXCLUSIVE-LOCK NO-ERROR.
                 IF AVAILABLE franvarotemp THEN DO: 
                    ASSIGN franvarotemp.TILL = regdat4.
                    franvarotemp.SLTID = slutid.
                 END.
              END. 
           END.           
            
           IF del1 = FALSE AND franvarotemp.TIMMAR > 0 THEN DO:
              CREATE frfel.
              ASSIGN frfel.PERSONALKOD = franvarotemp.PERSONALKOD frfel.AONR = franvarotemp.AONR
              frfel.DELNR = franvarotemp.DELNR frfel.LART = franvarotemp.LART 
              frfel.FRAN =   franvarotemp.FRAN frfel.TILL = franvarotemp.TILL 
              frfel.PROCENT = franvarotemp.PROCENT frfel.TIMMAR = franvarotemp.TIMMAR.
           END.      
            /*IF PERSONALTAB.OMREGTID = 1 AND franvarotemp.LART = "511" THEN DO:
               CREATE frfor.
               ASSIGN frfor.PERSONALKOD = franvarotemp.PERSONALKOD frfor.AONR = franvarotemp.AONR
               frfor.DELNR = franvarotemp.DELNR frfor.LART = franvarotemp.LART 
               frfor.FRAN =   franvarotemp.FRAN frfor.TILL = franvarotemp.TILL 
               frfor.PROCENT = franvarotemp.PROCENT frfor.TIMMAR = franvarotemp.TIMMAR.
            END.*/
        END.   	    
     END.
     GET NEXT tidq NO-LOCK.
  END.
  GET NEXT persq NO-LOCK.
END.

/*  Lägg ut arbetstidsförkortning både som frånvaro och tillägg*/
/*OPEN QUERY atlq FOR EACH PERSONALTAB WHERE PERSONALTAB.PERSMASK = TRUE 
USE-INDEX PERSONALKOD NO-LOCK,
EACH TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
TIDREGITAB.GODKAND BEGINS "G" AND TIDREGITAB.VECKOKORD = korvar AND
TIDREGITAB.DATUM <= vkdatum AND TIDREGITAB.TIDLOG = FALSE AND TIDREGITAB.LONTILLAGG = "ATL"
USE-INDEX PSTART NO-LOCK.     
GET FIRST atlq NO-LOCK.         
DO WHILE AVAILABLE(TIDREGITAB): 
   FIND FIRST ANSTFORMTAB WHERE ANSTFORMTAB.ANSTALLNING =
   PERSONALTAB.ANSTALLNING USE-INDEX ANSTF NO-LOCK NO-ERROR.   
   CREATE franvarotemp.
   ASSIGN franvarotemp.PERSONALKOD = TIDREGITAB.PERSONALKOD franvarotemp.AONR = "160"
   franvarotemp.DELNR = 0 franvarotemp.LART = "Atidkont"
   franvarotemp.FRAN = TIDREGITAB.DATUM franvarotemp.TILL = TIDREGITAB.DATUM
   /*franvarotemp.PROCENT = proc1*/ franvarotemp.TIMMAR = TIDREGITAB.LONTILLANTAL franvarotemp.PKOD = ANSTFORMTAB.KOD.                                                
   GET NEXT atlq NO-LOCK.         
END.
*/




prognamn3 = prognamn2 + "komp"  + STRING(TODAY,"999999") + ".d".
OUTPUT TO VALUE(prognamn3).     

FOR EACH franvarotemp WHERE franvarotemp.LART = "Komp" OR franvarotemp.LART = "Atidkont" OR franvarotemp.LART = "LFKALM1" OR franvarotemp.LART = "Utb i tj" 
OR franvarotemp.LART = "Led6juni"  :
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
OUTPUT CLOSE.

FOR EACH franvarotemp :   /*WHERE franvarotemp.FRAN NE franvarotemp.TILL*/ 
   /* summa timmar för frånvaroperioden läggs ut för timanställda både enstaka dag och period*/
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
    franvarotemp.FTIM = komptid.    
END.


/*special*/
IF Guru.Konstanter:globforetag = "gkal" THEN DO:
  prognamn3 = prognamn2 + "franvarotemp1"  + STRING(TODAY,"999999") + ".d".
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
   prognamn3 = prognamn2 + "fransu.d".
   OUTPUT TO VALUE(prognamn3) NO-ECHO.
   /*IF Guru.Konstanter:globforetag = "GKAL" THEN DO:
      OUTPUT TO \\extraguru\DELAD\server\PRO9S\gkal\fransu.d NO-ECHO.
   END.
   ELSE DO:
      OUTPUT TO \\pc112\delad\pro9s\korning\fransu.d NO-ECHO.
   END. */
END.
ELSE DO:
   prognamn3 = prognamn2 + "fransuomk.d".
   OUTPUT TO VALUE(prognamn3) NO-ECHO.
   /*IF Guru.Konstanter:globforetag = "GKAL" THEN DO:
      OUTPUT TO \\extraguru\DELAD\server\PRO9S\gkal\fransuomk.d NO-ECHO.
   END.
   ELSE DO:
      OUTPUT TO \\pc112\delad\pro9s\korning\fransuomk.d NO-ECHO.
   END. */
END.
FOR EACH franvarotemp USE-INDEX franvaro:
   EXPORT franvarotemp.
END.
IF korvar = "" THEN DO:
   prognamn3 = prognamn2 + "franfel.d".
   OUTPUT TO VALUE(prognamn3) NO-ECHO.
   /*IF Guru.Konstanter:globforetag = "GKAL" THEN DO:
      OUTPUT TO \\extraguru\DELAD\server\PRO9S\gkal\franfel.d NO-ECHO.
   END.
   ELSE DO:
      OUTPUT TO \\pc112\delad\pro9s\korning\franfel.d NO-ECHO.
   END.  */
END.
ELSE DO:
   prognamn3 = prognamn2 + "franfelomk.d".
   OUTPUT TO VALUE(prognamn3) NO-ECHO.
   /*IF Guru.Konstanter:globforetag = "GKAL" THEN DO:
      OUTPUT TO \\extraguru\DELAD\server\PRO9S\gkal\franfelomk.d NO-ECHO.
   END.
   ELSE DO:
      OUTPUT TO \\pc112\delad\pro9s\korning\franfelomk.d NO-ECHO.
   END.  */
END.
FOR EACH frfel USE-INDEX franvaro  NO-LOCK:
   EXPORT frfel.
END.
IF korvar = "" THEN DO:
   prognamn3 = prognamn2 + "franfor.d".
   OUTPUT TO VALUE(prognamn3) NO-ECHO.
   /*IF Guru.Konstanter:globforetag = "GKAL" THEN DO:
      OUTPUT TO \\extraguru\DELAD\server\PRO9S\gkal\franfor.d NO-ECHO.
   END.
   ELSE DO:
      OUTPUT TO \\pc112\delad\pro9s\korning\franfor.d NO-ECHO.
   END.  */
END.
ELSE DO:
   prognamn3 = prognamn2 + "franforomk.d".
   OUTPUT TO VALUE(prognamn3) NO-ECHO.
   /*IF Guru.Konstanter:globforetag = "GKAL" THEN DO:
      OUTPUT TO \\extraguru\DELAD\server\PRO9S\gkal\franforomk.d NO-ECHO.
   END.
   ELSE DO:
      OUTPUT TO \\pc112\delad\pro9s\korning\franforomk.d NO-ECHO.
   END.  */
END.
FOR EACH frfor USE-INDEX franvaro  NO-LOCK:
   EXPORT frfor.
END.
RUN sammut_UI (INPUT 2).
{EUROPEANAMERICAN.I}