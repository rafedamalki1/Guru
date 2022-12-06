/*GKALFR2N.P SKAPAR PA90FIL FRANVARO.*/  
DEFINE STREAM frkalmar. 
DEFINE STREAM frsmaland. 
DEFINE STREAM frsavsjo. 
DEFINE STREAM sjukkalmar. 
DEFINE STREAM sjuksmaland.

/*DEFINE STREAM sjuksavsjo.*/



/*DEFINE STREAM fransy.*/
{LESAMMAN.I}
DEFINE INPUT PARAMETER invkdatum AS DATE NO-UNDO.
DEFINE INPUT PARAMETER ingvisatidpermanad AS LOGICAL NO-UNDO.
DEFINE INPUT PARAMETER inglobforetag LIKE FORETAG.FORETAG NO-UNDO.
DEFINE INPUT PARAMETER inman AS INTEGER FORMAT "99" NO-UNDO.
DEFINE INPUT PARAMETER korvar AS CHARACTER NO-UNDO.
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
DEFINE NEW SHARED VARIABLE regvnr AS INTEGER FORMAT "999" NO-UNDO.
DEFINE NEW SHARED VARIABLE regdatum AS DATE NO-UNDO.
DEFINE NEW SHARED VARIABLE regstart LIKE TIDREGITAB.START NO-UNDO. 
DEFINE NEW SHARED VARIABLE regslut LIKE TIDREGITAB.SLUT NO-UNDO.
/*DEFINE NEW SHARED VARIABLE globanv LIKE ANVANDARE.ANVANDARE NO-UNDO.*/
DEFINE NEW SHARED VARIABLE persrec AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE regdagnamn AS CHARACTER FORMAT "X(3)" NO-UNDO.        
DEFINE NEW SHARED VARIABLE regtotalt LIKE TIDREGITAB.TOTALT NO-UNDO.
DEFINE NEW SHARED VARIABLE frustarten AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE fruslutet AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE kaffestart AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE kaffeslut AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE lunchstarten AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE lunchslutet AS DECIMAL NO-UNDO.

DEFINE VARIABLE overrapp1 AS CHARACTER FORMAT "X(300)" NO-UNDO.      /*LON*/
DEFINE VARIABLE regdat1 AS DATE NO-UNDO.
DEFINE VARIABLE regdat2 AS DATE NO-UNDO.  
DEFINE VARIABLE onr2 LIKE TIDREGITAB.AONR NO-UNDO.
DEFINE VARIABLE dnr2 LIKE TIDREGITAB.DELNR NO-UNDO.
DEFINE VARIABLE antal LIKE TIDREGITAB.LONTILLANTAL NO-UNDO.   /*LON*/
DEFINE VARIABLE proc1 AS INTEGER FORMAT "999" NO-UNDO.
DEFINE VARIABLE pnr LIKE PERSONALTAB.PERSONALKOD NO-UNDO.      /*LON*/
DEFINE VARIABLE anst LIKE ANSTFORM.KOD NO-UNDO.
DEFINE VARIABLE fkod AS CHARACTER NO-UNDO.
DEFINE VARIABLE kolldatum AS DATE NO-UNDO.
DEFINE VARIABLE i AS INTEGER NO-UNDO.
DEFINE VARIABLE arbdg AS INTEGER NO-UNDO.
DEFINE VARIABLE arbdg2 AS INTEGER NO-UNDO.
DEFINE VARIABLE filut AS CHARACTER NO-UNDO.
DEFINE VARIABLE lonfil AS CHARACTER NO-UNDO.
DEFINE VARIABLE prognamn AS CHARACTER FORMAT "X(41)" NO-UNDO.
DEFINE VARIABLE prognamn2 AS CHARACTER  NO-UNDO.
DEFINE VARIABLE prognamn3 AS CHARACTER  NO-UNDO.
DEFINE VARIABLE prognamn4 AS CHARACTER  NO-UNDO.
DEFINE VARIABLE anstperson LIKE PERSONALTAB.PERSONNUMMER NO-UNDO.
DEFINE VARIABLE nydag AS LOGICAL NO-UNDO.
DEFINE VARIABLE omfatt AS DECIMAL NO-UNDO.
DEFINE VARIABLE minut AS INTEGER NO-UNDO.
DEFINE VARIABLE radrakn AS INTEGER NO-UNDO.
DEFINE VARIABLE pekod AS CHARACTER NO-UNDO.
DEFINE VARIABLE stregtid AS DECIMAL NO-UNDO.
DEFINE VARIABLE stttid AS DECIMAL NO-UNDO.
DEFINE VARIABLE slregtid AS DECIMAL NO-UNDO.
DEFINE VARIABLE slttid AS DECIMAL NO-UNDO.
DEFINE VARIABLE koll1 AS LOGICAL NO-UNDO.
DEFINE VARIABLE ejhop AS LOGICAL NO-UNDO.
DEFINE VARIABLE ftpanv AS CHARACTER NO-UNDO.
DEFINE VARIABLE ftplord AS CHARACTER NO-UNDO. 
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
   FIELD TTID AS DECIMAL     /*regtotalt -dagens arbetstid*/
   FIELD TOTTID AS DECIMAL   /*periodens totala timmar*/
   FIELD STTID AS DECIMAL
   FIELD SLTID AS DECIMAL
   FIELD BPNR AS CHARACTER
   FIELD FTIM AS DECIMAL
   FIELD LATTHELG AS INTEGER
   INDEX FRANVARO IS PRIMARY PNR FRAN LART ASCENDING
   INDEX KALMAR VIJUDID PNR FRAN LART ASCENDING.
DEFINE BUFFER fbuff FOR franvarotemp.
DEFINE BUFFER fbuff2 FOR franvarotemp.
DEFINE BUFFER fbuff3 FOR franvarotemp.                     
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
DEFINE TEMP-TABLE felmeddtemp 
  FIELD FELMEDD AS CHARACTER
  FIELD VAL AS INTEGER.


FUNCTION klock100 RETURNS DECIMAL
  ( INPUT ber60 AS DECIMAL ):
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN  (TRUNCATE(ber60,0) * 3600 + (ber60 - TRUNCATE(ber60,0)) * 100 * 60) / 3600.

END FUNCTION.   
/*OBS! 7036 används troligen inte (bytt till 7048). För tillägg bytt från 7035 till 7047  */
IF Guru.Konstanter:globforetag = "GKAL" THEN prognamn2 = "d:\DELAD\server\PRO9S\gkal\".
/*IF Guru.Konstanter:globforetag = "GKAL" THEN prognamn2 = "d:\DELAD\server\PRO9S\gkal\".      */
ELSE IF Guru.Konstanter:globforetag = "ELPA" THEN prognamn2 = "\\pc112\delad\pro9s\korning\".
{AMERICANEUROPEAN.I}
IF korvar = "" THEN DO:
   prognamn3 = prognamn2 + "fransu.d".
   INPUT FROM VALUE(prognamn3) NO-ECHO.
END.
ELSE DO:
   prognamn3 = prognamn2 + "fransuomk.d".
   INPUT FROM VALUE(prognamn3) NO-ECHO.   
END.
REPEAT TRANSACTION:
  CREATE franvarotemp.
  ASSIGN.
  IMPORT franvarotemp.
  IF franvarotemp.LART = "530" THEN ASSIGN franvarotemp.ORSAK = "SJUK".
  IF franvarotemp.LART = "SJUK" THEN ASSIGN franvarotemp.ORSAK = "SJUK".
END.   

OPEN QUERY fq FOR EACH franvarotemp.
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
     IF franvarotemp.LART = "530" THEN ASSIGN franvarotemp.ORSAK = "SJUK".
     IF franvarotemp.LART = "SJUK" THEN ASSIGN franvarotemp.ORSAK = "SJUK".
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
     IF franvarotemp.LART = "530" THEN ASSIGN franvarotemp.ORSAK = "SJUK".
     IF franvarotemp.LART = "SJUK" THEN ASSIGN franvarotemp.ORSAK = "SJUK".
  END.
  GET NEXT fq NO-LOCK.
END.      
  
IF Guru.Konstanter:globforetag = "GKAL" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:      
   FOR EACH franvarotemp:      
      FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = franvarotemp.PERSONALKOD NO-LOCK NO-ERROR.      
      FIND FIRST OMRADETAB WHERE OMRADETAB.OMRADE = PERSONALTAB.OMRADE NO-LOCK NO-ERROR.
      FIND FIRST AVDELNING WHERE AVDELNING.AVDELNINGNR = OMRADETAB.AVDELNINGNR NO-LOCK NO-ERROR.
      FIND FIRST JURPERS WHERE JURPERS.JUDID = AVDELNING.POSTANST NO-LOCK NO-ERROR.      
      IF AVAILABLE JURPERS THEN DO:
         ASSIGN franvarotemp.VIJUDID = JURPERS.VIJUDID.
      END.
      ASSIGN
      franvarotemp.PNR = PERSONALTAB.PERSONNUMMER
      franvarotemp.TIMANSTALLD = FALSE.
      IF PERSONALTAB.BEFATTNING = "TIMANSTÄLLD" OR PERSONALTAB.BEFATTNING = "LOKALVÅRDARE" OR 
      PERSONALTAB.BEFATTNING = "PRAKTIKANT" THEN DO:
         franvarotemp.TIMANSTALLD = TRUE.
      END.
   END.
END.

IF korvar = "" THEN DO:
   prognamn3 = prognamn2 + "felman.d".
   OUTPUT TO VALUE(prognamn3) NO-ECHO.
END.
ELSE DO:
   prognamn3 = prognamn2 + "felmanomk.d ".
   OUTPUT TO VALUE(prognamn3) NO-ECHO.
END.

kolldatum = DATE(MONTH(vkdatum),01,YEAR(vkdatum)).
FOR EACH franvarotemp WHERE franvarotemp.FRAN < kolldatum USE-INDEX franvaro  NO-LOCK:   
  EXPORT franvarotemp.
END.
OUTPUT CLOSE.
prognamn3 = prognamn2 + "fvf"  + STRING(TODAY,"999999") + ".d".
OUTPUT TO VALUE(prognamn3).     
FOR EACH franvarotemp :
   EXPORT franvarotemp.
END.
OUTPUT CLOSE.

FOR EACH franvarotemp WHERE franvarotemp.FRAN = franvarotemp.TILL :   
   IF franvarotemp.TIMMAR > 0 THEN DO:   
      /*regtotalt DEL AV DAG*/
      FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = franvarotemp.PERSONALKOD USE-INDEX PERSONALKOD NO-LOCK.       
      persrec = RECID(PERSONALTAB).       
      regdatum = franvarotemp.FRAN. 
      RUN REGVEC.P.
      RUN SLUTARB.P.
      ASSIGN
      franvarotemp.TTID = klock100(regtotalt).         
   END.   
END.
RUN skift_UI.

prognamn3 = prognamn2 + "fve"  + STRING(TODAY,"999999") + ".d".
OUTPUT TO VALUE(prognamn3).     
FOR EACH franvarotemp :
   EXPORT franvarotemp.
END.
OUTPUT CLOSE.


FOR EACH franvarotemp :
  arbdg = 0.
  arbdg2 = 0.
  latthelg = 0.
  i = 1.
  regdatum = franvarotemp.FRAN.
  DO WHILE i <= 31:
     FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = franvarotemp.PERSONALKOD NO-LOCK NO-ERROR.      
     persrec = RECID(PERSONALTAB).         
     RUN REGVEC.P.
     RUN SLUTARB.P.      
     IF franvarotemp.TIMMAR > 0 THEN DO:
        
     END.
     IF franvarotemp.TIMANSTALLD = FALSE THEN DO:         
        nydag = TRUE.
        IF regstart = regslut THEN franvarotemp.SJUK[i] = 0.
        ELSE DO:                           
           FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = franvarotemp.PERSONALKOD AND
           TIDREGITAB.DATUM = regdatum AND  TIDREGITAB.AONR = franvarotemp.AONR NO-LOCK NO-ERROR.
           IF AVAILABLE TIDREGITAB THEN DO:
               IF franvarotemp.LART = "Komp" OR franvarotemp.LART = "Atidkont" OR franvarotemp.LART = "LFKALM1" OR franvarotemp.LART = "Utb i tj"
               OR franvarotemp.LART = "Led6juni"  THEN nydag = nydag.
               ELSE DO:                   
                  IF TIDREGITAB.START = 22 OR TIDREGITAB.START = 23 THEN nydag = FALSE.
                  IF TIDREGITAB.START = 18 OR TIDREGITAB.START = 19 THEN nydag = FALSE.
               END.
           END.           
           IF franvarotemp.LART = "Komp" OR franvarotemp.LART = "Atidkont" OR franvarotemp.LART = "LFKALM1" OR franvarotemp.LART = "Utb i tj" 
           OR franvarotemp.LART = "Led6juni" THEN nydag = nydag.
           ELSE IF franvarotemp.FRAN = franvarotemp.TILL  THEN nydag = nydag.
           ELSE DO:               
               IF regstart = 22 OR regstart = 23 THEN nydag = FALSE.
               IF regstart = 18 OR regstart = 19 THEN nydag = FALSE.
           END.
           IF franvarotemp.ORSAK = "SJUK" THEN franvarotemp.SJUK[i] = 1.
           IF nydag = TRUE THEN DO:                  
              arbdg = arbdg + 1. 
              IF franvarotemp.AONR = "150" THEN DO:                            
                 IF WEEKDAY(regdatum) = 1 THEN.
                 ELSE DO:                 
                    FIND FIRST OVERAVTAB WHERE OVERAVTAB.DATUM = regdatum AND OVERAVTAB.EQDAG = 1 NO-LOCK NO-ERROR.
                    IF NOT AVAILABLE OVERAVTAB THEN DO:
                       FIND FIRST OVERAVTAB WHERE OVERAVTAB.DATUM = regdatum AND OVERAVTAB.EQDAG = 7 NO-LOCK NO-ERROR.
                    END.
                    IF AVAILABLE OVERAVTAB THEN DO:                       
                       latthelg = latthelg + 1 .                       
                    END.
                 END.
              END.
           END.        
           
        END.
     END.
     ELSE DO:
        /*TIMANSTÄLLD*/
        IF regstart = regslut THEN franvarotemp.SJTIM[i] = 0.
        ELSE DO: 
           IF franvarotemp.ORSAK = "SJUK" THEN franvarotemp.SJTIM[i] = regtotalt.
           arbdg = arbdg + 1. 
        END.
     END.
     i = i + 1.
     regdatum = regdatum + 1.
     IF regdatum > franvarotemp.TILL THEN i = 32.
  END.
  ASSIGN
  franvarotemp.ARBDAGAR = arbdg
  franvarotemp.LATTHELG = latthelg.     

END.


DO TRANSACTION:   
   /*special*/
    IF Guru.Konstanter:globforetag = "gkal" THEN DO:
      IF korvar = "" THEN DO:      
         prognamn3 = prognamn2 + "franvarotemp2"  + STRING(TODAY,"99999999") + ".d".
         OUTPUT TO VALUE(prognamn3).     
      END.
      ELSE DO:
         prognamn3 = prognamn2 + "franvarotemp2omk"  + STRING(TODAY,"99999999") + ".d".
         OUTPUT TO VALUE(prognamn3).     
      END.
      FOR EACH franvarotemp BY franvarotemp.VIJUDID BY franvarotemp.PERSONALKOD BY franvarotemp.FRAN:
         EXPORT franvarotemp.
      END.
      OUTPUT CLOSE.
    END.
 
   /*special stans*/
   IF Guru.Konstanter:globforetag = "gkal" THEN DO:
      IF korvar = "" THEN DO:      
         prognamn3 = prognamn2 + "stanstid" + ".d".
         OUTPUT TO VALUE(prognamn3) APPEND.   
      END.
      ELSE DO:
         prognamn3 = prognamn2 + "stanstidomk" + ".d".
         OUTPUT TO VALUE(prognamn3) APPEND.   
      END.
      anstperson = "".
      FOR EACH franvarotemp BY franvarotemp.VIJUDID BY franvarotemp.PNR BY franvarotemp.FRAN:
         IF anstperson NE franvarotemp.PNR THEN DO:            
            ASSIGN overrapp1 = "".   
            FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONNUMMER = franvarotemp.PNR AND
            PERSONALTAB.AKTIV = TRUE NO-LOCK NO-ERROR. 
            IF NOT AVAILABLE PERSONALTAB THEN DO:      
               FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONNUMMER = franvarotemp.PNR NO-LOCK NO-ERROR. 
            END.
            ASSIGN
            SUBSTRING(overrapp1,2,11) = STRING(franvarotemp.PNR,"999999-9999").
            IF AVAILABLE PERSONALTAB THEN DO: 
               SUBSTRING(overrapp1,14,38) = PERSONALTAB.PERSONALKOD + " " + PERSONALTAB.FORNAMN + " " + PERSONALTAB.EFTERNAMN.  
            END.        
            PUT SKIP(1). 
            anstperson = franvarotemp.PNR.   
            PUT overrapp1.
            PUT SKIP(1). 
         END.                        
         ASSIGN overrapp1 = "".   
         ASSIGN                        
         SUBSTRING(overrapp1,2,8) = SUBSTRING(franvarotemp.LART,1,8)
         SUBSTRING(overrapp1,24,10) = STRING(franvarotemp.FRAN,"99999999")
         SUBSTRING(overrapp1,39,10) = STRING(franvarotemp.TILL,"99999999").         
         
         IF franvarotemp.TIMMAR > 0 THEN DO:
             IF franvarotemp.LART = "Komp" OR franvarotemp.LART = "Atidkont" OR franvarotemp.LART = "LFKALM1" OR franvarotemp.LART = "Utb i tj" 
             OR franvarotemp.LART = "Led6juni" OR franvarotemp.LART = "Tjl ulön"  THEN DO:
                IF franvarotemp.FRAN = franvarotemp.TILL THEN DO:               
                   IF franvarotemp.LART = "Tjl ulön" THEN SUBSTRING(overrapp1,2,8) = SUBSTRING("Tjl tim ",1,8).
                   SUBSTRING(overrapp1,50,6) = STRING((franvarotemp.TIMMAR),"99.99").         
                   IF franvarotemp.STTID = 0 AND franvarotemp.SLTID = 0  THEN.
                   ELSE DO:                   
                      SUBSTRING(overrapp1,67,5) = SUBSTRING(STRING(franvarotemp.STTID,"99.99"),1,2) + ":" + SUBSTRING(STRING(franvarotemp.STTID,"99.99"),4,2).         
                      SUBSTRING(overrapp1,73,5) = SUBSTRING(STRING(franvarotemp.SLTID,"99.99"),1,2) + ":" + SUBSTRING(STRING(franvarotemp.SLTID,"99.99"),4,2).         
                   END.
                END.
                ELSE DO:
                   SUBSTRING(overrapp1,50,6) = STRING((klock100(franvarotemp.TOTTID)),"999.99").
                END.

             END.
             ELSE DO:
                IF franvarotemp.TTID > 0 THEN DO:                
                   SUBSTRING(overrapp1,57,5) =  STRING((franvarotemp.TIMMAR / franvarotemp.TTID),"9.999").
                END.
                ELSE DO:
                   SUBSTRING(overrapp1,50,6) = STRING((franvarotemp.TIMMAR),"99.99").         
                END.
                SUBSTRING(overrapp1,67,5) = SUBSTRING(STRING(franvarotemp.STTID,"99.99"),1,2) + ":" + SUBSTRING(STRING(franvarotemp.STTID,"99.99"),4,2).         
                SUBSTRING(overrapp1,73,5) = SUBSTRING(STRING(franvarotemp.SLTID,"99.99"),1,2) + ":" + SUBSTRING(STRING(franvarotemp.SLTID,"99.99"),4,2).         
             END.
         END.
         ELSE DO:
             SUBSTRING(overrapp1,57,5) =  STRING (1.000,"9.999").
         END.
         SUBSTRING(overrapp1,64,2) = STRING(franvarotemp.ARBDAGAR).
        
         anstperson =franvarotemp.PNR.   
         IF overrapp1 NE ""  THEN DO:
            PUT overrapp1.   
            PUT SKIP. 
         END.
      END.
      OUTPUT CLOSE.
        
      prognamn = prognamn2 + "stanstid" + STRING(TODAY,"99999999") + ".d".              
      OS-COPY VALUE(prognamn3) VALUE(prognamn).      
   END.
   /*KALMAR*/
   IF Guru.Konstanter:globforetag = "GKAL" THEN DO:      
      IF korvar = "" THEN DO:      
         prognamn3 = prognamn2 + "frkollELNÄT.d".
         OUTPUT STREAM sjukkalmar TO VALUE(prognamn3) NO-ECHO.   
         prognamn3 = prognamn2 + "franELNÄT.d".
         OUTPUT STREAM frkalmar TO VALUE(prognamn3) NO-ECHO.
        
      END.
      ELSE DO:
         prognamn3 = prognamn2 + "frkollomkELNÄT.d".
         OUTPUT STREAM sjukkalmar TO VALUE(prognamn3) NO-ECHO.   
         prognamn3 = prognamn2 + "franomkELNÄT.d".
         OUTPUT STREAM frkalmar TO VALUE(prognamn3) NO-ECHO.         
      END.
      
   END.       
   ELSE IF Guru.Konstanter:globforetag = "ELPA" THEN DO:
      IF korvar = "" THEN DO:      
         prognamn3 = prognamn2 + "frkollELNÄT.d".
         OUTPUT STREAM sjukkalmar TO VALUE(prognamn3) NO-ECHO.   
         prognamn3 = prognamn2 + "franELNÄT.d".
         OUTPUT STREAM frkalmar TO VALUE(prognamn3) NO-ECHO.        
      END.
      ELSE DO:      
         prognamn3 = prognamn2 + "frkollomkELNÄT.d".
         OUTPUT STREAM sjukkalmar TO VALUE(prognamn3) NO-ECHO.   
         prognamn3 = prognamn2 + "franomkELNÄT.d".
         OUTPUT STREAM frkalmar TO VALUE(prognamn3) NO-ECHO.         
      END.
      
   END.            
   radrakn = 1.
   FOR EACH franvarotemp WHERE franvarotemp.VIJUDID = "GKEAB" :
      radrakn = radrakn + 1.
   END.

   overrapp1 = "7048;" + STRING(radrakn) + ";0;" + string(TODAY,"99999999") + ";" + STRING(TIME,"HH:MM").             
   PUT STREAM frkalmar overrapp1 SKIP.
   PUT SKIP.
   

   FOR EACH franvarotemp WHERE franvarotemp.VIJUDID = "GKEAB" USE-INDEX KALMAR:
      IF franvarotemp.PNR = "7802110176"  THEN pekod = "23120100".
      ELSE pekod = "0".
      minut = 0.
      omfatt = 0.
      IF franvarotemp.TIMMAR > 0 AND franvarotemp.TTID > 0 THEN DO:      
         omfatt = franvarotemp.TIMMAR / franvarotemp.TTID.                  
         IF omfatt > 1 THEN omfatt = 1.
      END.      
      IF franvarotemp.TIMMAR > 0 THEN DO:
         minut = franvarotemp.TIMMAR * 60.        
      END.
      IF franvarotemp.ORSAK = "SJUK" THEN DO:
         IF franvarotemp.TIMANSTALLD = FALSE THEN DO:         
            IF franvarotemp.TIMMAR > 0 THEN DO:
               /*del av dag - bara omfattning om det är sjuk annars 0.000*/               
                /*bara omfattning , inga timmar.*/
               overrapp1 = franvarotemp.PNR +
                        ";" + pekod + 
                        ";0;" +
                        STRING(franvarotemp.FRAN,"99999999") + ";" +
                        STRING(franvarotemp.TILL,"99999999") + ";" +
                        "0;0;" +
                        franvarotemp.LART + ";" +
                        STRING(omfatt,"9.999") + ";" +
                        "1;" +
                        "00.00;;" +
                        "0;0;" +
                        "0;" + 
                        "0;0;" +
                        STRING(franvarotemp.SJUK[1],"9") + ";" +
                        STRING(franvarotemp.SJUK[2],"9") + ";" +
                        STRING(franvarotemp.SJUK[3],"9") + ";" +
                        STRING(franvarotemp.SJUK[4],"9") + ";" +
                        STRING(franvarotemp.SJUK[5],"9") + ";" +
                        STRING(franvarotemp.SJUK[6],"9") + ";" +
                        STRING(franvarotemp.SJUK[7],"9") + ";" +
                        STRING(franvarotemp.SJUK[8],"9") + ";" +
                        STRING(franvarotemp.SJUK[9],"9") + ";" +
                        STRING(franvarotemp.SJUK[10],"9") + ";" +
                        STRING(franvarotemp.SJUK[11],"9") + ";" +
                        STRING(franvarotemp.SJUK[12],"9") + ";" +
                        STRING(franvarotemp.SJUK[13],"9") + ";" +
                        STRING(franvarotemp.SJUK[14],"9") + ";" +
                        "0;0;0;0;0;0;0;0;0;0;0;0;0;0;" +
                        "0;0;8;0;35;" +
                        STRING(franvarotemp.SJUK[15],"9") + ";" +
                        STRING(franvarotemp.SJUK[16],"9") + ";" +
                        STRING(franvarotemp.SJUK[17],"9") + ";" +
                        STRING(franvarotemp.SJUK[18],"9") + ";" +
                        STRING(franvarotemp.SJUK[19],"9") + ";" +
                        STRING(franvarotemp.SJUK[20],"9") + ";" +
                        STRING(franvarotemp.SJUK[21],"9") + ";" +                                             
                        "0;0;0;0;0;0;0;" +
                        SUBSTRING(franvarotemp.BPNR,1,10) + ";" +
                        STRING(franvarotemp.LATTHELG) + ";" +
                        "111".
            END.
            ELSE DO:
               overrapp1 = franvarotemp.PNR +
                        ";" + pekod + 
                        ";0;" +
                        STRING(franvarotemp.FRAN,"99999999") + ";" +
                        STRING(franvarotemp.TILL,"99999999") + ";" +
                        "0;0;" +
                        franvarotemp.LART + ";" +
                        STRING (1.000,"9.999") + ";" +                      
                        "1;" +
                        STRING(0,"99.99") + ";;" +
                        "0;0;" +
                        "0;" + 
                        "0;0;" +
                        STRING(franvarotemp.SJUK[1],"9") + ";" +
                        STRING(franvarotemp.SJUK[2],"9") + ";" +
                        STRING(franvarotemp.SJUK[3],"9") + ";" +
                        STRING(franvarotemp.SJUK[4],"9") + ";" +
                        STRING(franvarotemp.SJUK[5],"9") + ";" +
                        STRING(franvarotemp.SJUK[6],"9") + ";" +
                        STRING(franvarotemp.SJUK[7],"9") + ";" +
                        STRING(franvarotemp.SJUK[8],"9") + ";" +
                        STRING(franvarotemp.SJUK[9],"9") + ";" +
                        STRING(franvarotemp.SJUK[10],"9") + ";" +
                        STRING(franvarotemp.SJUK[11],"9") + ";" +
                        STRING(franvarotemp.SJUK[12],"9") + ";" +
                        STRING(franvarotemp.SJUK[13],"9") + ";" +
                        STRING(franvarotemp.SJUK[14],"9") + ";" +
                        "0;0;0;0;0;0;0;0;0;0;0;0;0;0;" +
                        "0;0;8;0;35;" +
                        STRING(franvarotemp.SJUK[15],"9") + ";" +
                        STRING(franvarotemp.SJUK[16],"9") + ";" +
                        STRING(franvarotemp.SJUK[17],"9") + ";" +
                        STRING(franvarotemp.SJUK[18],"9") + ";" +
                        STRING(franvarotemp.SJUK[19],"9") + ";" +
                        STRING(franvarotemp.SJUK[20],"9") + ";" +
                        STRING(franvarotemp.SJUK[21],"9") + ";" +                                             
                        "0;0;0;0;0;0;0;" +
                        SUBSTRING(franvarotemp.BPNR,1,10) + ";" +
                        STRING(franvarotemp.LATTHELG) + ";" +
                        "111".
            END.
         END.
         ELSE DO:         
            /*timanställda*/
            IF franvarotemp.TIMMAR > 0 THEN DO:
               overrapp1 = franvarotemp.PNR +
                        ";" + pekod + 
                        ";0;" +
                        STRING(franvarotemp.FRAN,"99999999") + ";" +
                        STRING(franvarotemp.TILL,"99999999") + ";" +
                        "0;0;" +
                        franvarotemp.LART + ";" +
                        "0;1;0;;" +                        
                        "0;0;" +
                        "0;" + 
                        "0;0;" +
                        "0;0;0;0;0;0;0;0;0;0;0;0;0;0;" +
                        STRING(franvarotemp.SJTIM[1],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[2],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[3],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[4],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[5],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[6],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[7],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[8],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[9],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[10],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[11],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[12],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[13],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[14],"9.99") + ";" +                        
                        STRING(franvarotemp.FTIM,"9.99") + ";" +                         
                        "0;8;0;35;" +
                        "0;0;0;0;0;0;0;" +
                        STRING(franvarotemp.SJTIM[15],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[16],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[17],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[18],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[19],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[20],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[21],"9.99") + ";" +                                                                     
                        SUBSTRING(franvarotemp.BPNR,1,10) + ";" +
                        STRING(franvarotemp.LATTHELG) + ";" +
                        "111".
            END.
            ELSE DO:
               overrapp1 = franvarotemp.PNR +
                        ";" + pekod + 
                        ";0;" +
                        STRING(franvarotemp.FRAN,"99999999") + ";" +
                        STRING(franvarotemp.TILL,"99999999") + ";" +
                        "0;0;" +
                        franvarotemp.LART + ";" +
                        "0;1;0;;" +                        
                        "0;0;" +
                        "0;" + 
                        "0;0;" +
                        "0;0;0;0;0;0;0;0;0;0;0;0;0;0;" +
                        STRING(franvarotemp.SJTIM[1],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[2],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[3],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[4],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[5],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[6],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[7],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[8],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[9],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[10],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[11],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[12],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[13],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[14],"9.99") + ";" +                                                
                        STRING(franvarotemp.FTIM,"9.99") + ";" +                         
                        "0;8;0;35;" +
                        "0;0;0;0;0;0;0;" +
                        STRING(franvarotemp.SJTIM[15],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[16],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[17],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[18],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[19],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[20],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[21],"9.99") + ";" +                                                                                             
                        SUBSTRING(franvarotemp.BPNR,1,10) + ";" +
                        STRING(franvarotemp.LATTHELG) + ";" +
                        "111".
            END.
         END.
      END.
      ELSE DO:
         IF franvarotemp.TIMMAR > 0 THEN DO:
            IF franvarotemp.LART = "Komp" OR franvarotemp.LART = "Atidkont" OR franvarotemp.LART = "LFKALM1" OR franvarotemp.LART = "Utb i tj" 
            OR franvarotemp.LART = "Led6juni" OR franvarotemp.LART = "Tjl ulön"  THEN DO:
                /*skall anges i timmar -ej omfattning. Om det gäller 1 dag används fältet franvarotemp.TIMMAR (antal timmar)
                  Gäller det flera dagar används fältet franvarotemp.TOTID (Totalt antal timmar under frånvaroperioden)*/
                IF franvarotemp.FRAN = franvarotemp.TILL THEN DO:             
                   IF franvarotemp.LART = "Tjl ulön" THEN franvarotemp.LART = "Tjl tim".
                    overrapp1 = franvarotemp.PNR +
                         ";" + pekod + 
                         ";0;" +
                         STRING(franvarotemp.FRAN,"99999999") + ";" +
                         STRING(franvarotemp.TILL,"99999999") + ";" +
                         "0;0;" +
                         franvarotemp.LART + ";" +          
                         STRING (0.000,"9.999") + ";" +                          
                         "1;" +                         
                         STRING((minut),"9999") + ";;" +
                         "0;0;" +
                         STRING(franvarotemp.ARBDAGAR) + ";" + 
                         "0;0;" +
                         "0;0;0;0;0;0;0;0;0;0;0;0;0;0;" +
                         "0;0;0;0;0;0;0;0;0;0;0;0;0;0;" +                         
                         "0;0;8;0;35;" +
                         "0;0;0;0;0;0;0;" +
                         "0;0;0;0;0;0;0;" +
                         SUBSTRING(franvarotemp.BPNR,1,10) + ";" +
                         STRING(franvarotemp.LATTHELG) + ";" +
                         "111".
                END.
                ELSE DO:
                   overrapp1 = franvarotemp.PNR +
                         ";" + pekod + 
                         ";0;" +
                         STRING(franvarotemp.FRAN,"99999999") + ";" +
                         STRING(franvarotemp.TILL,"99999999") + ";" +
                         "0;0;" +
                         franvarotemp.LART + ";" +          
                         STRING (0.000,"9.999") + ";" +                          
                         "1;" +                         
                         "0;0;" +
                         STRING(franvarotemp.ARBDAGAR) + ";" + 
                         "0;0;" +
                         "0;0;0;0;0;0;0;0;0;0;0;0;0;0;" +
                         "0;0;0;0;0;0;0;0;0;0;0;0;0;0;" +
                         STRING((klock100(franvarotemp.TOTTID)),"999.99") + ";" +
                         "0;8;0;35;" +
                         "0;0;0;0;0;0;0;" +
                         "0;0;0;0;0;0;0;" +
                         SUBSTRING(franvarotemp.BPNR,1,10) + ";" +
                         STRING(franvarotemp.LATTHELG) + ";" +
                         "111".
                END.
            END.
            ELSE DO:         
               /*bara omfattning ska ut - inte timmar*/
               IF omfatt > 0 AND omfatt LE 1  THEN DO:               
                   overrapp1 = franvarotemp.PNR +
                         ";" + pekod + 
                         ";0;" +
                         STRING(franvarotemp.FRAN,"99999999") + ";" +
                         STRING(franvarotemp.TILL,"99999999") + ";" +
                         "0;0;" +
                         franvarotemp.LART + ";" +                     
                         STRING(omfatt,"9.999") + ";" +
                         "1;" +                   
                         STRING(0,"99.99") + ";;" +
                         "0;0;" +
                         STRING(franvarotemp.ARBDAGAR) + ";" + 
                         "0;0;" +
                         "0;0;0;0;0;0;0;0;0;0;0;0;0;0;" +
                         "0;0;0;0;0;0;0;0;0;0;0;0;0;0;" +
                         "0;0;8;0;35;" +
                         "0;0;0;0;0;0;0;" +
                         "0;0;0;0;0;0;0;" +
                         SUBSTRING(franvarotemp.BPNR,1,10) + ";" +
                         STRING(franvarotemp.LATTHELG) + ";" +
                         "111".
               END.
               ELSE /*IF franvarotemp.LART = "Utb i tj" THEN*/ DO:                  
                   IF franvarotemp.TIMMAR > 0 AND franvarotemp.TTId = 0 THEN DO:
                   /* Ingen ordinarie arbetstid men "Utb i tj"- skicka omfattning 0 och antal timmar
                    gäller även tex 280 282 utan arbetstid*/
                      overrapp1 = franvarotemp.PNR +
                         ";" + pekod + 
                         ";0;" +
                         STRING(franvarotemp.FRAN,"99999999") + ";" +
                         STRING(franvarotemp.TILL,"99999999") + ";" +
                         "0;0;" +
                         franvarotemp.LART + ";" +                     
                         STRING(0,"9.999") + ";" +
                         "1;" +                                            
                         STRING(minut,"9999") + ";;" +
                         "0;0;" +
                         STRING(franvarotemp.ARBDAGAR) + ";" + 
                         "0;0;" +
                         "0;0;0;0;0;0;0;0;0;0;0;0;0;0;" +
                         "0;0;0;0;0;0;0;0;0;0;0;0;0;0;" +
                         "0;0;8;0;35;" +
                         "0;0;0;0;0;0;0;" +
                         "0;0;0;0;0;0;0;" +
                         SUBSTRING(franvarotemp.BPNR,1,10) + ";" +
                         STRING(franvarotemp.LATTHELG) + ";" +
                         "111".
                   END.
                   ELSE IF omfatt > 1  THEN DO:               
                      /* Liten ordinarie arbetstid tex 22-24 , men "Utb i tj"- skicka omfattning 0 och antal timmar*/
                      overrapp1 = franvarotemp.PNR +
                         ";" + pekod + 
                         ";0;" +
                         STRING(franvarotemp.FRAN,"99999999") + ";" +
                         STRING(franvarotemp.TILL,"99999999") + ";" +
                         "0;0;" +
                         franvarotemp.LART + ";" +                     
                         STRING(0,"9.999") + ";" +
                         "1;" +                                            
                         STRING(minut,"9999") + ";;" +
                         "0;0;" +
                         STRING(franvarotemp.ARBDAGAR) + ";" + 
                         "0;0;" +
                         "0;0;0;0;0;0;0;0;0;0;0;0;0;0;" +
                         "0;0;0;0;0;0;0;0;0;0;0;0;0;0;" +
                         "0;0;8;0;35;" +
                         "0;0;0;0;0;0;0;" +
                         "0;0;0;0;0;0;0;" +
                         SUBSTRING(franvarotemp.BPNR,1,10) + ";" +
                         STRING(franvarotemp.LATTHELG) + ";" +
                         "111".
                   END.
               END.
            END.

         END.
         ELSE DO:         
            overrapp1 = franvarotemp.PNR +
                     ";" + pekod + 
                     ";0;" +
                     STRING(franvarotemp.FRAN,"99999999") + ";" +
                     STRING(franvarotemp.TILL,"99999999") + ";" +
                     "0;0;" +
                     franvarotemp.LART + ";" +
                     STRING(1.000,"9.999") + ";" +
                     "1;" +
                     STRING(0,"99.99") + ";;0;0;" +
                     STRING(franvarotemp.ARBDAGAR) + ";" + 
                     "0;0;" +
                     "0;0;0;0;0;0;0;0;0;0;0;0;0;0;" +
                     "0;0;0;0;0;0;0;0;0;0;0;0;0;0;" +
                     "0;0;8;0;35;" +
                     "0;0;0;0;0;0;0;" +
                     "0;0;0;0;0;0;0;" +
                     SUBSTRING(franvarotemp.BPNR,1,10) + ";" +  
                     STRING(franvarotemp.LATTHELG) + ";" +
                     "111".
         END.
      

      END.            
      IF overrapp1 NE "" THEN DO:      
         IF  franvarotemp.LART = "SJUK" THEN DO:
            PUT STREAM sjukkalmar overrapp1 SKIP. 
         END.   
         PUT STREAM frkalmar overrapp1 SKIP.
      END.      
      DELETE franvarotemp.
      overrapp1 = "".
   END.
   PUT SKIP.

   OUTPUT STREAM sjukkalmar CLOSE.      
   OUTPUT STREAM frkalmar CLOSE.
    
   /*SMÅLAND*/
   IF Guru.Konstanter:globforetag = "GKAL" THEN DO:      
      IF korvar = "" THEN DO:      
         prognamn3 = prognamn2 + "frkollFÖRSÄLJNING.d".
         OUTPUT STREAM sjuksmaland TO VALUE(prognamn3) NO-ECHO.   
         prognamn3 = prognamn2 + "franFÖRSÄLJNING.d".
         OUTPUT STREAM frsmaland TO VALUE(prognamn3) NO-ECHO.
        
      END.
      ELSE DO:
         prognamn3 = prognamn2 + "frkollomkFÖRSÄLJNING.d".
         OUTPUT STREAM sjuksmaland TO VALUE(prognamn3) NO-ECHO.   
         prognamn3 = prognamn2 + "franomkFÖRSÄLJNING.d".
         OUTPUT STREAM frsmaland TO VALUE(prognamn3) NO-ECHO.         
      END.      
   END.       
   ELSE IF Guru.Konstanter:globforetag = "ELPA" THEN DO:
      IF korvar = "" THEN DO:      
         prognamn3 = prognamn2 + "frkollFÖRSÄLJNING.d".
         OUTPUT STREAM sjuksmaland TO VALUE(prognamn3) NO-ECHO.   
         prognamn3 = prognamn2 + "franFÖRSÄLJNING.d".
         OUTPUT STREAM frsmaland TO VALUE(prognamn3) NO-ECHO.        
      END.
      ELSE DO:      
         prognamn3 = prognamn2 + "frkollomkFÖRSÄLJNING.d".
         OUTPUT STREAM sjuksmaland TO VALUE(prognamn3) NO-ECHO.   
         prognamn3 = prognamn2 + "franomkFÖRSÄLJNING.d".
         OUTPUT STREAM frsmaland TO VALUE(prognamn3) NO-ECHO.         
      END.      
   END.            
   radrakn = 1.
   FOR EACH franvarotemp WHERE franvarotemp.VIJUDID = "GSEAB" :
      radrakn = radrakn + 1.
   END.


   overrapp1 = "7048;" + STRING(radrakn) + ";0;" + string(TODAY,"99999999") + ";" + STRING(TIME,"HH:MM").             
   PUT STREAM frsmaland overrapp1 SKIP.
   PUT SKIP.

   
   FOR EACH franvarotemp WHERE franvarotemp.VIJUDID = "GSEAB" USE-INDEX KALMAR:
      IF franvarotemp.PNR = "7802110176"  THEN pekod = "23120100".
      ELSE pekod = "0".
      omfatt = 0.
      minut = 0.
      IF franvarotemp.TIMMAR > 0 AND franvarotemp.TTID > 0 THEN DO:      
         omfatt = franvarotemp.TIMMAR / franvarotemp.TTID.
         IF omfatt > 1 THEN omfatt = 1.
      END.
      IF franvarotemp.TIMMAR > 0 THEN DO:
         minut = franvarotemp.TIMMAR * 60.         
      END.
      IF franvarotemp.ORSAK = "SJUK" THEN DO:
         IF franvarotemp.TIMANSTALLD = FALSE THEN DO:         
            IF franvarotemp.TIMMAR > 0 THEN DO:
               /*del av dag - bara omfattning om det är sjuk annars 0.000*/               
                /*bara omfattning , inga timmar.*/
               overrapp1 = franvarotemp.PNR +
                        ";" + pekod + 
                        ";0;" +
                        STRING(franvarotemp.FRAN,"99999999") + ";" +
                        STRING(franvarotemp.TILL,"99999999") + ";" +
                        "0;0;" +
                        franvarotemp.LART + ";" +
                        STRING(omfatt,"9.999") + ";" +
                        "1;" +
                        "00.00;;" +
                        "0;0;" +
                        "0;" + 
                        "0;0;" +
                        STRING(franvarotemp.SJUK[1],"9") + ";" +
                        STRING(franvarotemp.SJUK[2],"9") + ";" +
                        STRING(franvarotemp.SJUK[3],"9") + ";" +
                        STRING(franvarotemp.SJUK[4],"9") + ";" +
                        STRING(franvarotemp.SJUK[5],"9") + ";" +
                        STRING(franvarotemp.SJUK[6],"9") + ";" +
                        STRING(franvarotemp.SJUK[7],"9") + ";" +
                        STRING(franvarotemp.SJUK[8],"9") + ";" +
                        STRING(franvarotemp.SJUK[9],"9") + ";" +
                        STRING(franvarotemp.SJUK[10],"9") + ";" +
                        STRING(franvarotemp.SJUK[11],"9") + ";" +
                        STRING(franvarotemp.SJUK[12],"9") + ";" +
                        STRING(franvarotemp.SJUK[13],"9") + ";" +
                        STRING(franvarotemp.SJUK[14],"9") + ";" +
                        "0;0;0;0;0;0;0;0;0;0;0;0;0;0;" +
                        "0;0;8;0;35;" +
                        STRING(franvarotemp.SJUK[15],"9") + ";" +
                        STRING(franvarotemp.SJUK[16],"9") + ";" +
                        STRING(franvarotemp.SJUK[17],"9") + ";" +
                        STRING(franvarotemp.SJUK[18],"9") + ";" +
                        STRING(franvarotemp.SJUK[19],"9") + ";" +
                        STRING(franvarotemp.SJUK[20],"9") + ";" +
                        STRING(franvarotemp.SJUK[21],"9") + ";" +                                             
                        "0;0;0;0;0;0;0;" +
                        SUBSTRING(franvarotemp.BPNR,1,10) + ";" + 
                        STRING(franvarotemp.LATTHELG) + ";" +
                        "111".
            END.
            ELSE DO:
               overrapp1 = franvarotemp.PNR +
                        ";" + pekod + 
                        ";0;" +
                        STRING(franvarotemp.FRAN,"99999999") + ";" +
                        STRING(franvarotemp.TILL,"99999999") + ";" +
                        "0;0;" +
                        franvarotemp.LART + ";" +
                        STRING (1.000,"9.999") + ";" +                      
                        "1;" +
                        STRING(0,"99.99") + ";;" +
                        "0;0;" +
                        "0;" + 
                        "0;0;" +
                        STRING(franvarotemp.SJUK[1],"9") + ";" +
                        STRING(franvarotemp.SJUK[2],"9") + ";" +
                        STRING(franvarotemp.SJUK[3],"9") + ";" +
                        STRING(franvarotemp.SJUK[4],"9") + ";" +
                        STRING(franvarotemp.SJUK[5],"9") + ";" +
                        STRING(franvarotemp.SJUK[6],"9") + ";" +
                        STRING(franvarotemp.SJUK[7],"9") + ";" +
                        STRING(franvarotemp.SJUK[8],"9") + ";" +
                        STRING(franvarotemp.SJUK[9],"9") + ";" +
                        STRING(franvarotemp.SJUK[10],"9") + ";" +
                        STRING(franvarotemp.SJUK[11],"9") + ";" +
                        STRING(franvarotemp.SJUK[12],"9") + ";" +
                        STRING(franvarotemp.SJUK[13],"9") + ";" +
                        STRING(franvarotemp.SJUK[14],"9") + ";" +
                        "0;0;0;0;0;0;0;0;0;0;0;0;0;0;" +
                        "0;0;8;0;35;" +
                        STRING(franvarotemp.SJUK[15],"9") + ";" +
                        STRING(franvarotemp.SJUK[16],"9") + ";" +
                        STRING(franvarotemp.SJUK[17],"9") + ";" +
                        STRING(franvarotemp.SJUK[18],"9") + ";" +
                        STRING(franvarotemp.SJUK[19],"9") + ";" +
                        STRING(franvarotemp.SJUK[20],"9") + ";" +
                        STRING(franvarotemp.SJUK[21],"9") + ";" +                                             
                        "0;0;0;0;0;0;0;" +
                        SUBSTRING(franvarotemp.BPNR,1,10) + ";" +  
                        STRING(franvarotemp.LATTHELG) + ";" + 
                        "111".
            END.
         END.
         ELSE DO:         
            /*timanställda*/
            IF franvarotemp.TIMMAR > 0 THEN DO:
               overrapp1 = franvarotemp.PNR +
                        ";" + pekod + 
                        ";0;" +
                        STRING(franvarotemp.FRAN,"99999999") + ";" +
                        STRING(franvarotemp.TILL,"99999999") + ";" +
                        "0;0;" +
                        franvarotemp.LART + ";" +
                        "0;1;0;;" +                        
                        "0;0;" +
                        "0;" + 
                        "0;0;" +
                        "0;0;0;0;0;0;0;0;0;0;0;0;0;0;" +
                        STRING(franvarotemp.SJTIM[1],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[2],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[3],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[4],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[5],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[6],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[7],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[8],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[9],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[10],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[11],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[12],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[13],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[14],"9.99") + ";" +                                               
                        STRING(franvarotemp.FTIM,"9.99") + ";" +                         
                        "0;8;0;35;" +
                        "0;0;0;0;0;0;0;" +
                        STRING(franvarotemp.SJTIM[15],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[16],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[17],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[18],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[19],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[20],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[21],"9.99") + ";" +                                                                     
                        SUBSTRING(franvarotemp.BPNR,1,10) + ";" +
                        STRING(franvarotemp.LATTHELG) + ";" + 
                        "111".
            END.
            ELSE DO:
               overrapp1 = franvarotemp.PNR +
                        ";" + pekod +   
                        ";0;" +
                        STRING(franvarotemp.FRAN,"99999999") + ";" +
                        STRING(franvarotemp.TILL,"99999999") + ";" +
                        "0;0;" +
                        franvarotemp.LART + ";" +
                        "0;1;0;;" +                         
                        "0;0;" +
                        "0;" + 
                        "0;0;" +
                        "0;0;0;0;0;0;0;0;0;0;0;0;0;0;" +
                        STRING(franvarotemp.SJTIM[1],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[2],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[3],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[4],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[5],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[6],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[7],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[8],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[9],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[10],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[11],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[12],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[13],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[14],"9.99") + ";" +                                                
                        STRING(franvarotemp.FTIM,"9.99") + ";" +                         
                        "0;8;0;35;" +
                        "0;0;0;0;0;0;0;" +
                        STRING(franvarotemp.SJTIM[15],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[16],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[17],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[18],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[19],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[20],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[21],"9.99") + ";" +                                                                                             
                        SUBSTRING(franvarotemp.BPNR,1,10) + ";" +
                        STRING(franvarotemp.LATTHELG) + ";" +
                        "111".
            END.
         END.
      END.
      ELSE DO:
         IF franvarotemp.TIMMAR > 0 THEN DO:
            IF franvarotemp.LART = "Komp" OR franvarotemp.LART = "Atidkont" OR franvarotemp.LART = "LFKALM1" OR franvarotemp.LART = "Utb i tj" 
            OR franvarotemp.LART = "Led6juni" OR franvarotemp.LART = "Tjl ulön"  THEN DO:
                /*skall anges i timmar -ej omfattning. Om det gäller 1 dag används fältet franvarotemp.TIMMAR (antal timmar)
                  Gäller det flera dagar används fältet franvarotemp.TOTID (Totalt antal timmar under frånvaroperioden)*/
                IF franvarotemp.FRAN = franvarotemp.TILL THEN DO:               
                   IF franvarotemp.LART = "Tjl ulön" THEN franvarotemp.LART = "Tjl tim".
                    overrapp1 = franvarotemp.PNR +
                         ";" + pekod +  
                         ";0;" +
                         STRING(franvarotemp.FRAN,"99999999") + ";" +
                         STRING(franvarotemp.TILL,"99999999") + ";" +
                         "0;0;" +
                         franvarotemp.LART + ";" +          
                         STRING (0.000,"9.999") + ";" +                          
                         "1;" +                         
                         STRING((minut),"9999") + ";;" +
                         "0;0;" +
                         STRING(franvarotemp.ARBDAGAR) + ";" + 
                         "0;0;" +
                         "0;0;0;0;0;0;0;0;0;0;0;0;0;0;" +
                         "0;0;0;0;0;0;0;0;0;0;0;0;0;0;" +                         
                         "0;0;8;0;35;" +
                         "0;0;0;0;0;0;0;" +
                         "0;0;0;0;0;0;0;" +
                         SUBSTRING(franvarotemp.BPNR,1,10) + ";" + 
                         STRING(franvarotemp.LATTHELG) + ";" +
                         "111".
                END.
                ELSE DO:
                   overrapp1 = franvarotemp.PNR +
                         ";" + pekod + 
                         ";0;" +
                         STRING(franvarotemp.FRAN,"99999999") + ";" +
                         STRING(franvarotemp.TILL,"99999999") + ";" +
                         "0;0;" +
                         franvarotemp.LART + ";" +          
                         STRING (0.000,"9.999") + ";" +                          
                         "1;" +                         
                         "0;0;" +
                         STRING(franvarotemp.ARBDAGAR) + ";" + 
                         "0;0;" +
                         "0;0;0;0;0;0;0;0;0;0;0;0;0;0;" +
                         "0;0;0;0;0;0;0;0;0;0;0;0;0;0;" +
                         STRING((klock100(franvarotemp.TOTTID)),"999.99") + ";" +
                         "0;8;0;35;" +
                         "0;0;0;0;0;0;0;" +
                         "0;0;0;0;0;0;0;" +
                         SUBSTRING(franvarotemp.BPNR,1,10) + ";" +
                         STRING(franvarotemp.LATTHELG) + ";" +
                         "111".
                END.
            END.
            ELSE DO:         
               /*bara omfattning ska ut - inte timmar*/
               IF omfatt > 0 AND omfatt LE 1  THEN DO:               
                   overrapp1 = franvarotemp.PNR +
                         ";" + pekod + 
                         ";0;" +
                         STRING(franvarotemp.FRAN,"99999999") + ";" +
                         STRING(franvarotemp.TILL,"99999999") + ";" +
                         "0;0;" +
                         franvarotemp.LART + ";" +                     
                         STRING(omfatt,"9.999") + ";" +
                         "1;" +                   
                         STRING(0,"99.99") + ";;" +
                         "0;0;" +
                         STRING(franvarotemp.ARBDAGAR) + ";" + 
                         "0;0;" +
                         "0;0;0;0;0;0;0;0;0;0;0;0;0;0;" +
                         "0;0;0;0;0;0;0;0;0;0;0;0;0;0;" +
                         "0;0;8;0;35;" +
                         "0;0;0;0;0;0;0;" +
                         "0;0;0;0;0;0;0;" +
                         SUBSTRING(franvarotemp.BPNR,1,10) + ";" +
                         STRING(franvarotemp.LATTHELG) + ";" +
                         "111".
               END.
               ELSE /*IF franvarotemp.LART = "Utb i tj" THEN*/ DO:                  
                   IF franvarotemp.TIMMAR > 0 AND franvarotemp.TTId = 0 THEN DO:
                   /* Ingen ordinarie arbetstid men "Utb i tj"- skicka omfattning 0 och antal timmar*/
                      overrapp1 = franvarotemp.PNR +
                         ";" + pekod +  
                         ";0;" +
                         STRING(franvarotemp.FRAN,"99999999") + ";" +
                         STRING(franvarotemp.TILL,"99999999") + ";" +
                         "0;0;" +
                         franvarotemp.LART + ";" +                     
                         STRING(0,"9.999") + ";" +
                         "1;" +                                            
                         STRING(minut,"9999") + ";;" +
                         "0;0;" +
                         STRING(franvarotemp.ARBDAGAR) + ";" + 
                         "0;0;" +
                         "0;0;0;0;0;0;0;0;0;0;0;0;0;0;" +
                         "0;0;0;0;0;0;0;0;0;0;0;0;0;0;" +
                         "0;0;8;0;35;" +
                         "0;0;0;0;0;0;0;" +
                         "0;0;0;0;0;0;0;" +
                         SUBSTRING(franvarotemp.BPNR,1,10) + ";" +
                         STRING(franvarotemp.LATTHELG) + ";" +
                         "111".
                   END.
                   ELSE IF franvarotemp.TIMMAR > 0 AND omfatt > 1  THEN DO:               
                      /* Liten ordinarie arbetstid tex 22-24 , men "Utb i tj"- skicka omfattning 0 och antal timmar*/
                      overrapp1 = franvarotemp.PNR +
                         ";" + pekod + 
                         ";0;" +
                         STRING(franvarotemp.FRAN,"99999999") + ";" +
                         STRING(franvarotemp.TILL,"99999999") + ";" +
                         "0;0;" +
                         franvarotemp.LART + ";" +                     
                         STRING(0,"9.999") + ";" +
                         "1;" +                                            
                         STRING(minut,"9999") + ";;" +
                         "0;0;" +
                         STRING(franvarotemp.ARBDAGAR) + ";" + 
                         "0;0;" +
                         "0;0;0;0;0;0;0;0;0;0;0;0;0;0;" +
                         "0;0;0;0;0;0;0;0;0;0;0;0;0;0;" +
                         "0;0;8;0;35;" +
                         "0;0;0;0;0;0;0;" +
                         "0;0;0;0;0;0;0;" +
                         SUBSTRING(franvarotemp.BPNR,1,10) + ";" +
                         STRING(franvarotemp.LATTHELG) + ";" +
                         "111".
                   END.
               END.
        
            END.

         END.
         ELSE DO:         
            overrapp1 = franvarotemp.PNR +
                     ";" + pekod +  
                     ";0;" +
                     STRING(franvarotemp.FRAN,"99999999") + ";" +
                     STRING(franvarotemp.TILL,"99999999") + ";" +
                     "0;0;" +
                     franvarotemp.LART + ";" +
                     STRING(1.000,"9.999") + ";" +
                     "1;" +
                     STRING(0,"99.99") + ";;0;0;" +
                     STRING(franvarotemp.ARBDAGAR) + ";" + 
                     "0;0;" +
                     "0;0;0;0;0;0;0;0;0;0;0;0;0;0;" +
                     "0;0;0;0;0;0;0;0;0;0;0;0;0;0;" +
                     "0;0;8;0;35;" +
                     "0;0;0;0;0;0;0;" +
                     "0;0;0;0;0;0;0;" +
                     SUBSTRING(franvarotemp.BPNR,1,10) + ";" +  
                     STRING(franvarotemp.LATTHELG) + ";" +
                     "111".
         END.
      

      END.     
      IF overrapp1 NE "" THEN DO:      
         IF  franvarotemp.LART = "SJUK" THEN DO:
            PUT STREAM sjuksmaland overrapp1 SKIP. 
         END.   
         PUT STREAM frsmaland overrapp1 SKIP.
      END.
      DELETE franvarotemp.
      overrapp1 = "".

   END.
   PUT SKIP.
   OUTPUT STREAM sjuksmaland CLOSE.      
   OUTPUT STREAM frsmaland CLOSE.


   /*SÄVSJÖ*/
   IF Guru.Konstanter:globforetag = "GKAL" THEN DO:      
      IF korvar = "" THEN DO:      
         prognamn3 = prognamn2 + "frkollSÄVSJÖ.d".
         OUTPUT STREAM sjuksmaland TO VALUE(prognamn3) NO-ECHO.   
         prognamn3 = prognamn2 + "franSÄVSJÖ.d".
         OUTPUT STREAM frsavsjo TO VALUE(prognamn3) NO-ECHO.
        
      END.
      ELSE DO:
         prognamn3 = prognamn2 + "frkollomkSÄVSJÖ.d".
         OUTPUT STREAM sjuksmaland TO VALUE(prognamn3) NO-ECHO.   
         prognamn3 = prognamn2 + "franomkSÄVSJÖ.d".
         OUTPUT STREAM frsavsjo TO VALUE(prognamn3) NO-ECHO.         
      END.
      
   END.       
   ELSE IF Guru.Konstanter:globforetag = "ELPA" THEN DO:
      IF korvar = "" THEN DO:      
         prognamn3 = prognamn2 + "frkollSÄVSJÖ.d".
         OUTPUT STREAM sjuksmaland TO VALUE(prognamn3) NO-ECHO.   
         prognamn3 = prognamn2 + "franSÄVSJÖ.d".
         OUTPUT STREAM frsavsjo TO VALUE(prognamn3) NO-ECHO.        
      END.
      ELSE DO:      
         prognamn3 = prognamn2 + "frkollomkSÄVSJÖ.d".
         OUTPUT STREAM sjuksmaland TO VALUE(prognamn3) NO-ECHO.   
         prognamn3 = prognamn2 + "franomkSÄVSJÖ.d".
         OUTPUT STREAM frsavsjo TO VALUE(prognamn3) NO-ECHO.         
      END.
      
   END.            
   radrakn = 1.
   FOR EACH franvarotemp WHERE franvarotemp.VIJUDID = "STB" :
      radrakn = radrakn + 1.
   END.


   overrapp1 = "7048;" + STRING(radrakn) + ";0;" + string(TODAY,"99999999") + ";" + STRING(TIME,"HH:MM").             
   PUT STREAM frsavsjo overrapp1 SKIP.
   PUT SKIP.

   
   FOR EACH franvarotemp WHERE franvarotemp.VIJUDID = "STB" USE-INDEX KALMAR:
      /*IF franvarotemp.PNR = "7802110176"  THEN pekod = "23120100".
      ELSE pekod = "0".*/
      ASSIGN
      pekod = "0"
      omfatt = 0
      minut = 0.
      IF franvarotemp.TIMMAR > 0 AND franvarotemp.TTID > 0 THEN DO:      
         omfatt = franvarotemp.TIMMAR / franvarotemp.TTID.
         IF omfatt > 1 THEN omfatt = 1.
      END.
      IF franvarotemp.TIMMAR > 0 THEN DO:
         minut = franvarotemp.TIMMAR * 60.
         
      END.
      IF franvarotemp.ORSAK = "SJUK" THEN DO:
         IF franvarotemp.TIMANSTALLD = FALSE THEN DO:         
            IF franvarotemp.TIMMAR > 0 THEN DO:
               /*del av dag - bara omfattning om det är sjuk annars 0.000*/               
                /*bara omfattning , inga timmar.*/
               overrapp1 = franvarotemp.PNR +
                        ";" + pekod + 
                        ";0;" +
                        STRING(franvarotemp.FRAN,"99999999") + ";" +
                        STRING(franvarotemp.TILL,"99999999") + ";" +
                        "0;0;" +
                        franvarotemp.LART + ";" +
                        STRING(omfatt,"9.999") + ";" +
                        "1;" +
                        "00.00;;" +
                        "0;0;" +
                        "0;" + 
                        "0;0;" +
                        STRING(franvarotemp.SJUK[1],"9") + ";" +
                        STRING(franvarotemp.SJUK[2],"9") + ";" +
                        STRING(franvarotemp.SJUK[3],"9") + ";" +
                        STRING(franvarotemp.SJUK[4],"9") + ";" +
                        STRING(franvarotemp.SJUK[5],"9") + ";" +
                        STRING(franvarotemp.SJUK[6],"9") + ";" +
                        STRING(franvarotemp.SJUK[7],"9") + ";" +
                        STRING(franvarotemp.SJUK[8],"9") + ";" +
                        STRING(franvarotemp.SJUK[9],"9") + ";" +
                        STRING(franvarotemp.SJUK[10],"9") + ";" +
                        STRING(franvarotemp.SJUK[11],"9") + ";" +
                        STRING(franvarotemp.SJUK[12],"9") + ";" +
                        STRING(franvarotemp.SJUK[13],"9") + ";" +
                        STRING(franvarotemp.SJUK[14],"9") + ";" +
                        "0;0;0;0;0;0;0;0;0;0;0;0;0;0;" +
                        "0;0;8;0;35;" +
                        STRING(franvarotemp.SJUK[15],"9") + ";" +
                        STRING(franvarotemp.SJUK[16],"9") + ";" +
                        STRING(franvarotemp.SJUK[17],"9") + ";" +
                        STRING(franvarotemp.SJUK[18],"9") + ";" +
                        STRING(franvarotemp.SJUK[19],"9") + ";" +
                        STRING(franvarotemp.SJUK[20],"9") + ";" +
                        STRING(franvarotemp.SJUK[21],"9") + ";" +                                             
                        "0;0;0;0;0;0;0;" +
                        SUBSTRING(franvarotemp.BPNR,1,10) + ";" +
                        STRING(franvarotemp.LATTHELG) + ";" +
                        "111".
            END.
            ELSE DO:
               overrapp1 = franvarotemp.PNR +
                        ";" + pekod + 
                        ";0;" +
                        STRING(franvarotemp.FRAN,"99999999") + ";" +
                        STRING(franvarotemp.TILL,"99999999") + ";" +
                        "0;0;" +
                        franvarotemp.LART + ";" +
                        STRING (1.000,"9.999") + ";" +                    
                        "1;" +
                        STRING(0,"99.99") + ";;" +
                        "0;0;" +
                        "0;" + 
                        "0;0;" +
                        STRING(franvarotemp.SJUK[1],"9") + ";" +
                        STRING(franvarotemp.SJUK[2],"9") + ";" +
                        STRING(franvarotemp.SJUK[3],"9") + ";" +
                        STRING(franvarotemp.SJUK[4],"9") + ";" +
                        STRING(franvarotemp.SJUK[5],"9") + ";" +
                        STRING(franvarotemp.SJUK[6],"9") + ";" +
                        STRING(franvarotemp.SJUK[7],"9") + ";" +
                        STRING(franvarotemp.SJUK[8],"9") + ";" +
                        STRING(franvarotemp.SJUK[9],"9") + ";" +
                        STRING(franvarotemp.SJUK[10],"9") + ";" +
                        STRING(franvarotemp.SJUK[11],"9") + ";" +
                        STRING(franvarotemp.SJUK[12],"9") + ";" +
                        STRING(franvarotemp.SJUK[13],"9") + ";" +
                        STRING(franvarotemp.SJUK[14],"9") + ";" +
                        "0;0;0;0;0;0;0;0;0;0;0;0;0;0;" +
                        "0;0;8;0;35;" +
                        STRING(franvarotemp.SJUK[15],"9") + ";" +
                        STRING(franvarotemp.SJUK[16],"9") + ";" +
                        STRING(franvarotemp.SJUK[17],"9") + ";" +
                        STRING(franvarotemp.SJUK[18],"9") + ";" +
                        STRING(franvarotemp.SJUK[19],"9") + ";" +
                        STRING(franvarotemp.SJUK[20],"9") + ";" +
                        STRING(franvarotemp.SJUK[21],"9") + ";" +                                             
                        "0;0;0;0;0;0;0;" +
                        SUBSTRING(franvarotemp.BPNR,1,10) + ";" +
                        STRING(franvarotemp.LATTHELG) + ";" +
                        "111".
            END.
         END.
         ELSE DO:         
            /*timanställda*/
            IF franvarotemp.TIMMAR > 0 THEN DO:
               overrapp1 = franvarotemp.PNR +
                        ";" + pekod + 
                        ";0;" +
                        STRING(franvarotemp.FRAN,"99999999") + ";" +
                        STRING(franvarotemp.TILL,"99999999") + ";" +
                        "0;0;" +
                        franvarotemp.LART + ";" +
                        "0;1;0;;" +                        
                        "0;0;" +
                        "0;" + 
                        "0;0;" +
                        "0;0;0;0;0;0;0;0;0;0;0;0;0;0;" +
                        STRING(franvarotemp.SJTIM[1],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[2],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[3],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[4],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[5],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[6],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[7],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[8],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[9],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[10],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[11],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[12],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[13],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[14],"9.99") + ";" +                        
                        STRING(franvarotemp.FTIM,"9.99") + ";" +                         
                        "0;8;0;35;" +
                        "0;0;0;0;0;0;0;" +
                        STRING(franvarotemp.SJTIM[15],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[16],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[17],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[18],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[19],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[20],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[21],"9.99") + ";" +                                                                     
                        SUBSTRING(franvarotemp.BPNR,1,10) + ";" +
                        STRING(franvarotemp.LATTHELG) + ";" +
                        "111".
            END.
            ELSE DO:
               overrapp1 = franvarotemp.PNR +
                        ";" + pekod +   
                        ";0;" +
                        STRING(franvarotemp.FRAN,"99999999") + ";" +
                        STRING(franvarotemp.TILL,"99999999") + ";" +
                        "0;0;" +
                        franvarotemp.LART + ";" +
                        "0;1;0;;" +                        
                        "0;0;" +
                        "0;" + 
                        "0;0;" +
                        "0;0;0;0;0;0;0;0;0;0;0;0;0;0;" +
                        STRING(franvarotemp.SJTIM[1],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[2],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[3],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[4],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[5],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[6],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[7],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[8],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[9],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[10],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[11],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[12],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[13],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[14],"9.99") + ";" +                                                
                        STRING(franvarotemp.FTIM,"9.99") + ";" +                         
                        "0;8;0;35;" +
                        "0;0;0;0;0;0;0;" +
                        STRING(franvarotemp.SJTIM[15],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[16],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[17],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[18],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[19],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[20],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[21],"9.99") + ";" +                                                                                             
                        SUBSTRING(franvarotemp.BPNR,1,10) + ";" + 
                        STRING(franvarotemp.LATTHELG) + ";" +
                        "111".
            END.
         END.
      END.
      ELSE DO:
         IF franvarotemp.TIMMAR > 0 THEN DO:
            IF franvarotemp.LART = "Komp" OR franvarotemp.LART = "Atidkont" OR franvarotemp.LART = "LFKALM1" OR franvarotemp.LART = "Utb i tj" 
            OR franvarotemp.LART = "Led6juni" OR franvarotemp.LART = "Tjl ulön" THEN DO:
                /*skall anges i timmar -ej omfattning. Om det gäller 1 dag används fältet franvarotemp.TIMMAR (antal timmar)
                  Gäller det flera dagar används fältet franvarotemp.TOTID (Totalt antal timmar under frånvaroperioden)*/
                IF franvarotemp.FRAN = franvarotemp.TILL THEN DO:               
                   IF franvarotemp.LART = "Tjl ulön" THEN franvarotemp.LART = "Tjl tim".
                    overrapp1 = franvarotemp.PNR +
                         ";" + pekod +  
                         ";0;" +
                         STRING(franvarotemp.FRAN,"99999999") + ";" +
                         STRING(franvarotemp.TILL,"99999999") + ";" +
                         "0;0;" +
                         franvarotemp.LART + ";" +          
                         STRING (0.000,"9.999") + ";" +                          
                         "1;" +                         
                         STRING((minut),"9999") + ";;" +
                         "0;0;" +
                         STRING(franvarotemp.ARBDAGAR) + ";" + 
                         "0;0;" +
                         "0;0;0;0;0;0;0;0;0;0;0;0;0;0;" +
                         "0;0;0;0;0;0;0;0;0;0;0;0;0;0;" +                         
                         "0;0;8;0;35;" +
                         "0;0;0;0;0;0;0;" +
                         "0;0;0;0;0;0;0;" +
                         SUBSTRING(franvarotemp.BPNR,1,10) + ";" +
                         STRING(franvarotemp.LATTHELG) + ";" +
                         "111".
                END.
                ELSE DO:
                   overrapp1 = franvarotemp.PNR +
                         ";" + pekod + 
                         ";0;" +
                         STRING(franvarotemp.FRAN,"99999999") + ";" +
                         STRING(franvarotemp.TILL,"99999999") + ";" +
                         "0;0;" +
                         franvarotemp.LART + ";" +          
                         STRING (0.000,"9.999") + ";" +                          
                         "1;" +                         
                         "0;0;" +
                         STRING(franvarotemp.ARBDAGAR) + ";" + 
                         "0;0;" +
                         "0;0;0;0;0;0;0;0;0;0;0;0;0;0;" +
                         "0;0;0;0;0;0;0;0;0;0;0;0;0;0;" +
                         STRING((klock100(franvarotemp.TOTTID)),"999.99") + ";" +
                         "0;8;0;35;" +
                         "0;0;0;0;0;0;0;" +
                         "0;0;0;0;0;0;0;" +
                         SUBSTRING(franvarotemp.BPNR,1,10) + ";" +
                         STRING(franvarotemp.LATTHELG) + ";" +
                         "111".
                END.
            END.
            ELSE DO:         
               /*bara omfattning ska ut - inte timmar*/
               IF omfatt > 0 AND omfatt LE 1  THEN DO:               
                   overrapp1 = franvarotemp.PNR +
                         ";" + pekod + 
                         ";0;" +
                         STRING(franvarotemp.FRAN,"99999999") + ";" +
                         STRING(franvarotemp.TILL,"99999999") + ";" +
                         "0;0;" +
                         franvarotemp.LART + ";" +                     
                         STRING(omfatt,"9.999") + ";" +
                         "1;" +                   
                         STRING(0,"99.99") + ";;" +
                         "0;0;" +
                         STRING(franvarotemp.ARBDAGAR) + ";" + 
                         "0;0;" +
                         "0;0;0;0;0;0;0;0;0;0;0;0;0;0;" +
                         "0;0;0;0;0;0;0;0;0;0;0;0;0;0;" +
                         "0;0;8;0;35;" +
                         "0;0;0;0;0;0;0;" +
                         "0;0;0;0;0;0;0;" +
                         SUBSTRING(franvarotemp.BPNR,1,10) + ";" +
                         STRING(franvarotemp.LATTHELG) + ";" +
                         "111".
               END.
               ELSE /*IF franvarotemp.LART = "Utb i tj" THEN*/ DO:                  
                   IF franvarotemp.TIMMAR > 0 AND franvarotemp.TTId = 0 THEN DO:
                   /* Ingen ordinarie arbetstid men "Utb i tj"- skicka omfattning 0 och antal timmar*/
                      overrapp1 = franvarotemp.PNR +
                         ";" + pekod +  
                         ";0;" +
                         STRING(franvarotemp.FRAN,"99999999") + ";" +
                         STRING(franvarotemp.TILL,"99999999") + ";" +
                         "0;0;" +
                         franvarotemp.LART + ";" +                     
                         STRING(0,"9.999") + ";" +
                         "1;" +                                            
                         STRING(minut,"9999") + ";;" +
                         "0;0;" +
                         STRING(franvarotemp.ARBDAGAR) + ";" + 
                         "0;0;" +
                         "0;0;0;0;0;0;0;0;0;0;0;0;0;0;" +
                         "0;0;0;0;0;0;0;0;0;0;0;0;0;0;" +
                         "0;0;8;0;35;" +
                         "0;0;0;0;0;0;0;" +
                         "0;0;0;0;0;0;0;" +
                         SUBSTRING(franvarotemp.BPNR,1,10) + ";" +
                         STRING(franvarotemp.LATTHELG) + ";" +
                         "111".
                   END.
                   ELSE IF franvarotemp.TIMMAR > 0 AND omfatt > 1  THEN DO:               
                      /* Liten ordinarie arbetstid tex 22-24 , men "Utb i tj"- skicka omfattning 0 och antal timmar*/
                      overrapp1 = franvarotemp.PNR +
                         ";" + pekod + 
                         ";0;" +
                         STRING(franvarotemp.FRAN,"99999999") + ";" +
                         STRING(franvarotemp.TILL,"99999999") + ";" +
                         "0;0;" +
                         franvarotemp.LART + ";" +                     
                         STRING(0,"9.999") + ";" +
                         "1;" +                                            
                         STRING(minut,"9999") + ";;" +
                         "0;0;" +
                         STRING(franvarotemp.ARBDAGAR) + ";" + 
                         "0;0;" +
                         "0;0;0;0;0;0;0;0;0;0;0;0;0;0;" +
                         "0;0;0;0;0;0;0;0;0;0;0;0;0;0;" +
                         "0;0;8;0;35;" +
                         "0;0;0;0;0;0;0;" +
                         "0;0;0;0;0;0;0;" +
                         SUBSTRING(franvarotemp.BPNR,1,10) + ";" +
                         STRING(franvarotemp.LATTHELG) + ";" +
                         "111".
                   END.
               END.
            
            END.

         END.
         ELSE DO:         
            overrapp1 = franvarotemp.PNR +
                     ";" + pekod +  
                     ";0;" +
                     STRING(franvarotemp.FRAN,"99999999") + ";" +
                     STRING(franvarotemp.TILL,"99999999") + ";" +
                     "0;0;" +
                     franvarotemp.LART + ";" +
                     STRING(1.000,"9.999") + ";" +
                     "1;" +
                     STRING(0,"99.99") + ";;0;0;" +
                     STRING(franvarotemp.ARBDAGAR) + ";" + 
                     "0;0;" +
                     "0;0;0;0;0;0;0;0;0;0;0;0;0;0;" +
                     "0;0;0;0;0;0;0;0;0;0;0;0;0;0;" +
                     "0;0;8;0;35;" +
                     "0;0;0;0;0;0;0;" +
                     "0;0;0;0;0;0;0;" +
                     SUBSTRING(franvarotemp.BPNR,1,10) + ";" +  
                     STRING(franvarotemp.LATTHELG) + ";" +
                     "111".
         END.
      END.            
      IF overrapp1 NE "" THEN DO:      
         IF  franvarotemp.LART = "SJUK" THEN DO:
            PUT STREAM sjuksmaland overrapp1 SKIP. 
         END.   
         PUT STREAM frsavsjo overrapp1 SKIP.
      END.
      DELETE franvarotemp.
      overrapp1 = "".
   END.
   PUT SKIP.
   OUTPUT STREAM sjuksmaland CLOSE.      
   OUTPUT STREAM frsavsjo CLOSE.

   /*Värme*/
   IF Guru.Konstanter:globforetag = "GKAL" THEN DO:      
      IF korvar = "" THEN DO:      
         prognamn3 = prognamn2 + "frkollVÄRME.d".
         OUTPUT STREAM sjuksmaland TO VALUE(prognamn3) NO-ECHO.   
         prognamn3 = prognamn2 + "franVÄRME.d".
         OUTPUT STREAM frsavsjo TO VALUE(prognamn3) NO-ECHO.
        
      END.
      ELSE DO:
         prognamn3 = prognamn2 + "frkollomkVÄRME.d".
         OUTPUT STREAM sjuksmaland TO VALUE(prognamn3) NO-ECHO.   
         prognamn3 = prognamn2 + "franomkVÄRME.d".
         OUTPUT STREAM frsavsjo TO VALUE(prognamn3) NO-ECHO.         
      END.      
   END.       
   ELSE IF Guru.Konstanter:globforetag = "ELPA" THEN DO:
      IF korvar = "" THEN DO:      
         prognamn3 = prognamn2 + "frkollVÄRME.d".
         OUTPUT STREAM sjuksmaland TO VALUE(prognamn3) NO-ECHO.   
         prognamn3 = prognamn2 + "franVÄRME.d".
         OUTPUT STREAM frsavsjo TO VALUE(prognamn3) NO-ECHO.        
      END.
      ELSE DO:      
         prognamn3 = prognamn2 + "frkollomkVÄRME.d".
         OUTPUT STREAM sjuksmaland TO VALUE(prognamn3) NO-ECHO.   
         prognamn3 = prognamn2 + "franomkVÄRME.d".
         OUTPUT STREAM frsavsjo TO VALUE(prognamn3) NO-ECHO.         
      END.     
   END.            
   radrakn = 1.
   FOR EACH franvarotemp WHERE franvarotemp.VIJUDID = "KEV" :
      radrakn = radrakn + 1.
   END.
   overrapp1 = "7048;" + STRING(radrakn) + ";0;" + string(TODAY,"99999999") + ";" + STRING(TIME,"HH:MM").             
   PUT STREAM frsavsjo overrapp1 SKIP.
   PUT SKIP.
   FOR EACH franvarotemp WHERE franvarotemp.VIJUDID = "KEV" USE-INDEX KALMAR:
      /*IF franvarotemp.PNR = "7802110176"  THEN pekod = "23120100".
      ELSE pekod = "0".*/
      ASSIGN
      pekod = "0"
      omfatt = 0
      minut = 0.
      IF franvarotemp.TIMMAR > 0 AND franvarotemp.TTID > 0 THEN DO:      
         omfatt = franvarotemp.TIMMAR / franvarotemp.TTID.
         IF omfatt > 1 THEN omfatt = 1.
      END.
      IF franvarotemp.TIMMAR > 0 THEN DO:
         minut = franvarotemp.TIMMAR * 60.         
      END.
      IF franvarotemp.ORSAK = "SJUK" THEN DO:
         IF franvarotemp.TIMANSTALLD = FALSE THEN DO:         
            IF franvarotemp.TIMMAR > 0 THEN DO:
               /*del av dag - bara omfattning om det är sjuk annars 0.000*/               
                /*bara omfattning , inga timmar.*/
               overrapp1 = franvarotemp.PNR +
                        ";" + pekod + 
                        ";0;" +
                        STRING(franvarotemp.FRAN,"99999999") + ";" +
                        STRING(franvarotemp.TILL,"99999999") + ";" +
                        "0;0;" +
                        franvarotemp.LART + ";" +
                        STRING(omfatt,"9.999") + ";" +
                        "1;" +
                        "00.00;;" +
                        "0;0;" +
                        "0;" + 
                        "0;0;" +
                        STRING(franvarotemp.SJUK[1],"9") + ";" +
                        STRING(franvarotemp.SJUK[2],"9") + ";" +
                        STRING(franvarotemp.SJUK[3],"9") + ";" +
                        STRING(franvarotemp.SJUK[4],"9") + ";" +
                        STRING(franvarotemp.SJUK[5],"9") + ";" +
                        STRING(franvarotemp.SJUK[6],"9") + ";" +
                        STRING(franvarotemp.SJUK[7],"9") + ";" +
                        STRING(franvarotemp.SJUK[8],"9") + ";" +
                        STRING(franvarotemp.SJUK[9],"9") + ";" +
                        STRING(franvarotemp.SJUK[10],"9") + ";" +
                        STRING(franvarotemp.SJUK[11],"9") + ";" +
                        STRING(franvarotemp.SJUK[12],"9") + ";" +
                        STRING(franvarotemp.SJUK[13],"9") + ";" +
                        STRING(franvarotemp.SJUK[14],"9") + ";" +
                        "0;0;0;0;0;0;0;0;0;0;0;0;0;0;" +
                        "0;0;8;0;35;" +
                        STRING(franvarotemp.SJUK[15],"9") + ";" +
                        STRING(franvarotemp.SJUK[16],"9") + ";" +
                        STRING(franvarotemp.SJUK[17],"9") + ";" +
                        STRING(franvarotemp.SJUK[18],"9") + ";" +
                        STRING(franvarotemp.SJUK[19],"9") + ";" +
                        STRING(franvarotemp.SJUK[20],"9") + ";" +
                        STRING(franvarotemp.SJUK[21],"9") + ";" +                                             
                        "0;0;0;0;0;0;0;" +
                        SUBSTRING(franvarotemp.BPNR,1,10) + ";" +
                        STRING(franvarotemp.LATTHELG) + ";" +
                        "111".
            END.
            ELSE DO:
               overrapp1 = franvarotemp.PNR +
                        ";" + pekod + 
                        ";0;" +
                        STRING(franvarotemp.FRAN,"99999999") + ";" +
                        STRING(franvarotemp.TILL,"99999999") + ";" +
                        "0;0;" +
                        franvarotemp.LART + ";" +
                        STRING (1.000,"9.999") + ";" +                      
                        "1;" +
                        STRING(0,"99.99") + ";;" +
                        "0;0;" +
                        "0;" + 
                        "0;0;" +
                        STRING(franvarotemp.SJUK[1],"9") + ";" +
                        STRING(franvarotemp.SJUK[2],"9") + ";" +
                        STRING(franvarotemp.SJUK[3],"9") + ";" +
                        STRING(franvarotemp.SJUK[4],"9") + ";" +
                        STRING(franvarotemp.SJUK[5],"9") + ";" +
                        STRING(franvarotemp.SJUK[6],"9") + ";" +
                        STRING(franvarotemp.SJUK[7],"9") + ";" +
                        STRING(franvarotemp.SJUK[8],"9") + ";" +
                        STRING(franvarotemp.SJUK[9],"9") + ";" +
                        STRING(franvarotemp.SJUK[10],"9") + ";" +
                        STRING(franvarotemp.SJUK[11],"9") + ";" +
                        STRING(franvarotemp.SJUK[12],"9") + ";" +
                        STRING(franvarotemp.SJUK[13],"9") + ";" +
                        STRING(franvarotemp.SJUK[14],"9") + ";" +
                        "0;0;0;0;0;0;0;0;0;0;0;0;0;0;" +
                        "0;0;8;0;35;" +
                        STRING(franvarotemp.SJUK[15],"9") + ";" +
                        STRING(franvarotemp.SJUK[16],"9") + ";" +
                        STRING(franvarotemp.SJUK[17],"9") + ";" +
                        STRING(franvarotemp.SJUK[18],"9") + ";" +
                        STRING(franvarotemp.SJUK[19],"9") + ";" +
                        STRING(franvarotemp.SJUK[20],"9") + ";" +
                        STRING(franvarotemp.SJUK[21],"9") + ";" +                                             
                        "0;0;0;0;0;0;0;" +
                        SUBSTRING(franvarotemp.BPNR,1,10) + ";" +
                        STRING(franvarotemp.LATTHELG) + ";" +
                        "111".
            END.
         END.
         ELSE DO:         
            /*timanställda*/
            IF franvarotemp.TIMMAR > 0 THEN DO:
               overrapp1 = franvarotemp.PNR +
                        ";" + pekod + 
                        ";0;" +
                        STRING(franvarotemp.FRAN,"99999999") + ";" +
                        STRING(franvarotemp.TILL,"99999999") + ";" +
                        "0;0;" +
                        franvarotemp.LART + ";" +
                        "0;1;0;;" +                        
                        "0;0;" +
                        "0;" + 
                        "0;0;" +
                        "0;0;0;0;0;0;0;0;0;0;0;0;0;0;" +
                        STRING(franvarotemp.SJTIM[1],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[2],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[3],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[4],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[5],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[6],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[7],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[8],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[9],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[10],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[11],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[12],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[13],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[14],"9.99") + ";" +                        
                        STRING(franvarotemp.FTIM,"9.99") + ";" +                         
                        "0;8;0;35;" +
                        "0;0;0;0;0;0;0;" +
                        STRING(franvarotemp.SJTIM[15],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[16],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[17],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[18],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[19],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[20],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[21],"9.99") + ";" +                                                                     
                        SUBSTRING(franvarotemp.BPNR,1,10) + ";" +
                        STRING(franvarotemp.LATTHELG) + ";" +
                        "111".
            END.
            ELSE DO:
               overrapp1 = franvarotemp.PNR +
                        ";" + pekod +   
                        ";0;" +
                        STRING(franvarotemp.FRAN,"99999999") + ";" +
                        STRING(franvarotemp.TILL,"99999999") + ";" +
                        "0;0;" +
                        franvarotemp.LART + ";" +
                        "0;1;0;;" +                        
                        "0;0;" +
                        "0;" + 
                        "0;0;" +
                        "0;0;0;0;0;0;0;0;0;0;0;0;0;0;" +
                        STRING(franvarotemp.SJTIM[1],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[2],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[3],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[4],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[5],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[6],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[7],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[8],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[9],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[10],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[11],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[12],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[13],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[14],"9.99") + ";" +                                                
                        STRING(franvarotemp.FTIM,"9.99") + ";" +                         
                        "0;8;0;35;" +
                        "0;0;0;0;0;0;0;" +
                        STRING(franvarotemp.SJTIM[15],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[16],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[17],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[18],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[19],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[20],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[21],"9.99") + ";" +                                                                                             
                        SUBSTRING(franvarotemp.BPNR,1,10) + ";" +
                        STRING(franvarotemp.LATTHELG) + ";" +
                        "111".
            END.
         END.
      END.
      ELSE DO:
         IF franvarotemp.TIMMAR > 0 THEN DO:
            IF franvarotemp.LART = "Komp" OR franvarotemp.LART = "Atidkont" OR franvarotemp.LART = "LFKALM1" OR franvarotemp.LART = "Utb i tj" 
            OR franvarotemp.LART = "Led6juni" OR franvarotemp.LART = "Tjl ulön" THEN DO:
                /*skall anges i timmar -ej omfattning. Om det gäller 1 dag används fältet franvarotemp.TIMMAR (antal timmar)
                  Gäller det flera dagar används fältet franvarotemp.TOTID (Totalt antal timmar under frånvaroperioden)*/
                IF franvarotemp.FRAN = franvarotemp.TILL THEN DO:               
                   IF franvarotemp.LART = "Tjl ulön" THEN franvarotemp.LART = "Tjl tim".
                    overrapp1 = franvarotemp.PNR +
                         ";" + pekod +  
                         ";0;" +
                         STRING(franvarotemp.FRAN,"99999999") + ";" +
                         STRING(franvarotemp.TILL,"99999999") + ";" +
                         "0;0;" +
                         franvarotemp.LART + ";" +          
                         STRING (0.000,"9.999") + ";" +                          
                         "1;" +                         
                         STRING((minut),"9999") + ";;" +
                         "0;0;" +
                         STRING(franvarotemp.ARBDAGAR) + ";" + 
                         "0;0;" +
                         "0;0;0;0;0;0;0;0;0;0;0;0;0;0;" +
                         "0;0;0;0;0;0;0;0;0;0;0;0;0;0;" +                         
                         "0;0;8;0;35;" +
                         "0;0;0;0;0;0;0;" +
                         "0;0;0;0;0;0;0;" +
                         SUBSTRING(franvarotemp.BPNR,1,10) + ";" +
                         STRING(franvarotemp.LATTHELG) + ";" +
                         "111".
                END.
                ELSE DO:
                   overrapp1 = franvarotemp.PNR +
                         ";" + pekod + 
                         ";0;" +
                         STRING(franvarotemp.FRAN,"99999999") + ";" +
                         STRING(franvarotemp.TILL,"99999999") + ";" +
                         "0;0;" +
                         franvarotemp.LART + ";" +          
                         STRING (0.000,"9.999") + ";" +                          
                         "1;" +                         
                         "0;0;" +
                         STRING(franvarotemp.ARBDAGAR) + ";" + 
                         "0;0;" +
                         "0;0;0;0;0;0;0;0;0;0;0;0;0;0;" +
                         "0;0;0;0;0;0;0;0;0;0;0;0;0;0;" +
                         STRING((klock100(franvarotemp.TOTTID)),"999.99") + ";" +
                         "0;8;0;35;" +
                         "0;0;0;0;0;0;0;" +
                         "0;0;0;0;0;0;0;" +
                         SUBSTRING(franvarotemp.BPNR,1,10) + ";" +
                         STRING(franvarotemp.LATTHELG) + ";" +
                         "111".
                END.
            END.
            ELSE DO:         
               /*bara omfattning ska ut - inte timmar*/
               IF omfatt > 0 AND omfatt LE 1  THEN DO:               
                   overrapp1 = franvarotemp.PNR +
                         ";" + pekod + 
                         ";0;" +
                         STRING(franvarotemp.FRAN,"99999999") + ";" +
                         STRING(franvarotemp.TILL,"99999999") + ";" +
                         "0;0;" +
                         franvarotemp.LART + ";" +                     
                         STRING(omfatt,"9.999") + ";" +
                         "1;" +                   
                         STRING(0,"99.99") + ";;" +
                         "0;0;" +
                         STRING(franvarotemp.ARBDAGAR) + ";" + 
                         "0;0;" +
                         "0;0;0;0;0;0;0;0;0;0;0;0;0;0;" +
                         "0;0;0;0;0;0;0;0;0;0;0;0;0;0;" +
                         "0;0;8;0;35;" +
                         "0;0;0;0;0;0;0;" +
                         "0;0;0;0;0;0;0;" +
                         SUBSTRING(franvarotemp.BPNR,1,10) + ";" +
                         STRING(franvarotemp.LATTHELG) + ";" +
                         "111".
               END.
               ELSE /*IF franvarotemp.LART = "Utb i tj" THEN*/ DO:                  
                   IF franvarotemp.TIMMAR > 0 AND franvarotemp.TTId = 0 THEN DO:
                   /* Ingen ordinarie arbetstid men "Utb i tj"- skicka omfattning 0 och antal timmar*/
                      overrapp1 = franvarotemp.PNR +
                         ";" + pekod +  
                         ";0;" +
                         STRING(franvarotemp.FRAN,"99999999") + ";" +
                         STRING(franvarotemp.TILL,"99999999") + ";" +
                         "0;0;" +
                         franvarotemp.LART + ";" +                     
                         STRING(0,"9.999") + ";" +
                         "1;" +                                            
                         STRING(minut,"9999") + ";;" +
                         "0;0;" +
                         STRING(franvarotemp.ARBDAGAR) + ";" + 
                         "0;0;" +
                         "0;0;0;0;0;0;0;0;0;0;0;0;0;0;" +
                         "0;0;0;0;0;0;0;0;0;0;0;0;0;0;" +
                         "0;0;8;0;35;" +
                         "0;0;0;0;0;0;0;" +
                         "0;0;0;0;0;0;0;" +
                         SUBSTRING(franvarotemp.BPNR,1,10) + ";" +
                         STRING(franvarotemp.LATTHELG) + ";" +
                         "111".
                   END.
                   ELSE IF franvarotemp.TIMMAR > 0 AND omfatt > 1  THEN DO:               
                      /* Liten ordinarie arbetstid tex 22-24 , men "Utb i tj"- skicka omfattning 0 och antal timmar*/
                      overrapp1 = franvarotemp.PNR +
                         ";" + pekod + 
                         ";0;" +
                         STRING(franvarotemp.FRAN,"99999999") + ";" +
                         STRING(franvarotemp.TILL,"99999999") + ";" +
                         "0;0;" +
                         franvarotemp.LART + ";" +                     
                         STRING(0,"9.999") + ";" +
                         "1;" +                                            
                         STRING(minut,"9999") + ";;" +
                         "0;0;" +
                         STRING(franvarotemp.ARBDAGAR) + ";" + 
                         "0;0;" +
                         "0;0;0;0;0;0;0;0;0;0;0;0;0;0;" +
                         "0;0;0;0;0;0;0;0;0;0;0;0;0;0;" +
                         "0;0;8;0;35;" +
                         "0;0;0;0;0;0;0;" +
                         "0;0;0;0;0;0;0;" +
                         SUBSTRING(franvarotemp.BPNR,1,10) + ";" +
                         STRING(franvarotemp.LATTHELG) + ";" +
                         "111".
                   END.
               END.            
            END.
         END.
         ELSE DO:         
            overrapp1 = franvarotemp.PNR +
                     ";" + pekod +  
                     ";0;" +
                     STRING(franvarotemp.FRAN,"99999999") + ";" +
                     STRING(franvarotemp.TILL,"99999999") + ";" +
                     "0;0;" +
                     franvarotemp.LART + ";" +
                     STRING(1.000,"9.999") + ";" +
                     "1;" +
                     STRING(0,"99.99") + ";;0;0;" +
                     STRING(franvarotemp.ARBDAGAR) + ";" + 
                     "0;0;" +
                     "0;0;0;0;0;0;0;0;0;0;0;0;0;0;" +
                     "0;0;0;0;0;0;0;0;0;0;0;0;0;0;" +
                     "0;0;8;0;35;" +
                     "0;0;0;0;0;0;0;" +
                     "0;0;0;0;0;0;0;" +
                     SUBSTRING(franvarotemp.BPNR,1,10) + ";" +
                     STRING(franvarotemp.LATTHELG) + ";" +
                     "111".
         END.      
      END.     
      IF overrapp1 NE "" THEN DO:      
         IF  franvarotemp.LART = "SJUK" THEN DO:
            PUT STREAM sjuksmaland overrapp1 SKIP. 
         END.   
         PUT STREAM frsavsjo overrapp1 SKIP.
      END.
      DELETE franvarotemp.
      overrapp1 = "".
   END.
   PUT SKIP.
   OUTPUT STREAM sjuksmaland CLOSE.      
   OUTPUT STREAM frsavsjo CLOSE.
   /*Sävsjö Energi*/
   IF Guru.Konstanter:globforetag = "GKAL" THEN DO:      
      IF korvar = "" THEN DO:      
         prognamn3 = prognamn2 + "frkollSÄVSJÖENERGI.d".
         OUTPUT STREAM sjuksmaland TO VALUE(prognamn3) NO-ECHO.   
         prognamn3 = prognamn2 + "franSÄVSJÖENERGI.d".
         OUTPUT STREAM frsavsjo TO VALUE(prognamn3) NO-ECHO.        
      END.
      ELSE DO:
         prognamn3 = prognamn2 + "frkollomkSÄVSJÖENERGI.d".
         OUTPUT STREAM sjuksmaland TO VALUE(prognamn3) NO-ECHO.   
         prognamn3 = prognamn2 + "franomkSÄVSJÖENERGI.d".
         OUTPUT STREAM frsavsjo TO VALUE(prognamn3) NO-ECHO.         
      END.      
   END.       
   ELSE IF Guru.Konstanter:globforetag = "ELPA" THEN DO:
      IF korvar = "" THEN DO:      
         prognamn3 = prognamn2 + "frkollSÄVSJÖENERGI.d".
         OUTPUT STREAM sjuksmaland TO VALUE(prognamn3) NO-ECHO.   
         prognamn3 = prognamn2 + "franSÄVSJÖENERGI.d".
         OUTPUT STREAM frsavsjo TO VALUE(prognamn3) NO-ECHO.        
      END.
      ELSE DO:      
         prognamn3 = prognamn2 + "frkollomkSÄVSJÖENERGI.d".
         OUTPUT STREAM sjuksmaland TO VALUE(prognamn3) NO-ECHO.   
         prognamn3 = prognamn2 + "franomkSÄVSJÖENERGI.d".
         OUTPUT STREAM frsavsjo TO VALUE(prognamn3) NO-ECHO.         
      END.      
   END.            
   radrakn = 1.
   FOR EACH franvarotemp WHERE franvarotemp.VIJUDID = "SEAB" :
      radrakn = radrakn + 1.
   END.
   overrapp1 = "7048;" + STRING(radrakn) + ";0;" + string(TODAY,"99999999") + ";" + STRING(TIME,"HH:MM").             
   PUT STREAM frsavsjo overrapp1 SKIP.
   PUT SKIP.
   FOR EACH franvarotemp WHERE franvarotemp.VIJUDID = "SEAB" USE-INDEX KALMAR:
      /*IF franvarotemp.PNR = "7802110176"  THEN pekod = "23120100".
      ELSE pekod = "0".*/
      ASSIGN
      pekod = "0"
      omfatt = 0
      minut = 0.
      IF franvarotemp.TIMMAR > 0 AND franvarotemp.TTID > 0 THEN DO:      
         omfatt = franvarotemp.TIMMAR / franvarotemp.TTID.
         IF omfatt > 1 THEN omfatt = 1.
      END.
      IF franvarotemp.TIMMAR > 0 THEN DO:
         minut = franvarotemp.TIMMAR * 60.         
      END.
      IF franvarotemp.ORSAK = "SJUK" THEN DO:
         IF franvarotemp.TIMANSTALLD = FALSE THEN DO:         
            IF franvarotemp.TIMMAR > 0 THEN DO:
               /*del av dag - bara omfattning om det är sjuk annars 0.000*/               
                /*bara omfattning , inga timmar.*/
               overrapp1 = franvarotemp.PNR +
                        ";" + pekod + 
                        ";0;" +
                        STRING(franvarotemp.FRAN,"99999999") + ";" +
                        STRING(franvarotemp.TILL,"99999999") + ";" +
                        "0;0;" +
                        franvarotemp.LART + ";" +
                        STRING(omfatt,"9.999") + ";" +
                        "1;" +
                        "00.00;;" +
                        "0;0;" +
                        "0;" + 
                        "0;0;" +
                        STRING(franvarotemp.SJUK[1],"9") + ";" +
                        STRING(franvarotemp.SJUK[2],"9") + ";" +
                        STRING(franvarotemp.SJUK[3],"9") + ";" +
                        STRING(franvarotemp.SJUK[4],"9") + ";" +
                        STRING(franvarotemp.SJUK[5],"9") + ";" +
                        STRING(franvarotemp.SJUK[6],"9") + ";" +
                        STRING(franvarotemp.SJUK[7],"9") + ";" +
                        STRING(franvarotemp.SJUK[8],"9") + ";" +
                        STRING(franvarotemp.SJUK[9],"9") + ";" +
                        STRING(franvarotemp.SJUK[10],"9") + ";" +
                        STRING(franvarotemp.SJUK[11],"9") + ";" +
                        STRING(franvarotemp.SJUK[12],"9") + ";" +
                        STRING(franvarotemp.SJUK[13],"9") + ";" +
                        STRING(franvarotemp.SJUK[14],"9") + ";" +
                        "0;0;0;0;0;0;0;0;0;0;0;0;0;0;" +
                        "0;0;8;0;35;" +
                        STRING(franvarotemp.SJUK[15],"9") + ";" +
                        STRING(franvarotemp.SJUK[16],"9") + ";" +
                        STRING(franvarotemp.SJUK[17],"9") + ";" +
                        STRING(franvarotemp.SJUK[18],"9") + ";" +
                        STRING(franvarotemp.SJUK[19],"9") + ";" +
                        STRING(franvarotemp.SJUK[20],"9") + ";" +
                        STRING(franvarotemp.SJUK[21],"9") + ";" +                                             
                        "0;0;0;0;0;0;0;" +
                        SUBSTRING(franvarotemp.BPNR,1,10) + ";" +
                        STRING(franvarotemp.LATTHELG) + ";" +
                        "111".
            END.
            ELSE DO:
               overrapp1 = franvarotemp.PNR +
                        ";" + pekod + 
                        ";0;" +
                        STRING(franvarotemp.FRAN,"99999999") + ";" +
                        STRING(franvarotemp.TILL,"99999999") + ";" +
                        "0;0;" +
                        franvarotemp.LART + ";" +
                        STRING (1.000,"9.999") + ";" +                      
                        "1;" +
                        STRING(0,"99.99") + ";;" +
                        "0;0;" +
                        "0;" + 
                        "0;0;" +
                        STRING(franvarotemp.SJUK[1],"9") + ";" +
                        STRING(franvarotemp.SJUK[2],"9") + ";" +
                        STRING(franvarotemp.SJUK[3],"9") + ";" +
                        STRING(franvarotemp.SJUK[4],"9") + ";" +
                        STRING(franvarotemp.SJUK[5],"9") + ";" +
                        STRING(franvarotemp.SJUK[6],"9") + ";" +
                        STRING(franvarotemp.SJUK[7],"9") + ";" +
                        STRING(franvarotemp.SJUK[8],"9") + ";" +
                        STRING(franvarotemp.SJUK[9],"9") + ";" +
                        STRING(franvarotemp.SJUK[10],"9") + ";" +
                        STRING(franvarotemp.SJUK[11],"9") + ";" +
                        STRING(franvarotemp.SJUK[12],"9") + ";" +
                        STRING(franvarotemp.SJUK[13],"9") + ";" +
                        STRING(franvarotemp.SJUK[14],"9") + ";" +
                        "0;0;0;0;0;0;0;0;0;0;0;0;0;0;" +
                        "0;0;8;0;35;" +
                        STRING(franvarotemp.SJUK[15],"9") + ";" +
                        STRING(franvarotemp.SJUK[16],"9") + ";" +
                        STRING(franvarotemp.SJUK[17],"9") + ";" +
                        STRING(franvarotemp.SJUK[18],"9") + ";" +
                        STRING(franvarotemp.SJUK[19],"9") + ";" +
                        STRING(franvarotemp.SJUK[20],"9") + ";" +
                        STRING(franvarotemp.SJUK[21],"9") + ";" +                                             
                        "0;0;0;0;0;0;0;" +
                        SUBSTRING(franvarotemp.BPNR,1,10) + ";" +
                        STRING(franvarotemp.LATTHELG) + ";" +
                        "111".
            END.
         END.
         ELSE DO:         
            /*timanställda*/
            IF franvarotemp.TIMMAR > 0 THEN DO:
               overrapp1 = franvarotemp.PNR +
                        ";" + pekod + 
                        ";0;" +
                        STRING(franvarotemp.FRAN,"99999999") + ";" +
                        STRING(franvarotemp.TILL,"99999999") + ";" +
                        "0;0;" +
                        franvarotemp.LART + ";" +
                        "0;1;0;;" +                        
                        "0;0;" +
                        "0;" + 
                        "0;0;" +
                        "0;0;0;0;0;0;0;0;0;0;0;0;0;0;" +
                        STRING(franvarotemp.SJTIM[1],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[2],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[3],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[4],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[5],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[6],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[7],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[8],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[9],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[10],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[11],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[12],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[13],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[14],"9.99") + ";" +                        
                        STRING(franvarotemp.FTIM,"9.99") + ";" +                         
                        "0;8;0;35;" +
                        "0;0;0;0;0;0;0;" +
                        STRING(franvarotemp.SJTIM[15],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[16],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[17],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[18],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[19],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[20],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[21],"9.99") + ";" +                                                                     
                        SUBSTRING(franvarotemp.BPNR,1,10) + ";" +
                        STRING(franvarotemp.LATTHELG) + ";" +
                        "111".
            END.
            ELSE DO:
               overrapp1 = franvarotemp.PNR +
                        ";" + pekod +   
                        ";0;" +
                        STRING(franvarotemp.FRAN,"99999999") + ";" +
                        STRING(franvarotemp.TILL,"99999999") + ";" +
                        "0;0;" +
                        franvarotemp.LART + ";" +
                        "0;1;0;;" +
                        "0;0;" +
                        "0;" + 
                        "0;0;" +
                        "0;0;0;0;0;0;0;0;0;0;0;0;0;0;" +
                        STRING(franvarotemp.SJTIM[1],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[2],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[3],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[4],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[5],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[6],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[7],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[8],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[9],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[10],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[11],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[12],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[13],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[14],"9.99") + ";" +                                                
                        STRING(franvarotemp.FTIM,"9.99") + ";" +                         
                        "0;8;0;35;" +
                        "0;0;0;0;0;0;0;" +
                        STRING(franvarotemp.SJTIM[15],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[16],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[17],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[18],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[19],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[20],"9.99") + ";" +
                        STRING(franvarotemp.SJTIM[21],"9.99") + ";" +                                                                                             
                        SUBSTRING(franvarotemp.BPNR,1,10) + ";" +
                        STRING(franvarotemp.LATTHELG) + ";" +
                        "111".
            END.
         END.
      END.
      ELSE DO:
         IF franvarotemp.TIMMAR > 0 THEN DO:
            IF franvarotemp.LART = "Komp" OR franvarotemp.LART = "Atidkont" OR franvarotemp.LART = "LFKALM1" OR franvarotemp.LART = "Utb i tj" 
            OR franvarotemp.LART = "Led6juni" OR franvarotemp.LART = "Tjl ulön" THEN DO:
                /*skall anges i timmar -ej omfattning. Om det gäller 1 dag används fältet franvarotemp.TIMMAR (antal timmar)
                  Gäller det flera dagar används fältet franvarotemp.TOTID (Totalt antal timmar under frånvaroperioden)*/
                IF franvarotemp.FRAN = franvarotemp.TILL THEN DO:               
                   IF franvarotemp.LART = "Tjl ulön" THEN franvarotemp.LART = "Tjl tim".
                    overrapp1 = franvarotemp.PNR +
                         ";" + pekod +  
                         ";0;" +
                         STRING(franvarotemp.FRAN,"99999999") + ";" +
                         STRING(franvarotemp.TILL,"99999999") + ";" +
                         "0;0;" +
                         franvarotemp.LART + ";" +          
                         STRING (0.000,"9.999") + ";" +                          
                         "1;" +                         
                         STRING((minut),"9999") + ";;" +
                         "0;0;" +
                         STRING(franvarotemp.ARBDAGAR) + ";" + 
                         "0;0;" +
                         "0;0;0;0;0;0;0;0;0;0;0;0;0;0;" +
                         "0;0;0;0;0;0;0;0;0;0;0;0;0;0;" +                         
                         "0;0;8;0;35;" +
                         "0;0;0;0;0;0;0;" +
                         "0;0;0;0;0;0;0;" +
                         SUBSTRING(franvarotemp.BPNR,1,10) + ";" +
                         STRING(franvarotemp.LATTHELG) + ";" +
                         "111".
                END.
                ELSE DO:
                   overrapp1 = franvarotemp.PNR +
                         ";" + pekod + 
                         ";0;" +
                         STRING(franvarotemp.FRAN,"99999999") + ";" +
                         STRING(franvarotemp.TILL,"99999999") + ";" +
                         "0;0;" +
                         franvarotemp.LART + ";" +          
                         STRING (0.000,"9.999") + ";" +                          
                         "1;" +                         
                         "0;0;" +
                         STRING(franvarotemp.ARBDAGAR) + ";" + 
                         "0;0;" +
                         "0;0;0;0;0;0;0;0;0;0;0;0;0;0;" +
                         "0;0;0;0;0;0;0;0;0;0;0;0;0;0;" +
                         STRING((klock100(franvarotemp.TOTTID)),"999.99") + ";" +
                         "0;8;0;35;" +
                         "0;0;0;0;0;0;0;" +
                         "0;0;0;0;0;0;0;" +
                         SUBSTRING(franvarotemp.BPNR,1,10) + ";" +
                         STRING(franvarotemp.LATTHELG) + ";" +
                         "111".
                END.
            END.
            ELSE DO:         
               /*bara omfattning ska ut - inte timmar*/
               IF omfatt > 0 AND omfatt LE 1  THEN DO:               
                   overrapp1 = franvarotemp.PNR +
                         ";" + pekod + 
                         ";0;" +
                         STRING(franvarotemp.FRAN,"99999999") + ";" +
                         STRING(franvarotemp.TILL,"99999999") + ";" +
                         "0;0;" +
                         franvarotemp.LART + ";" +                     
                         STRING(omfatt,"9.999") + ";" +
                         "1;" +                   
                         STRING(0,"99.99") + ";;" +
                         "0;0;" +
                         STRING(franvarotemp.ARBDAGAR) + ";" + 
                         "0;0;" +
                         "0;0;0;0;0;0;0;0;0;0;0;0;0;0;" +
                         "0;0;0;0;0;0;0;0;0;0;0;0;0;0;" +
                         "0;0;8;0;35;" +
                         "0;0;0;0;0;0;0;" +
                         "0;0;0;0;0;0;0;" +
                         SUBSTRING(franvarotemp.BPNR,1,10) + ";" +
                         STRING(franvarotemp.LATTHELG) + ";" +
                         "111".
               END.
               ELSE /*IF franvarotemp.LART = "Utb i tj" THEN*/ DO:                  
                   IF franvarotemp.TIMMAR > 0 AND franvarotemp.TTId = 0 THEN DO:
                   /* Ingen ordinarie arbetstid men "Utb i tj"- skicka omfattning 0 och antal timmar*/
                      overrapp1 = franvarotemp.PNR +
                         ";" + pekod +  
                         ";0;" +
                         STRING(franvarotemp.FRAN,"99999999") + ";" +
                         STRING(franvarotemp.TILL,"99999999") + ";" +
                         "0;0;" +
                         franvarotemp.LART + ";" +                     
                         STRING(0,"9.999") + ";" +
                         "1;" +                                            
                         STRING(minut,"9999") + ";;" +
                         "0;0;" +
                         STRING(franvarotemp.ARBDAGAR) + ";" + 
                         "0;0;" +
                         "0;0;0;0;0;0;0;0;0;0;0;0;0;0;" +
                         "0;0;0;0;0;0;0;0;0;0;0;0;0;0;" +
                         "0;0;8;0;35;" +
                         "0;0;0;0;0;0;0;" +
                         "0;0;0;0;0;0;0;" +
                         SUBSTRING(franvarotemp.BPNR,1,10) + ";" +
                         STRING(franvarotemp.LATTHELG) + ";" +
                         "111".
                   END.
                   ELSE IF franvarotemp.TIMMAR > 0 AND omfatt > 1  THEN DO:               
                      /* Liten ordinarie arbetstid tex 22-24 , men "Utb i tj"- skicka omfattning 0 och antal timmar*/
                      overrapp1 = franvarotemp.PNR +
                         ";" + pekod + 
                         ";0;" +
                         STRING(franvarotemp.FRAN,"99999999") + ";" +
                         STRING(franvarotemp.TILL,"99999999") + ";" +
                         "0;0;" +
                         franvarotemp.LART + ";" +                     
                         STRING(0,"9.999") + ";" +
                         "1;" +                                            
                         STRING(minut,"9999") + ";;" +
                         "0;0;" +
                         STRING(franvarotemp.ARBDAGAR) + ";" + 
                         "0;0;" +
                         "0;0;0;0;0;0;0;0;0;0;0;0;0;0;" +
                         "0;0;0;0;0;0;0;0;0;0;0;0;0;0;" +
                         "0;0;8;0;35;" +
                         "0;0;0;0;0;0;0;" +
                         "0;0;0;0;0;0;0;" +
                         SUBSTRING(franvarotemp.BPNR,1,10) + ";" +
                         STRING(franvarotemp.LATTHELG) + ";" +
                         "111".
                   END.
               END.            
            END.
         END.
         ELSE DO:         
            overrapp1 = franvarotemp.PNR +
                     ";" + pekod +  
                     ";0;" +
                     STRING(franvarotemp.FRAN,"99999999") + ";" +
                     STRING(franvarotemp.TILL,"99999999") + ";" +
                     "0;0;" +
                     franvarotemp.LART + ";" +
                     STRING(1.000,"9.999") + ";" +
                     "1;" +
                     STRING(0,"99.99") + ";;0;0;" +
                     STRING(franvarotemp.ARBDAGAR) + ";" + 
                     "0;0;" +
                     "0;0;0;0;0;0;0;0;0;0;0;0;0;0;" +
                     "0;0;0;0;0;0;0;0;0;0;0;0;0;0;" +
                     "0;0;8;0;35;" +
                     "0;0;0;0;0;0;0;" +
                     "0;0;0;0;0;0;0;" +
                     SUBSTRING(franvarotemp.BPNR,1,10) + ";" +
                     STRING(franvarotemp.LATTHELG) + ";" +
                     "111".
         END.
      END.     
      IF overrapp1 NE "" THEN DO:      
         IF  franvarotemp.LART = "SJUK" THEN DO:
            PUT STREAM sjuksmaland overrapp1 SKIP. 
         END.   
         PUT STREAM frsavsjo overrapp1 SKIP.
      END.

      DELETE franvarotemp.
      overrapp1 = "".
   END.
   PUT SKIP.
   OUTPUT STREAM sjuksmaland CLOSE.      
   OUTPUT STREAM frsavsjo CLOSE.
END.
IF korvar = "" THEN DO:
   prognamn3 = prognamn2 + "fransu.d".
   OUTPUT TO VALUE(prognamn3) NO-ECHO.
END.
ELSE DO:
   prognamn3 = prognamn2 + "fransuomk.d".
   OUTPUT TO VALUE(prognamn3) NO-ECHO.
END.
FOR EACH franvarotemp USE-INDEX franvaro  NO-LOCK:
  EXPORT franvarotemp.
END.
OUTPUT CLOSE.
IF korvar = "" THEN DO:
   prognamn3 = prognamn2 + "felpers.d".
   OUTPUT TO VALUE(prognamn3) APPEND.
END.
ELSE DO:
   prognamn3 = prognamn2 + "felpersomk.d".
   OUTPUT TO VALUE(prognamn3) APPEND.   
END.
FOR EACH franvarotemp USE-INDEX franvaro  NO-LOCK:
  EXPORT franvarotemp.
END.
IF korvar = "" THEN DO:
   IF Guru.Konstanter:globforetag = "GKAL" THEN DO:      
      lonfil =  "franELNÄT" + STRING(TODAY,"99999999") + ".txt".
      prognamn = prognamn2  + lonfil.        
      prognamn3 = prognamn2 + "franELNÄT.d".      
      OS-COPY VALUE(prognamn3) VALUE(prognamn). 
      
       ASSIGN   
        ftpanv =  CHR(103) + CHR(101) + CHR(112) 
        ftplord = CHR(118) + CHR(115) + CHR(102) + CHR(49) + CHR(55) + CHR(103) + CHR(103) + CHR(114) . 
        RUN FTPFILE.P (INPUT ftpanv, 
                        INPUT ftplord, INPUT TRUE, INPUT 1,
                   INPUT prognamn, INPUT "/InRespons/" + lonfil,
                   INPUT "192.168.69.34", OUTPUT TABLE felmeddtemp).     

      lonfil =  "franFÖRSÄLJNING" + STRING(TODAY,"99999999") + ".txt".
      prognamn = prognamn2  + lonfil.        
      prognamn3 = prognamn2 + "franFÖRSÄLJNING.d".      
      OS-COPY VALUE(prognamn3) VALUE(prognamn). 
      
      ASSIGN   
        ftpanv =  CHR(103) + CHR(101) + CHR(112) 
        ftplord = CHR(118) + CHR(115) + CHR(102) + CHR(49) + CHR(55) + CHR(103) + CHR(103) + CHR(114) . 
        RUN FTPFILE.P (INPUT ftpanv, 
                        INPUT ftplord, INPUT TRUE, INPUT 1,
                   INPUT prognamn, INPUT "/InRespons/" + lonfil,
                   INPUT "192.168.69.34", OUTPUT TABLE felmeddtemp).     
      lonfil =  "franSÄVSJÖ" + STRING(TODAY,"99999999") + ".txt".
      prognamn = prognamn2  + lonfil.        
      prognamn3 = prognamn2 + "franSÄVSJÖ.d".      
      OS-COPY VALUE(prognamn3) VALUE(prognamn). 
      
       ASSIGN   
        ftpanv =  CHR(103) + CHR(101) + CHR(112) 
        ftplord = CHR(118) + CHR(115) + CHR(102) + CHR(49) + CHR(55) + CHR(103) + CHR(103) + CHR(114) . 
        RUN FTPFILE.P (INPUT ftpanv, 
                        INPUT ftplord, INPUT TRUE, INPUT 1,
                   INPUT prognamn, INPUT "/InRespons/" + lonfil,
                   INPUT "192.168.69.34", OUTPUT TABLE felmeddtemp).     
      lonfil =  "franVÄRME" + STRING(TODAY,"99999999") + ".txt".
      prognamn = prognamn2  + lonfil.        
      prognamn3 = prognamn2 + "franVÄRME.d".      
      OS-COPY VALUE(prognamn3) VALUE(prognamn). 
      
       ASSIGN   
        ftpanv =  CHR(103) + CHR(101) + CHR(112) 
        ftplord = CHR(118) + CHR(115) + CHR(102) + CHR(49) + CHR(55) + CHR(103) + CHR(103) + CHR(114) . 
        RUN FTPFILE.P (INPUT ftpanv, 
                        INPUT ftplord, INPUT TRUE, INPUT 1,
                   INPUT prognamn, INPUT "/InRespons/" + lonfil,
                   INPUT "192.168.69.34", OUTPUT TABLE felmeddtemp).     
      lonfil =  "franSÄVSJÖENERGI" + STRING(TODAY,"99999999") + ".txt".
      prognamn = prognamn2  + lonfil.        
      prognamn3 = prognamn2 + "franSÄVSJÖENERGI.d".      
      OS-COPY VALUE(prognamn3) VALUE(prognamn). 
      
       ASSIGN   
        ftpanv =  CHR(103) + CHR(101) + CHR(112) 
        ftplord = CHR(118) + CHR(115) + CHR(102) + CHR(49) + CHR(55) + CHR(103) + CHR(103) + CHR(114) . 
        RUN FTPFILE.P (INPUT ftpanv, 
                        INPUT ftplord, INPUT TRUE, INPUT 1,
                   INPUT prognamn, INPUT "/InRespons/" + lonfil,
                   INPUT "192.168.69.34", OUTPUT TABLE felmeddtemp).     
   END.     
   ELSE DO:
      prognamn = prognamn2 + "franELNÄT" + STRING(TODAY,"99999999") + ".d".        
      prognamn3 = prognamn2 + "franELNÄT.d".      
      OS-COPY VALUE(prognamn3) VALUE(prognamn).               
      prognamn = prognamn2 + "franFÖRSÄLJNING" + STRING(TODAY,"99999999") + ".d".        
      prognamn3 = prognamn2 + "franFÖRSÄLJNING.d".      
      OS-COPY VALUE(prognamn3) VALUE(prognamn).               
      prognamn = prognamn2 + "franSÄVSJÖ" + STRING(TODAY,"99999999") + ".d".        
      prognamn3 = prognamn2 + "franSÄVSJÖ.d".      
      OS-COPY VALUE(prognamn3) VALUE(prognamn).               
      prognamn = prognamn2 + "franVÄRME" + STRING(TODAY,"99999999") + ".d".        
      prognamn3 = prognamn2 + "franVÄRME.d".      
      OS-COPY VALUE(prognamn3) VALUE(prognamn).    
      prognamn = prognamn2 + "franSÄVSJÖENERGI" + STRING(TODAY,"99999999") + ".d".        
      prognamn3 = prognamn2 + "franSÄVSJÖENERGI.d".      
      OS-COPY VALUE(prognamn3) VALUE(prognamn).    
   END.
END.
ELSE DO:
   IF Guru.Konstanter:globforetag = "GKAL" THEN DO:
      prognamn = prognamn2 + "franomkELNÄT" + STRING(TODAY,"99999999") + ".d".        
      prognamn3 = prognamn2 + "franomkELNÄT.d".      
      OS-COPY VALUE(prognamn3) VALUE(prognamn).         
      prognamn = prognamn2 + "franomkFÖRSÄLJNING" + STRING(TODAY,"99999999") + ".d".        
      prognamn3 = prognamn2 + "franomkFÖRSÄLJNING.d".      
      OS-COPY VALUE(prognamn3) VALUE(prognamn).         
      prognamn = prognamn2 + "franomkSÄVSJÖ" + STRING(TODAY,"99999999") + ".d".        
      prognamn3 = prognamn2 + "franomkSÄVSJÖ.d".      
      OS-COPY VALUE(prognamn3) VALUE(prognamn).         
      prognamn = prognamn2 + "franomkVÄRME" + STRING(TODAY,"99999999") + ".d".        
      prognamn3 = prognamn2 + "franomkVÄRME.d".      
      OS-COPY VALUE(prognamn3) VALUE(prognamn).         
      prognamn = prognamn2 + "franomkSÄVSJÖENERGI" + STRING(TODAY,"99999999") + ".d".        
      prognamn3 = prognamn2 + "franomkSÄVSJÖENERGI.d".      
      OS-COPY VALUE(prognamn3) VALUE(prognamn).         
   END.     
   ELSE DO:
      prognamn = prognamn2 + "franomkELNÄT" + STRING(TODAY,"99999999") + ".d".        
      prognamn3 = prognamn2 + "franomkELNÄT.d".      
      OS-COPY VALUE(prognamn3) VALUE(prognamn).               
      prognamn = prognamn2 + "franomkFÖRSÄLJNING" + STRING(TODAY,"99999999") + ".d".        
      prognamn3 = prognamn2 + "franomkFÖRSÄLJNING.d".      
      OS-COPY VALUE(prognamn3) VALUE(prognamn).               
      prognamn = prognamn2 + "franomkSÄVSJÖ" + STRING(TODAY,"99999999") + ".d".        
      prognamn3 = prognamn2 + "franomkSÄVSJÖ.d".      
      OS-COPY VALUE(prognamn3) VALUE(prognamn).      
      prognamn = prognamn2 + "franomkVÄRME" + STRING(TODAY,"99999999") + ".d".        
      prognamn3 = prognamn2 + "franomkVÄRME.d".      
      OS-COPY VALUE(prognamn3) VALUE(prognamn).     
      prognamn = prognamn2 + "franomkSÄVSJÖENERGI" + STRING(TODAY,"99999999") + ".d".        
      prognamn3 = prognamn2 + "franomkSÄVSJÖENERGI.d".      
      OS-COPY VALUE(prognamn3) VALUE(prognamn).      
   END.
END.
OUTPUT CLOSE.
RUN sammut_UI (INPUT 2).
{EUROPEANAMERICAN.I}
PROCEDURE skift_UI.   
   /*SKIFT dela upp i skiftdygnet*/
   FOR EACH franvarotemp WHERE franvarotemp.SLTID = 24 AND franvarotemp.FRAN = franvarotemp.TILL BY franvarotemp.PERSONALKOD BY franvarotemp.FRAN:
      IF franvarotemp.LART = "Komp" OR franvarotemp.LART = "Atidkont" OR franvarotemp.LART = "LFKALM1" OR franvarotemp.LART = "Utb i tj" 
      OR franvarotemp.LART = "Led6juni"  THEN.      
      ELSE DO:         
         FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = franvarotemp.PERSONALKOD USE-INDEX PERSONALKOD NO-LOCK.       
         persrec = RECID(PERSONALTAB).       
         regdatum = franvarotemp.FRAN. 
         RUN REGVEC.P.
         RUN SLUTARB.P.      
         ASSIGN
         stregtid = klock100(franvarotemp.SLTID) - klock100(franvarotemp.STTID).      
         IF regstart = 0 THEN stttid = klock100(regslut) - klock100(lunchslutet).
         ELSE stttid = klock100(regslut) - klock100(regstart).
         regdatum = regdatum + 1.
         RUN REGVEC.P.
         RUN SLUTARB.P.
         IF regslut = 24 THEN slttid = klock100(lunchstarten) - klock100(regstart).
         ELSE slttid = klock100(regslut) - klock100(regstart).                          
         FIND FIRST fbuff WHERE fbuff.PERSONALKOD = franvarotemp.PERSONALKOD AND fbuff.FRAN = franvarotemp.TILL + 1
         /*AND fbuff.FRAN = fbuff.TILL*/ AND fbuff.LART = franvarotemp.LART AND fbuff.STTID = 00 NO-ERROR.
         IF AVAILABLE fbuff THEN DO:
            ASSIGN
            slregtid = klock100(fbuff.SLTID) - klock100(fbuff.STTID).                
            koll1 = FALSE.
            IF stregtid = stttid AND slregtid = slttid THEN koll1 = TRUE.
            IF stregtid = stttid AND fbuff.FRAN NE fbuff.TILL THEN koll1 = TRUE.
            IF koll1 = TRUE THEN DO:
               FIND FIRST fbuff2 WHERE fbuff2.PERSONALKOD = franvarotemp.PERSONALKOD AND fbuff2.TILL = franvarotemp.FRAN - 1
               AND fbuff2.LART = franvarotemp.LART  NO-ERROR.            
               IF stttid > slttid THEN DO:
                  ASSIGN franvarotemp.PROCENT = 100 franvarotemp.TIMMAR = stregtid + slregtid 
                  franvarotemp.TTID = franvarotemp.TIMMAR franvarotemp.SLTID = fbuff.SLTID.               
                  DELETE fbuff.
               END.
               ELSE DO:
                  FIND FIRST fbuff3 WHERE fbuff3.PERSONALKOD = franvarotemp.PERSONALKOD AND
                  fbuff3.FRAN = franvarotemp.FRAN AND fbuff3.LART = franvarotemp.LART AND RECID(fbuff3) NE RECID(franvarotemp)  NO-ERROR.
                  ASSIGN fbuff.PROCENT = 100 fbuff.TIMMAR = stregtid + slregtid 
                  fbuff.STTID = franvarotemp.STTID fbuff.TTID = fbuff.TIMMAR.
                  IF AVAILABLE fbuff2 THEN DO:                                    
                     IF NOT AVAILABLE fbuff3 THEN DO:
                        ASSIGN fbuff.FRAN = franvarotemp.FRAN.
                     END.                  
                     /*ELSE DO:                     
                        IF fbuff3.STTID = 0 THEN ASSIGN fbuff3.PROCENT = 100.                     
                     END.*/
                  END.
                  IF NOT AVAILABLE fbuff3 THEN DO:
                     DELETE franvarotemp.   
                  END.
               END.
            END.
            ELSE DO:
               IF franvarotemp.FRAN = franvarotemp.TILL THEN DO: 
                  ASSIGN franvarotemp.PROCENT = stregtid / (stttid + slttid)            
                  franvarotemp.TIMMAR = stregtid 
                  franvarotemp.TTID = stttid + slttid.
               END.
               IF fbuff.FRAN = fbuff.TILL THEN DO: 
                  ASSIGN fbuff.PROCENT = slregtid / (stttid + slttid)
                  fbuff.TIMMAR = slregtid 
                  fbuff.TTID = stttid + slttid.
               END.               
            END.
         END.
         ELSE DO:
            IF franvarotemp.FRAN = franvarotemp.TILL THEN DO: 
               ASSIGN franvarotemp.PROCENT = stregtid / (stttid + slttid)
               franvarotemp.TIMMAR = stregtid 
               franvarotemp.TTID = stttid + slttid.  
            END.
         END.
      END.   
   END.
   FOR EACH franvarotemp WHERE franvarotemp.FRAN = franvarotemp.TILL AND franvarotemp.TTID NE franvarotemp.TIMMAR :   
      /*Om uppdelning på flera registreringar samma dag , slå ihop till 1 registrering 20070423*/
      FIND FIRST fbuff WHERE fbuff.PERSONALKOD = franvarotemp.PERSONALKOD AND fbuff.TILL = franvarotemp.TILL
      AND fbuff.FRAN = franvarotemp.FRAN AND fbuff.LART = franvarotemp.LART AND RECID(fbuff) NE RECID(franvarotemp) NO-ERROR.
      IF AVAILABLE fbuff THEN DO:
         IF franvarotemp.PROCENT = 100  THEN DO:
            DELETE fbuff.   
         END.
         ELSE IF fbuff.PROCENT = 100 THEN DO:
            DELETE franvarotemp.
         END.
         ELSE DO:                      
            /* Om flera registreringar utspritt på dagen -slå ej ihop 20090114*/
            ejhop = TRUE.
            IF franvarotemp.STTID = 0 AND franvarotemp.SLTID = 0   THEN ejhop = FALSE.
            ELSE IF franvarotemp.SLTID = fbuff.STTID  THEN ejhop = FALSE.
            ELSE IF fbuff.SLTID = franvarotemp.STTID  THEN ejhop = FALSE.
            IF ejhop = FALSE THEN DO:            
                franvarotemp.TIMMAR = franvarotemp.TIMMAR + fbuff.TIMMAR.   
                franvarotemp.PROCENT = franvarotemp.PROCENT + fbuff.PROCENT.         
                DELETE fbuff.
            END.
         END.
      END.
   END.
   FOR EACH franvarotemp WHERE franvarotemp.FRAN NE franvarotemp.TILL :   
      /*Ta bort dubbelregistrering startdag*/
      FIND FIRST fbuff WHERE fbuff.PERSONALKOD = franvarotemp.PERSONALKOD AND fbuff.FRAN = franvarotemp.FRAN AND
      fbuff.LART = franvarotemp.LART AND RECID(fbuff) NE RECID(franvarotemp) NO-ERROR.
      IF AVAILABLE fbuff THEN DO:      
         DELETE fbuff.
      END.
   END.
END PROCEDURE.
