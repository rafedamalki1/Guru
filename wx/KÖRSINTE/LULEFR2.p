/*G:\DELAD\PRO9\GURU\LULEFR2.P SKAPAR PA90FIL FRANVARO.*/  
/*DEFINE STREAM frkalmar. */
/*DEFINE STREAM frsmaland. */
/*DEFINE STREAM sjukkalmar. */
/*DEFINE STREAM sjuksmaland.*/
DEFINE STREAM frlule. 
DEFINE STREAM frlulekop. 
DEFINE STREAM sjuklule. 
DEFINE STREAM stanslule. 



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

DEFINE VARIABLE overrapp1 AS CHARACTER FORMAT "X(184)" NO-UNDO.      /*LON*/
DEFINE VARIABLE overrapp2 AS CHARACTER FORMAT "X(184)" NO-UNDO.      /*LON*/
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
DEFINE VARIABLE prognamn5 AS CHARACTER  NO-UNDO.
DEFINE VARIABLE prognamn6 AS CHARACTER  NO-UNDO.
DEFINE VARIABLE prognamn3 AS CHARACTER  NO-UNDO.
DEFINE VARIABLE prognamn4 AS CHARACTER  NO-UNDO.
DEFINE VARIABLE anstperson LIKE PERSONALTAB.PERSONNUMMER NO-UNDO.
DEFINE VARIABLE nydag AS LOGICAL NO-UNDO.
DEFINE VARIABLE omfatt AS DECIMAL NO-UNDO.
DEFINE VARIABLE minut AS INTEGER NO-UNDO.
DEFINE VARIABLE frantim AS INTEGER NO-UNDO.
DEFINE VARIABLE startm AS LOGICAL NO-UNDO.
DEFINE VARIABLE slutm AS LOGICAL NO-UNDO.
DEFINE VARIABLE startdatum AS DATE NO-UNDO.
DEFINE VARIABLE slutdatum AS DATE NO-UNDO.
DEFINE VARIABLE rgr AS CHARACTER NO-UNDO.
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
   FIELD TTID AS DECIMAL     /*regtotalt -dagens arbetstid*/
   FIELD TOTTID AS DECIMAL   /*periodens totala timmar*/
   INDEX FRANVARO IS PRIMARY PNR FRAN LART ASCENDING
   INDEX VIJUDID VIJUDID PNR FRAN LART ASCENDING.
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
{AMERICANEUROPEAN.I}  
IF Guru.Konstanter:globforetag = "LULE" THEN DO:
   prognamn5 = "D:\elpool\DELAD\PRO9s\EXPORT\LON\Lonback\".
   prognamn6 = "D:\elpool\DELAD\PRO9s\EXPORT\LON\".
END.
ELSE IF Guru.Konstanter:globforetag = "ELPA" THEN prognamn5 = "\\pc112\delad\pro9s\korning\".

IF korvar = "" THEN DO:
   prognamn3 = prognamn5 + "fransu.d".
   INPUT FROM VALUE(prognamn3) NO-ECHO.
END.
ELSE DO:
   prognamn3 = prognamn5 + "fransuomk.d".
   INPUT FROM VALUE(prognamn3) NO-ECHO.   
END.
REPEAT TRANSACTION:
  CREATE franvarotemp.
  ASSIGN.
  IMPORT franvarotemp.
  /*IF franvarotemp.LART = "530" THEN ASSIGN franvarotemp.ORSAK = "SJUK".
  IF franvarotemp.LART = "SJUK" THEN ASSIGN franvarotemp.ORSAK = "SJUK".*/
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
   IF PERSONALTAB.BEFATTNING = "TIMANSTÄLLD" THEN DO:
      franvarotemp.TIMANSTALLD = TRUE.
   END.
END.


IF korvar = "" THEN DO:
   prognamn3 = prognamn5 + "felman.d".
   OUTPUT TO VALUE(prognamn3) NO-ECHO.
END.
ELSE DO:
   prognamn3 = prognamn5 + "felmanomk.d ".
   OUTPUT TO VALUE(prognamn3) NO-ECHO.
END.

kolldatum = DATE(MONTH(vkdatum),01,YEAR(vkdatum)).
FOR EACH franvarotemp WHERE franvarotemp.FRAN < kolldatum USE-INDEX franvaro  NO-LOCK:   
  EXPORT franvarotemp.
END.
OUTPUT CLOSE.

  



DO TRANSACTION:   
  

   /*Luleå Energi ELNÄT AB*/
   prognamn3 = prognamn5 + "stanstid.d".
   OUTPUT STREAM stanslule TO VALUE(prognamn3) APPEND.   
   
   IF Guru.Konstanter:globforetag = "LULE" THEN DO:      
      IF korvar = "" THEN DO:      
         prognamn3 = prognamn5 + "frkollLULE.d".
         OUTPUT STREAM sjuklule TO VALUE(prognamn3) NO-ECHO.   
         prognamn3 = prognamn5 + "LULEELNAT.d".
         OUTPUT STREAM frlule TO VALUE(prognamn3) APPEND.
         lonfil =  "LULEELNATfran" + STRING(TODAY,"99999999") + ".d".
         prognamn3 = prognamn5  + lonfil.                 
         OUTPUT STREAM frlulekop TO VALUE(prognamn3) NO-ECHO.           
      END.
      ELSE DO:
         prognamn3 = prognamn5 + "frkollomkLULE.d".
         OUTPUT STREAM sjuklule TO VALUE(prognamn3) NO-ECHO.   
         prognamn3 = prognamn5 + "LULEELNATomk.d".
         OUTPUT STREAM frlule TO VALUE(prognamn3) APPEND.      
         lonfil =  "LULEELNATomkfran" + STRING(TODAY,"99999999") + ".d".
         prognamn3 = prognamn5  + lonfil.                 
         OUTPUT STREAM frlulekop TO VALUE(prognamn3) NO-ECHO.            
      END.
      
   END.          
   
   anstperson = "".
   FOR EACH franvarotemp WHERE franvarotemp.VIJUDID = "03" USE-INDEX VIJUDID:
      overrapp2 = "".
      IF anstperson NE franvarotemp.PNR THEN DO: 
         overrapp2 = "".
         PUT STREAM stanslule overrapp2 SKIP.
         FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONNUMMER = franvarotemp.PNR NO-LOCK NO-ERROR.            
         ASSIGN
         SUBSTRING(overrapp2,2,11) = STRING(franvarotemp.PNR,"999999-9999").
         IF AVAILABLE PERSONALTAB THEN DO: 
            SUBSTRING(overrapp2,14,38) = PERSONALTAB.PERSONALKOD + " " + PERSONALTAB.FORNAMN + " " + PERSONALTAB.EFTERNAMN.  
         END.           
         anstperson = franvarotemp.PNR.            
         PUT STREAM stanslule overrapp2 SKIP.
      END.
      
      ASSIGN overrapp2 = "".   
      ASSIGN                        
      SUBSTRING(overrapp2,2,8) = SUBSTRING(franvarotemp.LART,1,8)
      SUBSTRING(overrapp2,24,10) = STRING(franvarotemp.FRAN,"99999999")
      SUBSTRING(overrapp2,39,10) = STRING(franvarotemp.TILL,"99999999").         
      
      IF franvarotemp.LART = "600" THEN DO:                 
         IF franvarotemp.TIMMAR > 0 THEN DO:                       
            franvarotemp.LART = "602".
            omfatt = 0.
            IF franvarotemp.TTID > 0 THEN DO:      
               omfatt = ( klock100(franvarotemp.TIMMAR) / klock100(franvarotemp.TTID)) * 100.
            END.
            overrapp1 = "214006" + "0" + franvarotemp.PNR +
            "A" + franvarotemp.LART + "0000000000000000000000000000000000000" +
            STRING(franvarotemp.FRAN,"99999999") +
            "00000" +            
            STRING(franvarotemp.TILL,"99999999") + 
            "00000" +            
            "0000000000000000" +
            "000000" +         
            "                                                                        " +
            STRING(omfatt,"999.99") .     
            SUBSTRING(overrapp2,57,5) =  STRING(omfatt,"999.99").
         END.
         ELSE DO:         
            overrapp1 = "214006" + "0" + franvarotemp.PNR +
            "A" + franvarotemp.LART + "0000000000000000000000000000000000000" +
            STRING(franvarotemp.FRAN,"99999999") +
            "00000" +            
            STRING(franvarotemp.TILL,"99999999") + 
            "00000" +            
            "0000000000000000" +
            "000000" +         
            "                                                                        " +
            "100.00" .              
            SUBSTRING(overrapp2,57,5) =  STRING(100.00,"999.99").
         END.         
      END.   
      ELSE IF franvarotemp.LART = "440" OR franvarotemp.LART = "442" OR franvarotemp.LART = "465" OR franvarotemp.LART = "623" OR franvarotemp.LART = "692"
      OR franvarotemp.LART = "693"  OR franvarotemp.LART = "697" OR franvarotemp.LART = "860" OR franvarotemp.LART = "865" OR franvarotemp.LART = "875" THEN  DO:
         /*timavdrag*/         
         rgr = "    ".
         IF franvarotemp.LART = "440" THEN rgr = "1420".         
         IF franvarotemp.TIMMAR > 0 THEN DO:  
            IF franvarotemp.FRAN = franvarotemp.TILL THEN DO:               
               frantim = klock100(franvarotemp.TIMMAR) * 100.
               overrapp1 = "214006" + "0" + franvarotemp.PNR +
               "L" + franvarotemp.LART + 
               STRING(frantim,"-999999999") +
               "000000000000000000000000000" +
               STRING(franvarotemp.FRAN,"99999999") +
               "00000" +
               STRING(franvarotemp.TILL,"99999999") + 
               "00000" +
               "0000000000000000" +
               "000000" +         
               "                    " +
               "                    " +
               "            " +
               SUBSTRING(rgr,1,4) + "                " +              
               "100.00" .            

               SUBSTRING(overrapp2,50,5) = STRING(klock100(franvarotemp.TIMMAR),"999.99").
               SUBSTRING(overrapp2,64,4) = SUBSTRING(rgr,1,4).
            END.
            ELSE DO:            
               frantim = franvarotemp.TOTTID * 100.
               overrapp1 = "214006" + "0" + franvarotemp.PNR +
               "L" + franvarotemp.LART + 
               STRING(frantim,"-999999999") +
               "000000000000000000000000000" +
               STRING(franvarotemp.FRAN,"99999999") +
               "00000" +
               STRING(franvarotemp.TILL,"99999999") + 
               "00000" +
               "0000000000000000" +
               "000000" +         
               "                    " +
               "                    " +
               "            " +
               SUBSTRING(rgr,1,4) + "                " +
               "100.00" .            
               SUBSTRING(overrapp2,50,5) = STRING(franvarotemp.TOTTID,"999.99").
               SUBSTRING(overrapp2,64,4) = SUBSTRING(rgr,1,4).
            END.
         END.
         /*PUT STREAM sjuklule overrapp1 SKIP. */
      END.
      ELSE DO:
         /*kalenderdagavdrag*/      
         IF franvarotemp.TIMMAR > 0 THEN DO:                          
            omfatt = 0.
            IF franvarotemp.TTID > 0 THEN DO:      
               omfatt = ( klock100(franvarotemp.TIMMAR) / klock100(franvarotemp.TTID)) * 100.
            END.
            overrapp1 = "214006" + "0" + franvarotemp.PNR +
            "L" + franvarotemp.LART + 
            /*STRING(frantim,"-999999999") +*/
            "0000000000" +
            "000000000000000000000000000" +
            STRING(franvarotemp.FRAN,"99999999") +
            "00000" +
            STRING(franvarotemp.TILL,"99999999") + 
            "00000" +
            "0000000000000000" +
            "000000" +         
            "                    " +
            "                    " +
            "            " +
            "                    " +
            STRING(omfatt,"999.99") .              
            SUBSTRING(overrapp2,57,5) =  STRING(omfatt,"999.99").
         END.
         ELSE DO:
            ASSIGN
            startm = FALSE
            slutm = FALSE
            startdatum = ?
            slutdatum = ?.
            IF franvarotemp.LART = "657" OR franvarotemp.LART = "621" OR franvarotemp.LART = "694" THEN DO:
               IF DAY(franvarotemp.FRAN)  > 01 AND DAY(franvarotemp.FRAN)  LE 04 THEN DO:
                  regdatum = franvarotemp.FRAN - 1.
                  FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = franvarotemp.PERSONALKOD NO-LOCK NO-ERROR.      
                  persrec = RECID(PERSONALTAB).         
                  REPEAT:                  
                     RUN REGVEC.P.
                     RUN SLUTARB.P.
                     IF regstart = regslut THEN DO:
                        regdatum = regdatum - 1.
                     END.
                     ELSE LEAVE.                     
                     IF DAY(regdatum) > DAY(franvarotemp.FRAN) THEN DO: 
                        regdatum = regdatum + 1.
                        IF DAY(regdatum) = 01 THEN DO: 
                           startm = TRUE.
                           startdatum = regdatum.
                        END.
                        LEAVE.
                     END.
                  END.
               END.
               IF DAY(franvarotemp.TILL)  > 26 AND DAY(franvarotemp.TILL + 1) > 26 THEN DO:
                  regdatum = franvarotemp.TILL + 1.
                  FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = franvarotemp.PERSONALKOD NO-LOCK NO-ERROR.      
                  persrec = RECID(PERSONALTAB).         
                  REPEAT:                  
                     RUN REGVEC.P.
                     RUN SLUTARB.P.
                     IF regstart = regslut THEN DO:
                        regdatum = regdatum + 1.
                     END.
                     ELSE LEAVE.                     
                     IF DAY(regdatum) < DAY(franvarotemp.TILL) THEN DO: 
                        regdatum = regdatum - 1.
                        IF DAY(regdatum + 1 ) = 01 THEN DO: 
                           slutm = TRUE.
                           slutdatum = regdatum.
                        END.
                        LEAVE.
                     END.
                  END.
                  
               END.
               IF startm = TRUE AND slutm = TRUE THEN DO:
                  IF startdatum NE ? AND slutdatum NE ? THEN DO:                  
                     ASSIGN
                     franvarotemp.FRAN = startdatum
                     franvarotemp.TILL = slutdatum.
                  END.
               END.
            END.
            IF franvarotemp.LART = "657" THEN DO:
               IF DAY(franvarotemp.FRAN) = 01 AND DAY(franvarotemp.TILL + 1) = 01 THEN DO:
                  ASSIGN franvarotemp.LART = "659".
               END.
            END.
            IF franvarotemp.LART = "621"  THEN DO:
               IF DAY(franvarotemp.FRAN) = 01 AND DAY(franvarotemp.TILL + 1) = 01 THEN DO:
                  ASSIGN franvarotemp.LART = "622".
               END.
            END.
            IF franvarotemp.LART = "694"  THEN DO:
               IF DAY(franvarotemp.FRAN) = 01 AND DAY(franvarotemp.TILL + 1) = 01 THEN DO:
                  ASSIGN franvarotemp.LART = "695".
               END.
            END.
            overrapp1 = "214006" + "0" + franvarotemp.PNR +
            "L" + franvarotemp.LART + "0000000000000000000000000000000000000" +
            STRING(franvarotemp.FRAN,"99999999") +
            "00000" +
            STRING(franvarotemp.TILL,"99999999") + 
            "00000" +
            "0000000000000000" +
            "000000" +         
            "                    " +
            "                    " +
            "            " +
            "                    " +
            "100.00" .              
            SUBSTRING(overrapp2,57,5) =  STRING(100.00,"999.99").
         END.
      /*   PUT STREAM sjuklule overrapp1 SKIP. */
      
      END.
      IF overrapp1 NE "" THEN DO:      
         IF franvarotemp.LART = "600" OR franvarotemp.LART = "602" THEN DO:                 
            PUT STREAM sjuklule overrapp1 SKIP. 
         END.
         PUT STREAM frlule overrapp1 SKIP.
         PUT STREAM frlulekop overrapp1 SKIP.
      END.
      PUT STREAM stanslule overrapp2 SKIP.
      DELETE franvarotemp.
      overrapp1 = "".

   END.   
   overrapp1 = "999999".
   PUT STREAM frlule overrapp1 SKIP.
   PUT STREAM frlulekop overrapp1 SKIP.
   PUT STREAM stanslule overrapp2 SKIP.
   OUTPUT STREAM sjuklule CLOSE.      
   OUTPUT STREAM frlule CLOSE.
   OUTPUT STREAM frlulekop CLOSE.
   OUTPUT STREAM stanslule CLOSE.
   prognamn3 = prognamn5 + "stanstid" + ".d".   
   prognamn = prognamn5 + "stanstid" + STRING(TODAY,"999999") + ".d".              
   OS-COPY VALUE(prognamn3) VALUE(prognamn).      

   /*LEAB Luleå Energi AB*/
   prognamn3 = prognamn5 + "LEABstanstid.d".
   OUTPUT STREAM stanslule TO VALUE(prognamn3) APPEND.   
   
   IF Guru.Konstanter:globforetag = "LULE" THEN DO:      
      IF korvar = "" THEN DO:      
         prognamn3 = prognamn5 + "frkollLEAB.d".
         OUTPUT STREAM sjuklule TO VALUE(prognamn3) NO-ECHO.   
         prognamn3 = prognamn5 + "LULELEAB.d".
         OUTPUT STREAM frlule TO VALUE(prognamn3) APPEND.
         lonfil =  "LULELEABfran" + STRING(TODAY,"99999999") + ".d".
         prognamn3 = prognamn5  + lonfil.                 
         OUTPUT STREAM frlulekop TO VALUE(prognamn3) NO-ECHO.           
      END.
      ELSE DO:
         prognamn3 = prognamn5 + "frkollomkLEAB.d".
         OUTPUT STREAM sjuklule TO VALUE(prognamn3) NO-ECHO.   
         prognamn3 = prognamn5 + "LULELEABomk.d".
         OUTPUT STREAM frlule TO VALUE(prognamn3) APPEND.      
         lonfil =  "LULELEABomkfran" + STRING(TODAY,"99999999") + ".d".
         prognamn3 = prognamn5  + lonfil.                 
         OUTPUT STREAM frlulekop TO VALUE(prognamn3) NO-ECHO.            
      END.
      
   END.             
   anstperson = "".
   FOR EACH franvarotemp WHERE franvarotemp.VIJUDID = "01" USE-INDEX VIJUDID:
      overrapp2 = "".
      IF anstperson NE franvarotemp.PNR THEN DO: 
         overrapp2 = "".
         PUT STREAM stanslule overrapp2 SKIP.
         FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONNUMMER = franvarotemp.PNR NO-LOCK NO-ERROR.            
         ASSIGN
         SUBSTRING(overrapp2,2,11) = STRING(franvarotemp.PNR,"999999-9999").
         IF AVAILABLE PERSONALTAB THEN DO: 
            SUBSTRING(overrapp2,14,38) = PERSONALTAB.PERSONALKOD + " " + PERSONALTAB.FORNAMN + " " + PERSONALTAB.EFTERNAMN.  
         END.           
         anstperson = franvarotemp.PNR.            
         PUT STREAM stanslule overrapp2 SKIP.
      END.
      
      ASSIGN overrapp2 = "".   
      ASSIGN                        
      SUBSTRING(overrapp2,2,8) = SUBSTRING(franvarotemp.LART,1,8)
      SUBSTRING(overrapp2,24,10) = STRING(franvarotemp.FRAN,"99999999")
      SUBSTRING(overrapp2,39,10) = STRING(franvarotemp.TILL,"99999999").         
      
      IF franvarotemp.LART = "600" THEN DO:                 
         IF franvarotemp.TIMMAR > 0 THEN DO:                       
            franvarotemp.LART = "602".
            omfatt = 0.
            IF franvarotemp.TTID > 0 THEN DO:      
               omfatt = ( klock100(franvarotemp.TIMMAR) / klock100(franvarotemp.TTID)) * 100.
            END.
            overrapp1 = "214006" + "0" + franvarotemp.PNR +
            "A" + franvarotemp.LART + "0000000000000000000000000000000000000" +
            STRING(franvarotemp.FRAN,"99999999") +
            "00000" +            
            STRING(franvarotemp.TILL,"99999999") + 
            "00000" +            
            "0000000000000000" +
            "000000" +         
            "                                                                        " +
            STRING(omfatt,"999.99") .     
            SUBSTRING(overrapp2,57,5) =  STRING(omfatt,"999.99").
         END.
         ELSE DO:         
            overrapp1 = "214006" + "0" + franvarotemp.PNR +
            "A" + franvarotemp.LART + "0000000000000000000000000000000000000" +
            STRING(franvarotemp.FRAN,"99999999") +
            "00000" +            
            STRING(franvarotemp.TILL,"99999999") + 
            "00000" +            
            "0000000000000000" +
            "000000" +         
            "                                                                        " +
            "100.00" .              
            SUBSTRING(overrapp2,57,5) =  STRING(100.00,"999.99").
         END.         
      END.   
      ELSE IF franvarotemp.LART = "440" OR franvarotemp.LART = "442" OR franvarotemp.LART = "465" OR franvarotemp.LART = "623" OR franvarotemp.LART = "692"
      OR franvarotemp.LART = "693"  OR franvarotemp.LART = "697" OR franvarotemp.LART = "860" OR franvarotemp.LART = "865" OR franvarotemp.LART = "875" THEN  DO:
         /*timavdrag*/         
         rgr = "    ".
         IF franvarotemp.LART = "440" THEN rgr = "1420".         
         IF franvarotemp.TIMMAR > 0 THEN DO:  
            IF franvarotemp.FRAN = franvarotemp.TILL THEN DO:               
               frantim = klock100(franvarotemp.TIMMAR) * 100.
               overrapp1 = "214006" + "0" + franvarotemp.PNR +
               "L" + franvarotemp.LART + 
               STRING(frantim,"-999999999") +
               "000000000000000000000000000" +
               STRING(franvarotemp.FRAN,"99999999") +
               "00000" +
               STRING(franvarotemp.TILL,"99999999") + 
               "00000" +
               "0000000000000000" +
               "000000" +         
               "                    " +
               "                    " +
               "            " +
               SUBSTRING(rgr,1,4) + "                " +              
               "100.00" .            

               SUBSTRING(overrapp2,50,5) = STRING(klock100(franvarotemp.TIMMAR),"999.99").
               SUBSTRING(overrapp2,64,4) = SUBSTRING(rgr,1,4).
            END.
            ELSE DO:            
               frantim = franvarotemp.TOTTID * 100.
               overrapp1 = "214006" + "0" + franvarotemp.PNR +
               "L" + franvarotemp.LART + 
               STRING(frantim,"-999999999") +
               "000000000000000000000000000" +
               STRING(franvarotemp.FRAN,"99999999") +
               "00000" +
               STRING(franvarotemp.TILL,"99999999") + 
               "00000" +
               "0000000000000000" +
               "000000" +         
               "                    " +
               "                    " +
               "            " +
               SUBSTRING(rgr,1,4) + "                " +
               "100.00" .            
               SUBSTRING(overrapp2,50,5) = STRING(franvarotemp.TOTTID,"999.99").
               SUBSTRING(overrapp2,64,4) = SUBSTRING(rgr,1,4).
            END.
         END.
         /*PUT STREAM sjuklule overrapp1 SKIP. */
      END.
      ELSE DO:
         /*kalenderdagavdrag*/      
         IF franvarotemp.TIMMAR > 0 THEN DO:                          
            omfatt = 0.
            IF franvarotemp.TTID > 0 THEN DO:      
               omfatt = ( klock100(franvarotemp.TIMMAR) / klock100(franvarotemp.TTID)) * 100.
            END.
            overrapp1 = "214006" + "0" + franvarotemp.PNR +
            "L" + franvarotemp.LART + 
            /*STRING(frantim,"-999999999") +*/
            "0000000000" +
            "000000000000000000000000000" +
            STRING(franvarotemp.FRAN,"99999999") +
            "00000" +
            STRING(franvarotemp.TILL,"99999999") + 
            "00000" +
            "0000000000000000" +
            "000000" +         
            "                    " +
            "                    " +
            "            " +
            "                    " +
            STRING(omfatt,"999.99") .              
            SUBSTRING(overrapp2,57,5) =  STRING(omfatt,"999.99").
         END.
         ELSE DO:
            ASSIGN
            startm = FALSE
            slutm = FALSE
            startdatum = ?
            slutdatum = ?.
            IF franvarotemp.LART = "657" OR franvarotemp.LART = "621" OR franvarotemp.LART = "694" THEN DO:
               IF DAY(franvarotemp.FRAN)  > 01 AND DAY(franvarotemp.FRAN)  LE 04 THEN DO:
                  regdatum = franvarotemp.FRAN - 1.
                  FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = franvarotemp.PERSONALKOD NO-LOCK NO-ERROR.      
                  persrec = RECID(PERSONALTAB).         
                  REPEAT:                  
                     RUN REGVEC.P.
                     RUN SLUTARB.P.
                     IF regstart = regslut THEN DO:
                        regdatum = regdatum - 1.
                     END.
                     ELSE LEAVE.                     
                     IF DAY(regdatum) > DAY(franvarotemp.FRAN) THEN DO: 
                        regdatum = regdatum + 1.
                        IF DAY(regdatum) = 01 THEN DO: 
                           startm = TRUE.
                           startdatum = regdatum.
                        END.
                        LEAVE.
                     END.
                  END.
               END.
               IF DAY(franvarotemp.TILL)  > 26 AND DAY(franvarotemp.TILL + 1) > 26 THEN DO:
                  regdatum = franvarotemp.TILL + 1.
                  FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = franvarotemp.PERSONALKOD NO-LOCK NO-ERROR.      
                  persrec = RECID(PERSONALTAB).         
                  REPEAT:                  
                     RUN REGVEC.P.
                     RUN SLUTARB.P.
                     IF regstart = regslut THEN DO:
                        regdatum = regdatum + 1.
                     END.
                     ELSE LEAVE.                     
                     IF DAY(regdatum) < DAY(franvarotemp.TILL) THEN DO: 
                        regdatum = regdatum - 1.
                        IF DAY(regdatum + 1 ) = 01 THEN DO: 
                           slutm = TRUE.
                           slutdatum = regdatum.
                        END.
                        LEAVE.
                     END.
                  END.
                  
               END.
               IF startm = TRUE AND slutm = TRUE THEN DO:
                  IF startdatum NE ? AND slutdatum NE ? THEN DO:                  
                     ASSIGN
                     franvarotemp.FRAN = startdatum
                     franvarotemp.TILL = slutdatum.
                  END.
               END.
            END.
            IF franvarotemp.LART = "657" THEN DO:
               IF DAY(franvarotemp.FRAN) = 01 AND DAY(franvarotemp.TILL + 1) = 01 THEN DO:
                  ASSIGN franvarotemp.LART = "659".
               END.
            END.
            IF franvarotemp.LART = "621"  THEN DO:
               IF DAY(franvarotemp.FRAN) = 01 AND DAY(franvarotemp.TILL + 1) = 01 THEN DO:
                  ASSIGN franvarotemp.LART = "622".
               END.
            END.
            IF franvarotemp.LART = "694"  THEN DO:
               IF DAY(franvarotemp.FRAN) = 01 AND DAY(franvarotemp.TILL + 1) = 01 THEN DO:
                  ASSIGN franvarotemp.LART = "695".
               END.
            END.
            overrapp1 = "214006" + "0" + franvarotemp.PNR +
            "L" + franvarotemp.LART + "0000000000000000000000000000000000000" +
            STRING(franvarotemp.FRAN,"99999999") +
            "00000" +
            STRING(franvarotemp.TILL,"99999999") + 
            "00000" +
            "0000000000000000" +
            "000000" +         
            "                    " +
            "                    " +
            "            " +
            "                    " +
            "100.00" .              
            SUBSTRING(overrapp2,57,5) =  STRING(100.00,"999.99").
         END.
      /*   PUT STREAM sjuklule overrapp1 SKIP. */
      
      END.
      IF overrapp1 NE "" THEN DO:      
         IF franvarotemp.LART = "600" OR franvarotemp.LART = "602" THEN DO:                 
            PUT STREAM sjuklule overrapp1 SKIP. 
         END.
         PUT STREAM frlule overrapp1 SKIP.
         PUT STREAM frlulekop overrapp1 SKIP.
      END.
      PUT STREAM stanslule overrapp2 SKIP.
      DELETE franvarotemp.
      overrapp1 = "".

   END.   
   overrapp1 = "999999".
   PUT STREAM frlule overrapp1 SKIP.
   PUT STREAM frlulekop overrapp1 SKIP.
   PUT STREAM stanslule overrapp2 SKIP.
   OUTPUT STREAM sjuklule CLOSE.      
   OUTPUT STREAM frlule CLOSE.
   OUTPUT STREAM frlulekop CLOSE.
   OUTPUT STREAM stanslule CLOSE.


   prognamn3 = prognamn5 + "LEABstanstid" + ".d".   
   prognamn = prognamn5 + "LEABstanstid" + STRING(TODAY,"999999") + ".d".              
   OS-COPY VALUE(prognamn3) VALUE(prognamn).      


   /*Bioenergi i Luleå  AB*/
   prognamn3 = prognamn5 + "BIOEstanstid.d".
   OUTPUT STREAM stanslule TO VALUE(prognamn3) APPEND.   
   
   IF Guru.Konstanter:globforetag = "LULE" THEN DO:      
      IF korvar = "" THEN DO:      
         prognamn3 = prognamn5 + "frkollBIOE.d".
         OUTPUT STREAM sjuklule TO VALUE(prognamn3) NO-ECHO.   
         prognamn3 = prognamn5 + "LULEBIOE.d".
         OUTPUT STREAM frlule TO VALUE(prognamn3) APPEND.
         lonfil =  "LULEBIOEfran" + STRING(TODAY,"99999999") + ".d".
         prognamn3 = prognamn5  + lonfil.                 
         OUTPUT STREAM frlulekop TO VALUE(prognamn3) NO-ECHO.           
      END.
      ELSE DO:
         prognamn3 = prognamn5 + "frkollomkBIOE.d".
         OUTPUT STREAM sjuklule TO VALUE(prognamn3) NO-ECHO.   
         prognamn3 = prognamn5 + "LULEBIOEomk.d".
         OUTPUT STREAM frlule TO VALUE(prognamn3) APPEND.      
         lonfil =  "LULEBIOEomkfran" + STRING(TODAY,"99999999") + ".d".
         prognamn3 = prognamn5  + lonfil.                 
         OUTPUT STREAM frlulekop TO VALUE(prognamn3) NO-ECHO.            
      END.
      
   END.     
   /*FOR EACH  franvarotemp WHERE franvarotemp.VIJUDID = "02" AND franvarotemp.LART = "502" USE-INDEX VIJUDID:
      ccc
   END.*/
   anstperson = "".
   FOR EACH franvarotemp WHERE franvarotemp.VIJUDID = "02" USE-INDEX VIJUDID:
      overrapp2 = "".
      IF anstperson NE franvarotemp.PNR THEN DO: 
         overrapp2 = "".
         PUT STREAM stanslule overrapp2 SKIP.
         FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONNUMMER = franvarotemp.PNR NO-LOCK NO-ERROR.            
         ASSIGN
         SUBSTRING(overrapp2,2,11) = STRING(franvarotemp.PNR,"999999-9999").
         IF AVAILABLE PERSONALTAB THEN DO: 
            SUBSTRING(overrapp2,14,38) = PERSONALTAB.PERSONALKOD + " " + PERSONALTAB.FORNAMN + " " + PERSONALTAB.EFTERNAMN.  
         END.           
         anstperson = franvarotemp.PNR.            
         PUT STREAM stanslule overrapp2 SKIP.
      END.
      
      ASSIGN overrapp2 = "".   
      ASSIGN                        
      SUBSTRING(overrapp2,2,8) = SUBSTRING(franvarotemp.LART,1,8)
      SUBSTRING(overrapp2,24,10) = STRING(franvarotemp.FRAN,"99999999")
      SUBSTRING(overrapp2,39,10) = STRING(franvarotemp.TILL,"99999999").         
      
      IF franvarotemp.LART = "600" THEN DO:                 
         IF franvarotemp.TIMMAR > 0 THEN DO:                       
            franvarotemp.LART = "602".
            omfatt = 0.
            IF franvarotemp.TTID > 0 THEN DO:      
               omfatt = ( klock100(franvarotemp.TIMMAR) / klock100(franvarotemp.TTID)) * 100.
            END.
            overrapp1 = "214006" + "0" + franvarotemp.PNR +
            "A" + franvarotemp.LART + "0000000000000000000000000000000000000" +
            STRING(franvarotemp.FRAN,"99999999") +
            "00000" +            
            STRING(franvarotemp.TILL,"99999999") + 
            "00000" +            
            "0000000000000000" +
            "000000" +         
            "                                                                        " +
            STRING(omfatt,"999.99") .     
            SUBSTRING(overrapp2,57,5) =  STRING(omfatt,"999.99").
         END.
         ELSE DO:         
            overrapp1 = "214006" + "0" + franvarotemp.PNR +
            "A" + franvarotemp.LART + "0000000000000000000000000000000000000" +
            STRING(franvarotemp.FRAN,"99999999") +
            "00000" +            
            STRING(franvarotemp.TILL,"99999999") + 
            "00000" +            
            "0000000000000000" +
            "000000" +         
            "                                                                        " +
            "100.00" .              
            SUBSTRING(overrapp2,57,5) =  STRING(100.00,"999.99").
         END.         
      END.   
      ELSE IF franvarotemp.LART = "440" OR franvarotemp.LART = "442" OR franvarotemp.LART = "465" OR franvarotemp.LART = "623" OR franvarotemp.LART = "692"
      OR franvarotemp.LART = "693"  OR franvarotemp.LART = "697" OR franvarotemp.LART = "860" OR franvarotemp.LART = "865" OR franvarotemp.LART = "875" THEN  DO:
         /*timavdrag*/         
         rgr = "    ".
         IF franvarotemp.LART = "440" THEN rgr = "1420".         
         IF franvarotemp.TIMMAR > 0 THEN DO:  
            IF franvarotemp.FRAN = franvarotemp.TILL THEN DO:               
               frantim = klock100(franvarotemp.TIMMAR) * 100.
               overrapp1 = "214006" + "0" + franvarotemp.PNR +
               "L" + franvarotemp.LART + 
               STRING(frantim,"-999999999") +
               "000000000000000000000000000" +
               STRING(franvarotemp.FRAN,"99999999") +
               "00000" +
               STRING(franvarotemp.TILL,"99999999") + 
               "00000" +
               "0000000000000000" +
               "000000" +         
               "                    " +
               "                    " +
               "            " +
               SUBSTRING(rgr,1,4) + "                " +              
               "100.00" .            

               SUBSTRING(overrapp2,50,5) = STRING(klock100(franvarotemp.TIMMAR),"999.99").
               SUBSTRING(overrapp2,64,4) = SUBSTRING(rgr,1,4).
            END.
            ELSE DO:            
               frantim = franvarotemp.TOTTID * 100.
               overrapp1 = "214006" + "0" + franvarotemp.PNR +
               "L" + franvarotemp.LART + 
               STRING(frantim,"-999999999") +
               "000000000000000000000000000" +
               STRING(franvarotemp.FRAN,"99999999") +
               "00000" +
               STRING(franvarotemp.TILL,"99999999") + 
               "00000" +
               "0000000000000000" +
               "000000" +         
               "                    " +
               "                    " +
               "            " +
               SUBSTRING(rgr,1,4) + "                " +
               "100.00" .            
               SUBSTRING(overrapp2,50,5) = STRING(franvarotemp.TOTTID,"999.99").
               SUBSTRING(overrapp2,64,4) = SUBSTRING(rgr,1,4).
            END.
         END.
         /*PUT STREAM sjuklule overrapp1 SKIP. */
      END.
      ELSE IF franvarotemp.LART = "502" THEN  DO:
         /*semesterdagar med faktor för bio antalet timmar för perioden lagda i franvarotemp.TOTTID*/         
         IF franvarotemp.PKOD = "T3" THEN DO:
            frantim = franvarotemp.TOTTID * 0.1486 * 100.
         END.
         ELSE DO:
            frantim = franvarotemp.TOTTID * 0.125 * 100.
         END.
         
         overrapp1 = "214006" + "0" + franvarotemp.PNR +
         "L" + franvarotemp.LART +
         STRING(frantim,"-999999999") +
         "000000000000000000000000000" +
         STRING(franvarotemp.FRAN,"99999999") +
         "00000" +
         STRING(franvarotemp.TILL,"99999999") + 
         "00000" +
         "0000000000000000" +
         "000000" +         
         "                    " +
         "                    " +
         "            " +
         "                    " +
         "100.00" .              
         SUBSTRING(overrapp2,57,5) =  STRING(100.00,"999.99").
         
      END.
      ELSE DO:
         /*kalenderdagavdrag*/      
         IF franvarotemp.TIMMAR > 0 THEN DO:                          
            omfatt = 0.
            IF franvarotemp.TTID > 0 THEN DO:      
               omfatt = ( klock100(franvarotemp.TIMMAR) / klock100(franvarotemp.TTID)) * 100.
            END.
            overrapp1 = "214006" + "0" + franvarotemp.PNR +
            "L" + franvarotemp.LART + 
            /*STRING(frantim,"-999999999") +*/
            "0000000000" +
            "000000000000000000000000000" +
            STRING(franvarotemp.FRAN,"99999999") +
            "00000" +
            STRING(franvarotemp.TILL,"99999999") + 
            "00000" +
            "0000000000000000" +
            "000000" +         
            "                    " +
            "                    " +
            "            " +
            "                    " +
            STRING(omfatt,"999.99") .              
            SUBSTRING(overrapp2,57,5) =  STRING(omfatt,"999.99").
         END.
         ELSE DO:
            ASSIGN
            startm = FALSE
            slutm = FALSE
            startdatum = ?
            slutdatum = ?.
            IF franvarotemp.LART = "657" OR franvarotemp.LART = "621" OR franvarotemp.LART = "694" THEN DO:
               IF DAY(franvarotemp.FRAN)  > 01 AND DAY(franvarotemp.FRAN)  LE 04 THEN DO:
                  regdatum = franvarotemp.FRAN - 1.
                  FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = franvarotemp.PERSONALKOD NO-LOCK NO-ERROR.      
                  persrec = RECID(PERSONALTAB).         
                  REPEAT:                  
                     RUN REGVEC.P.
                     RUN SLUTARB.P.
                     IF regstart = regslut THEN DO:
                        regdatum = regdatum - 1.
                     END.
                     ELSE LEAVE.                     
                     IF DAY(regdatum) > DAY(franvarotemp.FRAN) THEN DO: 
                        regdatum = regdatum + 1.
                        IF DAY(regdatum) = 01 THEN DO: 
                           startm = TRUE.
                           startdatum = regdatum.
                        END.
                        LEAVE.
                     END.
                  END.
               END.
               IF DAY(franvarotemp.TILL)  > 26 AND DAY(franvarotemp.TILL + 1) > 26 THEN DO:
                  regdatum = franvarotemp.TILL + 1.
                  FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = franvarotemp.PERSONALKOD NO-LOCK NO-ERROR.      
                  persrec = RECID(PERSONALTAB).         
                  REPEAT:                  
                     RUN REGVEC.P.
                     RUN SLUTARB.P.
                     IF regstart = regslut THEN DO:
                        regdatum = regdatum + 1.
                     END.
                     ELSE LEAVE.                     
                     IF DAY(regdatum) < DAY(franvarotemp.TILL) THEN DO: 
                        regdatum = regdatum - 1.
                        IF DAY(regdatum + 1 ) = 01 THEN DO: 
                           slutm = TRUE.
                           slutdatum = regdatum.
                        END.
                        LEAVE.
                     END.
                  END.
                  
               END.
               IF startm = TRUE AND slutm = TRUE THEN DO:
                  IF startdatum NE ? AND slutdatum NE ? THEN DO:                  
                     ASSIGN
                     franvarotemp.FRAN = startdatum
                     franvarotemp.TILL = slutdatum.
                  END.
               END.
            END.
            IF franvarotemp.LART = "657" THEN DO:
               IF DAY(franvarotemp.FRAN) = 01 AND DAY(franvarotemp.TILL + 1) = 01 THEN DO:
                  ASSIGN franvarotemp.LART = "659".
               END.
            END.
            IF franvarotemp.LART = "621"  THEN DO:
               IF DAY(franvarotemp.FRAN) = 01 AND DAY(franvarotemp.TILL + 1) = 01 THEN DO:
                  ASSIGN franvarotemp.LART = "622".
               END.
            END.
            IF franvarotemp.LART = "694"  THEN DO:
               IF DAY(franvarotemp.FRAN) = 01 AND DAY(franvarotemp.TILL + 1) = 01 THEN DO:
                  ASSIGN franvarotemp.LART = "695".
               END.
            END.
            overrapp1 = "214006" + "0" + franvarotemp.PNR +
            "L" + franvarotemp.LART + "0000000000000000000000000000000000000" +
            STRING(franvarotemp.FRAN,"99999999") +
            "00000" +
            STRING(franvarotemp.TILL,"99999999") + 
            "00000" +
            "0000000000000000" +
            "000000" +         
            "                    " +
            "                    " +
            "            " +
            "                    " +
            "100.00" .              
            SUBSTRING(overrapp2,57,5) =  STRING(100.00,"999.99").
         END.
      /*   PUT STREAM sjuklule overrapp1 SKIP. */
      
      END.
      IF overrapp1 NE "" THEN DO:      
         IF franvarotemp.LART = "600" OR franvarotemp.LART = "602" THEN DO:                 
            PUT STREAM sjuklule overrapp1 SKIP. 
         END.
         PUT STREAM frlule overrapp1 SKIP.
         PUT STREAM frlulekop overrapp1 SKIP.
      END.
      PUT STREAM stanslule overrapp2 SKIP.
      DELETE franvarotemp.
      overrapp1 = "".

   END.   
   overrapp1 = "999999".
   PUT STREAM frlule overrapp1 SKIP.
   PUT STREAM frlulekop overrapp1 SKIP.
   PUT STREAM stanslule overrapp2 SKIP.
   OUTPUT STREAM sjuklule CLOSE.      
   OUTPUT STREAM frlule CLOSE.
   OUTPUT STREAM frlulekop CLOSE.
   OUTPUT STREAM stanslule CLOSE.


   prognamn3 = prognamn5 + "BIOEstanstid" + ".d".   
   prognamn = prognamn5 + "BIOEstanstid" + STRING(TODAY,"999999") + ".d".              
   OS-COPY VALUE(prognamn3) VALUE(prognamn).      
                     
   /*Lunet AB*/
   prognamn3 = prognamn5 + "stanstid.d".
   OUTPUT STREAM stanslule TO VALUE(prognamn3) APPEND.   
   
   IF Guru.Konstanter:globforetag = "LULE" THEN DO:      
      IF korvar = "" THEN DO:      
         prognamn3 = prognamn5 + "frkollLUNET.d".
         OUTPUT STREAM sjuklule TO VALUE(prognamn3) NO-ECHO.   
         prognamn3 = prognamn5 + "LULELUNET.d".
         OUTPUT STREAM frlule TO VALUE(prognamn3) APPEND.
         lonfil =  "LULELUNETfran" + STRING(TODAY,"99999999") + ".d".
         prognamn3 = prognamn5  + lonfil.                 
         OUTPUT STREAM frlulekop TO VALUE(prognamn3) NO-ECHO.           
      END.
      ELSE DO:
         prognamn3 = prognamn5 + "frkollomkLUNET.d".
         OUTPUT STREAM sjuklule TO VALUE(prognamn3) NO-ECHO.   
         prognamn3 = prognamn5 + "LULELUNETomk.d".
         OUTPUT STREAM frlule TO VALUE(prognamn3) APPEND.      
         lonfil =  "LULELUNETomkfran" + STRING(TODAY,"99999999") + ".d".
         prognamn3 = prognamn5  + lonfil.                 
         OUTPUT STREAM frlulekop TO VALUE(prognamn3) NO-ECHO.            
      END.
      
   END.          
   
   anstperson = "".
   FOR EACH franvarotemp WHERE franvarotemp.VIJUDID = "04" USE-INDEX VIJUDID:
      overrapp2 = "".
      IF anstperson NE franvarotemp.PNR THEN DO: 
         overrapp2 = "".
         PUT STREAM stanslule overrapp2 SKIP.
         FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONNUMMER = franvarotemp.PNR NO-LOCK NO-ERROR.            
         ASSIGN
         SUBSTRING(overrapp2,2,11) = STRING(franvarotemp.PNR,"999999-9999").
         IF AVAILABLE PERSONALTAB THEN DO: 
            SUBSTRING(overrapp2,14,38) = PERSONALTAB.PERSONALKOD + " " + PERSONALTAB.FORNAMN + " " + PERSONALTAB.EFTERNAMN.  
         END.           
         anstperson = franvarotemp.PNR.            
         PUT STREAM stanslule overrapp2 SKIP.
      END.
      
      ASSIGN overrapp2 = "".   
      ASSIGN                        
      SUBSTRING(overrapp2,2,8) = SUBSTRING(franvarotemp.LART,1,8)
      SUBSTRING(overrapp2,24,10) = STRING(franvarotemp.FRAN,"99999999")
      SUBSTRING(overrapp2,39,10) = STRING(franvarotemp.TILL,"99999999").         
      
      IF franvarotemp.LART = "600" THEN DO:                 
         IF franvarotemp.TIMMAR > 0 THEN DO:                       
            franvarotemp.LART = "602".
            omfatt = 0.
            IF franvarotemp.TTID > 0 THEN DO:      
               omfatt = ( klock100(franvarotemp.TIMMAR) / klock100(franvarotemp.TTID)) * 100.
            END.
            overrapp1 = "214006" + "0" + franvarotemp.PNR +
            "A" + franvarotemp.LART + "0000000000000000000000000000000000000" +
            STRING(franvarotemp.FRAN,"99999999") +
            "00000" +            
            STRING(franvarotemp.TILL,"99999999") + 
            "00000" +            
            "0000000000000000" +
            "000000" +         
            "                                                                        " +
            STRING(omfatt,"999.99") .     
            SUBSTRING(overrapp2,57,5) =  STRING(omfatt,"999.99").
         END.
         ELSE DO:         
            overrapp1 = "214006" + "0" + franvarotemp.PNR +
            "A" + franvarotemp.LART + "0000000000000000000000000000000000000" +
            STRING(franvarotemp.FRAN,"99999999") +
            "00000" +            
            STRING(franvarotemp.TILL,"99999999") + 
            "00000" +            
            "0000000000000000" +
            "000000" +         
            "                                                                        " +
            "100.00" .              
            SUBSTRING(overrapp2,57,5) =  STRING(100.00,"999.99").
         END.         
      END.   
      ELSE IF franvarotemp.LART = "440" OR franvarotemp.LART = "442" OR franvarotemp.LART = "465" OR franvarotemp.LART = "623" OR franvarotemp.LART = "692"
      OR franvarotemp.LART = "693"  OR franvarotemp.LART = "697" OR franvarotemp.LART = "860" OR franvarotemp.LART = "865" OR franvarotemp.LART = "875" THEN  DO:
         /*timavdrag*/         
         rgr = "    ".
         IF franvarotemp.LART = "440" THEN rgr = "1420".         
         IF franvarotemp.TIMMAR > 0 THEN DO:  
            IF franvarotemp.FRAN = franvarotemp.TILL THEN DO:               
               frantim = klock100(franvarotemp.TIMMAR) * 100.
               overrapp1 = "214006" + "0" + franvarotemp.PNR +
               "L" + franvarotemp.LART + 
               STRING(frantim,"-999999999") +
               "000000000000000000000000000" +
               STRING(franvarotemp.FRAN,"99999999") +
               "00000" +
               STRING(franvarotemp.TILL,"99999999") + 
               "00000" +
               "0000000000000000" +
               "000000" +         
               "                    " +
               "                    " +
               "            " +
               SUBSTRING(rgr,1,4) + "                " +              
               "100.00" .            

               SUBSTRING(overrapp2,50,5) = STRING(klock100(franvarotemp.TIMMAR),"999.99").
               SUBSTRING(overrapp2,64,4) = SUBSTRING(rgr,1,4).
            END.
            ELSE DO:            
               frantim = franvarotemp.TOTTID * 100.
               overrapp1 = "214006" + "0" + franvarotemp.PNR +
               "L" + franvarotemp.LART + 
               STRING(frantim,"-999999999") +
               "000000000000000000000000000" +
               STRING(franvarotemp.FRAN,"99999999") +
               "00000" +
               STRING(franvarotemp.TILL,"99999999") + 
               "00000" +
               "0000000000000000" +
               "000000" +         
               "                    " +
               "                    " +
               "            " +
               SUBSTRING(rgr,1,4) + "                " +
               "100.00" .            
               SUBSTRING(overrapp2,50,5) = STRING(franvarotemp.TOTTID,"999.99").
               SUBSTRING(overrapp2,64,4) = SUBSTRING(rgr,1,4).
            END.
         END.
         /*PUT STREAM sjuklule overrapp1 SKIP. */
      END.
      ELSE DO:
         /*kalenderdagavdrag*/      
         IF franvarotemp.TIMMAR > 0 THEN DO:                          
            omfatt = 0.
            IF franvarotemp.TTID > 0 THEN DO:      
               omfatt = ( klock100(franvarotemp.TIMMAR) / klock100(franvarotemp.TTID)) * 100.
            END.
            overrapp1 = "214006" + "0" + franvarotemp.PNR +
            "L" + franvarotemp.LART + 
            /*STRING(frantim,"-999999999") +*/
            "0000000000" +
            "000000000000000000000000000" +
            STRING(franvarotemp.FRAN,"99999999") +
            "00000" +
            STRING(franvarotemp.TILL,"99999999") + 
            "00000" +
            "0000000000000000" +
            "000000" +         
            "                    " +
            "                    " +
            "            " +
            "                    " +
            STRING(omfatt,"999.99") .              
            SUBSTRING(overrapp2,57,5) =  STRING(omfatt,"999.99").
         END.
         ELSE DO:
            ASSIGN
            startm = FALSE
            slutm = FALSE
            startdatum = ?
            slutdatum = ?.
            IF franvarotemp.LART = "657" OR franvarotemp.LART = "621" OR franvarotemp.LART = "694" THEN DO:
               IF DAY(franvarotemp.FRAN)  > 01 AND DAY(franvarotemp.FRAN)  LE 04 THEN DO:
                  regdatum = franvarotemp.FRAN - 1.
                  FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = franvarotemp.PERSONALKOD NO-LOCK NO-ERROR.      
                  persrec = RECID(PERSONALTAB).         
                  REPEAT:                  
                     RUN REGVEC.P.
                     RUN SLUTARB.P.
                     IF regstart = regslut THEN DO:
                        regdatum = regdatum - 1.
                     END.
                     ELSE LEAVE.                     
                     IF DAY(regdatum) > DAY(franvarotemp.FRAN) THEN DO: 
                        regdatum = regdatum + 1.
                        IF DAY(regdatum) = 01 THEN DO: 
                           startm = TRUE.
                           startdatum = regdatum.
                        END.
                        LEAVE.
                     END.
                  END.
               END.
               IF DAY(franvarotemp.TILL)  > 26 AND DAY(franvarotemp.TILL + 1) > 26 THEN DO:
                  regdatum = franvarotemp.TILL + 1.
                  FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = franvarotemp.PERSONALKOD NO-LOCK NO-ERROR.      
                  persrec = RECID(PERSONALTAB).         
                  REPEAT:                  
                     RUN REGVEC.P.
                     RUN SLUTARB.P.
                     IF regstart = regslut THEN DO:
                        regdatum = regdatum + 1.
                     END.
                     ELSE LEAVE.                     
                     IF DAY(regdatum) < DAY(franvarotemp.TILL) THEN DO: 
                        regdatum = regdatum - 1.
                        IF DAY(regdatum + 1 ) = 01 THEN DO: 
                           slutm = TRUE.
                           slutdatum = regdatum.
                        END.
                        LEAVE.
                     END.
                  END.
                  
               END.
               IF startm = TRUE AND slutm = TRUE THEN DO:
                  IF startdatum NE ? AND slutdatum NE ? THEN DO:                  
                     ASSIGN
                     franvarotemp.FRAN = startdatum
                     franvarotemp.TILL = slutdatum.
                  END.
               END.
            END.
            IF franvarotemp.LART = "657" THEN DO:
               IF DAY(franvarotemp.FRAN) = 01 AND DAY(franvarotemp.TILL + 1) = 01 THEN DO:
                  ASSIGN franvarotemp.LART = "659".
               END.
            END.
            IF franvarotemp.LART = "621"  THEN DO:
               IF DAY(franvarotemp.FRAN) = 01 AND DAY(franvarotemp.TILL + 1) = 01 THEN DO:
                  ASSIGN franvarotemp.LART = "622".
               END.
            END.
            IF franvarotemp.LART = "694"  THEN DO:
               IF DAY(franvarotemp.FRAN) = 01 AND DAY(franvarotemp.TILL + 1) = 01 THEN DO:
                  ASSIGN franvarotemp.LART = "695".
               END.
            END.
            overrapp1 = "214006" + "0" + franvarotemp.PNR +
            "L" + franvarotemp.LART + "0000000000000000000000000000000000000" +
            STRING(franvarotemp.FRAN,"99999999") +
            "00000" +
            STRING(franvarotemp.TILL,"99999999") + 
            "00000" +
            "0000000000000000" +
            "000000" +         
            "                    " +
            "                    " +
            "            " +
            "                    " +
            "100.00" .              
            SUBSTRING(overrapp2,57,5) =  STRING(100.00,"999.99").
         END.
      /*   PUT STREAM sjuklule overrapp1 SKIP. */
      
      END.
      IF overrapp1 NE "" THEN DO:      
         IF franvarotemp.LART = "600" OR franvarotemp.LART = "602" THEN DO:                 
            PUT STREAM sjuklule overrapp1 SKIP. 
         END.
         PUT STREAM frlule overrapp1 SKIP.
         PUT STREAM frlulekop overrapp1 SKIP.
      END.
      PUT STREAM stanslule overrapp2 SKIP.
      DELETE franvarotemp.
      overrapp1 = "".

   END.   
   overrapp1 = "999999".
   PUT STREAM frlule overrapp1 SKIP.
   PUT STREAM frlulekop overrapp1 SKIP.
   PUT STREAM stanslule overrapp2 SKIP.
   OUTPUT STREAM sjuklule CLOSE.      
   OUTPUT STREAM frlule CLOSE.
   OUTPUT STREAM frlulekop CLOSE.
   OUTPUT STREAM stanslule CLOSE.
   prognamn3 = prognamn5 + "stanstid" + ".d".   
   prognamn = prognamn5 + "stanstid" + STRING(TODAY,"999999") + ".d".              
   OS-COPY VALUE(prognamn3) VALUE(prognamn).      
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
OUTPUT CLOSE.
IF korvar = "" THEN DO:
   prognamn3 = prognamn5 + "felpers.d".
   OUTPUT TO VALUE(prognamn3) APPEND.
END.
ELSE DO:
   prognamn3 = prognamn5 + "felpersomk.d".
   OUTPUT TO VALUE(prognamn3) APPEND.   
END.
FOR EACH franvarotemp USE-INDEX franvaro  NO-LOCK:
  EXPORT franvarotemp.
END.

IF korvar = "" THEN DO:
   IF Guru.Konstanter:globforetag = "LULE" THEN DO:      
      lonfil =  "LULEELNAT" + STRING(TODAY,"99999999") + ".wli".
      prognamn = prognamn6  + lonfil.        
      prognamn3 = prognamn5 + "LULEELNAT.d".      
      OS-COPY VALUE(prognamn3) VALUE(prognamn). 
      prognamn = prognamn5  + lonfil.              
      OS-COPY VALUE(prognamn3) VALUE(prognamn).            

      lonfil =  "LULELEAB" + STRING(TODAY,"99999999") + ".wli".
      prognamn = prognamn6  + lonfil.        
      prognamn3 = prognamn5 + "LULELEAB.d".      
      OS-COPY VALUE(prognamn3) VALUE(prognamn). 
      prognamn = prognamn5  + lonfil.              
      OS-COPY VALUE(prognamn3) VALUE(prognamn).            

      lonfil =  "LULEBIOE" + STRING(TODAY,"99999999") + ".wli".
      prognamn = prognamn6  + lonfil.        
      prognamn3 = prognamn5 + "LULEBIOE.d".      
      OS-COPY VALUE(prognamn3) VALUE(prognamn). 
      prognamn = prognamn5  + lonfil.              
      OS-COPY VALUE(prognamn3) VALUE(prognamn).     

      lonfil =  "LULELUNET" + STRING(TODAY,"99999999") + ".wli".
      prognamn = prognamn6  + lonfil.        
      prognamn3 = prognamn5 + "LULELUNET.d".      
      OS-COPY VALUE(prognamn3) VALUE(prognamn). 
      prognamn = prognamn5  + lonfil.              
      OS-COPY VALUE(prognamn3) VALUE(prognamn).    
   END.     
END.
ELSE DO:
   IF Guru.Konstanter:globforetag = "LULE" THEN DO:
      prognamn = prognamn5 + "LULEELNATomk" + STRING(TODAY,"999999") + ".d".        
      prognamn3 = prognamn5 + "LULEELNATomk.d".      
      OS-COPY VALUE(prognamn3) VALUE(prognamn).         

      prognamn = prognamn5 + "LULELEABomk" + STRING(TODAY,"999999") + ".d".        
      prognamn3 = prognamn5 + "LULELEABomk.d".      
      OS-COPY VALUE(prognamn3) VALUE(prognamn).         

      prognamn = prognamn5 + "LULEBIOEomk" + STRING(TODAY,"999999") + ".d".        
      prognamn3 = prognamn5 + "LULEBIOEomk.d".      
      OS-COPY VALUE(prognamn3) VALUE(prognamn).         

      prognamn = prognamn5 + "LULELUNETomk" + STRING(TODAY,"999999") + ".d".        
      prognamn3 = prognamn5 + "LULELUNETomk.d".      
      OS-COPY VALUE(prognamn3) VALUE(prognamn).         
   END.        
END.

OUTPUT CLOSE.
RUN sammut_UI (INPUT 2).
{EUROPEANAMERICAN.I}