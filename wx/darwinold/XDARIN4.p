/*XDARIN4.p INLÄSNING AV MODERDARS gruddata ledningar*/       
DEFINE NEW SHARED VARIABLE quotervar AS CHARACTER FORMAT "X(256)" NO-UNDO.




DEFINE VARIABLE musz AS LOGICAL NO-UNDO.

DEFINE VARIABLE rad AS INTEGER NO-UNDO.
DEFINE VARIABLE prognamn AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE prognamndat AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE prognamnque AS CHARACTER FORMAT "X(20)" NO-UNDO.                
DEFINE VARIABLE words AS CHARACTER FORMAT "X(132)" NO-UNDO.
DEFINE VARIABLE kommando AS CHARACTER FORMAT "X(132)" NO-UNDO.
DEFINE VARIABLE kommandoprog AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE satsvar AS CHARACTER FORMAT "X(11)" NO-UNDO.
DEFINE VARIABLE enrvar AS CHARACTER FORMAT "X(11)" NO-UNDO.
DEFINE VARIABLE melvar AS INTEGER NO-UNDO.
DEFINE VARIABLE melvar2 AS INTEGER NO-UNDO.
DEFINE VARIABLE langd AS INTEGER NO-UNDO.
DEFINE VARIABLE pos1 AS INTEGER NO-UNDO. 
DEFINE VARIABLE filnamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE nrvar AS INTEGER NO-UNDO.
DEFINE VARIABLE nat1 AS INTEGER NO-UNDO.
DEFINE VARIABLE nat2 AS INTEGER NO-UNDO.
DEFINE VARIABLE nat3 AS INTEGER NO-UNDO.
DEFINE VARIABLE nat4 AS INTEGER NO-UNDO.
DEFINE VARIABLE felvar AS LOGICAL NO-UNDO.

DEFINE TEMP-TABLE tidin
   FIELD B1                 AS INTEGER 
   FIELD B2                 AS CHARACTER
   FIELD B3                 AS INTEGER 
   FIELD B4                 AS CHARACTER
   FIELD B5                 AS CHARACTER 
   FIELD B6                 AS CHARACTER
   FIELD B7                 AS INTEGER 
   FIELD B8                 AS INTEGER
   FIELD B9                 AS INTEGER 
   FIELD B10                AS INTEGER
   FIELD B11                AS LOGICAL.
   
DEFINE TEMP-TABLE infil
   FIELD PROGNAMN AS CHARACTER FORMAT "X(78)" 
   INDEX PRO IS PRIMARY PROGNAMN.
   
DEFINE TEMP-TABLE intid
   FIELD TIN AS CHARACTER FORMAT "X(78)" .
   
DEFINE TEMP-TABLE temp_TEXT
   FIELD PROGNAMN AS CHARACTER FORMAT "X(100)".   

DEFINE BUFFER distbuff FOR STORDISTRIKT.   
{AMERICANEUROPEAN.I}
{muswait.i}         
RUN in_UI.
{musarrow.i}
{EUROPEANAMERICAN.I}
PROCEDURE in_UI: 
   FOR EACH intid:
      DELETE intid.
   END.
   FOR EACH tidin:
      DELETE tidin.
   END.    
   filnamn = "\\pc112\delad\elpool\elpnj\darwin\sven\sydkraft\GRLED.txt".
   RUN PROVAG.P.
   ASSIGN
   dlcvar = dlcvar + "QUOTER.EXE"
   wtidvar = wtidvar + "kabpris.q".   
   
   OS-COMMAND SILENT VALUE(dlcvar)
   VALUE(filnamn) > VALUE(wtidvar).   
   INPUT FROM VALUE(wtidvar) NO-ECHO.    
   /*CONVERT TARGET "iso8859-1" SOURCE "ibm850" NO-ECHO.
   iso8859-1 swedish-7-bit ibm850"*/
   REPEAT:
      DO TRANSACTION: 
         SET words VIEW-AS EDITOR INNER-CHARS 78 INNER-LINES 80 WITH FRAME DDD WIDTH 80.   
         CREATE intid.   
         ASSIGN intid.TIN = words.   
      END.
   END.
   INPUT CLOSE.               
   OUTPUT TO VALUE(wtidvar).
   FOR EACH intid:          
      PUT UNFORMATTED intid.TIN SKIP.     
   END.
   OUTPUT CLOSE.
   INPUT FROM VALUE(wtidvar) NO-ECHO.
   REPEAT:
      DO TRANSACTION: 
         CREATE tidin.
         ASSIGN.
         IMPORT DELIMITER ";" tidin   NO-ERROR.
      END.               
   END.
   RUN skapasats_UI.           
   OS-DELETE VALUE(wtidvar).
END PROCEDURE.

PROCEDURE skapasats_UI:   
   FOR EACH tidin NO-LOCK:
      FIND FIRST STORDISTRIKT WHERE STORDISTRIKT.AVDELNINGNR = tidin.B1 AND
      STORDISTRIKT.VIDISTRIKT = tidin.B2 AND STORDISTRIKT.ARTAL = tidin.B3
      NO-LOCK NO-ERROR.
      IF AVAILABLE STORDISTRIKT THEN DO:         
         RUN skapakund_UI.
      END.
/*       FIND FIRST AVDELNING WHERE AVDELNING.AVDELNINGNR = tidin.B1 NO-LOCK NO-ERROR.                              */
/*       IF AVAILABLE AVDELNING THEN DO:                                                                            */
/*          FIND FIRST STORDISTRIKT WHERE STORDISTRIKT.AVDELNINGNR = tidin.B1 AND                                   */
/*          STORDISTRIKT.VIDISTRIKT = tidin.B2 AND STORDISTRIKT.ARTAL = tidin.B3                                    */
/*          NO-LOCK NO-ERROR.                                                                                       */
/*          IF AVAILABLE STORDISTRIKT THEN DO:                                                                      */
/*             RUN skapakund_UI.                                                                                    */
/*          END.                                                                                                    */
/*          ELSE DO:                                                                                                */
/*             FIND LAST STORDISTRIKT USE-INDEX DISTRIKTID NO-LOCK NO-ERROR.                                        */
/*             nrvar = STORDISTRIKT.DISTRIKTID + 1.                                                                 */
/*             FIND FIRST STORDISTRIKT WHERE STORDISTRIKT.AVDELNINGNR = tidin.B1 AND                                */
/*             STORDISTRIKT.VIDISTRIKT = tidin.B2 AND STORDISTRIKT.ARTAL = 1999                                     */
/*             NO-LOCK NO-ERROR.                                                                                    */
/*             IF AVAILABLE STORDISTRIKT THEN DO:                                                                   */
/*                CREATE distbuff.                                                                                  */
/*                ASSIGN                                                                                            */
/*                distbuff.AVDELNINGNR = STORDISTRIKT.AVDELNINGNR                                                   */
/*                distbuff.DISTRIKTID = nrvar                                                                       */
/*                distbuff.VIDISTRIKT = tidin.B2                                                                    */
/*                distbuff.NAMN = STORDISTRIKT.NAMN                                                                 */
/*                distbuff.ARTAL = tidin.B3.                                                                        */
/*                FIND STORDISTRIKT WHERE STORDISTRIKT.DISTRIKTID = nrvar                                           */
/*                NO-LOCK NO-ERROR.                                                                                 */
/*                RUN skapakund_UI.                                                                                 */
/*             END.                                                                                                 */
/*             ELSE DO:                                                                                             */
/*                CREATE STORDISTRIKT.                                                                              */
/*                ASSIGN                                                                                            */
/*                STORDISTRIKT.AVDELNINGNR = tidin.B1                                                               */
/*                STORDISTRIKT.DISTRIKTID = nrvar                                                                   */
/*                STORDISTRIKT.VIDISTRIKT = tidin.B2                                                                */
/*                STORDISTRIKT.NAMN = tidin.B2                                                                      */
/*                STORDISTRIKT.ARTAL = tidin.B3.                                                                    */
/*                RUN skapakund_UI.                                                                                 */
/*             END.                                                                                                 */
/*          END.                                                                                                    */
/*       END.                                                                                                       */
/*       ELSE DO:                                                                                                   */
/*          CREATE temp_text.                                                                                       */
/*          ASSIGN                                                                                                  */
/*          temp_text.PROGNAMN = "FORE SAKNAS" + STRING(tidin.B1) + ";" + tidin.B2 + ";" + STRING(tidin.B3) + ";" + */
/*          tidin.B4 + ";" + tidin.B5 + ";" + tidin.B6 + ";"                                                        */
/*          + STRING(tidin.B7) + ";" + STRING(tidin.B8) + ";" + STRING(tidin.B9) + ";" + STRING(tidin.B10) + ";" +  */
/*          STRING(tidin.B11).                                                                                      */
/*       END.                                                                                                       */
   END.   
   OUTPUT TO "\\pc112\delad\elpool\elpnj\darwin\sven\sydkraft\FELLED.txt" 
   APPEND CONVERT TARGET "iso8859-1" SOURCE "iso8859-1" 
   NO-ECHO.
   FOR EACH temp_TEXT NO-LOCK.
      PUT UNFORMATTED temp_TEXT.PROGNAMN SKIP.
   END.
END PROCEDURE.

PROCEDURE skapakund_UI:   
   FIND FIRST INLASTAB WHERE INLASTAB.INKOD = "D" AND INLASTAB.INKODTYP = "1" AND
   INLASTAB.INKODPOSCH = SUBSTRING(tidin.B4,3,2) USE-INDEX INKOD
   NO-LOCK NO-ERROR.
   FIND FIRST SPANNINGSNIV WHERE SPANNINGSNIV.INKODID = INLASTAB.INKODID
   NO-LOCK NO-ERROR.
   RUN textkoll_UI.
   IF felvar = FALSE THEN DO:
      FIND FIRST LEDNINGSDATA WHERE LEDNINGSDATA.DISTRIKTID = STORDISTRIKT.DISTRIKTID AND
      LEDNINGSDATA.SPANID = SPANNINGSNIV.SPANID AND LEDNINGSDATA.ARTAL = STORDISTRIKT.ARTAL
      AND LEDNINGSDATA.NATUPPLAGGID1 = nat1 AND LEDNINGSDATA.NATUPPLAGGID2 = nat2 and
      LEDNINGSDATA.NATUPPLAGGID3 = nat3 AND LEDNINGSDATA.NATUPPLAGGID4 = nat4 EXCLUSIVE-LOCK NO-ERROR.
      IF AVAILABLE LEDNINGSDATA THEN DO:
         IF tidin.B11 = FALSE THEN DO:
            ASSIGN
            LEDNINGSDATA.LANGDLUFT = tidin.B7
            LEDNINGSDATA.LANGDBLAND = tidin.B8
            LEDNINGSDATA.LANGDKABEL = tidin.B9
            LEDNINGSDATA.LANGD = tidin.B10.
         END.
      END.
      ELSE DO:  
         IF tidin.B10 > 0 THEN DO:          
            CREATE LEDNINGSDATA.
            ASSIGN
            LEDNINGSDATA.DISTRIKTID = STORDISTRIKT.DISTRIKTID
            LEDNINGSDATA.ARTAL = STORDISTRIKT.ARTAL
            LEDNINGSDATA.LANGDLUFT = tidin.B7
            LEDNINGSDATA.LANGDBLAND = tidin.B8
            LEDNINGSDATA.LANGDKABEL = tidin.B9
            LEDNINGSDATA.LANGD = tidin.B10
            LEDNINGSDATA.SPANID = SPANNINGSNIV.SPANID
            LEDNINGSDATA.NATUPPLAGGID1 = nat1
            LEDNINGSDATA.NATUPPLAGGID2 = nat2
            LEDNINGSDATA.NATUPPLAGGID3 = nat3
            LEDNINGSDATA.NATUPPLAGGID4 = nat4.
         END.   
      END.   
   END.
END PROCEDURE.   

PROCEDURE textkoll_UI:
   felvar = FALSE.
   IF tidin.B5 + ";" + tidin.B6 = "Effektbrytare       -utomhus;Effektbrytare" THEN DO:
      ASSIGN
      nat1 = 4
      nat2 = 8
      nat3 = 20
      nat4 = 0.
   END.
   ELSE IF tidin.B5 + ";" + tidin.B6 = "Lastfrånskiljare    -utomhus;Lastfrånskiljare" THEN DO:
      ASSIGN
      nat1 = 4
      nat2 = 12
      nat3 = 27
      nat4 = 0.
   END.
   ELSE IF tidin.B5 + ";" + tidin.B6 = "Frånskiljare        -inomhus;Frånskiljare" THEN DO:
      ASSIGN
      nat1 = 4
      nat2 = 9
      nat3 = 21
      nat4 = 0.
   END.
   ELSE IF tidin.B5 + ";" + tidin.B6 = "Krafttrafo -olje med exp.kärl;Krafttransformator" THEN DO:
      ASSIGN
      nat1 = 4
      nat2 = 10
      nat3 = 23
      nat4 = 0.
   END.
   ELSE IF tidin.B5 + ";" + tidin.B6 = "Effektbrytare       -inomhus;Effektbrytare" THEN DO:
      ASSIGN
      nat1 = 4
      nat2 = 8
      nat3 = 19
      nat4 = 0.
   END.
   ELSE IF tidin.B5 + ";" + tidin.B6 = "Lastfrånskiljare    -inomhus;Lastfrånskiljare" THEN DO:
      ASSIGN
      nat1 = 4
      nat2 = 12
      nat3 = 26
      nat4 = 0.
   END.
   ELSE IF tidin.B5 + ";" + tidin.B6 = "Lastfr.sk. m. säkr. -inomhus;Lastfrånskiljare med säkring" THEN DO:
      ASSIGN
      nat1 = 4
      nat2 = 12
      nat3 = 28
      nat4 = 0.
   END.
   ELSE IF tidin.B5 + ";" + tidin.B6 = "-olja med exp.kärl;Krafttransformator" THEN DO:
      ASSIGN
      nat1 = 4
      nat2 = 10
      nat3 = 23
      nat4 = 0.
   END.
   ELSE IF tidin.B5 + ";" + tidin.B6 = "-olja sluten;Krafttransformator" THEN DO:
      ASSIGN
      nat1 = 4
      nat2 = 10
      nat3 = 30
      nat4 = 0.
   END.
   ELSE IF tidin.B5 + ";" + tidin.B6 = "-torrisolerad luft;Krafttransformator" THEN DO:
      ASSIGN
      nat1 = 4
      nat2 = 10
      nat3 = 31
      nat4 = 0.
   END.
   ELSE IF tidin.B5 + ";" + tidin.B6 = "totalt;Kondensatorbatterier" THEN DO:
      ASSIGN
      nat1 = 4
      nat2 = 11
      nat3 = 24
      nat4 = 0.
   END.
   ELSE IF tidin.B5 + ";" + tidin.B6 = "varav -PCB;Kondensatorbatterier" THEN DO:
      ASSIGN
      nat1 = 4
      nat2 = 11
      nat3 = 25
      nat4 = 0.
   END.
   ELSE IF tidin.B5 + ";" + tidin.B6 = "Kabel i mark, totalt;Markledning" THEN DO:
      ASSIGN
      nat1 = 2
      nat2 = 2
      nat3 = 3
      nat4 = 0.
   END.
   ELSE IF tidin.B5 + ";" + tidin.B6 = "Varav PEX;Markledning" THEN DO:
      ASSIGN
      nat1 = 2
      nat2 = 2
      nat3 = 4
      nat4 = 0.
   END.
   ELSE IF tidin.B5 + ";" + tidin.B6 = "Fördelningsstation;" THEN DO:
      ASSIGN
      nat1 = 3
      nat2 = 4
      nat3 = 0
      nat4 = 0.
   END.
   ELSE IF tidin.B5 + ";" + tidin.B6 = "Friledning, oisolerad;Luftledning" THEN DO:
      ASSIGN
      nat1 = 2
      nat2 = 1
      nat3 = 1
      nat4 = 0.
   END.
   ELSE IF tidin.B5 + ";" + tidin.B6 = "Kraftkabel i vatten;Markledning" THEN DO:
      ASSIGN
      nat1 = 2
      nat2 = 2
      nat3 = 5
      nat4 = 0.
   END.
   ELSE IF tidin.B5 + ";" + tidin.B6 = "Kopplingsstation;" THEN DO:
      ASSIGN
      nat1 = 3
      nat2 = 5
      nat3 = 0
      nat4 = 0.
   END.
   ELSE IF tidin.B5 + ";" + tidin.B6 = "Stolpstation;Nätstation" THEN DO:
      ASSIGN
      nat1 = 3
      nat2 = 7
      nat3 = 13
      nat4 = 0.
   END.
   ELSE IF tidin.B5 + ";" + tidin.B6 = "Btg el stenstn. inomhusman.;Nätstation" THEN DO:
      ASSIGN
      nat1 = 3
      nat2 = 7
      nat3 = 11
      nat4 = 0.
   END.
   ELSE IF tidin.B5 + ";" + tidin.B6 = "Btg el stenstn. utomhusman.;Nätstation" THEN DO:
      ASSIGN
      nat1 = 3
      nat2 = 7
      nat3 = 12
      nat4 = 0.
   END.
   ELSE IF tidin.B5 + ";" + tidin.B6 = "Plåtstation;Nätstation" THEN DO:
      ASSIGN
      nat1 = 3
      nat2 = 7
      nat3 = 14
      nat4 = 0.
   END.
   ELSE IF tidin.B5 + ";" + tidin.B6 = "Satellitstation;Nätstation" THEN DO:
      ASSIGN
      nat1 = 3
      nat2 = 7
      nat3 = 15
      nat4 = 0.
   END.
   ELSE IF tidin.B5 + ";" + tidin.B6 = "Kapslad transformator;Nätstation" THEN DO:
      ASSIGN
      nat1 = 3
      nat2 = 7
      nat3 = 17
      nat4 = 0.
   END.
   ELSE IF tidin.B5 + ";" + tidin.B6 = "Annan/Inhyst station;Nätstation" THEN DO:
      ASSIGN
      nat1 = 3
      nat2 = 7
      nat3 = 18
      nat4 = 0.
   END.
   ELSE IF tidin.B5 + ";" + tidin.B6 = "Hängkabelledning;Luftledning" THEN DO:
      ASSIGN
      nat1 = 2
      nat2 = 1
      nat3 = 7
      nat4 = 0.
   END.
   ELSE IF tidin.B5 + ";" + tidin.B6 = "Hängspiralkabelledning;Luftledning" THEN DO:
      ASSIGN
      nat1 = 2
      nat2 = 1
      nat3 = 8
      nat4 = 0.
   END.
   ELSE IF tidin.B5 + ";" + tidin.B6 = "Avgrenings-/Kabelskåp;Luftledning" THEN DO:
      ASSIGN
      nat1 = 2
      nat2 = 1
      nat3 = 33
      nat4 = 0.
   END.
   ELSE IF tidin.B5 + ";" + tidin.B6 = "Lastfrånskiljare;App. i luftledn. och stolpstn." THEN DO:
      ASSIGN
      nat1 = 2
      nat2 = 1
      nat3 = 2
      nat4 = 3.
   END.
   ELSE IF tidin.B5 + ";" + tidin.B6 = "Frånskiljare;App. i luftledn. och stolpstn." THEN DO:
      ASSIGN
      nat1 = 2
      nat2 = 1
      nat3 = 2
      nat4 = 2.
   END.
   ELSE IF tidin.B5 + ";" + tidin.B6 = "Frånskiljare        -utomhus;Frånskiljare" THEN DO:
      ASSIGN
      nat1 = 4
      nat2 = 9
      nat3 = 22
      nat4 = 0.
   END.
   ELSE IF tidin.B5 + ";" + tidin.B6 = "Friledning, isolerad;Luftledning" THEN DO:
      ASSIGN
      nat1 = 2
      nat2 = 1
      nat3 = 6
      nat4 = 0.
   END.
   ELSE IF tidin.B5 + ";" + tidin.B6 = "Avgrenings-/Kabelskåp;Markledning" THEN DO:
      ASSIGN
      nat1 = 2
      nat2 = 2
      nat3 = 10
      nat4 = 0.
   END.
   ELSE IF tidin.B5 + ";" + tidin.B6 = "Effektbrytare -utomhus;Effektbrytare" THEN DO:
      ASSIGN
      nat1 = 4
      nat2 = 8
      nat3 = 20
      nat4 = 0.
   END.
   ELSE IF tidin.B5 + ";" + tidin.B6 = "Frånskiljare  -utomhus;Frånskiljare" THEN DO:
      ASSIGN
      nat1 = 4
      nat2 = 9
      nat3 = 22
      nat4 = 0.
   END.
   ELSE IF tidin.B5 + ";" + tidin.B6 = "Krafttrafo -olje sluten;Krafttransformator" THEN DO:
      ASSIGN
      nat1 = 4
      nat2 = 10
      nat3 = 30
      nat4 = 0.
   END.
   ELSE IF tidin.B5 + ";" + tidin.B6 = "Krafttrafo -torrisol., luft;Krafttransformator" THEN DO:
      ASSIGN
      nat1 = 4
      nat2 = 10
      nat3 = 31
      nat4 = 0.
   END.
   ELSE IF tidin.B5 + ";" + tidin.B6 = "Kondensatorbatterier, totalt;Kondensatorbatterier" THEN DO:
      ASSIGN
      nat1 = 4
      nat2 = 11
      nat3 = 24
      nat4 = 0.
   END.
   ELSE IF tidin.B5 + ";" + tidin.B6 = "Regionstation;" THEN DO:
      ASSIGN
      nat1 = 3
      nat2 = 3
      nat3 = 0
      nat4 = 0.
   END.
   ELSE IF tidin.B5 + ";" + tidin.B6 = "Markstation;Nätstation" THEN DO:
      ASSIGN
      nat1 = 3
      nat2 = 7
      nat3 = 16
      nat4 = 0.
   END.
   ELSE IF tidin.B5 + ";" + tidin.B6 = "Lastfr.sk. m. säkr. -utomhus;Lastfrånskiljare med säkring" THEN DO:
      ASSIGN
      nat1 = 4
      nat2 = 12
      nat3 = 29
      nat4 = 0.
   END.
   ELSE IF tidin.B5 + ";" + tidin.B6 = "Effektbrytare;App. i luftledn. och stolpstn." THEN DO:
      ASSIGN
      nat1 = 2
      nat2 = 1
      nat3 = 2
      nat4 = 1.
   END.
   ELSE IF tidin.B5 + ";" + tidin.B6 = "Likriktarstation;" THEN DO:
      ASSIGN
      nat1 = 3
      nat2 = 6
      nat3 = 0
      nat4 = 0.
   END.
   ELSE IF tidin.B5 + ";" + tidin.B6 = "Krafttrafo -torrisol., solid;Krafttransformator" THEN DO:
      ASSIGN
      nat1 = 4
      nat2 = 10
      nat3 = 32
      nat4 = 0.
   END.
   ELSE IF tidin.B5 + ";" + tidin.B6 = "Effektbrytare -inomhus;Effektbrytare" THEN DO:
      ASSIGN
      nat1 = 4
      nat2 = 8
      nat3 = 19
      nat4 = 0.
   END.
   ELSE IF tidin.B5 + ";" + tidin.B6 = "Frånskiljare  -inomhus;Frånskiljare   " THEN DO:
      ASSIGN
      nat1 = 4
      nat2 = 9
      nat3 = 21
      nat4 = 0.
   END.
   ELSE IF tidin.B5 + ";" + tidin.B6 = "Kondensatorbatteri, serie;App. i luftledn. och stolpstn" THEN DO:
     ASSIGN
      nat1 = 2
      nat2 = 1
      nat3 = 2
      nat4 = 4.
   END.
   ELSE IF tidin.B5 + ";" + tidin.B6 = "Kondensatorbatteri, shunt;App. i luftledn. och stolpstn." THEN DO:
      ASSIGN
      nat1 = 2
      nat2 = 1
      nat3 = 2
      nat4 = 5.
   END.
   ELSE IF tidin.B5 + ";" + tidin.B6 = "Kondensatorbatteri, serie;App. i luftledn. och stolpstn." THEN DO:
      ASSIGN
      nat1 = 2
      nat2 = 1
      nat3 = 2
      nat4 = 4.
   END.
   ELSE IF tidin.B5 + ";" + tidin.B6 = "Avgreningsskåp;Markledning" THEN DO:
      ASSIGN
      nat1 = 2
      nat2 = 2
      nat3 = 9
      nat4 = 0.
   END.
   ELSE IF tidin.B5 + ";" + tidin.B6 = "Kondensatorbetterier, totalt;Kondensatorbatterier" THEN DO:
      ASSIGN
      nat1 = 4
      nat2 = 11
      nat3 = 24
      nat4 = 0.
   END.
   ELSE IF tidin.B5 + ";" + tidin.B6 = "Lastfr.sk. m. säkr  -utomhus;Lastfrånskiljare med säkring" THEN DO:
      ASSIGN
      nat1 = 4
      nat2 = 12
      nat3 = 29
      nat4 = 0.
   END.
   ELSE IF tidin.B5 + ";" + tidin.B6 = "Kabel i mark, toltalt;Markledning" THEN DO:
      ASSIGN
      nat1 = 2
      nat2 = 2
      nat3 = 3
      nat4 = 0.
   END.
   ELSE IF tidin.B5 + ";" + tidin.B6 = "Avgrenings-/Kabelskåp;Marklednin" THEN DO:
      ASSIGN
      nat1 = 2
      nat2 = 2
      nat3 = 10
      nat4 = 0.
   END.
   ELSE IF tidin.B5 + ";" + tidin.B6 = "Friledning, osiolerad;Luftledning" THEN DO:
      ASSIGN
      nat1 = 2
      nat2 = 1
      nat3 = 1
      nat4 = 0.
   END.
   ELSE IF tidin.B5 + ";" + tidin.B6 = "Effektbrytare;App. i luftledn. och stol" THEN DO:
      ASSIGN
      nat1 = 2
      nat2 = 1
      nat3 = 2
      nat4 = 1.
   END.
   ELSE IF tidin.B5 + ";" + tidin.B6 = "Frånskiljare;App. i luftledn. och stol" THEN DO:
      ASSIGN
      nat1 = 2
      nat2 = 1
      nat3 = 2
      nat4 = 2.
   END.
   ELSE IF tidin.B5 + ";" + tidin.B6 = "Hängkabel;Luftledning" THEN DO:
      ASSIGN
      nat1 = 2
      nat2 = 1
      nat3 = 7
      nat4 = 0.
   END.
   ELSE IF tidin.B5 + ";" + tidin.B6 = "Hängspiralkabel;Luftledning" THEN DO:
      ASSIGN
      nat1 = 2
      nat2 = 1
      nat3 = 8
      nat4 = 0.
   END.
   ELSE IF tidin.B5 + ";" + tidin.B6 = "Btg el stenstn. inomhusman;Nätstation" THEN DO:
      ASSIGN
      nat1 = 3
      nat2 = 7
      nat3 = 11
      nat4 = 0.
   END.
   ELSE IF tidin.B5 + ";" + tidin.B6 = "Btg el stenstn. utomhusman;Nätstation" THEN DO:
      ASSIGN
      nat1 = 3
      nat2 = 7
      nat3 = 12
      nat4 = 0.
   END.
   ELSE DO:
      felvar = TRUE.
      CREATE temp_text.
      ASSIGN
      temp_text.PROGNAMN = "TEXT SAKNAS" + STRING(tidin.B1) + ";" + tidin.B2 + ";" + STRING(tidin.B3) + ";" +
      tidin.B4 + ";" + tidin.B5 + ";" + tidin.B6 + ";"
      + STRING(tidin.B7) + ";" + STRING(tidin.B8) + ";" + STRING(tidin.B9) + ";" + STRING(tidin.B10) + ";" +
      STRING(tidin.B11).
   END.
END PROCEDURE.
