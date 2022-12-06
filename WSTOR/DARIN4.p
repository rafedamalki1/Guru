/*DARIN4.p INLÄSNING AV MODERDARS gruNddata ledningar*/       
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
DEFINE VARIABLE nrvar AS INTEGER NO-UNDO.
DEFINE VARIABLE nat1 AS INTEGER NO-UNDO.
DEFINE VARIABLE nat2 AS INTEGER NO-UNDO.
DEFINE VARIABLE nat3 AS INTEGER NO-UNDO.
DEFINE VARIABLE nat4 AS INTEGER NO-UNDO.
DEFINE VARIABLE felvar AS LOGICAL NO-UNDO.
DEFINE VARIABLE langdvar AS LOGICAL NO-UNDO.

{DARINTIDIN.I}
FUNCTION FFinlasttab RETURNS INTEGER 
(INPUT sid AS INTEGER):
   FIND FIRST SPANNINGSNIV WHERE SPANNINGSNIV.SPANID = sid USE-INDEX SPANID NO-LOCK NO-ERROR.
   IF AVAILABLE SPANNINGSNIV THEN DO:
      FIND FIRST INLASTAB WHERE INLASTAB.INKODID = SPANNINGSNIV.INKODID
      USE-INDEX INKODID NO-LOCK NO-ERROR.
      IF AVAILABLE INLASTAB THEN RETURN INTEGER(INLASTAB.INKODPOSCH).
      ELSE RETURN sid.
   END.
   ELSE RETURN sid.   
END FUNCTION.    
DEFINE TEMP-TABLE infil
   FIELD PROGNAMN AS CHARACTER FORMAT "X(78)" 
   INDEX PRO IS PRIMARY PROGNAMN.
   
   
DEFINE TEMP-TABLE temp_text
   FIELD B1 AS INTEGER /*företag*/
   FIELD B2 AS CHARACTER /*distrikt*/
   FIELD PROGNAMN AS CHARACTER FORMAT "X(100)".   

DEFINE BUFFER distbuff FOR STORDISTRIKT.   

DEFINE INPUT PARAMETER TABLE FOR tidin4.
DEFINE OUTPUT PARAMETER TABLE FOR temp_text.
DEFINE INPUT-OUTPUT PARAMETER ledfel AS LOGICAL NO-UNDO.

{muswait.i}         
RUN in_UI.
{musarrow.i}

PROCEDURE in_UI: 
   RUN skapasats_UI.           
/*    OS-DELETE VALUE(wtidvar). */
END PROCEDURE.

PROCEDURE skapasats_UI: 
   FIND FIRST tidin4 NO-LOCK NO-ERROR.
   IF NOT AVAILABLE tidin4 THEN DO:
      RETURN.
   END.
   FIND FIRST AVDELNING WHERE AVDELNING.AVDELNINGNR = tidin4.B1 NO-LOCK NO-ERROR.
   IF AVAILABLE AVDELNING THEN DO:
      FOR EACH tidin4 NO-LOCK:
         FIND FIRST STORDISTRIKT WHERE STORDISTRIKT.AVDELNINGNR = tidin4.B1 AND
         STORDISTRIKT.VIDISTRIKT = tidin4.B2 AND STORDISTRIKT.ARTAL = tidin4.B3
         NO-LOCK NO-ERROR.
         IF NOT AVAILABLE STORDISTRIKT THEN DO:
            FIND FIRST STORDISTRIKT WHERE STORDISTRIKT.AVDELNINGNR = tidin4.B1 AND
            INTEGER(STORDISTRIKT.VIDISTRIKT) = INTEGER(tidin4.B2) AND STORDISTRIKT.ARTAL = tidin4.B3
            NO-LOCK NO-ERROR.
         END.   
         IF NOT AVAILABLE STORDISTRIKT THEN DO:
            FIND LAST STORDISTRIKT USE-INDEX DISTRIKTID NO-LOCK NO-ERROR.                   
            nrvar = STORDISTRIKT.DISTRIKTID + 1.
            DO TRANSACTION:            
               CREATE STORDISTRIKT.                                               
               ASSIGN
               STORDISTRIKT.AVDELNINGNR = tidin4.B1
               STORDISTRIKT.DISTRIKTID = nrvar
               STORDISTRIKT.VIDISTRIKT = tidin4.B2            
               STORDISTRIKT.ARTAL = tidin4.B3.        
            END. 
         END.
      END.
      FIND FIRST temp_text NO-LOCK NO-ERROR.
      IF NOT AVAILABLE temp_text THEN DO:
         FOR EACH tidin4 NO-LOCK:
            FIND FIRST STORDISTRIKT WHERE STORDISTRIKT.AVDELNINGNR = tidin4.B1 AND
            STORDISTRIKT.VIDISTRIKT = tidin4.B2 AND STORDISTRIKT.ARTAL = tidin4.B3
            NO-LOCK NO-ERROR.
            RUN skapakund_UI.                                                                 
         END.
      END.
   END. 
   ELSE DO:
      CREATE temp_text.
      ASSIGN
      temp_text.B1 = tidin4.B1
      temp_text.B2 = tidin4.B2
      temp_text.PROGNAMN = "FÖRETAG SAKNAS". 
   END.
END PROCEDURE.

PROCEDURE skapakund_UI:   
   /*Anders Olsson Elpool i Umeå AB  30 nov 2022 10:47:46 
   finns spänningsnivån B4 = INKODPOSCH
   */
   FIND FIRST INLASTAB WHERE INLASTAB.INKOD = "D" AND INLASTAB.INKODTYP = "1" AND
   INLASTAB.INKODPOSCH = tidin4.B4 USE-INDEX INKOD
   NO-LOCK NO-ERROR.
   IF NOT AVAILABLE INLASTAB THEN RETURN.
   /*Anders Olsson Elpool i Umeå AB  30 nov 2022 10:47:46 
   finns spänningsnivån i Darwin = INKODID
   */
   
   FIND FIRST SPANNINGSNIV WHERE SPANNINGSNIV.INKODID = INLASTAB.INKODID
   NO-LOCK NO-ERROR.
   RUN textkoll_UI.   
   IF NOT AVAILABLE SPANNINGSNIV THEN RETURN.
   
   IF felvar = FALSE THEN DO:
      FIND FIRST LEDNINGSDATA WHERE LEDNINGSDATA.DISTRIKTID = STORDISTRIKT.DISTRIKTID AND
      LEDNINGSDATA.SPANID = SPANNINGSNIV.SPANID AND LEDNINGSDATA.ARTAL = STORDISTRIKT.ARTAL
      AND LEDNINGSDATA.NATUPPLAGGID1 = nat1 AND LEDNINGSDATA.NATUPPLAGGID2 = nat2 AND 
      LEDNINGSDATA.NATUPPLAGGID3 = nat3 AND LEDNINGSDATA.NATUPPLAGGID4 = nat4 EXCLUSIVE-LOCK NO-ERROR.
      IF AVAILABLE LEDNINGSDATA THEN DO:                  
         IF langdvar = FALSE THEN DO:
            ASSIGN
            LEDNINGSDATA.LANGDLUFT = tidin4.B7
            LEDNINGSDATA.LANGDBLAND = tidin4.B8
            LEDNINGSDATA.LANGDKABEL = tidin4.B9
            LEDNINGSDATA.LANGD = tidin4.B10.
            
         END.
         ELSE DO:
            ASSIGN
            LEDNINGSDATA.LANGDLUFT = tidin4.B7 / 1000
            LEDNINGSDATA.LANGDBLAND = tidin4.B8 / 1000
            LEDNINGSDATA.LANGDKABEL = tidin4.B9 / 1000
            LEDNINGSDATA.LANGD = tidin4.B10  / 1000.
         END.
         LEDNINGSDATA.SPANDARWINID = FFinlasttab(LEDNINGSDATA.SPANID).         
      END.
      ELSE DO:            
         CREATE LEDNINGSDATA.
         ASSIGN
         LEDNINGSDATA.DISTRIKTID = STORDISTRIKT.DISTRIKTID
         LEDNINGSDATA.ARTAL = STORDISTRIKT.ARTAL         
         LEDNINGSDATA.SPANID = SPANNINGSNIV.SPANID
         LEDNINGSDATA.NATUPPLAGGID1 = nat1
         LEDNINGSDATA.NATUPPLAGGID2 = nat2
         LEDNINGSDATA.NATUPPLAGGID3 = nat3
         LEDNINGSDATA.NATUPPLAGGID4 = nat4.
         IF langdvar = FALSE THEN DO:            
            ASSIGN
            LEDNINGSDATA.LANGDLUFT = tidin4.B7
            LEDNINGSDATA.LANGDBLAND = tidin4.B8
            LEDNINGSDATA.LANGDKABEL = tidin4.B9
            LEDNINGSDATA.LANGD = tidin4.B10.
         END.
         ELSE DO:            
            ASSIGN
            LEDNINGSDATA.LANGDLUFT = tidin4.B7 / 1000
            LEDNINGSDATA.LANGDBLAND = tidin4.B8 / 1000
            LEDNINGSDATA.LANGDKABEL = tidin4.B9 / 1000
            LEDNINGSDATA.LANGD = tidin4.B10  / 1000.
         END.  
         LEDNINGSDATA.SPANDARWINID = FFinlasttab(LEDNINGSDATA.SPANID).       
      END.   
   END.   
END PROCEDURE.   

PROCEDURE textkoll_UI:   
   felvar = FALSE.
   IF tidin4.B5 + ";" + tidin4.B6 = "Effektbrytare -utomhus;Effektbrytare" THEN DO:
      ASSIGN
      nat1 = 4
      nat2 = 8
      nat3 = 20
      nat4 = 0
      langdvar = FALSE.
   END.
   ELSE IF tidin4.B5 + ";" + tidin4.B6 = "Lastfrånskiljare -utomhus;Lastfrånskiljare" THEN DO:
      ASSIGN
      nat1 = 4
      nat2 = 12
      nat3 = 27
      nat4 = 0
      langdvar = FALSE.
   END.
   ELSE IF tidin4.B5 + ";" + tidin4.B6 = "Frånskiljare -inomhus;Frånskiljare" THEN DO:
      ASSIGN
      nat1 = 4
      nat2 = 9
      nat3 = 21
      nat4 = 0
      langdvar = FALSE.
   END.   
   ELSE IF tidin4.B5 + ";" + tidin4.B6 = "Effektbrytare -inomhus;Effektbrytare" THEN DO:
      ASSIGN
      nat1 = 4
      nat2 = 8
      nat3 = 19
      nat4 = 0
      langdvar = FALSE.
   END.
   ELSE IF tidin4.B5 + ";" + tidin4.B6 = "Lastfrånskiljare -inomhus;Lastfrånskiljare" THEN DO:
      ASSIGN
      nat1 = 4
      nat2 = 12
      nat3 = 26
      nat4 = 0
      langdvar = FALSE.
   END.
   ELSE IF tidin4.B5 + ";" + tidin4.B6 = "Lastfr.sk. m. säkr. -inomhus;Lastfrånskiljare med säkring" THEN DO:
      ASSIGN
      nat1 = 4
      nat2 = 12
      nat3 = 28
      nat4 = 0
      langdvar = FALSE.
   END.
   ELSE IF tidin4.B5 + ";" + tidin4.B6 = "-olja med exp.kärl;Krafttransformator" THEN DO:
      ASSIGN
      nat1 = 4
      nat2 = 10
      nat3 = 23
      nat4 = 0
      langdvar = FALSE.
   END.
   ELSE IF tidin4.B5 + ";" + tidin4.B6 = "-olja sluten;Krafttransformator" THEN DO:
      ASSIGN
      nat1 = 4
      nat2 = 10
      nat3 = 30
      nat4 = 0
      langdvar = FALSE.
   END.
   ELSE IF tidin4.B5 + ";" + tidin4.B6 = "-olje med exp.kärl;Krafttransformator" THEN DO:
      ASSIGN
      nat1 = 4
      nat2 = 10
      nat3 = 23
      nat4 = 0
      langdvar = FALSE.
   END.
   ELSE IF tidin4.B5 + ";" + tidin4.B6 = "-olje sluten;Krafttransformator" THEN DO:
      ASSIGN
      nat1 = 4
      nat2 = 10
      nat3 = 30
      nat4 = 0
      langdvar = FALSE.
   END.
   ELSE IF tidin4.B5 + ";" + tidin4.B6 = "-torrisolerad luft;Krafttransformator" THEN DO:
      ASSIGN
      nat1 = 4
      nat2 = 10
      nat3 = 31
      nat4 = 0
      langdvar = FALSE.
   END.
   ELSE IF tidin4.B5 + ";" + tidin4.B6 = "Kondensatorbatterier totalt;Kondensatorbatterier" THEN DO:
      ASSIGN
      nat1 = 4
      nat2 = 11
      nat3 = 24
      nat4 = 0
      langdvar = FALSE.
   END.
   ELSE IF tidin4.B5 + ";" + tidin4.B6 = "varav -PCB;Kondensatorbatterier" THEN DO:
      ASSIGN
      nat1 = 4
      nat2 = 11
      nat3 = 25
      nat4 = 0
      langdvar = FALSE.
   END.
   ELSE IF tidin4.B5 + ";" + tidin4.B6 = "Kraftkabel i mark;Markledning" THEN DO:
      ASSIGN
      nat1 = 2
      nat2 = 2
      nat3 = 3
      nat4 = 0
      langdvar = TRUE.
   END.
   ELSE IF tidin4.B5 + ";" + tidin4.B6 = "Varav PEX;Markledning" THEN DO:
      ASSIGN
      nat1 = 2
      nat2 = 2
      nat3 = 4
      nat4 = 0
      langdvar = TRUE.
   END.
   ELSE IF tidin4.B5 + ";" + tidin4.B6 = "Fördelningsstation;Station" THEN DO:
      ASSIGN
      nat1 = 3
      nat2 = 4
      nat3 = 0
      nat4 = 0
      langdvar = FALSE.
   END.
   ELSE IF tidin4.B5 + ";" + tidin4.B6 = "Friledning, oisolerad;Luftledning" THEN DO:
      ASSIGN
      nat1 = 2
      nat2 = 1
      nat3 = 1
      nat4 = 0
      langdvar = TRUE.
   END.
   ELSE IF tidin4.B5 + ";" + tidin4.B6 = "Kraftkabel i vatten;Markledning" THEN DO:
      ASSIGN
      nat1 = 2
      nat2 = 2
      nat3 = 5
      nat4 = 0
      langdvar = TRUE.
   END.
   ELSE IF tidin4.B5 + ";" + tidin4.B6 = "Kopplingsstation;Station" THEN DO:
      ASSIGN
      nat1 = 3
      nat2 = 5
      nat3 = 0
      nat4 = 0
      langdvar = FALSE.
   END.
   ELSE IF tidin4.B5 + ";" + tidin4.B6 = "Stolpstation;Nätstation" THEN DO:
      ASSIGN
      nat1 = 3
      nat2 = 7
      nat3 = 13
      nat4 = 0
      langdvar = FALSE.
   END.
   ELSE IF tidin4.B5 + ";" + tidin4.B6 = "Btg el stenstn. inomhusman.;Nätstation" THEN DO:
      ASSIGN
      nat1 = 3
      nat2 = 7
      nat3 = 11
      nat4 = 0
      langdvar = FALSE.
   END.
   ELSE IF tidin4.B5 + ";" + tidin4.B6 = "Btg el stenstn. utomhusman.;Nätstation" THEN DO:
      ASSIGN
      nat1 = 3
      nat2 = 7
      nat3 = 12
      nat4 = 0
      langdvar = FALSE.
   END.
   ELSE IF tidin4.B5 + ";" + tidin4.B6 = "Plåtstation;Nätstation" THEN DO:
      ASSIGN
      nat1 = 3
      nat2 = 7
      nat3 = 14
      nat4 = 0
      langdvar = FALSE.
   END.
   ELSE IF tidin4.B5 + ";" + tidin4.B6 = "Satellitstation;Nätstation" THEN DO:
      ASSIGN
      nat1 = 3
      nat2 = 7
      nat3 = 15
      nat4 = 0
      langdvar = FALSE.
   END.
   ELSE IF tidin4.B5 + ";" + tidin4.B6 = "Kapslad transformator;Nätstation" THEN DO:
      ASSIGN
      nat1 = 3
      nat2 = 7
      nat3 = 17
      nat4 = 0
      langdvar = FALSE.
   END.   
   ELSE IF tidin4.B5 + ";" + tidin4.B6 = "Inhyst station;Nätstation" THEN DO:      
      ASSIGN
      nat1 = 3
      nat2 = 7
      nat3 = 18
      nat4 = 0
      langdvar = FALSE.
   END.
   ELSE IF tidin4.B5 + ";" + tidin4.B6 = "Annan stationstyp;Nätstation" THEN DO:
      ASSIGN
      nat1 = 3
      nat2 = 7
      nat3 = 34
      nat4 = 0
      langdvar = FALSE.
   END.   
   ELSE IF tidin4.B5 + ";" + tidin4.B6 = "Hängkabelledning;Luftledning" THEN DO:
      ASSIGN
      nat1 = 2
      nat2 = 1
      nat3 = 7
      nat4 = 0
      langdvar = TRUE.
   END.
   ELSE IF tidin4.B5 + ";" + tidin4.B6 = "Hängspiralkabelledning;Luftledning" THEN DO:
      ASSIGN
      nat1 = 2
      nat2 = 1
      nat3 = 8
      nat4 = 0
      langdvar = TRUE.
   END.   
   ELSE IF tidin4.B5 + ";" + tidin4.B6 = "Säkrings- och apparatlåda;Luftledning" THEN DO:      
      ASSIGN
      nat1 = 2
      nat2 = 1
      nat3 = 33
      nat4 = 0
      langdvar = FALSE.
   END.
   ELSE IF tidin4.B5 + ";" + tidin4.B6 = "Annan ledning;Luftledning" THEN DO:      
      ASSIGN
      nat1 = 2
      nat2 = 1
      nat3 = 35
      nat4 = 0
      langdvar = TRUE.
   END.   
   ELSE IF tidin4.B5 + ";" + tidin4.B6 = "Lastfrånskiljare;App. i luftledn. och stolpstn." THEN DO:
      ASSIGN
      nat1 = 2
      nat2 = 1
      nat3 = 2
      nat4 = 3
      langdvar = FALSE.
   END.
   ELSE IF tidin4.B5 + ";" + tidin4.B6 = "Frånskiljare;App. i luftledn. och stolpstn." THEN DO:
      ASSIGN
      nat1 = 2
      nat2 = 1
      nat3 = 2
      nat4 = 2
      langdvar = FALSE.
   END.
   ELSE IF tidin4.B5 + ";" + tidin4.B6 = "Frånskiljare -utomhus;Frånskiljare" THEN DO:
      ASSIGN
      nat1 = 4
      nat2 = 9
      nat3 = 22
      nat4 = 0
      langdvar = FALSE.
   END.
   ELSE IF tidin4.B5 + ";" + tidin4.B6 = "Friledning, isolerad;Luftledning" THEN DO:
      ASSIGN
      nat1 = 2
      nat2 = 1
      nat3 = 6
      nat4 = 0
      langdvar = TRUE.
   END.
   ELSE IF tidin4.B5 + ";" + tidin4.B6 = "Avgrenings-/Kabelskåp;Markledning" THEN DO:
      ASSIGN
      nat1 = 2
      nat2 = 2
      nat3 = 9
      nat4 = 0
      langdvar = FALSE.
   END.                  
   ELSE IF tidin4.B5 + ";" + tidin4.B6 = "Säkrings- och apparatlåda;Markledning" THEN DO:      
      ASSIGN
      nat1 = 2
      nat2 = 2
      nat3 = 10
      nat4 = 0
      langdvar = FALSE.
   END.        
   ELSE IF tidin4.B5 + ";" + tidin4.B6 = "Regionstation;Station" THEN DO:
      ASSIGN
      nat1 = 3
      nat2 = 3
      nat3 = 0
      nat4 = 0
      langdvar = FALSE.
   END.
   ELSE IF tidin4.B5 + ";" + tidin4.B6 = "Markstation;Nätstation" THEN DO:
      ASSIGN
      nat1 = 3
      nat2 = 7
      nat3 = 16
      nat4 = 0
      langdvar = FALSE.
   END.
   ELSE IF tidin4.B5 + ";" + tidin4.B6 = "Lastfr.sk. m. säkr. -utomhus;Lastfrånskiljare med säkring" THEN DO:
      ASSIGN
      nat1 = 4
      nat2 = 12
      nat3 = 29
      nat4 = 0   
      langdvar = FALSE.
   END.
   ELSE IF tidin4.B5 + ";" + tidin4.B6 = "Effektbrytare;App. i luftledn. och stolpstn." THEN DO:
      ASSIGN
      nat1 = 2
      nat2 = 1
      nat3 = 2
      nat4 = 1
      langdvar = FALSE.
   END.
   ELSE IF tidin4.B5 + ";" + tidin4.B6 = "Likriktarstation;Station" THEN DO:
      ASSIGN
      nat1 = 3
      nat2 = 6
      nat3 = 0
      nat4 = 0
      langdvar = FALSE.
   END.
   ELSE IF tidin4.B5 + ";" + tidin4.B6 = "-torrisolerad, solid;Krafttransformator" THEN DO:
      ASSIGN
      nat1 = 4
      nat2 = 10
      nat3 = 32
      nat4 = 0
      langdvar = FALSE.
   END.                                   
   ELSE IF tidin4.B5 + ";" + tidin4.B6 = "Kondensatorbatteri, serie;App. i luftledn. och stolpstn." THEN DO:
     ASSIGN
      nat1 = 2
      nat2 = 1
      nat3 = 2
      nat4 = 4
      langdvar = FALSE.
   END.                                                            
   ELSE IF tidin4.B5 + ";" + tidin4.B6 = "Kondensatorbatteri, shunt;App. i luftledn. och stolpstn." THEN DO:
      ASSIGN
      nat1 = 2
      nat2 = 1
      nat3 = 2
      nat4 = 5
      langdvar = FALSE.
   END.
   ELSE IF tidin4.B5 + ";" + tidin4.B6 = "Avgreningsskåp;Markledning" THEN DO:
      ASSIGN
      nat1 = 2
      nat2 = 2
      nat3 = 9
      nat4 = 0
      langdvar = FALSE.
   END.                              

   /*VARIANT 2*/

   ELSE IF tidin4.B5 + ";" + tidin4.B6 = "101;" THEN DO:
      ASSIGN
      nat1 = 4
      nat2 = 8
      nat3 = 20
      nat4 = 0
      langdvar = FALSE.
   END.
   ELSE IF tidin4.B5 + ";" + tidin4.B6 = "105;" THEN DO:
      ASSIGN
      nat1 = 4
      nat2 = 12
      nat3 = 27
      nat4 = 0
      langdvar = FALSE.
   END.
   ELSE IF tidin4.B5 + ";" + tidin4.B6 = "102;" THEN DO:
      ASSIGN
      nat1 = 4
      nat2 = 9
      nat3 = 21
      nat4 = 0
      langdvar = FALSE.
   END.   
   ELSE IF tidin4.B5 + ";" + tidin4.B6 = "100;" THEN DO:
      ASSIGN
      nat1 = 4
      nat2 = 8
      nat3 = 19
      nat4 = 0
      langdvar = FALSE.
   END.
   ELSE IF tidin4.B5 + ";" + tidin4.B6 = "104;" THEN DO:
      ASSIGN
      nat1 = 4
      nat2 = 12
      nat3 = 26
      nat4 = 0
      langdvar = FALSE.
   END.
   ELSE IF tidin4.B5 + ";" + tidin4.B6 = "106;" THEN DO:
      ASSIGN
      nat1 = 4
      nat2 = 12
      nat3 = 28
      nat4 = 0
      langdvar = FALSE.
   END.
   ELSE IF tidin4.B5 + ";" + tidin4.B6 = "110;" THEN DO:
      ASSIGN
      nat1 = 4
      nat2 = 10
      nat3 = 23
      nat4 = 0
      langdvar = FALSE.
   END.
   ELSE IF tidin4.B5 + ";" + tidin4.B6 = "111;" THEN DO:
      ASSIGN
      nat1 = 4
      nat2 = 10
      nat3 = 30
      nat4 = 0
      langdvar = FALSE.
   END.
   ELSE IF tidin4.B5 + ";" + tidin4.B6 = "112;" THEN DO:
      ASSIGN
      nat1 = 4
      nat2 = 10
      nat3 = 31
      nat4 = 0
      langdvar = FALSE.
   END.
   ELSE IF tidin4.B5 + ";" + tidin4.B6 = "115;" THEN DO:
      ASSIGN
      nat1 = 4
      nat2 = 11
      nat3 = 24
      nat4 = 0
      langdvar = FALSE.
   END.
   /*FINNS EJ
   ELSE IF tidin4.B5 + ";" + tidin4.B6 = "varav -PCB;Kondensatorbatterier" THEN DO:
      ASSIGN
      nat1 = 4
      nat2 = 11
      nat3 = 25
      nat4 = 0
      langdvar = FALSE.
   END.
   */
   ELSE IF tidin4.B5 + ";" + tidin4.B6 = "61;" THEN DO:
      ASSIGN
      nat1 = 2
      nat2 = 2
      nat3 = 3
      nat4 = 0
      langdvar = TRUE.
   END.
   /*FINNS EJ
   ELSE IF tidin4.B5 + ";" + tidin4.B6 = "Varav PEX;Markledning" THEN DO:
      ASSIGN
      nat1 = 2
      nat2 = 2
      nat3 = 4
      nat4 = 0
      langdvar = TRUE.
   END.
   */
   ELSE IF tidin4.B5 + ";" + tidin4.B6 = "20;" THEN DO:
      ASSIGN
      nat1 = 3
      nat2 = 4
      nat3 = 0
      nat4 = 0
      langdvar = FALSE.
   END.
   ELSE IF tidin4.B5 + ";" + tidin4.B6 = "51;" THEN DO:
      ASSIGN
      nat1 = 2
      nat2 = 1
      nat3 = 1
      nat4 = 0
      langdvar = TRUE.
   END.
   ELSE IF tidin4.B5 + ";" + tidin4.B6 = "62;" THEN DO:
      ASSIGN
      nat1 = 2
      nat2 = 2
      nat3 = 5
      nat4 = 0
      langdvar = TRUE.
   END.
   ELSE IF tidin4.B5 + ";" + tidin4.B6 = "30;" THEN DO:
      ASSIGN
      nat1 = 3
      nat2 = 5
      nat3 = 0
      nat4 = 0
      langdvar = FALSE.
   END.
   ELSE IF tidin4.B5 + ";" + tidin4.B6 = "40;" THEN DO:
      ASSIGN
      nat1 = 3
      nat2 = 7
      nat3 = 35  /*Nätstationer*/
      nat4 = 0
      langdvar = FALSE.
   END.
   ELSE IF tidin4.B5 + ";" + tidin4.B6 = "41;" THEN DO:
       ASSIGN
      nat1 = 3  /*stationer*/   /*motsvarar STORNATUPPKOPP.NATUPPLAGGID1   NATUPPLAGGID2  NATUPPLAGGID3  NATUPPLAGGID4*/ 
      nat2 = 7  /*SPÄNNING*/
      nat3 = 13  /*Stolpstationer*/
      nat4 = 0   /*brytare-frånskiljare*/ 
      langdvar = FALSE.

   END.
   ELSE IF tidin4.B5 + ";" + tidin4.B6 = "42;" THEN DO:
      ASSIGN
      nat1 = 3
      nat2 = 7
      nat3 = 11
      nat4 = 0
      langdvar = FALSE.
   END.
   ELSE IF tidin4.B5 + ";" + tidin4.B6 = "43;" THEN DO:
      ASSIGN
      nat1 = 3
      nat2 = 7
      nat3 = 12
      nat4 = 0
      langdvar = FALSE.
   END.
   ELSE IF tidin4.B5 + ";" + tidin4.B6 = "44;" THEN DO:
      ASSIGN
      nat1 = 3
      nat2 = 7
      nat3 = 14
      nat4 = 0
      langdvar = FALSE.
   END.
   ELSE IF tidin4.B5 + ";" + tidin4.B6 = "45;" THEN DO:
      ASSIGN
      nat1 = 3
      nat2 = 7
      nat3 = 15
      nat4 = 0
      langdvar = FALSE.
   END.
   ELSE IF tidin4.B5 + ";" + tidin4.B6 = "47;" THEN DO:
      ASSIGN
      nat1 = 3
      nat2 = 7
      nat3 = 17
      nat4 = 0
      langdvar = FALSE.
   END.   
   ELSE IF tidin4.B5 + ";" + tidin4.B6 = "48;" THEN DO:      
      ASSIGN
      nat1 = 3
      nat2 = 7
      nat3 = 18
      nat4 = 0
      langdvar = FALSE.
   END.
   ELSE IF tidin4.B5 + ";" + tidin4.B6 = "49;" THEN DO:
      ASSIGN
      nat1 = 3
      nat2 = 7
      nat3 = 34
      nat4 = 0
      langdvar = FALSE.
   END.   
   ELSE IF tidin4.B5 + ";" + tidin4.B6 = "53;" THEN DO:
      ASSIGN
      nat1 = 2
      nat2 = 1
      nat3 = 7
      nat4 = 0
      langdvar = TRUE.
   END.
   ELSE IF tidin4.B5 + ";" + tidin4.B6 = "54;" THEN DO:
      ASSIGN
      nat1 = 2
      nat2 = 1
      nat3 = 8
      nat4 = 0
      langdvar = TRUE.
   END.   
   ELSE IF tidin4.B5 + ";" + tidin4.B6 = "59;" THEN DO:      
      ASSIGN
      nat1 = 2
      nat2 = 1
      nat3 = 33
      nat4 = 0
      langdvar = FALSE.
   END.
   ELSE IF tidin4.B5 + ";" + tidin4.B6 = "55;" THEN DO:      
      ASSIGN
      nat1 = 2
      nat2 = 1
      nat3 = 35
      nat4 = 0
      langdvar = TRUE.
   END.   
   ELSE IF tidin4.B5 + ";" + tidin4.B6 = "122;" THEN DO:
      ASSIGN
      nat1 = 2
      nat2 = 1
      nat3 = 2
      nat4 = 3
      langdvar = FALSE.
   END.
   ELSE IF tidin4.B5 + ";" + tidin4.B6 = "121;" THEN DO:
      ASSIGN
      nat1 = 2
      nat2 = 1
      nat3 = 2
      nat4 = 2
      langdvar = FALSE.
   END.
   ELSE IF tidin4.B5 + ";" + tidin4.B6 = "103;" THEN DO:
      ASSIGN
      nat1 = 4
      nat2 = 9
      nat3 = 22
      nat4 = 0
      langdvar = FALSE.
   END.
   ELSE IF tidin4.B5 + ";" + tidin4.B6 = "52;" THEN DO:
      ASSIGN
      nat1 = 2
      nat2 = 1
      nat3 = 6
      nat4 = 0
      langdvar = TRUE.
   END.
   ELSE IF tidin4.B5 + ";" + tidin4.B6 = "68;" THEN DO:
      ASSIGN
      nat1 = 2
      nat2 = 2
      nat3 = 9
      nat4 = 0
      langdvar = FALSE.
   END.                  
   ELSE IF tidin4.B5 + ";" + tidin4.B6 = "69;" THEN DO:      
      ASSIGN
      nat1 = 2
      nat2 = 2
      nat3 = 10
      nat4 = 0
      langdvar = FALSE.
   END.        
   ELSE IF tidin4.B5 + ";" + tidin4.B6 = "10;" THEN DO:
      ASSIGN
      nat1 = 3
      nat2 = 3
      nat3 = 0
      nat4 = 0
      langdvar = FALSE.
   END.
   ELSE IF tidin4.B5 + ";" + tidin4.B6 = "46;" THEN DO:
      ASSIGN
      nat1 = 3
      nat2 = 7
      nat3 = 16
      nat4 = 0
      langdvar = FALSE.
   END.
   ELSE IF tidin4.B5 + ";" + tidin4.B6 = "107;" THEN DO:
      ASSIGN
      nat1 = 4
      nat2 = 12
      nat3 = 29
      nat4 = 0
      langdvar = FALSE.
   END.
   ELSE IF tidin4.B5 + ";" + tidin4.B6 = "120;" THEN DO:
      ASSIGN
      nat1 = 2
      nat2 = 1
      nat3 = 2
      nat4 = 1
      langdvar = FALSE.
   END.
   ELSE IF tidin4.B5 + ";" + tidin4.B6 = "35;" THEN DO:
      ASSIGN
      nat1 = 3
      nat2 = 6
      nat3 = 0
      nat4 = 0
      langdvar = FALSE.
   END.
   ELSE IF tidin4.B5 + ";" + tidin4.B6 = "113;" THEN DO:
      ASSIGN
      nat1 = 4
      nat2 = 10
      nat3 = 32
      nat4 = 0
      langdvar = FALSE.
   END.      
   ELSE IF tidin4.B5 + ";" + tidin4.B6 = "123;" THEN DO:
     ASSIGN
      nat1 = 2
      nat2 = 1
      nat3 = 2
      nat4 = 4
      langdvar = FALSE.
   END.
   ELSE IF tidin4.B5 + ";" + tidin4.B6 = "124;" THEN DO:
      ASSIGN
      nat1 = 2
      nat2 = 1
      nat3 = 2
      nat4 = 5
      langdvar = FALSE.
   END. 
   ELSE IF tidin4.B5 + ";" + tidin4.B6 = "128;" OR 
           tidin4.B5 + ";" + tidin4.B6 = "129;" OR
           tidin4.B5 + ";" + tidin4.B6 = "130;" OR
           tidin4.B5 + ";" + tidin4.B6 = "131;" OR
           tidin4.B5 + ";" + tidin4.B6 = "132;" OR
           tidin4.B5 + ";" + tidin4.B6 = "133;" OR
           tidin4.B5 + ";" + tidin4.B6 = "134;" OR
           tidin4.B5 + ";" + tidin4.B6 = "135;" OR
           tidin4.B5 + ";" + tidin4.B6 = "136;" OR
           tidin4.B5 + ";" + tidin4.B6 = "150;" OR
           tidin4.B5 + ";" + tidin4.B6 = "151;" 
   THEN DO:       
      FIND FIRST STORNATUPPKOPP WHERE STORNATUPPKOPP.TEXTKOMBID = INTEGER(tidin4.B5) NO-LOCK NO-ERROR.
      IF AVAILABLE STORNATUPPKOPP THEN DO:
         ASSIGN
         nat1 = STORNATUPPKOPP.NATUPPLAGGID1
         nat2 = STORNATUPPKOPP.NATUPPLAGGID2
         nat3 = STORNATUPPKOPP.NATUPPLAGGID3
         nat4 = STORNATUPPKOPP.NATUPPLAGGID4
         langdvar = FALSE.
      END.
      ELSE DO:
         ASSIGN
         ledfel = TRUE
         felvar = TRUE.
         CREATE temp_text.
         ASSIGN
         temp_text.PROGNAMN = "TEXT DATABASPOST SAKNAS" + STRING(tidin4.B1) + ";" + tidin4.B2 + ";" + STRING(tidin4.B3) + ";" +
         tidin4.B4 + ";" + tidin4.B5 + ";" + tidin4.B6 + ";"
         + STRING(tidin4.B7) + ";" + STRING(tidin4.B8) + ";" + STRING(tidin4.B9) + ";" + STRING(tidin4.B10).
      END.      
   END.                               
   ELSE DO:
      ASSIGN
      ledfel = TRUE
      felvar = TRUE.
      CREATE temp_text.
      ASSIGN
      temp_text.PROGNAMN = "TEXT SAKNAS" + STRING(tidin4.B1) + ";" + tidin4.B2 + ";" + STRING(tidin4.B3) + ";" +
      tidin4.B4 + ";" + tidin4.B5 + ";" + tidin4.B6 + ";"
      + STRING(tidin4.B7) + ";" + STRING(tidin4.B8) + ";" + STRING(tidin4.B9) + ";" + STRING(tidin4.B10).
   END.
END PROCEDURE.
