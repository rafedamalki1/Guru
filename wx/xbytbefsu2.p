/*xbytbefsu.p steg3*/
/*OUTPUT TO C:\NOLLPRBEFMAJ.TXT.*/
FOR EACH personaltab NO-LOCK :
    IF personaltab.befattning = "SEAB Teknik" OR personaltab.befattning = "SEAB Planering" OR personaltab.befattning = "SEAB N�tutbyggnad" OR
   personaltab.befattning = "SEAB N�tdrift" OR personaltab.befattning = "SEAB M�t" OR personaltab.befattning = "SEAB Milj�" OR
   personaltab.befattning = "SEAB Mek" OR personaltab.befattning = "SEAB Marknad" OR personaltab.befattning = "SEAB Larm o Signal" OR
   personaltab.befattning = "SEAB Kundservice" OR personaltab.befattning = "Farligt avfall" OR personaltab.befattning = "SEAB El" OR
   personaltab.befattning = "SEAB Drift" OR personaltab.befattning = "N�tdokumentatio" OR personaltab.befattning = "SEAB Data" OR

   personaltab.befattning = "SEAB Proj/ Upphandl" OR personaltab.befattning = "SEAB Prod gemensamt" OR personaltab.befattning = "SEAB Instrument"    
   OR personaltab.befattning = "Avfallsbehandling" OR personaltab.befattning = "SEAB Arbetsmilj�"   THEN DO:
       FIND FIRST PERSONALPRIS WHERE 
        PERSONALPRIS.PERSONALKOD = personaltab.PERSONALKOD AND
        PERSONALPRIS.BEFATTNING =  personaltab.befattning AND 
        PERSONALPRIS.STARTDATUM <= 01/01/2010  AND 
        PERSONALPRIS.SLUTDATUM >= 01/01/2010 NO-LOCK NO-ERROR.
        IF AVAILABLE PERSONALPRIS THEN DO:

           DO TRANSACTION:       
               FOR EACH TIDREGITAB WHERE TIDREGITAB.personalkod = personaltab.personalkod AND TIDREGITAB.DATUM GE 01/01/10 AND TIDREGITAB.DATUM LE 04/30/10 
               AND TIDREGITAB.veckokord NE "" AND TIDREGITAB.TIDLOG = TRUE AND tidregitab.overtidtill = personaltab.befattning EXCLUSIVE-LOCK:
                  IF TIDREGITAB.PRISTYP = "FR�NVARO." THEN.
                  ELSE IF TIDREGITAB.PRISTYP = "EJ.KOSTN." THEN.
                  ELSE IF TIDREGITAB.PRISTYP = "RESTID..." THEN.
                  ELSE IF tidregitab.pris NE PERSONALPRIS.pris THEN DO:
                     /*DISP TIDREGITAB.PERSONALKOD TIDREGITAB.DATUM TIDREGITAB.OVERTIDTILL FORMAT "X(12)"  TIDREGITAB.PRIS PERSONALPRIS.pris TIDREGITAB.AONR TIDREGITAB.TOTALT /*personaltab.befattning */.*/
                    ASSIGN tidregitab.pris = PERSONALPRIS.pris.
                  END.
                END.
           END.
        END.

   END.
END.
/*FOR EACH TIDREGITAB WHERE TIDREGITAB.DATUM GE 01/01/10 AND TIDREGITAB.DATUM LE 04/30/10 AND TIDREGITAB.veckokorw NE ""
AND TIDREGITAB.TIDLOG = TRUE NO-LOCK:
    IF TIDREGITAB.PRISTYP = "FR�NVARO." THEN.
    ELSE IF TIDREGITAB.PRISTYP = "EJ.KOSTN." THEN.
    ELSE IF TIDREGITAB.PRISTYP = "RESTID..." THEN.
    ELSE DO:
       IF TIDREGITAB.OVERTIDTILL = "SEAB Teknik" OR TIDREGITAB.OVERTIDTILL = "SEAB Planering" OR TIDREGITAB.OVERTIDTILL = "SEAB N�tutbyggnad" OR
       TIDREGITAB.OVERTIDTILL = "SEAB N�tdrift" OR TIDREGITAB.OVERTIDTILL = "SEAB M�t" OR TIDREGITAB.OVERTIDTILL = "SEAB Milj�" OR
       TIDREGITAB.OVERTIDTILL = "SEAB Mek" OR TIDREGITAB.OVERTIDTILL = "SEAB Marknad" OR TIDREGITAB.OVERTIDTILL = "SEAB Larm o Signal" OR
       TIDREGITAB.OVERTIDTILL = "SEAB Kundservice" OR TIDREGITAB.OVERTIDTILL = "SEAB Farligt avfall" OR TIDREGITAB.OVERTIDTILL = "SEAB El" OR
       TIDREGITAB.OVERTIDTILL = "SEAB Drift" OR TIDREGITAB.OVERTIDTILL = "SEAB Dokumentation" OR TIDREGITAB.OVERTIDTILL = "SEAB Data" THEN DO:
            FIND FIRST personaltab WHERE personaltab.personalkod = TIDREGITAB.PERSONALKOD NO-LOCK NO-ERROR.
            FIND FIRST PERSONALPRIS WHERE 
            PERSONALPRIS.PERSONALKOD = TIDREGITAB.PERSONALKOD AND
            PERSONALPRIS.BEFATTNING =  TIDREGITAB.OVERTIDTILL AND 
            PERSONALPRIS.STARTDATUM <= 05/01/2010  AND 
            PERSONALPRIS.SLUTDATUM >= 05/01/2010 NO-LOCK NO-ERROR.
            IF AVAILABLE PERSONALPRIS THEN DO:
               IF TIDREGITAB.PRIS NE PERSONALPRIS.PRIS  THEN DO:
                   DISP TIDREGITAB.PERSONALKOD TIDREGITAB.DATUM TIDREGITAB.OVERTIDTILL FORMAT "X(12)"  TIDREGITAB.PRIS PERSONALPRIS.PRIS /*TIDREGITAB.PRISTYP*/ TIDREGITAB.AONR TIDREGITAB.TOTALT personaltab.befattning .
               END.
            END.
       END.        
    END.
END.*/
