/*xbytbefsu.p steg1*/
/*OUTPUT TO C:\NOLLPRBEFMAJ.TXT.*/
FOR EACH personaltab NO-LOCK :
    IF personaltab.befattning = "SEAB Teknik" OR personaltab.befattning = "SEAB Planering" OR personaltab.befattning = "SEAB Nätutbyggnad" OR
   personaltab.befattning = "SEAB Nätdrift" OR personaltab.befattning = "SEAB Mät" OR personaltab.befattning = "SEAB Miljö" OR
   personaltab.befattning = "SEAB Mek" OR personaltab.befattning = "SEAB Marknad" OR personaltab.befattning = "SEAB Larm o Signal" OR
   personaltab.befattning = "SEAB Kundservice" OR personaltab.befattning = "Farligt avfall" OR personaltab.befattning = "SEAB El" OR
   personaltab.befattning = "SEAB Drift" OR personaltab.befattning = "SEAB Nätdokumentatio" OR personaltab.befattning = "SEAB Data" OR
   personaltab.befattning = "SEAB Proj/ Upphandl" OR personaltab.befattning = "SEAB Prod gemensamt" OR personaltab.befattning = "SEAB Instrument"    
   OR personaltab.befattning = "Avfallsbehandling" OR personaltab.befattning = "SEAB Arbetsmiljö"   THEN DO:
       DO TRANSACTION:       
           FOR EACH TIDREGITAB WHERE TIDREGITAB.personalkod = personaltab.personalkod AND TIDREGITAB.DATUM GE 01/01/10 AND TIDREGITAB.DATUM LE 04/30/10 
           AND TIDREGITAB.veckokord NE "" AND TIDREGITAB.TIDLOG = TRUE AND tidregitab.overtidtill NE personaltab.befattning EXCLUSIVE-LOCK:
               ASSIGN tidregitab.overtidtill = personaltab.befattning.
               
               /*DISP TIDREGITAB.PERSONALKOD TIDREGITAB.DATUM TIDREGITAB.OVERTIDTILL FORMAT "X(12)"  TIDREGITAB.PRIS /*TIDREGITAB.PRISTYP*/ TIDREGITAB.AONR TIDREGITAB.TOTALT personaltab.befattning .*/
      
           END.
       END.

   END.
END.
