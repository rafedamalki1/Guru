/*TIDKOLW2.I*/
PROCEDURE tidkoll2_UI.
   CREATE felmeddtemp.                 
   FOR EACH TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = pkod AND TIDREGItAB.DATUM = regdatum AND
   TIDREGITAB.TIDLOG = TRUE NO-LOCK:
      felmeddtemp.felmedd = felmeddtemp.felmedd + STRING(TIDREGITAB.START) + " " + STRING(TIDREGITAB.SLUT)  + " " + STRING(TIDREGITAB.AONR)  + " " + STRING(TIDREGITAB.DELNR)
      + " " + STRING(TIDREGITAB.TRAKTAMENTE) + " "  + CHR(13).
   END.
   IF felmeddtemp.felmedd = "" THEN DELETE felmeddtemp.

END PROCEDURE.

