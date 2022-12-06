OUTPUT TO C:\GURU\KALLE.TXT.
for each personaltab where integer(personaltab.personalkod) < 1000 no-lock:
   FOR each tidregitab where tidregitab.personalkod = personaltab.personalkod and
   tidregitab.beredskap begins "42" no-lock break by tidregitab.personalkod.
   ACCUMULATE TIDREGITAB.BERANTAL (TOTAL BY TIDREGITAB.PERSONALKOD).
      IF LAST-OF(TIDREGITAB.PERSONALKOD) THEN DO:
        PUT TIDREGITAB.PERSONALKOD (ACCUM TOTAL BY TIDREGITAB.PERSONALKOD TIDREGITAB.BERANTAL)  SKIP.
      END.  
   END.
END.        
