   /*bef6.i*/
   /*KALKÅR*/
   IF FASTSPEC.EGETPRIS = TRUE THEN DO:
      prisvar = KALKBEF.PRIS6.
   END.
   ELSE DO:
      IF KALKBEF.EBR6 = 1 THEN prisvar = monpris.  /*EBRPRIS.MONT.*/
      ELSE IF KALKBEF.EBR6 = 2 THEN prisvar = EBRPRIS.MASK1.
      ELSE prisvar = EBRPRIS.MASK2.
   END.
   IF AVAILABLE FAKTOR AND FASTSPEC.FAKTOR = TRUE THEN DO: 
      faktvar = FAKTOR.FAK6.
      IF FAKTOR.FAK9 = 2 THEN faktvar = faktvar / 100.
   END.
   ELSE faktvar = 1.
