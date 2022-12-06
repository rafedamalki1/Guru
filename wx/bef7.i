   /*bef7.i*/
   /*KALKÅR*/
   IF FASTSPEC.EGETPRIS = TRUE THEN DO:
      prisvar = KALKBEF.PRIS7.
   END.
   ELSE DO:
      IF KALKBEF.EBR7 = 1 THEN prisvar = monpris. /* EBRPRIS.MONT.*/
      ELSE IF KALKBEF.EBR7 = 2 THEN prisvar = EBRPRIS.MASK1.
      ELSE IF KALKBEF.EBR7 = 3 THEN prisvar = EBRPRIS.MASK2.
      ELSE IF KALKBEF.EBR7 = 4 THEN prisvar = emask3.
      /*MASKIN3*/
   END.
   IF AVAILABLE FAKTOR AND FASTSPEC.FAKTOR = TRUE THEN DO: 
      faktvar = FAKTOR.FAK7.
      IF FAKTOR.FAK9 = 2 THEN faktvar = faktvar / 100.
   END.
   ELSE faktvar = 1.
