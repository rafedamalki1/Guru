   /*bef8.i*/
   /*KALK?R*/
   IF FASTSPEC.EGETPRIS = TRUE THEN DO:
      prisvar = KALKBEF.PRIS8.
   END.
   ELSE DO:
      IF KALKBEF.EBR8 = 1 THEN prisvar = monpris. /* EBRPRIS.MONT.*/
      ELSE IF KALKBEF.EBR8 = 2 THEN prisvar = EBRPRIS.MASK1.
      ELSE prisvar = EBRPRIS.MASK2.
   END.
   IF AVAILABLE FAKTOR AND FASTSPEC.FAKTOR = TRUE THEN DO: 
      faktvar = FAKTOR.FAK8.
      IF FAKTOR.FAK9 = 2 THEN faktvar = faktvar / 100.
   END.
   ELSE faktvar = 1.
