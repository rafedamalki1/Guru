   /*bef9.i*/
   /*KALKÅR*/
   /* 4 decimaler på faktorer om FAK9 = 2
   IF FASTSPEC.EGETPRIS = TRUE THEN DO:
      prisvar = KALKBEF.PRIS9.
   END.
   ELSE DO:
      IF KALKBEF.EBR9 = 1 THEN prisvar = monpris.  /*EBRPRIS.MONT.*/
      ELSE IF KALKBEF.EBR9 = 2 THEN prisvar = EBRPRIS.MASK1.
      ELSE prisvar = EBRPRIS.MASK2.
   END.
   IF FASTSPEC.FAKTOR = TRUE THEN faktvar = FAKTOR.FAK9.
   ELSE faktvar = 1.*/
