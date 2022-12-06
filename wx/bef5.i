   /*bef5.i*/
   /*KALKÅR*/
   IF FASTSPEC.EGETPRIS = TRUE THEN DO:
      prisvar = KALKBEF.PRIS5.
   END.
   ELSE DO:
      IF KALKBEF.EBR5 = 1 THEN prisvar = monpris. /* EBRPRIS.MONT.*/
      ELSE IF KALKBEF.EBR5 = 2 THEN prisvar = EBRPRIS.MASK1.
      ELSE IF KALKBEF.EBR5 = 3 THEN prisvar = EBRPRIS.MASK2.     
   END.
   IF AVAILABLE FAKTOR AND FASTSPEC.FAKTOR = TRUE THEN DO: 
      faktvar = FAKTOR.FAK5.
      IF FAKTOR.FAK9 = 2 THEN faktvar = faktvar / 100.
   END.
   ELSE faktvar = 1.
