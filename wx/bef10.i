   /*bef10.i*/
   /*KALKÅR*/
   /*KALKmatris
   IF FASTSPEC.EGETPRIS = TRUE THEN DO:
      prisvar = KALKBEF.PRIS10.
   END.
   ELSE DO:
      IF KALKBEF.EBR10 = 1 THEN prisvar = monpris. /* EBRPRIS.MONT.*/
      ELSE IF KALKBEF.EBR10 = 2 THEN prisvar = EBRPRIS.MASK1.
      ELSE prisvar = EBRPRIS.MASK2.
   END.
   IF FASTSPEC.FAKTOR = TRUE THEN faktvar = FAKTOR.FAK10.

   ELSE faktvar = 1.
   */
