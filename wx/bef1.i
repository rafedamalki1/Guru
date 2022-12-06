   /*bef1.i*/
   /*KALKÅR*/
IF FASTSPEC.EGETPRIS = TRUE THEN DO:
   prisvar = KALKBEF.PRIS1.
END.
ELSE DO:
   IF KALKBEF.EBR1 = 1 THEN DO:
      /*region*/
      IF globforetag = "gran" THEN DO:
         IF EBRPRIS.ARTAL < 2013 THEN berpris = monpris.
      END.
      ELSE IF EBRPRIS.ARTAL < 2012 THEN berpris = monpris.                    
      IF region = TRUE THEN prisvar = berpris.
      ELSE prisvar = monpris.
      /*prisvar = monpris. /*EBRPRIS.MONT.*/*/
   END.    
   ELSE IF KALKBEF.EBR1 = 2 THEN prisvar = EBRPRIS.MASK1.
   ELSE IF KALKBEF.EBR1 = 3 THEN prisvar = EBRPRIS.MASK2.
 
END.
IF AVAILABLE FAKTOR AND  FASTSPEC.FAKTOR = TRUE THEN DO: 
   faktvar = FAKTOR.FAK1.
   IF FAKTOR.FAK9 = 2 THEN faktvar = faktvar / 100.
END.
ELSE faktvar = 1.
