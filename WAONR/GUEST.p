/*GUEST.P.*/

IF TODAY >= 12/12/2003 THEN DO:
   MESSAGE "Er utv�rderingsperiod av GURU �r nu slut. Kontakta Elpool - 090/184540 f�r mer information."
   VIEW-AS ALERT-BOX.
   QUIT.
END.
ELSE DO:
   RUN GURUST.P.
   QUIT.
END.

