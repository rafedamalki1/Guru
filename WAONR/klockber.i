/*KLOCKBER.I*/

FUNCTION klockan100 RETURNS DECIMAL
  ( INPUT ber60 AS DECIMAL ):
  RETURN  (TRUNCATE(ber60,0) * 3600 + (ber60 - TRUNCATE(ber60,0)) * 100 * 60) / 3600.

END FUNCTION.

FUNCTION klockan60 RETURNS DECIMAL 
  ( INPUT ber100 AS DECIMAL  ):
   DEFINE VARIABLE tidhj AS DECIMAL NO-UNDO.
   DEFINE VARIABLE tidhj2 AS INTEGER NO-UNDO.
   tidhj = TRUNCATE(ber100,0) + ((ber100 - TRUNCATE(ber100,0)) / 100) * 60 . 
   tidhj2 = INDEX(STRING(tidhj),".").
   /*fixad för fallet där tiden blir 11.59677 som avrundades till 11.60*/
   IF tidhj2 > 0 THEN DO:          
      IF SUBSTRING(STRING(tidhj),tidhj2 + 1) > "594" THEN tidhj = ROUND(tidhj,0).
   END.
  RETURN tidhj.

END FUNCTION.

/*
FUNCTION klockan60 RETURNS DECIMAL
  ( INPUT ber100 AS DECIMAL ):
  RETURN TRUNCATE(ber100,0) + ((ber100 - TRUNCATE(ber100,0)) / 100) * 60 . 

END FUNCTION.*/

