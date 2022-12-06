/*TIDBER.P*/
/* ************************  Function Implementations ***************** */

FUNCTION klockan100 RETURNS DECIMAL
  ( INPUT ber60 AS DECIMAL ):

  RETURN  (TRUNCATE(ber60,0) * 3600 + (ber60 - TRUNCATE(ber60,0)) * 100 * 60) / 3600.

END FUNCTION.


FUNCTION klockan60 RETURNS DECIMAL
  ( INPUT ber100 AS DECIMAL ):
  RETURN TRUNCATE(ber100,0) + ((ber100 - TRUNCATE(ber100,0)) / 100) * 60 . 

END FUNCTION.

DEFINE INPUT PARAMETER startdag AS DATE NO-UNDO.
DEFINE INPUT PARAMETER slutdag AS DATE NO-UNDO.
DEFINE INPUT PARAMETER starttid AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER sluttid AS DECIMAL NO-UNDO.  
DEFINE OUTPUT PARAMETER antaltimmar100 AS DECIMAL NO-UNDO.
DEFINE OUTPUT PARAMETER antaltimmar60 AS DECIMAL NO-UNDO.
IF starttid = 24 THEN starttid = 00.
IF sluttid = 00 THEN sluttid = 24.
IF slutdag = startdag THEN DO:
   RUN endagtid_UI.
   RETURN.
END.
RUN flerdag_UI.
PROCEDURE endagtid_UI: 
   antaltimmar100 = (klockan100(sluttid) - klockan100(starttid)).               
   antaltimmar60 = klockan60(antaltimmar100).
END PROCEDURE.
PROCEDURE flerdag_UI:
   antaltimmar100 = ((slutdag - startdag) - 1) * 24.
   antaltimmar100 = antaltimmar100 + (24 - klockan100(starttid)).
   antaltimmar100 = antaltimmar100 + klockan100(sluttid).            
   antaltimmar60 = klockan60(antaltimmar100).
END PROCEDURE.
