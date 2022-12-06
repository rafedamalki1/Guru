/*INKKOLL1.I*/
{INKKOLL0.I}
IF Guru.Konstanter:appcon = TRUE THEN DO:
   RUN FINNSTABELL.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT (INPUT "INKBER", OUTPUT inkopkollvar).      
END.
ELSE DO:
   {INKKOLL2.I}
END.