/*ANDMEDIN.I*/          

IF Guru.Konstanter:appcon THEN DO:                           
   RUN MEDSKAP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
   (INPUT TABLE meddpers,INPUT EDITOR_MEDD,INPUT Guru.Konstanter:globanv).

END.
ELSE DO:
   RUN MEDSKAP.P 
   (INPUT TABLE meddpers,INPUT EDITOR_MEDD,INPUT Guru.Konstanter:globanv).                  
END.
