/*SOKANROP.I*/
IF Guru.Konstanter:appcon THEN DO:                           
   RUN SOKPROG.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
   (INPUT-OUTPUT TABLE soktemp).
END.
ELSE DO:
   RUN SOKPROG.P 
   (INPUT-OUTPUT TABLE soktemp).
END.
FIND FIRST soktemp NO-ERROR.

