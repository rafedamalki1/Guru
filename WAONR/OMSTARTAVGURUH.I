/*OMSTARTAVGURUH.I*/   
/*I HUVUDMENYN*/

Guru.Konstanter:Omstart(TRUE).
IF Guru.Konstanter:globanvbyt = {LOSENKOLLFEL10.I} THEN DO:
   APPLY "CLOSE" TO THIS-PROCEDURE.
   RETURN. 
END.   
