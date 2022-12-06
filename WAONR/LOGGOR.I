/*LOGGOR.I*/
/*FOREBILDER*/

IF Guru.Konstanter:appcon THEN DO:
   RUN LOGGORIN.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT (INPUT Guru.Konstanter:globforetag, INPUT Guru.Konstanter:gurubilder,OUTPUT link).
END.
ELSE DO:
   RUN LOGGORIN.P (INPUT Guru.Konstanter:globforetag, INPUT Guru.Konstanter:gurubilder,OUTPUT link).
END.   
   
   
