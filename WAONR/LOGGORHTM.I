
/*------------------------------------------------------------------------
    File        : LOGGORHTM.I
 
*/
DEFINE VARIABLE webblink AS CHARACTER NO-UNDO.
IF Guru.Konstanter:appcon THEN DO:
   
   RUN LOGGHTM.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT (INPUT Guru.Konstanter:globforetag,INPUT Guru.Konstanter:gurubilder,OUTPUT namnvar, OUTPUT hogvar,OUTPUT breddvar, OUTPUT logga).
END.
ELSE DO:
   
   RUN LOGGHTM.P (INPUT Guru.Konstanter:globforetag,INPUT Guru.Konstanter:gurubilder,OUTPUT namnvar, OUTPUT hogvar,OUTPUT breddvar, OUTPUT logga).
END.   
{UTHTMLBILD.I}

