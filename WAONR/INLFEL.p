/*INLFEL.P*/
DEFINE INPUT PARAMETER globanvnt LIKE ANVANDARE.ANVANDARE NO-UNDO.
DEFINE INPUT PARAMETER globanvguru LIKE ANVANDARE.ANVANDARE NO-UNDO.


                              


DEFINE VARIABLE kommando AS CHARACTER NO-UNDO.
{AMERICANEUROPEAN.I}
kommando = ?.
FIND FIRST FORETAG NO-LOCK NO-ERROR.
Guru.Konstanter:globforetag = FORETAG.FORETAG.
IF Guru.Konstanter:globforetag = "ELPA" THEN kommando = "\\pc112\DELAD\PRO9S\DB\INFELOG.TXT".
ELSE IF Guru.Konstanter:globforetag = "BORL" THEN kommando = "c:\DELAD\PRO9S\DB\INFELOG.TXT".
ELSE IF Guru.Konstanter:globforetag = "UMEA" THEN kommando = "D:\DELAD\PRO9S\DB\INFELOG.TXT".
ELSE IF Guru.Konstanter:globforetag = "GRIT" OR 
        Guru.Konstanter:globforetag = "GADM"  OR Guru.Konstanter:globforetag = "GKAL"  THEN DO:
   kommando = "d:\DELAD\server\PRO9S\DB\INFELOG.TXT". 
END.
ELSE DO:    
   kommando = SEARCH("INFELOG.TXT").  /*statistik*/   
END.   
IF kommando = ? THEN DO: 
   RUN PROVAG.P.
   kommando = Guru.Konstanter:guruvar + "INFELOG.TXT".
END.
RUN ut_UI.
{EUROPEANAMERICAN.I}    
RETURN.

PROCEDURE ut_UI :
   OUTPUT TO VALUE(kommando) APPEND.
   PUT UNFORMATTED globanvnt " NT-LOG " globanvguru " GURU-LOGG " Guru.Konstanter:globforetag " FÖRETAG" TODAY SKIP.
   /*globanvnt FORMAT "x(30)"  " NT-LOG " globanvnt FORMAT "x(30)" " GURU-LOGG" SKIP.*/
   OUTPUT CLOSE.   
END PROCEDURE.

