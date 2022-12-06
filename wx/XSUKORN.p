/*XSUKORN.P. STANSKÖRNING sundsvall*/
DEFINE NEW SHARED VARIABLE vkdatum AS DATE NO-UNDO.
DEFINE NEW SHARED VARIABLE globanv LIKE ANVANDARE.ANVANDARE NO-UNDO.
DEFINE NEW SHARED VARIABLE globforetag LIKE FORETAG.FORETAG NO-UNDO.

DEFINE NEW SHARED VARIABLE appcon AS LOGICAL NO-UNDO.

DEFINE VARIABLE samvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE korvar AS CHARACTER  NO-UNDO.
&Scoped-define NEW   
&Scoped-define SHARED 
{LONEDEF.I}

DEFINE NEW SHARED TEMP-TABLE tidut
   FIELD UT AS CHARACTER FORMAT "X(132)".
{CONAPP.I}

FIND FIRST FORETAG USE-INDEX FORETAG NO-LOCK NO-ERROR.
globforetag = FORETAG.FORETAG.




korvar = "".
globanv = CHR(69) + CHR(76) + CHR(80) + CHR(65) + CHR(79).
vkdatum = 10/31/2006.
samvar = "\\BEREDNING1\DELAD\SERVER\PRO9S\SULESAMM.TXT".        
RUN LESAMMAN.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
   (INPUT 1,INPUT samvar,OUTPUT TABLE tidut).

   RUN SUTILLN1.P  ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
   (INPUT samvar,INPUT vkdatum, INPUT Guru.Konstanter:globanv, INPUT "",OUTPUT TABLE lonefil).

   
   RUN xSUFRN1.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
   (INPUT samvar,INPUT vkdatum, INPUT Guru.Konstanter:globanv,INPUT "", INPUT TABLE lonefil).      

/*IF Guru.Konstanter:appcon THEN DO:
   RUN SUNDEKO.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT (INPUT samvar,INPUT vkdatum,INPUT korvar).                                                  
   RUN SUFEEKO.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT (INPUT samvar,INPUT vkdatum,INPUT korvar).           
END.*/

IF Guru.Konstanter:appcon THEN Guru.Konstanter:appcon = Guru.Konstanter:apphand:DISCONNECT().
DELETE OBJECT Guru.Konstanter:apphand.
appcon = FALSE.
