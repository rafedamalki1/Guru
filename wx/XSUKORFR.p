
/*XSUKOR.P. STANSKÖRNING sundsvall*/
DEFINE NEW SHARED VARIABLE vkdatum AS DATE NO-UNDO.
DEFINE NEW SHARED VARIABLE globanv LIKE ANVANDARE.ANVANDARE NO-UNDO.
DEFINE NEW SHARED VARIABLE globforetag LIKE FORETAG.FORETAG NO-UNDO.

DEFINE NEW SHARED VARIABLE appcon AS LOGICAL NO-UNDO.

DEFINE VARIABLE samvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE korvar AS CHARACTER  NO-UNDO.
DEFINE NEW SHARED TEMP-TABLE tidut
   FIELD UT AS CHARACTER FORMAT "X(132)".
/*{CONAPP.I}*/

FIND FIRST FORETAG USE-INDEX FORETAG NO-LOCK NO-ERROR.
globforetag = FORETAG.FORETAG.




/*korvar = "W20160607".
globanv = "ELPAO".
vkdatum = 05/31/2016.*/
korvar = "W20190702".
globanv = "ELPAO".
vkdatum = 06/30/2019.


/*samvar = "C:\DELAD\PRO10S\SNATBERGET\SULESAMM.TXT".*/
samvar = "d:\DELAD\SERVER\PRO10S\SULESAMM.TXT".        
/*RUN LESAMMAN.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
   (INPUT 1,INPUT samvar,OUTPUT TABLE tidut).*/
 RUN SULOHOGIA.P (INPUT samvar,INPUT vkdatum, INPUT Guru.Konstanter:globanv, INPUT korvar).
/* RUN SUFRHOGIA.P (INPUT samvar,INPUT vkdatum, INPUT Guru.Konstanter:globanv,INPUT korvar).*/     



/*RUN SUNDEKO.P (INPUT samvar,INPUT vkdatum,INPUT korvar).                                                  
RUN SUFEEKO.P (INPUT samvar,INPUT vkdatum,INPUT korvar).


IF Guru.Konstanter:appcon THEN DO:
   RUN SUNDEKO.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT (INPUT samvar,INPUT vkdatum,INPUT korvar).                                                  
   RUN SUFEEKO.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT (INPUT samvar,INPUT vkdatum,INPUT korvar).           
END.

IF Guru.Konstanter:appcon THEN Guru.Konstanter:appcon = Guru.Konstanter:apphand:DISCONNECT().
DELETE OBJECT Guru.Konstanter:apphand.
appcon = FALSE.*/
=======
/*XSUKOR.P. STANSKÖRNING sundsvall*/
DEFINE NEW SHARED VARIABLE vkdatum AS DATE NO-UNDO.
DEFINE NEW SHARED VARIABLE globanv LIKE ANVANDARE.ANVANDARE NO-UNDO.
DEFINE NEW SHARED VARIABLE globforetag LIKE FORETAG.FORETAG NO-UNDO.

DEFINE NEW SHARED VARIABLE appcon AS LOGICAL NO-UNDO.

DEFINE VARIABLE samvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE korvar AS CHARACTER  NO-UNDO.
DEFINE NEW SHARED TEMP-TABLE tidut
   FIELD UT AS CHARACTER FORMAT "X(132)".
/*{CONAPP.I}*/

FIND FIRST FORETAG USE-INDEX FORETAG NO-LOCK NO-ERROR.
globforetag = FORETAG.FORETAG.




/*korvar = "W20160607".
globanv = "ELPAO".
vkdatum = 05/31/2016.*/
korvar = "W20190702".
globanv = "ELPAO".
vkdatum = 06/30/2019.


/*samvar = "C:\DELAD\PRO10S\SNATBERGET\SULESAMM.TXT".*/
samvar = "d:\DELAD\SERVER\PRO10S\SULESAMM.TXT".        
/*RUN LESAMMAN.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
   (INPUT 1,INPUT samvar,OUTPUT TABLE tidut).*/
 RUN SULOHOGIA.P (INPUT samvar,INPUT vkdatum, INPUT Guru.Konstanter:globanv, INPUT korvar).
/* RUN SUFRHOGIA.P (INPUT samvar,INPUT vkdatum, INPUT Guru.Konstanter:globanv,INPUT korvar).*/     



/*RUN SUNDEKO.P (INPUT samvar,INPUT vkdatum,INPUT korvar).                                                  
RUN SUFEEKO.P (INPUT samvar,INPUT vkdatum,INPUT korvar).


IF Guru.Konstanter:appcon THEN DO:
   RUN SUNDEKO.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT (INPUT samvar,INPUT vkdatum,INPUT korvar).                                                  
   RUN SUFEEKO.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT (INPUT samvar,INPUT vkdatum,INPUT korvar).           
END.

IF Guru.Konstanter:appcon THEN Guru.Konstanter:appcon = Guru.Konstanter:apphand:DISCONNECT().
DELETE OBJECT Guru.Konstanter:apphand.
appcon = FALSE.*/
>>>>>>> branch 'master' of file:///\\server05\delad\REMOTEGURU\GuruRemote.git
