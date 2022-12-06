/*XSUF0.P*/   


DEFINE new SHARED VARIABLE appcon AS LOGICAL NO-UNDO.

DEFINE new shared VARIABLE globforetag AS CHARACTER NO-UNDO.    /*DEN VAR SKA EJ VARA DELAD*/
DEFINE VARIABLE samvar AS CHARACTER NO-UNDO.
FIND FIRST FORETAG USE-INDEX FORETAG NO-LOCK NO-ERROR.
globforetag = FORETAG.FORETAG.
{CONAPP.I}
samvar = "D:\DELAD\SERVER\PRO9S\SULESAMM.TXT".           
RUN XXSUFEEKO.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT samvar,INPUT TODAY).                                                               

         
IF Guru.Konstanter:appcon THEN Guru.Konstanter:appcon = Guru.Konstanter:apphand:DISCONNECT().
DELETE OBJECT Guru.Konstanter:apphand NO-ERROR.
     

