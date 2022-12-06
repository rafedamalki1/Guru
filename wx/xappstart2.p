/*XAPPSTART2.P*/   


DEFINE new SHARED VARIABLE appcon AS LOGICAL NO-UNDO.

DEFINE new shared VARIABLE globforetag AS CHARACTER NO-UNDO.    /*DEN VAR SKA EJ VARA DELAD*/
DEFINE VARIABLE samvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE vkdatum AS DATE NO-UNDO.
DEFINE VARIABLE vknummer AS CHARACTER NO-UNDO.
FIND FIRST FORETAG USE-INDEX FORETAG NO-LOCK NO-ERROR.
globforetag = FORETAG.FORETAG.
{CONAPP.I}
  vkdatum = 07/04/2007.
vknummer = "w20070704".
samvar = "D:\DELAD\SERVER\PRO9S\SULESAMM.TXT".        

/*vknummer = "FLEX".*/

RUN XSPSUNDEKO.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
   (INPUT samvar,INPUT vkdatum,INPUT vknummer).                                                           

/*RUN SUFEEKO.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT samvar,INPUT vkdatum,INPUT vknummer).           */

IF Guru.Konstanter:appcon THEN Guru.Konstanter:appcon = Guru.Konstanter:apphand:DISCONNECT().
DELETE OBJECT Guru.Konstanter:apphand NO-ERROR.
     

