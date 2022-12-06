/*xapp2.p*/   
/*WSTARTAP.P*/

DEFINE new SHARED VARIABLE appcon AS LOGICAL NO-UNDO.

DEFINE new shared VARIABLE globforetag AS CHARACTER NO-UNDO.    /*DEN VAR SKA EJ VARA DELAD*/
FIND FIRST FORETAG USE-INDEX FORETAG NO-LOCK NO-ERROR.
globforetag = FORETAG.FORETAG.
{CONAPP.I}
   
                                                    
      RUN d:\delad\pro9\guru\wx\xhej.p ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
         
IF Guru.Konstanter:appcon THEN Guru.Konstanter:appcon = Guru.Konstanter:apphand:DISCONNECT().
DELETE OBJECT Guru.Konstanter:apphand.
     

