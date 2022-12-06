/*xappstart.p*/   


DEFINE new SHARED VARIABLE appcon AS LOGICAL NO-UNDO.

DEFINE new shared VARIABLE globforetag AS CHARACTER NO-UNDO.    /*DEN VAR SKA EJ VARA DELAD*/
DEFINE VARIABLE samvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE vkdatum AS DATE NO-UNDO.
DEFINE VARIABLE vknummer AS CHARACTER format "x(10)" NO-UNDO.
FIND FIRST FORETAG USE-INDEX FORETAG NO-LOCK NO-ERROR.
globforetag = FORETAG.FORETAG.
/*
{CONAPP.I}
*/
DEFINE TEMP-TABLE wtt NO-UNDO
  FIELD KORa AS INTEGER
  FIELD SLUT AS DATE
  INDEX KORA KORA.
       
DEFINE VARIABLE WNR AS INTEGER NO-UNDO.
WNR = 20160110.
REPEAT: 
   FIND FIRST TIDREGITAB WHERE tidregitab.datum > 01/01/16 AND INTEGER(substring(TIDREGITAB.VECKOKORD,2)) > WNR  NO-LOCK NO-ERROR.
   IF AVAILABLE TIDREGITAB THEN DO:
      CREATE wtt.
      WTT.KORa =  INTEGER(substring(TIDREGITAB.VECKOKORD,2)).
      WTT.SLUT = DATE(MONTH(TIDREGITAB.DATUM) + 1,1,year(TIDREGITAB.DATUM)) - 1.
      WNR = WTT.KORa.
   END.
   ELSE LEAVE.
END.   
/*
w20160504

  
  /*vknummer = "w2010".*/
/*vkdatum = 11/30/2011.  
vknummer = "w20111202".*/
/*vkdatum = 05/31/2010.  
vknummer = "w20100603".*/ 
  */  
  
FOR EACH wtt WHERE NO-LOCK:
   
   samvar = "d:\DELAD\SERVER\PRO10S\SULESAMM.TXT".
   vkdatum = WTT.SLUT.  
   vknummer = "w" + STRING(WTT.KORa).
    DISPLAY vkdatum  vknummer.
   RUN SUNDEKO.P 
   (INPUT samvar,INPUT vkdatum,INPUT vknummer).                                                           

   RUN SUFEEKO.P  
    (INPUT samvar,INPUT vkdatum,INPUT vknummer).           
  
  
  
END.           


          




IF Guru.Konstanter:appcon THEN Guru.Konstanter:appcon = Guru.Konstanter:apphand:DISCONNECT().
DELETE OBJECT Guru.Konstanter:apphand NO-ERROR.
     

