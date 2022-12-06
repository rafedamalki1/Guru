/*
               KSV Editor
   Filename: GETTABLEout.P
   
*/




{EKGKAT.I}


DEFINE INPUT-OUTPUT PARAMETER TABLE-HANDLE ekgArbkodJmfTTouth.

DEFINE VARIABLE ekgArbkodJmfTTh AS HANDLE NO-UNDO.



DEFINE VARIABLE queh AS HANDLE NO-UNDO.


ekgArbkodJmfTTh = ekgArbkodJmfTTouth:DEFAULT-BUFFER-HANDLE.

   
   CREATE BUFFER ekgArbkodJmfTTh FOR TABLE "ekgp1arbkodTT".
   ekgArbkodJmfTTouth = TEMP-TABLE ekgp1arbkodTT:HANDLE.
   

DEFINE VARIABLE orgbufh AS HANDLE NO-UNDO.
/*DEFINE VARIABLE orgtabch AS CHARACTER NO-UNDO.*/




MESSAGE ekgArbkodJmfTTh:TABLE
VIEW-AS ALERT-BOX.
MESSAGE ekgArbkodJmfTTouth:TABLE
VIEW-AS ALERT-BOX.
MESSAGE ekgArbkodJmfTTouth:name
VIEW-AS ALERT-BOX.
/*orgtabch = tth:NAME.*/

CREATE BUFFER orgbufh FOR TABLE "EKGP1ARBKOD". 
   CREATE QUERY queh.
   
   queh:SET-BUFFERS(orgbufh).
   queh:QUERY-PREPARE("FOR EACH " + orgbufh:TABLE + " WHERE EKGP1ARBKOD.EKGSUBID = 1 NO-LOCK.").
   queh:QUERY-OPEN().
   queh:GET-FIRST(NO-LOCK).
   DO WHILE queh:QUERY-OFF-END = FALSE:
      /* Kopierar all data från skarp- till temp-tabell*/
      ekgArbkodJmfTTh:BUFFER-CREATE().
      ekgArbkodJmfTTh:BUFFER-COPY(orgbufh).
      queh:GET-NEXT(NO-LOCK).
   END.
   queh:QUERY-CLOSE.
   
   DELETE OBJECT queh.
   DELETE OBJECT orgbufh.