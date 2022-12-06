/*
               KSV Editor
    Copyright: (C) 2000-2003 Serguey Klimoff (bulkl0DD)
     Filename: GETTABLE.P
      Comment: <comment>
   Parameters:
         Uses:
      Used by:
      Created: 2008.11.03 12:15 ELPAO   
     Modified: 2008.11.06 13:05 ELPAO    
     Modified: 
     
     
     KÖR EJ PERSISTENT!
*/



/*DEFINE INPUT PARAMETER TABLE FOR tabellin.*/

DEFINE INPUT-OUTPUT PARAMETER TABLE-HANDLE tth.
DEFINE INPUT PARAMETER orgtabch AS CHARACTER NO-UNDO.

DEFINE VARIABLE tempbufh AS HANDLE NO-UNDO.


DEFINE VARIABLE orgbufh AS HANDLE NO-UNDO.
/*DEFINE VARIABLE orgtabch AS CHARACTER NO-UNDO.*/

DEFINE VARIABLE queh AS HANDLE NO-UNDO.


tempbufh = tth:DEFAULT-BUFFER-HANDLE.
/*orgtabch = tth:NAME.*/

CREATE BUFFER orgbufh FOR TABLE orgtabch. 
   CREATE QUERY queh.
    
   /* Kollar så att table-handle inte hör till en skarp tabell */
   IF tempbufh:DBNAME NE "PROGRESST" THEN RETURN.
   /*
   tempbufh:EMPTY-TEMP-TABLE ( ) NO-ERROR.
   */
   /* Kopplar Queryn till temp-tabellens buffer */
   queh:SET-BUFFERS(tempbufh).
   /*kommandoquery = "FOR EACH " + tempbufh:TABLE + " NO-LOCK.".*/
   queh:QUERY-PREPARE("FOR EACH " + tempbufh:TABLE + " NO-LOCK.").
   queh:QUERY-OPEN().
   queh:GET-FIRST(NO-LOCK).
   DO WHILE queh:QUERY-OFF-END = FALSE:
      /* Tar bort alla poster som finns i temp-tabellen */
      DO TRANSACTION:
         queh:GET-CURRENT(EXCLUSIVE-LOCK).
         tempbufh:BUFFER-DELETE().
      END.
      queh:GET-NEXT(NO-LOCK).
   END.
   queh:QUERY-CLOSE.
   /* Kopplar queryn till den skarpa tabellens buffer */
   queh:SET-BUFFERS(orgbufh).
   queh:QUERY-PREPARE("FOR EACH " + orgbufh:TABLE + " NO-LOCK.").
   queh:QUERY-OPEN().
   queh:GET-FIRST(NO-LOCK).
   DO WHILE queh:QUERY-OFF-END = FALSE:
      /* Kopierar all data från skarp- till temp-tabell*/
      tempbufh:BUFFER-CREATE().
      tempbufh:BUFFER-COPY(orgbufh).
      queh:GET-NEXT(NO-LOCK).
   END.
   queh:QUERY-CLOSE.
   DELETE OBJECT tth.
   DELETE OBJECT queh.
   DELETE OBJECT orgbufh.
