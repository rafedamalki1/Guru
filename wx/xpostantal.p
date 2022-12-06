DEFINE VARIABLE kommandoquery AS CHARACTER NO-UNDO.
DEFINE VARIABLE kommandoorgfalt AS CHARACTER NO-UNDO.
DEFINE VARIABLE kommandonyfalt AS CHARACTER NO-UNDO.
DEFINE VARIABLE orginaltab AS CHARACTER NO-UNDO.
DEFINE VARIABLE jointab AS CHARACTER NO-UNDO.
DEFINE VARIABLE nytab      AS CHARACTER NO-UNDO.

DEFINE VARIABLE qh AS HANDLE NO-UNDO.
DEFINE VARIABLE orgtabh AS HANDLE NO-UNDO.
DEFINE VARIABLE jointabh AS HANDLE NO-UNDO.
DEFINE VARIABLE orgfalth AS HANDLE NO-UNDO.
DEFINE VARIABLE nyfalth AS HANDLE NO-UNDO.
DEFINE VARIABLE nytabh AS HANDLE NO-UNDO.
DEFINE VARIABLE extratemptabh AS HANDLE NO-UNDO.
DEFINE VARIABLE extrajointemptabh AS HANDLE NO-UNDO.
DEFINE VARIABLE aa AS INTEGER NO-UNDO.

FOR EACH _file :
   IF _file._FILE-NAME BEGINS "_" THEN NEXT.
   IF _file._FILE-NAME BEGINS "SYS" THEN NEXT.
   FIND _storageobject WHERE
   _storageobject._object-number = _file._file-number AND
   _storageobject._object-type = 1
   NO-LOCK NO-ERROR.
   FIND _area WHERE _area._area-number = _storageobject._area-number
   NO-LOCK NO-ERROR.
   IF _area._area-name BEGINS "sch" THEN DO:
      aa = 0.
      DISPLAY _area._area-name FORMAT "X(20)" _file._file-name FORMAT "X(20)" aa.
      orginaltab = _file._file-name.
      kommandoquery = "for each " + orginaltab + " no-lock".
      CREATE BUFFER orgtabh FOR TABLE orginaltab. 
      CREATE QUERY qh.
      qh:SET-BUFFERS(orgtabh).
      qh:QUERY-PREPARE(kommandoquery).   
      qh:QUERY-OPEN().
      qh:GET-FIRST(NO-LOCK).
      DO WHILE orgtabh:AVAILABLE:   
         aa = aa  + 1.
         qh:GET-NEXT(NO-LOCK).        
      END.
      qh:QUERY-CLOSE().
      DISPLAY _area._area-name FORMAT "X(20)" _file._file-name FORMAT "X(20)" aa.
   END.
END.
