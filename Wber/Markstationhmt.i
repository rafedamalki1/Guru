
/*------------------------------------------------------------------------
    File        : Markstationhmt.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Wed Oct 31 16:13:09 CET 2018
    Notes       :
  ----------------------------------------------------------------------*/
PROCEDURE MarkstationBort_UI :
   DEFINE INPUT PARAMETER MarkStaonrvar AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER MarkStomradevar AS CHARACTER NO-UNDO.
   {MarkstationDefDyn.i}
   DO TRANSACTION:
      marksqh:GET-FIRST(EXCLUSIVE-LOCK).
      DO WHILE marksqh:QUERY-OFF-END = FALSE:
         bermarkstbuffh:BUFFER-DELETE(). 
         marksqh:GET-NEXT(EXCLUSIVE-LOCK).
      END.
   END.   
   DELETE WIDGET-POOL "MarkFriDynTable" NO-ERROR.
END PROCEDURE.

PROCEDURE Markstationhmt_UI :
   DEFINE INPUT PARAMETER MarkStaonrvar AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER MarkStomradevar AS CHARACTER NO-UNDO.
   {MarkstationDefDyn.i}
   EMPTY TEMP-TABLE markgrupp NO-ERROR. 
   marksqh:GET-FIRST(NO-LOCK).
   DO WHILE marksqh:QUERY-OFF-END = FALSE:
      CREATE markgrupp.
      BUFFER markgrupp:HANDLE:BUFFER-COPY(bermarkstbuffh).  
      marksqh:GET-NEXT(NO-LOCK).
   END.
   DELETE WIDGET-POOL "MarkFriDynTable" NO-ERROR.
   
END PROCEDURE.
PROCEDURE MarkstationKopi_UI :
   DEFINE INPUT PARAMETER MarkStaonrvar AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER MarkStomradevar AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER MarkStaonrvar2 AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER MarkStomradevar2 AS CHARACTER NO-UNDO.
   {MarkstationDefDyn.i}
   DEFINE VARIABLE bermarkstbuffh2 AS HANDLE NO-UNDO.
   CREATE BUFFER bermarkstbuffh2 FOR TABLE "MARKSTATIONIBEREDNING" IN WIDGET-POOL "MarkFriDynTable".
   marksqh:GET-FIRST(NO-LOCK).
   DO WHILE marksqh:QUERY-OFF-END = FALSE:
      DO TRANSACTION:
         bermarkstbuffh2:BUFFER-CREATE(). 
         bermarkstbuffh2:BUFFER-COPY(bermarkstbuffh).
         bermarkstbuffh2:BUFFER-FIELD("AONR"):BUFFER-VALUE = MarkStaonrvar2.
         bermarkstbuffh2:BUFFER-FIELD("OMRADE"):BUFFER-VALUE = MarkStomradevar2.
      END.     
      marksqh:GET-NEXT(NO-LOCK).
   END.
   DELETE WIDGET-POOL "MarkFriDynTable" NO-ERROR.
   
END PROCEDURE.
PROCEDURE CreateCustomQueryMarkST:
   DEFINE INPUT PARAMETER tth  AS HANDLE NO-UNDO.
   DEFINE INPUT PARAMETER q AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER CustomQueryh AS HANDLE NO-UNDO.
   CREATE QUERY CustomQueryh IN WIDGET-POOL "MarkFriDynTable".
   CustomQueryh:SET-BUFFERS(tth).
   CustomQueryh:QUERY-PREPARE(q).
   CustomQueryh:QUERY-OPEN().
END PROCEDURE.

