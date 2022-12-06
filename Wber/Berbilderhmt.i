
/*------------------------------------------------------------------------
    File        : Berbilderhmt.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Wed Oct 31 16:13:09 CET 2018
    Notes       :
  ----------------------------------------------------------------------*/

PROCEDURE BerbildBort_UI :
   DEFINE INPUT PARAMETER Berbildbernr AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER Berbildomradevar AS CHARACTER NO-UNDO.
   {BerbildDefDyn.i}
   DO TRANSACTION:
      berbildqh:GET-FIRST(EXCLUSIVE-LOCK).
      DO WHILE berbildqh:QUERY-OFF-END = FALSE:
         berbildbuffh:BUFFER-DELETE(). 
         berbildqh:GET-NEXT(EXCLUSIVE-LOCK).
      END.
   END.   
   DELETE WIDGET-POOL "BerbildFriDynTable" NO-ERROR.
END PROCEDURE.

PROCEDURE BildiBeredhmt_UI :
   DEFINE INPUT PARAMETER Berbildbernr AS INTEGER  NO-UNDO.
   DEFINE INPUT PARAMETER Berbildomradevar AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER gf AS CHARACTER NO-UNDO.
   DEFINE VARIABLE bsok AS CHARACTER NO-UNDO.
   DEFINE VARIABLE diranv AS CHARACTER NO-UNDO.
   {BerbildDefDyn.i}
   EMPTY TEMP-TABLE bildbertemp NO-ERROR. 
   berbildqh:GET-FIRST(NO-LOCK).
   DO WHILE berbildqh:QUERY-OFF-END = FALSE:
      CREATE bildbertemp.
      BUFFER bildbertemp:HANDLE:BUFFER-COPY(berbildbuffh).  
      berbildqh:GET-NEXT(NO-LOCK).
      IF gf = "KRAF" THEN DO:
         IF bildbertemp.FILNAMN BEGINS "c:\users\" THEN DO:
            diranv = Guru.Konstanter:globanv.
            {MOLNETMAPPEXTRA.I}
            bsok = SUBSTRING(bildbertemp.FILNAMN,10,INDEX(bildbertemp.FILNAMN,"\",10) - 10).
            bildbertemp.FILNAMN = REPLACE(bildbertemp.FILNAMN,bsok,diranv).  
         END.   
      END.
   END.
   DELETE WIDGET-POOL "BerbildFriDynTable" NO-ERROR.
   
END PROCEDURE.
PROCEDURE Bildkombhmt_UI :
   DEFINE INPUT PARAMETER Berbildbernr AS INTEGER  NO-UNDO.
   DEFINE INPUT PARAMETER Berbildomradevar AS CHARACTER NO-UNDO.
   {BerbildDefDyn.i}
   
   berbildqh:GET-FIRST(NO-LOCK).
   DO WHILE berbildqh:QUERY-OFF-END = FALSE:
      FIND FIRST bildkomb WHERE bildkomb.NUM = berbildbuffh:BUFFER-FIELD("NUM"):BUFFER-VALUE NO-LOCK NO-ERROR.
      IF NOT AVAILABLE bildkomb THEN DO:
         CREATE bildkomb.
      END.
      ASSIGN
      bildkomb.NUM = berbildbuffh:BUFFER-FIELD("NUM"):BUFFER-VALUE
      bildkomb.BILD = "B".  
      berbildqh:GET-NEXT(NO-LOCK).
   END.
   DELETE WIDGET-POOL "BerbildFriDynTable" NO-ERROR.
   
END PROCEDURE.

PROCEDURE CreateCustomQueryBerbild:
   DEFINE INPUT PARAMETER tth  AS HANDLE NO-UNDO.
   DEFINE INPUT PARAMETER q AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER CustomQueryh AS HANDLE NO-UNDO.
   CREATE QUERY CustomQueryh IN WIDGET-POOL "BerbildFriDynTable".
   CustomQueryh:SET-BUFFERS(tth).
   CustomQueryh:QUERY-PREPARE(q).
   CustomQueryh:QUERY-OPEN().
END PROCEDURE.

