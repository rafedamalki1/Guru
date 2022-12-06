
/*------------------------------------------------------------------------
    File        : FINNSBEN.I
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : elpao
    Created     : Thu Feb 07 10:30:25 CET 2013
    Notes       :
  ----------------------------------------------------------------------*/
DEFINE VARIABLE brwwidth AS DECIMAL NO-UNDO.
DEFINE VARIABLE tcolwidth AS DECIMAL NO-UNDO.
DEFINE VARIABLE finnsben AS LOGICAL NO-UNDO.

PROCEDURE finnsben_UI :
   DEFINE VARIABLE dyncurcolh AS HANDLE NO-UNDO.
   finnsben = FALSE.
   dyncurcolh = h:FIRST-COLUMN.
   IF dyncurcolh = ? THEN RETURN.
   REPEAT:
      IF dyncurcolh:NAME = "BENAMNING" THEN DO:
         finnsben = TRUE.
         h:NO-EMPTY-SPACE = FALSE NO-ERROR.
         LEAVE.
      END.
      dyncurcolh = dyncurcolh:NEXT-COLUMN.
      IF NOT VALID-HANDLE(dyncurcolh) THEN LEAVE.
   END.  
   dyncurcolh = h:FIRST-COLUMN.
END PROCEDURE.

PROCEDURE colwidth_UI :
   DEFINE VARIABLE dyncurcolh AS HANDLE NO-UNDO.
   DEFINE VARIABLE bencolh AS HANDLE NO-UNDO.
   brwwidth = h:WIDTH-CHARS. 
   tcolwidth = 0.
   dyncurcolh = h:FIRST-COLUMN.
   IF dyncurcolh = ? THEN RETURN.
   REPEAT:
      IF dyncurcolh:NAME = "BENAMNING" THEN bencolh = dyncurcolh.
      tcolwidth = tcolwidth + dyncurcolh:WIDTH-CHARS.
      dyncurcolh = dyncurcolh:NEXT-COLUMN.
      IF NOT VALID-HANDLE(dyncurcolh) THEN LEAVE.
   END.
   dyncurcolh = h:FIRST-COLUMN.
   IF tcolwidth < brwwidth THEN DO:
      bencolh:WIDTH-CHARS = bencolh:WIDTH-CHARS +  (brwwidth - (tcolwidth + 7)). 
   END.   
  
END PROCEDURE.
  