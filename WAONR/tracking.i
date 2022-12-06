
/*------------------------------------------------------------------------
    File        : tracking.i
    Purpose     : 

    Syntax      :
FUNKAR INTE MÅSTE KÖRA MOT TEMP-TABLE  2015-12-15
    Description : 

    Author(s)   : elpao
    Created     : Mon Jul 02 16:12:11 CEST 2012
    Notes       :
  ----------------------------------------------------------------------*/

  METHOD VOID Tracking{1}( INPUT paav AS LOGICAL):
     DEFINE VARIABLE  iBuff    AS INTEGER NO-UNDO.  
     DEFINE VARIABLE  hTopBuff    AS HANDLE NO-UNDO.  
      
     DO iBuff = 1 TO DATASET {1}:NUM-BUFFERS:
        hTopBuff = DATASET {1}:GET-BUFFER-HANDLE(iBuff).
        hTopBuff:TRACKING-CHANGES = paav.
        IF hTopBuff:PARENT-RELATION NE ? THEN NEXT. 
     END.
  END METHOD.