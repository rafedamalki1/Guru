
/*------------------------------------------------------------------------
    File        : StorFelMedtt.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Thu Jun 15 09:45:37 CEST 2017
    Notes       :
  ----------------------------------------------------------------------*/

DEFINE TEMP-TABLE Storfelmedtt NO-UNDO
   FIELD SMEDD AS CHARACTER FORMAT "X(132)"
   FIELD LAGGTILL AS LOGICAL
   FIELD SKRIVOVER AS LOGICAL
   FIELD TABORT AS LOGICAL
   FIELD ORDNING AS INTEGER
   FIELD DATATABELL AS CHARACTER
   FIELD IdentifikationsnrForetag AS INTEGER
   FIELD DistriktsId AS CHARACTER 
   FIELD SPANID AS INTEGER
   FIELD SpanNivId AS INTEGER
   FIELD TTRECID AS RECID
   INDEX  ORDNING IS PRIMARY ORDNING
   INDEX SMEDD SMEDD. 


DEFINE BUFFER Storfelmedbuff FOR Storfelmedtt.   