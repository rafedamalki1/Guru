
/*------------------------------------------------------------------------
    File        : GURUFRAG.I
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Wed Oct 20 11:45:10 CEST 2021
    Notes       :
  ----------------------------------------------------------------------*/

DEFINE TEMP-TABLE funktionsfragor NO-UNDO   
   FIELD ID AS INTEGER
   FIELD ORDNING AS INTEGER
   FIELD STEG AS CHARACTER
   FIELD FRAGA AS CHARACTER
   FIELD SVAR1 AS LOGICAL
   FIELD SVAR2 AS LOGICAL
   FIELD SVAR3 AS LOGICAL
   FIELD SVAR4 AS LOGICAL
   FIELD SVAR5 AS LOGICAL
   FIELD SVAR6 AS LOGICAL
   FIELD SVAR7 AS LOGICAL
   FIELD SVAR8 AS LOGICAL
   FIELD SVAR9 AS LOGICAL
   FIELD SVAR10 AS LOGICAL 
   INDEX ID ORDNING. 

DEFINE TEMP-TABLE funktionstexter NO-UNDO
   FIELD ID AS INTEGER
   FIELD ORDNING AS INTEGER
   FIELD UNDERTEXT1 AS CHARACTER
   FIELD UNDERTEXT2 AS CHARACTER
   INDEX ID ORDNING.
