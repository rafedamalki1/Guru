
/*------------------------------------------------------------------------
    File        : HDKALKTEMP.I
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Wed Mar 02 14:12:54 CET 2016
    Notes       :
  ----------------------------------------------------------------------*/

DEFINE {&NEW} {&SHARED} TEMP-TABLE hdkalktemp NO-UNDO
   FIELD BERNR  AS INTEGER 
   FIELD OMRADE  AS CHARACTER 
   FIELD SID  AS INTEGER 
   FIELD MATRIS  AS INTEGER 
   FIELD ARBKOD  AS CHARACTER 
   FIELD LOPNR  AS INTEGER FORMAT ">99"
   FIELD BENAMNING  AS CHARACTER 
   FIELD ENHET  AS CHARACTER 
   FIELD ANTAL  AS DECIMAL  
   FIELD KLID  AS INTEGER 
   FIELD PID  AS INTEGER 
   FIELD SKAPAD AS CHARACTER
   FIELD ANDRAD AS LOGICAL
   FIELD BORT AS LOGICAL
   FIELD TTRECID AS RECID
   INDEX BERNR BERNR OMRADE SID KLID. 
DEFINE TEMP-TABLE ehdkalktemp NO-UNDO LIKE hdkalktemp.
DEFINE TEMP-TABLE Vhdkalktemp NO-UNDO LIKE hdkalktemp
INDEX ARBKOD IS PRIMARY ARBKOD LOPNR.
   
