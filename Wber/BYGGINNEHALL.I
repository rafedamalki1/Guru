
/*------------------------------------------------------------------------
    File        : BYGGINNEHALL.I
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Wed Jan 03 08:19:35 CET 2018
    Notes       :
  ----------------------------------------------------------------------*/


    FIELD ORD AS INTEGER 
    FIELD ROWVAR AS INTEGER
    FIELD NUM AS INTEGER
    FIELD BERNR AS INTEGER
    FIELD OMRADE AS CHARACTER
    FIELD BYGGNR AS CHARACTER
    FIELD FRIID AS CHARACTER
    FIELD UPPLAG AS INTEGER
    FIELD KARTREF AS CHARACTER
    FIELD F1 AS CHARACTER
    FIELD F2 AS CHARACTER
    FIELD F3 AS CHARACTER
    FIELD F4 AS CHARACTER
    FIELD F5 AS CHARACTER
    FIELD F6 AS CHARACTER
    FIELD SIDA AS INTEGER
    FIELD UT AS CHARACTER
    FIELD TTRECID AS RECID
    INDEX ROWVAR SIDA ROWVAR.
DEFINE TEMP-TABLE einnehall NO-UNDO LIKE innehall.
