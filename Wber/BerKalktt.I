
/*------------------------------------------------------------------------
    File        : BerKalktt.I

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Fri Dec 15 11:59:16 CET 2017
    Notes       :
  ----------------------------------------------------------------------*/

DEFINE TEMP-TABLE berkalktt NO-UNDO
   FIELD BERNR AS INTEGER
   FIELD OMRADE AS CHARACTER
   FIELD KALKNR AS CHARACTER 
   INDEX BERNR BERNR OMRADE.
