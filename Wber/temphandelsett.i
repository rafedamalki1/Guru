
/*------------------------------------------------------------------------
    File        : temphandelsett.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Thu Nov 14 17:16:44 CET 2019
    Notes       :
  ----------------------------------------------------------------------*/
DEFINE {&NEW} {&SHARED} TEMP-TABLE temphandelse NO-UNDO
   FIELD ID AS INTEGER
   FIELD BENAMNING AS CHARACTER
   FIELD BORT AS LOGICAL
   FIELD TYP AS CHARACTER /* typ + sortchar samma */
   FIELD SORTCHAR AS CHARACTER
   FIELD ORDNING AS INTEGER
   FIELD ENHET AS CHARACTER
   INDEX ORDNING ORDNING
   INDEX BENAMNING BENAMNING
   INDEX ID ID.
