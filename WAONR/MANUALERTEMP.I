
/*------------------------------------------------------------------------
    File        : MANUALERTEMP.I
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Fri Aug 31 13:28:40 CEST 2018
    Notes       :
  ----------------------------------------------------------------------*/

DEFINE TEMP-TABLE imptemp NO-UNDO
   FIELD INVAR AS CHARACTER
   FIELD INDIR AS CHARACTER
   FIELD FILNAMN AS CHARACTER
   FIELD ORDNING AS INTEGER
   INDEX INVAR ORDNING INVAR.