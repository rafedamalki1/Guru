
/*------------------------------------------------------------------------
    File        : SKAPTEMP.I
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Tue Oct 30 17:02:52 CET 2018
    Notes       :
  ----------------------------------------------------------------------*/

  DEFINE TEMP-TABLE skap_temp
   FIELD NUM AS INTEGER 
   FIELD KABNUM AS INTEGER
   FIELD F1 AS CHARACTER 
   FIELD F2 AS CHARACTER 
   FIELD F3 AS CHARACTER
   FIELD F4 AS CHARACTER
   FIELD F5 AS CHARACTER
   FIELD F6 AS CHARACTER
   FIELD SKAPNUM AS INTEGER   
   INDEX NUM IS PRIMARY NUM SKAPNUM ASCENDING.    