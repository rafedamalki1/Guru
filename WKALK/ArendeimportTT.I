
/*------------------------------------------------------------------------
    File        : 
    Purpose     : ArendeimportTT.I

    Syntax      :

    Description : 

    Author(s)   : elpao
    Created     : Fri Dec 21 13:18:35 CET 2012
    Notes       :
  ----------------------------------------------------------------------*/

DEFINE TEMP-TABLE ArendeimportTT
   FIELD ARENDENR AS INTEGER
   FIELD OMRADE AS CHARACTER
   FIELD NUM AS INTEGER
   FIELD MATRIS AS INTEGER
   FIELD ARBKOD AS CHARACTER 
   FIELD LOPNR AS INTEGER
   FIELD ANTAL AS DECIMAL
   FIELD BENAMNING AS CHARACTER
   FIELD MARKNING AS CHARACTER
   FIELD MARKSUB AS CHARACTER
   FIELD ANMARKNING AS CHARACTER
   FIELD RISK AS DECIMAL
   FIELD VINST AS DECIMAL
   FIELD TTRECID AS RECID
   INDEX MATRIS MATRIS ARBKOD LOPNR NUM.