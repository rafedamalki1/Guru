
/*------------------------------------------------------------------------
    File        : KaladmimportTT.I
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : elpao
    Created     : Fri Dec 21 13:18:35 CET 2012
    Notes       :
  ----------------------------------------------------------------------*/

DEFINE TEMP-TABLE KaladmimportTT
   FIELD ARBKOD AS CHARACTER 
   FIELD LOPNR AS INTEGER
   FIELD BENAMNING AS CHARACTER
   FIELD ENHET AS CHARACTER
   FIELD BANTAL AS DECIMAL
   FIELD MOANTAL AS DECIMAL
   FIELD MAANTAL AS DECIMAL
   FIELD ARBKOST AS DECIMAL
   FIELD MATERIEL AS DECIMAL
   FIELD MASKKOST AS DECIMAL
   FIELD OVRKOST AS DECIMAL
   FIELD TOTSUM AS DECIMAL
   FIELD KOMM AS CHARACTER
   FIELD TTRECID AS RECID
   FIELD HKID AS INTEGER
   FIELD BKID AS INTEGER
   INDEX ARBKOD ARBKOD LOPNR.
DEFINE BUFFER KaladmimportTTbuff FOR KaladmimportTT. 