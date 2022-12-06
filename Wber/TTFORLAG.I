
/*------------------------------------------------------------------------
    File        : TTFORLAG.I
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Thu Nov 14 14:54:30 CET 2019
    Notes       :
  ----------------------------------------------------------------------*/

DEFINE {&NEW} {&SHARED} TEMP-TABLE tempforlagg NO-UNDO
   FIELD BENAMNING AS CHARACTER
   FIELD DJUP      AS INTEGER
   FIELD FAKTOR    AS DECIMAL
   FIELD FORLAGG   AS CHARACTER
   FIELD MARK      AS LOGICAL
   FIELD ROR       AS LOGICAL
   FIELD SAM       AS LOGICAL
   FIELD TILLAGG   AS INTEGER
   FIELD ID        AS INTEGER
   FIELD ORDNING   AS INTEGER
   FIELD BREDD     AS DECIMAL
   FIELD ARBKOD    AS CHARACTER
   FIELD LOPNR     AS INTEGER
   FIELD BORT  AS LOGICAL
   INDEX ORDNING IS PRIMARY ORDNING
   INDEX BENAMNING BENAMNING
   INDEX ARBKOD ARBKOD LOPNR.