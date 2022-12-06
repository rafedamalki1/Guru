
/*------------------------------------------------------------------------
    File        : LOGSEKREGTT.I
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Tue Aug 28 12:33:52 CEST 2018
    Notes       :
  ----------------------------------------------------------------------*/

DEFINE TEMP-TABLE logsekregTT NO-UNDO
  FIELD ANVANDARE AS CHARACTER
  FIELD DATUM  AS DATETIME
  FIELD AVANVANDARE AS CHARACTER
  FIELD PROGRAM AS CHARACTER
  FIELD TYP AS CHARACTER
  FIELD PERSONALKOD AS CHARACTER
  FIELD TILLFRAN AS LOGICAL FORMAT "Till/Bort"
  INDEX DATUM DATUM ANVANDARE AVANVANDARE.