
/*------------------------------------------------------------------------
    File        : JMFLEVTEP.I
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Mon Jun 03 11:24:12 CEST 2019
    Notes       :
  ----------------------------------------------------------------------*/

DEFINE TEMP-TABLE jmflev_temp
    FIELD LEVKOD AS CHARACTER
    FIELD LEVNAMN AS CHARACTER
    FIELD FAKTOR AS DECIMAL
    INDEX LEVKOD LEVKOD.
