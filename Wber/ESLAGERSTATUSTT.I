
/*------------------------------------------------------------------------
    File        : ESLAGERSTATUSTT.I
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Thu Sep 07 13:24:48 CEST 2017
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE eslagersatatustt NO-UNDO
   FIELD ENR AS CHARACTER
   FIELD UTANEENR AS CHARACTER 
   FIELD LEVKOD AS CHARACTER
   FIELD BENAMNING AS CHARACTER
   FIELD ANTAL AS DECIMAL
   FIELD SVAR AS CHARACTER
   FIELD ANTALILAGER AS DECIMAL 
   FIELD SVARMTRL AS CHARACTER
  
   FIELD TTRECID AS RECID
   INDEX ENR ENR.

