
/*------------------------------------------------------------------------
    File        : MobileArendeDSorg.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Fri Nov 15 16:57:33 CET 2013
    Notes       :
  ----------------------------------------------------------------------*/
DEFINE TEMP-TABLE arendehuvtt NO-UNDO  
   BEFORE-TABLE arendehuvbef
   FIELD ARENDENR AS INTEGER
   FIELD OMRADE AS CHARACTER
   FIELD BENAMNING AS CHARACTER
   FIELD KLOGID AS INTEGER
   FIELD TYPKALK AS INTEGER 
   FIELD EGETMTRL AS LOGICAL
   FIELD EGNAPRISER AS LOGICAL
   FIELD FAKTORER AS LOGICAL
   FIELD TTRECID AS RECID
   FIELD ANMARKNING AS CHARACTER
   FIELD BESTID AS CHARACTER
   FIELD KALKANV AS CHARACTER
   FIELD ANVANDARE AS CHARACTER
   FIELD AKTIV AS LOGICAL INITIAL TRUE
   FIELD UTYP AS INTEGER INITIAL 1
   INDEX ARENDENR ARENDENR
   INDEX TYPKALK TYPKALK.  
DEFINE DATASET dsMobileArende FOR arendehuvtt.
