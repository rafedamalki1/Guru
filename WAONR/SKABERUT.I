/*SKABERUT.I*/            
DEFINE TEMP-TABLE bertemp
   FIELD FORETAG AS CHARACTER
   FIELD ANVANDARE AS CHARACTER
   FIELD DAG2 AS CHARACTER    
   FIELD DAG AS CHARACTER
   FIELD PKOD AS CHARACTER 
   FIELD SLUT2 AS DECIMAL  
   FIELD VECKA2 AS INTEGER
   FIELD START AS DECIMAL
   FIELD VECKA AS INTEGER
   FIELD DATUM1 AS DATE
   FIELD DATUM2 AS DATE
   FIELD AONR AS CHARACTER
   FIELD DELNR AS INTEGER
   FIELD BERBEORD AS LOGICAL
   FIELD BDATUM AS DATE 
   FIELD AVDATUM AS DATE
   FIELD JOUR AS LOGICAL
   FIELD FAKTOR AS LOGICAL
   FIELD BERALT AS INTEGER.

DEFINE INPUT PARAMETER TABLE FOR bertemp.
DEFINE OUTPUT PARAMETER musfel AS LOGICAL NO-UNDO.
  
