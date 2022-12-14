/*LISTDEF.I*/

DEFINE NEW SHARED TEMP-TABLE mtrl_temp 
   {MTRLTEMPTT.I}

DEFINE NEW SHARED TEMP-TABLE lin_upp   
   FIELD METER AS INTEGER
   FIELD ENR AS CHARACTER
   FIELD BENAMNING AS CHARACTER
   FIELD PRIS AS DECIMAL
   FIELD ENHET AS CHARACTER
   FIELD TOTMETER AS INTEGER
   FIELD UPPLAG AS INTEGER
   FIELD LEVKOD AS CHARACTER
   FIELD TOTPRIS AS DECIMAL   
   INDEX ENR ENR ASCENDING.    
   
DEFINE NEW SHARED TEMP-TABLE lin_temp  
   FIELD NUM1 AS INTEGER
   FIELD NUM2 AS INTEGER
   FIELD METER AS INTEGER             
   FIELD BENAMNING AS CHARACTER      
   INDEX NUM NUM1 NUM2 ASCENDING.
