/*DEFINITIONER TEMP-TABLE kopp_lina BERLINKAB*/  
   
DEFINE {&NEW} {&SHARED} TEMP-TABLE kopp_lina NO-UNDO
   FIELD NUM1        AS INTEGER  
   FIELD NUM2        AS INTEGER    
   FIELD METER       AS INTEGER  LABEL "Meter"   
   FIELD LEDARE      AS INTEGER  
   FIELD ENR         AS CHARACTER LABEL "Enr"       
   FIELD BENAMNING   AS CHARACTER LABEL "Ben�mning"
   FIELD PRIS        AS DECIMAL  
   FIELD ENHET       AS CHARACTER
   FIELD KABNR       AS INTEGER  
   FIELD SKAP        AS CHARACTER
   FIELD SKAPADR     AS CHARACTER
   FIELD KABADR      AS CHARACTER
   FIELD TYP         AS CHARACTER
   FIELD ARTAL       AS INTEGER INITIAL ? 
   FIELD SAKR        AS INTEGER  
   FIELD MAXSAKR     AS INTEGER  
   FIELD ANMARK      AS CHARACTER
   FIELD KORTKOD     AS INTEGER INITIAL ? 
   FIELD KABNR2      AS INTEGER     
   FIELD TOTMETER    AS INTEGER  
   FIELD UPPLAG      AS INTEGER INITIAL ? 
   FIELD LEVKOD      AS CHARACTER
   FIELD APPARAT     AS CHARACTER 
   FIELD DIAMETER    AS INTEGER 
   INDEX NUM IS PRIMARY NUM1 NUM2 ASCENDING 
   INDEX ENR ENR ASCENDING
   INDEX KABNR KABNR ASCENDING
   INDEX LIN NUM1 NUM2 KABNR ASCENDING.
