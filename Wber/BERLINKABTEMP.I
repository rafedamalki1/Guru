/*BERLINKABTEMP.I*/

DEFINE {&NEW} {&SHARED} TEMP-TABLE berlinkabtemp NO-UNDO
   FIELD AONR      AS CHARACTER  FORMAT "X(6)"                LABEL "AONR"      
   FIELD DELNR     AS INTEGER    FORMAT 999       INITIAL 0   LABEL "DELNR"               
   FIELD NUM1      AS INTEGER    FORMAT ">>>9"    INITIAL 0                         
   FIELD NUM2      AS INTEGER    FORMAT ">>>9"    INITIAL 0                         
   FIELD METER     AS INTEGER    FORMAT ">>>>>9" INITIAL 0   LABEL "Meter"               
   FIELD LEDARE    AS INTEGER    FORMAT 9         INITIAL 0   LABEL "LEDARE"              
   FIELD ENR       AS CHARACTER  FORMAT "X(11)"               LABEL "Enr"                 
   FIELD BENAMNING AS CHARACTER  FORMAT "x(40)"               LABEL "Benämning"           
   FIELD KABNR     AS INTEGER    FORMAT ">>>"     INITIAL 0   LABEL "KABNR"               
   FIELD SKAP      AS CHARACTER  FORMAT "X(40)"               LABEL "Skåptyp"             
   FIELD SKAPADR   AS CHARACTER  FORMAT "X(30)"               LABEL "Skåpadress"          
   FIELD KABADR    AS CHARACTER  FORMAT "X(30)"               LABEL "Adress"              
   FIELD TYP       AS CHARACTER  FORMAT "X(6)"                LABEL "Typ"                 
   FIELD ARTAL     AS INTEGER    FORMAT ">>>>"   INITIAL ?    LABEL "Årtal"               
   FIELD SAKR      AS INTEGER    FORMAT ">>>"    INITIAL 0    LABEL "Säkring"             
   FIELD MAXSAKR   AS INTEGER    FORMAT ">>>"    INITIAL 0    LABEL "Maxsäkring"          
   FIELD ANMARK    AS CHARACTER  FORMAT "X(50)"               LABEL "Anmärkning"          
   FIELD KORTKOD   AS INTEGER    FORMAT ">>>9"   INITIAL  ?                         
   FIELD KABNR2    AS INTEGER    FORMAT ">>>"    INITIAL  0   LABEL "KABNR2"              
   FIELD PRIS      AS DECIMAL    FORMAT ">>>>99.99" INITIAL 0 LABEL "Pris"                
   FIELD ENHET     AS CHARACTER  FORMAT "x(5)"                LABEL "Enhet"               
   FIELD UPPLAG    AS INTEGER    FORMAT ">>"       INITIAL ?  LABEL "UPPLAG"              
   FIELD TOTMETER  AS INTEGER    FORMAT ">>>>>9"  INITIAL 0  LABEL "Meter"               
   FIELD DATUM     AS DATE       FORMAT "99/99/99" INITIAL ?  LABEL "Datum"               
   FIELD LEVKOD    AS CHARACTER  FORMAT "x(4)"                LABEL "LEV-ID"              
   FIELD APPARAT   AS CHARACTER  FORMAT "X(20)"               LABEL "Apparat"             
   FIELD DIAMETER  AS INTEGER    FORMAT ">>9"      INITIAL 0  LABEL "Diameter"            
   FIELD OMRADE    AS CHARACTER  FORMAT "x(6)"                LABEL "Område"              
   INDEX DATUM OMRADE AONR DATUM
   INDEX INKOP OMRADE AONR DATUM KORTKOD
   INDEX OMR IS PRIMARY OMRADE AONR.
