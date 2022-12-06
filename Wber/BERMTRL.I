/*BERMTRL.I*/
DEFINE {&NEW} {&SHARED} TEMP-TABLE bermtrltemp NO-UNDO             
   FIELD AONR      AS CHARACTER FORMAT "X(6)"                 LABEL "AONR"                               
   FIELD DELNR     AS INTEGER   FORMAT 999         INITIAL 0  LABEL "DELNR"                 
   FIELD NUM       AS INTEGER   FORMAT ">>>9"      INITIAL 0                    
   FIELD ENR       AS CHARACTER FORMAT "X(11)"                LABEL "Enr"                   
   FIELD BENAMNING AS CHARACTER FORMAT "x(40)"                LABEL "Benämning"             
   FIELD ENHET     AS CHARACTER FORMAT "x(5)"                 LABEL "Enhet"                 
   FIELD ANTAL     AS INTEGER   FORMAT ">>>>>9"   INITIAL 0  LABEL "Antal"                 
   FIELD PRIS      AS DECIMAL   FORMAT ">>>>99.99" INITIAL 0  LABEL "Pris"                  
   FIELD LEVKOD    AS CHARACTER FORMAT "x(4)"                 LABEL "LEV-ID"                
   FIELD LINKAB    AS LOGICAL   FORMAT "Ja/Nej"    INITIAL NO LABEL "Lin/Kab"               
   FIELD MODUL     AS INTEGER   FORMAT ">>>9"      INITIAL 0  LABEL "Moduler"               
   FIELD SKAPNUM   AS INTEGER   FORMAT ">>>9"      INITIAL 0                       
   FIELD TYPBER    AS LOGICAL   FORMAT "yes/no"    INITIAL no LABEL "Beroende av kopplingst"
   FIELD SKAPMTRL  AS LOGICAL   FORMAT "yes/no"    INITIAL no LABEL "Kabelskåpsmateriel"    
   FIELD SKAPMODUL AS INTEGER   FORMAT ">>>9"      INITIAL 0  LABEL "Skåpmoduler"           
   FIELD BESTANT   AS INTEGER   FORMAT ">>>>>9"   INITIAL 0  LABEL "Antal"                 
   FIELD BERLEV    AS CHARACTER FORMAT "x(4)"                 LABEL "LEV-ID"                
   FIELD INKOP     AS LOGICAL   FORMAT "yes/no"    INITIAL no LABEL "Inköp"                 
   FIELD DBEST     AS CHARACTER FORMAT "X(5)"                 LABEL "Till"                  
   FIELD DATUM     AS DATE      FORMAT "99/99/99"  INITIAL ?  LABEL "Datum"                 
   FIELD KLAR      AS LOGICAL   FORMAT "yes/no"    INITIAL no LABEL "Klar"                  
   FIELD OPRIS     AS DECIMAL   FORMAT ">>>>99.99" INITIAL 0  LABEL "Offertpris"            
   FIELD OFFERT    AS LOGICAL   FORMAT "yes/no"    INITIAL no LABEL "Offert"                
   FIELD DIAMETER  AS INTEGER   FORMAT ">>9"       INITIAL 0  LABEL "Diameter"              
   FIELD MTRLTEXT  AS CHARACTER FORMAT "X(40)"                LABEL "Text"                  
   FIELD OMRADE    AS CHARACTER FORMAT "x(6)"                 LABEL "Område"                
   FIELD SATS      AS LOGICAL   FORMAT "Ja/Nej"    INITIAL NO LABEL "Sats"                  
   FIELD PAR       AS INTEGER   FORMAT 9           INITIAL 0  LABEL "Par"                   
   INDEX DATUM OMRADE AONR INKOP DATUM
   INDEX INKOP OMRADE AONR INKOP KLAR
   INDEX OMR IS PRIMARY OMRADE AONR.

