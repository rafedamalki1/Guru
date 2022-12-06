/*KONSTRMTRL.I*/
      
DEFINE {&NEW} {&SHARED} TEMP-TABLE konsttemp NO-UNDO
   FIELD KONSKOD AS INTEGER FORMAT ">>>>9" INITIAL 0 LABEL "Konstruktionsgruppkod"
   FIELD KTYPKOD AS CHARACTER FORMAT "X(8)" LABEL "Konstruktionskod"
   FIELD BENAMNING AS CHARACTER FORMAT "X(40)" LABEL "Benämning"
   FIELD ORDNING AS INTEGER  FORMAT ">9" INITIAL ? LABEL "Ordning" 
   FIELD BILD AS CHARACTER   
   FIELD UID AS INTEGER  FORMAT "->,>>>,>>9" INITIAL 0 
   FIELD UNAMN AS CHARACTER FORMAT "X(8)" LABEL "Kund"   
   INDEX KOD IS PRIMARY KONSKOD KTYPKOD
   INDEX ORD KONSKOD ORDNING
   INDEX ORDNING ORDNING
   INDEX UID UID.
DEFINE TEMP-TABLE ukonsttemp NO-UNDO LIKE konsttemp.
DEFINE TEMP-TABLE vkonsttemp NO-UNDO LIKE konsttemp.

/*DEFINE {&NEW} {&SHARED} TEMP-TABLE konstgrptemp NO-UNDO
   FIELD KONSKOD   AS INTEGER FORMAT ">>>>9" INITIAL 0 LABEL "Konstruktionsgruppkod"
   FIELD BENAMNING AS CHARACTER FORMAT "X(40)" LABEL "Konstruktionsgrupp"
   FIELD ORDNING   AS INTEGER FORMAT ">9" INITIAL ? LABEL "Ordning"
   INDEX BEN BENAMNING
   INDEX KOD IS PRIMARY KONSKOD
   INDEX ORD ORDNING.*/

{KONSTGRUPP.I} /* ovanstående finns i denna */
{KONSTVALTEMP.I}

DEFINE TEMP-TABLE ekonstvaltemp NO-UNDO LIKE konstvaltemp.
DEFINE {&NEW} {&SHARED} TEMP-TABLE mtrlbertemp NO-UNDO
   FIELD KTYPKOD    AS CHARACTER FORMAT "X(8)"                 LABEL "Konstruktionskod"      
   FIELD F1         AS CHARACTER FORMAT "X(8)"                 LABEL "F1"                              
   FIELD F2         AS CHARACTER FORMAT "X(8)"                 LABEL "F2"                  
   FIELD F3         AS CHARACTER FORMAT "X(8)"                 LABEL "F3"                  
   FIELD F4         AS CHARACTER FORMAT "X(8)"                 LABEL "F4"                  
   FIELD F5         AS CHARACTER FORMAT "X(8)"                 LABEL "F5"                  
   FIELD ENR        AS CHARACTER FORMAT "X(11)"                LABEL "Enr"                   
   FIELD BENAMNING  AS CHARACTER FORMAT "x(40)"                LABEL "Benämning"             
   FIELD ENHET      AS CHARACTER FORMAT "x(5)"                 LABEL "Enhet"                 
   FIELD ANTAL      AS INTEGER   FORMAT ">>>>>9"   INITIAL 0  LABEL "Antal"
   FIELD PRIS       AS DECIMAL   FORMAT ">>>>99.99" INITIAL 0  LABEL "Pris"                  
   FIELD LEVKOD     AS CHARACTER FORMAT "x(4)"                 LABEL "LEV-ID"                
   FIELD LINKAB     AS LOGICAL   FORMAT "Ja/Nej"    INITIAL NO LABEL "Lin/Kab"               
   FIELD MODUL      AS INTEGER   FORMAT ">>>9"      INITIAL 0  LABEL "Moduler"               
   FIELD TYPBER     AS LOGICAL   FORMAT "yes/no"    INITIAL NO LABEL "Beroende av kopplingst"
   FIELD DIAMETER   AS INTEGER   FORMAT ">>9"       INITIAL 0  LABEL "Diameter"              
   FIELD SATS       AS LOGICAL   FORMAT "Ja/Nej"    INITIAL NO LABEL "Sats" 
   FIELD MTRLROW    AS ROWID
   INDEX KOD IS PRIMARY KTYPKOD F1 F2 F3 F4 F5
   INDEX ENR ENR.

DEFINE {&NEW} {&SHARED} TEMP-TABLE emtrlbertemp NO-UNDO LIKE mtrlbertemp.

DEFINE {&NEW} {&SHARED} TEMP-TABLE kalkbertemp NO-UNDO
   FIELD KTYPKOD   AS CHARACTER FORMAT "X(8)"  LABEL "Konstruktionskod" 
   FIELD F1        AS CHARACTER FORMAT "X(8)"                                
   FIELD F2        AS CHARACTER FORMAT "X(8)"                                
   FIELD F3        AS CHARACTER FORMAT "X(8)"                                
   FIELD F4        AS CHARACTER FORMAT "X(8)"                                
   FIELD F5        AS CHARACTER FORMAT "X(8)"                                
   FIELD ARBKOD    AS CHARACTER FORMAT "X(5)"            LABEL "Arbetskod"        
   FIELD LOPNR     AS INTEGER   FORMAT ">>>"   INITIAL 0 LABEL "LÖPNR"            
   FIELD BENAMNING AS CHARACTER FORMAT "X(40)"           LABEL "Benämning"        
   FIELD ENHET     AS CHARACTER FORMAT "X(3)"            LABEL "Enhet"            
   FIELD ANTAL     AS DECIMAL   FORMAT "->>>>9.999" INITIAL 0 LABEL "Antal"            
   FIELD KATAR     AS INTEGER   FORMAT 9999          INITIAL ? LABEL "Katalog år"       
   FIELD KALKROW   AS ROWID
   INDEX AR IS PRIMARY KATAR KTYPKOD F1 F2 F3 F4 F5.

DEFINE {&NEW} {&SHARED} TEMP-TABLE ekalkbertemp NO-UNDO LIKE kalkbertemp.
DEFINE TEMP-TABLE vkalkbertemp NO-UNDO LIKE kalkbertemp.
