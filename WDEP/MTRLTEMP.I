/*MTRLTEMP.I*/
/* DEFINE {&NEW} {&SHARED} TEMP-TABLE mtrltemp NO-UNDO                  */
/*    FIELD Benamning     AS CHARACTER FORMAT "x(40)" LABEL "Benämning" */
/*    FIELD BERKVANT    AS  DECIMAL FORMAT "->>>>9.99"                 */
/*    FIELD BERLEV     AS  CHARACTER  FORMAT "x(4)"                     */
/*    FIELD Bestkvant     AS  DECIMAL FORMAT "->>>>9.99"               */
/*    FIELD BPRIS     AS  DECIMAL FORMAT ">>>>99.99" LABEL "Bruttopris" */
/*    FIELD Enhet AS  CHARACTER    FORMAT "x(5)"                        */
/*    FIELD Enr     AS  CHARACTER  FORMAT "x(11)" LABEL "Enr"           */
/*    FIELD KALKNR    AS  INTEGER FORMAT ">>>>>>9"                    */
/*    FIELD KPRIS     AS  DECIMAL   FORMAT ">>>>99.99"                  */
/*    FIELD KUND     AS  LOGICAL  FORMAT "Ja/Nej"                       */
/*    FIELD LEVKOD     AS  CHARACTER FORMAT "x(4)"                      */
/*    FIELD NPRIS     AS  DECIMAL FORMAT ">>>>99.99" LABEL "Nettopris"  */
/*    FIELD OFFERT     AS  LOGICAL FORMAT "Ja/Nej"                      */
/*    FIELD SUMMA     AS  DECIMAL FORMAT ">>>>99.99"                    */
/*    FIELD MTRLROW AS ROWID                                            */
/*    INDEX Enr IS PRIMARY enr                                          */
/*    INDEX Lev LEVKOD KALKNR enr                                       */
/*    INDEX BEN LEVKOD KALKNR BENAMNING.                                */
{MTRLTAB.I}

        
DEFINE {&NEW} {&SHARED} TEMP-TABLE mtrldeptemp NO-UNDO
   FIELD Benamning     AS CHARACTER FORMAT "x(40)" LABEL "Benämning"                            
   FIELD BERKVANT    AS  DECIMAL FORMAT ">>>>9"
   FIELD Bestkvant     AS  DECIMAL FORMAT "->>>>9.99"     
   FIELD BESTPUNKT    AS  DECIMAL FORMAT ">>>>9"
   FIELD BPRIS     AS  DECIMAL FORMAT ">>>>99.99"
   FIELD DEPNR    AS  INTEGER FORMAT ">>9"
   FIELD Enhet AS  CHARACTER    FORMAT "x(5)"                    
   FIELD Enr     AS  CHARACTER  FORMAT "x(11)" LABEL "Enr"                        
   FIELD FACKID     AS  CHARACTER  FORMAT "x(8)" LABEL "Fack-id"                          
   FIELD IB     AS  INTEGER  FORMAT ">>>>9"                         
   FIELD IBDATUM     AS  DATE  FORMAT "99/99/99"                         
   FIELD INVANT     AS  INTEGER  FORMAT ">>>>9"                         
   FIELD INVDATUM     AS  DATE  FORMAT "99/99/99"  
   FIELD LAGER     AS  LOGICAL  FORMAT "Ja/Nej"     
   FIELD LEVKOD     AS  CHARACTER FORMAT "x(4)"
   FIELD NPRIS     AS  DECIMAL FORMAT ">>>>99.99" LABEL "Nettopris"
   FIELD OMSATT     AS  DECIMAL  FORMAT "->>>>9.99"                         
   FIELD SALDO     AS  INTEGER  FORMAT ">>>>>9"    
   FIELD MTRLROW    AS ROWID                     
   INDEX Enr IS PRIMARY depnr enr
   INDEX DEPNR DEPNR.

DEFINE {&NEW} {&SHARED} TEMP-TABLE satstemp NO-UNDO
  FIELD KOD          AS CHARACTER  FORMAT "X(11)"                 LABEL "Kod"       
  FIELD BENAMNING    AS CHARACTER  FORMAT "x(30)"                 LABEL "Benämning" 
  FIELD ENR          AS CHARACTER  FORMAT "X(11)"                 LABEL "Enr"       
  FIELD ENHET        AS CHARACTER  FORMAT "x(5)"                  LABEL "Enhet"     
  FIELD ANTAL        AS INTEGER    FORMAT ">>>>>9"   INITIAL 0   LABEL "Antal"     
  FIELD PRIS         AS DECIMAL    FORMAT ">>>>99.99" INITIAL 0   LABEL "Pris"      
  FIELD LEVKOD       AS CHARACTER  FORMAT "x(4)"                  LABEL "LEV-ID"    
  FIELD SATS         AS LOGICAL    FORMAT "Ja/Nej"    INITIAL YES LABEL "Sats"      
  FIELD ENR2         AS CHARACTER  FORMAT "X(11)"                 LABEL "Enr"       
  FIELD BENAMNING2   AS CHARACTER  FORMAT "x(40)"                 LABEL "Benämning" 
  FIELD PRIS2        AS DECIMAL    FORMAT ">>>>99.99" INITIAL 0   LABEL "Pris"      
  FIELD ENHET2       AS CHARACTER  FORMAT "x(5)"                  LABEL "Enhet"                       
  FIELD SATSROW      AS ROWID
  INDEX KOD IS PRIMARY SATS LEVKOD KOD ENR ENR2.
DEFINE TEMP-TABLE emtrldeptemp NO-UNDO LIKE mtrldeptemp.

