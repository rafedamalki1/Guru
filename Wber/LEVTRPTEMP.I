/*LEVTRPTEMP.I*/

DEFINE {&NEW} {&SHARED} TEMP-TABLE levtrptemp NO-UNDO
   FIELD BESTNR     AS INTEGER   FORMAT "->>>>>>9"                 
   FIELD BERNR      AS INTEGER   FORMAT "->>>>>>9"    INITIAL  0   
   FIELD LEVKOD     AS CHARACTER FORMAT "x(4)"                       LABEL "LEV-ID"           
   FIELD LEVNAMN    AS CHARACTER FORMAT "x(25)"                      LABEL "Namn"             
   FIELD LEVADR     AS CHARACTER FORMAT "X(20)"                      LABEL "ADRESS"           
   FIELD LEVORT     AS CHARACTER FORMAT "x(15)"                      LABEL "ORT"              
   FIELD LEVTEL     AS CHARACTER FORMAT "xxxx-xxxxxxx"               LABEL "TEL"              
   FIELD LEVKONTAKT AS CHARACTER FORMAT "x(25)"                      LABEL "KONTAKT"          
   FIELD LEVPNR     AS CHARACTER FORMAT "x(6)"                       LABEL "POSTNR"           
   FIELD KUNDNR     AS INTEGER   FORMAT "->>>>>>9"     INITIAL 0   
   FIELD FORE       AS CHARACTER FORMAT "x(16)"                      LABEL "Företag"          
   FIELD KADR       AS CHARACTER FORMAT "x(25)"                      LABEL "Adress"           
   FIELD BOX        AS CHARACTER FORMAT "X(8)"                       
   FIELD KPNR       AS CHARACTER FORMAT "999 99"                     LABEL "Postnr"         
   FIELD KORT       AS CHARACTER FORMAT "x(20)"                      LABEL "Ort"              
   FIELD KIKONTAKT  AS CHARACTER FORMAT "x(25)"                      
   FIELD FAX        AS CHARACTER FORMAT "x(11)"                      
   FIELD KITELE     AS CHARACTER FORMAT "X(11)"                      
   FIELD KIMOBIL    AS CHARACTER FORMAT "X(11)"                      
   FIELD KTKONTAKT  AS CHARACTER FORMAT "X(25)"                      
   FIELD KTTELE     AS CHARACTER FORMAT "X(11)"                      
   FIELD LEVERANS   AS CHARACTER FORMAT "X(8)"                       
   FIELD DATUM      AS DATE      FORMAT "99/99/99"                  
   FIELD L1         AS CHARACTER FORMAT "X(508)"                     
   FIELD L2         AS CHARACTER FORMAT "X(50)"                      
   FIELD L3         AS CHARACTER FORMAT "X(50)"                      
   FIELD MARK       AS CHARACTER FORMAT "X(35)"                      
   FIELD KOM        AS CHARACTER FORMAT "X(40)"                      
   FIELD AVISPERS   AS CHARACTER FORMAT "X(20)"                      
   FIELD AVIS       AS LOGICAL   FORMAT "yes/no"         INITIAL no  
   FIELD BESTALLD   AS CHARACTER FORMAT "X(18)"                      LABEL "Beställd"         
   FIELD Dep-Nr     AS INTEGER   FORMAT ">>>"            INITIAL 0   LABEL "Depå-Nr"          
   FIELD OMRADE     AS CHARACTER FORMAT "x(6)"                       LABEL "OMRÅDE"           
   FIELD KTMOBIL    AS CHARACTER FORMAT "X(11)"                      
   FIELD BERBESTNR  AS CHARACTER FORMAT "X(20)"                      
   INDEX BERNR BERNR LEVKOD
   INDEX BERNR2 OMRADE BERNR LEVKOD
   INDEX BESTNR IS PRIMARY BESTNR LEVKOD
   INDEX BESTNR2 Dep-Nr BESTNR LEVKOD.

DEFINE {&NEW} {&SHARED} TEMP-TABLE elevtrptemp NO-UNDO LIKE levtrptemp.

DEFINE {&NEW} {&SHARED} TEMP-TABLE levtrp2temp NO-UNDO
   FIELD BESTNR    AS  INTEGER   FORMAT "->>>>>>9"              
   FIELD BERNR     AS  INTEGER   FORMAT "->>>>>>9" INITIAL 0          
   FIELD LEVKOD    AS  CHARACTER FORMAT "x(4)"                  LABEL "LEV-ID"   
   FIELD DATUM     AS  DATE      FORMAT "99/99/99"   INITIAL ?             
   FIELD OMRADE    AS  CHARACTER FORMAT "x(6)"                  LABEL "OMRÅDE"   
   FIELD BESTALLD  AS  CHARACTER FORMAT "X(18)"                 LABEL "Beställd" 
   INDEX BERED IS PRIMARY OMRADE BERNR LEVKOD DATUM
   INDEX NR OMRADE BERNR BESTNR.

DEFINE {&NEW} {&SHARED} TEMP-TABLE elevtrp2temp NO-UNDO LIKE levtrp2temp.
