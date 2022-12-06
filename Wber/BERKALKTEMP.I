/*BERKALKTEMP.I*/

DEFINE {&NEW} {&SHARED} TEMP-TABLE berkalktemp NO-UNDO   
   FIELD AONR        AS CHARACTER FORMAT "X(6)"                   LABEL "AONR"             
   FIELD OMRADE      AS CHARACTER FORMAT "x(6)"                   LABEL "UTFÖRANDE OMRÅDE" 
   FIELD NUM         AS INTEGER   FORMAT ">>>9"        INITIAL 0                      
   FIELD ARBKOD      AS CHARACTER FORMAT "X(5)"                   LABEL "Arbetskod"        
   FIELD LOPNR       AS INTEGER   FORMAT ">>>"         INITIAL 0  LABEL "Löpnr"            
   FIELD BENAMNING   AS CHARACTER FORMAT "X(40)"                  LABEL "Benämning"        
   FIELD ENHET       AS CHARACTER FORMAT "X(3)"                   LABEL "Enhet"            
   FIELD ANTAL       AS DECIMAL   FORMAT "->>>>9.999" INITIAL 0  LABEL "Antal" 
 /*  FIELD KALKNUM     AS INTEGER 
   FIELD BIKID       AS INTEGER*/          
   INDEX OMR IS PRIMARY OMRADE AONR.
