/*BERSCHAKTTEMP.I*/
DEFINE {&NEW} {&SHARED} TEMP-TABLE berschakttemp NO-UNDO
   FIELD AONR        AS CHARACTER FORMAT "X(6)"                  LABEL "AONR"             
   FIELD DELNR       AS INTEGER   FORMAT 999          INITIAL 0  LABEL "DELNR"            
   FIELD PUNKT1      AS INTEGER   FORMAT ">>>9"       INITIAL 0                    
   FIELD PUNKT2      AS INTEGER   FORMAT ">>>9"       INITIAL 0                    
   FIELD LANGD       AS INTEGER   FORMAT ">>>>>9"    INITIAL 0  LABEL "Längd"            
   FIELD ANMARK      AS CHARACTER FORMAT "X(50)"                 LABEL "Anmärkning"       
   FIELD DATUM       AS DATE      FORMAT "99/99/99"   INITIAL ?  LABEL "Datum"            
   FIELD DJUP        AS DECIMAL   FORMAT "->>>>9.99" INITIAL 0  LABEL "Djup"             
   FIELD BREDD       AS DECIMAL   FORMAT "->>>>9.99" INITIAL 0  LABEL "Bredd"            
   FIELD KSKYDD      AS CHARACTER FORMAT "X(11)"                 LABEL "Kabelskydd"       
   FIELD FORLAGG     AS CHARACTER FORMAT "X(10)"                 LABEL "Förläggningssätt" 
   FIELD YTBELAGG    AS CHARACTER FORMAT "X(20)"                 LABEL "Ytbeläggning"     
   FIELD NUM1        AS INTEGER   FORMAT ">>>9"       INITIAL 0                    
   FIELD NUM2        AS INTEGER   FORMAT ">>>9"       INITIAL 0                    
   FIELD EGEN        AS LOGICAL   FORMAT "Ja/Nej"     INITIAL NO LABEL "Egen"             
   FIELD OMRADE      AS CHARACTER FORMAT "x(6)"                  LABEL "Område"           
   INDEX OMR IS PRIMARY OMRADE AONR.

