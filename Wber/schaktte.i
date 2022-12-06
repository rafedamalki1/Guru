/*schaktte.i DEFINITIONER TEMP-TABLE schakt_temp BERSCHAKT*/  
      
DEFINE {&NEW} {&SHARED} TEMP-TABLE schakt_temp NO-UNDO
   FIELD NUM1        AS INTEGER FORMAT ">>>9"
   FIELD NUM2        AS INTEGER FORMAT ">>>9"
   FIELD PUNKT1      AS INTEGER FORMAT ">>>9"
   FIELD PUNKT2      AS INTEGER FORMAT ">>>9"
   FIELD LANGD       AS INTEGER FORMAT ">>>>>9"
   FIELD DJUP        AS DECIMAL FORMAT "->>>>9.99"
   FIELD BREDD       AS DECIMAL FORMAT "->>>>9.99"
   FIELD KSKYDD      AS CHARACTER FORMAT "X(11)"
   FIELD FORLAGG     AS CHARACTER FORMAT "X(10)"
   FIELD YTBELAGG    AS CHARACTER FORMAT "X(20)"
   FIELD ANMARK      AS CHARACTER FORMAT "X(50)"
   FIELD EGEN        AS LOGICAL FORMAT "Ja/Nej" INITIAL NO
   FIELD KLAR        AS LOGICAL. 
        
   
  
   
 
   
