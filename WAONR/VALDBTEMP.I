/*VALDBTEMP.I*/
DEFINE TEMP-TABLE valdbtemp 
   FIELD ORDNING AS INTEGER
   FIELD FORETAG AS CHARACTER LABEL "F?retag"
   FIELD VALDB AS CHARACTER   LABEL "Databas"
   FIELD DBNAMN AS CHARACTER 
   FIELD DBCON AS CHARACTER 
   FIELD DBCACHE AS CHARACTER
   FIELD DBPLATS AS CHARACTER
   FIELD APPCON AS CHARACTER 
   FIELD GFORETAG AS CHARACTER
   FIELD WWWFTP AS LOGICAL
   FIELD WWWSTART AS CHARACTER
   FIELD WWWSTART10 AS CHARACTER
  
   INDEX ORDNING AS PRIMARY FORETAG ORDNING
   INDEX ORDNING2 ORDNING FORETAG 
   INDEX GFORETAG GFORETAG
   INDEX DBNAMN DBNAMN.
DEFINE BUFFER valdbtempbuff FOR valdbtemp.


     
     
     
   
     