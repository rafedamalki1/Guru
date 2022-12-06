/*SUMTD.I*/

DEFINE {&NEW} {&SHARED} TEMP-TABLE sumtidtemp NO-UNDO
   FIELD DATUM        AS DATE     FORMAT "99/99/99"   INITIAL TODAY                    
   FIELD PRIS         AS DECIMAL FORMAT "->>>>9.99"  INITIAL 0           LABEL "PRIS"                
   FIELD PRISTYP      AS CHARACTER    FORMAT "X(9)"   INITIAL "TOT.PRIS." LABEL "PRISTYP"             
   FIELD PERSONALKOD  AS CHARACTER    FORMAT "x(5)"                       LABEL "ENHET/SIGN"          
   FIELD ORT          AS CHARACTER    FORMAT "x(40)"                      LABEL "ORT/BENÄMNING"       
   FIELD AONR         AS CHARACTER    FORMAT "X(6)"                                      
   FIELD DELNR        AS INTEGER    FORMAT 999        INITIAL 0                        
   FIELD OMRADE       AS CHARACTER    FORMAT "x(6)"                       LABEL "UTFÖRANDE OMRÅDE"    
   FIELD GEOMRADE     AS CHARACTER    FORMAT "x(8)"                                       
   FIELD TIMMAR       AS DECIMAL  FORMAT "->>>>9.99" INITIAL 0                       
   FIELD BELOPP       AS DECIMAL  FORMAT "->>>>9.99" INITIAL 0                           
   FIELD OBELOPP      AS DECIMAL  FORMAT "->>>>9.99" INITIAL 0                            
   FIELD TBELOPP      AS DECIMAL  FORMAT "->>>>9.99" INITIAL 0                           
   FIELD AUTODATUM    AS DATE     FORMAT "99/99/99"   INITIAL TODAY                        
   FIELD EFTERNAMN    AS CHARACTER    FORMAT "x(25)"                                         
   FIELD FORNAMN      AS CHARACTER    FORMAT "x(15)"                      LABEL "FÖRNAMN"             
   FIELD BEFATTNING   AS CHARACTER    FORMAT "x(15)"                                          
   FIELD PERSMASK     AS LOGICAL    FORMAT "JA/NEJ"   INITIAL YES                             
   FIELD FASTAAONR    AS LOGICAL     FORMAT "JA/NEJ"  INITIAL FALSE                       
   FIELD OTIMMAR      AS DECIMAL  FORMAT "->>>>9.99" INITIAL 0                             
   FIELD PRISI        AS DECIMAL  FORMAT "->>>>9.99" INITIAL 0           LABEL "INTERNPRIS"          
   FIELD LONKOST      AS DECIMAL  FORMAT "->>>>9.99" INITIAL 0           LABEL "ÖVRIGA KOSTNADER"    
   FIELD VECKOKORD    AS CHARACTER    FORMAT "x(4)"                       LABEL "VECKOKÖRDTIDSEDEL"   
   FIELD IKOSTNAD     AS DECIMAL  FORMAT "->>>>9.99" INITIAL 0                           
   INDEX AONR IS PRIMARY DATUM AONR DELNR
   INDEX GEOPRISTYP DATUM GEOMRADE PRISTYP
   INDEX GEORGAONR DATUM GEOMRADE AONR DELNR
   INDEX PERSONALKOD DATUM PERSONALKOD AONR DELNR
   INDEX PRISTYP DATUM PRISTYP.


DEFINE {&NEW} {&SHARED} TEMP-TABLE sumtiddagtemp NO-UNDO
  FIELD DATUM       AS DATE      FORMAT "99/99/99"   INITIAL TODAY              
  FIELD PRIS        AS DECIMAL   FORMAT "->>>>9.99" INITIAL 0            LABEL "PRIS"               
  FIELD PRISTYP     AS CHARACTER FORMAT "X(9)"       INITIAL "TOT.PRIS."  LABEL "PRISTYP"            
  FIELD PERSONALKOD AS CHARACTER FORMAT "x(5)"                    LABEL        "ENHET/SIGN"         
  FIELD ORT         AS CHARACTER FORMAT "x(40)"                  LABEL          "ORT/BENÄMNING"      
  FIELD AONR        AS CHARACTER FORMAT "X(6)"                                        
  FIELD DELNR       AS INTEGER   FORMAT 999           INITIAL 0                    
  FIELD OMRADE      AS CHARACTER FORMAT "x(6)"                            LABEL  "UTFÖRANDE OMRÅDE"   
  FIELD GEOMRADE    AS CHARACTER FORMAT "x(8)"                                      
  FIELD TIMMAR      AS DECIMAL   FORMAT "->>>>9.99"  INITIAL 0                  
  FIELD BELOPP      AS DECIMAL   FORMAT "->>>>9.99"  INITIAL 0                     
  FIELD OBELOPP     AS DECIMAL   FORMAT "->>>>9.99"  INITIAL 0                   
  FIELD TBELOPP     AS DECIMAL   FORMAT "->>>>9.99"  INITIAL 0                    
  FIELD AUTODATUM   AS DATE       FORMAT "99/99/99"   INITIAL TODAY                
  FIELD EFTERNAMN   AS CHARACTER  FORMAT "x(25)"                                
  FIELD FORNAMN     AS CHARACTER  FORMAT "x(15)"                          LABEL   "FÖRNAMN"            
  FIELD BEFATTNING  AS CHARACTER  FORMAT "x(15)"                                        
  FIELD PERSMASK    AS LOGICAL    FORMAT "JA/NEJ"     INITIAL YES                      
  FIELD FASTAAONR   AS LOGICAL   FORMAT  "JA/NEJ"     INITIAL FALSE               
  FIELD OTIMMAR     AS DECIMAL  FORMAT "->>>>9.99"   INITIAL 0                    
  FIELD PRISI       AS DECIMAL  FORMAT " ->>>>9.99"   INITIAL 0          LABEL    "INTERNPRIS"         
  FIELD LONKOST     AS DECIMAL  FORMAT "->>>>9.99"    INITIAL 0          LABEL    "ÖVRIGA KOSTNADER"   
  FIELD VECKOKORD   AS CHARACTER  FORMAT "x(4)"                           LABEL   "VECKOKÖRDTIDSEDEL"  
  FIELD IKOSTNAD    AS DECIMAL  FORMAT "->>>>9.99"    INITIAL 0                    
  INDEX AONR IS PRIMARY DATUM AONR DELNR
  INDEX BARAAONR AONR DELNR
  INDEX GEOPRISTYP DATUM GEOMRADE PRISTYP
  INDEX GEORGAONR DATUM GEOMRADE AONR DELNR
  INDEX PERSONALKOD DATUM PERSONALKOD AONR DELNR
  INDEX PRISTYP DATUM PRISTYP.
