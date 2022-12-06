/*MTRLTEMPc.I*/
DEFINE {&STATIC} TEMP-TABLE mtrltemp NO-UNDO
   FIELD Enr            AS CHARACTER                         
   FIELD Benamning      AS CHARACTER                            
   FIELD Bestkvant      AS DECIMAL                          
   FIELD Enhet          AS CHARACTER                    
   FIELD BERKVANT       AS DECIMAL
   FIELD SUMMA          AS DECIMAL
   FIELD LEVKOD         AS CHARACTER
   FIELD BPRIS          AS DECIMAL
   FIELD NPRIS          AS DECIMAL
   FIELD KALKNR         AS INTEGER
   FIELD BERLEV         AS CHARACTER                           
   FIELD OFFERT         AS LOGICAL
   FIELD KPRIS          AS DECIMAL
   FIELD KUND           AS LOGICAL
   FIELD MTRLROW        AS ROWID
   FIELD TTRECID        AS RECID
   FIELD ANDRAD         AS LOGICAL
   INDEX BEN LEVKOD KALKNR BENAMNING
   INDEX Enr IS PRIMARY enr
   INDEX Lev LEVKOD KALKNR enr.
   
DEFINE TEMP-TABLE emtrltemp NO-UNDO LIKE mtrltemp.
DEFINE {&STATIC} TEMP-TABLE  kmtrltemp NO-UNDO LIKE mtrltemp.  /*kopia av bermtrl för att märka nettopris*/
DEFINE {&STATIC} TEMP-TABLE  mkmtrltemp NO-UNDO LIKE mtrltemp.  /*kopia av för mtrlmeny.p nettopris*/
DEFINE TEMP-TABLE ikmtrltemp NO-UNDO LIKE mtrltemp.  /*kopia av bermtrl för att märka nettopris inköp*/
DEFINE TEMP-TABLE eikmtrltemp NO-UNDO LIKE mtrltemp.  /*kopia av bermtrl för att märka nettopris inköp*/
DEFINE TEMP-TABLE ekmtrltemp NO-UNDO LIKE mtrltemp.  /*kopia kmtrltemp för kommentarer*/

        
DEFINE {&STATIC} TEMP-TABLE mtrldeptemp NO-UNDO
   FIELD Benamning      AS    CHARACTER                            
   FIELD BERKVANT       AS    DECIMAL
   FIELD Bestkvant      AS    DECIMAL     
   FIELD BESTPUNKT      AS    DECIMAL
   FIELD BPRIS          AS    DECIMAL
   FIELD DEPNR          AS    INTEGER
   FIELD Enhet          AS    CHARACTER                    
   FIELD Enr            AS    CHARACTER                        
   FIELD FACKID         AS    CHARACTER                          
   FIELD IB             AS    INTEGER                         
   FIELD IBDATUM        AS    DATE                         
   FIELD INVANT         AS    INTEGER                         
   FIELD INVDATUM       AS    DATE  
   FIELD LAGER          AS    LOGICAL     
   FIELD LEVKOD         AS    CHARACTER
   FIELD NPRIS          AS    DECIMAL
   FIELD OMSATT         AS    DECIMAL                         
   FIELD SALDO          AS    INTEGER        
   FIELD TTRECID        AS    RECID                 
   INDEX Enr IS PRIMARY depnr enr
   INDEX DEPNR DEPNR.

DEFINE {&STATIC} TEMP-TABLE satstemp NO-UNDO
  FIELD KOD          AS CHARACTER       
  FIELD BENAMNING    AS CHARACTER 
  FIELD ENR          AS CHARACTER       
  FIELD ENHET        AS CHARACTER     
  FIELD ANTAL        AS INTEGER     
  FIELD PRIS         AS DECIMAL      
  FIELD LEVKOD       AS CHARACTER    
  FIELD SATS         AS LOGICAL      
  FIELD ENR2         AS CHARACTER       
  FIELD BENAMNING2   AS CHARACTER 
  FIELD PRIS2        AS DECIMAL      
  FIELD ENHET2       AS CHARACTER                       
  FIELD SATSROW      AS ROWID
  FIELD TTRECID      AS RECID
  INDEX KOD IS PRIMARY SATS LEVKOD KOD ENR ENR2.
DEFINE TEMP-TABLE emtrldeptemp NO-UNDO LIKE mtrldeptemp.

