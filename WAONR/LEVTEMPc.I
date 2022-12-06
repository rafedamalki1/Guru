/*LEVTEMPc.I*/
DEFINE {&STATIC} TEMP-TABLE levtemp NO-UNDO
   FIELD LEVKOD     AS  CHARACTER                              
   FIELD LEVNAMN    AS  CHARACTER                            
   FIELD LEVADR     AS  CHARACTER                             
   FIELD LEVORT     AS  CHARACTER                             
   FIELD LEVTEL     AS  CHARACTER                      
   FIELD LEVKONTAKT AS  CHARACTER                             
   FIELD LEVPNR     AS  CHARACTER                              
   FIELD BORTTAG    AS  LOGICAL
   FIELD TTRECID    AS  RECID
   FIELD ORDNING AS INTEGER INITIAL 999
   INDEX LEV IS PRIMARY ORDNING LEVKOD.
   
   
DEFINE {&STATIC} TEMP-TABLE huvlevtemp NO-UNDO
   FIELD LEVKOD     AS CHARACTER
   FIELD Dep-nr     AS INTEGER
   FIELD TTRECID    AS  RECID
   INDEX DEPNR IS PRIMARY Dep-nr. 


DEFINE TEMP-TABLE extralevtemp NO-UNDO LIKE levtemp.
/*DEFINE TEMP-TABLE huvlevtemp NO-UNDO LIKE levtemp.*/