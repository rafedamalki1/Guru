/*SPRAKTEMP.I*/
DEFINE {&STATIC} TEMP-TABLE spraktemp NO-UNDO
   BEFORE-TABLE spraktempbef
   FIELD ID           AS  INTEGER                              
   FIELD BENAMNING    AS  CHARACTER                            
   FIELD TTROWID      AS  ROWID
   FIELD TTRECID      AS  RECID
   FIELD ANDRAD       AS  LOGICAL
   INDEX SPRAKID IS PRIMARY ID.

   
DEFINE {&STATIC} TEMP-TABLE sprakstrangtemp NO-UNDO
   BEFORE-TABLE sprakstrangtempbef
   FIELD ID           AS  INTEGER
   FIELD SPRAKID      AS  INTEGER
   FIELD SOKID        AS  INTEGER                              
   FIELD BENAMNING    AS  CHARACTER
   FIELD SOKCHAR    AS  CHARACTER                                   
   FIELD TTROWID      AS  ROWID
   FIELD TTRECID      AS RECID
   FIELD ANDRAD       AS LOGICAL
   INDEX SOKCHAR SOKCHAR SPRAKID
   INDEX SPRAK IS PRIMARY SOKID SPRAKID
   INDEX ID ID
   INDEX SOKID SOKID.
DEFINE TEMP-TABLE textsprakstrangtemp NO-UNDO LIKE sprakstrangtemp.   
DEFINE TEMP-TABLE espraktemp NO-UNDO LIKE spraktemp.   
DEFINE TEMP-TABLE esprakstrangtemp NO-UNDO LIKE sprakstrangtemp.   
DEFINE {&STATIC} TEMP-TABLE sprakstrangtempc NO-UNDO LIKE sprakstrangtemp .
DEFINE {&STATIC} TEMP-TABLE sprakstrangtempcf NO-UNDO LIKE sprakstrangtemp .
DEFINE {&STATIC} TEMP-TABLE spraktempc NO-UNDO LIKE spraktemp .
DEFINE {&STATIC} TEMP-TABLE spraktempcf NO-UNDO LIKE spraktemp .