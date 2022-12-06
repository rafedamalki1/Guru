/*AVTALAONR.I*/
DEFINE {&NEW} {&SHARED} TEMP-TABLE avtalaonrtemp NO-UNDO
   FIELD AONR         AS CHARACTER  FORMAT    "X(6)"                          
   FIELD DELNR        AS INTEGER    FORMAT     999            INITIAL 0       
   FIELD ORDN         AS INTEGER    FORMAT     "->>>>>>9"   INITIAL 0       
   FIELD FIL          AS CHARACTER  FORMAT      "x(8)"                        
   FIELD ANM          AS CHARACTER  FORMAT      "x(8)"                        
   FIELD AVTID        AS INTEGER    FORMAT     "->>>>>>9"   INITIAL 0       
   FIELD BEN          AS CHARACTER  FORMAT     "x(8)"                         
   FIELD KLR          AS DATE       FORMAT     "99/99/99"     INITIAL ? LABEL "Klar"      
   FIELD LNK          AS LOGICAL    FORMAT     "Ja/Nej"       INITIAL TRUE    
   FIELD AOAVTALREC   AS RECID
   FIELD ORDNING    AS INTEGER    FORMAT    "->>>>>>9"     INITIAL 0      
   FIELD AVTAL      AS CHARACTER  FORMAT      "x(30)"                       
   FIELD FILNAMN    AS CHARACTER  FORMAT      "x(8)"                        
   FIELD ANMARK     AS CHARACTER  FORMAT      "x(8)"                        
   FIELD AVTALID    AS INTEGER    FORMAT   "->>>>>>9"     INITIAL 0       
   FIELD LANK       AS LOGICAL    FORMAT    "Ja/Nej"         INITIAL TRUE   
   INDEX AOAVTAL IS PRIMARY AONR DELNR AVTID ORDN LNK
   INDEX ID AVTALID 
   INDEX ORD ORDNING.
