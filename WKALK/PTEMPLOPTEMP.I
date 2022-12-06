/*   Filename: PTEMPLOPTEMP.I*/

DEFINE {&NEW} {&SHARED} TEMP-TABLE ptemp1
   FIELD ARBKOD    AS CHARACTER FORMAT "X(5)" LABEL "Arbetskod"  
   FIELD BENAMNING AS CHARACTER FORMAT "X(40)" LABEL "Benämning"  
   FIELD KATAR     AS INTEGER FORMAT 9999 INITIAL ? LABEL "Katalog år"                              
   INDEX AR KATAR ARBKOD
   INDEX BEN BENAMNING
   INDEX KOD IS PRIMARY ARBKOD.
DEFINE {&NEW} {&SHARED} TEMP-TABLE ptemp2 NO-UNDO LIKE ptemp1.
DEFINE {&NEW} {&SHARED} TEMP-TABLE ptemp3 NO-UNDO LIKE ptemp1.
DEFINE {&NEW} {&SHARED} TEMP-TABLE ptemp5 NO-UNDO LIKE ptemp1.
DEFINE TEMP-TABLE eptemp NO-UNDO LIKE ptemp1.

DEFINE {&NEW} {&SHARED} TEMP-TABLE loptemp1 NO-UNDO                                                         
   FIELD ARBKOD    AS   CHARACTER FORMAT "X(5)"                         LABEL "Arbetskod"          
   FIELD LOPNR        AS INTEGER   FORMAT ">>>" INITIAL 0               LABEL "Löpnr"              
   FIELD BENAMNING    AS CHARACTER FORMAT "X(40)"                       LABEL "Benämning"          
   FIELD ENHET        AS CHARACTER FORMAT "X(3)"                        LABEL "Enhet"              
   FIELD F1           AS DECIMAL   FORMAT ">>>9.99" INITIAL 0           LABEL "F1"                 
   FIELD F2           AS DECIMAL   FORMAT ">>>9.99" INITIAL 0           LABEL "F2"                 
   FIELD F3           AS DECIMAL   FORMAT ">>>9.99" INITIAL 0           LABEL "F3"                 
   FIELD F4           AS DECIMAL   FORMAT ">>>9.99" INITIAL 0           LABEL "F4"                 
   FIELD F5           AS DECIMAL   FORMAT ">>>9.99" INITIAL 0           LABEL "F5"                 
   FIELD F6           AS DECIMAL   FORMAT ">>>9.99" INITIAL 0           LABEL "F6"                
   FIELD F7           AS DECIMAL   FORMAT ">>>9.99" INITIAL 0           LABEL "F7"                 
   FIELD F8           AS DECIMAL   FORMAT ">>>9.99" INITIAL 0           LABEL "F8"                 
   FIELD F9           AS DECIMAL   FORMAT ">>>9.99" INITIAL 0           LABEL "F9"                 
   FIELD F10          AS DECIMAL   FORMAT ">>>9.99" INITIAL 0           LABEL "F10"                
   FIELD EA           AS DECIMAL   FORMAT ">>>9.99" INITIAL 0           LABEL "EA"                 
   FIELD ARBETE       AS DECIMAL   FORMAT "->>>>9.99" INITIAL 0        LABEL "Arbete"             
   FIELD MATERIEL     AS DECIMAL   FORMAT "->>>>9.99" INITIAL 0        LABEL "Materiel"           
   FIELD MASKINKOST   AS DECIMAL   FORMAT "->>>>9.99" INITIAL 0        LABEL "Maskinkost."        
   FIELD OVRIGT       AS DECIMAL   FORMAT "->>>>9.99" INITIAL 0        LABEL "Övrigt"             
   FIELD KATAR        AS INTEGER   FORMAT 9999 INITIAL  ?               LABEL "Katalog år"         
   FIELD UTRUST       AS DECIMAL   FORMAT ">>>9.99" INITIAL 0           LABEL "Utrustning"         
   FIELD UTRUSTKOST   AS DECIMAL   FORMAT "->>>>9.99" INITIAL 0        LABEL "Utrustningskostnad" 
   FIELD FAST         AS LOGICAL   FORMAT "Ja/Nej" INITIAL NO           LABEL "Fast"                
   INDEX AR KATAR ARBKOD LOPNR
   INDEX LOP IS PRIMARY ARBKOD LOPNR
   INDEX P5 ARBKOD.
DEFINE {&NEW} {&SHARED} TEMP-TABLE loptemp2 NO-UNDO LIKE loptemp1.
DEFINE {&NEW} {&SHARED} TEMP-TABLE loptemp3 NO-UNDO LIKE loptemp1.                         
DEFINE {&NEW} {&SHARED} TEMP-TABLE loptemp5 NO-UNDO LIKE loptemp1.                          
DEFINE TEMP-TABLE eloptemp NO-UNDO LIKE loptemp1.                    
