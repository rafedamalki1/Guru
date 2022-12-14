/*byggtemp.I */

DEFINE TEMP-TABLE byggtemp NO-UNDO
   FIELD BLADNR AS INTEGER
   FIELD DELBLAD AS INTEGER
   FIELD RADNR AS INTEGER
   FIELD ERADNR AS INTEGER
   FIELD BENAMNING AS CHARACTER
   FIELD BYGG AS CHARACTER EXTENT 10
   FIELD ENR AS CHARACTER
   FIELD ANTAL AS DECIMAL
   FIELD ANM AS CHARACTER
   FIELD NUMMER AS INTEGER    EXTENT 10
   FIELD GRUPP AS INTEGER    EXTENT 10
   FIELD F1 AS CHARACTER    EXTENT 10
   FIELD F2 AS CHARACTER    EXTENT 10
   FIELD F3 AS CHARACTER    EXTENT 10
   FIELD F4 AS CHARACTER    EXTENT 10
   FIELD F5 AS CHARACTER    EXTENT 10
   FIELD F6 AS CHARACTER    EXTENT 10
   FIELD F1NAMN AS CHARACTER    EXTENT 10
   FIELD F2NAMN AS CHARACTER    EXTENT 10
   FIELD F3NAMN AS CHARACTER    EXTENT 10
   FIELD F4NAMN AS CHARACTER    EXTENT 10
   FIELD F5NAMN AS CHARACTER    EXTENT 10
   FIELD F6NAMN AS CHARACTER    EXTENT 10
   FIELD SIDBRYT AS LOGICAL
   INDEX ORD IS PRIMARY BLADNR DELBLAD RADNR
   INDEX ENR BLADNR DELBLAD ENR.
   
   DEFINE BUFFER byggtempbuff FOR byggtemp.
   DEFINE TEMP-TABLE ebyggtemp LIKE byggtemp.














