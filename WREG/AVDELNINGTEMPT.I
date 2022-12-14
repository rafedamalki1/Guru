/*AVDELNINGTEMPT.I*/
DEFINE {&NEW} {&SHARED} TEMP-TABLE avdelningtemp  NO-UNDO
   FIELD ANTALAB          AS INTEGER
   FIELD ANTALANST        AS INTEGER
   FIELD AVDELNINGNAMN    AS CHARACTER FORMAT "X(40)"
   FIELD AVDELNINGNR      AS INTEGER
   FIELD BUDGRAMINV       AS INTEGER
   FIELD BUDGRAMUH        AS INTEGER
   FIELD ELVOMRKOD        AS INTEGER
   FIELD GATUADR          AS CHARACTER FORMAT "X(40)"
   FIELD KONTOA           AS CHARACTER
   FIELD KONTOB           AS CHARACTER
   FIELD KONTOC           AS CHARACTER
   FIELD KONTOD           AS CHARACTER
   FIELD KOSTMASK         AS INTEGER
   FIELD KOSTNTYPINV      AS CHARACTER
   FIELD KOSTNTYPUH       AS CHARACTER
   FIELD KOSTPERS         AS INTEGER
   FIELD NUAK             AS INTEGER
   FIELD POSTADR          AS CHARACTER  FORMAT "X(40)"
   FIELD POSTANST         AS CHARACTER  FORMAT "X(40)"
   FIELD POSTNR           AS CHARACTER
   FIELD TELJOUR          AS CHARACTER  FORMAT "X(40)"
   FIELD TELVXL           AS CHARACTER  FORMAT "X(40)"
   INDEX AVDELNINGNR IS PRIMARY AVDELNINGNR
   INDEX AVDELNINGNAMN AVDELNINGNAMN.         
/*    INDEX AVDELNING AVDELNINGNR. */
