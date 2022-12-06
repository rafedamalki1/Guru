/*BESTKUNDALLT.I*/
DEFINE VARIABLE bestvad AS INTEGER NO-UNDO.
DEFINE {&NEW} {&SHARED} TEMP-TABLE bestkundallt NO-UNDO
   FIELD ADRESS                    AS CHARACTER              FORMAT "X(25)"
   FIELD AOKUND                    AS LOGICAL                FORMAT "Ja/Nej" INITIAL TRUE
   FIELD BESTID                    AS CHARACTER              FORMAT "X(6)"
   FIELD BESTNAMN                  AS CHARACTER              FORMAT "X(16)"
   FIELD FAKADRESS                 AS CHARACTER              FORMAT "X(25)"
   FIELD FAKORT                    AS CHARACTER              FORMAT "X(15)"
   FIELD FAKPNR                    AS CHARACTER              FORMAT "XXX XX"
   FIELD FAXNR                     AS CHARACTER              FORMAT "X(11)"
   FIELD KONTAKT                   AS CHARACTER              FORMAT "X(25)"
   FIELD KUNDPRISF                 AS DECIMAL                FORMAT ">>9.99"  INITIAL 1
   FIELD LEVADRESS                 AS CHARACTER              FORMAT "X(25)"
   FIELD LEVORT                    AS CHARACTER              FORMAT "X(15)"
   FIELD LEVPNR                    AS CHARACTER              FORMAT "XXX XX"
   FIELD ORT                       AS CHARACTER              FORMAT "X(15)"
   FIELD PNR                       AS CHARACTER              FORMAT "XXX XX"
   FIELD TEL                       AS CHARACTER              FORMAT "X(11)"
   FIELD VIBESTID                  AS CHARACTER              FORMAT "X(6)"
   INDEX BESTID IS PRIMARY BESTID
   INDEX VIBESTID VIBESTID
   INDEX BESTNAMN BESTNAMN.
DEFINE {&NEW} {&SHARED} TEMP-TABLE bestkundextra NO-UNDO
   FIELD BESTID                    AS CHARACTER              FORMAT "X(6)"
   FIELD BESTNAMN                  AS CHARACTER              FORMAT "X(16)"
   FIELD VIBESTID                  AS CHARACTER              FORMAT "X(6)"
   FIELD AVDELNING                 AS CHARACTER
   FIELD EPOST                     AS CHARACTER
   INDEX BESTID IS PRIMARY BESTID
   INDEX VIBESTID VIBESTID
   INDEX BESTNAMN BESTNAMN
   INDEX AVDELNING AVDELNING.
