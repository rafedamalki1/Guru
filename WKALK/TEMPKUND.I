/*TEMPKUND.I*/
DEFINE {&NEW} {&SHARED} TEMP-TABLE tempkund
   FIELD KUNDNR AS INTEGER
   FIELD KUNDNAMN AS CHARACTER
   FIELD ADRESS AS CHARACTER
   FIELD PNR AS CHARACTER
   FIELD TEL AS CHARACTER
   FIELD ORT AS CHARACTER
   FIELD KONTAKT AS CHARACTER
   INDEX KUNDNR KUNDNR ASCENDING.
DEFINE TEMP-TABLE etempkund LIKE tempkund.
