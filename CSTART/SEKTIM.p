/* SEKTIM.P Omvandlar tiden i sekunder till tiden i timmar och minuter */

DEFINE SHARED VARIABLE sekunder AS INTEGER FORMAT "-9999999" NO-UNDO.
DEFINE SHARED VARIABLE nytid AS DECIMAL NO-UNDO.
DEFINE VARIABLE minuter AS INTEGER NO-UNDO.

IF sekunder < 86400 THEN DO:
   nytid = DECIMAL(SUBSTRING(STRING(sekunder,"HH:MM"),1,2)) +
           DECIMAL(SUBSTRING(STRING(sekunder,"HH:MM"),4,2)) / 100. 
END.
ELSE DO:
   minuter = sekunder MOD 3600.
   nytid = ((sekunder - minuter) / 3600) + (minuter / 6000).
END.     

  
