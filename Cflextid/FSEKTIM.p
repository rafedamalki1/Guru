/* FSEKTIM.P Omvandlar tiden i sekunder till tiden i timmar och minuter */

DEFINE SHARED VARIABLE sekunder AS INTEGER FORMAT "-9999999" NO-UNDO.
DEFINE SHARED VARIABLE fnytid AS DECIMAL NO-UNDO.
DEFINE VARIABLE minuter AS INTEGER NO-UNDO.
DEFINE VARIABLE hjsek AS INTEGER NO-UNDO.
IF sekunder LE -86400 THEN DO:
   hjsek = 0 - sekunder.
   minuter = hjsek MOD 3600.
   fnytid =  0 - (((hjsek - minuter) / 3600) + (minuter / 6000)).

END.
ELSE IF sekunder < 0 THEN DO:
   hjsek = 0 - sekunder.
   fnytid = 0 - ( DECIMAL(SUBSTRING(STRING(hjsek,"HH:MM"),1,2)) +
           DECIMAL(SUBSTRING(STRING(hjsek,"HH:MM"),4,2)) / 100).  
END.

ELSE IF sekunder < 86400 THEN DO:
   fnytid = DECIMAL(SUBSTRING(STRING(sekunder,"HH:MM"),1,2)) +
           DECIMAL(SUBSTRING(STRING(sekunder,"HH:MM"),4,2)) / 100. 
END.
ELSE DO:
   minuter = sekunder MOD 3600.
   fnytid = ((sekunder - minuter) / 3600) + (minuter / 6000).
END.     
  
