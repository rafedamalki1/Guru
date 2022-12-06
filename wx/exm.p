DEFINE TEMP-TABLE excellTT NO-UNDO
      FIELD KOLUMNRAD    AS CHARACTER
      FIELD KOLUMN       AS CHARACTER
      FIELD KOLUMNNUMMER AS INTEGER
      FIELD RAD          AS INTEGER
      FIELD VARDET       AS CHARACTER
      FIELD FONTNAMN     AS CHARACTER
      FIELD BOLD         AS LOGICAL
      FIELD FARG         AS INTEGER
      FIELD RADHOJD      AS DECIMAL
      INDEX KOLUMN    KOLUMN RAD
      INDEX RAD       RAD KOLUMNNUMMER 
      INDEX KOLUMNRAD KOLUMNRAD.

INPUT FROM C:\TEMP\p2.d.

   REPEAT:
      CREATE excellTT.
      ASSIGN.
      IMPORT excellTT.
   END.
   INPUT CLOSE.
  
 DEFINE VARIABLE startkalkroot AS Guru.excel NO-UNDO.  
 startkalkroot = NEW Guru.excel().
 
startkalkroot:StartExcel().
DEF VAR AA AS INTEGER.
AA = ETIME.
startkalkroot:valueDataOutTT(input table excellTT).
MESSAGE eTIME - AA VIEW-AS ALERT-BOX.

startkalkroot:SlutExcel().
 
