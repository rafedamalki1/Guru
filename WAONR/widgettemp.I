/*widgettemp.I*/
DEFINE TEMP-TABLE widgettemp NO-UNDO
   FIELD WIDGETHAND AS HANDLE
   FIELD WIDGETNAMN AS CHARACTER
   FIELD WIDGETTYP AS CHARACTER
   FIELD ANDHIDDEN AS INTEGER
   INDEX WIDGETTYP WIDGETTYP
   INDEX WIDGETHAND WIDGETHAND
   INDEX ANDHIDDEN ANDHIDDEN.
DEFINE TEMP-TABLE widgettemp2 NO-UNDO LIKE widgettemp
   FIELD ANDFRAME AS INTEGER.
