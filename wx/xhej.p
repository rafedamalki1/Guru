/*
output to a:\h.txt.


                              
DEFINE VARIABLE i AS INTEGER.
DEFINE TEMP-TABLE provag
   FIELD VAG AS CHARACTER.

REPEAT i=1 TO NUM-ENTRIES(PROPATH):
    CREATE provag.
    provag.VAG = STRING(ENTRY(i,PROPATH),"x(78)").
END.
FOR EACH provag:
   DISPLAY provag.VAG FORMAT "x(75)".
END.
*/
MESSAGE "HEJ!" VIEW-AS ALERT-BOX.
