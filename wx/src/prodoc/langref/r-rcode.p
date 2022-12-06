DEFINE VARIABLE rcode-file  AS CHARACTER FORMAT "x(60)" LABEL "File".

REPEAT:
   SET rcode-file WITH FRAME rc-info.

   RCODE-INFO:FILE-NAME = rcode-file.

   DISPLAY RCODE-INFO:CRC-VALUE LABEL "CRC"
           RCODE-INFO:LANGUAGES FORMAT "x(60)" LABEL "Languages"
           WITH FRAME rc-info SIDE-LABELS TITLE "R-code Check".
END.
