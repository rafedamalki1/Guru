/* r-codpag.p */

DEFINE VARIABLE cp850string AS CHARACTER INITIAL "text with umlaut (a)".
DEFINE VARIABLE charsetstring AS CHARACTER.

charsetstring = CODEPAGE-CONVERT(cp850string, SESSION:CHARSET, "ibm850").

FOR EACH item:
    IF LOOKUP(charsetstring, item.cat-description) > 0 THEN DO:
        DISPLAY item.item-name.
    END.
END.
