/* r-clear.p */

DEFINE VARIABLE a AS CHARACTER INITIAL "xxxxxxxx".
DEFINE VARIABLE b AS DATE INITIAL TODAY.
DEFINE VARIABLE c AS DECIMAL INITIAL "-12,345.67".
DEFINE VARIABLE d AS INTEGER INITIAL "-1,234,567".
DEFINE VARIABLE e AS LOGICAL INITIAL yes.

DISPLAY "This illustrates the default formats for the different data types"
     SKIP(2) WITH CENTERED ROW 4 NO-BOX FRAME head.
FORM "CHARACTER default format is ""x(8)""            "      a  SKIP
     "DATE default format is 99/99/99               "        b  SKIP
     "DECIMAL default format is ->>,>>9.99        "          c  SKIP
     "INTEGER default format is ->,>>>,>>9        "          d  SKIP
     "LOGICAL default format is yes/no                    "  e  TO 55 SKIP
     WITH ROW 8 NO-BOX NO-LABELS CENTERED FRAME ex.
REPEAT:
    DISPLAY a b c d e WITH FRAME ex.
    MESSAGE "Do you want to put in some values?" UPDATE e.
    IF e THEN DO:
	CLEAR FRAME ex NO-PAUSE.
	SET a b c d e WITH FRAME ex.
    END.
    ELSE LEAVE.
END.
