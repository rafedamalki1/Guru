/* r-colphr.p */

DEFINE VARIABLE hilite AS CHARACTER EXTENT 3.
DEFINE VARIABLE loop AS INTEGER.

hilite[1] = "NORMAL".
hilite[2] = "INPUT".
hilite[3] = "MESSAGES".

REPEAT WHILE loop <= 10:
    FORM bar AS CHARACTER WITH ROW(RANDOM(3,17)) COLUMN(RANDOM(5,50))
	 NO-BOX NO-LABELS FRAME bursts.
    COLOR DISPLAY VALUE(hilite[RANDOM(1,3)]) bar WITH FRAME bursts.
    DISPLAY FILL("*",RANDOM(1,8)) @ bar WITH FRAME bursts.
    PAUSE 1 NO-MESSAGE.
    HIDE FRAME bursts NO-PAUSE.
    loop = loop + 1.
END.
