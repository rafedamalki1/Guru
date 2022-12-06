/* r-cursor.p */

DEFINE VARIABLE comment AS CHARACTER FORMAT "x(30)" EXTENT 4.
DEFINE VARIABLE r       AS INTEGER.
DEFINE VARIABLE c       AS INTEGER.
DEFINE VARIABLE lmargin AS INTEGER INITIAL 5.
DEFINE VARIABLE rmargin AS INTEGER INITIAL 34.
DEFINE VARIABLE ptop    AS INTEGER INITIAL 10.
DEFINE VARIABLE pbot    AS INTEGER INITIAL 13.
DEFINE VARIABLE r-ofst  AS INTEGER INITIAL 9.
DEFINE VARIABLE c-ofst  AS INTEGER INITIAL 4.
FORM SKIP(4) WITH WIDTH 32 ROW 9 COL 4 TITLE "Editor".
MESSAGE "Type text into the editor.  Press" KBLABEL("GO") "to end.".
VIEW.
r = ptop.
c = lmargin.
REPEAT:
   PUT CURSOR ROW r COLUMN c.
   READKEY.
   IF KEYFUNCTION(LASTKEY) = "GO" THEN LEAVE.
   IF KEYFUNCTION(LASTKEY) = "END-ERROR" THEN RETURN.
   IF LASTKEY = KEYCODE("CURSOR-RIGHT") THEN DO:
      c = c + 1.
      IF c > rmargin
      THEN c = lmargin.
      NEXT.
   END.
   IF LASTKEY = KEYCODE("CURSOR-LEFT") THEN DO:
      c = c - 1.
      IF c < lmargin
      THEN c = rmargin.
      NEXT.
   END.
   IF LASTKEY = KEYCODE("CURSOR-DOWN") THEN DO:
      r = r + 1.
      IF r > pbot
      THEN r = ptop.
      NEXT.
   END.
   IF LASTKEY = KEYCODE("CURSOR-UP") THEN DO:
      r = r - 1.
      IF r < ptop
      THEN r = pbot.
      NEXT.
   END.
   IF LASTKEY = KEYCODE("RETURN") THEN DO :
       r = r + 1.
       IF r > pbot
       THEN r = ptop.
       c = lmargin.
       NEXT.
   END.
   IF LASTKEY = KEYCODE("BACKSPACE") THEN DO:
      IF c = lmargin AND r = ptop
      THEN NEXT.
      c = c - 1.
      IF c < lmargin
      THEN DO:
	 c = rmargin.
	 r = r - 1.
	 IF r < ptop
	 THEN r = ptop.
      END.
      PUT SCREEN ROW r COLUMN c " ".
      OVERLAY(comment[r - r-ofst], c - c-ofst) = " ".
      NEXT.
   END.
   IF LASTKEY >= 32 AND LASTKEY <= 126
   THEN DO:
      PUT SCREEN ROW r COLUMN c KEYLABEL(LASTKEY).
      OVERLAY(comment[r - r-ofst], c - c-ofst) = KEYLABEL(LASTKEY).

      c = c + 1.
      IF c > rmargin
      THEN DO:
	 c = lmargin.
	 r = r + 1.
	 IF r > pbot
	 THEN r = ptop.
      END.
   END.
END.

DISPLAY comment WITH FRAME x NO-LABELS
	 TITLE "Comments Array" 1 COLUMN ROW 09 COLUMN 40.
MESSAGE "Information stored in the comments array.".
