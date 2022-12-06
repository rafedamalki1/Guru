/* PROGRESS PostScript Interface */
/* psgentxt.i */

DEFINE VARIABLE j AS INTEGER NO-UNDO.

OUTPUT TO VALUE(graph-file) APPEND.

PUT "       % Header, footer or other text fonts and sizes. " SKIP (1).

FOR EACH textline j = 1 TO num-lines WITH NO-LABELS:
		/* Generate the font definition line */
    PUT "\/" + textfont + " findfont " + STRING(textsize) +
	" scalefont setfont " FORMAT "x(60)" SKIP(0).

    IF (NOT textctr) THEN
	PUT " (" + textstr + ") " FORMAT "x(60)" textxpos textypos
	    " PrtText" SKIP(1).
    ELSE DO:
	PUT "612 " textxpos " sub (" + textstr + ") " FORMAT "x(60)"
	    textxpos textypos " PrtCenterText" SKIP (1).
    END.
END.

PUT "       % Label font and size. " SKIP(1).

PUT "\/" + label-font + " findfont " + STRING(label-size) +
	" scalefont setfont " FORMAT "x(60)" SKIP(1).

OUTPUT CLOSE.

HIDE ALL NO-PAUSE.
