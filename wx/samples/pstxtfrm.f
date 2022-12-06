/* PROGRESS PostScript Interface */

/* pstxtfrm.f (form definition include file)
   file name  : customer
   database name: psdb   */
"Label Font:" AT 1
label-font FORMAT "x(30)" AT 13 NO-LABEL
HELP "Enter label font"
SKIP (0)
"Point size:" AT 1
label-size FORMAT "999" AT 13 NO-LABEL
HELP "Enter label point size"
SKIP (2)
"Text:" AT 1
textstr FORMAT "x(60)" AT 7 NO-LABEL
HELP "Insert text exactly."
SKIP (0)
"Font:" AT 1
textfont FORMAT "x(30)" AT 7 NO-LABEL
HELP "Insert font (Times-Roman, Helvetica, Courier etc.)"
SKIP (0)
"Size:" AT 1
textsize FORMAT "999" AT 7 NO-LABEL
HELP "Enter point size"
SKIP (0)
"Center?:" AT 1
textctr FORMAT "yes/no" AT 10 NO-LABEL
HELP "Text to be centered?"
SKIP (0)
"X-coordinate:" AT 1
textxpos FORMAT "999" AT 15 NO-LABEL
HELP "Enter x-coordinate"
"Y-coordinate:" AT 37
textypos FORMAT "999" AT 51 NO-LABEL
HELP "Enter y-coordinate"
WITH SIDE-LABELS
TITLE COLOR MESSAGE "Text Entry Screen"
ROW 3 CENTERED  OVERLAY ATTR-SPACE 1 DOWN
