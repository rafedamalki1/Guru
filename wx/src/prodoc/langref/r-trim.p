/* r-trim.p */
DEFINE VARIABLE menu AS CHARACTER EXTENT 3.
DO WHILE TRUE:
    DISPLAY
      "   1.  Display Customer Data" @ menu[1] SKIP
      "   2.  Display Order Data"   @ menu[2] SKIP
      "   3.  Exit"                 @ menu[3] SKIP
	  WITH FRAME choices NO-LABELS.
    CHOOSE FIELD menu AUTO-RETURN WITH FRAME choices
       TITLE "Demonstration Menu" CENTERED ROW 10.
    HIDE FRAME choices.
    IF TRIM(FRAME-VALUE) BEGINS "1" THEN RUN r-dblnkc.p.
    IF TRIM(FRAME-VALUE) BEGINS "2" THEN RUN r-dblnko.p.
    IF TRIM(FRAME-VALUE) BEGINS "3" THEN LEAVE.
END.
