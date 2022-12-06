/* r-fr-indx.p */
DEFINE VARIABLE menu AS CHARACTER EXTENT 3.
DO WHILE TRUE:
    DISPLAY
      "1. Display Customer Data" @ menu[1] SKIP
      "2. Display Order Data"    @ menu[2] SKIP
      "3  Exit"                   @ menu[3] SKIP
      WITH FRAME choices NO-LABELS.
    CHOOSE FIELD menu AUTO-RETURN WITH FRAME choices
      TITLE "Demonstration menu " CENTERED ROW 10.
    HIDE FRAME choices.
    IF FRAME-INDEX EQ 1 THEN
	MESSAGE "You picked option 1.".
    ELSE IF FRAME-INDEX EQ 2 THEN
	MESSAGE " You picked option 2.".
    ELSE IF FRAME-INDEX EQ 3 THEN LEAVE.
END.
