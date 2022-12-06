/* r-progfn.p */
/* Depending on the version of PROGRESS you are running, */
/* the main menu reflects available features for end-user */

DEFINE VARIABLE menu AS CHARACTER EXTENT 3.
DEFINE VARIABLE exit-prompt AS CHARACTER.

IF PROGRESS EQ "FULL" THEN
    exit-prompt = " 3. Return to Full Editor ".
ELSE IF PROGRESS EQ "QUERY" THEN
    exit-prompt = " 3. Return to Query Editor".
ELSE IF PROGRESS EQ "RUN-TIME" THEN
    exit-prompt = "3. Exit Program".

DO WHILE TRUE:
    DISPLAY
      "  1. Display Customer Data" @ menu[1] SKIP
      "  2. Display Order Data"    @ menu[2] SKIP
      exit-prompt                 @ menu[3]
	FORMAT "x(26)" SKIP
	WITH FRAME choices NO-LABELS.

CHOOSE FIELD menu AUTO-RETURN WITH FRAME choices
    TITLE "Demonstration menu" CENTERED ROW 10.
HIDE FRAME choices.
IF FRAME-INDEX EQ 1 THEN MESSAGE
    "You picked option 1.".
ELSE IF FRAME-INDEX EQ 2 THEN MESSAGE
    "You picked option 2.".
ELSE IF FRAME-INDEX EQ 3 THEN RETURN.
END.
