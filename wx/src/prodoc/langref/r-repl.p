DEFINE VARIABLE greeting AS CHARACTER FORMAT "x(40)"
                         INITIAL "Starting user's session . . .".

IF USERID("DICTDB") <> ""
THEN greeting = REPLACE(greeting, "user", USERID("DICTDB")).

DISPLAY greeting WITH NO-LABELS.
