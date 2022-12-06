/* r-pdbnm1.p */
IF SEARCH(PDBNAME(1) + ".db") = ? THEN DO:
BELL.
MESSAGE
  "Your database has been deleted, so this session will now terminate.".
PAUSE.
QUIT.
END.
