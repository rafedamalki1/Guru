
/*DBBACKAI.I*/
   IF CONNECTED(LDBNAME(1)) THEN DO:
      OUTPUT TO VALUE(prognamnque)  APPEND.
      PUT "SEK UPPDATE START " dbfilename " "  TODAY " " STRING(TIME,"HH:MM:SS") SKIP.
      OUTPUT CLOSE.
      RUN SETKUND.P.
      RUN SEKDBSET.P.
      OUTPUT TO VALUE(prognamnque)  APPEND.
      PUT "BACKUPP START " dbfilename " "  TODAY " " STRING(TIME,"HH:MM:SS") SKIP.
      OUTPUT CLOSE.
      IF dbfilename NE "" THEN RUN DBBACKAI.P (INPUT prognamnque,INPUT dbfilename, INPUT "dbkopia\",INPUT valdbtemp.WWWFTP).
      DISCONNECT VALUE(LDBNAME(1)) NO-ERROR.
   END.
   ELSE DO:
      OUTPUT TO VALUE(prognamnque)  APPEND.
      PUT "GICK INTE ATT ANSLUTA " dbfilename " "  TODAY " " STRING(TIME,"HH:MM:SS") SKIP.
      OUTPUT CLOSE.
   END.      