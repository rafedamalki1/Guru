/*xoutput.p*/
DEFINE NEW SHARED VARIABLE globforetag LIKE FORETAG.FORETAG NO-UNDO.
FIND FIRST FORETAG NO-LOCK NO-ERROR.
globforetag = FORETAG.FORETAG.
IF globforetag = "GKAL" THEN DO:
  OUTPUT TO \\extraguru\guru_ser\server\PRO9S\kalle.txt.
  PUT "1".
  PUT SKIP.
  OUTPUT CLOSE.
END.
IF globforetag = "GKAL" THEN DO:
  
  OUTPUT TO D:\DELAD\SERVER\PRO9S\kalle2.txt.
  PUT "2".
  PUT SKIP.
  OUTPUT CLOSE.
END.
IF globforetag = "GKAL" THEN DO:
  
  OUTPUT TO \\192.121.247.104\server\PRO9S\kalle3.txt.
  PUT "3".
  PUT SKIP.
  OUTPUT CLOSE.
END.
