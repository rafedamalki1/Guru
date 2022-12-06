/*VARDRB.P*/  
/*
DEFINE INPUT PARAMETER magare LIKE MARKAGARE.MARKNR NO-UNDO.
DEFINE INPUT PARAMETER varder LIKE VARDERING.VARDNR NO-UNDO.
*/
DEFINE VARIABLE rb-filter AS CHARACTER NO-UNDO. 
DEFINE VARIABLE magare LIKE MARKAGARE.MARKNR NO-UNDO.
DEFINE VAR varder LIKE VARDERING.VARDNR NO-UNDO.
magare = 10.
varder = 4007.
  

FIND FIRST FORETAG NO-LOCK NO-ERROR.
IF FORETAG.FORETAG = "ELPA" THEN DO:
   rb-filter = " VARDERING.VARDNR = " + STRING(varder) + " AND " + " MARKAGARE.MARKNR = " + STRING(magare). 
   RUN F:\pro8\dlc\gui\aderb\_printrb.p
   ("f:\pro8\guru\wmark\reports.prl",
   "vardtest",
   "rt8 -H pc012 -S elpoolserver -N tcp -U ELPAO -P KAGGEN",
   "O",
   rb-filter,   
   "",
   "D",
   "",
   "",
   "",
   1,
   1,
   99999,
   no,
   "",
   no,
   no,
   no,
   "").   
END.
