/*RBTERM.P*/
DEFINE VARIABLE pkod LIKE UTOBJEKT.PERSONALKOD NO-UNDO.
DEFINE  VARIABLE utslutvar AS LOGICAL NO-UNDO.
DEFINE VARIABLE rb-filter AS CHARACTER NO-UNDO.    
PKOD = "1".
FIND FIRST FORETAG NO-LOCK NO-ERROR.
   rb-filter = "UTOBJEKT.PERSONALKOD = "  + "'" + pkod + "'".       
   RUN F:\pro8\dlc\gui\aderb\_printrb.p
   ("f:\pro8\guru\wfast\prov.prl",
   "faxen2",
   "FAST8 -H ntserver1 -S faxserver -N tcp -U ELPAO -P KAGGEN",
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
   IF utslutvar = TRUE THEN DO:                
      rb-filter = "UTSLUT.PERSONALKOD = "  + "'" + pkod + "'".
      RUN F:\pro8\dlc\gui\aderb\_printrb.p
      ("f:\pro8\guru\wfast\prov.prl",
      "sokk",
      "FAST8 -H ntserver1 -S faxserver -N tcp -U ELPAO -P KAGGEN",
      "O",
      "",
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
