/* r-chsmnu.p */

DEFINE VARIABLE menu AS CHARACTER EXTENT 4 FORMAT "x(7)"
   INITIAL [ "Browse", "Create", "Update", "Exit" ].

DEFINE VARIABLE proglist AS CHARACTER EXTENT 4
   INITIAL [ "brws.p", "cre.p", "upd.p", "exit.p"].

DEFINE VARIABLE i AS INTEGER.
FORM "Use the sample strip menu to select an action."
  WITH FRAME instruc CENTERED ROW 10.

REPEAT:
   VIEW FRAME instruc.
   DISPLAY menu WITH NO-LABELS ROW 13 NO-BOX ATTR-SPACE FRAME f-menu CENTERED.
   HIDE MESSAGE.
   CHOOSE FIELD menu GO-ON (F5) AUTO-RETURN WITH FRAME f-menu.


   IF SEARCH(proglist[FRAME-INDEX]) = ?
   THEN DO:
       MESSAGE "The program" proglist[FRAME-INDEX] "does not exist.".
       MESSAGE "Please make another choice.".
   END.
   ELSE RUN VALUE(proglist[FRAME-INDEX]).
END.
