/* p-chsmnu.p */

DEFINE VARIABLE menu AS CHARACTER EXTENT 4 FORMAT "x(7)"
   INITIAL [ "Browse", "Create" , "Update", "Exit" ].

DEFINE VARIABLE proglist AS CHARACTER EXTENT 4
   INITIAL [ "brws.p", "cre.p", "upd.p", "exit.p"].

FORM "Use the sample strip menu to select an action."
  WITH FRAME instruc CENTERED ROW 5.

REPEAT:
   VIEW FRAME instruc.
   DISPLAY menu 
        WITH NO-LABELS ROW SCREEN-LINES - 2 NO-BOX FRAME f-menu CENTERED.
   HIDE MESSAGE.
   CHOOSE FIELD menu AUTO-RETURN WITH FRAME f-menu.
   IF SEARCH(proglist[FRAME-INDEX]) = ?
   THEN DO:
       MESSAGE "The program" proglist[FRAME-INDEX] "does not exist.".
       MESSAGE "Please make another choice.".
   END.
   ELSE RUN VALUE(proglist[FRAME-INDEX]).
END.
