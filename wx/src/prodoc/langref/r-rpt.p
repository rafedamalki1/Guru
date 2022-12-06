/* r-rpt.p  -  REPEAT */

DEFINE VAR Selection AS INTEGER FORMAT "9".

FORM skip(3)
      "0 - Exit" at 32
      "1 - Edit Customer File" at 32
      "2 - List Customer File" at 32
      "3 - Edit Item File" at 32
      "4 - List Item File" at 32
      "Enter Choice" TO 30 Selection AUTO-RETURN
      HEADER "Application Name"  "Master Menu" AT 34  "Company" TO 79
	 WITH NO-BOX NO-LABELS CENTERED FRAME menu.

/* Create the procedures that are called from the following repeat block. */

REPEAT ON ENDKEY UNDO, RETRY:
    UPDATE Selection WITH FRAME menu.
    HIDE FRAME menu.

    CASE(Selection):
      WHEN 0 THEN
        LEAVE.
      WHEN 1 THEN
        RUN custedit.p.
      WHEN 2 THEN
        RUN custrpt.p.
      WHEN 3 THEN
        RUN itemedit.p.
      WHEN 4 THEN
        RUN itemrpt.p.
      OTHERWISE DO:
        BELL.
        MESSAGE "Not a valid choice. Try again.".
      END.
    END CASE.

END.  /* REPEAT  */
