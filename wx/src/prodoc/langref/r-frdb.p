/*r-frdb.p */
FOR EACH customer:
UPDATE cust-num name address address2 city
  state postal-code WITH 1 DOWN 1 COLUMN CENTERED
  EDITING:
    DISPLAY
	"You are editing field: " FRAME-FIELD SKIP
	" of file: " FRAME-FILE SKIP
	" In database: " FRAME-DB
	WITH FRAME a ROW 15 NO-LABELS CENTERED.
	READKEY.
	APPLY LASTKEY.
    END.  /*Editing*/
 END.
