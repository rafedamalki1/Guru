DEFINE VARIABLE  but1  AS WIDGET-HANDLE.

DISPLAY "Dynamic Button Example" SKIP(3) WITH FRAME x SIDE-LABELS.

OPEN QUERY all-custs FOR EACH Customer.
GET FIRST all-custs.

DISPLAY Customer.Name WITH FRAME x.

CREATE BUTTON but1
    ASSIGN ROW = 3
	   COLUMN = 5
	   LABEL = "Next Customer"
	   FRAME = FRAME x:HANDLE
	   SENSITIVE = TRUE
	   VISIBLE = TRUE
    TRIGGERS:
       ON CHOOSE
           DO:
              GET NEXT all-custs.
	      DISPLAY Customer.Name WITH FRAME x. 
	   END.
    END TRIGGERS.

WAIT-FOR WINDOW-CLOSE OF CURRENT-WINDOW.
