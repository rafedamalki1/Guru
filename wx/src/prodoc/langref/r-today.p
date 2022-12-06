/* r-today.p */

DEFINE VARIABLE rptdate AS DATE.

OUTPUT TO PRINTER.

rptdate = TODAY.
FORM HEADER rptdate "Customer List" AT 34
	    "Page" AT 66 PAGE-NUMBER FORMAT ">>>9" SKIP(2)
	    WITH NO-BOX PAGE-TOP.
VIEW.
FOR EACH customer:
    DISPLAY name AT 1 address AT 31
	    city + ", " + " " + state FORMAT "x(35)" at 31
	    WITH NO-BOX NO-LABELS CENTERED.
END.
