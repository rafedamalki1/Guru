/* r-view2.p */

OUTPUT TO slsrep PAGED PAGE-SIZE 10.
FOR EACH salesrep:
    PAGE.
    FORM HEADER "Sales rep report" "Page" AT 60 PAGE-NUMBER FORMAT ">>>9".
    DISPLAY SKIP(1) sales-rep rep-name region WITH NO-LABELS.

    FORM HEADER "Sales rep report" sales-rep "(continued)"
	  "Page" AT 60 PAGE-NUMBER FORMAT ">>>9" SKIP(1)
	  WITH FRAME hdr2 PAGE-TOP.
    VIEW FRAME hdr2.

    FOR EACH customer OF salesrep:
	DISPLAY cust-num name address city state.
    END.
END.
