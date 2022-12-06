/* r-frame2.p */

OUTPUT TO phone.lst PAGED PAGE-SIZE 20.

FOR EACH customer:

    FORM HEADER "Customer List" AT 1 PAGE-NUMBER TO 60
	WITH FRAME hdr PAGE-TOP CENTERED NO-BOX.
    VIEW FRAME hdr.

    FORM "Customer List Continued On Next Page"
	WITH FRAME footr PAGE-BOTTOM CENTERED.
    VIEW FRAME footr.

    DISPLAY cust-num name phone WITH CENTERED.

END.
HIDE FRAME footr.
OUTPUT CLOSE.
