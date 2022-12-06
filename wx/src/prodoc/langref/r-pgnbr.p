/* r-pgnbr.p */

OUTPUT TO PRINTER.
FOR EACH customer:
    FORM HEADER "Customer report" AT 30
		"Page:" AT 60 PAGE-NUMBER FORMAT ">>9" SKIP(1).
    DISPLAY cust-num name address city state country.
END.
