/* r-prtdlg.p */

DEFINE BUTTON bprintset LABEL "Printer Setup".
DEFINE BUTTON bprintnames LABEL "Print Customer Names".
DEFINE BUTTON bcancel LABEL "Cancel".
DEFINE FRAME PrintFrame 
    bprintset bprintnames bcancel
WITH TITLE "Quick Printer" VIEW-AS DIALOG-BOX.

ON CHOOSE OF bprintset DO: 
    SYSTEM-DIALOG PRINTER-SETUP. 
END.

ON CHOOSE OF bprintnames DO:
    OUTPUT TO PRINTER.
    FOR EACH customer BY name:
        DISPLAY name WITH STREAM-IO.
    END.
    OUTPUT CLOSE.
END.

ENABLE ALL WITH FRAME PrintFrame.
WAIT-FOR CHOOSE OF bcancel IN FRAME PrintFrame.
