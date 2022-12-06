/**********  DEFINE WIDGETS  **********/
DEFINE VARIABLE Start-Compress AS CHARACTER
       INITIAL "~033[3w".
DEFINE VARIABLE Stop-Compress AS CHARACTER
       INITIAL "~032[3w".

DEFINE BUTTON b-normal LABEL "Print Normal Report".
DEFINE BUTTON b-compress LABEL "Print Compressed Report".
DEFINE BUTTON b-exit LABEL "Exit".

/**********  DEFINE FRAMES  **********/
DEFINE FRAME Frame1
    SKIP(1)
    b-normal b-compress SKIP(1)
    b-exit
        WITH NO-BOX CENTERED THREE-D.

/**********  DEFINE TRIGGERS  **********/        
ON CHOOSE OF b-normal
DO:
    OUTPUT TO PRINTER.
    FOR EACH Customer WHERE Balance >= 1400 
        WITH STREAM-IO:
        DISPLAY Name Phone Balance Sales-rep.
    END.
    OUTPUT CLOSE.
END.

ON CHOOSE OF b-compress
DO:
    OUTPUT TO PRINTER.
    PUT CONTROL Start-Compress.
    FOR EACH Customer WHERE Balance >= 1400 
        WITH STREAM-IO:
        DISPLAY Name Phone Balance Sales-rep.
    END.
    PUT CONTROL Stop-Compress.
    OUTPUT CLOSE.
END.

/**********  MAIN LOGIC  **********/
ENABLE ALL WITH FRAME Frame1.
WAIT-FOR CHOOSE OF b-exit.



