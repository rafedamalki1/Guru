/* r-cusbug.p */

DEFINE NEW SHARED BUFFER CustBuf FOR customer.

DEFINE VARIABLE debug AS LOGICAL. 
debug = DEBUGGER:INITIATE().
debug = DEBUGGER:SET-BREAK("r-ordbug.p",6).

FOR EACH CustBuf:
    IF CAN-FIND(order OF CustBuf) THEN
        RUN r-ordbug.p.
END. /* FOR EACH CustBuf */

debug = DEBUGGER:CLEAR().
