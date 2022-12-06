/* r-stop.p */

DEFINE VARIABLE ans AS LOGICAL.

FOR EACH customer:
    DISPLAY cust-num name.
    UPDATE credit-limit.
    ans = no.
    MESSAGE
	"Stopping now undoes changes to this record.".
    MESSAGE
	"Do you want to stop now?" UPDATE ans.
    IF ans THEN STOP.
END.
