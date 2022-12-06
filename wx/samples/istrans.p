
/* istrans.p */

DEFINE VARIABLE istrans AS LOGICAL INITIAL yes.

DO ON ERROR UNDO:
  istrans = no.
  UNDO, LEAVE.
END.

/* If variable was undone within DO ON ERROR, then a */
/* transaction was active when this procedure was called. */
/* Use argument to procedure in message to identify */
/* where called from to aid in testing. */


IF istrans
THEN  MESSAGE "Transaction active ..." VIEW-AS ALERT-BOX INFORMATION
           BUTTONS OK TITLE " Transaction ".
ELSE  MESSAGE "Transaction not active ..." VIEW-AS ALERT-BOX INFORMATION
           BUTTONS OK TITLE " Transaction ".


