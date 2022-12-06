/* p-istran.p */

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
THEN  MESSAGE "Transaction active at" "{1}".
ELSE  MESSAGE "Transaction not active at" "{1}".
