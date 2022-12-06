/* r-leave.p */

DEFINE VARIABLE valid-choice AS CHARACTER INITIAL "NPFQ".
DEFINE VARIABLE selection AS CHARACTER FORMAT "x".

main-loop:
REPEAT:
  choose:
  REPEAT ON ENDKEY UNDO choose, RETURN:
    MESSAGE "(N)ext  (P)rev  (F)ind  (Q)uit"
      UPDATE selection AUTO-RETURN.
    IF INDEX(valid-choice,selection) <> 0
    THEN LEAVE choose. /* selection was valid */
    BELL.
  END. /* choose */

  /* Processing for menu choices N, P, F goes here */

  IF selection = "Q" THEN LEAVE main-loop.
END.
