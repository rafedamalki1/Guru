/* r-login.p */

DEFINE VARIABLE id       LIKE _User._USERID.
DEFINE VARIABLE password LIKE _PASSWORD.
DEFINE VARIABLE tries    AS INT NO-UNDO.

IF NOT CAN-FIND (FIRST _User) THEN RETURN.
DO ON ENDKEY UNDO, NEXT:  /*quit if they hit endkey*/
    /* reset id and password to blank in case of retry */
    id = "".
    password = "".
    UPDATE SPACE(2) id SKIP password BLANK
       WITH CENTERED ROW 8 SIDE-LABELS ATTR-SPACE.

    IF SETUSERID(id,password) = FALSE
    THEN DO:
       MESSAGE "Sorry, userid/password is incorrect.".
       IF tries > 1 THEN QUIT.   /* only allow 3 tries*/
       tries = tries + 1.
       UNDO, RETRY.
    END.
    HIDE ALL.
    RETURN.
END.
QUIT.
