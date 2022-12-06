/* r-length.p */

DEFINE VARIABLE short-name AS CHARACTER
    FORMAT "x(11)" LABEL "Desc".

FOR EACH item:
    IF LENGTH(item-name,"CHARACTER") > 8 THEN
        short-name = SUBSTRING(item-name,1,8, "FIXED") + "..." .
    ELSE short-name = item-name.
    DISPLAY item-num short-name on-hand allocated
            re-order on-order price FORMAT "$>>>9.99".
END.
