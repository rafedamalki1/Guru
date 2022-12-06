/* r-recph2.p */

REPEAT:
    FIND NEXT customer USE-INDEX country-post WHERE name BEGINS "S"
        EXCLUSIVE-LOCK.
    UPDATE name country postal-code phone.
END.
