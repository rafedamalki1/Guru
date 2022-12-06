/* UPDATE trigger for Chapter 8 database access form */

ON CHOOSE OF btn-Update 
DO:
    /* Current-Record = RECID(Item).*/
    Current-Record = RECID(Item).
    FIND FIRST Item WHERE RECID(Item) = Current-Record EXCLUSIVE-LOCK
        NO-ERROR.
    IF AVAILABLE(Item) THEN DO:
        UPDATE Item.Item-Name Price On-Hand Allocated Re-Order 
        On-Order Cat-Page Cat-Description btn-OK btn-Cancel 
            WITH FRAME Dialog1.
        DISPLAY Item.Item-Num Item-Name Price On-Hand Allocated Re-Order 
            On-Order Cat-Page Cat-Description WITH FRAME Frame1.
    END.
    ELSE DO:
        FIND FIRST Item WHERE RECID(Item) = Current-Record NO-LOCK.
        MESSAGE "Record in use. Unable to update." 
            VIEW-AS ALERT-BOX WARNING BUTTONS OK Title "Update Error".
    END.
END.
