/**********  DEFINE QUERY  **********/
DEFINE QUERY Item-Query FOR Item.

/**********  DEFINE VARIABLES  **********/
DEFINE VARIABLE Current-Record AS ROWID.

/**********  DEFINE FORM  **********/
{lt-08-f1.i}
  
/**********  DEFINE TRIGGERS  **********/
{lt-08-t1.i}   
  
ON CHOOSE OF btn-Update 
DO:
    Current-Record = ROWID(Item).
    FIND FIRST Item WHERE ROWID(Item) = Current-Record EXCLUSIVE-LOCK
        NO-ERROR.
    IF AVAILABLE(Item) THEN DO:
        UPDATE Item.Item-Name Price On-Hand Allocated Re-Order 
        On-Order Cat-Page Cat-Description btn-OK btn-Cancel 
            WITH FRAME Dialog1.
        DISPLAY Item-Num Item-Name Price On-Hand Allocated Re-Order 
            On-Order Cat-Page Cat-Description WITH FRAME Frame1.
    END.
    ELSE DO:
        FIND FIRST Item WHERE ROWID(Item) = Current-Record NO-LOCK.
        MESSAGE "Record in use. Unable to update." 
            VIEW-AS ALERT-BOX WARNING BUTTONS OK Title "Update Error".
    END.
END.

/**********  MAIN LOGIC  **********/
OPEN QUERY Item-Query FOR EACH Item NO-LOCK.
GET FIRST Item-Query.
DISPLAY Item.Item-Num Item-Name Price On-Hand Allocated Re-Order On-Order 
    Cat-Page Cat-Description WITH FRAME Frame1 USE-TEXT.
ENABLE btn-Prev btn-Next btn-Update btn-Exit WITH FRAME Frame1.
WAIT-FOR CHOOSE OF btn-Exit.
CLOSE QUERY Item-Query.

