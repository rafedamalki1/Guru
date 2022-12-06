/**********  DEFINE QUERY  **********/
DEFINE QUERY Item-Query FOR Item.

/**********  DEFINE VARIABLES  **********/
DEFINE VARIABLE Current-Record AS RECID.

/**********  DEFINE FORM  **********/
{lt-08-f1.i}
  
/**********  DEFINE TRIGGERS  **********/
{lt-08-t1.i} {lt-08-t2.i} {lt-08-t3.i}
  
ON CHOOSE OF btn-Delete
DO:
    MESSAGE "Do you really want to delete" Item.Item-Name "?"
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE Answer.
    IF Answer THEN DO:
        Current-Record = RECID(Item).
        FIND FIRST Item WHERE RECID(Item) = Current-Record EXCLUSIVE-LOCK
            NO-ERROR.
        IF AVAILABLE(Item) THEN DO:
            DELETE Item.
            OPEN QUERY Item-Query FOR EACH Item NO-LOCK.
            GET FIRST Item-Query.
            DISPLAY Item.Item-Num Item-Name Price On-Hand Allocated Re-Order 
                On-Order Cat-Page Cat-Description WITH FRAME Frame1.
        END.
        ELSE DO:
            FIND FIRST Item WHERE RECID(Item) = Current-Record NO-LOCK.
            MESSAGE "Record in use. Unable to delete." 
                VIEW-AS ALERT-BOX WARNING BUTTONS OK Title "Delete Error".
        END.
    END.
END.

/**********  MAIN LOGIC  **********/
OPEN QUERY Item-Query FOR EACH Item NO-LOCK.
GET FIRST Item-Query.
DISPLAY Item.Item-Num Item-Name Price On-Hand Allocated Re-Order 
    On-Order Cat-Page Cat-Description WITH FRAME Frame1 USE-TEXT.
ENABLE btn-Prev btn-Next btn-Update btn-Add btn-Delete btn-Exit 
    WITH FRAME Frame1.
WAIT-FOR CHOOSE OF btn-Exit.
CLOSE QUERY Item-Query.




