/**********  DEFINE QUERY  **********/
DEFINE QUERY Item-Query FOR Item.

/**********  DEFINE VARIABLES  **********/
DEFINE VARIABLE Current-Record AS RECID.

/**********  DEFINE FORM  **********/
{lt-08-f1.i}
  
/**********  DEFINE TRIGGERS  **********/
{lt-08-t1.i} {lt-08-t2.i} 
  
ON CHOOSE OF btn-Add 
DO:
    CREATE Item.
    UPDATE Item.Item-Name Price On-Hand Allocated Re-Order 
        On-Order Cat-Page Cat-Description btn-OK btn-Cancel 
            WITH FRAME Dialog1.
    DISPLAY Item-Num Item-Name Price On-Hand Allocated Re-Order 
        On-Order Cat-Page Cat-Description WITH FRAME Frame1.
END.

/**********  MAIN LOGIC  **********/
OPEN QUERY Item-Query FOR EACH Item NO-LOCK.
GET FIRST Item-Query.
DISPLAY Item.Item-Num Item-Name Price On-Hand Allocated Re-Order 
    On-Order Cat-Page Cat-Description WITH FRAME Frame1 USE-TEXT.
ENABLE btn-Prev btn-Next btn-Update btn-Add btn-Exit WITH FRAME Frame1.
WAIT-FOR CHOOSE OF btn-Exit.
CLOSE QUERY Item-Query.







