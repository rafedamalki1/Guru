/**********  DEFINE QUERY  **********/
DEFINE QUERY Item-Query FOR Item.

/**********  DEFINE FORM  **********/
{lt-08-f1.i}
  
/**********  DEFINE TRIGGERS  **********/     
ON CHOOSE OF btn-Prev 
DO:
    GET PREV Item-Query.
    IF QUERY-OFF-END("Item-Query") THEN GET LAST Item-Query.  
    DISPLAY Item.Item-Num Item-Name Price On-Hand Allocated Re-Order 
        On-Order Cat-Page Cat-Description WITH FRAME Frame1.
END.

ON CHOOSE OF btn-Next 
DO:
    GET NEXT Item-Query.
    IF QUERY-OFF-END("Item-Query") THEN GET FIRST Item-Query.
    DISPLAY Item.Item-Num Item-Name Price On-Hand Allocated Re-Order 
        On-Order Cat-Page Cat-Description WITH FRAME Frame1.
END.

/**********  MAIN LOGIC  **********/
OPEN QUERY Item-Query FOR EACH Item.
GET FIRST Item-Query.
DISPLAY Item.Item-Num Item-Name Price On-Hand Allocated Re-Order 
    On-Order Cat-Page Cat-Description WITH FRAME Frame1 USE-TEXT.
ENABLE btn-Prev btn-Next btn-Exit WITH FRAME Frame1.
WAIT-FOR CHOOSE OF btn-Exit.
CLOSE QUERY Item-Query.

