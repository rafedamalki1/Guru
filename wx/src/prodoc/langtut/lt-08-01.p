/**********  DEFINE FORM  **********/
{lt-08-f1.i}
  
/**********  DEFINE TRIGGERS  **********/     
ON CHOOSE OF btn-Prev 
DO:
    FIND PREV Item NO-ERROR.
    IF NOT AVAILABLE(Item) THEN FIND LAST Item.   
    DISPLAY Item.Item-Num Item-Name Price On-Hand Allocated Re-Order 
        On-Order Cat-Page Cat-Description WITH FRAME Frame1.
END.

ON CHOOSE OF btn-Next 
DO:
    FIND NEXT Item NO-ERROR.
    IF NOT AVAILABLE(Item) THEN FIND FIRST Item.
    DISPLAY Item.Item-Num Item-Name Price On-Hand Allocated Re-Order 
        On-Order Cat-Page Cat-Description WITH FRAME Frame1.
END.

/**********  MAIN LOGIC  **********/
FIND FIRST Item.
DISPLAY Item.Item-Num Item-Name Price On-Hand Allocated Re-Order 
    On-Order Cat-Page Cat-Description WITH FRAME Frame1 USE-TEXT.
ENABLE btn-Next btn-Prev btn-Exit WITH FRAME Frame1.
WAIT-FOR CHOOSE OF btn-Exit.

