/* Naviagation triggers for Chapter 8 database access form */

ON CHOOSE OF btn-Prev 
DO:
    GET PREV Item-Query.
    IF QUERY-OFF-END("Item-Query") THEN GET LAST Item-Query.  
    DISPLAY Item.Item-Num Item-Name Price On-Hand Allocated Re-Order 
        On-Order Cat-Page Cat-Description WITH FRAME Frame1 THREE-D.
END.

ON CHOOSE OF btn-Next 
DO:
    GET NEXT Item-Query.
    IF QUERY-OFF-END("Item-Query") THEN GET FIRST Item-Query.
    DISPLAY Item.Item-Num Item-Name Price On-Hand Allocated Re-Order 
        On-Order Cat-Page Cat-Description WITH FRAME Frame1.
END.


