/* CREATE trigger for Chapter 8 database access form */

ON CHOOSE OF btn-Add
DO:
    CREATE Item.
    UPDATE Item.Item-Name Price On-Hand Allocated Re-Order 
        On-Order Cat-Page Cat-Description btn-OK btn-Cancel 
            WITH FRAME Dialog1.
    DISPLAY Item-Num Item-Name Price On-Hand Allocated Re-Order 
        On-Order Cat-Page Cat-Description WITH FRAME Frame1.
END.
