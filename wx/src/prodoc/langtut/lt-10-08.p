{lt-10-in.i} /* Common Interface Setup Code */  
 
/**********  DEFINE TRIGGERS  **********/
ON CHOOSE of b-rep
DO:  
    OUTPUT TO "tut-temp.txt".
    FOR EACH Customer FIELDS (Balance Credit-Limit Name Contact Cust-Num)
        WHERE Balance >= (Credit-Limit * .85)
        WITH STREAM-IO:
        DISPLAY Name  FORMAT "x(20)" Contact  FORMAT "x(15)" 
            Balance Credit-Limit WITH NO-BOX.
            
        FOR EACH Order FIELDS (Cust-Num Order-Num Order-Date Ship-Date
             Promise-Date) WHERE Order.Cust-Num = Customer.Cust-Num 
             WITH STREAM-IO:
            DISPLAY Order-Num Order-Date Ship-Date 
                Promise-Date SKIP(1) WITH 2 COLUMNS.
                
            FOR EACH Order-Line FIELDS (Order-Num Item-Num Qty)
                WHERE Order-Line.Order-Num = 
                    Order.Order-Num WITH STREAM-IO:
                FIND Item WHERE Item.Item-Num = Order-Line.Item-Num.
                DISPLAY Qty Order-Line.Item-Num 
                    Item-Name FORMAT "x(13)"
                    Item.Price LABEL "Unit Price"
                    Item.Price * Qty (TOTAL) 
                    LABEL "Price" FORMAT "$zzz,zz9.99 CR" WITH NO-BOX.
            END.
        END.
    END.    
    OUTPUT CLOSE.
    
    ASSIGN Rep-Editor:READ-ONLY IN FRAME Dialog1 = YES
         Rep-Editor:SENSITIVE IN FRAME Dialog1 = YES 
         FRAME Dialog1:TITLE = "Report Output"
         Stat = Rep-Editor:READ-FILE("tut-temp.txt") IN FRAME Dialog1. 
             
    IF Stat THEN DO:
        ENABLE Rep-Editor b-ok WITH FRAME Dialog1.
        WAIT-FOR GO OF FRAME Dialog1.
        HIDE FRAME Dialog1.
    END.
END.

/**********  MAIN LOGIC  **********/
ENABLE ALL WITH FRAME Frame1.
WAIT-FOR CHOOSE OF b-exit.

        
