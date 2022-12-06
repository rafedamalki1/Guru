/* p-join1.p */

DEFINE QUERY q1 FOR Customer, Order, Order-Line, Item.
DEFINE BROWSE b1 QUERY q1
    DISPLAY Customer.Name Customer.Credit-Limit Order.Order-num
            Item.Item-Name
WITH 10 DOWN.

OPEN QUERY q1 PRESELECT EACH Customer,
    EACH Order OF Customer,
    EACH Order-Line OF Order 
        WHERE (Order-Line.Price * Order-Line.Qty) >
              (.667 * Customer.Credit-Limit),
    EACH Item OF Order-Line.
    
ENABLE b1 WITH SIZE 68 BY 10.
WAIT-FOR WINDOW-CLOSE OF CURRENT-WINDOW.
