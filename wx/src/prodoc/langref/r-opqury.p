/* r-opqry.p */

DEFINE QUERY q-order FOR
    customer FIELDS (customer.cust-num customer.name customer.phone), 
    order FIELDS (order.order-num order.order-date),
    order-line FIELDS (order-line.line-num order-line.price 
                       order-line.qty),
    item FIELDS (item.item-num item.item-name item.cat-desc).

OPEN QUERY q-order FOR EACH customer,
    EACH order OF customer,
    EACH order-line OF order,
    EACH item OF order-line NO-LOCK.
 
GET FIRST q-order.

DO WHILE AVAILABLE(customer):

  DISPLAY customer.cust-num 
    customer.name skip
    customer.phone skip
    order.order-num order.order-date skip
    order-line.line-num
    order-line.price order-line.qty skip
    item.item-num item.item-name skip
    item.cat-desc VIEW-AS EDITOR SIZE 50 BY 2 SCROLLBAR-VERTICAL
    WITH FRAME ord-info CENTERED SIDE-LABELS TITLE "Order Information".

   /* Allow scrolling, but not modification, of cat-desc. */
   ASSIGN item.cat-desc:READ-ONLY IN FRAME ord-info = TRUE
          item.cat-desc:SENSITIVE IN FRAME ord-info = TRUE. 
  
  PAUSE.
 
  GET NEXT q-order.
    
END. /* DO WHILE AVAIL(customer) */
 
  
                    
