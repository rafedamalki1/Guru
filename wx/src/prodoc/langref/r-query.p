/* r-query.p */

DEFINE SHARED BUFFER x-cust FOR customer.
DEFINE SHARED QUERY  q-cust FOR x-cust.

GET FIRST q-cust.

DO WHILE AVAILABLE(x-cust):
  DISPLAY x-cust.name
          x-cust.cust-num
    WITH FRAME cust-info CENTERED DOWN ROW 3 USE-TEXT.
  DOWN 1 WITH FRAME cust-info.
  GET NEXT q-cust.
END.
           
