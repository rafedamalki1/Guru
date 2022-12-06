SELECT order-num, cust-num, order-date, "Order" FROM order 
      WHERE order-date < 2/1/93
UNION ALL
SELECT order-num, cust-num, ship-date, "Ship" FROM order
      WHERE ship-date < 2/1/93
UNION ALL
SELECT order-num, cust-num, promise-date, "Promise" FROM order
      WHERE promise-date < 2/1/93
ORDER BY order-num.
