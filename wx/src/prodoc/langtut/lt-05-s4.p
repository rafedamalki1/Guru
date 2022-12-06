/* Solution to Language Tutorial Problem 5-4 */

FOR EACH Order BY Promise-Date:

  IF(Ship-Date - 5) >= Promise-Date THEN 
    
     DISPLAY Order.Promise-Date Order.Order-Num Order.Cust-Num 
       Order.Ship-date (Order.Ship-Date - Order.Promise-Date)
       LABEL "Days Late" WITH USE-TEXT THREE-D.
                
END. /* FOR EACH Order */

