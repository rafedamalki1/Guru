/* Solution to Language Tutorial Problem 5-3 */

FOR EACH Order WHERE Order.Order-Date >= 01/14/93 AND 
               Order.Order-Date <= TODAY:
                     
  DISPLAY Order.Order-Date Order.Cust-Num Order.Order-Num 
       Order.Promise-Date Order.Sales-Rep Order.Ship-Date 
       WITH USE-TEXT THREE-D.

END. /* FOR EACH Order */






