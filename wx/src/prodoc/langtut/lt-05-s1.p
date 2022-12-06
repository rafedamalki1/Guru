/* Solution to Language Tutorial Problem 5-1 */

DEFINE VARIABLE Old-Limit LIKE Customer.Credit-Limit
 LABEL "Old Limit".

FOR EACH Customer:

   ASSIGN Old-Limit = Customer.Credit-limit
     Customer.Credit-Limit = Customer.Credit-Limit * 1.10. 
                  
   DISPLAY Customer.Name Old-Limit Customer.Credit-Limit 
     WITH USE-TEXT THREE-D. 
            
END. /* FOR EACH Customer */





