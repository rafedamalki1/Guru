define frame CustFrame
  vcustnum 
   like customer.custnum
   label "Cust #" 
   at row 3 column 5
  
  vname 
   like customer.name    
   format "x(22)"
  
  with side-label three-d size 80 by 10.

view frame custframe.
/* assign current-window:height-pixels = frame custframe:height-pixels */
/* .                                                                   */
update vCustNum vName with frame CustFrame. 
