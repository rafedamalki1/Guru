define variable hName as handle.

define frame CustFrame
  vcustnum 
   like customer.custnum
   label "Cust #" 
   at row 3 column 5
  
  vname 
   like customer.name    
   format "x(22)"
  
  with side-label three-d size 80 by 10.

assign
  hName        = vname:handle
  hName:format = "x(3)" 
  
  .
update vCustNum vName with frame CustFrame. 
