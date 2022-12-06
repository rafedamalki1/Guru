define variable hField    as handle.
define variable vFldName  as character init "Address".
define variable vFormat   as character init "'x(30)'".
define variable vDatatype as character init "character". 
define variable vRow      as decimal   init 5.
define variable vColumn   as decimal   init 15.

define frame CustFrame
  vcustnum 
   like customer.custnum
   label "Cust #" 
   at row 3 column 8
  with side-label three-d size 80 by 10.

create fill-in hField
  assign
  name      = vFldName
  format    = vFormat
  data-type = vDataType
  row       = vrow
  column    = vcolumn
  frame     = frame CustFrame:handle.

enable all with frame CustFrame.
wait-for close of this-procedure.
