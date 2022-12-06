define variable hComments as handle.
define variable vLabel as char format "x(10)" 
  init "Comments:".
define frame CustInfo
  vLabel at row 3 column 5
  with three-d no-label
  size 80 by 10.

create editor hComments
  assign 
  width  = 40 
  height = 5
  row    = 3
  column = 20
  frame  = frame CustInfo:handle
  .

display vLabel with frame CustInfo.
current-window:height = frame Custinfo:height.
enable all except vlabel with frame custinfo.

wait-for close of this-procedure.
