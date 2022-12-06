define variable vComments as character
  view-as editor size 40 by 5 
  label "Comments".
define frame CustInfo
  vComments at row 3 column 5
  with three-d side-label
  size 80 by 10.
view frame CustInfo.
current-window:height = frame Custinfo:height.
update vComments with frame custinfo.
