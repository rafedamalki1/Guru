/*************************************************************/
/* Copyright (c) 1984-1993 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

DEFINE INPUT PARAMETER dbs AS CHARACTER NO-UNDO.
FIND DICTDB._Db WHERE DICTDB._Db._Db-name = dbs NO-ERROR.

FIND _File "order" OF _Db.
ASSIGN
  _File._Dump-name = "order"
  _File._Desc      = "Order header information"
  _File._Valexp    = "NOT CAN-FIND(FIRST order-line OF order)"
  _File._Valmsg    = "Cannot delete Order, Order-line records still exist".

FIND _Field "Order-num" OF _File.
ASSIGN
  _Field._Data-type = "integer"
  _Field._Format    = ">>>>9"
  _Field._Initial   = "0"
  _Field._Label     = "Ord num"
  _Field._Valexp    = "order-num > 0"
  _Field._Valmsg    = "Order number must be greater than zero"
  _Field._Help      = "Enter an order number between 1 and 99999"
  _Field._Order     = 10.

FIND _Field "Cust-num" OF _File.
ASSIGN
  _Field._Data-type = "integer"
  _Field._Desc = "                                                       Help:Name"
  _Field._Format    = ">>>>9"
  _Field._Initial   = "0"
  _Field._Label     = "Cust num"
  _Field._Valexp    = "can-find(customer of order)"
  _Field._Valmsg    = "Customer must already exist"
  _Field._Help      = "Enter an existing customer number"
  _Field._Order     = 20.

FIND _Field "Name" OF _File.
ASSIGN
  _Field._Data-type = "character"
  _Field._Format    = "x(20)"
  _Field._Initial   = ""
  _Field._Label     = "Name"
  _Field._Order     = 30.

FIND _Field "Address" OF _File.
ASSIGN
  _Field._Data-type = "character"
  _Field._Format    = "x(20)"
  _Field._Initial   = ""
  _Field._Label     = "Addr"
  _Field._Order     = 40.

FIND _Field "Address2" OF _File.
ASSIGN
  _Field._Data-type = "character"
  _Field._Format    = "x(20)"
  _Field._Initial   = ""
  _Field._Label     = "Addr 2"
  _Field._Order     = 50.

FIND _Field "City" OF _File.
ASSIGN
  _Field._Data-type = "character"
  _Field._Format    = "x(12)"
  _Field._Initial   = ""
  _Field._Label     = "City"
  _Field._Order     = 60.

FIND _Field "St" OF _File.
ASSIGN
  _Field._Data-type = "character"
  _Field._Format    = "x(2)"
  _Field._Initial   = ""
  _Field._Label     = "State"
  _Field._Valexp    = "can-find(state of order)"
  _Field._Valmsg    = "State abbreviation must exist in the state file"
  _Field._Help      = "Enter standard state abbreviation"
  _Field._Order     = 70.

FIND _Field "Zip" OF _File.
ASSIGN
  _Field._Data-type = "integer"
  _Field._Format    = "99999"
  _Field._Initial   = "0"
  _Field._Label     = "Zip"
  _Field._Order     = 80.

FIND _Field "Odate" OF _File.
ASSIGN
  _Field._Data-type = "date"
  _Field._Format    = "99/99/99"
  _Field._Initial   = ?
  _Field._Label     = "Ord date"
  _Field._Help      = "Date of order"
  _Field._Order     = 90.

FIND _Field "Sdate" OF _File.
ASSIGN
  _Field._Data-type = "date"
  _Field._Format    = "99/99/99"
  _Field._Initial   = ?
  _Field._Label     = "Shp date"
  _Field._Order     = 100.

FIND _Field "Pdate" OF _File.
ASSIGN
  _Field._Data-type = "date"
  _Field._Format    = "99/99/99"
  _Field._Initial   = ?
  _Field._Label     = "Prom date"
  _Field._Order     = 110.

FIND _Field "Shp-via" OF _File.
ASSIGN
  _Field._Data-type = "character"
  _Field._Format    = "x(20)"
  _Field._Initial   = ""
  _Field._Label     = "Ship via"
  _Field._Order     = 120.

FIND _Field "Misc-info" OF _File.
ASSIGN
  _Field._Data-type = "character"
  _Field._Format    = "x(20)"
  _Field._Initial   = ""
  _Field._Label     = "Misc info"
  _Field._Order     = 130.

FIND _Field "Cust-po" OF _File.
ASSIGN
  _Field._Data-type = "character"
  _Field._Format    = "x(20)"
  _Field._Initial   = ""
  _Field._Label     = "Cust po"
  _Field._Order     = 140.

FIND _Field "Terms" OF _File.
ASSIGN
  _Field._Data-type = "character"
  _Field._Format    = "x(20)"
  _Field._Initial   = "Net30"
  _Field._Label     = "Terms"
  _Field._Order     = 150.

FIND _Field "Sales-rep" OF _File.
ASSIGN
  _Field._Data-type = "character"
  _Field._Format    = "!(3)"
  _Field._Initial   = ""
  _Field._Label     = "Sls rep"
  _Field._Valexp    = "can-find(salesrep of order)"
  _Field._Valmsg    = "Sales rep must be on file"
  _Field._Help      = "Initials for sales representative"
  _Field._Order     = 160.

FIND _Field "Shipped" OF _File.
ASSIGN
  _Field._Data-type = "character"
  _Field._Format    = "x(1)"
  _Field._Initial   = ""
  _Field._Label     = "Shp flag"
  _Field._Order     = 170.

FIND _Index "order-num" OF _File.
_File._Prime-Index = RECID(_Index).

FIND _File "order-line" OF _Db.
ASSIGN
  _File._Dump-name = "order-li"
  _File._Desc      = "Order line information".

FIND _Field "Order-num" OF _File.
ASSIGN
  _Field._Data-type = "integer"
  _Field._Format    = ">>>>9"
  _Field._Initial   = "0"
  _Field._Label     = "Order num"
  _Field._Valexp    = "can-find(order of order-line)"
  _Field._Valmsg    = "Order must exist"
  _Field._Help      = "Order number for this order line"
  _Field._Order     = 10.

FIND _Field "Line-num" OF _File.
ASSIGN
  _Field._Data-type = "integer"
  _Field._Format    = ">>9"
  _Field._Initial   = "0"
  _Field._Label     = "Line num"
  _Field._Order     = 20.

FIND _Field "Item-num" OF _File.
ASSIGN
  _Field._Data-type = "integer"
  _Field._Desc      = "                                                      Help:Idesc"
  _Field._Format    = "99999"
  _Field._Initial   = "0"
  _Field._Label     = "Item num"
  _Field._Valexp    = "can-find(item of order-line)"
  _Field._Valmsg    = "Item must be on file"
  _Field._Help      = "Item number"
  _Field._Order     = 30.

FIND _Field "Price" OF _File.
ASSIGN
  _Field._Data-type = "decimal"
  _Field._Format    = "->,>>>,>>9.99"
  _Field._Initial   = "0"
  _Field._Label     = "Price"
  _Field._Decimals  = 2
  _Field._Order     = 40.

FIND _Field "Qty" OF _File.
ASSIGN
  _Field._Data-type = "integer"
  _Field._Format    = "->>>>9"
  _Field._Initial   = "0"
  _Field._Label     = "Qty"
  _Field._Order     = 50.

FIND _Field "Qty-ship" OF _File.
ASSIGN
  _Field._Data-type = "integer"
  _Field._Format    = "->,>>>,>>9"
  _Field._Initial   = "0"
  _Field._Label     = "Qty ship"
  _Field._Order     = 60.

FIND _Field "Disc" OF _File.
ASSIGN
  _Field._Data-type = "integer"
  _Field._Format    = ">>9"
  _Field._Initial   = "0"
  _Field._Label     = "Disc %"
  _Field._Order     = 70.

FIND _Index "order-line" OF _File.
_File._Prime-Index = RECID(_Index).

FIND _File "salesrep" OF _Db.
ASSIGN
  _File._Dump-name = "salesrep"
  _File._Desc      = "Sales representative information".

FIND _Field "sales-rep" OF _File.
ASSIGN
  _Field._Data-type = "character"
  _Field._Format    = "!(3)"
  _Field._Initial   = ""
  _Field._Label     = "Sales Rep"
  _Field._Help      = "Sales Representative's initials"
  _Field._Order     = 10.

FIND _Field "slsname" OF _File.
ASSIGN
  _Field._Data-type = "character"
  _Field._Format    = "x(30)"
  _Field._Initial   = ""
  _Field._Label     = "Name"
  _Field._Help      = "Name of Salesman"
  _Field._Order     = 20.

FIND _Field "slsrgn" OF _File.
ASSIGN
  _Field._Data-type = "character"
  _Field._Format    = "x(8)"
  _Field._Initial   = ""
  _Field._Label     = "Region"
  _Field._Help      = "Sales Region covered by this salesman"
  _Field._Order     = 30.

FIND _Field "slstitle" OF _File.
ASSIGN
  _Field._Data-type = "character"
  _Field._Format    = "x(30)"
  _Field._Initial   = ""
  _Field._Label     = "Title"
  _Field._Help      = "Title of this person"
  _Field._Order     = 40.

FIND _Field "slsquota" OF _File.
ASSIGN
  _Field._Data-type = "decimal"
  _Field._Format    = "->,>>>,>>9"
  _Field._Initial   = "0"
  _Field._Label     = "Yearly Quota"
  _Field._Help      = "Annual budgeted sales quota"
  _Field._Decimals  = 0
  _Field._Order     = 50.

FIND _Field "hire-date" OF _File.
ASSIGN
  _Field._Data-type = "date"
  _Field._Format    = "99/99/99"
  _Field._Initial   = "today"
  _Field._Label     = "Date hired"
  _Field._Order     = 60.

FIND _Index "rep" OF _File.
_File._Prime-Index = RECID(_Index).

FIND _File "shipping" OF _Db.
ASSIGN
  _File._Dump-name = "shipping"
  _File._Desc      = "Table of shipping charges by weight break".

FIND _Field "weight-break" OF _File.
ASSIGN
  _Field._Data-type = "decimal"
  _Field._Desc      = "Weight break for shipping charge"
  _Field._Format    = ">>>>9.99"
  _Field._Initial   = "0"
  _Field._Label     = "Weight break"
  _Field._Decimals  = 2
  _Field._Order     = 10.

FIND _Field "charge" OF _File.
ASSIGN
  _Field._Data-type = "decimal"
  _Field._Desc      = "Shipping charge"
  _Field._Format    = "->>,>>9.99"
  _Field._Initial   = "0"
  _Field._Label     = "Charge"
  _Field._Decimals  = 2
  _Field._Order     = 20.

FIND _Index "shipping" OF _File.
_File._Prime-Index = RECID(_Index).

FIND _File "state" OF _Db.
ASSIGN
  _File._Dump-name = "state"
  _File._Desc      = "Table of state abbreviation with sales regions".

FIND _Field "st" OF _File.
ASSIGN
  _Field._Data-type = "character"
  _Field._Format    = "!!"
  _Field._Initial   = ""
  _Field._Label     = "st abbr"
  _Field._Help      = "State abbreviation"
  _Field._Order     = 10.

FIND _Field "st-desc" OF _File.
ASSIGN
  _Field._Data-type = "character"
  _Field._Format    = "x(15)"
  _Field._Initial   = ""
  _Field._Label     = "State"
  _Field._Help      = "Full state name"
  _Field._Order     = 20.

FIND _Field "sls-reg" OF _File.
ASSIGN
  _Field._Data-type = "character"
  _Field._Format    = "x(8)"
  _Field._Initial   = ""
  _Field._Label     = "Sls reg"
  _Field._Help      = "Sales region for state"
  _Field._Order     = 30.

FIND _Index "state" OF _File.
_File._Prime-Index = RECID(_Index).

FIND _File "syscontrol" OF _Db.
ASSIGN
  _File._Dump-name = "syscontr"
  _File._Desc      = "System control information".

FIND _Field "company" OF _File.
ASSIGN
  _Field._Data-type = "character"
  _Field._Format    = "x(30)"
  _Field._Initial   = ""
  _Field._Label     = "Company name"
  _Field._Order     = 30.

FIND _Field "closing-date#1" OF _File.
ASSIGN
  _Field._Data-type = "date"
  _Field._Format    = "99/99/99"
  _Field._Initial   = ?
  _Field._Label     = "Closing dates[1]"
  _Field._Order     = 40.
FIND _Field "closing-date#2" OF _File.
ASSIGN
  _Field._Data-type = "date"
  _Field._Format    = "99/99/99"
  _Field._Initial   = ?
  _Field._Label     = "Closing dates[2]"
  _Field._Order     = 41.
FIND _Field "closing-date#3" OF _File.
ASSIGN
  _Field._Data-type = "date"
  _Field._Format    = "99/99/99"
  _Field._Initial   = ?
  _Field._Label     = "Closing dates[3]"
  _Field._Order     = 42.
FIND _Field "closing-date#4" OF _File.
ASSIGN
  _Field._Data-type = "date"
  _Field._Format    = "99/99/99"
  _Field._Initial   = ?
  _Field._Label     = "Closing dates[4]"
  _Field._Order     = 43.
FIND _Field "closing-date#5" OF _File.
ASSIGN
  _Field._Data-type = "date"
  _Field._Format    = "99/99/99"
  _Field._Initial   = ?
  _Field._Label     = "Closing dates[5]"
  _Field._Order     = 44.
FIND _Field "closing-date#6" OF _File.
ASSIGN
  _Field._Data-type = "date"
  _Field._Format    = "99/99/99"
  _Field._Initial   = ?
  _Field._Label     = "Closing dates[6]"
  _Field._Order     = 45.
FIND _Field "closing-date#7" OF _File.
ASSIGN
  _Field._Data-type = "date"
  _Field._Format    = "99/99/99"
  _Field._Initial   = ?
  _Field._Label     = "Closing dates[7]"
  _Field._Order     = 46.
FIND _Field "closing-date#8" OF _File.
ASSIGN
  _Field._Data-type = "date"
  _Field._Format    = "99/99/99"
  _Field._Initial   = ?
  _Field._Label     = "Closing dates[8]"
  _Field._Order     = 47.
FIND _Field "closing-date#9" OF _File.
ASSIGN
  _Field._Data-type = "date"
  _Field._Format    = "99/99/99"
  _Field._Initial   = ?
  _Field._Label     = "Closing dates[9]"
  _Field._Order     = 48.
FIND _Field "closing-date#10" OF _File.
ASSIGN
  _Field._Data-type = "date"
  _Field._Format    = "99/99/99"
  _Field._Initial   = ?
  _Field._Label     = "Closing dates[10]"
  _Field._Order     = 49.
FIND _Field "closing-date#11" OF _File.
ASSIGN
  _Field._Data-type = "date"
  _Field._Format    = "99/99/99"
  _Field._Initial   = ?
  _Field._Label     = "Closing dates[11]"
  _Field._Order     = 50.
FIND _Field "closing-date#12" OF _File.
ASSIGN
  _Field._Data-type = "date"
  _Field._Format    = "99/99/99"
  _Field._Initial   = ?
  _Field._Label     = "Closing dates[12]"
  _Field._Order     = 51.

FIND _Field "sales-acct" OF _File.
ASSIGN
  _Field._Data-type = "integer"
  _Field._Desc      = "Default G/L sales account"
  _Field._Format    = ">>>>>9"
  _Field._Initial   = "0"
  _Field._Label     = "Sales acct"
  _Field._Order     = 70.

FIND _Field "ship-acct" OF _File.
ASSIGN
  _Field._Data-type = "integer"
  _Field._Desc      = "Default G/L shipping charges account"
  _Field._Format    = ">>>>>9"
  _Field._Initial   = "0"
  _Field._Label     = "Ship acct"
  _Field._Order     = 80.

FIND _Field "tax-acct" OF _File.
ASSIGN
  _Field._Data-type = "integer"
  _Field._Desc      = "Default G/L sales tax account"
  _Field._Format    = ">>>>>9"
  _Field._Initial   = "0"
  _Field._Label     = "Tax acct"
  _Field._Order     = 90.

FIND _Field "tax-amount" OF _File.
ASSIGN
  _Field._Data-type = "decimal"
  _Field._Desc      = "Default sales tax rate"
  _Field._Format    = ">>9.99"
  _Field._Initial   = "5.00"
  _Field._Label     = "Tax amount"
  _Field._Decimals  = 2
  _Field._Order     = 100.

FIND _Field "tax-state" OF _File.
ASSIGN
  _Field._Data-type = "character"
  _Field._Desc      = "Default sales tax state"
  _Field._Format    = "x(2)"
  _Field._Initial   = "MA"
  _Field._Label     = "Tax state"
  _Field._Order     = 110.

FIND _Field "currfismon" OF _File.
ASSIGN
  _Field._Data-type = "integer"
  _Field._Format    = ">9"
  _Field._Initial   = "1"
  _Field._Label     = "Current Fiscal Month"
  _Field._Valexp    = "currfismon ge 1 and currfismon le 12"
  _Field._Order     = 120.

FIND _Field "applname" OF _File.
ASSIGN
  _Field._Data-type = "character"
  _Field._Format    = "x(30)"
  _Field._Initial   = "PROGRESS Test Drive"
  _Field._Label     = "Application Name"
  _Field._Order     = 130.

FIND _Field "aracct" OF _File.
ASSIGN
  _Field._Data-type = "integer"
  _Field._Format    = ">>>>>9"
  _Field._Initial   = "0"
  _Field._Label     = "Accounts Receivable Account"
  _Field._Order     = 140.

FIND _Field "cashacct" OF _File.
ASSIGN
  _Field._Data-type = "integer"
  _Field._Format    = ">>>>>9"
  _Field._Initial   = "0"
  _Field._Label     = "Cash Account"
  _Field._Order     = 150.

FIND _Field "printr" OF _File.
ASSIGN
  _Field._Data-type = "logical"
  _Field._Desc      = "Are reports to be sent to system printer"
  _Field._Format    = "yes/no"
  _Field._Initial   = "yes"
  _Field._Order     = 160.

FIND _Index "key" OF _File.
_File._Prime-index = RECID(_Index).

RETURN.
