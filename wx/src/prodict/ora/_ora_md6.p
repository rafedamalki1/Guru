/*************************************************************/
/* Copyright (c) 1984-1993 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

DEFINE INPUT PARAMETER dbs AS CHARACTER NO-UNDO.
FIND DICTDB._Db WHERE DICTDB._Db._Db-name = dbs NO-ERROR.

FIND _File "agedar" OF _Db.
ASSIGN
  _File._Dump-name = "agedar"
  _File._Desc = "This contains transactions for the aged receivables system".

FIND _Field "ar_inv" OF _File.
ASSIGN
  _Field._Data-type = "integer"
  _Field._Format    = ">>>>>9"
  _Field._Initial   = "0"
  _Field._Label     = "Invoice number"
  _Field._Valexp    = "ar_inv > 0"
  _Field._Valmsg    = "Invoice number cannot be zero"
  _Field._Order     = 10.

FIND _Field "ar_cust" OF _File.
ASSIGN
  _Field._Data-type = "integer"
  _Field._Format    = ">>>>>9"
  _Field._Initial   = "0"
  _Field._Label     = "Customer number"
  _Field._Order     = 20.

FIND _Field "ar_invdat" OF _File.
ASSIGN
  _Field._Data-type = "date"
  _Field._Format    = "99/99/99"
  _Field._Initial   = ""
  _Field._Label     = "Invoice date"
  _Field._Order     = 30.

FIND _Field "ar_amt" OF _File.
ASSIGN
  _Field._Data-type = "decimal"
  _Field._Format    = "->>,>>9.99"
  _Field._Initial   = "0"
  _Field._Label     = "Invoice amount"
  _Field._Help      = "Enter total invoice amount including shipping and sales tax"
  _Field._Decimals  = 2
  _Field._Order     = 40.

FIND _Field "ar_pay" OF _File.
ASSIGN
  _Field._Data-type = "decimal"
  _Field._Format    = "->>,>>9.99"
  _Field._Initial   = "0"
  _Field._Label     = "Total payments"
  _Field._Decimals  = 2
  _Field._Order     = 50.

FIND _Field "ar_adj" OF _File.
ASSIGN
  _Field._Data-type = "decimal"
  _Field._Format    = "->>,>>9.99"
  _Field._Initial   = "0"
  _Field._Label     = "Total adjustments"
  _Field._Decimals  = 2
  _Field._Order     = 60.

FIND _Field "ar_lastpay" OF _File.
ASSIGN
  _Field._Data-type = "date"
  _Field._Format    = "99/99/99"
  _Field._Initial   = ?
  _Field._Label     = "Last payment date"
  _Field._Order     = 70.

FIND _Field "ar_order" OF _File.
ASSIGN
  _Field._Data-type = "integer"
  _Field._Format    = ">>>>9"
  _Field._Initial   = ""
  _Field._Label     = "Order Number"
  _Field._Order     = 80.

FIND _Field "ar_ship" OF _File.
ASSIGN
  _Field._Data-type = "decimal"
  _Field._Format    = "->>,>>9.99"
  _Field._Initial   = "0"
  _Field._Label     = "Shipping Charge"
  _Field._Decimals  = 2
  _Field._Order     = 120.

FIND _Index "ar_inv" OF _File.
_File._Prime-Index = RECID(_Index).

FIND _File "customer" OF _Db.
ASSIGN
  _File._Dump-name = "customer"
  _File._Desc      = "Customer information"
  _File._Valexp    = "NOT CAN-FIND(FIRST order OF customer)"
  _File._Valmsg    = "Can not delete customer with outstanding orders".

FIND _Field "Cust-num" OF _File.
ASSIGN
  _Field._Data-type = "integer"
  _Field._Format    = ">>>>9"
  _Field._Initial   = "0"
  _Field._Label     = "Cust num"
  _Field._Valexp    = "cust-num > 0"
  _Field._Valmsg    = "Customer number must be greater than zero"
  _Field._Order     = 10.

FIND _Field "Name" OF _File.
ASSIGN
  _Field._Data-type = "character"
  _Field._Format    = "x(20)"
  _Field._Initial   = ""
  _Field._Label     = "Name"
  _Field._Order     = 20.

FIND _Field "Address" OF _File.
ASSIGN
  _Field._Data-type = "character"
  _Field._Format    = "x(20)"
  _Field._Initial   = ""
  _Field._Label     = "Addr"
  _Field._Order     = 30.

FIND _Field "Address2" OF _File.
ASSIGN
  _Field._Data-type = "character"
  _Field._Format    = "x(20)"
  _Field._Initial   = ""
  _Field._Label     = "Addr 2"
  _Field._Order     = 40.

FIND _Field "City" OF _File.
ASSIGN
  _Field._Data-type = "character"
  _Field._Format    = "x(12)"
  _Field._Initial   = ""
  _Field._Label     = "City"
  _Field._Order     = 50.

FIND _Field "St" OF _File.
ASSIGN
  _Field._Data-type = "character"
  _Field._Format    = "xx"
  _Field._Initial   = ""
  _Field._Label     = "State"
  _Field._Valexp    = "can-find(state of customer)"
  _Field._Valmsg    = "State must be one defined in state file"
  _Field._Help      = "Enter standard state abbreviation"
  _Field._Order     = 60.

FIND _Field "Zip" OF _File.
ASSIGN
  _Field._Data-type = "integer"
  _Field._Format    = "99999"
  _Field._Initial   = "0"
  _Field._Label     = "Zip"
  _Field._Order     = 70.

FIND _Field "Phone" OF _File.
ASSIGN
  _Field._Data-type = "character"
  _Field._Format    = "(999) 999-9999"
  _Field._Initial   = ""
  _Field._Label     = "Tel num"
  _Field._Order     = 80.

FIND _Field "Contact" OF _File.
ASSIGN
  _Field._Data-type = "character"
  _Field._Format    = "x(20)"
  _Field._Initial   = ""
  _Field._Label     = "Contact"
  _Field._Order     = 90.

FIND _Field "Sales-rep" OF _File.
ASSIGN
  _Field._Data-type = "character"
  _Field._Format    = "!(3)"
  _Field._Initial   = ""
  _Field._Label     = "Sls rep"
  _Field._Valexp    = "can-find(salesrep of customer)"
  _Field._Valmsg    = "The sales rep must be one that already exists"
  _Field._Help      = "Enter initials for a sales rep"
  _Field._Order     = 95.

FIND _Field "Sales-region" OF _File.
ASSIGN
  _Field._Data-type = "character"
  _Field._Format    = "x(8)"
  _Field._Initial   = ""
  _Field._Label     = "Sls reg"
  _Field._Order     = 100.

FIND _Field "Max-credit" OF _File.
ASSIGN
  _Field._Data-type = "decimal"
  _Field._Desc      = "Maximum credit"
  _Field._Format    = "->,>>>,>>9"
  _Field._Initial   = "0"
  _Field._Label     = "Max cred"
  _Field._Valexp    = "max-credit >=  0 and max-credit <= 9999999"
  _Field._Valmsg    = "Max credit must be >= 0 and <= 9,999,999"
  _Field._Help      = "Please enter a credit limit"
  _Field._Decimals  = 2
  _Field._Order     = 105.

FIND _Field "Curr-bal" OF _File.
ASSIGN
  _Field._Data-type = "decimal"
  _Field._Format    = "->,>>>,>>9.99"
  _Field._Initial   = "0"
  _Field._Label     = "Unpaid bal"
  _Field._Decimals  = 2
  _Field._Order     = 110.

FIND _Field "Terms" OF _File.
ASSIGN
  _Field._Data-type = "character"
  _Field._Format    = "x(20)"
  _Field._Initial   = "Net30"
  _Field._Label     = "Terms"
  _Field._Order     = 115.

FIND _Field "Tax-no" OF _File.
ASSIGN
  _Field._Data-type = "character"
  _Field._Format    = "x(15)"
  _Field._Initial   = ""
  _Field._Label     = "Tax num"
  _Field._Help      = "Enter tax-exempt number if any"
  _Field._Order     = 120.

FIND _Field "Discount" OF _File.
ASSIGN
  _Field._Data-type = "integer"
  _Field._Format    = ">>9"
  _Field._Initial   = "0"
  _Field._Label     = "Disc %"
  _Field._Valexp    = "Discount >= 0"
  _Field._Valmsg    = "Discount must be greater or equal to 0"
  _Field._Help      = "Enter a percentage from 0 to 999"
  _Field._Order     = 125.

FIND _Field "Mnth-sales#1" OF _File.
ASSIGN
  _Field._Data-type = "decimal"
  _Field._Format    = "->,>>>,>>9.99"
  _Field._Initial   = "0"
  _Field._Label     = "Mnth sls[1]"
  _Field._Decimals  = 2
  _Field._Order     = 126.
FIND _Field "Mnth-sales#2" OF _File.
ASSIGN
  _Field._Data-type = "decimal"
  _Field._Format    = "->,>>>,>>9.99"
  _Field._Initial   = "0"
  _Field._Label     = "Mnth sls[2]"
  _Field._Decimals  = 2
  _Field._Order     = 127.
FIND _Field "Mnth-sales#3" OF _File.
ASSIGN
  _Field._Data-type = "decimal"
  _Field._Format    = "->,>>>,>>9.99"
  _Field._Initial   = "0"
  _Field._Label     = "Mnth sls[3]"
  _Field._Decimals  = 2
  _Field._Order     = 128.
FIND _Field "Mnth-sales#4" OF _File.
ASSIGN
  _Field._Data-type = "decimal"
  _Field._Format    = "->,>>>,>>9.99"
  _Field._Initial   = "0"
  _Field._Label     = "Mnth sls[4]"
  _Field._Decimals  = 2
  _Field._Order     = 129.
FIND _Field "Mnth-sales#5" OF _File.
ASSIGN
  _Field._Data-type = "decimal"
  _Field._Format    = "->,>>>,>>9.99"
  _Field._Initial   = "0"
  _Field._Label     = "Mnth sls[5]"
  _Field._Decimals  = 2
  _Field._Order     = 130.
FIND _Field "Mnth-sales#6" OF _File.
ASSIGN
  _Field._Data-type = "decimal"
  _Field._Format    = "->,>>>,>>9.99"
  _Field._Initial   = "0"
  _Field._Label     = "Mnth sls[6]"
  _Field._Decimals  = 2
  _Field._Order     = 131.
FIND _Field "Mnth-sales#7" OF _File.
ASSIGN
  _Field._Data-type = "decimal"
  _Field._Format    = "->,>>>,>>9.99"
  _Field._Initial   = "0"
  _Field._Label     = "Mnth sls[7]"
  _Field._Decimals  = 2
  _Field._Order     = 132.
FIND _Field "Mnth-sales#8" OF _File.
ASSIGN
  _Field._Data-type = "decimal"
  _Field._Format    = "->,>>>,>>9.99"
  _Field._Initial   = "0"
  _Field._Label     = "Mnth sls[8]"
  _Field._Decimals  = 2
  _Field._Order     = 133.
FIND _Field "Mnth-sales#9" OF _File.
ASSIGN
  _Field._Data-type = "decimal"
  _Field._Format    = "->,>>>,>>9.99"
  _Field._Initial   = "0"
  _Field._Label     = "Mnth sls[9]"
  _Field._Decimals  = 2
  _Field._Order     = 134.
FIND _Field "Mnth-sales#10" OF _File.
ASSIGN
  _Field._Data-type = "decimal"
  _Field._Format    = "->,>>>,>>9.99"
  _Field._Initial   = "0"
  _Field._Label     = "Mnth sls[10]"
  _Field._Decimals  = 2
  _Field._Order     = 135.
FIND _Field "Mnth-sales#11" OF _File.
ASSIGN
  _Field._Data-type = "decimal"
  _Field._Format    = "->,>>>,>>9.99"
  _Field._Initial   = "0"
  _Field._Label     = "Mnth sls[11]"
  _Field._Decimals  = 2
  _Field._Order     = 136.
FIND _Field "Mnth-sales#12" OF _File.
ASSIGN
  _Field._Data-type = "decimal"
  _Field._Format    = "->,>>>,>>9.99"
  _Field._Initial   = "0"
  _Field._Label     = "Mnth sls[12]"
  _Field._Decimals  = 2
  _Field._Order     = 137.

FIND _Field "Ytd-sls" OF _File.
ASSIGN
  _Field._Data-type = "decimal"
  _Field._Format    = "->,>>>,>>9.99"
  _Field._Initial   = "0"
  _Field._Label     = "Ytd sls"
  _Field._Decimals  = 2
  _Field._Order     = 138.

FIND _Index "cust-num" OF _File.
_File._Prime-Index = RECID(_Index).

FIND _File "item" OF _Db.
ASSIGN
  _File._Dump-name = "item"
  _File._Desc      = "Item file"
  _File._Valexp    = "NOT CAN-FIND(FIRST order-line OF item)"
  _File._Valmsg    = "Cannot delete Item, order-line records exist with this item code".

FIND _Field "Item-num" OF _File.
ASSIGN
  _Field._Data-type = "integer"
  _Field._Format    = "99999"
  _Field._Initial   = "0"
  _Field._Label     = "Item num"
  _Field._Valexp    = "item-num >= 0"
  _Field._Valmsg    = "Item number must be greater or equal to 0"
  _Field._Help      = "Enter a number between 1 and 99999"
  _Field._Order     = 10.

FIND _Field "Idesc" OF _File.
ASSIGN
  _Field._Data-type = "character"
  _Field._Format    = "x(15)"
  _Field._Initial   = ""
  _Field._Label     = "Desc"
  _Field._Order     = 20.

FIND _Field "Subs-item" OF _File.
ASSIGN
  _Field._Data-type = "integer"
  _Field._Format    = "99999"
  _Field._Initial   = "0"
  _Field._Label     = "Subs item"
  _Field._Valexp    = "if sub <> 0 then can-find(item where it.item-n = sub) else yes"
  _Field._Valmsg    = "Item must already exist"
  _Field._Help      = "Enter a subsitute item number"
  _Field._Order     = 21.

FIND _Field "Cost" OF _File.
ASSIGN
  _Field._Data-type = "decimal"
  _Field._Format    = "->,>>>,>>9.99"
  _Field._Initial   = "0"
  _Field._Label     = "Cost"
  _Field._Decimals  = 2
  _Field._Order     = 22.

FIND _Field "Loc" OF _File.
ASSIGN
  _Field._Data-type = "character"
  _Field._Format    = "x(8)"
  _Field._Initial   = ""
  _Field._Label     = "Loc"
  _Field._Order     = 29.

FIND _Field "Prod-line" OF _File.
ASSIGN
  _Field._Data-type = "character"
  _Field._Format    = "x(6)"
  _Field._Initial   = ""
  _Field._Label     = "Product line"
  _Field._Order     = 30.

FIND _Field "On-hand" OF _File.
ASSIGN
  _Field._Data-type = "integer"
  _Field._Format    = "->>>>9"
  _Field._Initial   = "0"
  _Field._Label     = "On hand"
  _Field._Order     = 50.

FIND _Field "Alloc" OF _File.
ASSIGN
  _Field._Data-type = "integer"
  _Field._Format    = "->>>>9"
  _Field._Initial   = "0"
  _Field._Label     = "Alloc"
  _Field._Order     = 60.

FIND _Field "Rop" OF _File.
ASSIGN
  _Field._Data-type = "integer"
  _Field._Format    = "->>>>9"
  _Field._Initial   = "0"
  _Field._Label     = "Rop"
  _Field._Order     = 80.

FIND _Field "Oorder" OF _File.
ASSIGN
  _Field._Data-type = "integer"
  _Field._Format    = "->>>>9"
  _Field._Initial   = "0"
  _Field._Label     = "On order"
  _Field._Order     = 90.

FIND _Field "Iweight" OF _File.
ASSIGN
  _Field._Data-type = "decimal"
  _Field._Desc      = "Per item weight"
  _Field._Format    = ">>9.99"
  _Field._Initial   = "0"
  _Field._Label     = "Item weight"
  _Field._Decimals  = 2
  _Field._Order     = 120.

FIND _Field "Mnth-shp#1" OF _File.
ASSIGN
  _Field._Data-type = "integer"
  _Field._Format    = "->>>>9"
  _Field._Initial   = "0"
  _Field._Label     = "Mnth shp[1]"
  _Field._Order     = 175.
FIND _Field "Mnth-shp#2" OF _File.
ASSIGN
  _Field._Data-type = "integer"
  _Field._Format    = "->>>>9"
  _Field._Initial   = "0"
  _Field._Label     = "Mnth shp[2]"
  _Field._Order     = 176.
FIND _Field "Mnth-shp#3" OF _File.
ASSIGN
  _Field._Data-type = "integer"
  _Field._Format    = "->>>>9"
  _Field._Initial   = "0"
  _Field._Label     = "Mnth shp[3]"
  _Field._Order     = 177.
FIND _Field "Mnth-shp#4" OF _File.
ASSIGN
  _Field._Data-type = "integer"
  _Field._Format    = "->>>>9"
  _Field._Initial   = "0"
  _Field._Label     = "Mnth shp[4]"
  _Field._Order     = 178.
FIND _Field "Mnth-shp#5" OF _File.
ASSIGN
  _Field._Data-type = "integer"
  _Field._Format    = "->>>>9"
  _Field._Initial   = "0"
  _Field._Label     = "Mnth shp[5]"
  _Field._Order     = 179.
FIND _Field "Mnth-shp#6" OF _File.
ASSIGN
  _Field._Data-type = "integer"
  _Field._Format    = "->>>>9"
  _Field._Initial   = "0"
  _Field._Label     = "Mnth shp[6]"
  _Field._Order     = 180.
FIND _Field "Mnth-shp#7" OF _File.
ASSIGN
  _Field._Data-type = "integer"
  _Field._Format    = "->>>>9"
  _Field._Initial   = "0"
  _Field._Label     = "Mnth shp[7]"
  _Field._Order     = 181.
FIND _Field "Mnth-shp#8" OF _File.
ASSIGN
  _Field._Data-type = "integer"
  _Field._Format    = "->>>>9"
  _Field._Initial   = "0"
  _Field._Label     = "Mnth shp[8]"
  _Field._Order     = 182.
FIND _Field "Mnth-shp#9" OF _File.
ASSIGN
  _Field._Data-type = "integer"
  _Field._Format    = "->>>>9"
  _Field._Initial   = "0"
  _Field._Label     = "Mnth shp[9]"
  _Field._Order     = 183.
FIND _Field "Mnth-shp#10" OF _File.
ASSIGN
  _Field._Data-type = "integer"
  _Field._Format    = "->>>>9"
  _Field._Initial   = "0"
  _Field._Label     = "Mnth shp[10]"
  _Field._Order     = 184.
FIND _Field "Mnth-shp#11" OF _File.
ASSIGN
  _Field._Data-type = "integer"
  _Field._Format    = "->>>>9"
  _Field._Initial   = "0"
  _Field._Label     = "Mnth shp[11]"
  _Field._Order     = 185.
FIND _Field "Mnth-shp#12" OF _File.
ASSIGN
  _Field._Data-type = "integer"
  _Field._Format    = "->>>>9"
  _Field._Initial   = "0"
  _Field._Label     = "Mnth shp[12]"
  _Field._Order     = 186.

FIND _Index "item-num" OF _File.
_File._Prime-Index = RECID(_Index).

FIND _File "monthly" OF _Db.
ASSIGN
  _File._Dump-name = "monthly"
  _File._Desc      = "Transactions for monthly functions".

FIND _Field "tf_inv" OF _File.
ASSIGN
  _Field._Data-type = "integer"
  _Field._Format    = ">>>>>9"
  _Field._Initial   = "0"
  _Field._Label     = "Invoice Number"
  _Field._Valexp    = "tf_inv > 0"
  _Field._Valmsg    = "Invoice number cannot be zero"
  _Field._Order     = 10.

FIND _Field "tf_cust" OF _File.
ASSIGN
  _Field._Data-type = "integer"
  _Field._Format    = ">>>>9"
  _Field._Initial   = "0"
  _Field._Label     = "Customer number"
  _Field._Order     = 20.

FIND _Field "tf_date" OF _File.
ASSIGN
  _Field._Data-type = "date"
  _Field._Format    = "99/99/99"
  _Field._Initial   = ?
  _Field._Label     = "Transaction date"
  _Field._Order     = 30.

FIND _Field "tf_amt" OF _File.
ASSIGN
  _Field._Data-type = "decimal"
  _Field._Format    = "->>,>>9.99"
  _Field._Initial   = "0"
  _Field._Label     = "Total amount"
  _Field._Decimals  = 2
  _Field._Order     = 40.

FIND _Field "tf_dist" OF _File.
ASSIGN
  _Field._Data-type = "decimal"
  _Field._Format    = "->>,>>9.99"
  _Field._Initial   = "0"
  _Field._Label     = "Distribution amount"
  _Field._Decimals  = 2
  _Field._Order     = 50.

FIND _Field "tf_glacct" OF _File.
ASSIGN
  _Field._Data-type = "integer"
  _Field._Format    = ">>>>>9"
  _Field._Initial   = "0"
  _Field._Label     = "G/L account number"
  _Field._Order     = 60.

FIND _Field "tf_print" OF _File.
ASSIGN
  _Field._Data-type = "logical"
  _Field._Format    = "yes/no"
  _Field._Initial   = "no"
  _Field._Label     = "Invoice Printed yet"
  _Field._Order     = 80.

FIND _Field "tf_order" OF _File.
ASSIGN
  _Field._Data-type = "integer"
  _Field._Format    = ">>>>9"
  _Field._Initial   = ""
  _Field._Label     = "Order Number"
  _Field._Order     = 90.

FIND _Field "tf_type" OF _File.
ASSIGN
  _Field._Data-type = "character"
  _Field._Format    = "x(1)"
  _Field._Initial   = ""
  _Field._Order     = 100.

FIND _Field "tf_sales" OF _File.
ASSIGN
  _Field._Data-type = "character"
  _Field._Format    = "x(3)"
  _Field._Initial   = ""
  _Field._Label     = "Salesperson"
  _Field._Order     = 130.

FIND _Index "tf_cust" OF _File.
_File._Prime-Index = RECID(_Index).

RETURN.
