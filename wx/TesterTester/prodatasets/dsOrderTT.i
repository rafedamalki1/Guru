/* Include file dsOrderTT.i -- temp-table definitions for ttOrder,
    ttOline and ttItem. */
DEFINE TEMP-TABLE ttOrder
  FIELD Ordernum AS INTEGER LABEL "Order Num" FORMAT "zzzzzzzzz9" INITIAL "0"
  FIELD CustNum AS INTEGER LABEL "Cust Num" FORMAT ">>>>9" INITIAL "0"
  FIELD OrderDate AS DATE LABEL "Ordered" FORMAT "99/99/99" INITIAL "TODAY"
  FIELD ShipDate AS DATE LABEL "Shipped" FORMAT "99/99/9999"
  FIELD PromiseDate AS DATE LABEL "Promised" FORMAT "99/99/99"
  FIELD Carrier AS CHARACTER LABEL "Carrier" FORMAT "x(25)"
  FIELD Instructions AS CHARACTER LABEL "Instructions" FORMAT "x(50)"
  FIELD PO AS CHARACTER LABEL "PO" FORMAT "x(20)"
  FIELD Terms AS CHARACTER LABEL "Terms" FORMAT "x(20)" INITIAL "Net30"
  FIELD SalesRep AS CHARACTER LABEL "Sales Rep" FORMAT "x(4)"
  FIELD BillToID AS INTEGER LABEL "Bill To ID" FORMAT "zzzzzzzzz9" INITIAL "0"
  FIELD ShipToID AS INTEGER LABEL "Ship To ID" FORMAT "zzzzzzzzz9" INITIAL "0"
  FIELD OrderStatus AS CHARACTER LABEL "Order Status" FORMAT "x(20)" INITIAL "Ordered"
    VIEW-AS COMBO-BOX
    LIST-ITEMS "Ordered","Back Ordered", "Partially Shipped", "Shipped"
  FIELD WarehouseNum AS INTEGER LABEL "Warehouse Num" FORMAT "zzzzzzzzz9" INITIAL "0"
  FIELD Creditcard AS CHARACTER LABEL "Credit Card" FORMAT "x(20)" INITIAL "Visa"
    VIEW-AS COMBO-BOX
    LIST-ITEMS "Visa","American Express", "Master Card"
  FIELD OrderTotal AS DECIMAL LABEL "Order Total" FORMAT ">>>>>9.99"
  FIELD CustName   AS CHARACTER LABEL "Cust Name" FORMAT "X(20)"
  FIELD RepName    AS CHARACTER LABEL "Sales Rep" FORMAT "X(20)"
  INDEX OrderNum IS PRIMARY UNIQUE Ordernum.

DEFINE TEMP-TABLE ttOLine BEFORE-TABLE ttOlineBefore
  FIELD Ordernum AS INTEGER LABEL "Order Num" FORMAT "zzzzzzzzz9" INITIAL "0"
  FIELD Linenum AS INTEGER LABEL "Line Num" FORMAT ">>9" INITIAL "0"
  FIELD Itemnum AS INTEGER LABEL "Item Num" FORMAT "zzzzzzzzz9" INITIAL "0"
  FIELD Price AS DECIMAL LABEL "Price" FORMAT "->,>>>,>>9.99" INITIAL "0" DECIMALS 2
  FIELD Qty AS INTEGER LABEL "Qty" FORMAT "->>>>9" INITIAL "0"
  FIELD Discount AS INTEGER LABEL "Discount" FORMAT ">>9%" INITIAL "0"
  FIELD ExtendedPrice AS DECIMAL LABEL "Extended Price" FORMAT "->>>,>>9.99" INITIAL "0" DECIMALS 2
  FIELD OrderLineStatus AS CHARACTER LABEL "Order Line Status" FORMAT "x(20)" INITIAL "Ordered"
    VIEW-AS COMBO-BOX
    LIST-ITEMS "Ordered","Back Ordered","Shipped"
  INDEX orderline IS PRIMARY UNIQUE Ordernum Linenum.

DEFINE TEMP-TABLE ttItem
  FIELD Itemnum AS INTEGER LABEL "Item Num" FORMAT "zzzzzzzzz9" INITIAL "0"
  FIELD ItemName AS CHARACTER LABEL "Item Name" FORMAT "x(25)"
  FIELD Price AS DECIMAL LABEL "Price" FORMAT "->,>>>,>>9.99" INITIAL "0" DECIMALS 2
  FIELD Onhand AS INTEGER LABEL "On Hand" FORMAT "->>>>9" INITIAL "0"
  FIELD Allocated AS INTEGER LABEL "Allocated" FORMAT "->>>>9" INITIAL "0"
  FIELD ReOrder AS INTEGER LABEL "Re Order" FORMAT "->>>>9" INITIAL "0"
  FIELD OnOrder AS INTEGER LABEL "On Order" FORMAT "->>>>9" INITIAL "0"
  FIELD CatPage AS INTEGER LABEL "Cat Page" FORMAT ">>9" INITIAL "0"
  FIELD CatDescription AS CHARACTER LABEL "Cat-Description" FORMAT "X(200)"
    VIEW-AS EDITOR SIZE 41 by 5 SCROLLBAR-VERTICAL 
  FIELD Category1 AS CHARACTER LABEL "Category1" FORMAT "x(30)"
  FIELD Category2 AS CHARACTER LABEL "Category2" FORMAT "x(30)"
  FIELD Special AS CHARACTER LABEL "Special" FORMAT "x(8)"
  FIELD Weight AS DECIMAL LABEL "Weight" FORMAT "->>,>>9.99" INITIAL "0" DECIMALS 2
  FIELD Minqty AS INTEGER LABEL "Min Qty" FORMAT "->>>>9" INITIAL "0"
  INDEX ItemNum IS PRIMARY UNIQUE Itemnum.
