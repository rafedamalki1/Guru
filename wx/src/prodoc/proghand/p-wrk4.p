/* p-wrk4.p */

DEFINE WORK-TABLE cpage
   FIELD w-cat-page LIKE item.cat-page
   FIELD w-inv-value AS DECIMAL FORMAT "->>>,>>>,>>9.99"
       LABEL "Inventory Value"
   FIELD w-item-value AS DECIMAL FORMAT ">>>,>>9.99"
       LABEL "Item Inv. Value"
   FIELD w-item-num LIKE item.item-num.
       
FOR EACH item:
   FIND FIRST cpage WHERE cpage.w-cat-page >= item.cat-page NO-ERROR.
   
   IF NOT AVAILABLE cpage OR cpage.w-cat-page > item.cat-page
   THEN DO:
      FIND PREV cpage NO-ERROR.
      CREATE cpage.
      cpage.w-cat-page = item.cat-page.
   END.
   
   IF price * on-hand > w-item-value
   THEN DO:
      ASSIGN w-item-value = price * on-hand
             w-item-num = item.item-num.
   END.
   
   cpage.w-inv-value = cpage.w-inv-value + (item.price * item.on-hand).
END.

FOR EACH cpage:
   DISPLAY w-cat-page w-inv-value w-item-num w-item-value.
END.
