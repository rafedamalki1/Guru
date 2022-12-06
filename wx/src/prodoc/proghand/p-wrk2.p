/* p-wrk2.p */

DEFINE WORK-TABLE cpage
   FIELD w-cat-page LIKE item.cat-page
   FIELD w-inv-value AS DECIMAL FORMAT "->>>,>>>,>>9.99"
       LABEL "Inventory Value".
       
FOR EACH item:
   FIND FIRST cpage WHERE cpage.w-cat-page = item.cat-page NO-ERROR.
   
   IF NOT AVAILABLE cpage
   THEN DO:
      CREATE cpage.
      cpage.w-cat-page = item.cat-page.
   END.
   
   cpage.w-inv-value = cpage.w-inv-value + (item.price * item.on-hand).
END.

FOR EACH cpage BY w-cat-page:
   DISPLAY w-cat-page w-inv-value.
END.
