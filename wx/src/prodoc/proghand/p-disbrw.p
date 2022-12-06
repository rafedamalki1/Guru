/* p-disbrw.p */

DEFINE BUTTON upd-button LABEL "Update".

DEFINE QUERY itm FOR Item.
DEFINE BROWSE brws-itm QUERY itm DISPLAY Item.Item-num Item.Item-name
 WITH 20 DOWN.

OPEN QUERY itm FOR EACH Item.

ENABLE brws-itm upd-button WITH FRAME brws-frame.

ON CHOOSE OF upd-button
   DO: /* TRANSACTION */
      DEFINE VARIABLE xrecid  AS RECID.

      xrecid = RECID(Item).
      FIND Item WHERE RECID(Item) = xrecid.
      UPDATE Item.Item-name WITH FRAME upd-frame VIEW-AS DIALOG-BOX
             TITLE "Update Item".
      DISPLAY Item.Item-name WITH BROWSE brws-itm.
   END.

WAIT-FOR WINDOW-CLOSE OF CURRENT-WINDOW.
