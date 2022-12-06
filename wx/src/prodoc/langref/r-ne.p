/* r-ne.p */

FOR EACH item WHERE cat-page <> 0:
    DISPLAY item-num item-name cat-page
       WITH TITLE "Catalog Items" USE-TEXT.
END.
