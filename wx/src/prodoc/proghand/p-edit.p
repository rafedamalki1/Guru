/* p-edit.p */

FORM 
   item.item-num item-name
   item.price on-hand
   allocated re-order
   on-order cat-page
   item.cat-description
      VIEW-AS EDITOR SIZE-CHARS 30 BY 5
   WITH FRAME x SIDE-LABELS.
   
FOR EACH item:
   UPDATE item WITH FRAME x.
END.
