/* r-vaedit.p */

DEFINE VARIABLE my_clipbd AS CHARACTER VIEW-AS EDITOR SIZE 60 BY 6
                            SCROLLBAR-VERTICAL LABEL "Scratch Pad".

DEFINE BUTTON b_quit LABEL "Quit" AUTO-ENDKEY.

FORM
  item.item-num
  item.item-name
  item.price
  item.on-hand
  item.allocated
  item.re-order
  item.on-order
  item.cat-page
  item.cat-description VIEW-AS EDITOR SIZE 35 BY 3 SCROLLBAR-VERTICAL
  WITH FRAME item-info 1 DOWN ROW 1 CENTERED SIDE-LABELS 
  TITLE "Update Item Category Description".

FORM
  my_clipbd
  WITH FRAME clip.

DEFINE FRAME butt-frame
  b_quit
WITH CENTERED.

ON GO OF item.item-num
  DO:
    FIND item USING item.item-num EXCLUSIVE-LOCK.
    DISPLAY item WITH FRAME item-info.
    ENABLE item.cat-description WITH FRAME item-info.
    ENABLE my_clipbd WITH FRAME clip.
  END.

ON GO OF item.cat-description
   DO:
     ASSIGN item.cat-description.
     CLEAR FRAME item-info.
     DISABLE item.cat-description WITH FRAME item-info.
   END.

ENABLE item.item-num WITH FRAME item-info.
ENABLE b_quit WITH FRAME butt-frame.

WAIT-FOR WINDOW-CLOSE OF CURRENT-WINDOW.
