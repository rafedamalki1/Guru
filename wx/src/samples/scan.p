/* scan.p 
   Scans through the widget tree.
   Run this program whenever you want to see which widgets
   are defined.
   You can also change attributes on widgets with this program.
   The input parameter should be the root widget.
   
   Ex. RUN scan.p(session) 
       will display all widgets defined at this specific 
       moment in the session.
       
   Ex. RUN scan.p(current-window)
       will display all widget beloning to the current window. 
       
   Ex. RUN scan.p(FRAME x:handle)
       will display all widgets in FRAME x.
*/
/*Code_Start*/

def input param root_widget as widget-handle no-undo.
def var tmp_win  as widget-handle no-undo.
def var save_win as widget-handle no-undo.
def var i        as int           no-undo.

def temp-table tmp_widget no-undo
  field t_order     as int 
  field t_level     as int
  field t_handle    as widget-handle
  field t_Type      as cha format "x(20)"   column-label "Type"
  field t_Dynamic   as log format "D/ "     column-label "D"
  field t_Sensitive as log format "S/ "     column-label "S"
  field t_Visible   as log format "V/ "     column-label "V"
  field t_Hidden    as log format "H/ "     column-label "H"
  field t_Label     as cha format "x(30)"   column-label "Label/Title/Screen-value"
  field t_row       as dec format "->>9.99" column-label "Row"
  field t_column    as dec format "->>9.99" column-label "Column"
  field t_width     as dec format "->>9.99" column-label "Width"
  field t_height    as dec format "->>9.99" column-label "Height"

  index t_order is unique primary t_order.

def button but_ok label "Ok".


DEFINE FRAME brf WITH 1 DOWN 
NO-BOX KEEP-TAB-ORDER OVERLAY
   SIDE-LABELS NO-UNDERLINE THREE-D AT COL 1 ROW 1
SIZE 108 BY 14.50.

define query widgets for tmp_widget scrolling.
define browse br_widgets query widgets
  display t_Type
          t_Dynamic
          t_Sensitive
          t_Visible
          t_Hidden 
          t_Label
          t_row
          t_column
          t_width
          t_height
     with 15 down.
def frame full_info
  t_order     
  t_level     
  t_Type      
  t_Dynamic   
  t_Sensitive 
  t_Visible   
  t_Hidden    
  t_Label     
  t_row   
  t_column
    but_ok
    with 2 columns view-as dialog-box cancel-button but_ok.

def button but_exit   label "Exit".

form br_widgets 
     but_exit AT 1
with frame brf 
cancel-button but_exit.

run scan(root_widget, 0).

create window tmp_win
ASSIGN
         HEIGHT             = 14.50
         WIDTH              = 108
         STATUS-AREA        = no
         MESSAGE-AREA       = no
.

open query widgets for each tmp_widget.
view frame brf in window tmp_win.
enable all with frame brf in window tmp_win.
save_win = current-window.
current-window = tmp_win.

apply "cursor-down" to br_widgets.

wait-for window-close of tmp_win or choose of but_exit.
current-window = save_win.        
delete widget tmp_win.

procedure scan:
  def input param wh    as widget-handle no-undo.
  def input param level as integer       no-undo.

  if not valid-handle(wh) or wh = ? then return.
  i = i + 1.  
  create tmp_widget.
  assign t_order     = i
         t_level     = level
         t_handle    = wh
         t_Type      = fill("- ",level) + wh:type
                            /* if can-query(wh,"Type")
                              then wh:Type      else ? */
         t_Dynamic   = if can-query(wh,"Dynamic")   then wh:Dynamic   else ?
         t_Sensitive = if can-query(wh,"Sensitive") then wh:Sensitive else ?
         t_Visible   = if can-query(wh,"Visible")   then wh:Visible   else ?
         t_Hidden    = if can-query(wh,"Hidden")    then wh:Hidden    else ?
         t_row       = if can-query(wh,"Row")       then wh:Row       else ?
         t_column    = if can-query(wh,"Column")    then wh:Column    else ?
         t_width     = if can-query(wh,"width")     then wh:width     else ?
         t_height    = if can-query(wh,"height")    then wh:height    else ?.

  if (t_label = ? or t_label = "") and can-query(wh,"Label")        then t_label = wh:Label.
  if (t_label = ? or t_label = "") and can-query(wh,"Title")        then t_label = wh:Title.
  if (t_label = ? or t_label = "") and can-query(wh,"Screen-value") then t_label = wh:Screen-value.
      
  if can-query(wh,"menu-bar")   then run scan(wh:menu-bar  , level + 1).
  if can-query(wh,"popup-menu") then run scan(wh:popup-menu, level + 1).
  if can-query(wh,"First-child") then do:
    wh = wh:first-child.
    do while wh <> ?:
      run scan(wh, level + 1).
      if can-query(wh,"Next-sibling")
      then wh = wh:next-sibling.
      else leave.
    end.
  end.    
end.
