/* menuhndl.p

   Per Ekman 93-08-20.

   Returns the widget-handle for a specific menu-item or sub-menu.   
   For a description, see menuex.p.
*/
/*Code_Start*/

def input  param menu_handle as widget-handle no-undo.
def input  param menu_item   as cha           no-undo.
def output param menu_hndl   as widget-handle no-undo.

def var lbl as cha no-undo.
def var i   as int no-undo.

menu_hndl = ?.
run scan(menu_handle,"/").

procedure scan:
  def input param parent_handle as widget-handle no-undo.
  def input param menu_path     as cha           no-undo.
  
  def var ih  as widget-handle no-undo.
  
  ih = parent_handle:first-child.
  do while ih <> ?:
    lbl = ih:label.
    i = index(lbl,"&").
    if i > 0 then substr(lbl,i,1) = "".
    if menu_item = lbl or
       menu_item = menu_path + lbl 
    then do:
      menu_hndl = ih.
      return "Found".
    end.
    if can-query(ih,"first-child") then do:
      run scan(ih,menu_path + lbl + "/").
      if return-value = "Found" then return "Found".
    end.
    ih = ih:next-sibling.
  end.
end.

if menu_hndl = ?
then message "The requested item was not found."
             view-as alert-box message buttons ok. 
