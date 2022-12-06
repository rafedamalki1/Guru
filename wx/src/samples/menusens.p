/* menusens.p

   Per Ekman 93-08-20.
   
   For a description, see menuex.p.
*/
/*Code_Start*/

def input param menu_handle as widget-handle no-undo.
def input param menu_items  as cha           no-undo.
def input param menu_sens   as log           no-undo.

def var lbl as cha no-undo.
def var i   as int no-undo.

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
    if can-do(menu_items,lbl) or
       can-do(menu_items,menu_path + lbl)
    then ih:sensitive = menu_sens.
    if can-query(ih,"first-child")
       then run scan(ih,menu_path + lbl + "/").
    ih = ih:next-sibling.
  end.
end.
