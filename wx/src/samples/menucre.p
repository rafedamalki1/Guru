/* cremenu.p

   Per Ekman 93-08-20
   
   Creates a menu-bar or popup-menu dynamically.   
   See the program menuex.p for a description. */
/*Code_Start*/
def input param menu_desc   as cha    no-undo.
def input param par_handle  as widget no-undo.
def input param main_del    as cha    no-undo. /* Delimiter char */
def input param detail_del  as cha    no-undo. /* Delimiter char */

def var i         as int no-undo init 1.
def var j         as int no-undo.
def var num_items as int no-undo.
  
def var next_level  as int    no-undo.
def var next_entry  as cha    no-undo.
def var next_label  as cha    no-undo.
def var main_entry  as cha    no-undo.
def var main_label  as cha    no-undo.
def var flags       as cha    no-undo init ",R,S,D,C,U,DC,DU,CD,UD".
def var flag        as cha    no-undo.
def var mh          as widget no-undo.
def var mb          as widget no-undo.
def var pgm         as cha    no-undo.
def var lbl         as cha    no-undo.

num_items = num-entries(menu_desc,main_del).

create menu mb assign popup-only = (par_handle:type <> "Window").

run create(1,mb).

if par_handle:type = "window" then par_handle:menu-bar   = mb.
                              else par_handle:popup-menu = mb.

procedure create:
  def input param level   as int    no-undo.
  def input param mparent as widget no-undo.
  
  repeat while i <= num_items:
    assign main_entry = entry(i, menu_desc , main_del).
    if num-entries(main_entry,detail_del) <= 1
    then main_entry = main_entry + detail_del + "  ".
    if num-entries(main_entry,detail_del) = 2
    then main_entry = main_entry + detail_del + "".
    
    assign main_label = entry(1, main_entry, detail_del)
           flag       = entry(2,main_entry,detail_del)
           lbl        = trim(substr(main_label,level))
           i          = i + 1.
    if not can-do(flags,trim(flag)) then do:
      message "Bad flags in Menu entry """ +
              main_entry + """. Flag =" flag + "."
              "Should be one of" substr(flags,2) "or blank."
              view-as alert-box message buttons ok.
    end.
    if i <= num_items then do:
      assign next_entry = entry(i, menu_desc, main_del)
             next_label = entry(1, next_entry, detail_del)
             next_level = 2 + length(next_label) -
                              length(trim(next_label + "X","-")).
    end.
    else next_level = level - 1.
    if next_level = level + 1 then do:
      create sub-menu mh assign
          parent    = mparent
          label     = lbl
          sensitive = (index(flag,"D") = 0).
      run create(next_level,mh).
      if /*i > num_items or*/ next_level < level then return.
    end.
    else do:
      j = index(flag,"R").
      if j = 0 then j = - index(flag,"S").
      if j <> 0 then do:
        create menu-item mh assign
            parent    = mparent
            subtype   = (if j > 0 then "RULE" else "SKIP").
      end.
      else do:
        if entry(3,main_entry,detail_del) = "" then do:
          pgm = lbl.
          j = index(pgm,"&").
          if j > 0 then substr(pgm,j,1) = "".
          pgm = lc(pgm).
        end.
        else pgm = entry(3,main_entry,detail_del).
        create menu-item mh assign
            parent     = mparent
            sensitive  = (index(flag,"D") = 0)
            label      = lbl
        triggers:
          on choose persistent run value(pgm).
        end.
        j = index(flag,"C").
        if j = 0 then j = - index(flag,"U").
        if j <> 0 then do:
          assign mh:toggle-box = yes
                 mh:checked    = (j > 0).
        end.
      end.
      if      next_level = level then next.
      else if next_level < level then return.
      else do:
        message "Menu syntax error found in entry """ +
                main_entry + """ or """ + next_entry + """"
                view-as alert-box message buttons ok.
      end.
    end.
  end.
end.
