/* menuex2.p 

   Per Ekman 93-08-19.
   
   This is an example of how you can create a dynamic menu-bar.
   See menuread.me for a complete description.
   See also menuex1.p and menuex3.p.

*/   
/*Code_Start*/

create widget-pool. /* All dynamic widgets created from now on, will
                       come into this widget-pool. When this program
                       exits, they will automatically be deleted. */

/* Define a variable to store the menu description. */
def var menu_desc as cha no-undo.
/*                         D
                           RC
            Label          SU Trigger pgm
            -------------- -- ---------------                 */
menu_desc = "File          !  !," +
            "-Open         !  !," +
            "--xxx         !  !xxxxxx/xxxx.p," +
            "--xyy         !D !yyyyyy/yyyy.p," + /* D = Disabled */
            "--zzz         !  !zzzzzz/zzzz.p," +
            "-Ne&w         !  !," +
            "-Close        !  !," +              /* D = Disabled */
            "-             !R !," +              /* R = Rule */
            "-Exit         !  !," +
            "Edit          !  !," +
            "-Cut          !  !," +
            "-             !S !," +              /* S = Skip */
            "-Paste        !  !," + 
            "Help          !  !," +
            "-Toggle on    ! C!," +              /* C = Checked */
            "-To&ggle off  ! U!," +              /* U = Unchecked */
            "-Disabled on  !DC!," + /* Disabled and Checked */
            "-Di&sabled off!DU!," + /* Disabled and Unchecked */
            "-Nopp         !  !".

/* Explanations:

 - The DRS column determines if the menu-item is disabled (D),
   or if its a rule (R) or a skip (S)
 - The CU column determines if the menu-item is a check-box,
   if it is (C or U), a C means that it is checked by default,
   a U means that it is unchecked by default. 
 - It is not important in which column the letters are,
   a D will allways be interpreted as "Disable", a C as "Checked"
   and so on. The columns are used here for readability. 
 - If you don't specify a program name, you can omit the last
   exclamation mark.
 - If you don't specify a program name and the DRS column and
   the CU column are blank, you can omit both excamation marks.
 - If you want to use the exclamation mark in the label, use
   an other delimiter (like % for example), but remember to
   also pass the same delimiter as the fourth parameter to
   menucre.p.
 - The allowed values are R, S, D, C, U, DC, DU, CD, UD or blank.
*/

/* create the menu and assign it to the current-window. */
run samples/menucre.p(menu_desc,current-window,",","!").
/* The first parameter is the description string for the menu */
/* The second parameter says to which widget the menu should
   belong to. If it is a widget for a window, a menu-bar is
   created. Otherwise, a popu-menu will be created. */
/* The third and fourth parameter is the delimiter characters
   for the menu-items resp. program name. */

form "You are using a dynamic menu." with centered row 8.
view.
/* disable all menu-items that begins with "x". This could be
   done in the menu description also. */
run samples/menusens.p(current-window:menu-bar,"x*",no).
/* The third parameter is used to disable (no) or enable (yes) the
   specified menu-items */

/* Now, get the widget handle for menu-item "Toggle on"
   (no wildcards allowed) and give a message to say if it
   checked or not. */
   
def var tglon as widget-handle no-undo.
run samples/menuhndl.p(current-window:menu-bar,"Toggle on",output tglon).
message "Is menu-item ""Toggle on"" checked?" tglon:checked.

wait-for window-close of current-window.

procedure exit: /* Trigger procedure for "Exit" */
  apply "window-close" to current-window.
end.
