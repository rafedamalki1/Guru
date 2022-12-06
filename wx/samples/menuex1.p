/* menuex1.p 

   Per Ekman 93-08-19.
   
   This is an example of how you can create a dynamic menu. The
   program menucre.p is used to do that.
   
   See menuread.me for a complete description.
   See also menuex2.p and menuex3.p.
*/   
/*Code_Start*/

create widget-pool. /* All dynamic widgets created from now on, will
                       come into this widget-pool. When this program
                       exits, they will automatically be deleted. */

/* Here is the simplified syntax used to create the menu.
   The menu is created and assign to the current window in
   one staement. See also menuex2.p and menuex3.p. */
run samples/menucre.p("XXX,-x&1,-y&2,-x&3,--x3&1,--y32,YYY,-Y1",current-window,",","!").
display "Simple menu." with row 8 centered.
wait-for window-close of current-window.
