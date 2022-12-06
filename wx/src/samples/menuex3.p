/* menuex3.p

   Per Ekman 93-08-20.
   
   Example of a dynamic popup-menu.
   See menuread.me for a complete description.
   See also menuex1.p and menuex2.p.
*/
/*Code_Start*/

form a as cha skip
     "Position the mouse over the fill-in field a," skip
     "and press the menu-drop key to see the popup menu".

run samples/menucre.p("A,B,-B1,-B2,-B3,--B31,--B32,C,D",a:handle,",","!").
enable all.

wait-for window-close of current-window.
