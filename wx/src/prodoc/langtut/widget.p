if session:display-type = "gui" then
do:
    define variable gui-window as widget-handle.
    create window gui-window.
    assign gui-window:status-area = no
           gui-window:message-area = no
           gui-window:title = "Widget Tutorial"
           gui-window:height = 18
           session:system-alert-boxes = yes
           current-window = gui-window.          
end.
  
define variable instruct as character view-as editor 
    inner-chars 67 inner-lines 8.
        
define button b-fill label "Fill-in" size-chars 16 by 1.
define button b-text label "Text" size-chars 16 by 1.
define button b-edit label "Editor" size-chars 16 by 1.
define button b-togx label "Toggle Box" size-chars 16 by 1.
define button b-radi label "Radio Set" size-chars 16 by 1.
define button b-slid label "Slider" size-chars 16 by 1.
define button b-list label "Selection List" size-chars 16 by 1.
define button b-comb label "Combo Box" size-chars 16 by 1.
define button b-diag label "Dialog Box" size-chars 16 by 1.
define button b-exit label "Exit" size-chars 16 by 1.
    
define frame f-control
    "Choose a Button:" at row 1 column 2
    b-fill at row 3 column 2
    b-text at row 4 column 2
    b-edit at row 5 column 2
    b-togx at row 6 column 2
    b-radi at row 7 column 2
    b-slid at row 8 column 2    
    b-list at row 9 column 2
    b-comb at row 10 column 2
    b-diag at row 11 column 2
    b-exit at row 13 column 2 
        with side-labels no-box size-chars 20 by 16 at row 2 col 60.

define frame f-info
    "Welcome to the Widget Tutorial!" skip
    "This program shows you how to use the basic objects" skip
    "that make up a PROGRESS interface. To your right, you" skip
    "can see a column of rectangular objects called buttons." skip
    "Buttons issue commands when you choose them. Choosing" skip
    "a button here brings up a display that demonstrates" skip
    "one type of widget. Each new display has a button" skip
    "labeled DONE. Choose DONE to return to the main" skip
    "display." skip
    "To choose a button, press TAB until you highlight the" skip
    "button and then press SPACEBAR. If you have a mouse," skip
    "simply click the button." skip
    "Choose EXIT from this display to quit the program."
        with side-labels no-box size-chars 55 by 15 at row 2 column 3.

define frame f-info2
    instruct label "Info" to 76
        with side-labels no-box size-chars 76 by 6 at row 13 column 3.

on choose of b-fill
do:
    hide frame f-control.
    hide frame f-info.
    run p-fillin.
    view frame f-control.
    view frame f-info.
end.

on choose of b-text
do:
    hide frame f-control.
    hide frame f-info.
    run p-text.
    view frame f-control.
    view frame f-info.
end.

on choose of b-edit
do:
    hide frame f-control.
    hide frame f-info.
    run p-editor.
    view frame f-control.
    view frame f-info.
end.

on choose of b-togx
do:
    hide frame f-control.
    hide frame f-info.
    run p-toggle.
    view frame f-control.
    view frame f-info.
end. 

on choose of b-radi
do:
    hide frame f-control.
    hide frame f-info.
    run p-radio.
    view frame f-control.
    view frame f-info.
end.

on choose of b-slid
do:
    hide frame f-control.
    hide frame f-info.
    run p-slider.
    view frame f-control.
    view frame f-info.
end.

on choose of b-list
do:
    hide frame f-control.
    hide frame f-info.
    run p-select.
    view frame f-control.
    view frame f-info.
end.

on choose of b-comb
do:
    hide frame f-control.
    hide frame f-info.
    run p-combo.
    view frame f-control.
    view frame f-info.
end.

on choose of b-diag
do:
    run p-diag.
end.
 
view frame f-info.
enable all with frame f-control.
wait-for choose of b-exit.

procedure p-fillin:
define variable f1 as character label "Character" initial "Hello!".
define variable f2 as integer   label "Integer"   initial 123.
define variable f3 as decimal   label "Decimal"   initial 123.00.
define variable f4 as logical   label "Logical"   initial yes.
define variable f5 as character label "Date"      initial 12/31/94.
define variable f6 as character label "Disabled"  initial "Hello!".
define button b-done            label "Done".

define frame f-fillin    
skip(1)
f1 colon 20 f6 colon 50 skip
f2 colon 20 f3 colon 50 skip
f4 colon 20 f5 colon 50 skip(2)
b-done to 38
    with side-labels title "Learning to Use Fill-in Fields" size-chars 76 by 10
            at row 2 column 3.
            
display f1 f2 f3 f4 f5 f6 with frame f-fillin.
enable all except f6 with frame f-fillin.

instruct = "Press TAB or click a fill-in field to highlight it. Once a
 fill-in field has focus (is highlighted), you can use the keyboard to
 change the data. Fill-in field data must match the data type, shown
 here by the fill-in field label. Notice that one fill-in is
 programmatically disabled and you cannot highlight it. Choose DONE to
 return to the main display.".
 
instruct:read-only in frame f-info2 = yes.

display instruct with frame f-info2.
enable all with frame f-info2.

wait-for choose of b-done.

hide frame f-fillin.
hide frame f-info2.
end procedure.


procedure p-text:
define variable t1 as character label "Character" initial "Hello!" 
    view-as text.
define variable t2 as integer label "Integer" initial 123 
    view-as text.
define variable t3 as decimal label "Decimal" initial 123.00 
    view-as text.
define variable t4 as logical label "Logical" initial yes 
    view-as text.
define variable t5 as character label "Date" initial 12/31/94 
    view-as text.
define button b-done label "Done".

define frame f-text    
    skip(1)
    "Use text widgets to add static text to your display, like this." 
        colon 5 skip(1)
    t1 colon 20 skip
    t2 colon 20 t3 colon 50 skip
    t4 colon 20 t5 colon 50 skip(1)
    b-done to 38
        with side-labels title "Learning to Use Text Widgets" 
             size-chars 76 by 10 at row 2 column 3.
            
define frame f-info2
    instruct label "Info" to 76
        with side-labels no-box size-chars 76 by 6
            at row 13 column 3.
            
display t1 t2 t3 t4 t5 with frame f-text.
enable b-done with frame f-text.
instruct = "Like fill-in fields, text widgets can represent all data
 types. However, text widgets are read-only: a user cannot interact
 with them. Text widgets can also add titles or static information to
 your displays. Choose DONE to return to the main display.".
instruct:read-only in frame f-info2 = yes.
display instruct with frame f-info2.
enable all with frame f-info2.
wait-for choose of b-done.
hide frame f-text.
hide frame f-info2.
end procedure.

procedure p-editor.
define variable e1 as character label "Notes" view-as editor
    inner-chars 40 inner-lines 4 scrollbar-vertical.
define button b-done label "Done".

define frame f-editor
    
    e1 at row 2 column 10
    b-done at row 8 column 35
        with side-labels title "Learning to Use Editors" 
             size-chars 76 by 10 at row 2 column 3.
            
define frame f-info2
    instruct label "Info" to 76
        with side-labels no-box size-chars 76 by 6
            at row 13 column 3.
 
e1 = "Type your notes here.".          
display e1 with frame f-editor.
enable all with frame f-editor.
instruct = "Editor widgets make reading, adding, and changing long
 strings of text easy for the user. Click an editor or press TAB to
 give it focus. Your normal text editing keys work in an editor. An
 editor can hold more text than you can see. The scollbar, which you
 can manipulate with a mouse or arrow keys, lets you move through the
 data. Choose DONE to return to the main display.".
instruct:read-only in frame f-info2 = yes.
display instruct with frame f-info2.
enable all with frame f-info2.
wait-for choose of b-done.
hide frame f-editor.
hide frame f-info2.
end procedure.

procedure p-toggle:
define variable t1 as logical label "Air Conditioning" view-as toggle-box.
define variable t2 as logical label "Leather Seats" view-as toggle-box.
define variable t3 as logical label "Extended Warranty" view-as toggle-box.
define variable t4 as logical label "Do you have a driver's license?" view-as toggle-box.
define variable t5
     
                  
                   as logical label "Do you have auto insurance?" view-as toggle-box.
define button b-done label "Done".

define frame f-toggle    
"New Car Order Form" colon 3 skip(1)
"Options:" colon 3 "Questions:" colon 30 skip
t1 colon 3 t4 colon 30 skip 
t2 colon 3 t5 colon 30 skip
t3 colon 3 skip (1)
b-done colon 35
    with side-labels title "Learning to Use Toggle Boxes" 
        size-chars 76 by 10 at row 2 column 3.
            
define frame f-info2
    instruct label "Info" to 76
        with side-labels no-box size-chars 76 by 6
            at row 13 column 3.
            
display t1 t2 t3 t4 t5 with frame f-toggle.
enable all with frame f-toggle.
instruct = "Toggle boxes represent TRUE/FALSE or YES/NO data. A filled
 toggle box indicates TRUE while an empty box indicates FALSE. Click a
 toggle box to change the value. With the keyboard, press TAB to move
 focus to it and then press SPACEBAR to change the value. Choose DONE
 to return to the main display.".  
instruct:read-only in frame f-info2 = yes.
display instruct with frame f-info2.
enable all with frame f-info2.
wait-for choose of b-done.
hide frame f-toggle.
hide frame f-info2.
end procedure.

procedure p-radio:
define variable r1 as character label "Size" initial "Large" 
    view-as radio-set vertical radio-buttons "Large", "Large", 
        "Medium", "Medium", "Small", "Small".
define variable r2 as character label "Legs" initial "4" 
    view-as radio-set horizontal 
        radio-buttons "4", "4", "2", "2", "0", "0".
define variable pet as character label "Perfect Pet" 
    initial "Great Dane" format "x(12)" view-as text.
define button b-done label "Done".

define frame f-radio 
    "Pet Selection Advisor" at row 1 column 5 
    r1 at row 3 column 5
    pet at row 4 column 35
    r2 at row 7 column 5
    b-done at row 7 column 35
        with side-labels title "Learning to Use Radio Sets" 
             size-chars 76 by 10 at row 2 column 3.
            
define frame f-info2
    instruct label "Info" to 76
        with side-labels no-box size-chars 76 by 6
            at row 13 column 3.
            
on value-changed of r1, r2
do:
    assign r1 r2.
    case r1:
    when "Large" then case r2:
                      when "4" then pet = "Great Dane".
                      when "2" then pet = "Ostrich".
                      when "0" then pet = "Palm Tree".
                      end.
    when "Medium" then case r2:
                      when "4" then pet = "Siamese Cat".
                      when "2" then pet = "Parrot".
                      when "0" then pet = "Python".
                      end.
    when "Small" then case r2:
                      when "4" then pet = "Hamster".
                      when "2" then pet = "Canary".
                      when "0" then pet = "Rock".
                      end.
    end.
    display pet with frame f-radio.
end.

            
display r1 r2 pet with frame f-radio.  
enable all with frame f-radio.  

if session:window-system = "MS-WINDOWS" then

instruct = "A radio set is a group of buttons that each represent
 one value from a limited set of possible values. Select a button to
 choose a value. Clicking a button with a mouse selects it. With a
 keyboard, press TAB until the radio set has focus, then use the
 arrow keys to select a button. Choose DONE to return to the main
 display.".    
 
else  

instruct = "A radio set is a group of buttons that each represent
 one value from a limited set of possible values. Select a button to
 choose a value. Clicking a button with a mouse selects it. With a
 keyboard, press TAB until the radio set has focus, then use the
 arrow keys to highlight a button. Press SPACEBAR to select a
 highlighted button. Choose DONE to return to the main display.".    
 
instruct:read-only in frame f-info2 = yes.
display instruct with frame f-info2.
enable all with frame f-info2.
wait-for choose of b-done.
hide frame f-radio.
hide frame f-info2.
end procedure.

procedure p-slider:
define variable s1 as integer label "Height" initial 1 
    view-as slider max-value 4 min-value 1 vertical size 7 by 4.
define variable s2 as integer label "Width" initial 1 
    view-as slider max-value 15 min-value 1 horizontal size 20 by 2.
define rectangle r-box size 1 by 1.
define button b-done label "Done".

define frame f-slid 
    "BoxMaker" at row 2 column 3   
    s1 at row 2 column 20
    s2 at row 7 column 28
    r-box at row 5 column 36
    b-done at row 4 column 4
        with side-labels title "Learning to Use Sliders" 
             size-chars 76 by 10 at row 2 column 3.
            
define frame f-info2
    instruct label "Info" to 76
        with side-labels no-box size-chars 76 by 6
            at row 13 column 3.
            
on value-changed of s1
    assign s1
           r-box:height = s1
           r-box:row = 6 - s1.
           
on value-changed of s2
assign s2
       r-box:width = s2.
            
display s1 s2 with frame f-slid.
enable all with frame f-slid.

if session:window-system = "MS-WINDOWS" then

instruct = "Sliders let you change integer values within a defined range
 by dragging the control with a mouse, clicking the arrow buttons, or
 using the arrow keys. To use the arrow keys, first move focus to a slider
 with the TAB key. Sliders are most appropriate for environments that
 include mouse support. Choose DONE to return to the main display.".   
 
else

instruct = "Sliders let you change integer values within a defined range
 by dragging the control with a mouse or using the arrow keys. To use
 the arrow keys, first move focus to a slider with the TAB key. Sliders
 are most appropriate for environments that include mouse support. Choose
 DONE to return to the main display.".   

instruct:read-only in frame f-info2 = yes.
display instruct with frame f-info2.
enable all with frame f-info2.
wait-for choose of b-done.
hide frame f-slid.
hide frame f-info2.
end procedure.

procedure p-select.
define variable s1 as character label "Vacation Paradises" initial "Bedford"
    view-as selection-list list-items "Aruba", "Bahamas", "Bedford", 
    "Belize", "Bermuda", "Hawaii", "Riviera", "Tahiti", "Virigin Islands"
    inner-chars 15 inner-lines 5 scrollbar-vertical.
define button b-done label "Done".

define frame f-slid 
    s1 at row 2 column 2
    b-done at row 5 column 46
        with side-labels title "Learning to Use Selection Lists" 
             size-chars 76 by 10 at row 2 column 3.
            
define frame f-info2
    instruct label "Info" to 76
        with side-labels no-box size-chars 76 by 6
            at row 13 column 3.
      
display s1 with frame f-slid.
enable all with frame f-slid.
instruct = "A selection list represents all the acceptable choices for
 one value. Clicking a list item selects it. With the keyboard, use TAB
 to give a list focus and then use the arrow keys to select an item.
 Since a selection list can hold more choices than it can show, drag the
 scrollbar or use the arrow keys to review all your choices. Choose DONE
 to return to the main display.".
instruct:read-only in frame f-info2 = yes.
display instruct with frame f-info2.
enable all with frame f-info2.
wait-for choose of b-done.
hide frame f-slid.
hide frame f-info2.
end procedure.
                                                                
procedure p-combo.
define variable ttle as character label "Title" initial "Mr."
    view-as combo-box list-items "Dr.", "Hon.", "Mr.", "Ms.".           
define variable fname as character label "First" format "x(12)" 
    initial "Pro".
define variable lname as character label "Last" format "x(20)" 
    initial "Gress".
define button b-done label "Done".

define frame f-cmb 
    ttle at row 2 column 2 fname lname
    b-done at row 5 column 35
        with side-labels title "Learning to Use Combo Boxes" 
             size-chars 76 by 10 at row 2 column 3.
            
define frame f-info2
    instruct label "Info" to 76
        with side-labels no-box size-chars 76 by 6
            at row 13 column 3.

display ttle fname lname with frame f-cmb.
enable all with frame f-cmb.     
   
if session:window-system = "MS-WINDOWS" then  

instruct = "A combo box
                        combines a fill-in field, a button, and a
 selection list. With a mouse, click the combo box button and a drop
 down list appears. Click an item to select it and close the list. With
 the keyboard, press TAB to move focus to the combo box and use the
 arrow keys to select an item without opening the drop down list. Choose
 DONE to return to the main display.". 

else if session:window-system = "OSF/Motif" then

instruct = "A combo box combines a fill-in field, a button, and a
 selection list. With a mouse, click the combo box button and a drop
 down list appears. Click an item to select it and close the list. With
 the keyboard, press TAB to focus it and press SPACEBAR to display the
 list. Use the arrow keys to select an item and press SPACEBAR to close
 the list. Choose DONE to return to the main display.".   
 
else

instruct = "A combo box combines a fill-in field, a button, and a
 selection list. With the keyboard, press TAB to move input focus
 to the combo box. Use the down arrow to open the drop down list,
 and then use the arrow keys to select an item. Press RETURN to
 close the list. Choose DONE to return to the main display.".
 
instruct:read-only in frame f-info2 = yes.
display instruct with frame f-info2.
enable all with frame f-info2.
wait-for choose of b-done.
hide frame f-cmb.
hide frame f-info2.
end procedure.

procedure p-diag.
define variable instruct2 as character view-as editor 
    inner-chars 44 inner-lines 10.
define button b-done label "Done".

define frame f-diag      
    instruct2 label "Info" at row 2 column 2
    b-done at row 14 column 28
        with side-labels size 60 by 16 at row 3 column 10
            view-as 
                    dialog-box title "Learning to Use Dialog Boxes".
     
instruct2 = "A dialog box is a separate interface that overlays your
 main interface, or even another dialog box. When a dialog box appears,
 you must respond to the dialog box before you can continue working with
 the interface underneath. Any other widget can be part of a dialog box,
 so you work with them exactly as you would in a main interface. Choose
 DONE to return to the main display.".
instruct2:read-only in frame f-diag = yes.
display instruct2 with frame f-diag.
enable all with frame f-diag.
wait-for choose of b-done.
end procedure.
