/************************************************************************************
	PROCEDURE: calcval.p

	PURPOSE:   Calculator that returns a value

	SYNTAX:    "RUN samples/calcval.p"

	REMARKS:   This code displays a calculator and returns the value

	PARAMETERS: row, column, result

	AUTHORS:   Progress Consulting
	DATE:      March 1993

	LAST INSPECTED:
	INSPECTED BY:

 ************************************************************************************/
 /* Copyright(c) PROGRESS SOFTWARE CORPORATION, 1993 - All Rights Reserved.	    */
 

define INPUT  PARAMETER the_row as integer.	/* Calculator row position */
define INPUT  PARAMETER the_col as integer.	/*         column position */
define OUTPUT PARAMETER the_calc as char.       /* The result              */

define temp-table tt NO-UNDO			/* Temporary table of      */
   field h_widget as widget-handle		/* widget handles	   */
   field h_num as int
   field h_lbl as char.
define variable butt_c  as char extent 20	/* Calculator buttons      */
   initial ["0",".","+/-","+","=","1","2","3","/","4","5","6","-","C","7",
   	    "8","9","X","CA"," "] 		NO-UNDO.
define variable butt_x  as int extent 20	/* Button x-positions      */
   initial [10 ,40 ,70 ,100,130,10 ,40 ,70 ,130,10 ,40 ,70 ,100,130,
            10 ,40 ,70 ,100,130,3  ] 		NO-UNDO.
define variable butt_y  as int extent 20	/* Button y-positions      */
   initial [150,150,150,120,150,120,120,120,120,90 ,90 ,90 ,90 ,90 ,
            60 ,60 ,60 ,60 ,60 ,3  ] 		NO-UNDO.
define variable number as widget-handle 	NO-UNDO.  /* fill-in var   */
define variable border as widget-handle		NO-UNDO.  /* border var    */
define variable butts as integer     		NO-UNDO.  /* loop var      */
define variable operation as char initial " "	NO-UNDO.  /* last operation*/
define variable clear_calc as Logic init False  NO-UNDO.  /* clear toggle  */
define variable memory  as char			NO-UNDO.  /* last number   */
FORM SKIP(8) with frame calc centered COL the_col Width 24 ROW the_row OVERLAY NO-BOX.
&scoped-def keys ".","c","C","=","+","-","x","X","/","0","1","2","3","4","5","6","7","8","9"

create rectangle border		/* A border within the frame for style     */
assign
    x = 0
    y = 0
    WIDTH-PIXELS = 166
    HEIGHT-PIXELS = 192
    EDGE-PIXELS = 1
    Frame = frame calc:handle.

pause 0 before-hide.
view frame calc.
create fill-in number		/* A fill-in to show the calculator digits */
/*71C* WARNING: CREATE FILL-IN now creates a PROGRESS fill-in instead of native */
assign
    Frame = frame calc:handle
    Col = 5
    Row = 2
    SCREEN-VALUE = "0"
    Format = "X(14)"
    SENSITIVE = NO.	/* Not sensitive, must press buttons */

REPEAT butts = 1 to 20: /* Create all button widgets dynamicaly  */
    define var calc_button as widget-handle no-undo.    
    create button calc_button
    assign
        x = butt_x[butts]
        y = butt_y[butts]
        HEIGHT-PIXELS = if butt_c[butts]="+" then 55 
                 else if butt_c[butts]=" " then 18 else 25 
        WIDTH-PIXELS =  if butt_c[butts]=" " then 10 else 25
        Label = butt_c[butts]
        Frame = frame calc:handle
        SENSITIVE = yes
    triggers:
        on CHOOSE persistent run calc-press.
        on {&keys} RUN do-key.
    end triggers.
    create tt.		/* store all button widgets in work table  */
    assign tt.h_widget = calc_button
    	   tt.h_num = butts
    	   tt.h_lbl = butt_c[butts].
    view calc_button.   /* view each button  */
end.
view border.
view number.
   
procedure do-key.
    FIND tt WHERE tt.h_lbl = caps(chr(lastkey)).
    APPLY "CHOOSE" TO tt.h_widget.    
END.
wait-for choose of calc_button.  /* Exit once the press last button */
the_calc = number:screen-value.   /* Return the final value */

PROCEDURE calc-press.
    find tt where tt.h_widget = SELF NO-ERROR.	/* Find the pressed button	*/
    IF NOT AVAILABLE tt THEN LEAVE.
/* If numeric digit is pressed, add it to number displayed:		*/    
    if CAN-DO("1,2,3,4,5,6,7,8,9,0",butt_c[tt.h_num]) then DO:
        if clear_calc then number:screen-value = "0".
    	number:screen-value = if number:screen-value = "0" then butt_c[tt.h_num]
                       else number:screen-value + butt_c[tt.h_num].
        clear_calc = NO.
        END.

/* If "." is pressed and is not already present, add it to number	*/
    if butt_c[tt.h_num]="." then
        if 0=index(number:screen-value,".") then DO:
            if clear_calc then number:screen-value = "0.".
    	    number:screen-value = if number:screen-value = "0" then "0."
                           else number:screen-value + ".".
       	    clear_calc = NO.
            END.

/* CA will clear number and memory					*/
    if butt_c[tt.h_num]="CA" then DO:
    	number:screen-value="0".
    	operation = " ".
    	END.
    
/* C will clear the current number being displayed but retain memory	*/
    if butt_c[tt.h_num]="C" then number:screen-value="0".
    
    if butt_c[tt.h_num]=" " then leave.
    if butt_c[tt.h_num]="+/-" then	/* Switch signs  */
    	number:screen-value = string(0 - decimal(number:screen-value)).

/* This case section, performs the mathematical operations:		*/
    if CAN-DO("=,+,-,/,X",butt_c[tt.h_num]) then DO:
    	clear_calc = YES.
    	CASE operation:
    	    WHEN " " OR WHEN "=" THEN DO: /* First operation */
       	    	operation = butt_c[tt.h_num].
                memory = number:screen-value.
                END.
            WHEN "+" THEN DO:
                number:screen-value = string(decimal(memory) + decimal(number:screen-value)).
                memory = number:screen-value.
                operation = butt_c[tt.h_num].
                END.
    	    WHEN "-" THEN DO:
                number:screen-value = string(decimal(memory) - decimal(number:screen-value)).
		memory = number:screen-value.
                operation = butt_c[tt.h_num].
                END.
            WHEN "/" THEN DO:
                number:screen-value = string(decimal(memory) / decimal(number:screen-value)).                memory = number:screen-value.
                operation = butt_c[tt.h_num].
                END.
            WHEN "X" THEN DO:
                number:screen-value = string(decimal(memory) * decimal(number:screen-value)).
                memory = number:screen-value.
                operation = butt_c[tt.h_num].
                END.
	END CASE.
    END.
END.
    












