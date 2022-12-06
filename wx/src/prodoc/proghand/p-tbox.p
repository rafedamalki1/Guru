/* p-tbox.p */

DEFINE VARIABLE choices   AS LOGICAL EXTENT 3
                          LABEL "Name", "Address", "Other"
                          INITIAL [yes, yes, no]
                          VIEW-AS TOGGLE-BOX. 
DEFINE VARIABLE curcust AS INTEGER.

FORM
    curcust LABEL "Customer Number" SKIP
    "Information to Show:" SKIP
    choices SKIP
    WITH SIDE-LABELS FRAME choice-frame TITLE "Show Customer Information".
    
FORM
    customer.name
    WITH FRAME name-frame NO-LABELS TITLE "Customer Name".

FORM
    customer.address SKIP customer.address2 SKIP
    customer.city customer.state SKIP customer.country customer.postal-code
    WITH FRAME addr-frame NO-LABELS USE-TEXT TITLE "Customer Address".
    
FORM
    customer
    EXCEPT name cust-num address address2 city state country postal-code
    WITH FRAME oth-frame SIDE-LABELS USE-TEXT TITLE "Other Customer Data".
    
ON GO OF FRAME choice-frame
   OR RETURN OF curcust
    DO:  
        HIDE FRAME name-frame FRAME addr-frame FRAME oth-frame.
          
        FIND  customer WHERE cust-num = INPUT curcust NO-ERROR.
        IF AMBIGUOUS customer OR NOT AVAILABLE customer
                THEN RETURN NO-APPLY.
        IF INPUT choices[1] /* name */
                THEN DISPLAY customer.name 
                        WITH FRAME name-frame.
	IF INPUT choices[2] /* address */
		THEN DISPLAY customer.address customer.address2
			customer.city customer.state country postal-code
			WITH FRAME addr-frame.
	IF INPUT choices[3] /* other */
		THEN DISPLAY customer EXCEPT name cust-num address
			address2 city state country postal-code
		        WITH FRAME oth-frame.
    RETURN NO-APPLY.
    END.
    

DISPLAY choices WITH FRAME choice-frame.
ENABLE ALL WITH FRAME choice-frame.
STATUS INPUT "Enter customer number and GO.".

WAIT-FOR WINDOW-CLOSE OF CURRENT-WINDOW.





