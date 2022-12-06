/* r-bar.p */

DEFINE SUB-MENU topic
        MENU-ITEM numbr     LABEL "Cust. Number"
        MENU-ITEM addr      LABEL "Address"
        MENU-ITEM othrinfo  LABEL "Other".
DEFINE SUB-MENU move  
        MENU-ITEM forward          LABEL "NextRec" ACCELERATOR "PAGE-DOWN"
        MENU-ITEM backward          LABEL "PrevRec" ACCELERATOR "PAGE-UP".
    DEFINE SUB-MENU quitit
        MENU-ITEM quititem   LABEL "E&xit".

DEFINE MENU mbar     MENUBAR
        SUB-MENU topic   LABEL "Topic"
        SUB-MENU move    LABEL "Move"
        SUB-MENU quitit  LABEL "E&xit".

ON CHOOSE OF MENU-ITEM numbr
        DISPLAY customer.cust-num.
ON CHOOSE OF MENU-ITEM addr
        DISPLAY customer.address customer.address2 customer.city 
                customer.state customer.postal-code WITH FRAME addr-frame NO-LABELS 
                COLUMN 25.
ON CHOOSE OF MENU-ITEM othrinfo
        DISPLAY customer EXCEPT name cust-num address 
                address2 city state postal-code
                WITH FRAME oth-frame SIDE-LABELS.
ON CHOOSE OF MENU-ITEM forward
        DO:
                HIDE ALL NO-PAUSE.
                CLEAR FRAME name-frame.
                FIND NEXT customer NO-ERROR.
                IF AVAILABLE(customer)
                THEN DISPLAY customer.name WITH FRAME name-frame. 
        END.
ON CHOOSE OF MENU-ITEM backward
        DO:
                HIDE ALL NO-PAUSE.
                CLEAR FRAME name-frame.
                FIND PREV customer NO-ERROR.
                IF AVAILABLE(customer)
                THEN DISPLAY customer.name WITH FRAME name-frame.
        END.         
    
FIND FIRST customer.
DISPLAY customer.name LABEL "Customer Name" WITH FRAME name-frame.
ASSIGN CURRENT-WINDOW:MENUBAR = MENU mbar:HANDLE.
WAIT-FOR CHOOSE OF MENU-ITEM quititem.
