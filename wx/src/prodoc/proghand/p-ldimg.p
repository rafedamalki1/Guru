/* p-ldimg.p */

DEFINE VARIABLE filename AS CHARACTER.
DEFINE VARIABLE filter-string AS CHARACTER.
DEFINE VARIABLE got-file AS LOGICAL.
DEFINE VARIABLE status-ok AS LOGICAL.
DEFINE VARIABLE myimage AS WIDGET-HANDLE.

DEFINE BUTTON get-image LABEL "Get Image...".

FORM 
   WITH FRAME img-frame TITLE "IMAGE" WIDTH 40.

FORM
   get-image WITH FRAME but-frame.

ON CHOOSE OF get-image
  DO:
    SYSTEM-DIALOG GET-FILE filename 
             TITLE "Choose an Image to Display"
             FILTERS "Image files" filter-string
             MUST-EXIST
             UPDATE got-file.
         
    IF got-file
    THEN DO:
     status-ok = myimage:LOAD-IMAGE(filename).
     ASSIGN FRAME img-frame:HEIGHT-CHARS = myimage:HEIGHT-CHARS + 1
            FRAME img-frame:WIDTH-CHARS = 
                        MAX(myimage:WIDTH-CHARS + 2, 15). 
       VIEW FRAME img-frame.
    END.
  END.

CREATE IMAGE myimage
       ASSIGN FRAME = FRAME img-frame:HANDLE.
              
CASE SESSION:WINDOW-SYSTEM:
   WHEN "TTY"
     THEN MESSAGE "Images are not supported for this interface.". 
   OTHERWISE
     ASSIGN filter-string = "*.ico, *.bmp".

END CASE. 

IF filter-string = ""
THEN RETURN.
    
ENABLE get-image WITH FRAME but-frame.

WAIT-FOR WINDOW-CLOSE OF CURRENT-WINDOW.


