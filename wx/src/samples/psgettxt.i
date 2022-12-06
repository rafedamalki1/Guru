/************************************************************************************

        PROCEDURE: psgettxt.i



        PURPOSE:   Getting text from the user



        SYNTAX:    "{samples/psgettxt.i}"



        REMARKS:   



        PARAMETERS:NONE



        AUTHORS:   Progress Consulting

        DATE:      March 1993



        LAST INSPECTED:

        INSPECTED BY:



 ************************************************************************************/

 /* Copyright(c) PROGRESS SOFTWARE CORPORATION, 1993 - All Rights Reserved.            */



/*Code_Start*/



/* PROGRESS PostScript Interface */

/* psgettxt.i */



        /* Getting text interactively from the user */

FORM {samples/pstxtfrm.f} FRAME text-frame VIEW-AS DIALOG-BOX.

PAUSE 0.

UPDATE label-font label-size WITH FRAME text-frame.

REPEAT WITH FRAME text-frame:
    CREATE textline.
    UPDATE textstr textfont textsize textctr textxpos textypos.
    num-lines = num-lines + 1.
END.
HIDE FRAME text-frame NO-PAUSE.



