/*************************************************************/
/* Copyright (c) 1984-1994 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from Progress Software Corporation. */
/*************************************************************/

/**************************************************************************
    Procedure :  _pwgetfh.p
    
    Purpose   : Returns the handle of the Procedure Window Editor Frame
                given a Procedure Window handle.                
    
    Syntax    : RUN adecomm/_pwgetfh.p ( INPUT p_Window , OUTPUT p_Frame ).

    
    Parmameters :
    Description:
    Notes     : If the widget is an invalid widget or a pseudo-widget,
                routine returns Unknown (?) for p_Editor.
                                
    Authors   : J. Palazzo
**************************************************************************/
              
DEFINE INPUT  PARAMETER p_Window       AS WIDGET-HANDLE NO-UNDO.
    /* Handle of Editor Frame's Procedure window. */
DEFINE OUTPUT PARAMETER p_Frame AS WIDGET-HANDLE NO-UNDO.
    /* Handle of Procedure Window Editor Frame widget. */

DO:
    ASSIGN p_Frame = ?.
    /* PW's first child is Editor Frame. */
    IF p_Window:TYPE = "WINDOW" THEN ASSIGN p_Frame = p_Window:FIRST-CHILD.
    
END.
