/*************************************************************/
/* Copyright (c) 1984-1995 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from Progress Software Corporation. */
/*************************************************************/

/******************************************************************************
    Procedure :	pinit.i
    Purpose   : Editor Initialization Procedures.
******************************************************************************/
    

PROCEDURE InitEditor.
/*----------------------------------------------------------------------------
Syntax:
        RUN InitEditor.

Description:

  Runs all PROGRESS Editor Initializatin routines.
  

Author: John Palazzo
----------------------------------------------------------------------------*/

  /*---------------- Initialize Stuff ----------------*/
 
  /* Set global active ade tool procedure handle to Procedure Editor. */
  ASSIGN h_ade_tool = THIS-PROCEDURE.

  RUN InitSysOpts .
  RUN InitColors .
  RUN InitTemps .
  RUN InitFileList .
  RUN InitFindReplace .

  
  /*----------------------------------------------------------------------
     In GUI or CUI, always have editor set CURRENT-WINDOW to DEFAULT-WINDOW
     when about to run user's code.  This will allow user to write code
     which will behaive the same when run from the editor or as the
     PROGRESS -p startup procedure (which always uses DEFAULT-WINDOW by
     default) .
  ----------------------------------------------------------------------*/
  ASSIGN Run_Window             = DEFAULT-WINDOW
         DEFAULT-WINDOW:VISIBLE = ( SESSION:WINDOW-SYSTEM = "TTY" ) 
  .
  
  /*----------------------------------------------------------------------
     Open initial editor window and assign values to system defines:
     win_ProEdit, and ProEditor.
  ----------------------------------------------------------------------*/
  RUN OpenWindow.
 
  /* If no files to open on start-up, then open with untitled. */
  IF LENGTH( p_File_List ) = 0 THEN
    RUN NewFile.
  ELSE DO:
    RUN FileOpen
        ( INPUT        win_ProEdit ,
          INPUT        p_File_List ,
          INPUT        FALSE , /* Don't allow duplicates. */
          INPUT-OUTPUT ProEditor ).
    /* Be sure editor was able to find and open specified file(s). */
    FIND FIRST Edit_Buffer NO-ERROR.
    IF NOT AVAILABLE Edit_Buffer THEN RUN NewFile.
    File_Name = ProEditor:PRIVATE-DATA.
    APPLY "ENTRY" TO ProEditor.
  END.

  
END PROCEDURE . /* InitEditor. */


PROCEDURE InitSysOpts .
/*----------------------------------------------------------------------------
Syntax:
        RUN InitSysOpts .

Description:

  Initializes editor's System Options defaults.
  

Author: John Palazzo

Date Created: 11.17.92 
----------------------------------------------------------------------------*/

  CREATE Sys_Options .

  RUN SysOptsGetPut ( INPUT "GET" ) .
   
END PROCEDURE . /* InitSysOpts */


PROCEDURE InitColors .
/*----------------------------------------------------------------------------
Syntax:
        RUN InitColors .

Description:

  Initializes editor's fg/bg colors.

Author: John Palazzo
----------------------------------------------------------------------------*/

  IF ( SESSION:WINDOW-SYSTEM = "TTY" )
      THEN RETURN.
  
  /* ADE Standard Vars from adecomm/adestds.i. */
  IF ( Sys_Options.BG_Color = ? )
      THEN ASSIGN Sys_Options.BG_Color = std_ed4gl_bgcolor NO-ERROR.
  IF ( Sys_Options.FG_Color = ? )
      THEN ASSIGN Sys_Options.FG_Color = std_ed4gl_fgcolor NO-ERROR.
END PROCEDURE.



PROCEDURE InitTemps.
/*----------------------------------------------------------------------------

Syntax:
        RUN InitTemps.

Description:

  Initializes the editor's temporary file names and locations.

Author: John Palazzo

Date Created: 08.14.92 

----------------------------------------------------------------------------*/

  DO ON STOP UNDO, LEAVE:
    RUN adecomm/_tmpfile.p ( "r" , ".ped" , OUTPUT Compile_FileName ).
    RUN adecomm/_tmpfile.p ( "m" , ".ped" , OUTPUT Compiler_Message_Log ).
  END.
    
END PROCEDURE . /* InitTemps. */


PROCEDURE InitFileList .
/*----------------------------------------------------------------------------
  Syntax:
        RUN InitFileList .

  Description:

    Initializes editor's startup Buffer List.
    Uses p_Edit_Command (defined in adeedit/dsystem.i).  Behavoir is as
    follows:
        1. "NEW"         - Ignore p_File_List and Saved Buffer List.  Gives
                           user Empty Untitled buffer.
        2. Anything Else - If p_File_List has value, use it.
                           Otherwise, use Saved Buffer List.
  
  Author: John Palazzo
----------------------------------------------------------------------------*/

    CASE p_Edit_Command :
        WHEN "NEW"
            THEN DO:
                /* Ignore Saved Buf List and return Null open list. */
                ASSIGN p_File_List = "".
            END.
            
        OTHERWISE
            DO:
            /*--------------------------------------------------------------
                If an ADE Tool is passing files in to the editor or the user
                passed files to the editor using -param, don't overwrite.
                Otherwise, we do use Saved Buffer List.
            --------------------------------------------------------------*/
                IF ( p_File_List = "" ) OR ( p_File_List = ? )
                THEN DO ON ERROR UNDO, LEAVE:
                    /* Reset to default Progress Environment File. */
                    USE "" NO-ERROR.
                    RUN adecomm/_kvlist.p ( INPUT "GET" ,
                                            INPUT KeyValue_Section ,
                                            INPUT "BufList",
                                            INPUT-OUTPUT Sys_Options.BufList ).
                    ASSIGN p_File_List = Sys_Options.BufList.
                END.
            END.
    END CASE.

END PROCEDURE . /* InitFileList */


PROCEDURE InitFindReplace .
/*----------------------------------------------------------------------------
  Syntax:
        RUN InitFindReplace .

  Description:

    Initializes editor's Find and Replace Alert-Box Titles.
    May some day restore Find/Replace option settings from Environment file.
    Someday.
  
  Author: John Palazzo

  Date Created: 12.10.92 

----------------------------------------------------------------------------*/

  /* These are defined in adeedit/dsearch.i and adeedit/dedit.i. 
     Used for Application Style.
  */
  ASSIGN Search_Info_Title  = Editor_Name 
         Search_Quest_Title = Editor_Name 
         Edit_Error_Title   = Editor_Name .
         
END PROCEDURE . /* InitFindReplace */










