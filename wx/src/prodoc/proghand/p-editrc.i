/* An include file to illustrate the use of preprocessor names.          */
/* PREPROCESSOR             DESCRIPTION                                  */
/*     NAME                                                              */
/*                                                                       */
/* ifile_ver   : The version of this file. The name is used with the     */
/*               preprocessor name banner defined in the main            */
/*               procedure through deferred evaluation.                  */
/* display     : Combines the file field list and the frame name to make */
/*               a more managable statement when displaying and updating */
/*               database records.                                       */
/* but-list    : A list of allowable actions that can be performed       */
/*                 during execution of the browse utility.               */        
/* but-nums    : The number of user-specified actions allowed.           */
/*               Generated through the use of {&SEQUENCE}.               */
/*               Displayed during at compile time using &MESSAGE.        */

DEFINE BUFFER {&file_name} FOR {&file_name}.
DEFINE VARIABLE do_return AS LOGICAL INITIAL FALSE.

/* Do some initial setup work */
PAUSE 0 BEFORE-HIDE.

&IF "{&frame_name}" = "" &THEN  /* Make up a default frame name */
  &MESSAGE A frame name was not supplied, using default name
  &SCOPED-DEFINE frame_name Default_Frame
&ENDIF

&IF "{&form_file}" <> "" &THEN   /* Define a frame for the user */
  {{&form_file}}   /* This includes the form definition file if one is given */
&ELSE

  &MESSAGE A Form file was not supplied, using default form
  FORM {&file_fields} 
  WITH FRAME {&frame_name} ROW 7 CENTERED.
&ENDIF

&SCOPED-DEFINE ifile_ver 1.1a
&SCOPED-DEFINE display {&file_fields} WITH FRAME {&frame_name}

/* Define buttons to be used in browse as indicated by developer through*/ 
/* the preprocessor name {&use-buts} */

DEFINE BUTTON next_but LABEL "Next"
  TRIGGERS:
  ON CHOOSE RUN getrec (INPUT "Next"). /* On choose find nextrecord*/
  END TRIGGERS.

&IF LOOKUP("Next", "{&use-buts}") <> 0 &THEN 
/* Will enable button for NEXT */ 
  &SCOPED-DEFINE but-list next_but
  &SCOPED-DEFINE but-nums {&SEQUENCE}
&ENDIF

DEFINE BUTTON prev_but LABEL "Prev"
  TRIGGERS:
  ON CHOOSE RUN getrec (INPUT "Prev"). /* On choose find prev record   */
  END TRIGGERS.

&IF LOOKUP("Prev", "{&use-buts}") <> 0 &THEN
/* Will enable button for PREV */ 
 &SCOPED-DEFINE but-list {&but-list} prev_but 
  &SCOPED-DEFINE but-nums {&SEQUENCE}
&ENDIF

DEFINE BUTTON up_but LABEL "Update"       /* On choose update record */
  TRIGGERS:
  ON CHOOSE UPDATE {&display}.
  END TRIGGERS.

&IF LOOKUP("Update", "{&use-buts}") <> 0 &THEN  
/* Enable button for UPDATE */ 
  &SCOPED-DEFINE but-list {&but-list} up_but 
  &SCOPED-DEFINE but-nums {&SEQUENCE}
&ENDIF

DEFINE BUTTON cr_but LABEL "Create"   /* On choose create and update */
  TRIGGERS:
  ON CHOOSE
  DO:
    DEFINE BUFFER localbuf FOR {&file_name}.
    /* First save current location in the file */
    FIND FIRST localbuf WHERE ROWID(localbuf) = ROWID({&file_name}).
    /* Make the new record */
    CREATE {&file_name}.
    UPDATE {&display}.
    /* Restore the location in the file */
    FIND FIRST {&file_name} WHERE ROWID({&file_name}) = ROWID(localbuf).
  END.
  
  ON END-ERROR RETURN NO-APPLY. 
/* If end-error, restores current location */ 

  END TRIGGERS.

&IF LOOKUP("Create", "{&use-buts}") <> 0 &THEN 
/* Enable button for CREATE */ 
  &SCOPED-DEFINE but-list {&but-list} cr_but 
  &SCOPED-DEFINE but-nums {&SEQUENCE}
&ENDIF

DEFINE BUTTON del_but LABEL "Delete"     /* On choose, delete record */
  TRIGGERS:         
  ON CHOOSE RUN delrec.
  END TRIGGERS.

&IF LOOKUP("Delete", "{&use-buts}") <> 0 &THEN 
/* Enable button for DELETE */ 
  &SCOPED-DEFINE but-list {&but-list} del_but 
  &SCOPED-DEFINE but-nums {&SEQUENCE}
&ENDIF

DEFINE BUTTON ret_but LABEL "Return"
  TRIGGERS:
  ON CHOOSE do_return = TRUE.
  END TRIGGERS.

DEFINE BUTTON quit_but LABEL "Quit"
  TRIGGERS.
  ON CHOOSE QUIT.
  END TRIGGERS.

&SCOPED-DEFINE but-list {&but-list} ret_but quit_but
&MESSAGE {&but-nums} Out of 5 user definable buttons are active

FORM  next_but Prev_but up_but cr_but Del_but ret_but quit_but
  WITH FRAME but_fr ROW 4 CENTERED 
  TITLE "Work With {&file_name} Records" . 

DISPLAY "{&Banner}" WITH FRAME ban_fr CENTERED .
VIEW FRAME but_fr.
ENABLE {&but-list} WITH FRAME but_fr.
REPEAT:  /* Cycle through records, or add one. */
  WAIT-FOR CHOOSE OF  next_but,Prev_but,up_but,cr_but,Del_but,
                         ret_but, quit_but IN FRAME but_fr. 
  IF do_return THEN
    RETURN.
  DISPLAY {&display}.
END.

/**********************************************************************/
/* Procedure to find records in the database file being modified.     */
/* It takes one parameter, action, which indicates whether you move   */
/* forward or backwards in the file.                                  */
/**********************************************************************/

PROCEDURE getrec.
  DEFINE INPUT PARAMETER action AS CHARACTER.
  DEFINE BUFFER localbuf FOR {&file_name}.

  IF action = "Next" OR action = "" THEN
  DO:
    FIND LAST localbuf.
    IF ROWID(localbuf) = ROWID({&file_name}) THEN
    DO:
      BELL.
      MESSAGE "You are already at the end of this file.".
      RETURN ERROR.
    END.
    ELSE
      FIND NEXT {&file_name} .
  END.
  ELSE
  DO:
    FIND FIRST localbuf.
    IF ROWID(localbuf) = ROWID({&file_name}) THEN
    DO:
      BELL.
      MESSAGE "You are already at the start of this file.".
      RETURN ERROR.
    END.
    ELSE
    DO:
      FIND PREV {&file_name} .
    END.
  END.
END PROCEDURE.   /* getrec */

/************************************************************************/
/* Procedure to delete a record from the file.  Cannot delete the last  */
/* record from the file if it is the only record in the file. When the  */
/* record is deleted, the next record is found. If this is not available*/
/* the previous record is found.                                       */
/************************************************************************/

PROCEDURE delrec.
  DEFINE BUFFER localbuf FOR {&file_name}.

  /* Before delete of record, find another record to display after */
  FIND localbuf WHERE ROWID(localbuf) = ROWID({&file_name}).
  /* find the next record */
  FIND NEXT {&file_name} NO-ERROR.
  IF NOT AVAILABLE {&file_name} THEN
  DO:
    /* reset location in file */
    FIND {&file_name} WHERE ROWID({&file_name}) = ROWID(localbuf).
    /* find previous record */ 
    FIND PREV {&file_name} NO-ERROR.
  END.
  IF NOT AVAILABLE {&file_name} THEN
  DO:
    BELL.
    MESSAGE "You may not delete the last record in the file.".
    RETURN ERROR.
  END.
  ELSE  
    DELETE localbuf.
END PROCEDURE.  /* delrec */

