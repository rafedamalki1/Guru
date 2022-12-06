/************************************************************************************

        PROCEDURE: notepad.p



        PURPOSE:   Notepad (Messages) utility



        SYNTAX:    "RUN samples/notepad.p"



        REMARKS:   This code uses TEMPORARY TABLES and displays save messages 

                   for a user



        PARAMETERS:NONE



        AUTHORS:   Judy Rothermal

        DATE:      February 1993



        LAST INSPECTED:

        INSPECTED BY:



 ************************************************************************************/

 /* Copyright(c) PROGRESS SOFTWARE CORPORATION, 1993 - All Rights Reserved.            */



 /*Code_Start*/

 

DEFINE VARIABLE np_lin     AS CHARACTER                NO-UNDO.

DEFINE VARIABLE np_pad     AS CHARACTER FORMAT "!"     NO-UNDO.

DEFINE VARIABLE search_pad AS CHARACTER                NO-UNDO.

DEFINE VARIABLE np_text    AS CHARACTER FORMAT "x(40)" NO-UNDO.

DEFINE VARIABLE load_file  AS CHARACTER                NO-UNDO.

DEFINE VARIABLE ok_status  AS LOGICAL                  NO-UNDO.



DEFINE BUTTON   btn_cancel LABEL "CANCEL" AUTO-ENDKEY. 

DEFINE BUTTON   btn_search LABEL "SEARCH" .

DEFINE BUTTON   btn_exit   LABEL "EXIT"   AUTO-ENDKEY.

DEFINE BUTTON   btn_save   LABEL "SAVE"   AUTO-GO.



DEFINE TEMP-TABLE notepad 

   FIELD  Usr   AS CHAR  FORMAT "x(8)" INIT ? LABEL "User" 

   FIELD  Code  AS CHAR  FORMAT "x" LABEL "Code"

   FIELD  Txt   AS CHAR  FORMAT "x(80)"  LABEL "Text"

   INDEX  Notepad IS UNIQUE PRIMARY

      Usr  ASC

      Code ASC.

      

  

FORM np_lin  VIEW-AS EDITOR SIZE 60 BY 10 SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL

  btn_save AT ROW 6 COL 65 btn_cancel AT ROW 8 COL 65

  WITH FRAME frm_edit 

  NO-LABELS OVERLAY CENTERED VIEW-AS DIALOG-BOX TITLE "Notepad " + np_pad.

 

load_file = SEARCH("notepad.dat").

IF load_file <> ? THEN DO:

   INPUT FROM VALUE(load_file) NO-ECHO.

   REPEAT:                

      CREATE notepad.

      IMPORT notepad.

   END. /* REPEAT */

   INPUT CLOSE.

END.



FORM SKIP(1)

    "  Edit which of your Notepads" np_pad "(A to Z)  " SKIP

    "    (leave blank to search)" SKIP(1)

    btn_search AT 15 btn_exit 

    SKIP(1)

    WITH FRAME frm_which OVERLAY COLUMN 1 NO-LABELS CENTERED ROW 3

    VIEW-AS DIALOG-BOX.

  

ON CHOOSE OF btn_search IN FRAME frm_which DO:

   _notepad: 

   DO WHILE TRUE:

      DO ON ERROR UNDO,LEAVE ON ENDKEY UNDO,LEAVE:

         PAUSE 0.

         UPDATE SKIP(1)

            "Enter the text to search for" np_text SKIP(1)

             WITH FRAME frm_search OVERLAY ROW 4 NO-LABELS

             VIEW-AS DIALOG-BOX.

         HIDE FRAME frm_search NO-PAUSE.
      END.

      IF np_text = "" OR KEYFUNCTION(LASTKEY) = "END-ERROR" THEN DO:

         HIDE FRAME frm_search NO-PAUSE.

         RETURN.

      END.

      FOR EACH Notepad WHERE 

               Usr = USERID("notepad") AND INDEX(Txt,np_text) > 0:

          ASSIGN

             search_pad = Code.

          LEAVE.

      END.

      IF search_pad = "" THEN DO:

         DISPLAY SPACE(29) "Text not found.  Try again." SKIP(1) 

              WITH FRAME frm_search.

         NEXT _notepad.

      END.

      ELSE DO:

         np_pad = search_pad.

         RUN note-disp.

         ENABLE ALL WITH FRAME frm_edit.

         UPDATE np_lin WITH FRAME frm_edit.

         DISABLE ALL WITH FRAME frm_edit.

         HIDE FRAME frm_edit NO-PAUSE.

      END. /* found */

      LEAVE _notepad.

   END. /* _notepad */

   HIDE FRAME frm_search NO-PAUSE.

END. /* SELECTION OF btn_search */

   

ON CHOOSE OF btn_save IN FRAME frm_edit DO:

   DO WITH FRAME frm_edit:

      IF INPUT np_lin <> "" THEN DO TRANSACTION:

         FIND Notepad

              WHERE Usr = USERID("notepad") AND 

                    Code = np_pad NO-ERROR.

         IF NOT AVAILABLE Notepad THEN DO:

            CREATE Notepad.

            ASSIGN

               notepad.Usr  = USERID("notepad")

               notepad.Code = np_pad.

         END. /* notepad not found */

         notepad.Txt = INPUT np_lin.

      END. /* DO TRANSACTION  */

   END.    /* DO WITH FRAME   */

END.      /* ON CHOOSE OF btn_save */

    

ON RETURN OF np_pad IN FRAME frm_which DO:

   IF INPUT FRAME frm_which np_pad <> "" THEN DO:

      ASSIGN INPUT FRAME frm_which np_pad.

      HIDE FRAME frm_which.

      RUN note-disp.

      ENABLE ALL WITH FRAME frm_edit.

      UPDATE np_lin WITH FRAME frm_edit.

      DISABLE ALL WITH FRAME frm_edit.

      HIDE FRAME frm_edit NO-PAUSE.

   END. /* IF INPUT np_pad <> ""  */

   np_pad = "".

   ENABLE np_pad btn_exit btn_search WITH FRAME frm_which.

END. /* ON RETURN OF np_pad IN FRAME frm_which */



ON CHOOSE OF btn_exit DO:

   MESSAGE "(Saving notepad contents...)" .

   FOR EACH Notepad WHERE Usr = USERID("notepad") AND Txt = "":

      DELETE Notepad. /* do for all this user's notepads */  

   END.

   load_file = SEARCH("notepad.dat").

   DO ON ERROR UNDO, RETRY:

     IF RETRY THEN DO:

        MESSAGE "Notepad contents not saved due to error"

           VIEW-AS ALERT-BOX WARNING BUTTONS OK.

        UNDO, LEAVE.

     END. /* retry - state notepad not saved */

     

     IF load_file = ? THEN OUTPUT TO notepad.dat.

     ELSE OUTPUT TO VALUE(load_file).

     

     FOR EACH notepad ON ERROR UNDO, RETRY:

         IF RETRY THEN DO:

            MESSAGE "Notepad contents not saved due to error"

               VIEW-AS ALERT-BOX WARNING BUTTONS OK.

            UNDO, LEAVE.

         END. /* retry - state notepad not saved */

         EXPORT notepad.

     END. /* FOR EACH notepad */

     OUTPUT CLOSE.

   END.

END.



ENABLE np_pad btn_search btn_exit  WITH FRAME frm_which.

  

UPDATE np_pad WITH FRAME frm_which.



PROCEDURE note-disp.

   FIND Notepad

        WHERE Usr = USERID("notepad") AND Code = np_pad 

        NO-LOCK NO-ERROR. 

   ASSIGN

      np_lin = IF AVAILABLE NotePad THEN txt ELSE "".

   CLEAR FRAME frm_edit NO-PAUSE.

   DISPLAY np_lin WITH FRAME frm_edit.

END PROCEDURE. /* PROCEDURE */











