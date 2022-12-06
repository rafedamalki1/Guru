/*SMTPATT.I*/
RUN typ_UI.
/*
DEFINE VARIABLE antalvar AS INTEGER NO-UNDO.
   DEFINE VARIABLE maxantal AS INTEGER NO-UNDO.
   DEFINE VARIABLE filhelp AS CHARACTER NO-UNDO.
   DEFINE VARIABLE slash AS CHARACTER NO-UNDO.
   slash = "\".
   LocalFiles = REPLACE(LocalFiles,",",";").
   Attachmentstyp = LocalFiles.
   IF INDEX(Attachmentstyp,"/") > 0 THEN slash = "/".
   IF INDEX(Attachmentstyp,slash) > 0 THEN DO:
      Attachmentstyp = "".
      maxantal = NUM-ENTRIES(LocalFiles,";").
      IF maxantal > 0 THEN DO:
         antalvar = 1.
         REPEAT:
            /*
            seb01.jpg:type=image/jpeg:charset=iso-8859-1:filetype=binary
            */
            filhelp = ENTRY(antalvar, LocalFiles,";").
            IF Attachmentstyp = "" THEN Attachmentstyp = SUBSTRING(filhelp, R-INDEX(filhelp,slash) + 1, LENGTH(filhelp)) + ":type=text/plain:charset=iso-8859-1:filetype=ascii".
            ELSE Attachmentstyp = Attachmentstyp + ";" + SUBSTRING(filhelp, R-INDEX(filhelp,slash) + 1, LENGTH(filhelp)) + ":type=text/plain:charset=iso-8859-1:filetype=ascii".
            antalvar = antalvar + 1.
            IF antalvar > maxantal THEN LEAVE.
         END.
      END.
      ELSE DO:
         Attachmentstyp = SUBSTRING(LocalFiles, R-INDEX(LocalFiles, "\") + 1, LENGTH(LocalFiles)).
      END.
   END.
   Attachmentstyp = REPLACE(Attachmentstyp,";",",").
   */                    
