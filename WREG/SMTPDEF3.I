/*SMTPDEF3.I*/
 /*
 
 
*/                     
DEFINE VARIABLE mailhub         AS CHARACTER NO-UNDO. /*smtpserver*/                                                                                                                                                  
DEFINE VARIABLE EmailTo         AS CHARACTER NO-UNDO. /*till en eller flera ,*/                                                                                                                                       
DEFINE VARIABLE EmailFrom       AS CHARACTER NO-UNDO. /*från en eller flera ,*/                                                                                                                                       
DEFINE VARIABLE EmailCC         AS CHARACTER NO-UNDO. /*copia till 0 eller flera ,*/                                                                                                                                  
DEFINE VARIABLE Attachmentstyp     AS CHARACTER NO-UNDO. 
/*file[:type=<mimetype>][:charset=<charset>][:filetype=<filetype>]*/
/*ex seb01:type=text/html:CHARACTERset=ascii:filetype=ascii"*/
DEFINE VARIABLE LocalFiles      AS CHARACTER NO-UNDO. /*filer 0 eller flera ,*/                                                                                                                                       
DEFINE VARIABLE Subject         AS CHARACTER NO-UNDO. /*ämne*/                                                                                                                                                        
DEFINE VARIABLE Bodysmtp        AS CHARACTER NO-UNDO. /*body*/                                                                                                                                                        
DEFINE VARIABLE MIMEHeader      AS CHARACTER NO-UNDO. /*MIMEHeader  CHARACTER - [type=<mimetype>][:CHARACTERset=<chrset>][:filetype=<type>]*/                                                       
DEFINE VARIABLE BodyType        AS CHARACTER NO-UNDO. /*BodyType text om du skapar html direkt i bodyn eller file om du skapar en htmlfil först*/                                                                  
DEFINE VARIABLE Importance      AS INTEGER NO-UNDO.   /*INT - Importance flag for the mail header,of the message sent. Valid values include 0 to 3, 1 = HIGH; 3 = Low */
DEFINE VARIABLE L_DoAUTH        AS LOGICAL NO-UNDO.   /*LOGICAL - yes if authentication is requiered*/                     
DEFINE VARIABLE C_AuthType      AS CHARACTER NO-UNDO. /*CHAR - Type of authentication. Currently supported types:base64 */ 
DEFINE VARIABLE C_User          AS CHARACTER NO-UNDO. /*CHAR - The name of the SMTP server user*/                          
DEFINE VARIABLE C_Password      AS CHARACTER NO-UNDO. /*CHAR - The password of the SMTP server user*/                      
DEFINE VARIABLE oSuccessful     AS LOGICAL NO-UNDO.   /*Om true är allt ok*/                                                                                                                                                            
DEFINE VARIABLE vMessage        AS CHARACTER NO-UNDO. /*meddelande i klartext hur sändninge gick*/
/*EX. vanligt mail*/
/*
ASSIGN 
mailhub             = "mail.obbit.se"     
EmailTo             = "elpool.ume@elpool.se" 
EmailFrom           = "server@elpool.se"
EmailCC             = ""
Attachmentstyp      = "seb01.jpg:type=image/jpeg:charset=iso-8859-1:filetype=binary,andersl40.doc:type=application/msword:charset=iso-8859-1:filetype=binary"
LocalFiles          = "c:\protemp9\seb01.jpg,c:\protemp9\andersl40.doc"
Subject             = "TEST AV SMTP MED TVÅ FILER"
Bodysmtp            = "c:\protemp9\Boende.html"
MIMEHeader          = "type=text/html:charset=iso-8859-1:filetype=ascii"
BodyType            = "".
RUN smtpmail_UI (INPUT FALSE).
t.txt:type=text/plain:charset=iso-8859-1:filetype=ascii
seb01.jpg:type=image/jpeg:charset=iso-8859-1:filetype=binary                                                                                   
andersl40.xls:type=application/msexcel:charset=iso-8859-1:filetype=binary
andersl40.doc:type=application/msword:charset=iso-8859-1:filetype=binary
prova.pdf:type=application/pdf:filetype=binary
*/



/*EX. html i bodyn via en redan skapad html fil*/
/*
ASSIGN 
mailhub             = "mail.obbit.se"     
EmailTo             = "elpool.ume@elpool.se" 
EmailFrom           = "anders@elpool.se"
EmailCC             = ""
Attachmentstyp      = ""
LocalFiles          = ""
Subject             = "TEST AV SMTP MED html i body åäö"
Bodysmtp            = "c:\protemp9\Boende.html"
MIMEHeader          = "type=text/html:charset=iso-8859-1:filetype=ascii"
BodyType            = "file".
RUN smtpmail_UI (INPUT FALSE). */
/*SMTPDEF3.I*/
PROCEDURE smtpmail_UI:
   DEFINE INPUT PARAMETER appat AS LOGICAL NO-UNDO.
   {AMERICANEUROPEAN.I}
   {SMTPATT.I}
   
   IF appat = TRUE AND Guru.Konstanter:appcon = TRUE THEN DO:  
      RUN SMTPMAIL3.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
      (INPUT mailhub,        /*smtpserver*/                                                                                    
       INPUT EmailTo,        /*till en eller flera ,*/                                                                         
       INPUT EmailFrom,      /*från en eller flera ,*/                                                                         
       INPUT EmailCC,        /*copia till 0 eller flera ,*/                                                                    
       INPUT Attachmentstyp, /*[type=<mimetype>][:CHARACTERset=<chrset>][:filetype=<type>]*/                                   
       INPUT LocalFiles,     /*filer 0 eller flera ,*/                                                                         
       INPUT Subject,        /*ämne*/                                                                                          
       INPUT Bodysmtp,       /*body*/                                                                                          
       INPUT MIMEHeader,     /*[type=<mimetype>][:CHARACTERset=<chrset>][:filetype=<type>]*/                                                                                          
       INPUT BodyType,       /*text om du skapar html direkt i bodyn eller file om du skapar en htmlfil först*/                
       INPUT Importance,     /*INT - Importance flag for the mail header,of the message sent. Valid values include 0 to 3, 1 = HIGH; 3 = Low */
       INPUT L_DoAUTH,       /*LOGICAL - yes if authentication is requiered*/
       INPUT C_AuthType,     /*CHAR - Type of authentication. Currently supported types:base64 */
       INPUT C_User,         /*CHAR - The name of the SMTP server user*/
       INPUT C_Password,     /*CHAR - The password of the SMTP server user*/
       OUTPUT oSuccessful,   /*Om true är allt ok*/                                                                            
       OUTPUT vMessage).     /*meddelande i klartext hur sändninge gick*/                                       
   END.
   ELSE DO:
     
      RUN SMTPMAIL3.P  
      (INPUT mailhub,        /*smtpserver*/                                                                                    
      INPUT EmailTo,        /*till en eller flera ,*/                                                                         
      INPUT EmailFrom,      /*från en eller flera ,*/                                                                         
      INPUT EmailCC,        /*copia till 0 eller flera ,*/                                                                    
      INPUT Attachmentstyp, /*[type=<mimetype>][:CHARACTERset=<chrset>][:filetype=<type>]*/                                   
      INPUT LocalFiles,     /*filer 0 eller flera ,*/                                                                         
      INPUT Subject,        /*ämne*/                                                                                          
      INPUT Bodysmtp,       /*body*/                                                                                          
      INPUT MIMEHeader,     /*[type=<mimetype>][:CHARACTERset=<chrset>][:filetype=<type>]*/                                                                                          
      INPUT BodyType,       /*text om du skapar html direkt i bodyn eller file om du skapar en htmlfil först*/                
      INPUT Importance,     /*INT - Importance flag for the mail header,of the message sent. Valid values include 0 to 3, 1 = HIGH; 3 = Low */
      INPUT L_DoAUTH,       /*LOGICAL - yes if authentication is requiered*/
      INPUT C_AuthType,     /*CHAR - Type of authentication. Currently supported types:base64 */
      INPUT C_User,         /*CHAR - The name of the SMTP server user*/
      INPUT C_Password,     /*CHAR - The password of the SMTP server user*/
      OUTPUT oSuccessful,   /*Om true är allt ok*/                                                                            
      OUTPUT vMessage).     /*meddelande i klartext hur sändninge gick*/
   END.  
   {EUROPEANAMERICAN.I} 
END PROCEDURE.

PROCEDURE typ_UI :
   DEFINE VARIABLE antalvar AS INTEGER NO-UNDO.
   DEFINE VARIABLE maxantal AS INTEGER NO-UNDO.
   DEFINE VARIABLE filhelp AS CHARACTER NO-UNDO.
   DEFINE VARIABLE slash AS CHARACTER NO-UNDO.
   DEFINE VARIABLE typevar AS CHARACTER NO-UNDO.
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
            filhelp = ENTRY(antalvar, LocalFiles,";").
            IF INDEX(filhelp,".") > 0 THEN DO: 
               IF SUBSTRING(filhelp,INDEX(filhelp,".") + 1,3) = "xls" THEN typevar = ":type=application/msexcel:charset=iso-8859-1:filetype=binary".
               ELSE IF SUBSTRING(filhelp,INDEX(filhelp,".") + 1,3) = "doc" THEN typevar = ":type=application/msword:charset=iso-8859-1:filetype=binary".
               ELSE IF SUBSTRING(filhelp,INDEX(filhelp,".") + 1,3) = "jpg" THEN typevar = ":type=image/jpeg:charset=iso-8859-1:filetype=binary".
               ELSE IF SUBSTRING(filhelp,INDEX(filhelp,".") + 1,3) = "pdf" THEN typevar = ":type=application/pdf:charset=iso-8859-1:filetype=binary".
               ELSE typevar = ":type=text/plain:charset=iso-8859-1:filetype=ascii".
            END.   
            ELSE typevar = ":type=text/plain:charset=iso-8859-1:filetype=ascii".  
            IF Attachmentstyp NE "" THEN Attachmentstyp = Attachmentstyp + ";".
            Attachmentstyp = Attachmentstyp + SUBSTRING(filhelp, R-INDEX(filhelp,slash) + 1, LENGTH(filhelp)) + typevar.
                       
            antalvar = antalvar + 1.
            IF antalvar > maxantal THEN LEAVE.
         END.
      END.
      ELSE DO:
         Attachmentstyp = SUBSTRING(LocalFiles, R-INDEX(LocalFiles, "\") + 1, LENGTH(LocalFiles)).
      END.
   END.
   Attachmentstyp = REPLACE(Attachmentstyp,";",",").
   
END PROCEDURE.
