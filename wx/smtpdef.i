/*SMTPDEF.I*/
 /*
     andvänds ej   
 
 
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
DEFINE VARIABLE oSuccessful     AS LOGICAL NO-UNDO.   /*Om true är allt ok*/                                                                                                                                                            
DEFINE VARIABLE vMessage        AS CHARACTER NO-UNDO. /*meddelade i klar text hur sändninge gick*/
/*EX
vanligt mail*/
/*
ASSIGN 
      mailhub             = "mail.obbit.se"     
      EmailTo             = "elpool.ume@elpool.se"  
      EmailFrom           = "server@elpool.se"     
      EmailCC             = ""
      Attachmentstyp      = ""
      LocalFiles          = ""
      Subject             = "Nya " + " från Guru"
      Bodysmtp            = "åäåö"
      MIMEHeader          = "type=text/html:charset=iso-8859-1:filetype=ascii"
      BodyType            = "".
      RUN smtpmail_UI (INPUT FALSE).
      */
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
*/

/*EX
html i bodyn via en redan skapad html fil*/
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
RUN smtpmail_UI (INPUT FALSE).          
 */
PROCEDURE smtpmail_UI:
   DEFINE INPUT PARAMETER appat AS LOGICAL NO-UNDO.
   IF globforetag = "sund" THEN EmailFrom = "webguru@sundsvallenergi.se".
   IF globforetag = "SNAT" THEN EmailFrom = "@guru.sundsvallelnat.se".
   IF globforetag = "MISV" THEN EmailFrom = "webguru@mittsverigevatten.se".
   IF appat = TRUE AND Guru.Konstanter:appcon = TRUE THEN DO:  
      RUN SMTPMAIL.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
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
       OUTPUT oSuccessful,   /*Om true är allt ok*/                                                                            
       OUTPUT vMessage).     /*medelade i klart text hur sändninge gick*/                                       
   END.
   ELSE DO:
      RUN SMTPMAIL.P  
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
      OUTPUT oSuccessful,   /*Om true är allt ok*/                                                                            
      OUTPUT vMessage).     /*medelade i klart text hur sändninge gick*/
   END.   
END PROCEDURE.
