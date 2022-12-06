 
 /*------------------------------------------------------------------------
    File        : FtpNet.cls 
    Purpose     : för att skicka/hämta ftp
    Syntax      : 
    Description : 
    Author(s)   : elprs
    Created     : Fri Aug 12 13:06:21 CEST 2016
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING System.Net.*.
USING System.IO.*.
USING System.Text.*.

CLASS Helpers.FtpNet: 
   
   DEFINE PRIVATE VARIABLE ftprequest AS System.Net.WebClient.
   
   CONSTRUCTOR PUBLIC FtpNet(INPUT r AS Guru.Root):
 
   END CONSTRUCTOR. 
   
   CONSTRUCTOR PUBLIC FtpNet():
      THIS-OBJECT:ftprequest = NEW WebClient().
      
   END CONSTRUCTOR.

   METHOD PUBLIC CHARACTER Skicka(ftpUsername AS CHARACTER, ftpPassword AS CHARACTER, targetFilePath AS CHARACTER, localFilePath AS CHARACTER):
      DEFINE VARIABLE results AS "System.Byte[]" NO-UNDO.
      DEFINE VARIABLE FtpError AS CHARACTER NO-UNDO.
      FtpError = "Skickad".
      THIS-OBJECT:ftprequest:Credentials = NEW NetworkCredential (ftpUsername, ftpPassword).
      results = THIS-OBJECT:ftprequest:UploadFile(targetFilePath, "STOR", localFilePath).
      CATCH FtpLoggInError AS System.Net.WebException:
         FtpError =  "FTP fel: " + FtpLoggInError:MESSAGE  .
      END CATCH.
      FINALLY:
         RETURN FtpError.
      END.   
   END METHOD.
 
       
   METHOD PUBLIC CHARACTER Hamta(ftpUsername AS CHARACTER, ftpPassword AS CHARACTER, targetFilePath AS CHARACTER, localFilePath AS CHARACTER):
      DEFINE VARIABLE FtpError AS CHARACTER NO-UNDO.
      DEFINE VARIABLE results AS "System.Byte[]" NO-UNDO.
      
      THIS-OBJECT:ftprequest:Credentials = NEW NetworkCredential (ftpUsername, ftpPassword).
      THIS-OBJECT:ftprequest:DownloadFile(targetFilePath, localFilePath).
      FtpError = "Hämtad".
      CATCH FtpLoggInError AS System.Net.WebException:
          FtpError =  "FTP fel: " + FtpLoggInError:MESSAGE  .
            
      END CATCH.
      FINALLY:
         RETURN FtpError.
      END.    
   END METHOD. 
   
   

   METHOD PUBLIC CHARACTER ListaTH(ftpUsername AS CHARACTER, ftpPassword AS CHARACTER, targetFilePath AS CHARACTER):
      DEFINE VARIABLE results AS "System.Byte[]" NO-UNDO.
      DEFINE VARIABLE FtpError AS CHARACTER NO-UNDO.
      DEFINE VARIABLE vres AS CHARACTER  NO-UNDO.
      FtpError = "Listad".
      THIS-OBJECT:ftprequest:Credentials = NEW NetworkCredential (ftpUsername, ftpPassword).         
      results = THIS-OBJECT:ftprequest:DownloadData(targetFilePath). /* Kan man få hem info om vad som finns där och sen plocka ut det man vill ha? */
      vres = Encoding:ASCII:GetString(results).   
      
      CATCH FtpLoggInError AS System.Net.WebException:
         FtpError =  "FTP fel: " + FtpLoggInError:MESSAGE  .
      END CATCH.
      FINALLY:
         RETURN FtpError.
      END.    
   END METHOD.          
         

END CLASS.

  
         /*
         
          UploadFile(String, String, String)
          public byte[] UploadFile (string address, string? method, string fileName);
          Parameters
            address
          method
            String 
            The method used to send the file to the resource. If null, the default is POST for http and STOR for ftp.
         fileName
            String 
         The file to send to the resource.
         */
      /*   
         MESSAGE Encoding:ASCII:GetString(results)
         VIEW-AS ALERT-BOX.
         
         ftprequest
         
         Dim remoteUri As String = "http://www.contoso.com/library/homepage/images/"
Dim fileName As String = "ms-banner.gif"
Dim myStringWebResource As String = Nothing
' Create a new WebClient instance.
Dim myWebClient As New WebClient()
' Concatenate the domain with the Web resource filename. Because DownloadFile 
'requires a fully qualified resource name, concatenate the domain with the Web resource file name.
myStringWebResource = remoteUri + fileName
Console.WriteLine("Downloading File ""{0}"" from ""{1}"" ......." + ControlChars.Cr + ControlChars.Cr, fileName, myStringWebResource)
' The DownloadFile() method downloads the Web resource and saves it into the current file-system folder.
myWebClient.DownloadFile(myStringWebResource, fileName)
Console.WriteLine("Successfully Downloaded file ""{0}"" from ""{1}""", fileName, myStringWebResource)
Console.WriteLine((ControlChars.Cr + "Downloaded file saved in the following file system folder:" + ControlChars.Cr 
        */ 
   
   

     /*The following code example demonstrates downloading a file from an FTP server by using the WebClient class.




public static bool DisplayFileFromServer(Uri serverUri)
{
    // The serverUri parameter should start with the ftp:// scheme.
    if (serverUri.Scheme != Uri.UriSchemeFtp)
    {
        return false;
    }
    // Get the object used to communicate with the server.
    WebClient request = new WebClient();

    // This example assumes the FTP site uses anonymous logon.
    request.Credentials = new NetworkCredential ("anonymous","janeDoe@contoso.com");
    try 
    {
        byte [] newFileData = request.DownloadData (serverUri.ToString());
        string fileString = System.Text.Encoding.UTF8.GetString(newFileData);
        Console.WriteLine(fileString);
    }
    catch (WebException e)
    {
        Console.WriteLine(e.ToString());
    }
    return true;
}

  
  */