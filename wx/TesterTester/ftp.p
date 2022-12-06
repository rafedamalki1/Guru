/*ftp.p*/
   DEFINE VARIABLE ftpnet AS Helpers.FtpNet.
   
   ftpnet = new Helpers.FtpNet().
   
   ftpnet:Skicka(CHR(69) + CHR(76) + CHR(80) + CHR(65) + CHR(79), "xxxxxxx","ftp://ftp.guruonweb.se/fortum/fors9.bck","C:\delad\pro9s\DBKOPIA\fors9.bck").