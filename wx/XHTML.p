<body>
<FORM>
<INPUT TYPE="Hidden" NAME="login" Value="` get-field("login") `">
<INPUT TYPE="Hidden" NAME="password" Value="` get-field("password") `">
</FORM>
<script language="SpeedScript">  
  /*------------------------------------------------------------------ 
    File: 
    Description: 
    Created:
  -------------------------------------------------------------------*/
   DEFINE VARIABLE htmanv AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE htmlos AS CHARACTER NO-UNDO.
   DEFINE VARIABLE inlogvar AS LOGICAL NO-UNDO. 
   RUN KOPPLA.P (INPUT htmanv,INPUT htmlos,OUTPUT inlogvar).
   {&OUT} inlogvar.
</script>
</body>
