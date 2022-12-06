
/* A procedure to illustrate the use of preprocessor names.              */
/* This procedure includes p-editrc.i, an include file that uses          */
/* preprocessor names to allow you to easily access and modify any       */
/* file in the demo database.  Before you compile this procedure,        */
/* you must set the following preprocessor definitions:                  */
/*                                                                       */                                                 
/* NOTE: This file does NOT set a value to the form_file                 */
/*       preprocessor name.                                              */ 
/*                                                                         
*/
/* PREPROCESSOR             DESCRIPTION                                  */
/*     NAME                                                              */
/*                                                                       */
/* file_name   : The database file to modify.                            */
/*               This name must have a value.                            */
/* file_fields : The list of fields in the database file to modify.      */
/*               This name must have a value.                            */
/* form_file   : The name of an external form definition include (.f)    */
/*               file. This name is optional.                            */
/* frame_name  : The name of the frame to be used in accessing the       */
/*               database file.                                          */
/* frame_title : The title to be added to the frame upon display.  If a  */
/*               form include file is used, TITLE should not be part of  */
/*               the form statement.                                     */
/* use-buts    : A comma separated list of actions you may perform       */
/*               against the database in the browse utility.  Available  */
/*               actions are Next, Prev, Update, Create, and Delete.     */
/* banner      : An example of deferred evaluation, this name references */
/*               the ifile_ver preprocessor definition, which is set     */
/*               in the include file p-editrc.i.                         */
&GLOBAL-DEFINE file_name customer
&GLOBAL-DEFINE file_fields customer.cust-num customer.name~
               customer.address customer.address2~
               customer.city  customer.st customer.postal-code~
               customer.contact customer.phone 
&GLOBAL-DEFINE form_file p-cust.f
&GLOBAL-DEFINE frame_name cust_fr
&GLOBAL-DEFINE frame_title Customer
&GLOBAL-DEFINE Banner File Modification Utility, Version  ~{&ifile_ver}
&GLOBAL-DEFINE use-buts Next,Prev,Update,Create,Delete
{p-editrc.i}


