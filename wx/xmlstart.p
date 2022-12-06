
/*
/*
OpenEdge Development: Programming Interfaces 


--------------------------------------------------------------------------------
  
XML Support 
Extensible Markup Language (XML) is a data format for structured document interchange over networks. This chapter describes language extensions to the Progress 4GL that enable OpenEdge applications to use XML through the integration of a third-party DOM parser, as described in the following sections: 

Introduction
The Document Object Model
The New 4GL Objects and Handles
Creating XML output from the 4GL
Reading XML input into the 4GL
Internationalization
Error handling
Note: For complete information on using the Simple API for XML (SAX) with the Progress 4GL, see "Simple API for XML (SAX)," in this manual. 
Introduction 
The Extensible Markup Language (XML) is a data format for structured document interchange on the Web and other networks. It is hardware architecture neutral and application independent. XML documents are composed of storage units called entities, that contain either parsed or unparsed data. Parsed data is made up of characters, some of which form character data, and some of which form markup. Markup encodes a description of a document’s storage layout and logical structure. 

A software module called an XML processor is used to read XML documents and provide access to their content and structure. It is assumed that an XML processor is doing its work on behalf of another module, called the application. 

XML documents 
XML documents are made up of two parts: 

The prolog contains optional information such as the XML version the document conforms to, information about the character encoding used to encode the contents of the document, and a document type definition (DTD) which describes the grammar and vocabulary of the document.
The body may contain elements, entity references, and other markup information.
DTDs are rules that define the elements that can exist in a particular document or group of documents, and the relationships among the various elements. A DTD can be part of the content of an XML document or can be separate from it and referred to by the documents. Here is an example of a DTD: 
*/

 <? xml version="1.0" encoding="UTF-8" ?> 
<!DOCTYPE customer 
[ 
<!ELEMENT customer(name, custnum)> 
<!ELEMENT name(#PCDATA)> 
<!ELEMENT custnum(#PCDATA)> 
]> 
<customer> 
XMLSTART.PLift Line Skiing</name><custnum>1</custnum> 
</customer>  


Elements represent the logical components of documents. They can contain data or other elements. For example, a customer element can contain a number of column (field) elements and each column element can contain one data value. Here is an example of an element: 


 XMLSTART.PClyde</name>  


Elements can have additional information called attributes attached to them. Attributes describe properties of elements. Here is an example of an element with an attribute, emp-num: 


 <name emp-num="1">Mary</name>  


Here is an example of elements that contain other elements: 


 <phone><entry>XMLSTART.PChip</name><extension>3</extension></entry></phone>  


The Document Object Model 
The Document Object Model (DOM) is an application programming interface (API) for XML documents. It defines the logical structure of documents and the way a document is accessed and manipulated. In the DOM specification, the term “document” is used in the broad sense to include many different kinds of information that might be stored in diverse systems. Much of this would traditionally be seen as data rather than as documents. Nevertheless, XML presents this data as documents, and the DOM manages this data. 

When you read an XML document via the DOM API, the DOM parser reads and parses the complete input document before making it available to the application. 

Progress has defined an initial set of extensions to the Progress 4GL to allow the use of XML through the DOM interface. These extensions provide 4GL applications with the basic input, output, and low-level data manipulation capabilities required to use data contained in XML documents. They are not intended to provide access to the entire DOM interface, nor are they intended to include all the high-level constructs. 

Note on DOM compatibility with the 4GL 
The DOM API is designed to be compatible with a wide range of programming languages, but the naming convention chosen by the World Wide Web Consortium (W3C) does not match what already exists in the Progress 4GL. In some cases, PSC elected to use the familiar names already used in the 4GL rather than the names given in the DOM specification. Similarly, where there are existing 4GL features that provide the same capability as the DOM interfaces, PSC has chosen to use the 4GL implementation rather than introduce new language features that match the DOM more closely. 

The DOM structure model 
The DOM presents documents as a hierarchy or tree of node objects that also implement other, more specialized interfaces. Some types of nodes may have child nodes of various types, and others are leaf nodes that cannot have anything below them in the document structure. The node types, and which node types they may have as children, are shown in Table 17–1. 


Table 17–1: Node interface types  Node interfaces  Description  Children  
DocumentType  Represents the Document Type Definition or Schema declaration of the XML document.  Notation, Entity  
DocumentFragment  Represents a lightweight object used to store sections of an XML document temporarily.  Element, ProcessingInstruction, Comment, Text, CDATASection, EntityReference  
EntityReference  Represents a reference to an entity within the XML document.  Element, ProcessingInstruction, Comment, Text, CDATASection, EntityReference  
Element  Represents an element node. This interface represents the data, or the tags of the XML document. The text of the element is stored in a Text or CDATASection node, which is the child of the element.  Element, Text, Comment, ProcessingInstruction, CDATASection, EntityReference  
Attribute  Represents an attribute of a document or an element. The allowable values for the attribute are defined in a document type definition. Attributes are NOT considered as child nodes of the element they describe.  Text, EntityReference  
CDATASection  CDATA sections are used to escape blocks of text that would otherwise be regarded as markup. The primary purpose is for including XML fragments, without needing to escape all the delimiters.  None  
Comment  Represents the content of a comment.  None  
Entity  Represents an entity, either parsed or unparsed, in the XML document.  None  
Notation  Represents a notation declared within the DTD.  None  
ProcessingInstruction  The “Processing Instruction” is a way to keep processor-specific information in the text of the document.  None  
Text  Represents a Text node that is a child of an element node.  None  


The New 4GL Objects and Handles 
This section describes some more Progress 4GL objects and handles. 

DOM node interfaces as subtypes 
Since it is necessary to be able to use some of the other specialized interfaces as well as the simplified interface, the DOM objects that have these specialized interfaces are implemented as Subtypes of the Progress object. 

The Progress 4GL supports the following interfaces as Subtypes on the X-NODEREF: 

CDATA-SECTION
COMMENT
DOCUMENT-FRAGMENT
ELEMENT
ENTITY-REFERENCE
PROCESSING-INSTRUCTION
TEXT
The default Subtype will be ELEMENT. 

Table 17–2 shows the “simplified” DOM Node interface nodeNames and nodeValues. 


Table 17–2: Node names and values   Type  nodeName  nodeValue  
DocumentType  Document type name  Null  
DocumentFragment  #document-fragment  Null  
Element  Tag name  Null  
EntityReference  Name of entity referenced  Null  
Text  #text  Content of the text node  
CDATASection  #cdata-section  Content of the CDATA-section  
Comment  #comment  Content of the comment  
ProcessingInstruction  Target  Content excluding the target  


The Progress 4GL NAME attribute will be used to return the nodeName, while a new NODE-VALUE character attribute will return or set the node’s nodeValue. 

Document and node reference objects 
The X-DOCUMENT is a Progress 4GL object that represents a DOM Document object. The X-DOCUMENT object is assigned to the HANDLE data type and is used to manipulate the XML document and its tree representation. 

The X-NODEREF object is a Progress 4GL object that is a reference to any node in an XML tree except a Document node. The X-NODEREF object is assigned to the HANDLE data type and is used to manipulate the DOM nodes. Note that an X-NODEREF object is not an actual node in the XML tree but is more like a cursor which is used to navigate the tree and manipulate the nodes in it. 

Creating a document object 
The creation and saving of a document is not part of the DOM Core API, but is left to the application that calls the API. You create an XML document using the X-DOCUMENT option of the CREATE Widget statement: 


 DEFINE VARIABLE hXDoc AS HANDLE.
CREATE X-DOCUMENT hXDoc.  


This statement creates a handle for an object of the type X-DOCUMENT that “wraps” an XML document. You may start adding nodes to it or use the LOAD( ) method to populate it from an existing XML document. 

Creating a node reference object 
You add a node reference object to the XML document using the X-NODEREF option of the CREATE Widget statement: 


 DEFINE VARIABLE hNRef AS HANDLE.
CREATE X-NODEREF hNRef.  


This statement creates a handle for an object which is not an actual node, but which is used as a reference or pointer to an actual node in the tree. The X-NODEREF object provides a path to access and manipulate the actual document nodes and can be used as a parameter or as a return-value for methods that will associate the handle with an XML node. 

Creating XML output from the 4GL 
This section describes how to create XML output from the Progress 4GL. 

To create an output XML document: 
Create an X-DOCUMENT object.
Create a Root Node reference object.
Create a Node reference object for each type of node.
Create the root node and append it to the document.
Create each specific node required.
Append each node to its parent.
Set node attributes.
Steps 5 through 7 are iterative.
Save the document as an XML file.
Delete the objects.
The root node reference object 
The root node is the unique top-level node that is not a child of any other node. All other nodes are children or other descendents of the root node. A root node is necessary so that you have a top-level node to which you can append the child nodes. 

Creating and appending a node 
In order to create an actual XML node, you use the CREATE-NODE( ) method on the parent object. After the node is created, you must append it to its parent by using the 
APPEND-CHILD( ) method. The following code fragment is an example of creating and appending the root node: 


 CREATE X-NODEREF hRoot.
hDoc:CREATE-NODE(hRoot,"Root","ELEMENT").
hDoc:APPEND-CHILD(hRoot).
. . .  


Setting node attributes and values 
You can set the attributes of a node or the value of a node either before or after it is appended by using the SET-ATTRIBUTE( ) method or the NODE-VALUE attribute. The following code fragment depicts setting attributes of the “employee” ELEMENT node with the SET-ATTRIBUTE( ) method and setting the value of the “address” TEXT node with the NODE-VALUE attribute. Note that in this case, the “employee” node is a child of the root node and the “address” node is a child of the “employee” node: 


 hDoc:CREATE-NODE(hEmp,"employee","ELEMENT").
hDoc:CREATE-NODE(hAddr,?,"TEXT").

hEmp:SET-ATTRIBUTE("empID","10263").
hEmp:SET-ATTRIBUTE("empDept","Sales").
hRoot:APPEND-CHILD(hEmp).

hEmp:APPEND-CHILD(hAddr).
hAddr:NODE-VALUE = "121 State Street".
. . .  


For more information on attributes and methods associated with the X-DOCUMENT object and the X-NODEREF object, see their entries in the OpenEdge Development: Progress 4GL Reference. 

Example of creating an output XML file 
The following sample program creates an XML file consisting of all fields in all the customer records where the custnum is less than “5". You must use the SAVE( ) method on the X-DOCUMENT object in order to create the actual XML file: 


i-outcus.p  /* i-outcus.p - Export the Customer table to an xml file*/
   */
DEFINE VARIABLE hDoc AS HANDLE.
DEFINE VARIABLE hRoot AS HANDLE.
DEFINE VARIABLE hRow AS HANDLE.
DEFINE VARIABLE hField AS HANDLE.
DEFINE VARIABLE hText AS HANDLE.
DEFINE VARIABLE hBuf AS HANDLE.
DEFINE VARIABLE hDBFld AS HANDLE.
DEFINE VARIABLE i AS INTEGER.

CREATE X-DOCUMENT hDoc.
CREATE X-NODEREF hRoot.
CREATE X-NODEREF hRow.
CREATE X-NODEREF hField.
CREATE X-NODEREF hText.

hBuf = BUFFER customer:HANDLE.

/*set up a root node*/
hDoc:CREATE-NODE(hRoot,"Customers","ELEMENT").
hDoc:APPEND-CHILD(hRoot).
FOR EACH customer WHERE custnum < 5:
hDoc:CREATE-NODE(hRow,"Customer","ELEMENT"). /*create a row node*/
hRoot:APPEND-CHILD(hRow). /*put the row in the tree*/
hRow:SET-ATTRIBUTE("custnum",STRING(custnum)).
hRow:SET-ATTRIBUTE("Name",NAME).
/*Add the other fields as tags in the xml*/
REPEAT i = 1 TO hBuf:NUM-FIELDS:
hDBFld = hBuf:BUFFER-FIELD(i).
IF hDBFld:NAME = "custnum" OR hDBFld:NAME = "NAME" THEN NEXT.
/*create a tag with the field name*/
hDoc:CREATE-NODE(hField, hDBFld:NAME, "ELEMENT").
/*put the new field as next child of row*/
hRow:APPEND-CHILD(hField).
/*add a node to hold field value*/
hDoc:CREATE-NODE(hText, "", "TEXT").  
/*attach the text to the field*/
hField:APPEND-CHILD(hText).
hText:NODE-VALUE = STRING(hDBFld:BUFFER-VALUE).
END.
END.
/*write the XML node tree to an xml file*/
hDoc:SAVE("file","cust.xml").

DELETE OBJECT hDoc.
DELETE OBJECT hRoot.
DELETE OBJECT hRow.
DELETE OBJECT hField.
DELETE OBJECT hText.  

/*
A partial output of the above program appears below. Note that the carriage returns and indentations have been entered for readability; the actual file contains one long string: 
*/

 <?xml version=’1.0’ ?>
<Customers>
<Customer Name="Lift Line Skiing" custnum="1">
<Country>USA</Country>
<Address>276 North Street</Address>
<Address2></Address2>
<City>Boston</City>
<State>MA</State>
<Postal-Code>02114</Postal-Code>
<Contact>Gloria Shepley</Contact>
<Phone>(617) 450-0087</Phone>
<Sales-Rep>HXM</Sales-Rep>
<Credit-Limit>66700</Credit-Limit>
<Balance>42568</Balance>
<Terms>Net30</Terms>
<Discount>35</Discount>
<Comments>This customer is on credit hold.</Comments>
</Customer>
<Customer Name="Urpon Frisbee" custnum="2">
<Country>Finland</Country>
<Address>Rattipolku 3</Address>
. . .

</Customer>
</Customers>  


Writing an XML file to a MEMPTR or a stream 
You can also write an XML file to a MEMPTR or to an output stream as the following code fragments demonstrate. The following fragment shows saving an XML file to a MEMPTR: 


 DEFINE VARIABLE memfile AS MEMPTR.
   . 
   . 
   .
hDoc:SAVE("memptr",memfile). /* SAVE() will set the memptr size */
   . 
   . 
   .  


The following fragment displays saving an XML file to an output stream: 


 DEFINE STREAM xmlstream.
   . 
   . 
   .
OUTPUT STREAM xmlstream TO custxml.xml.
hDoc:SAVE("stream","xmlstream").
OUTPUT CLOSE.
   . 
   . 
   .  


Reading XML input into the 4GL 
This section describes how to read XML input into the Progress 4GL. 

To read in an XML file and process it: 
Create an X-DOCUMENT object.
Create a root node reference object.
Use the LOAD( ) method to read the input file.
Use the GET-DOCUMENT-ELEMENT( ) method to get the root node reference handle.
Create a node reference object.
Using the GET-CHILD( ) method, read through the child nodes.
Using the GET-ATTRIBUTE( ) METHOD, the NAME attribute, the NODE-VALUE attribute and other attributes and methods, access the XML data.
Update the database or other fields as necessary.
Delete the objects.
Loading an XML file 
The LOAD( ) method reads the specified file into memory, parses it, optionally validates it, and makes its contents available to the 4GL. Once the XML file is in memory, you must get the handle to its root element by using the GET-DOCUMENT-ELEMENT( ) method. Once you have the root node handle, you can manipulate the remaining child nodes. 



/* The following code fragment demonstrates loading an XML file called “myfile.xml”:  */ 
 DEFINE VARIABLE hDocMine AS HANDLE.
DEFINE VARIABLE hRootMine AS HANDLE.

CREATE X-DOCUMENT hDocMine.
CREATE X-NODEREF hRootMine.

hDocMine:LOAD("FILE","myfile.xml",TRUE).
hDocMine:GET-DOCUMENT-ELEMENT(hRootMine).
   . 
   . 
   .  


/*Loading an XML file from a MEMPTR 
An XML file can be read from a MEMPTR as the following code fragment demonstrates: 
*/
 
   DEFINE VARIABLE memfile AS MEMPTR.
   . 
   . 
   .
FILE-INFO:FILE-NAME = "meminp.xml".
SET-SIZE(memfile) = FILE-INFO:FILE-SIZE.
INPUT FROM "meminp.xml" BINARY NO-CONVERT. 
IMPORT memfile. 
INPUT CLOSE.
hDoc:LOAD("memptr",memfile,FALSE).
   . 
   . 
   .  

/*
Accessing the child nodes 
Before you can work with a child node of an XML document, you must create a node reference object WITH
which to access the node. Then you access the node by using the GET-CHILD( ) method. 
The following code fragment shows obtaining the third child node of the parent node, hParent: 
*/

    . 
   . 
   .
DEFINE VARIABLE hChildNode AS HANDLE.
CREATE X-NODEREF hChildNode.
logvar = hParent:GET-CHILD(hChildNode,3).
   . 
   . 
   .  

/*
Using node attributes and values 
You can get information about the child node by using various attributes and methods.
For example, if you do not know how many nodes there are below the node referred to by the node reference, 
you can use the NUM-CHILDREN attribute. You can obtain or set the value of the node by using the NODE-VALUE attribute: 
*/

DEFINE VARIABLE hChNode AS HANDLE.
CREATE X-NODEREF hChNode.
REPEAT i = 1 TO hParent:NUM-CHILDREN:
logvar = hParent:GET-CHILD(hChNode, i).
IF hChNode:NODE-VALUE > 0 THEN
hChNode:NODE-VALUE = hChNode:NODE-VALUE + i.
. . .  

/*
You can obtain a list of an element’s attribute names using the ATTRIBUTE-NAMES attribute,
get the value of an attribute by using the GET-ATTRIBUTE( ) method or set the value of an 
attribute by using the SET-ATTRIBUTE( ) method. You can also REMOVE-ATTRIBUTE( ): 
*/
 . . .
REPEAT i = 1 TO hNode1:NUM-CHILDREN:
logvar = hNode1:GET-CHILD(hChNode, i).
IF NOT logvar THEN LEAVE.
entries = hNode1:ATTRIBUTE-NAMES.
REPEAT j = 1 TO NUM-ENTRIES(entries):
aname = ENTRY(j, entries).
MESSAGE "attrname is " aname "value is " hNode1:GET-ATTRIBUTE(aname).
END.
END.
. . .  


In addition to creating nodes, you can IMPORT-NODE( ), CLONE-NODE( ), and DELETE-NODE( ). In addition to appending and getting a child, you can REMOVE-CHILD( ), REPLACE-CHILD( ), and GET-PARENT( ). The following example demonstrates the CLONE-NODE( ) method: 


i-clone.p  /* i-clone.p */
DEFINE VARIABLE hXref AS HANDLE.
DEFINE VARIABLE hXref1 AS HANDLE.
DEFINE VARIABLE hText AS HANDLE.
DEFINE VARIABLE hText1 AS HANDLE.
DEFINE VARIABLE hClone AS HANDLE.
DEFINE VARIABLE hRoot AS HANDLE.
DEFINE VARIABLE hDoc AS HANDLE.

CREATE X-NODEREF hXref.
CREATE X-NODEREF hXref1.
CREATE X-NODEREF hText.
CREATE X-NODEREF hText1.
CREATE X-NODEREF hClone.
CREATE X-NODEREF hRoot.
CREATE X-DOCUMENT hDoc.

hDoc:CREATE-NODE(hRoot,"root","ELEMENT").
hDoc:INSERT-BEFORE(hRoot,?).
hDoc:CREATE-NODE(hXref,"customer","ELEMENT").
hDoc:CREATE-NODE(hXref1,"order","ELEMENT").
hDoc:CREATE-NODE(hText,?,"TEXT").
hDoc:CREATE-NODE(hText1,?,"TEXT").  
/* Add the two element nodes to the root, each with a text*/
hXref:SET-ATTRIBUTE("id","54").
hXref:SET-ATTRIBUTE("name","Second Skin Scuba").
hRoot:APPEND-CHILD(hXref).
hXref:APPEND-CHILD(hText).

hXref1:SET-ATTRIBUTE("id","55").
hXref1:SET-ATTRIBUTE("name","Off the Wall").
hRoot:APPEND-CHILD(hXref1).
hXref1:APPEND-CHILD(hText1).
hText:NODE-VALUE = "hi from customer".
hText1:NODE-VALUE = "hi from order".

hXref1:CLONE-NODE(hClone,TRUE).
hRoot:APPEND-CHILD(hClone). 
/* Save the file */
hDoc:SAVE("file","clone1.xml").
DELETE OBJECT hXref.
DELETE OBJECT hXref1.
DELETE OBJECT hText.
DELETE OBJECT hText1.
DELETE OBJECT hClone.
DELETE OBJECT hRoot.
DELETE OBJECT hDoc.  

/* Ett exempel till
 
There are more methods and attributes that apply to the X-DOCUMENT object and the X-NODEREF objects.
For more information on these attributes and methods, see their entries in the OpenEdge Development: 
Progress 4GL Reference. 

Examples of reading an input XML file 
The following sample program shows reading in a file called “personal.xml”, processing through all 
the child nodes and displaying information if the node name is “person”: 

 */
i-attnam.p   /* i-attnam.p */
DEFINE VARIABLE hDoc AS HANDLE.
DEFINE VARIABLE hRoot AS HANDLE.
DEFINE VARIABLE good AS LOGICAL.

CREATE X-DOCUMENT hDoc.
CREATE X-NODEREF hRoot.

hDoc:LOAD("file","personal.xml",TRUE).
hDoc:GET-DOCUMENT-ELEMENT(hRoot).

RUN GetChildren(hRoot, 1).
DELETE OBJECT hDoc.
DELETE OBJECT hRoot.

PROCEDURE GetChildren:
DEFINE INPUT PARAMETER hParent AS HANDLE.
DEFINE INPUT PARAMETER level AS INTEGER.
DEFINE VARIABLE i AS INTEGER.
DEFINE VARIABLE hNoderef AS HANDLE.

CREATE X-NODEREF hNoderef.

REPEAT i = 1 TO hParent:NUM-CHILDREN:
good = hParent:GET-CHILD(hNoderef,i).
IF NOT good THEN LEAVE.
IF hNoderef:SUBTYPE <> "element" THEN NEXT.
IF hNoderef:NAME = "person" THEN
MESSAGE "getattr id gives" hNoderef:GET-ATTRIBUTE("id")
hNoderef:ATTRIBUTE-NAMES.
RUN GetChildren(hNoderef, (level + 1)).
END.

DELETE OBJECT hNoderef.
END PROCEDURE.  

/*
The following program reads in the output file created by the previous program, i-outcus.p and
creates temp-table entries: 

*/

i-incus.p  /* i-incus.p - Import the Customer table from an xml file*/
DEFINE VARIABLE hDoc AS HANDLE.
DEFINE VARIABLE hRoot AS HANDLE.
DEFINE VARIABLE hTable AS HANDLE.
DEFINE VARIABLE hField AS HANDLE.
DEFINE VARIABLE hText AS HANDLE.
DEFINE VARIABLE hBuf AS HANDLE.
DEFINE VARIABLE hDBFld AS HANDLE.
DEFINE VARIABLE i AS INTEGER.
DEFINE VARIABLE j AS INTEGER.

/*so we can create new recs*/
DEFINE TEMP-TABLE Custt LIKE Customer.

CREATE X-DOCUMENT hDoc.
CREATE X-NODEREF hRoot.
CREATE X-NODEREF hTable.
CREATE X-NODEREF hField.
CREATE X-NODEREF hText.
hBuf = BUFFER Custt:HANDLE.

/*read in the file created in the last example*/
hDoc:LOAD("file", "cust.xml", FALSE).
hDoc:GET-DOCUMENT-ELEMENT(hRoot).

/*read each Customer from the root*/
REPEAT i = 1 TO hRoot:NUM-CHILDREN:
hRoot:GET-CHILD(hTable,i).
CREATE Custt.
/*get the fields given as attributes*/
custnum = integer(hTable:GET-ATTRIBUTE("custnum")).
NAME = hTable:GET-ATTRIBUTE("Name").
/*get the remaining fields given as elements with text*/
REPEAT j = 1 TO hTable:NUM-CHILDREN:
hTable:GET-CHILD(hField,j).
IF hField:NUM-CHILDREN < 1 THEN NEXT.
/*skip any null value*/
hDBFld = hBuf:BUFFER-FIELD(hField:NAME).
hField:GET-CHILD(hText,1).
/*get the text value of the field*/
hDBFld:BUFFER-VALUE = hTEXT:NODE-VALUE.
END.
END.  
DELETE OBJECT hDoc.
DELETE OBJECT hRoot.
DELETE OBJECT hTable.
DELETE OBJECT hField.
DELETE OBJECT hText.

/* show data made it by displaying temp-table */
FOR EACH Custt:
DISPLAY custt.
END.  


Internationalization 
XML documents may be encoded using any of a wide a variety of character encoding. The DOM parser returns character data to the Progress 4GL interpreter encoded, if possible, according to -cpinternal, the Internal Code Page parameter. This translation is performed by the DOM parser using its own translation functions. If the DOM parser cannot do the translation according to -cpinternal, it translates to UTF8 which is then translated by the interpreter from UNICODE to the character encoding specified by -cpinternal. The encoding used in an XML document is specified by an optional encoding declaration at its very beginning. If the encoding declaration is present, it specifies the encoding used in the remainder of the document. If the declaration is not present, the document’s encoding is assumed to be UTF-8 or UTF-16. 

When the LOAD method is used to load an XML document, the ENCODING attribute of the X-DOCUMENT will be set to the name of encoding found in the encoding declaration of the document. For output, you can set the X-DOCUMENT’s ENCODING attribute to the name of the desired encoding. 

When the SAVE method is used to write an output XML document from a memory-resident DOM tree, the generated XML text is encoded by the DOM parser according to the value of the ENCODING attribute. When you SAVE a document to a stream, the specified encoding is used and the value of -cpstream is ignored. 

According to the XML recommendation, “it is a fatal error when an XML processor encounters an entity with an encoding that it is unable to process”. If this error occurs while Progress is attempting to load a document, the document will be empty. 

Error handling 
Any of the methods listed above may fail, but this will not normally cause the Progress error status to be raised. Instead, the method will return FALSE if that is appropriate. Also, the parsing may encounter errors that do not cause the operation as a whole to fail. So instead of testing for ERROR-STATUS:ERROR after running a method with NO-ERROR, you should test for ERROR-STATUS:NUM-MESSAGES being greater than 0. 

Note that the DOM parser may detect errors in an input XML document even if validation is not specified in the LOAD( ) method call. Validation checks the document for conformance to a DTD, but there could be other errors, such a missing end-tag or mismatched tags. The parser will report these errors independently of validation against a DTD. 



--------------------------------------------------------------------------------
Copyright © 2005 Progress Software Corporation
www.progress.com
Voice: (781) 280-4000
Fax: (781) 280-4095
 
  

