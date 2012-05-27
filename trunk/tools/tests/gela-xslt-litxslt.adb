with Interfaces.C;
with Gela.Conv;
--  with League.Strings.Internals;
--  with Matreshka.Internals.Utf16;

package body Gela.XSLT is
   use Interfaces;

   type xmlDocPtr is access all Integer;
   type xsltStylesheetPtr is access all Integer;
   type Buffer_Access is access all C.char_array (1 .. C.size_t'Last);
   type Buffer_Access_Access is access all Buffer_Access;
   type Int_Ptr is access all C.int;

   pragma Linker_Options ("-lxml2");
   pragma Linker_Options ("-lxslt");

   --  function xmlReadMemory
   --    (Buffer   : Matreshka.Internals.Utf16.Utf16_String;
   --     Size     : Interfaces.C.int;
   --     Base     : Interfaces.C.char_array;
   --     Encoding : Interfaces.C.char_array;
   --     Options  : Interfaces.C.int)
   --    return xmlDocPtr;
   --  pragma Import (C, xmlReadMemory, "xmlReadMemory");

   function xmlParseFile (XML : C.char_array) return xmlDocPtr;
   pragma Import (C, xmlParseFile, "xmlParseFile");

   --  function xsltParseStylesheetDoc
   --    (XML : Interfaces.C.char_array)
   --    return xsltStylesheetPtr;
   --  pragma Import (C, xsltParseStylesheetDoc, "xsltParseStylesheetDoc");

   function xsltParseStylesheetFile
     (File : C.char_array)
     return xsltStylesheetPtr;
   pragma Import (C, xsltParseStylesheetFile, "xsltParseStylesheetFile");

   function xsltApplyStylesheet
     (Style  : xsltStylesheetPtr;
      XML    : xmlDocPtr;
      Params : xmlDocPtr)
     return xmlDocPtr;
   pragma Import (C, xsltApplyStylesheet, "xsltApplyStylesheet");

   function xsltSaveResultToString
     (Buffer : Buffer_Access_Access;
      Size   : Int_Ptr;
      XML    : xmlDocPtr;
      Style  : xsltStylesheetPtr)
     return C.int;
   pragma Import (C, xsltSaveResultToString, "xsltSaveResultToString");

   procedure xmlFreeDoc (Doc : xmlDocPtr);
   pragma Import (C, xmlFreeDoc, "xmlFreeDoc");

   procedure xsltFreeStylesheet (Doc : xsltStylesheetPtr);
   pragma Import (C, xsltFreeStylesheet, "xsltFreeStylesheet");

   procedure free (Doc : Buffer_Access);
   pragma Import (C, free, "free");

   function Transform
     (XML : League.Strings.Universal_String;
      XSL : League.Strings.Universal_String)
     return League.Strings.Universal_String
   is
      use type C.int;

      XML_Name : constant C.char_array := C.To_C (Gela.Conv.To_String (XML));
      XSL_Name : constant C.char_array := C.To_C (Gela.Conv.To_String (XSL));
      XML_Doc : xmlDocPtr;
      Result  : xmlDocPtr;
      Style   : xsltStylesheetPtr;
      Buffer  : aliased Buffer_Access;
      Size    : aliased C.int;
      X       : C.int;
   begin
      XML_Doc := xmlParseFile (XML_Name);

      Style := xsltParseStylesheetFile (XSL_Name);

      Result := xsltApplyStylesheet (Style, XML_Doc, null);

      X := xsltSaveResultToString
        (Buffer => Buffer'Unchecked_Access,
         Size   => Size'Unchecked_Access,
         XML    => Result,
         Style  => Style);

      pragma Assert (X = 0);

      xmlFreeDoc (XML_Doc);
      xsltFreeStylesheet (Style);

      return Result : constant League.Strings.Universal_String
        := Gela.Conv.To_Universal_String
        (C.To_Ada (Buffer (1 .. C.size_t (Size)), False))
      do
         free (Buffer);
      end return;
   end Transform;

end Gela.XSLT;
