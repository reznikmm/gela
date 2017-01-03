with Gela.Elements;

limited with Gela.Element_Sequences;
limited with Gela.Elements.Defining_Names;

package Gela.Declarations is
   pragma Preelaborate;

   type Declaration is limited interface and Gela.Elements.Element;
   --  Declaration element
   type Declaration_Access is access constant Declaration'Class;
   for Declaration_Access'Storage_Size use 0;

   not overriding function Names
     (Self : aliased Declaration)
      return Gela.Element_Sequences.Defining_Name_Sequence_Access
        is abstract;
   --  Returns a list of names defined by the declaration, in their order of
   --  appearance.  Declarations that define a single name will return a list
   --  of length one.

   function Name
     (Self : aliased Declaration'Class)
      return Gela.Elements.Defining_Names.Defining_Name_Access;
   --  Shortcut for getting first name from Names

   not overriding function Aspects
     (Self : aliased Declaration)
      return Gela.Element_Sequences.Element_Sequence_Access is abstract;
   --  Return aspects for given declaration if any

end Gela.Declarations;