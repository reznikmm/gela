--  limited with Gela.Element_Sequences;
with Gela.Symbols;

package Gela.Elements.Defining_Names is
   pragma Preelaborate;

   type Defining_Name is limited interface and Gela.Elements.Element;
   --  Defining name element
   type Defining_Name_Access is access constant Defining_Name'Class;
   for Defining_Name_Access'Storage_Size use 0;

   not overriding function Symbol
     (Self : aliased Defining_Name)
      return Gela.Symbols.Symbol_Access is abstract;
   --  Return symbol of given defining name

   not overriding function Prefix
     (Self : aliased Defining_Name)
      return Gela.Elements.Element_Access is abstract;
   --  Return prefix of given defining_program_unit_name

   --  TODO: completion_chain?
   --  TODO: override?
end Gela.Elements.Defining_Names;
