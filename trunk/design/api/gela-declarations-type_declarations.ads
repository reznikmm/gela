limited with Gela.Element_Sequences;

package Gela.Declarations.Type_Declarations is
   pragma Preelaborate;

   type Type_Declaration is limited interface
     and Gela.Declarations.Declaration;
   --  Type declaration element
   type Type_Declaration_Access is
     access constant Type_Declaration'Class;
   for Type_Declaration_Access'Storage_Size use 0;

   not overriding function Discriminants
     (Self : aliased Type_Declaration)
      return Gela.Element_Sequences.Element_Sequence_Access is abstract;
   --  How to return Unknown discriminant part? as Box_Expression?

   not overriding function Type_Definition
     (Self : aliased Type_Declaration)
      return Gela.Elements.Element_Access is abstract;

end Gela.Declarations.Type_Declarations;
