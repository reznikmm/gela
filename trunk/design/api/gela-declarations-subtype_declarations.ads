package Gela.Declarations.Subtype_Declarations is
   pragma Preelaborate;

   type Subtype_Declaration is limited interface
     and Gela.Declarations.Declaration;
   --  Subtype declaration element
   type Subtype_Declaration_Access is
     access constant Subtype_Declaration'Class;
   for Subtype_Declaration_Access'Storage_Size use 0;

   not overriding function Subtype_Indication
     (Self : aliased Subtype_Declaration)
      return Gela.Elements.Element_Access is abstract;

end Gela.Declarations.Subtype_Declarations;
