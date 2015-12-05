limited with Gela.Element_Sequences;

package Gela.Declarations.Package_Declarations is
   pragma Preelaborate;

   type Package_Declaration is limited interface
     and Gela.Declarations.Declaration;
   --  Package declaration element
   type Package_Declaration_Access is
     access constant Package_Declaration'Class;
   for Package_Declaration_Access'Storage_Size use 0;

   not overriding function Visible_Part
     (Self : aliased Package_Declaration)
      return Gela.Element_Sequences.Defining_Name_Sequence_Access
        is abstract;

   not overriding function Private_Part
     (Self : aliased Package_Declaration)
      return Gela.Element_Sequences.Defining_Name_Sequence_Access
        is abstract;

   function Assigned (Self : access Package_Declaration'Class) return Boolean
     is (Self /= null);

end Gela.Declarations.Package_Declarations;
