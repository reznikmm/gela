package Gela.Declarations.Entry_Declarations is
   pragma Preelaborate;

   type Entry_Declaration is limited interface
     and Gela.Declarations.Declaration;
   --  Entry declarations from task and protected declarations
   type Entry_Declaration_Access is
     access constant Entry_Declaration'Class;
   for Entry_Declaration_Access'Storage_Size use 0;

   not overriding function Entry_Family_Definition
     (Self : aliased Entry_Declaration)
      return Gela.Elements.Element_Access is abstract;

end Gela.Declarations.Entry_Declarations;
