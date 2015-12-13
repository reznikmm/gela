package Gela.Declarations.Entry_Body_Declarations is
   pragma Preelaborate;

   type Entry_Body_Declaration is limited interface
     and Gela.Declarations.Declaration;
   --  Entry body declarations
   type Entry_Body_Declaration_Access is
     access constant Entry_Body_Declaration'Class;
   for Entry_Body_Declaration_Access'Storage_Size use 0;

   not overriding function Entry_Index_Specification
     (Self : aliased Entry_Body_Declaration)
      return Gela.Elements.Element_Access is abstract;

   not overriding function Entry_Barrier
     (Self : aliased Entry_Body_Declaration)
      return Gela.Elements.Element_Access is abstract;

   not overriding function Body_Declarative_Items
     (Self : aliased Entry_Body_Declaration)
      return Gela.Element_Sequences.Element_Sequence_Access is abstract;

   not overriding function Body_Statements
     (Self : aliased Entry_Body_Declaration)
      return Gela.Element_Sequences.Element_Sequence_Access is abstract;

   not overriding function Body_Exception_Handlers
     (Self : aliased Entry_Body_Declaration)
      return Gela.Element_Sequences.Element_Sequence_Access is abstract;

end Gela.Declarations.Entry_Body_Declarations;
