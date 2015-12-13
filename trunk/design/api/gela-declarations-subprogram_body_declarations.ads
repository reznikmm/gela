with Gela.Declarations.Callables;

package Gela.Declarations.Subprogram_Body_Declarations is
   pragma Preelaborate;

   type Subprogram_Body_Declaration is limited interface
     and Gela.Declarations.Callables.Callable;
   --  Function and procedure body declarations
   type Subprogram_Body_Declaration_Access is
     access constant Subprogram_Body_Declaration'Class;
   for Subprogram_Body_Declaration_Access'Storage_Size use 0;

   not overriding function Is_Function
     (Self : aliased Subprogram_Body_Declaration) return Boolean is abstract;

   not overriding function Is_Procedure
     (Self : aliased Subprogram_Body_Declaration) return Boolean is abstract;

   not overriding function Body_Declarative_Items
     (Self : aliased Subprogram_Body_Declaration)
      return Gela.Element_Sequences.Element_Sequence_Access is abstract;

   not overriding function Body_Statements
     (Self : aliased Subprogram_Body_Declaration)
      return Gela.Element_Sequences.Element_Sequence_Access is abstract;

   not overriding function Body_Exception_Handlers
     (Self : aliased Subprogram_Body_Declaration)
      return Gela.Element_Sequences.Element_Sequence_Access is abstract;

end Gela.Declarations.Subprogram_Body_Declarations;
