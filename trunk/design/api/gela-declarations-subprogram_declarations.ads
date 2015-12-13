with Gela.Declarations.Callables;

package Gela.Declarations.Subprogram_Declarations is
   pragma Preelaborate;

   type Subprogram_Declaration is limited interface
     and Gela.Declarations.Callables.Callable;
   --  Function and procedure declarations
   type Subprogram_Declaration_Access is
     access constant Subprogram_Declaration'Class;
   for Subprogram_Declaration_Access'Storage_Size use 0;

   not overriding function Is_Function
     (Self : aliased Subprogram_Declaration) return Boolean is abstract;

   not overriding function Is_Procedure
     (Self : aliased Subprogram_Declaration) return Boolean is abstract;

   not overriding function Is_Abstract
     (Self : aliased Subprogram_Declaration)
      return Boolean is abstract;

   not overriding function Is_Null_Procedure
     (Self : aliased Subprogram_Declaration) return Boolean is abstract;

   not overriding function Is_Expression_Function
     (Self : aliased Subprogram_Declaration) return Boolean is abstract;

   not overriding function Expression
     (Self : aliased Subprogram_Declaration)
      return Gela.Elements.Element_Access is abstract;
   --  Present only if Self.Is_Expression_Function

   not overriding function Is_Renaming_Declaration
     (Self : aliased Subprogram_Declaration) return Boolean is abstract;

   not overriding function Renamed_Entity
     (Self : aliased Subprogram_Declaration)
      return Gela.Elements.Element_Access is abstract;
   --  Present only if Self.Is_Renaming_Declaration

end Gela.Declarations.Subprogram_Declarations;
