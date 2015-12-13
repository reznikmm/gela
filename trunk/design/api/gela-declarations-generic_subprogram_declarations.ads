with Gela.Declarations.Callables;

package Gela.Declarations.Generic_Subprogram_Declarations is
   pragma Preelaborate;

   type Generic_Subprogram_Declaration is limited interface
     and Gela.Declarations.Callables.Callable;
   --  Generic function and procedure declarations
   type Generic_Subprogram_Declaration_Access is
     access constant Generic_Subprogram_Declaration'Class;
   for Generic_Subprogram_Declaration_Access'Storage_Size use 0;

   not overriding function Is_Function
     (Self : aliased Generic_Subprogram_Declaration) return Boolean
        is abstract;

   not overriding function Is_Procedure
     (Self : aliased Generic_Subprogram_Declaration) return Boolean
        is abstract;

   not overriding function Generic_Formal_Part
     (Self : aliased Generic_Subprogram_Declaration)
      return Gela.Element_Sequences.Element_Sequence_Access is abstract;

end Gela.Declarations.Generic_Subprogram_Declarations;
