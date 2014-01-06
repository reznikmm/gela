limited with Gela.Compilation_Units;
with Gela.Lexical_Types;

package Gela.Compilation_Unit_Sets is
   pragma Preelaborate;

   type Compilation_Unit_Set is limited interface;
   type Compilation_Unit_Set_Access is access all Compilation_Unit_Set'Class;
   for Compilation_Unit_Set_Access'Storage_Size use 0;

   type Compilation_Unit_Cursor is interface;
   type Compilation_Unit_Cursor_Access is access all
     Compilation_Unit_Cursor'Class;
   for Compilation_Unit_Cursor_Access'Storage_Size use 0;

   not overriding function Is_Empty
     (Self : Compilation_Unit_Set) return Boolean is abstract;

   not overriding function Length
     (Self : Compilation_Unit_Set) return Natural is abstract;

   not overriding function Find
     (Self   : Compilation_Unit_Set;
      Symbol : Gela.Lexical_Types.Symbol)
      return Gela.Compilation_Units.Compilation_Unit_Access is abstract;

   not overriding procedure Add
     (Self : in out Compilation_Unit_Set;
      Item : Gela.Compilation_Units.Compilation_Unit_Access) is abstract;

   not overriding function First
     (Self : Compilation_Unit_Set)
      return Compilation_Unit_Cursor'Class is abstract;

   not overriding function Has_Element
     (Self : Compilation_Unit_Cursor)
      return Boolean is abstract;

   not overriding function Element
     (Self : Compilation_Unit_Cursor)
      return Gela.Compilation_Units.Compilation_Unit_Access is abstract;

   not overriding procedure Next
     (Self : in out Compilation_Unit_Cursor) is abstract;

end Gela.Compilation_Unit_Sets;
