--  This package provides Compilation_Unit_Set interface and its methods.

limited with Gela.Compilation_Units;
with Gela.Lexical_Types;

package Gela.Compilation_Unit_Sets is
   pragma Preelaborate;

   type Compilation_Unit_Set is limited interface;
   --  Set of compilation unit. Only one unit with particular Name cound be
   --  stored in set.
   type Compilation_Unit_Set_Access is access all Compilation_Unit_Set'Class;
   for Compilation_Unit_Set_Access'Storage_Size use 0;

   type Compilation_Unit_Cursor is interface;
   --  Cursor to iterate over compilation unit set.
   type Compilation_Unit_Cursor_Access is access all
     Compilation_Unit_Cursor'Class;
   for Compilation_Unit_Cursor_Access'Storage_Size use 0;

   not overriding function Is_Empty
     (Self : Compilation_Unit_Set) return Boolean is abstract;
   --  Check is set is empty

   not overriding function Length
     (Self : Compilation_Unit_Set) return Natural is abstract;
   --  Return length of set

   not overriding function Find
     (Self   : Compilation_Unit_Set;
      Symbol : Gela.Lexical_Types.Symbol)
      return Gela.Compilation_Units.Compilation_Unit_Access is abstract;
   --  Find compilation unit with given name (Symbol).
   --  Return null if not found.

   not overriding procedure Add
     (Self : in out Compilation_Unit_Set;
      Item : Gela.Compilation_Units.Compilation_Unit_Access) is abstract;
   --  Add compilation unit into set.

   not overriding function First
     (Self : Compilation_Unit_Set)
      return Compilation_Unit_Cursor'Class is abstract;
   --  Return cursor pointing to first unit in the set.

   not overriding function Has_Element
     (Self : Compilation_Unit_Cursor) return Boolean is abstract;
   --  Check if cursor pointing to unit.

   not overriding function Element
     (Self : Compilation_Unit_Cursor)
      return Gela.Compilation_Units.Compilation_Unit_Access is abstract;
   --  Return compilation unit pointed by cursor.

   not overriding procedure Next
     (Self : in out Compilation_Unit_Cursor) is abstract;
   --  Shift cursor to next compilation unit in set.

end Gela.Compilation_Unit_Sets;
