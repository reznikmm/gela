--  This package provides Compilation_Unit_Set interface and its methods.

with Ada.Iterator_Interfaces;

with League.Strings;

with Gela.Contexts;
with Gela.Compilation_Units;
with Gela.Symbols;

package Gela.Compilation_Unit_Sets is
   pragma Preelaborate;

   type Compilation_Unit_Set is limited interface
     with
       Constant_Indexing => Constant_Indexing,
       Default_Iterator  => Iterate,
       Iterator_Element  => Gela.Compilation_Units.Compilation_Unit'Class;
   --  Set of compilation unit. Only one unit with particular Name cound be
   --  stored in set.
   type Compilation_Unit_Set_Access is
     access constant Compilation_Unit_Set'Class;
   for Compilation_Unit_Set_Access'Storage_Size use 0;

   not overriding function Is_Empty
     (Self : Compilation_Unit_Set) return Boolean is abstract;
   --  Check is set is empty

   not overriding function Length
     (Self : Compilation_Unit_Set) return Natural is abstract;
   --  Return length of set

   not overriding function Find
     (Self   : Compilation_Unit_Set;
      Symbol : not null Gela.Symbols.Symbol_Access)
    return Gela.Compilation_Units.Compilation_Unit_Access is abstract;
   --  Find compilation unit with given name (Symbol).
   --  Return null if not found.

   not overriding function Context
     (Self : Compilation_Unit_Set)
      return Gela.Contexts.Context_Access is abstract;
   --  Return corresponding context

   --  Iterator related syntactic sugar below

   function Assigned
     (Self : Gela.Compilation_Units.Compilation_Unit_Access) return Boolean;
   pragma Inline (Assigned);

   package Iterator_Interfaces is new Ada.Iterator_Interfaces
     (Cursor       => Gela.Compilation_Units.Compilation_Unit_Access,
      Has_Element  => Assigned);

   not overriding function Iterate
     (Self : Compilation_Unit_Set)
      return Iterator_Interfaces.Forward_Iterator'Class is abstract;
   --  Return iterator over the set.

   type Reference_Type
     (Unit : not null access constant
        Gela.Compilation_Units.Compilation_Unit'Class) is null record
          with Implicit_Dereference => Unit;

   function Constant_Indexing
     (Self   : Compilation_Unit_Set'Class;
      Cursor : not null Gela.Compilation_Units.Compilation_Unit_Access)
      return Reference_Type;

   function Constant_Indexing
     (Self  : Compilation_Unit_Set'Class;
      Value : Wide_Wide_String)
      return Reference_Type;

   function Constant_Indexing
     (Self  : Compilation_Unit_Set'Class;
      Value : League.Strings.Universal_String)
      return Reference_Type;
   pragma Inline (Constant_Indexing);

end Gela.Compilation_Unit_Sets;
