with Ada.Containers.Hashed_Maps;

with Gela.Compilation_Unit_Sets;
with Gela.Compilation_Units;
with Gela.Lexical_Types;

package Gela.Plain_Compilation_Unit_Sets is
   pragma Preelaborate;

   type Compilation_Unit_Set is
     limited new Gela.Compilation_Unit_Sets.Compilation_Unit_Set with private;
   type Compilation_Unit_Set_Access is access all Compilation_Unit_Set'Class;

   not overriding procedure Add
     (Self : in out Compilation_Unit_Set;
      Item : Gela.Compilation_Units.Compilation_Unit_Access);
   --  Add compilation unit into set.

private

   function Hash
     (Item : Gela.Lexical_Types.Symbol) return Ada.Containers.Hash_Type;

   package Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Gela.Lexical_Types.Symbol,
      Element_Type    => Gela.Compilation_Units.Compilation_Unit_Access,
      Hash            => Hash,
      Equivalent_Keys => Gela.Lexical_Types."=",
      "="             => Gela.Compilation_Units."=");

   type Compilation_Unit_Set is
     limited new Gela.Compilation_Unit_Sets.Compilation_Unit_Set with
   record
      Map : Maps.Map;
   end record;

   type Compilation_Unit_Cursor is
     new Gela.Compilation_Unit_Sets.Compilation_Unit_Cursor with
   record
      Value : Maps.Cursor;
   end record;

   type Compilation_Unit_Cursor_Access is access all
     Compilation_Unit_Cursor'Class;

   overriding function Is_Empty
     (Self : Compilation_Unit_Set) return Boolean;

   overriding function Length
     (Self : Compilation_Unit_Set) return Natural;

   overriding function Find
     (Self   : Compilation_Unit_Set;
      Symbol : Gela.Lexical_Types.Symbol)
      return Gela.Compilation_Units.Compilation_Unit_Access;

   overriding function First
     (Self : Compilation_Unit_Set)
      return Gela.Compilation_Unit_Sets.Compilation_Unit_Cursor'Class;

   overriding function Has_Element
     (Self : Compilation_Unit_Cursor)
      return Boolean;

   overriding function Element
     (Self : Compilation_Unit_Cursor)
      return Gela.Compilation_Units.Compilation_Unit_Access;

   overriding procedure Next
     (Self : in out Compilation_Unit_Cursor);

end Gela.Plain_Compilation_Unit_Sets;
