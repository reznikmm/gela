------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with League.Strings;

with Gela.Compilation_Units;
with Gela.Compilation_Unit_Lists;
with Gela.Mutables;
with Gela.Types;

package Gela.Simple_Compilation_Units is
--   pragma Preelaborate;

   type Simple_Compilation_Unit is
     new Gela.Compilation_Units.Abstract_Compilation_Unit
     and Gela.Compilation_Unit_Lists.Abstract_Compilation_Unit_Cursor
       with private;

   type Simple_Compilation_Unit_Access is access all
     Simple_Compilation_Unit'Class;

   function Create
     (Compilation : Gela.Mutables.Mutable_Compilation_Access;
      Payload     : Gela.Types.Payload;
      Kind        : Gela.Types.Unit_Kinds;
      Unit_Class  : Gela.Types.Unit_Classes;
      Origin      : Gela.Types.Unit_Origins;
      Full_Name   : Gela.Types.Symbol)
     return Gela.Types.Compilation_Unit;

   function Create_Body
     (Compilation : Gela.Mutables.Mutable_Compilation_Access;
      Payload     : Gela.Types.Payload;
      Kind        : Gela.Types.Unit_Kinds;
      Unit_Class  : Gela.Types.Unit_Classes;
      Origin      : Gela.Types.Unit_Origins;
      Full_Name   : Gela.Types.Symbol;
      Declaration : Gela.Types.Compilation_Unit)
     return Gela.Types.Compilation_Unit;

   function Create_Subunit
     (Compilation : Gela.Mutables.Mutable_Compilation_Access;
      Payload     : Gela.Types.Payload;
      Kind        : Gela.Types.Unit_Kinds;
      Unit_Class  : Gela.Types.Unit_Classes;
      Origin      : Gela.Types.Unit_Origins;
      Full_Name   : Gela.Types.Symbol;
      Inside      : Gela.Types.Compilation_Unit)
     return Gela.Types.Compilation_Unit;

--     function Payload
--       (Self : access Simple_Compilation_Unit) return Gela.Types.Payload;

   not overriding function Children
     (Self    : access Simple_Compilation_Unit;
      Payload : Gela.Types.Payload)
      return Gela.Types.Compilation_Unit_List;

private

   type Simple_Unit_List is
     new Gela.Compilation_Unit_Lists.Abstract_Compilation_Unit_List with
   record
      Head  : Gela.Types.Compilation_Unit;
      Count : Natural := 0;
   end record;

   overriding function Units_Count
     (Self    : access Simple_Unit_List;
      Payload : Gela.Types.Payload)
      return Natural;

   overriding function First
     (Self    : access Simple_Unit_List;
      Payload : Gela.Types.Payload)
      return Gela.Types.Compilation_Unit_Cursor;

   not overriding procedure Append
     (Self    : access Simple_Unit_List;
      Unit    : Gela.Types.Compilation_Unit);

   type Simple_Compilation_Unit is
     new Gela.Compilation_Units.Abstract_Compilation_Unit
     and Gela.Compilation_Unit_Lists.Abstract_Compilation_Unit_Cursor with
   record
      Payload     : Gela.Types.Payload;
      Compilation : Gela.Mutables.Mutable_Compilation_Access;
      Kind        : Gela.Types.Unit_Kinds;
      Unit_Class  : Gela.Types.Unit_Classes;
      Origin      : Gela.Types.Unit_Origins;
      Full_Name   : Gela.Types.Symbol;
      Subunits    : aliased Simple_Unit_List;
      Children    : aliased Simple_Unit_List;
      Corresponding_Declaration : Gela.Types.Compilation_Unit;
      Corresponding_Body        : Gela.Types.Compilation_Unit;
      Corresponding_Subunit_Parent_Body : Gela.Types.Compilation_Unit;
      Next_Unit : Gela.Types.Compilation_Unit;
   end record;

   overriding function Unit_Kind
     (Self    : access Simple_Compilation_Unit;
      Payload : Gela.Types.Payload)
      return Gela.Types.Unit_Kinds;

   overriding function Unit_Class
     (Self    : access Simple_Compilation_Unit;
      Payload : Gela.Types.Payload)
      return Gela.Types.Unit_Classes;

   overriding function Unit_Origin
     (Self    : access Simple_Compilation_Unit;
      Payload : Gela.Types.Payload)
      return Gela.Types.Unit_Origins;

   overriding function Enclosing_Container
     (Self    : access Simple_Compilation_Unit;
      Payload : Gela.Types.Payload)
      return Gela.Types.Container_Access;

--     function Corresponding_Children
--       (Self    : Simple_Compilation_Unit;
--        Payload : Gela.Types.Payload)
--        return Gela.Types.Compilation_Unit;
--  This should be calculated and stored in context

   overriding function Corresponding_Parent_Declaration
     (Self    : access Simple_Compilation_Unit;
      Payload : Gela.Types.Payload)
      return Gela.Types.Compilation_Unit;

   overriding function Corresponding_Declaration
     (Self    : access Simple_Compilation_Unit;
      Payload : Gela.Types.Payload)
      return Gela.Types.Compilation_Unit;

   overriding function Corresponding_Body
     (Self    : access Simple_Compilation_Unit;
      Payload : Gela.Types.Payload)
      return Gela.Types.Compilation_Unit;

   overriding function Subunits
     (Self    : access Simple_Compilation_Unit;
      Payload : Gela.Types.Payload)
      return Gela.Types.Compilation_Unit_List;

   overriding function Corresponding_Subunit_Parent_Body
     (Self    : access Simple_Compilation_Unit;
      Payload : Gela.Types.Payload)
      return Gela.Types.Compilation_Unit;

   overriding function With_List
     (Self    : access Simple_Compilation_Unit;
      Payload : Gela.Types.Payload)
      return Gela.Types.Compilation_Unit_List;

   overriding function Limited_With_List
     (Self    : access Simple_Compilation_Unit;
      Payload : Gela.Types.Payload)
      return Gela.Types.Compilation_Unit_List;

   overriding function Unit_Full_Name
     (Self    : access Simple_Compilation_Unit;
      Payload : Gela.Types.Payload)
      return League.Strings.Universal_String;

   overriding function Unique_Name
     (Self    : access Simple_Compilation_Unit;
      Payload : Gela.Types.Payload)
      return League.Strings.Universal_String;

   overriding function Flags
     (Self    : access Simple_Compilation_Unit;
      Payload : Gela.Types.Payload)
      return Gela.Compilation_Units.Unit_Flag;

   overriding function Compilation
     (Self    : access Simple_Compilation_Unit;
      Payload : Gela.Types.Payload)
      return Gela.Types.Compilation_Access;

   overriding function Element
     (Self    : access Simple_Compilation_Unit;
      Payload : Gela.Types.Payload)
      return Gela.Types.Compilation_Unit;

   overriding function Next
     (Self    : access Simple_Compilation_Unit;
      Payload : Gela.Types.Payload)
      return Gela.Types.Compilation_Unit_Cursor;

end Gela.Simple_Compilation_Units;
