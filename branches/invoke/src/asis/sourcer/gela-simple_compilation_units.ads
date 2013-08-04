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
with Gela.Mutables.Compilations;
with Gela.Types;

package Gela.Simple_Compilation_Units is
--   pragma Preelaborate;

   type Simple_Compilation_Unit is
     new Gela.Compilation_Units.Abstract_Compilation_Unit with private;

   function Create
     (Compilation : Gela.Mutables.Compilations.Compilation_Access;
      Payload     : Gela.Types.Payload)
     return Gela.Types.Compilation_Unit;

--     function Payload
--       (Self : access Simple_Compilation_Unit) return Gela.Types.Payload;

private

   type Simple_Compilation_Unit is
     new Gela.Compilation_Units.Abstract_Compilation_Unit with
   record
      Payload     : Gela.Types.Payload;
      Compilation : Gela.Mutables.Compilations.Compilation_Access;
   end record;

   type Simple_Compilation_Unit_Access is access all Simple_Compilation_Unit;

   overriding function Unit_Kind
     (Self    : Simple_Compilation_Unit;
      Payload : Gela.Types.Payload)
      return Natural;

   overriding function Unit_Class
     (Self    : Simple_Compilation_Unit;
      Payload : Gela.Types.Payload)
      return Natural;

   overriding function Unit_Origin
     (Self    : Simple_Compilation_Unit;
      Payload : Gela.Types.Payload)
      return Natural;

   overriding function Enclosing_Container
     (Self    : Simple_Compilation_Unit;
      Payload : Gela.Types.Payload)
      return Gela.Types.Container_Access;

--     function Corresponding_Children
--       (Self    : Simple_Compilation_Unit;
--        Payload : Gela.Types.Payload)
--        return Gela.Types.Compilation_Unit;
--  This should be calculated and stored in context

   overriding function Corresponding_Parent_Declaration
     (Self    : Simple_Compilation_Unit;
      Payload : Gela.Types.Payload)
      return Gela.Types.Compilation_Unit;

   overriding function Corresponding_Declaration
     (Self    : Simple_Compilation_Unit;
      Payload : Gela.Types.Payload)
      return Gela.Types.Compilation_Unit;

   overriding function Corresponding_Body
     (Self    : Simple_Compilation_Unit;
      Payload : Gela.Types.Payload)
      return Gela.Types.Compilation_Unit;

   overriding function Subunits
     (Self    : Simple_Compilation_Unit;
      Payload : Gela.Types.Payload)
      return Gela.Types.Compilation_Unit_List;

   overriding function Corresponding_Subunit_Parent_Body
     (Self    : Simple_Compilation_Unit;
      Payload : Gela.Types.Payload)
      return Gela.Types.Compilation_Unit;

   overriding function With_List
     (Self    : Simple_Compilation_Unit;
      Payload : Gela.Types.Payload)
      return Gela.Types.Compilation_Unit_List;

   overriding function Limited_With_List
     (Self    : Simple_Compilation_Unit;
      Payload : Gela.Types.Payload)
      return Gela.Types.Compilation_Unit_List;

   overriding function Unit_Full_Name
     (Self    : Simple_Compilation_Unit;
      Payload : Gela.Types.Payload)
      return League.Strings.Universal_String;

   overriding function Unique_Name
     (Self    : Simple_Compilation_Unit;
      Payload : Gela.Types.Payload)
      return League.Strings.Universal_String;

   overriding function Flags
     (Self    : Simple_Compilation_Unit;
      Payload : Gela.Types.Payload)
      return Gela.Compilation_Units.Unit_Flag;

   overriding function Compilation
     (Self    : Simple_Compilation_Unit;
      Payload : Gela.Types.Payload)
      return Gela.Types.Compilation_Access;

end Gela.Simple_Compilation_Units;
