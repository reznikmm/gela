--  SPDX-FileCopyrightText: 2019 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Program.Compilation_Units;

package Program.Unit_Dependencies is
   pragma Preelaborate;

   type Unit_Dependency_Listener is limited interface;

   not overriding procedure Required_Declaration
     (Self   : in out Unit_Dependency_Listener;
      Name   : Program.Text;
      If_Any : Boolean := False) is null;
   --  Library unit declaration is required (if any when If_Any).

   not overriding procedure Required_Body
     (Self : in out Unit_Dependency_Listener;
      Name : Program.Text) is null;
   --  Library unit body or subunit is required

   not overriding procedure Required_Unit
     (Self       : in out Unit_Dependency_Listener;
      Name       : Program.Text;
      Is_Limited : Boolean) is null;
   --  Library unit declaration or body is required

   not overriding procedure Needed_Declaration
     (Self : in out Unit_Dependency_Listener;
      Name : Program.Text) is null;
   --  Library unit declaration and all other compilation units needed by it
   --  are required

   procedure Find_Semantic_Dependencies
     (Unit   : Program.Compilation_Units.Compilation_Unit_Access;
      Report : in out Unit_Dependency_Listener'Class);

   procedure Find_Needed_Units
     (Unit   : Program.Compilation_Units.Compilation_Unit_Access;
      Report : in out Unit_Dependency_Listener'Class)
        with No_Return;

   procedure Find_Elaboration_Dependencies
     (Unit   : Program.Compilation_Units.Compilation_Unit_Access;
      Report : in out Unit_Dependency_Listener'Class)
        with No_Return;

end Program.Unit_Dependencies;
