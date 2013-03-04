------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with Asis;

with League.Strings;

with Gela.Types;

package Gela.Compilation_Units is

   type Abstract_Compilation_Unit is interface;

   function Unit_Kind
     (Self    : Abstract_Compilation_Unit;
      Payload : Gela.Types.Payload)
      return Asis.Unit_Kinds is abstract;

   function Unit_Class
     (Self    : Abstract_Compilation_Unit;
      Payload : Gela.Types.Payload)
      return Asis.Unit_Classes is abstract;

   function Unit_Origin
     (Self    : Abstract_Compilation_Unit;
      Payload : Gela.Types.Payload)
      return Asis.Unit_Origins is abstract;

   function Enclosing_Container
     (Self    : Abstract_Compilation_Unit;
      Payload : Gela.Types.Payload)
      return Gela.Types.Container_Access is abstract;

--     function Corresponding_Children
--       (Self    : Abstract_Compilation_Unit;
--        Payload : Gela.Types.Payload)
--        return Gela.Types.Compilation_Unit is abstract;
--  This should be calculated and stored in context

   function Corresponding_Parent_Declaration
     (Self    : Abstract_Compilation_Unit;
      Payload : Gela.Types.Payload)
      return Gela.Types.Compilation_Unit is abstract;

   function Corresponding_Declaration
     (Self    : Abstract_Compilation_Unit;
      Payload : Gela.Types.Payload)
      return Gela.Types.Compilation_Unit is abstract;

   function Corresponding_Body
     (Self    : Abstract_Compilation_Unit;
      Payload : Gela.Types.Payload)
      return Gela.Types.Compilation_Unit is abstract;

   function Subunits
     (Self    : Abstract_Compilation_Unit;
      Payload : Gela.Types.Payload)
      return Gela.Types.Compilation_Unit_List is abstract;

   function Corresponding_Subunit_Parent_Body
     (Self    : Abstract_Compilation_Unit;
      Payload : Gela.Types.Payload)
      return Gela.Types.Compilation_Unit is abstract;

   function With_List
     (Self    : Abstract_Compilation_Unit;
      Payload : Gela.Types.Payload)
      return Gela.Types.Compilation_Unit_List is abstract;

   function Limited_With_List
     (Self    : Abstract_Compilation_Unit;
      Payload : Gela.Types.Payload)
      return Gela.Types.Compilation_Unit_List is abstract;

   function Unit_Full_Name
     (Self    : Abstract_Compilation_Unit;
      Payload : Gela.Types.Payload)
      return League.Strings.Universal_String is abstract;

   function Unique_Name
     (Self    : Abstract_Compilation_Unit;
      Payload : Gela.Types.Payload)
      return League.Strings.Universal_String is abstract;

   type Unit_Flag is mod 2 ** 3;
   function Not_Exists    return Unit_Flag is (1) with Inline;
   function Can_Be_Main   return Unit_Flag is (2) with Inline;
   function Body_Required return Unit_Flag is (4) with Inline;

   function Flags
     (Self    : Abstract_Compilation_Unit;
      Payload : Gela.Types.Payload)
      return Unit_Flag is abstract;

   function Compilation
     (Self    : Abstract_Compilation_Unit;
      Payload : Gela.Types.Payload)
      return Gela.Types.Compilation_Access is abstract;

   --  Shall we dublicate these in Compilation_Unit to speedup something?
   --  function Text_Name (Compilation_Unit : in Asis.Compilation_Unit)
   --  function Object_Name (Compilation_Unit : in Asis.Compilation_Unit)
   --  function Compilation_Command_Line_Options

end Gela.Compilation_Units;
