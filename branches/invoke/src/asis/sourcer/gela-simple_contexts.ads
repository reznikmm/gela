------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with Ada.Containers.Ordered_Maps;

with League.Strings;

with Gela.Types;
with Gela.Contexts;
with Gela.Name_Schemas.GNAT;
with Gela.Source_Finders;
with Gela.Errors;
with Gela.Grammars;
with Gela.Grammars.LR_Tables;
with Gela.Unit_Containers;
with Gela.Compilation_Unit_Lists;
limited with Gela.Simple_Contexts.Loaders;
with Gela.Mutables.Symbol_Sets;

package Gela.Simple_Contexts is

   type Context (On_Error : Gela.Errors.Error_Handler_Access)
     is limited new Gela.Contexts.Context
     and Gela.Unit_Containers.Unit_Container with private;

   type Context_Access is access all Context;

   not overriding function Symbols
     (Self    : access Context) return Gela.Types.Symbol_Set_Access;

   not overriding function Library_Unit_Declaration
     (Self  : access Context;
      Name  : Gela.Types.Symbol)
      return Gela.Types.Compilation_Unit;

   not overriding function Compilation_Unit_Body
     (Self  : access Context;
      Name  : Gela.Types.Symbol)
      return Gela.Types.Compilation_Unit;

private

   package Unit_Maps is new Ada.Containers.Ordered_Maps
     (Gela.Types.Symbol,
      Gela.Types.Compilation_Unit,
      Gela.Types."<",
      Gela.Types."=");

   package Payload_Maps is new Ada.Containers.Ordered_Maps
     (Gela.Types.Payload,
      Gela.Types.Compilation_Unit_Access,
      Gela.Types."<",
      Gela.Types."=");

   type Unit_List (Context : access Gela.Simple_Contexts.Context) is
     new Gela.Compilation_Unit_Lists.Abstract_Compilation_Unit_List
     and Gela.Compilation_Unit_Lists.Abstract_Compilation_Unit_Cursor with
   record
      Map   : Unit_Maps.Map;
   end record;

   overriding function Units_Count
     (Self    : access Unit_List;
      Payload : Gela.Types.Payload) return Natural;

   overriding function First
     (Self    : access Unit_List;
      Payload : Gela.Types.Payload) return Gela.Types.Compilation_Unit_Cursor;

   overriding function Element
     (Self    : access Unit_List;
      Payload : Gela.Types.Payload)
      return Gela.Types.Compilation_Unit;

   overriding function Next
     (Self    : access Unit_List;
      Payload : Gela.Types.Payload)
      return Gela.Types.Compilation_Unit_Cursor;

   type Loader_Access is access all Gela.Simple_Contexts.Loaders.Loader;

   type Context (On_Error : Gela.Errors.Error_Handler_Access)
     is limited new Gela.Contexts.Context
     and Gela.Unit_Containers.Unit_Container with
   record
      Is_Open    : Boolean;
      Name       : League.Strings.Universal_String;
      Parameters : League.Strings.Universal_String;
      --  Parsed parameters
      File_Name : League.Strings.Universal_String;
      --  File to read
      Path : League.Strings.Universal_String;
      --  Search path
      Debug : League.Strings.Universal_String;
      --  TODO
      Loader  : Loader_Access;
      Specs   : aliased Unit_List (Context'Unchecked_Access);
      Bodies  : aliased Unit_List (Context'Unchecked_Access);
      Units   : Payload_Maps.Map;
      Schema  : aliased Gela.Name_Schemas.GNAT.GNAT_Name_Schema;
      Finder  : Gela.Source_Finders.Source_Finder_Access;
      Errors  : Gela.Errors.Error_Handler_Access;
      Grammar : Gela.Grammars.Grammar_Access;
      Table   : Gela.Grammars.LR_Tables.Table_Access;
      Symbols : aliased Mutables.Symbol_Sets.Symbol_Set;
   end record;

   procedure Parse_Parameters (Self : access Context);
   --  Parse Self.Parameters and fill Context fields.
   --  Raise ASIS_Failed if parameters inconsistent

   function Default_Path
     (Self : access Context) return League.Strings.Universal_String;

   overriding procedure Associate
     (Self       : access Context;
      Name       : League.Strings.Universal_String;
      Parameters : League.Strings.Universal_String);

   overriding procedure Open (Self : access Context);

   overriding function Is_Open (Self : access Context) return Boolean;

   overriding procedure Close (Self : access Context);

   overriding procedure Dissociate (Self : access Context);

   overriding function Name
     (Self : access Context)
      return League.Strings.Universal_String;

   overriding function Parameters
     (Self : access Context)
      return League.Strings.Universal_String;

   overriding function Length (Self : access Context) return Natural;

   overriding function Container
     (Self  : access Context;
      Index : Positive)
      return Gela.Types.Container_Access;

   overriding function Debug_Image
     (Self : access Context) return League.Strings.Universal_String;

   --  Unit_Container functions
   overriding function Parent
     (Self : access Context)
      return Gela.Types.Context_Access;

   overriding function Library_Unit_Declaration
     (Self  : access Context;
      Name  : League.Strings.Universal_String)
      return Gela.Types.Compilation_Unit;

   overriding function Compilation_Unit_Body
     (Self  : access Context;
      Name  : League.Strings.Universal_String)
      return Gela.Types.Compilation_Unit;

   overriding function Library_Unit_Declarations
     (Self  : access Context)
      return Gela.Types.Compilation_Unit_List;

   overriding function Compilation_Unit_Bodies
     (Self  : access Context)
      return Gela.Types.Compilation_Unit_List;

end Gela.Simple_Contexts;
