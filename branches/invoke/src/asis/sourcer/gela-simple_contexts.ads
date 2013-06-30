------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with League.Strings;

with Gela.Types;
with Gela.Contexts;
with Gela.Context_Fabrics;
with Gela.Mutables;
with Gela.Name_Schemas.GNAT;
with Gela.Source_Finders;
with Gela.Errors;
with Gela.Grammars;
with Gela.Grammars.LR_Tables;

package Gela.Simple_Contexts is

   type Context (On_Error : Gela.Context_Fabrics.On_Error_Callback)
     is new Gela.Contexts.Context with private;

   type Context_Access is access all Context;

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

private

   type Context (On_Error : Gela.Context_Fabrics.On_Error_Callback)
     is new Gela.Contexts.Context with
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
      Schema  : aliased Gela.Name_Schemas.GNAT.GNAT_Name_Schema;
      Finder  : Gela.Source_Finders.Source_Finder_Access;
      Errors  : Gela.Errors.Error_Handler_Access;
      Grammar : Gela.Grammars.Grammar_Access;
      Table   : Gela.Grammars.LR_Tables.Table_Access;
      Comp    : Gela.Mutables.Mutable_Compilation_Access;
   end record;

   procedure Parse_Parameters (Self : access Context);
   --  Parse Self.Parameters and fill Context fields.
   --  Raise ASIS_Failed if parameters inconsistent

   function Default_Path
     (Self : access Context) return League.Strings.Universal_String;

end Gela.Simple_Contexts;
