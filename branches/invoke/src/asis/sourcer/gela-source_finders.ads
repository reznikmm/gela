------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with League.Strings;

with Gela.Name_Schemas;

package Gela.Source_Finders is

   type Source_Finder is tagged private;
   type Source_Finder_Access is access all Source_Finder'Class;

   procedure Lookup_File
     (Self  : access Source_Finder;
      Name  : League.Strings.Universal_String;
      Found : out Boolean;
      File  : in out League.Strings.Universal_String;
      Text  : out League.Strings.Universal_String);
   --  Lookup file with given base Name.
   --  Return full name as File and content as Text if Found

   procedure Lookup_Declaration
     (Self  : access Source_Finder;
      Unit  : League.Strings.Universal_String;
      Found : out Boolean;
      File  : out League.Strings.Universal_String;
      Text  : out League.Strings.Universal_String);

   procedure Lookup_Body
     (Self  : access Source_Finder;
      Unit  : League.Strings.Universal_String;
      Found : out Boolean;
      File  : out League.Strings.Universal_String;
      Text  : out League.Strings.Universal_String);

   function Is_Predefined
     (Self  : access Source_Finder;
      File  : League.Strings.Universal_String) return Boolean;


   function Create
     (Path   : League.Strings.Universal_String;
      Schema : Gela.Name_Schemas.Name_Schema_Access;
      Next   : Source_Finder_Access := null)
      return Source_Finder_Access;

   procedure Destroy (Self : in out Source_Finder_Access);

private

   type Source_Finder is tagged record
      Directory : League.Strings.Universal_String;
      Schema    : Gela.Name_Schemas.Name_Schema_Access;
      Next      : Source_Finder_Access;
   end record;

end Gela.Source_Finders;
