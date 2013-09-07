------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

package Gela.Simple_Contexts.Loaders is

   type Loader (Context : Context_Access) is tagged private;

   procedure Try_Read_File_And_Supporters
     (Self      : in out Loader;
      File_Name : League.Strings.Universal_String);
   --  Procedure reads a unit, its declaration, parent and withed units,
   --  then runs passes and adds resulting Compilation_Unit to Self.Context

private

   type Loader (Context : Context_Access) is tagged null record;

   procedure Read_File_And_Supporters
     (Self : in out Loader;
      Name : League.Strings.Universal_String;
      Text : League.Strings.Universal_String);
   --  Main recursive subprogram to process sources.
   --  Name - full name of the file containing Text.
   --  Procedure reads a unit, its declaration, parent and withed units,
   --  then runs passes and adds resulting Compilation_Unit to Self.Context

   not overriding procedure Read_Unit_Declaration
     (Self      : in out Loader;
      Full_Name : Gela.Types.Symbol;
      Result    : out Gela.Types.Compilation_Unit;
      Found     : out Boolean);

   not overriding procedure Read_Unit_Body
     (Self      : in out Loader;
      Full_Name : Gela.Types.Symbol;
      Result    : out Gela.Types.Compilation_Unit;
      Found     : out Boolean);

end Gela.Simple_Contexts.Loaders;
