------------------------------------------------------------------------------
--                        G E L A   G R A M M A R S                         --
--          Library for dealing with grammars for Gela project,             --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with AG_Tools.Contexts;
with AG_Tools.Visit_Generators;
with AG_Tools.NT_Generators;
with AG_Tools.Part_Generators;

package AG_Tools.Generator_Fabrics is

   type Fabric (Context : AG_Tools.Contexts.Context_Access) is
     new AG_Tools.Visit_Generators.Fabric with private;

   overriding function Get
     (Self : access Fabric;
      NT   : Gela.Grammars.Non_Terminal)
      return AG_Tools.Visit_Generators.NT_Generator_Access;

   overriding function Get
     (Self : access Fabric;
      Part : Gela.Grammars.Part)
      return AG_Tools.Visit_Generators.Part_Generator_Access;

   overriding function Get
     (Self : access Fabric;
      Attr : Gela.Grammars.Attribute;
      NT   : Gela.Grammars.Non_Terminal)
      return AG_Tools.Visit_Generators.Generator_Access;

private

   type Fabric (Context : AG_Tools.Contexts.Context_Access) is
     new AG_Tools.Visit_Generators.Fabric with record
      NT   : aliased AG_Tools.NT_Generators.Generator (Context);
      List : aliased AG_Tools.NT_Generators.List_Generator (Context);
      Part : aliased AG_Tools.Part_Generators.Generator (Context);
      Seq  : aliased AG_Tools.Part_Generators.List_Generator (Context);
      Opt  : aliased AG_Tools.Part_Generators.Option_Generator (Context);
      Head : aliased AG_Tools.Part_Generators.Head_Generator (Context);
   end record;

end AG_Tools.Generator_Fabrics;
