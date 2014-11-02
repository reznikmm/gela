------------------------------------------------------------------------------
--                        G E L A   G R A M M A R S                         --
--          Library for dealing with grammars for Gela project,             --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with Ada.Containers.Hashed_Sets;

with League.String_Vectors;

with Gela.Grammars.Ordered;

with AG_Tools.Visit_Generators;
with AG_Tools.Writers;

package AG_Tools.Contexts is

   type Partition_Array_Access is
     access all Gela.Grammars.Ordered.Partition_Array;

   type Part_Map is array (Gela.Grammars.Part_Index range <>) of Boolean;
   type Part_Map_Access is access all Part_Map;

   type Attr is record
      Origin : League.Strings.Universal_String;
      Decl   : Gela.Grammars.Attribute_Declaration_Index;
   end record;

   function Hash (Self : Attr) return Ada.Containers.Hash_Type;

   package Attr_Sets is new Ada.Containers.Hashed_Sets
     (Element_Type        => Attr,
      Hash                => Hash,
      Equivalent_Elements => "=");

   type Unit_Kinds is (Spec_Unit, Body_Unit);
   type With_Records is array (Unit_Kinds) of
     League.String_Vectors.Universal_String_Vector;

   type Context is tagged record
      Factory    : AG_Tools.Visit_Generators.Factory_Access;
      Grammar   : Gela.Grammars.Grammar_Access;
      Partition : Partition_Array_Access;
      Part_Map  : Part_Map_Access;
      Attr_Map  : Attr_Sets.Set;
      Withs     : With_Records;
      Spec      : AG_Tools.Writers.Writer;
      Impl      : AG_Tools.Writers.Writer;
      Code      : AG_Tools.Writers.Writer;
   end record;

   type Context_Access is access all Context;

   procedure Add_With
     (Self : access Context;
      Name : League.Strings.Universal_String;
      Kind : Unit_Kinds := Body_Unit);

   procedure Add_With
     (Self : access Context;
      Name : Wide_Wide_String;
      Kind : Unit_Kinds := Body_Unit);

   procedure Print_Withes
     (Self : access Context;
      Kind : Unit_Kinds);

end AG_Tools.Contexts;
