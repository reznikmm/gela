------------------------------------------------------------------------------
--                        G E L A   G R A M M A R S                         --
--          Library for dealing with grammars for Gela project,             --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with Gela.Grammars.Ordered;
with AG_Tools.Contexts;
with AG_Tools.Visit_Generators;

package AG_Tools.NT_Generators is

   type Generator (Context : AG_Tools.Contexts.Context_Access)
     is new AG_Tools.Visit_Generators.NT_Generator with null record;

   overriding procedure Make_Procedure
     (Self : access Generator;
      Pos  : in out Gela.Grammars.Ordered.Order_Maps.Cursor);

   overriding procedure Make_Local_Variable
     (Self      : access Generator;
      Origin    : League.Strings.Universal_String;
      Attribute : Gela.Grammars.Attribute_Declaration);

   overriding procedure Make_Get
     (Self      : access Generator;
      Attribute : Gela.Grammars.Attribute);

   overriding procedure Make_Set
     (Self      : access Generator;
      Attribute : Gela.Grammars.Attribute);

   type List_Generator (Context : AG_Tools.Contexts.Context_Access)
     is new AG_Tools.Visit_Generators.NT_Generator with null record;

   overriding procedure Make_Procedure
     (Self : access List_Generator;
      Pos  : in out Gela.Grammars.Ordered.Order_Maps.Cursor);

   overriding procedure Make_Local_Variable
     (Self      : access List_Generator;
      Origin    : League.Strings.Universal_String;
      Attribute : Gela.Grammars.Attribute_Declaration);

   overriding procedure Make_Get
     (Self      : access List_Generator;
      Attribute : Gela.Grammars.Attribute);

   overriding procedure Make_Set
     (Self      : access List_Generator;
      Attribute : Gela.Grammars.Attribute);

end AG_Tools.NT_Generators;
