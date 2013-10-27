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

package AG_Tools.Part_Generators is

   type Generator (Context : AG_Tools.Contexts.Context_Access)
     is new AG_Tools.Visit_Generators.Part_Generator with null record;

   overriding procedure Make_Descent
     (Self : access Generator;
      Part : Gela.Grammars.Part_Index;
      Pass : Positive);

   overriding procedure Make_Local_Variable
     (Self      : access Generator;
      Origin    : League.Strings.Universal_String;
      Attribute : Gela.Grammars.Attribute_Declaration);

   overriding procedure Make_Local_Variable
     (Self : access Generator;
      Part : Gela.Grammars.Part_Index);

   overriding procedure Make_Get
     (Self      : access Generator;
      Attribute : Gela.Grammars.Attribute);

   overriding procedure Make_Set
     (Self      : access Generator;
      Attribute : Gela.Grammars.Attribute);

   type List_Generator (Context : AG_Tools.Contexts.Context_Access)
     is new Generator (Context) with null record;

   overriding procedure Make_Descent
     (Self : access List_Generator;
      Part : Gela.Grammars.Part_Index;
      Pass : Positive);

   overriding procedure Make_Set
     (Self      : access List_Generator;
      Attribute : Gela.Grammars.Attribute);

   type Option_Generator (Context : AG_Tools.Contexts.Context_Access)
     is new Generator (Context) with null record;

   overriding procedure Make_Descent
     (Self : access Option_Generator;
      Part : Gela.Grammars.Part_Index;
      Pass : Positive);

   type Head_Generator (Context : AG_Tools.Contexts.Context_Access)
     is new Generator (Context) with null record;

   overriding procedure Make_Descent
     (Self : access Head_Generator;
      Part : Gela.Grammars.Part_Index;
      Pass : Positive);

   overriding procedure Make_Set
     (Self      : access Head_Generator;
      Attribute : Gela.Grammars.Attribute);

   overriding procedure Make_Get
     (Self      : access Head_Generator;
      Attribute : Gela.Grammars.Attribute);

end AG_Tools.Part_Generators;
