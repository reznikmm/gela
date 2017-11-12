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
with Anagram.Grammars.Rule_Templates;

package AG_Tools.Part_Generators is

   type Generator (Context : AG_Tools.Contexts.Context_Access)
     is new AG_Tools.Visit_Generators.Part_Generator with null record;

   overriding procedure Make_Descent
     (Self : access Generator;
      Part : Anagram.Grammars.Part_Index;
      Pass : Positive);

   overriding procedure Make_Local_Variable
     (Self      : access Generator;
      Origin    : League.Strings.Universal_String;
      Attribute : Anagram.Grammars.Attribute_Declaration);

   overriding procedure Make_Local_Variable
     (Self : access Generator;
      Part : Anagram.Grammars.Part_Index);

   overriding procedure Make_Get
     (Self      : access Generator;
      Attribute : Anagram.Grammars.Attribute;
      Template  : Anagram.Grammars.Rule_Templates.Rule_Template);

   overriding procedure Make_Set
     (Self      : access Generator;
      Attribute : Anagram.Grammars.Attribute);

   type List_Generator (Context : AG_Tools.Contexts.Context_Access)
     is new Generator (Context) with null record;

   overriding procedure Make_Descent
     (Self : access List_Generator;
      Part : Anagram.Grammars.Part_Index;
      Pass : Positive);

   overriding procedure Make_Get
     (Self      : access List_Generator;
      Attribute : Anagram.Grammars.Attribute;
      Template  : Anagram.Grammars.Rule_Templates.Rule_Template);

   overriding procedure Make_Set
     (Self      : access List_Generator;
      Attribute : Anagram.Grammars.Attribute);

   type Option_Generator (Context : AG_Tools.Contexts.Context_Access)
     is new Generator (Context) with null record;

   overriding procedure Make_Descent
     (Self : access Option_Generator;
      Part : Anagram.Grammars.Part_Index;
      Pass : Positive);

   overriding procedure Make_Get
     (Self      : access Option_Generator;
      Attribute : Anagram.Grammars.Attribute;
      Template  : Anagram.Grammars.Rule_Templates.Rule_Template);

   overriding procedure Make_Set
     (Self      : access Option_Generator;
      Attribute : Anagram.Grammars.Attribute);

   type Head_Generator (Context : AG_Tools.Contexts.Context_Access)
     is new Generator (Context) with null record;

   overriding procedure Make_Descent
     (Self : access Head_Generator;
      Part : Anagram.Grammars.Part_Index;
      Pass : Positive);

   overriding procedure Make_Set
     (Self      : access Head_Generator;
      Attribute : Anagram.Grammars.Attribute);

   overriding procedure Make_Get
     (Self      : access Head_Generator;
      Attribute : Anagram.Grammars.Attribute;
      Template  : Anagram.Grammars.Rule_Templates.Rule_Template);

end AG_Tools.Part_Generators;
