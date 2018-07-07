------------------------------------------------------------------------------
--                        G E L A   G R A M M A R S                         --
--          Library for dealing with grammars for Gela project,             --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with Anagram.Grammars.Ordered;

with AG_Tools.Contexts;
with AG_Tools.Visit_Generators;
with Anagram.Grammars.Rule_Templates;

package AG_Tools.NT_Generators is

   type Generator (Context : AG_Tools.Contexts.Context_Access)
     is new AG_Tools.Visit_Generators.NT_Generator with null record;

   overriding procedure Make_Procedure
     (Self  : access Generator;
      Order : Anagram.Grammars.Ordered.Order_Maps.Map;
      NT    : Anagram.Grammars.Non_Terminal;
      Pass  : Positive);

   overriding procedure Make_Local_Variable
     (Self      : access Generator;
      Origin    : League.Strings.Universal_String;
      Attribute : Anagram.Grammars.Attribute_Declaration);

   overriding procedure Make_Get
     (Self      : access Generator;
      Attribute : Anagram.Grammars.Attribute;
      Template  : Anagram.Grammars.Rule_Templates.Rule_Template);

   overriding procedure Make_Set
     (Self      : access Generator;
      Attribute : Anagram.Grammars.Attribute);

   type List_Generator (Context : AG_Tools.Contexts.Context_Access)
     is new AG_Tools.Visit_Generators.NT_Generator with null record;

   overriding procedure Make_Procedure
     (Self  : access List_Generator;
      Order : Anagram.Grammars.Ordered.Order_Maps.Map;
      NT    : Anagram.Grammars.Non_Terminal;
      Pass  : Positive);

   overriding procedure Make_Local_Variable
     (Self      : access List_Generator;
      Origin    : League.Strings.Universal_String;
      Attribute : Anagram.Grammars.Attribute_Declaration);

   overriding procedure Make_Get
     (Self      : access List_Generator;
      Attribute : Anagram.Grammars.Attribute;
      Template  : Anagram.Grammars.Rule_Templates.Rule_Template);

   overriding procedure Make_Set
     (Self      : access List_Generator;
      Attribute : Anagram.Grammars.Attribute);

   type Abstract_Generator (Context : AG_Tools.Contexts.Context_Access)
   is new AG_Tools.Visit_Generators.NT_Generator with null record;

   overriding procedure Make_Procedure
     (Self  : access Abstract_Generator;
      Order : Anagram.Grammars.Ordered.Order_Maps.Map;
      NT    : Anagram.Grammars.Non_Terminal;
      Pass  : Positive);

   overriding procedure Make_Local_Variable
     (Self      : access Abstract_Generator;
      Origin    : League.Strings.Universal_String;
      Attribute : Anagram.Grammars.Attribute_Declaration) is null;

   overriding procedure Make_Get
     (Self      : access Abstract_Generator;
      Attribute : Anagram.Grammars.Attribute;
      Template  : Anagram.Grammars.Rule_Templates.Rule_Template) is null;

   overriding procedure Make_Set
     (Self      : access Abstract_Generator;
      Attribute : Anagram.Grammars.Attribute) is null;

end AG_Tools.NT_Generators;
