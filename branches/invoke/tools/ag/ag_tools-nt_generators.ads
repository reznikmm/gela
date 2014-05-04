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
     (Self  : access Generator;
      Order : Gela.Grammars.Ordered.Order_Maps.Map;
      NT    : Gela.Grammars.Non_Terminal;
      Pass  : Positive);

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
     (Self  : access List_Generator;
      Order : Gela.Grammars.Ordered.Order_Maps.Map;
      NT    : Gela.Grammars.Non_Terminal;
      Pass  : Positive);

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

   type Abstract_Generator (Context : AG_Tools.Contexts.Context_Access)
   is new AG_Tools.Visit_Generators.NT_Generator with null record;

   overriding procedure Make_Procedure
     (Self  : access Abstract_Generator;
      Order : Gela.Grammars.Ordered.Order_Maps.Map;
      NT    : Gela.Grammars.Non_Terminal;
      Pass  : Positive);

   overriding procedure Make_Local_Variable
     (Self      : access Abstract_Generator;
      Origin    : League.Strings.Universal_String;
      Attribute : Gela.Grammars.Attribute_Declaration) is null;

   overriding procedure Make_Get
     (Self      : access Abstract_Generator;
      Attribute : Gela.Grammars.Attribute) is null;

   overriding procedure Make_Set
     (Self      : access Abstract_Generator;
      Attribute : Gela.Grammars.Attribute) is null;

end AG_Tools.NT_Generators;
