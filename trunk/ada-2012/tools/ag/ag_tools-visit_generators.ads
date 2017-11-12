------------------------------------------------------------------------------
--                        G E L A   G R A M M A R S                         --
--          Library for dealing with grammars for Gela project,             --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with League.Strings;

with Anagram.Grammars.Ordered;
with Anagram.Grammars.Rule_Templates;

package AG_Tools.Visit_Generators is

   type Generator is limited interface;
   type Generator_Access is access all Generator'Class;

   not overriding procedure Make_Local_Variable
     (Self      : access Generator;
      Origin    : League.Strings.Universal_String;
      Attribute : Anagram.Grammars.Attribute_Declaration) is abstract;
   --  Generate local variable to store value of given Attribute

   not overriding procedure Make_Get
     (Self      : access Generator;
      Attribute : Anagram.Grammars.Attribute;
      Template  : Anagram.Grammars.Rule_Templates.Rule_Template) is abstract;
   --  Generate code to get/fetch value of given Attribute

   not overriding procedure Make_Set
     (Self      : access Generator;
      Attribute : Anagram.Grammars.Attribute) is abstract;
   --  Generate code to set/update value in given Attribute

   type Part_Generator is limited interface and Generator;
   type Part_Generator_Access is access all Part_Generator'Class;

   not overriding procedure Make_Descent
     (Self : access Part_Generator;
      Part : Anagram.Grammars.Part_Index;
      Pass : Positive) is abstract;
   --  Generate code to go deeper in the tree

   not overriding procedure Make_Local_Variable
     (Self : access Part_Generator;
      Part : Anagram.Grammars.Part_Index) is abstract;
   --  Generate local variable to store value of given Attribute

   type NT_Generator is limited interface and Generator;
   type NT_Generator_Access is access all NT_Generator'Class;

   not overriding procedure Make_Procedure
     (Self  : access NT_Generator;
      Order : Anagram.Grammars.Ordered.Order_Maps.Map;
      NT    : Anagram.Grammars.Non_Terminal;
      Pass  : Positive) is abstract;
   --  Generate procedure

   type Factory is limited interface;
   type Factory_Access is access all Factory'Class;

   not overriding function Get
     (Self : access Factory;
      NT   : Anagram.Grammars.Non_Terminal)
      return NT_Generator_Access is abstract;

   not overriding function Get
     (Self : access Factory;
      Part : Anagram.Grammars.Part)
      return Part_Generator_Access is abstract;

   not overriding function Get
     (Self : access Factory;
      Attr : Anagram.Grammars.Attribute;
      NT   : Anagram.Grammars.Non_Terminal)
      return AG_Tools.Visit_Generators.Generator_Access is abstract;

end AG_Tools.Visit_Generators;
