------------------------------------------------------------------------------
--                        G E L A   G R A M M A R S                         --
--          Library for dealing with grammars for Gela project,             --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with AG_Tools.Input;

package body AG_Tools.Generator_Factories is

   Tail : constant League.Strings.Universal_String :=
     League.Strings.To_Universal_String ("tail");

   ---------
   -- Get --
   ---------

   overriding function Get
     (Self : access Factory;
      NT   : Anagram.Grammars.Non_Terminal)
      return AG_Tools.Visit_Generators.NT_Generator_Access
   is
   begin
      if Is_Converted_List (Self.Context.Grammar.all, NT) then
         return Self.List'Access;
      elsif AG_Tools.Input.Is_Concrete (NT.Index) then
         return Self.NT'Access;
      else
         return Self.Abst'Access;
      end if;
   end Get;

   ---------
   -- Get --
   ---------

   overriding function Get
     (Self : access Factory;
      Part : Anagram.Grammars.Part)
      return AG_Tools.Visit_Generators.Part_Generator_Access
   is
      use type League.Strings.Universal_String;
      G    : Anagram.Grammars.Grammar renames Self.Context.Grammar.all;
   begin
      if Part.Name = Tail then
         return Self.Head'Access;
      elsif not Part.Is_Terminal_Reference
        and then Is_Converted_List (G, G.Non_Terminal (Part.Denote))
      then
         return Self.Seq'Access;
      elsif AG_Tools.Input.Is_Option (G, Part) then
         return Self.Opt'Access;
      else
         return Self.Part'Access;
      end if;
   end Get;

   ---------
   -- Get --
   ---------

   overriding function Get
     (Self : access Factory;
      Attr : Anagram.Grammars.Attribute;
      NT   : Anagram.Grammars.Non_Terminal)
      return AG_Tools.Visit_Generators.Generator_Access
   is

   begin
      if Attr.Is_Left_Hand_Side then
         return AG_Tools.Visit_Generators.Generator_Access (Self.Get (NT));
      else
         return AG_Tools.Visit_Generators.Generator_Access
           (Self.Get (Self.Context.Grammar.Part (Attr.Origin)));
      end if;
   end Get;

end AG_Tools.Generator_Factories;
