------------------------------------------------------------------------------
--                        G E L A   G R A M M A R S                         --
--          Library for dealing with grammars for Gela project,             --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with AG_Tools.Input;

package body AG_Tools.Generator_Fabrics is

   ---------
   -- Get --
   ---------

   overriding function Get
     (Self : access Fabric;
      NT   : Gela.Grammars.Non_Terminal)
      return AG_Tools.Visit_Generators.NT_Generator_Access
   is
   begin
      if Is_Converted_List (Self.Context.Grammar.all, NT) then
         return Self.List'Access;
      else
         return Self.NT'Access;
      end if;
   end Get;

   ---------
   -- Get --
   ---------

   overriding function Get
     (Self : access Fabric;
      Part : Gela.Grammars.Part)
      return AG_Tools.Visit_Generators.Part_Generator_Access
   is
      G    : Gela.Grammars.Grammar renames Self.Context.Grammar.all;
   begin
      if not Part.Is_Terminal_Reference
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
     (Self : access Fabric;
      Attr : Gela.Grammars.Attribute;
      NT   : Gela.Grammars.Non_Terminal)
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

end AG_Tools.Generator_Fabrics;
