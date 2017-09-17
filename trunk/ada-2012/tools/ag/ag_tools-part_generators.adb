------------------------------------------------------------------------------
--                        G E L A   G R A M M A R S                         --
--          Library for dealing with grammars for Gela project,             --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with AG_Tools.Writers;
with Gela.Grammars.Ordered;

package body AG_Tools.Part_Generators is

   Tail : constant League.Strings.Universal_String :=
     League.Strings.To_Universal_String ("tail");
   This : constant League.Strings.Universal_String :=
     League.Strings.To_Universal_String ("This");

   function Origin
     (G         : Gela.Grammars.Grammar;
      Attribute : Gela.Grammars.Attribute)
      return League.Strings.Universal_String;

   ------------------
   -- Make_Descent --
   ------------------

   overriding procedure Make_Descent
     (Self : access Generator;
      Part : Gela.Grammars.Part_Index;
      Pass : Positive)
   is
      P    : Gela.Grammars.Part renames Self.Context.Grammar.Part (Part);
      NT   : Gela.Grammars.Non_Terminal renames
        Self.Context.Grammar.Non_Terminal (P.Denote);
      Code : AG_Tools.Writers.Writer renames Self.Context.Code;
   begin
      Generator'Class (Self.all).Make_Local_Variable (Part);

      Code.N ("      ");
      Code.N (To_Ada (NT.Name));
      Code.N ("_");
      Code.N (Pass);
      Code.P;
      Code.N ("        (Self, ");
      Code.N (To_Ada (P.Name));
      Code.P (");");
   end Make_Descent;

   ------------------
   -- Make_Descent --
   ------------------

   overriding procedure Make_Descent
     (Self : access Option_Generator;
      Part : Gela.Grammars.Part_Index;
      Pass : Positive)
   is
      P    : Gela.Grammars.Part renames Self.Context.Grammar.Part (Part);
      Code : AG_Tools.Writers.Writer renames Self.Context.Code;
   begin
      Self.Make_Local_Variable (Part);

      Code.N ("      if Assigned (");
      Code.N (To_Ada (P.Name));
      Code.P (") then");
      Generator (Self.all).Make_Descent (Part, Pass);
      Code.P ("      end if;");
   end Make_Descent;

   ------------------
   -- Make_Descent --
   ------------------

   overriding procedure Make_Descent
     (Self : access List_Generator;
      Part : Gela.Grammars.Part_Index;
      Pass : Positive)
   is
      G    : Gela.Grammars.Grammar renames Self.Context.Grammar.all;
      P    : Gela.Grammars.Part renames Self.Context.Grammar.Part (Part);
      Code : AG_Tools.Writers.Writer renames Self.Context.Code;
      NT   : Gela.Grammars.Non_Terminal renames G.Non_Terminal (P.Denote);
      Parts : Gela.Grammars.Ordered.Partition_Array renames
        Self.Context.Partition.all (NT.First_Attribute .. NT.Last_Attribute);
   begin
      Self.Make_Local_Variable (Part);
      Code.N ("   --  Make_Descent LIST ");
      Code.N (Pass);
      Code.P;
      Code.N ("      ");
      Code.N (To_Ada (P.Name));
      Code.N ("_");
      Code.N (Pass);
      Code.N (" (Self, ");
      Code.N (To_Ada (P.Name));

      for J in NT.First_Attribute .. NT.Last_Attribute loop
         if Gela.Grammars.Ordered.To_Pass (Parts, J) = Pass then
            Self.Make_Local_Variable
              (P.Name, G.Declaration (J));

            Code.N (", ");
            Code.N (To_Ada (P.Name));
            Code.N ("_");
            Code.N (To_Ada (G.Declaration (J).Name));
         end if;
      end loop;

      Code.P (");");
   end Make_Descent;

   ------------------
   -- Make_Descent --
   ------------------

   overriding procedure Make_Descent
     (Self : access Head_Generator;
      Part : Gela.Grammars.Part_Index;
      Pass : Positive)
   is
      G    : Gela.Grammars.Grammar renames Self.Context.Grammar.all;
      P    : Gela.Grammars.Part renames Self.Context.Grammar.Part (Part);
      Code : AG_Tools.Writers.Writer renames Self.Context.Code;
      NT   : Gela.Grammars.Non_Terminal renames G.Non_Terminal (P.Denote);
      Parts : Gela.Grammars.Ordered.Partition_Array renames
        Self.Context.Partition.all (NT.First_Attribute .. NT.Last_Attribute);
   begin
--      Self.Make_Local_Variable (Part);
      Code.N ("   --  Make_Descent HEAD ");
      Code.N (Pass);
      Code.P;
      Code.N ("      Descent");
      Code.N (" (This");

      for J in NT.First_Attribute .. NT.Last_Attribute loop
         if Gela.Grammars.Ordered.To_Pass (Parts, J) = Pass then
            Self.Make_Local_Variable
              (P.Name, G.Declaration (J));

            Code.N (", ");
            Code.N (To_Ada (P.Name));
            Code.N ("_");
            Code.N (To_Ada (G.Declaration (J).Name));
         end if;
      end loop;

      Code.P (");");
   end Make_Descent;

   --------------
   -- Make_Get --
   --------------

   overriding procedure Make_Get
     (Self      : access Generator;
      Attribute : Gela.Grammars.Attribute;
      Template  : Gela.Grammars.Rule_Templates.Rule_Template)
   is
      pragma Unreferenced (Template);
      G    : Gela.Grammars.Grammar renames Self.Context.Grammar.all;
      Origin : League.Strings.Universal_String;
      Code   : AG_Tools.Writers.Writer renames Self.Context.Code;
      D      : Gela.Grammars.Attribute_Declaration renames
        G.Declaration (Attribute.Declaration);
   begin
      Generator'Class (Self.all).Make_Local_Variable (Attribute.Origin);
      Origin := To_Ada (G.Part (Attribute.Origin).Name);

      Code.N ("      ");
      Code.N (Origin);
      Code.N ("_");
      Code.N (To_Ada (D.Name));
      Code.P (" :=");
      Code.N ("        ");
      Code.N (Origin);
      if G.Part (Attribute.Origin).Is_Non_Terminal_Reference then
         Code.N (".");
         Code.N (To_Ada (D.Name));
      end if;

      Code.P (";");
      Generator'Class (Self.all).Make_Local_Variable (Origin, D);
   end Make_Get;

   --------------
   -- Make_Get --
   --------------

   overriding procedure Make_Get
     (Self      : access Head_Generator;
      Attribute : Gela.Grammars.Attribute;
      Template  : Gela.Grammars.Rule_Templates.Rule_Template)
   is
      pragma Unreferenced (Template);

      G    : Gela.Grammars.Grammar renames Self.Context.Grammar.all;
      Code : AG_Tools.Writers.Writer renames Self.Context.Code;
      D    : Gela.Grammars.Attribute_Declaration renames
        G.Declaration (Attribute.Declaration);
   begin
      Self.Make_Local_Variable (Tail, D);
      Code.P ("         --  Make_Get tail");
   end Make_Get;

   --------------
   -- Make_Get --
   --------------

   overriding procedure Make_Get
     (Self      : access List_Generator;
      Attribute : Gela.Grammars.Attribute;
      Template  : Gela.Grammars.Rule_Templates.Rule_Template)
   is
      pragma Unreferenced (Template);
      G      : Gela.Grammars.Grammar renames Self.Context.Grammar.all;
      Code   : AG_Tools.Writers.Writer renames Self.Context.Code;
   begin
      Code.P ("   --  Make_Get LIST");

      Self.Make_Local_Variable
        (Origin (G, Attribute),
         G.Declaration (Attribute.Declaration));
   end Make_Get;

   --------------
   -- Make_Get --
   --------------

   overriding procedure Make_Get
     (Self      : access Option_Generator;
      Attribute : Gela.Grammars.Attribute;
      Template  : Gela.Grammars.Rule_Templates.Rule_Template)
   is
      use type League.Strings.Universal_String;
      G      : Gela.Grammars.Grammar renames Self.Context.Grammar.all;
      Code   : AG_Tools.Writers.Writer renames Self.Context.Code;
      Part   : constant League.Strings.Universal_String :=
        G.Part (Attribute.Origin).Name;
      Origin : constant League.Strings.Universal_String := To_Ada (Part);
      Attr   : constant League.Strings.Universal_String :=
        G.Declaration (Attribute.Declaration).Name;
   begin
      Code.N ("      if Assigned (");
      Code.N (Origin);
      Code.P (") then");
      Generator (Self.all).Make_Get (Attribute, Template);

      for J in 1 .. Template.Count loop
         if Template.Part_Name (J) = Part and then
           Template.Attribute_Name (J) = Attr and then
           Template.Has_Default (J)
         then
            Code.P ("      else");
            Code.N ("      ");
            Code.N (Origin);
            Code.N ("_");
            Code.N (To_Ada (Attr));
            Code.N (" := ");
            Code.N (Template.Default (J));
            Code.P (";");
         end if;
      end loop;

      Code.P ("      end if;");
   end Make_Get;

   -------------------------
   -- Make_Local_Variable --
   -------------------------

   overriding procedure Make_Local_Variable
     (Self      : access Generator;
      Origin    : League.Strings.Universal_String;
      Attribute : Gela.Grammars.Attribute_Declaration)
   is
      Impl : AG_Tools.Writers.Writer renames Self.Context.Impl;
      Attr : constant AG_Tools.Contexts.Attr := (Origin, Attribute.Index);
   begin
      if Self.Context.Attr_Map.Contains (Attr) then
         return;
      else
         Self.Context.Attr_Map.Insert (Attr);
      end if;

      Impl.N ("      ");
      Impl.N (To_Ada (Origin));
      Impl.N ("_");
      Impl.N (To_Ada (Attribute.Name));
      Impl.N (" : ");
      Impl.N (To_Ada (Attribute.Type_Name));
      Impl.P (";");
      Self.Context.Add_With (Package_Name (To_Ada (Attribute.Type_Name)));
   end Make_Local_Variable;

   -------------------------
   -- Make_Local_Variable --
   -------------------------

   overriding procedure Make_Local_Variable
     (Self : access Generator;
      Part : Gela.Grammars.Part_Index)
   is
      G    : Gela.Grammars.Grammar renames Self.Context.Grammar.all;
      P    : Gela.Grammars.Part renames G.Part (Part);
      Impl : AG_Tools.Writers.Writer renames Self.Context.Impl;
      Prod : Gela.Grammars.Production renames G.Production (P.Parent);
      NT   : Gela.Grammars.Non_Terminal renames G.Non_Terminal (Prod.Parent);
      RT   : constant League.Strings.Universal_String := Return_Type (G, P);
   begin
      if not Self.Context.Part_Map (Part) then
         Self.Context.Part_Map (Part) := True;
         Self.Context.Add_With (Package_Name (RT));
         Impl.N ("      ");
         Impl.N (To_Ada (P.Name));

         if Is_Converted_List (G, NT) then
            Impl.N (" : ");
            Impl.N (Return_Type (G, P));
            Impl.P ("_Access;");
         else
            Impl.N (" : constant ");
            Impl.N (RT);
            if not P.Is_Terminal_Reference then
               Impl.N ("_Access");
            end if;
            Impl.P (" :=");
            Impl.N ("        ");
            Impl.N (This);
            Impl.N (".");
            Impl.N (To_Ada (P.Name));
            Impl.P (";");
         end if;
      end if;
   end Make_Local_Variable;

   --------------
   -- Make_Set --
   --------------

   overriding procedure Make_Set
     (Self      : access Generator;
      Attribute : Gela.Grammars.Attribute)
   is
      G      : Gela.Grammars.Grammar renames Self.Context.Grammar.all;
      Origin : League.Strings.Universal_String;
      Code   : AG_Tools.Writers.Writer renames Self.Context.Code;
      D      : Gela.Grammars.Attribute_Declaration renames
        G.Declaration (Attribute.Declaration);
   begin
      Generator'Class (Self.all).Make_Local_Variable (Attribute.Origin);
      Code.N ("      ");

      Origin := To_Ada (G.Part (Attribute.Origin).Name);
      Code.N (Origin);
      Code.N (".Set_");
      Code.P (To_Ada (D.Name));
      Code.N ("        (");
      Code.N (Origin);
      Code.N ("_");
      Code.N (To_Ada (D.Name));
      Code.P (");");
      Generator'Class (Self.all).Make_Local_Variable (Origin, D);
   end Make_Set;

   --------------
   -- Make_Set --
   --------------

   overriding procedure Make_Set
     (Self      : access Head_Generator;
      Attribute : Gela.Grammars.Attribute)
   is
      G      : Gela.Grammars.Grammar renames Self.Context.Grammar.all;
      Code   : AG_Tools.Writers.Writer renames Self.Context.Code;
      D      : Gela.Grammars.Attribute_Declaration renames
        G.Declaration (Attribute.Declaration);
   begin
      Self.Make_Local_Variable (Tail, D);
      Code.P ("         --  Make_Set tail");
   end Make_Set;

   --------------
   -- Make_Set --
   --------------

   overriding procedure Make_Set
     (Self      : access List_Generator;
      Attribute : Gela.Grammars.Attribute)
   is
      G      : Gela.Grammars.Grammar renames Self.Context.Grammar.all;
      Code   : AG_Tools.Writers.Writer renames Self.Context.Code;
   begin
      Code.P ("   --  Make_Set LIST");

      Self.Make_Local_Variable
        (Origin (G, Attribute),
         G.Declaration (Attribute.Declaration));
   end Make_Set;

   --------------
   -- Make_Set --
   --------------

   overriding procedure Make_Set
     (Self : access Option_Generator;
      Attribute : Gela.Grammars.Attribute)
   is
      G      : Gela.Grammars.Grammar renames Self.Context.Grammar.all;
      Origin : League.Strings.Universal_String;
      Code   : AG_Tools.Writers.Writer renames Self.Context.Code;
   begin
      Origin := To_Ada (G.Part (Attribute.Origin).Name);
      Code.N ("      if Assigned (");
      Code.N (Origin);
      Code.P (") then");
      Generator (Self.all).Make_Set (Attribute);
      Code.P ("      end if;");
   end Make_Set;

   ------------
   -- Origin --
   ------------

   function Origin
     (G         : Gela.Grammars.Grammar;
      Attribute : Gela.Grammars.Attribute)
      return League.Strings.Universal_String
   is
      Rule : Gela.Grammars.Rule renames G.Rule (Attribute.Parent);
      Prod : Gela.Grammars.Production renames G.Production (Rule.Parent);
      NT   : Gela.Grammars.Non_Terminal renames G.Non_Terminal (Prod.Parent);
   begin
      if Attribute.Is_Left_Hand_Side then
         return NT.Name;
      else
         return G.Part (Attribute.Origin).Name;
      end if;
   end Origin;

end AG_Tools.Part_Generators;
