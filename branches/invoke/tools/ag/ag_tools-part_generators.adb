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

   Head : constant League.Strings.Universal_String :=
     League.Strings.To_Universal_String ("head");
   This : constant League.Strings.Universal_String :=
     League.Strings.To_Universal_String ("This");

   ------------------
   -- Make_Descent --
   ------------------

   overriding procedure Make_Descent
     (Self : access Generator;
      Part : Gela.Grammars.Part_Index;
      Pass : Positive)
   is
      pragma Unreferenced (Pass);
      P    : Gela.Grammars.Part renames Self.Context.Grammar.Part (Part);
      Code : AG_Tools.Writers.Writer renames Self.Context.Code;
   begin
      Self.Make_Local_Variable (Part);

      Code.N ("      ");
      Code.N (To_Ada (P.Name));
      Code.P (".its.Visit");
      Code.N ("        (");
      Code.N (To_Ada (P.Name));
      Code.P (".Payload, Self);");
   end Make_Descent;

   ------------------
   -- Make_Descent --
   ------------------

   overriding procedure Make_Descent
     (Self : access Option_Generator;
      Part : Gela.Grammars.Part_Index;
      Pass : Positive)
   is
      pragma Unreferenced (Pass);
      P    : Gela.Grammars.Part renames Self.Context.Grammar.Part (Part);
      Code : AG_Tools.Writers.Writer renames Self.Context.Code;
   begin
      Self.Make_Local_Variable (Part);

      Code.N ("      if ");
      Code.N (To_Ada (P.Name));
      Code.P (".its /= null then");
      Code.N ("         ");
      Code.N (To_Ada (P.Name));
      Code.P (".its.Visit");
      Code.N ("           (");
      Code.N (To_Ada (P.Name));
      Code.P (".Payload, Self);");
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
      use type League.Strings.Universal_String;
      G    : Gela.Grammars.Grammar renames Self.Context.Grammar.all;
      P    : Gela.Grammars.Part renames Self.Context.Grammar.Part (Part);
      Code : AG_Tools.Writers.Writer renames Self.Context.Code;
      NT   : Gela.Grammars.Non_Terminal renames G.Non_Terminal (P.Denote);
      Parts : Gela.Grammars.Ordered.Partition_Array renames
        Self.Context.Partition.all;
   begin
      if P.Name = Head then
         --  Dont descent into list's head
         return;
      end if;

      Self.Make_Local_Variable (Part);
      Code.N ("      ");
      Code.N ("Self.");
      Code.N (To_Ada (P.Name));
      Code.N (" (");
      Code.N (To_Ada (P.Name));

      for J in NT.First_Attribute .. NT.Last_Attribute loop
         if Gela.Grammars.Ordered.To_Pass (Parts, J) = Pass then
            Self.Make_Local_Variable (P.Name, G.Declaration (J));

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
      Attribute : Gela.Grammars.Attribute)
   is
      G    : Gela.Grammars.Grammar renames Self.Context.Grammar.all;
      Origin : League.Strings.Universal_String;
      Code   : AG_Tools.Writers.Writer renames Self.Context.Code;
      D      : Gela.Grammars.Attribute_Declaration renames
        G.Declaration (Attribute.Declaration);
   begin
      Self.Make_Local_Variable (Attribute.Origin);
      Origin := To_Ada (G.Part (Attribute.Origin).Name);

      Code.N ("      ");
      Code.N (Origin);
      Code.N ("_");
      Code.N (To_Ada (D.Name));
      Code.P (" :=");
      Code.N ("        ");
      Code.N (Origin);
      Code.N (".its.");
      Code.N (To_Ada (D.Name));
      Code.N (" (");
      Code.N (Origin);
      Code.P (".Payload);");
      Self.Make_Local_Variable (Origin, D);
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
   begin
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
   begin
      if not Self.Context.Part_Map (Part) then
         Self.Context.Part_Map (Part) := True;
         Impl.N ("      ");
         Impl.N (To_Ada (P.Name));

         if Is_Converted_List (G, NT) then
            Impl.N (" : Gela.Nodes.");
            Impl.N (Return_Type (G, P));
            Impl.P (" :=");
            Impl.P ("        This.Head (Node.Payload);");
         else
            Impl.N (" : constant Gela.Nodes.");
            Impl.N (Return_Type (G, P));
            Impl.P (" :=");
            Impl.N ("        ");
            Impl.N (This);
            Impl.N (".");
            Impl.N (To_Ada (P.Name));
            Impl.P (" (Node.Payload);");
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
      Self.Make_Local_Variable (Attribute.Origin);
      Code.N ("      ");

      Origin := To_Ada (G.Part (Attribute.Origin).Name);
      Code.N (Origin);
      Code.N (".its.Set_");
      Code.P (To_Ada (D.Name));
      Code.N ("        (");
      Code.N (Origin);

      Code.N (".Payload, ");
      Code.N (Origin);
      Code.N ("_");
      Code.N (To_Ada (D.Name));
      Code.P (");");
   end Make_Set;

end AG_Tools.Part_Generators;
