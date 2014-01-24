------------------------------------------------------------------------------
--                        G E L A   G R A M M A R S                         --
--          Library for dealing with grammars for Gela project,             --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with Ada.Command_Line;
with Ada.Text_IO;

with League.Strings;
with League.String_Vectors;

with Gela.Grammars;

with AG_Tools; use AG_Tools;
with AG_Tools.Writers; use AG_Tools.Writers;
--  with Gela.Grammars_Debug;
with AG_Tools.Input;
with AG_Tools.Element_Generators;

procedure AG_Driver is
   use AG_Tools.Input;

   use type Gela.Grammars.Production_Index;
   use type Gela.Grammars.Part_Count;
   use type Gela.Grammars.Non_Terminal_Count;
   use type League.Strings.Universal_String;

   function Return_Type
     (Part : Gela.Grammars.Part)
      return League.Strings.Universal_String;
   --  Get identifier of type of given Part

   function Non_Terminal_Unit
     (NT : Gela.Grammars.Non_Terminal)
      return League.Strings.Universal_String;
   --  Return Gela.Nodes.xxx unit corresponding to NT

   procedure Write_Attr_With
     (NT     : Gela.Grammars.Non_Terminal;
      Output : in out Writer;
      Done   : in out League.String_Vectors.Universal_String_Vector);

   procedure Write_Attr_Get
     (Decl   : Gela.Grammars.Attribute_Declaration;
      Output : in out Writer;
      Impl   : Boolean);

   procedure Write_Attr_Set
     (Decl   : Gela.Grammars.Attribute_Declaration;
      Output : in out Writer;
      Impl   : Boolean);

   procedure Generate_Fabric;
   procedure Generate_Stores_Prod
     (NT   : Gela.Grammars.Non_Terminal;
      Prod : Gela.Grammars.Production);
   procedure Generate_Stores_List (NT : Gela.Grammars.Non_Terminal);
   procedure Generate_Visiter;
   procedure Generate_2;

   Name  : constant String := Ada.Command_Line.Argument (1);
   G     : Gela.Grammars.Grammar_Access;

   Reserved : constant := 3;  --  Tag, Count

   Offset      : Natural;

   procedure Generate_2 is
      Fab_With    : Writer;
      Fab_Kind    : Writer;
      Fab_When    : Writer;
      Fabrics     : Writer;
      Impl        : Writer;
   begin
      Fabrics.P ("with Gela.Lexical_Types;");
      Fabrics.P ("with Gela.Elements;");
      Fabrics.P;
      Fabrics.P ("package Gela.LARL_Parsers_Nodes is");
      Impl.P ("package body Gela.LARL_Parsers_Nodes is");
      Fabrics.P ("   pragma Preelaborate;");
      Fabrics.P;
      Fabrics.P ("   type Node is private;");
      Fabrics.P ("   type Node_Array is array (Positive range <>) of Node;");
      Fabrics.P;
      Fabrics.P ("   None     : constant Node;");
      Fabrics.P ("   No_Token : constant Node;");
      Fabrics.P;
      Fabrics.P ("   No_Token_Index : constant " &
                   "Gela.Lexical_Types.Token_Count := 0;");
      Fabrics.P;
      Fabrics.N ("   function ""-"" (X : Node)" &
                   " return Gela.Lexical_Types.Token_Count", Impl);
      Fabrics.P (";");
      Impl.P (" is");
      Impl.P ("   begin");
      Impl.P ("      return X.Token;");
      Impl.P ("   end ""-"";");
      Impl.P;

      Fabrics.N ("   function ""-"" (X : Node)" &
                   " return access Gela.Elements.Element'Class", Impl);
      Fabrics.P (";");
      Impl.P (" is");
      Impl.P ("   begin");
      Impl.P ("      return X.Element;");
      Impl.P ("   end ""-"";");
      Impl.P;

      Fabrics.N ("   function ""+"" (X : Gela.Lexical_Types.Token_Count)" &
                   " return Node", Impl);
      Fabrics.P (";");
      Impl.P (" is");
      Impl.P ("   begin");
      Impl.P ("      return (Token, X);");
      Impl.P ("   end ""+"";");
      Impl.P;

      Fabrics.N ("   function ""+"" (X : access Gela.Elements.Element'Class)" &
                   " return Node", Impl);
      Fabrics.P (";");
      Impl.P (" is");
      Impl.P ("   begin");
      Impl.P ("      return (Element, X);");
      Impl.P ("   end ""+"";");
      Impl.P;

      Fab_Kind.P ("   type Node_Kinds is");
      Fab_Kind.P ("     (Token,");
      Fab_Kind.N ("      Element");

      for NT of G.Non_Terminal loop
         if not NT.Is_List then
            if Has_List (NT.Index) then
               Fab_With.N ("with Gela.Elements.");
               Fab_With.N (To_Ada (Plural (NT.Name)));
               Fab_With.P (";");

               Fab_Kind.P (",");
               Fab_Kind.N ("      ");
               Fab_Kind.N (To_Ada (NT.Name));
               Fab_Kind.N ("_Sequence");

               Fabrics.P ("   function ""-"" (X : Node) return", Impl);
               Fabrics.N ("     Gela.Elements.", Impl);
               Fabrics.N (To_Ada (Plural (NT.Name)), Impl);
               Fabrics.N (".", Impl);
               Fabrics.N (To_Ada (NT.Name), Impl);
               Fabrics.N ("_Sequence_Access", Impl);
               Fabrics.P (";");
               Impl.P (" is");
               Impl.P ("   begin");
               Impl.P ("      if X = None then");
               Impl.P ("         return null;");
               Impl.P ("      end if;");
               Impl.N ("      return X.");
               Impl.N (To_Ada (NT.Name));
               Impl.P ("_Sequence;");
               Impl.P ("   end ""-"";");
               Impl.P;

               Fabrics.P ("   function ""+""", Impl);
               Fabrics.N ("     (X : Gela.Elements.", Impl);
               Fabrics.N (To_Ada (Plural (NT.Name)), Impl);
               Fabrics.N (".", Impl);
               Fabrics.N (To_Ada (NT.Name), Impl);
               Fabrics.P ("_Sequence_Access)", Impl);
               Fabrics.N ("     return Node", Impl);
               Fabrics.P (";");
               Impl.P (" is");
               Impl.P ("   begin");
               Impl.N ("      return (");
               Impl.N (To_Ada (NT.Name));
               Impl.P ("_Sequence, X);");
               Impl.P ("   end ""+"";");
               Impl.P;

               Fab_When.N ("         when ");
               Fab_When.N (To_Ada (NT.Name));
               Fab_When.P ("_Sequence =>");
               Fab_When.N ("            ");
               Fab_When.N (To_Ada (NT.Name));
               Fab_When.P ("_Sequence :");
               Fab_When.N ("              Gela.Elements.");
               Fab_When.N (To_Ada (Plural (NT.Name)));
               Fab_When.N (".");
               Fab_When.N (To_Ada (NT.Name));
               Fab_When.P ("_Sequence_Access;");

            end if;
         end if;
      end loop;

      Fabrics.P;
      Fabrics.P ("private");
      Fabrics.P;
      Fabrics.N (Fab_Kind.Text);
      Fabrics.P (");");
      Fabrics.P;
      Fabrics.P ("   type Node (Kind : Node_Kinds := Token) is record");
      Fabrics.P ("      case Kind is");
      Fabrics.P ("         when Token =>");
      Fabrics.P ("            Token : Gela.Lexical_Types.Token_Count;");
      Fabrics.P ("         when Element =>");
      Fabrics.P ("            Element : Gela.Elements.Element_Access;");

      Ada.Text_IO.Put_Line (Fab_With.Text.To_UTF_8_String);
      Ada.Text_IO.Put_Line (Fabrics.Text.To_UTF_8_String);
      Ada.Text_IO.Put_Line (Fab_When.Text.To_UTF_8_String);
      Fabrics.Clear;

      Fabrics.P ("      end case;");
      Fabrics.P ("   end record;");
      Fabrics.P;
      Fabrics.P ("   None     : constant Node := (Element, null);");
      Fabrics.P ("   No_Token : constant Node := (Token, 0);");
      Fabrics.P;
      Fabrics.N ("end Gela.LARL_Parsers_Nodes;", Impl);

      Ada.Text_IO.Put_Line (Fabrics.Text.To_UTF_8_String);
      Ada.Text_IO.Put_Line (Impl.Text.To_UTF_8_String);
   end Generate_2;

   ---------------------
   -- Generate_Fabric --
   ---------------------

   procedure Generate_Fabric is
      Fab_With    : Writer;
      Body_With   : Writer;
      Fab_Body    : Writer;
      Fab_Init    : Writer;
      Fabrics     : Writer;
      Lists       : Natural := 0;
--      List_Attrs  : Writer;
   begin
--      Fabrics.P ("with Gela.Mutables;");
      Fabrics.P ("with Gela.Nodes;");
      Fabrics.P ("with Gela.Stores.Tokens;");
      Fabrics.P;
      Fabrics.P ("package Gela.Stores.Base_Fabrics is");
      Fabrics.P ("   pragma Preelaborate;");
      Fabrics.P;
      Fabrics.P ("   type Node_Array is array (Natural range <>) of" &
                   " Gela.Nodes.Node_Access;");
      Fabrics.P;
      Fabrics.P ("   type Base_Fabric (Store : not null Store_Access) is");
      Fabrics.P ("   abstract new Abstract_Fabric with record");
      Fabrics.P ("      Token : aliased Tokens.Token (Store);");

      Fab_Body.P ("package body Gela.Stores.Base_Fabrics is");
      Fab_Body.P ("   pragma Style_Checks (""-o"");");
      Fab_Body.P ("   package N renames Gela.Nodes;");
      Fab_Body.P;
      Fab_Init.P ("   procedure Initialize (Self : access Base_Fabric) is");
      Fab_Init.P ("   begin");
      Fab_Init.P ("      Self.Map :=");
      Fab_Init.N ("        (0 => Self.Token'Access");

      for NT of G.Non_Terminal loop
         if not NT.Is_List then
            if Has_List (NT.Index) then
               Fab_With.N ("with Gela.Stores.");
               Fab_With.N (To_Ada (NT.Name));
               Fab_With.P ("_Sequences;");

               Fabrics.N ("      L");
               Fabrics.N (Positive (NT.Index));
               Fabrics.N (" : aliased ");
               Fabrics.N (To_Ada (NT.Name));
               Fabrics.P ("_Sequences.List (Store);");

               Lists := Lists + 1;
               Fab_Init.P (",");
               Fab_Init.N ("         Last_Production + ");
               Fab_Init.N (Lists);
               Fab_Init.N (" => Self.L");
               Fab_Init.N (Positive (NT.Index));
               Fab_Init.N ("'Access");
            end if;

            for Prod of G.Production (NT.First .. NT.Last) loop
               if Is_Concrete (NT.Index) then
                  Fab_With.N ("with Gela.Stores.");
                  Fab_With.N (Plural (NT.Name));
                  Fab_With.P (";");

                  Fabrics.N ("      P");
                  Fabrics.N (Positive (Prod.Index));
                  Fabrics.N (" : aliased ");
                  Fabrics.N (Plural (NT.Name));
                  Fabrics.P (".Object (Store);");

                  Fab_Init.P (",");
                  Fab_Init.N ("         ");
                  Fab_Init.N (Positive (Prod.Index));
                  Fab_Init.N (" => Self.P");
                  Fab_Init.N (Positive (Prod.Index));
                  Fab_Init.N ("'Access");
               end if;
            end loop;
         end if;
      end loop;

      Fabrics.N ("      Map : Node_Array (0 .. ");
      Fabrics.N (Positive (G.Last_Production) + Lists);
      Fabrics.P (");");
      Fabrics.P ("   end record;");
      Fabrics.P;

      Lists := 0;

      for NT of G.Non_Terminal loop
         if not NT.Is_List then
            Body_With.N ("with Gela.Nodes.");
            Body_With.N (Plural (NT.Name));
            Body_With.P (";");
         end if;

         if Has_List (NT.Index) then
            Lists := Lists + 1;

            Fabrics.N ("   function ", Fab_Body);
            Fabrics.N (To_Ada (NT.Name), Fab_Body);
            Fabrics.P ("_Sequence", Fab_Body);
            Fabrics.P ("     (Self : access Base_Fabric'Class)", Fab_Body);
            Fabrics.N ("      return Gela.Nodes.", Fab_Body);
            Fabrics.N (To_Ada (NT.Name), Fab_Body);
            Fabrics.N ("_Sequence", Fab_Body);
            Fabrics.P (";");
            Fab_Body.P;
            Fab_Body.P ("   is");
            Fab_Body.N ("      Object : constant Stores.");
            Fab_Body.N (To_Ada (NT.Name));
            Fab_Body.P ("_Sequences");
            Fab_Body.N ("        .List_Access := Self.L");
            Fab_Body.N (Positive (NT.Index));
            Fab_Body.P ("'Access;");
            Fab_Body.N ("      Int    : constant N.");
            Fab_Body.N (Plural (NT.Name));
            Fab_Body.P (".List_Access :=");
            Fab_Body.N ("        N.");
            Fab_Body.N (Plural (NT.Name));
            Fab_Body.P (".List_Access (Object);");
            Fab_Body.N ("      Result : constant N.");
            Fab_Body.N (To_Ada (NT.Name));
            Fab_Body.P ("_Sequence :=");
            Fab_Body.N
              ("        (Payload => Self.Create_Sequence (Last_Production + ");
            Fab_Body.N (Lists);
            Fab_Body.P ("),");
            Fab_Body.N ("         its => N.");
            Fab_Body.N (To_Ada (NT.Name));
            Fab_Body.P ("_Sequence_Access (Int));");
            Fab_Body.P ("   begin");
            Fab_Body.P ("      return Result;");
            Fab_Body.N ("   end ");
            Fab_Body.N (To_Ada (NT.Name));
            Fab_Body.P ("_Sequence;");
            Fabrics.P ("", Fab_Body);
         end if;

         for Prod of G.Production (NT.First .. NT.Last) loop
            declare
               Name : League.Strings.Universal_String;
--                 M : constant Gela.Grammars.Non_Terminal_Count :=
--                   Macro_Reference (Prod);
            begin
               if Is_Concrete (NT.Index) and not NT.Is_List then
                  Fabrics.N ("   function ", Fab_Body);
                  Name := To_Ada (NT.Name);

                  Fabrics.P (Name, Fab_Body);
                  Fabrics.N
                    ("     (Self : access Base_Fabric'Class", Fab_Body);

                  for Part of G.Part (Prod.First .. Prod.Last) loop
                     Fabrics.P (";", Fab_Body);
                     Fabrics.N ("      ", Fab_Body);
                     Fabrics.N (To_Ada (Part.Name), Fab_Body);
                     Fabrics.N (" : Gela.Nodes.", Fab_Body);

                     if Part.Name.Length + Return_Type (Part).Length > 58 then
                        Fabrics.P ("", Fab_Body);
                        Fabrics.N ("        ", Fab_Body);
                     end if;

                     Fabrics.N (Return_Type (Part), Fab_Body);
                  end loop;

                  Fabrics.P (")", Fab_Body);
                  Fabrics.N ("      return Gela.Nodes.", Fab_Body);
                  Fabrics.N (To_Ada (NT.Name), Fab_Body);
                  Fabrics.P (";");
                  Fabrics.P ("", Fab_Body);

                  Fab_Body.P ("   is");
                  Fab_Body.N ("      Object : constant Stores.");
                  Fab_Body.P (Plural (NT.Name));
                  Fab_Body.N ("        .Object_Access := Self.P");
                  Fab_Body.N (Natural (Prod.Index));
                  Fab_Body.P ("'Access;");
                  Fab_Body.P ("      Int    : constant");
                  Fab_Body.N ("        N.");
                  Fab_Body.N (Plural (NT.Name));
                  Fab_Body.P (".Object_Access :=");
                  Fab_Body.N ("        N.");
                  Fab_Body.N (Plural (NT.Name));
                  Fab_Body.P (".Object_Access (Object);");
                  Fab_Body.N ("      Result : constant N.");
                  Fab_Body.N (To_Ada (NT.Name));
                  Fab_Body.P (" :=");
                  Fab_Body.N ("        (Payload => Self.Create_Production (");
                  Fab_Body.N (Natural (Prod.Index));
                  Fab_Body.P ("),");
                  Fab_Body.N ("         its => N.");
                  Fab_Body.N (To_Ada (NT.Name));
                  Fab_Body.P ("_Access (Int));");
                  Fab_Body.P ("   begin");

                  Offset := Positive (Prod.First) - 1;

                  for Part of G.Part (Prod.First .. Prod.Last) loop
                     Fab_Body.N ("      Self.P");
                     Fab_Body.N (Natural (Prod.Index));
                     Fab_Body.P (".Set_Child");
                     Fab_Body.N ("        (Result.Payload, ");
                     Fab_Body.N (Positive (Part.Index) - Offset);
                     Fab_Body.N (", ");
                     Fab_Body.N (To_Ada (Part.Name));
                     Fab_Body.P (".Payload);");
                  end loop;

                  Fab_Body.P ("      return Result;");
                  Fab_Body.N ("   end ");
                  Fab_Body.N (Name);
                  Fab_Body.P (";");
                  Fab_Body.P;
               end if;
            end;
         end loop;
      end loop;

      Fabrics.N ("   Last_Production : constant := ");
      Fabrics.N (Positive (G.Last_Production));
      Fabrics.P (";");
      Fabrics.P;

      Fabrics.P ("   procedure Initialize (Self : access Base_Fabric);");
      Fabrics.P;
      Fabrics.N ("end Gela.Stores.Base_Fabrics;");

      --   XXX:
      Fab_Init.P (",");
      Fab_Init.N ("         others => null");

      Fab_Init.P (");");
      Fab_Init.P ("   end Initialize;");
      Fab_Body.N (Fab_Init.Text);
      Fab_Body.P;
      Fab_Body.N ("end Gela.Stores.Base_Fabrics;");

      Ada.Text_IO.Put_Line (Fab_With.Text.To_UTF_8_String);
      Ada.Text_IO.Put_Line (Fabrics.Text.To_UTF_8_String);
      Ada.Text_IO.Put_Line (Body_With.Text.To_UTF_8_String);
      Ada.Text_IO.Put (Fab_Body.Text.To_UTF_8_String);
   end Generate_Fabric;

   -----------------
   -- Return_Type --
   -----------------

   function Return_Type
     (Part : Gela.Grammars.Part)
      return League.Strings.Universal_String is
   begin
      return Return_Type (G.all, Part);
   end Return_Type;

   ---------------------
   -- Write_Attr_With --
   ---------------------

   procedure Write_Attr_With
     (NT     : Gela.Grammars.Non_Terminal;
      Output : in out Writer;
      Done   : in out League.String_Vectors.Universal_String_Vector)
   is
      Pkg_Name   : League.Strings.Universal_String;
   begin
      for A in NT.First_Attribute .. NT.Last_Attribute loop
         Pkg_Name := Package_Name (G.Declaration (A).Type_Name);

         if Done.Index (Pkg_Name) = 0 then
            Done.Append (Pkg_Name);
            Output.N ("with ");
            Output.N (Pkg_Name);
            Output.P (";");
         end if;
      end loop;
   end Write_Attr_With;

   --------------------
   -- Write_Attr_Get --
   --------------------

   procedure Write_Attr_Get
     (Decl   : Gela.Grammars.Attribute_Declaration;
      Output : in out Writer;
      Impl   : Boolean) is
   begin
      Output.N ("   ");
      if Impl then
         Output.N ("overriding ");
      end if;
      Output.N ("function ");
      Output.P (To_Ada (Decl.Name));
      Output.P ("     (Self    : access Object;");
      Output.P ("      Payload : Gela.Types.Payload)");
      Output.N ("     return ");
      Output.N (To_Ada (Decl.Type_Name));
   end Write_Attr_Get;

   --------------------
   -- Write_Attr_Set --
   --------------------

   procedure Write_Attr_Set
     (Decl   : Gela.Grammars.Attribute_Declaration;
      Output : in out Writer;
      Impl   : Boolean) is
   begin
      Output.N ("   ");
      if Impl then
         Output.N ("overriding ");
      end if;
      Output.N ("procedure Set_");
      Output.P (To_Ada (Decl.Name));
      Output.P ("     (Self    : access Object;");
      Output.P ("      Payload : Gela.Types.Payload;");
      Output.N ("      Value   : ");
      Output.N (To_Ada (Decl.Type_Name));
      Output.N (")");
   end Write_Attr_Set;

   --------------------------
   -- Generate_Stores_List --
   --------------------------

   procedure Generate_Stores_List (NT : Gela.Grammars.Non_Terminal) is
      Output : Writer;
   begin
      Output.N ("with Gela.Nodes.");
      Output.N (Plural (NT.Name));
      Output.P (";");
      Output.P ("with Gela.Nodes;");
      Output.P ("with Gela.Stores.Lists;");
      Output.P ("with Gela.Nodes.Convertions;");
      Output.P;
      Output.N ("package Gela.Stores.");
      Output.N (To_Ada (NT.Name));
      Output.P ("_Sequences is");
      Output.P ("  new Gela.Stores.Lists");
      Output.N ("  (Node_List    => Gela.Nodes.");
      Output.N (Plural (NT.Name));
      Output.P (".List,");
      Output.N ("   Item         => Gela.Nodes.");
      Output.N (Plural (NT.Name));
      Output.P (".Object,");
      Output.N ("   Item_Record  => Gela.Nodes.");
      Output.N (To_Ada (NT.Name));
      Output.P (",");
      Output.P ("   Null_Item    => (null, 0),");
      Output.P ("   From_Element => Gela.Nodes.Convertions.""-"",");
      Output.N ("   Get_Payload  => Gela.Nodes.");
      Output.N (Plural (NT.Name));
      Output.P (".Get_Payload,");
      Output.N ("   Next         => Gela.Nodes.");
      Output.N (Plural (NT.Name));
      Output.P (".Next,");
      Output.N ("   Head         => Gela.Nodes.");
      Output.N (Plural (NT.Name));
      Output.P (".Head,");
      Output.N ("   Append       => Gela.Nodes.");
      Output.N (Plural (NT.Name));
      Output.P (".Append,");
      Output.N ("   Prepend      => Gela.Nodes.");
      Output.N (Plural (NT.Name));
      Output.P (".Prepend);");
      Output.N ("pragma Preelaborate (Gela.Stores.");
      Output.N (To_Ada (NT.Name));
      Output.P ("_Sequences);");
      Ada.Text_IO.Put_Line (Output.Text.To_UTF_8_String);
   end Generate_Stores_List;

   --------------------------
   -- Generate_Stores_Prod --
   --------------------------

   procedure Generate_Stores_Prod
     (NT   : Gela.Grammars.Non_Terminal;
      Prod : Gela.Grammars.Production)
   is
      procedure Write_Attr
        (NT   : Gela.Grammars.Non_Terminal;
         Done : in out League.String_Vectors.Universal_String_Vector);

      Store_Each  : Writer;
      Store_Body  : Writer;

      Last : Positive := Natural (Prod.Last - Prod.First) + 1;

      ----------------
      -- Write_Attr --
      ----------------

      procedure Write_Attr
        (NT   : Gela.Grammars.Non_Terminal;
         Done : in out League.String_Vectors.Universal_String_Vector) is
      begin
         for A in NT.First_Attribute .. NT.Last_Attribute loop
            if Done.Index (G.Declaration (A).Name) > 0 then
               goto Continue;
            end if;

            Done.Append (G.Declaration (A).Name);

            Last := Last + 1;
            Write_Attr_Get (G.Declaration (A), Store_Each, Impl => True);
            Write_Attr_Get (G.Declaration (A), Store_Body, Impl => True);
            Store_Body.P;
            Store_Body.P ("   is");
            Store_Body.N ("      Result : constant Gela.Stores.Element :=" &
                            " Self.Property (Payload, ");
            Store_Body.N (Last);
            Store_Body.P (");");
            Store_Body.P ("   begin");
            Store_Body.N ("      return ");
            Store_Body.N (To_Ada (G.Declaration (A).Type_Name));
            Store_Body.P ("'Val (Result);");
            Store_Body.N ("   end ");
            Store_Body.N (To_Ada (G.Declaration (A).Name));

            Store_Each.P (";", Store_Body);
            Store_Each.P ("", Store_Body);

            Write_Attr_Set (G.Declaration (A), Store_Each, Impl => True);
            Write_Attr_Set (G.Declaration (A), Store_Body, Impl => True);
            Store_Body.P;
            Store_Body.P ("   is");
            Store_Body.P ("   begin");
            Store_Body.N ("      Self.Set_Property (Payload, ");
            Store_Body.N (Last);
            Store_Body.N (", ");
            Store_Body.N (To_Ada (G.Declaration (A).Type_Name));
            Store_Body.P ("'Pos (Value));");
            Store_Body.N ("   end Set_");
            Store_Body.N (To_Ada (G.Declaration (A).Name));
            Store_Each.P (";", Store_Body);
            Store_Each.P ("", Store_Body);

            <<Continue>>
         end loop;
      end Write_Attr;

      Store_Each_Name : League.Strings.Universal_String;
      Type_List  : League.String_Vectors.Universal_String_Vector;
      Attr_List  : League.String_Vectors.Universal_String_Vector;
   begin
      if Macro_Reference (Prod) /= 0 then
         return;
      end if;

      Type_List.Append
        (League.Strings.To_Universal_String ("Gela.Types"));

      for J in G.Non_Terminal'Range loop
         if Implement (NT.Index, J) then
            Write_Attr_With (G.Non_Terminal (J), Store_Each, Type_List);
         end if;
      end loop;

      Write_Attr_With (NT, Store_Each, Type_List);

      Store_Each_Name.Clear;
      Store_Each_Name.Append ("Gela.Stores.");

      Store_Each_Name.Append (Plural (NT.Name));

      Store_Each.P ("with Gela.Stores.Productions;");
      Store_Each.P ("with Gela.Types;");
      Store_Each.P ("with Gela.Nodes.Visiters;");
      Store_Each.N ("with ");
      Store_Each.N (Non_Terminal_Unit (NT));
      Store_Each.P (";");
      Store_Each.P;
      Store_Each.N ("package ");

      Store_Body.P ("with Gela.Mutables.Compilations;");
      Store_Body.P ("pragma Unreferenced (Gela.Mutables.Compilations);");
      Store_Body.P;

      Store_Body.N ("package body ");
      Store_Each.N (Store_Each_Name, Store_Body);
      Store_Each.P (" is", Store_Body);
      Store_Each.P ("", Store_Body);
      Store_Each.P ("   pragma Preelaborate;");

      Store_Body.P ("   pragma Style_Checks (""-o"");");
      Store_Body.P ("   pragma Warnings (""F"");");
      Store_Body.P;

      Store_Each.P
        ("   type Object is new Gela.Stores.Productions.Production");
      Store_Each.N ("     and ");
      Store_Each.N (Non_Terminal_Unit (NT));
      Store_Each.P (".Object");
      Store_Each.P ("     with null record;");
      Store_Each.P;
      Store_Each.P
        ("   type Object_Access is access all Object;");
      Store_Each.P;

      Write_Attr (NT, Attr_List);

      for J in G.Non_Terminal'Range loop
         if Implement (NT.Index, J) then
            Write_Attr (G.Non_Terminal (J), Attr_List);
         end if;
      end loop;

      Store_Each.P ("   overriding procedure Visit", Store_Body);
      Store_Each.P ("     (Self    : access Object;", Store_Body);
      Store_Each.P ("      Payload : Gela.Types.Payload;", Store_Body);
      Store_Each.N ("      Visiter : in out " &
                      "Gela.Nodes.Visiters.Visiter'Class)",
                    Store_Body);
      Store_Body.P;
      Store_Body.P ("   is");
      Store_Body.N ("      Its : constant Gela.Nodes.");
      Store_Body.N (To_Ada (NT.Name));
      Store_Body.P ("_Access");
      Store_Body.N ("         := Gela.Nodes.");
      Store_Body.N (To_Ada (NT.Name));
      Store_Body.P ("_Access (Self);");
      Store_Body.P ("   begin");
      Store_Body.N ("      Visiter.");
      Store_Body.P (To_Ada (NT.Name));
      Store_Body.P ("        ((Its, Payload));");
      Store_Body.N ("   end Visit");
      Store_Each.P (";", Store_Body);
      Store_Each.P ("", Store_Body);

      Store_Each.P ("   overriding function Size", Store_Body);
      Store_Each.P ("     (Self    : access Object;", Store_Body);
      Store_Each.N ("      Payload : Gela.Types.Payload) return Natural",
                    Store_Body);

      Store_Body.P (" is");
      Store_Body.P ("   begin");
      Store_Body.N ("      return ");
      Store_Body.N (Reserved + Last);
      Store_Body.P (";");
      Store_Body.N ("   end Size");
      Store_Each.P (";", Store_Body);
      Store_Each.P ("", Store_Body);
      Store_Each.P ("   overriding function Last_Child", Store_Body);
      Store_Each.P ("     (Self    : access Object;", Store_Body);
      Store_Each.N ("      Payload : Gela.Types.Payload) return Natural",
                    Store_Body);

      Store_Body.P (" is");
      Store_Body.P ("   begin");
      Store_Body.N ("      return ");
      Store_Body.N (Natural (Prod.Last - Prod.First) + 1);
      Store_Body.P (";");
      Store_Body.N ("   end Last_Child");
      Store_Each.P (";", Store_Body);
      Store_Each.P ("", Store_Body);

      for Part of G.Part (Prod.First .. Prod.Last) loop
         Store_Each.N ("   overriding function ", Store_Body);
         Store_Each.P (To_Ada (Part.Name), Store_Body);
         Store_Each.P ("     (Self    : access Object;", Store_Body);
         Store_Each.P ("      Payload : Gela.Types.Payload)", Store_Body);
         Store_Each.N ("      return Gela.Nodes.", Store_Body);
         Store_Each.N (Return_Type (Part), Store_Body);
         Store_Each.P (";");
         Store_Body.P;
         Store_Body.P ("   is");
         Store_Body.N ("      Id   : constant Gela.Types.Payload " &
                         ":= Self.Child (Payload, ");
         Store_Body.N (Positive (Part.Index) - Offset);
         Store_Body.P (");");
         Store_Body.P ("      Node : constant Gela.Nodes.Node_Access := " &
                         "Self.To_Node (Id);");
         Store_Body.P ("   begin");
         Store_Body.N ("      return (Gela.Nodes.");
         Store_Body.N (Return_Type (Part));
         Store_Body.N ("_Access");

         if Return_Type (Part).Length > 34 then
            Store_Body.P;
            Store_Body.N ("       ");
         end if;
         Store_Body.P (" (Node), Id);");
         Store_Body.N ("   end ");
         Store_Body.N (To_Ada (Part.Name));
         Store_Body.P (";");

         Store_Each.P ("", Store_Body);
      end loop;

      Store_Each.N ("end ", Store_Body);
      Store_Each.N (Store_Each_Name, Store_Body);
      Store_Each.N (";", Store_Body);

      Ada.Text_IO.Put_Line (Store_Each.Text.To_UTF_8_String);
      Ada.Text_IO.Put_Line (Store_Body.Text.To_UTF_8_String);
   end Generate_Stores_Prod;

   ----------------------
   -- Generate_Visiter --
   ----------------------

   procedure Generate_Visiter is
      Withes : Writer;
      Spec   : Writer;
      Name : League.Strings.Universal_String;
   begin
      Spec.P ("package Gela.Element_Visiters is");
      Spec.P ("   pragma Preelaborate;");
      Spec.P;
      Spec.P ("   type Visiter is limited interface;");

      for NT of G.Non_Terminal loop
         for Prod of G.Production (NT.First .. NT.Last) loop
            if Is_Concrete (NT.Index) and not NT.Is_List then
               Name := To_Ada (NT.Name);
               Withes.N ("with Gela.Elements.");
               Withes.N (Plural (Name));
               Withes.P (";");
               Spec.P;
               Spec.N ("   not overriding procedure ");
               Spec.P (To_Ada (Name));
               Spec.P ("     (Self : in out Visiter;");
               Spec.N ("      Node : not null Gela.Elements.");
               Spec.N (Plural (Name));
               Spec.N (".");
               Spec.N (To_Ada (Name));
               Spec.P ("_Access)");
               Spec.P ("        is null;");
            end if;
         end loop;
      end loop;

      Spec.P;
      Spec.P ("end Gela.Element_Visiters;");
      Ada.Text_IO.Put_Line (Withes.Text.To_UTF_8_String);
      Ada.Text_IO.Put_Line (Spec.Text.To_UTF_8_String);
   end Generate_Visiter;

   ---------------------
   -- Production_Unit --
   ---------------------

   function Non_Terminal_Unit
     (NT : Gela.Grammars.Non_Terminal)
      return League.Strings.Universal_String is
   begin
      return "Gela.Nodes." & Plural (NT.Name);
   end Non_Terminal_Unit;

begin
   AG_Tools.Input.Initialize (Name);
   G := AG_Tools.Input.Grammar;

--     Gela.Grammars_Debug.Print (G);

   for NT of G.Non_Terminal loop
      if Has_List (NT.Index) then
         Generate_Stores_List (NT);
      end if;

      if not NT.Is_List then
         for Prod of G.Production (NT.First .. NT.Last) loop
            Offset := Positive (Prod.First) - 1;
            Generate_Stores_Prod (NT, Prod);
         end loop;
      end if;
   end loop;

   Generate_Fabric;
   Generate_2;
   Generate_Visiter;
   AG_Tools.Element_Generators.Generate_Elements (G);
   AG_Tools.Element_Generators.Generate_Fabric (G);
end AG_Driver;
