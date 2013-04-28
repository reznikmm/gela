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
with Gela.Grammars.Reader;
--  with Gela.Grammars_Convertors;

with AG_Tools.Writers; use AG_Tools.Writers;
--  with Gela.Grammars_Debug;

procedure AG_Driver is

   use type Gela.Grammars.Production_Index;
   use type Gela.Grammars.Part_Count;
   use type Gela.Grammars.Non_Terminal_Count;
   use type League.Strings.Universal_String;

   function To_Ada
     (Text : League.Strings.Universal_String)
      return League.Strings.Universal_String;

   function Plural
     (Text : League.Strings.Universal_String)
      return League.Strings.Universal_String;

   function Is_Ambiguous (NT : Gela.Grammars.Non_Terminal) return Boolean;

   function Count_Ambiguous (G : Gela.Grammars.Grammar) return Natural;

   function Return_Type
     (Part : Gela.Grammars.Part)
      return League.Strings.Universal_String;

   function Production_Unit
     (Prod : Gela.Grammars.Production)
      return League.Strings.Universal_String;

   procedure Write_Parts
     (Prod     : Gela.Grammars.Production;
      Nodes_NT : in out Writer);

   procedure Write_Nodes_NT
     (NT       : Gela.Grammars.Non_Terminal;
      Nodes_NT : in out Writer);
   --  Write Gela.Nodes.<Non_Terminal>s package specification

   function Macro_Reference
     (Prod : Gela.Grammars.Production)
      return Gela.Grammars.Non_Terminal_Count;

   function List_Reference
     (Part : Gela.Grammars.Part)
      return Gela.Grammars.Non_Terminal_Count;

   procedure Look_For_List (Prod : Gela.Grammars.Production);

   procedure Generate_Nodes;
   procedure Generate_Fabric;
   procedure Generate_Conv;
   procedure Generate_Nodes_NT (NT : Gela.Grammars.Non_Terminal);
   procedure Generate_Stores_Prod
     (NT   : Gela.Grammars.Non_Terminal;
      Prod : Gela.Grammars.Production);
   procedure Generate_Stores_NT_Switch (NT : Gela.Grammars.Non_Terminal);
   procedure Generate_Stores_List (NT : Gela.Grammars.Non_Terminal);

     ---------------------
   -- Count_Ambiguous --
   ---------------------

   function Count_Ambiguous (G : Gela.Grammars.Grammar) return Natural is
      Result : Natural := 0;
   begin
      for NT of G.Non_Terminal loop
         if Is_Ambiguous (NT) then
            Result := Result + 1;
         end if;
      end loop;

      return Result;
   end Count_Ambiguous;

   Name  : constant String := Ada.Command_Line.Argument (1);
   G     : constant Gela.Grammars.Grammar := Gela.Grammars.Reader.Read (Name);
   Plain : constant Gela.Grammars.Grammar := G;

   Is_Macro : array (Plain.Non_Terminal'Range) of Boolean := (others => False);
   Has_List : array (Plain.Non_Terminal'Range) of Boolean := (others => False);
   Implement : array (Plain.Non_Terminal'Range, Plain.Non_Terminal'Range)
     of Boolean := (others => (others => False));

   Reserved : constant := 3;  --  Tag, Count

   Offset      : Natural;

   -------------------
   -- Generate_Conv --
   -------------------

   procedure Generate_Conv is
      Conv_With    : Writer;
      Conv_Spec    : Writer;
   begin
      Conv_With.P ("with Gela.Nodes.Tokens;");
      Conv_With.P ("pragma Unreferenced (Gela.Nodes.Tokens);");
      Conv_Spec.P ("package Gela.Nodes.Convertions is");
      Conv_Spec.P ("   pragma Preelaborate;");
      Conv_Spec.P;

      for NT of Plain.Non_Terminal loop
         if not NT.Is_List then
            Conv_With.N ("with Gela.Nodes.");
            Conv_With.N (Plural (NT.Name));
            Conv_With.P (";");
            Conv_With.N ("pragma Unreferenced (Gela.Nodes.");
            Conv_With.N (Plural (NT.Name));
            Conv_With.P (");");

            Conv_Spec.P ("   function ""+""");
            Conv_Spec.N ("     (X : Gela.Nodes.");
            Conv_Spec.N (To_Ada (NT.Name));
            Conv_Spec.P (")");
            Conv_Spec.P ("      return Gela.Nodes.Element is" &
                           " (X.Object, X.Payload);");
            Conv_Spec.P;
            Conv_Spec.P ("   function ""-""");
            Conv_Spec.P ("     (X : Gela.Nodes.Element)");
            Conv_Spec.N ("      return Gela.Nodes.");
            Conv_Spec.N (To_Ada (NT.Name));
            Conv_Spec.P (" is (X.Object, X.Payload);");
            Conv_Spec.P;

            if Has_List (NT.Index) then
               Conv_Spec.P ("   function ""+""");
               Conv_Spec.N ("     (X : Gela.Nodes.");
               Conv_Spec.N (To_Ada (NT.Name));
               Conv_Spec.P ("_List)");
               Conv_Spec.P ("      return Gela.Nodes.Element is" &
                              " (X.Object, X.Payload);");
               Conv_Spec.P;
               Conv_Spec.P ("   function ""-""");
               Conv_Spec.P ("     (X : Gela.Nodes.Element)");
               Conv_Spec.N ("      return Gela.Nodes.");
               Conv_Spec.N (To_Ada (NT.Name));
               Conv_Spec.P ("_List is (X.Object, X.Payload);");
               Conv_Spec.P;
            end if;
         end if;
      end loop;

      Conv_Spec.P ("   function ""+""");
      Conv_Spec.P ("     (X : Gela.Nodes.Token)");
      Conv_Spec.P ("      return Gela.Nodes.Element is" &
                     " (X.Object, X.Payload);");
      Conv_Spec.P;
      Conv_Spec.P ("   function ""-""");
      Conv_Spec.P ("     (X : Gela.Nodes.Element)");
      Conv_Spec.N ("      return Gela.Nodes.Token");
      Conv_Spec.P (" is (X.Object, X.Payload);");
      Conv_Spec.P;
      Conv_Spec.P ("end Gela.Nodes.Convertions;");
      Ada.Text_IO.Put_Line (Conv_With.Text.To_UTF_8_String);
      Ada.Text_IO.Put (Conv_Spec.Text.To_UTF_8_String);
   end Generate_Conv;

   ---------------------
   -- Generate_Fabric --
   ---------------------

   procedure Generate_Fabric is
      Ambiguous_Count : constant Natural := Count_Ambiguous (Plain);
      Amb_Index       : Natural := Natural (Plain.Last_Production);

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
      Fab_Body.P;
      Fab_Init.P ("   procedure Initialize (Self : access Base_Fabric) is");
      Fab_Init.P ("   begin");
      Fab_Init.P ("      Self.Map :=");
      Fab_Init.N ("        (0 => Self.Token'Access");

      for NT of Plain.Non_Terminal loop
         if not NT.Is_List then
            if Is_Ambiguous (NT) then
               Fabrics.N ("      S");
               Fabrics.N (Positive (NT.Index));
               Fabrics.N (" : aliased ");
               Fabrics.N (Plural (NT.Name));
               Fabrics.P (".Switch (Store);");

               Amb_Index := Amb_Index + 1;

               Fab_Init.P (",");
               Fab_Init.N ("         ");

               Fab_Init.N (Amb_Index);
               Fab_Init.N (" => Self.S");
               Fab_Init.N (Positive (NT.Index));
               Fab_Init.N ("'Access");

            end if;

            if Has_List (NT.Index) then
               Fab_With.N ("with Gela.Stores.");
               Fab_With.N (To_Ada (NT.Name));
               Fab_With.P ("_Lists;");

               Fabrics.N ("      L");
               Fabrics.N (Positive (NT.Index));
               Fabrics.N (" : aliased Gela.Stores.");
               Fabrics.N (To_Ada (NT.Name));
               Fabrics.P ("_Lists.List (Store);");

               Lists := Lists + 1;
               Fab_Init.P (",");
               Fab_Init.N ("         Last_Production + ");
               Fab_Init.N (Lists);
               Fab_Init.N (" => Self.L");
               Fab_Init.N (Positive (NT.Index));
               Fab_Init.N ("'Access");
            end if;

            for Prod of Plain.Production (NT.First .. NT.Last) loop
               declare
                  M : constant Gela.Grammars.Non_Terminal_Count :=
                    Macro_Reference (Prod);
               begin
                  if M /= 0 then
                     null;
                  elsif Is_Macro (NT.Index) then
                     Fab_With.N ("with Gela.Stores.");
                     Fab_With.N (Plural (NT.Name));
                     Fab_With.P (";");

                     Fabrics.N ("      P");
                     Fabrics.N (Positive (Prod.Index));
                     Fabrics.N (" : aliased ");
                     Fabrics.N (Plural (NT.Name));
                     Fabrics.P (".Object (Store);");
                  else  --   M = 0
                     Fab_With.N ("with Gela.Stores.");
                     Fab_With.N (To_Ada (NT.Name));
                     Fab_With.N ("_");
                     Fab_With.N (Plural (Prod.Name));
                     Fab_With.P (";");

                     Fabrics.N ("      P");
                     Fabrics.N (Positive (Prod.Index));
                     Fabrics.N (" : aliased ");
                     Fabrics.N (To_Ada (NT.Name));
                     Fabrics.N ("_");
                     Fabrics.N (Plural (Prod.Name));
                     Fabrics.P (".Object (Store);");
                  end if;

                  Fab_Init.P (",");
                  Fab_Init.N ("         ");

                  Fab_Init.N (Positive (Prod.Index));

                  if M = 0 then
                     Fab_Init.N (" => Self.P");
                     Fab_Init.N (Positive (Prod.Index));
                     Fab_Init.N ("'Access");
                  else
                     Fab_Init.N (" => null");
                  end if;
               end;
            end loop;
         end if;
      end loop;

      Fabrics.N ("      Map : Node_Array (0 .. ");
      Fabrics.N (Positive (Plain.Last_Production) + Lists);
      Fabrics.P (");");
      Fabrics.P ("   end record;");
      Fabrics.P;

      Lists := 0;

      for NT of Plain.Non_Terminal loop
         if Has_List (NT.Index) then
            Lists := Lists + 1;

            Fabrics.N ("   function ", Fab_Body);
            Fabrics.N (To_Ada (NT.Name), Fab_Body);
            Fabrics.P ("_List", Fab_Body);
            Fabrics.P ("     (Self : access Base_Fabric'Class)", Fab_Body);
            Fabrics.N ("      return Gela.Nodes.", Fab_Body);
            Fabrics.N (To_Ada (NT.Name), Fab_Body);
            Fabrics.N ("_List", Fab_Body);
            Fabrics.P (";");
            Fab_Body.P;
            Fab_Body.P ("   is");
            Fab_Body.N ("      Object : constant ");
            Fab_Body.N (To_Ada (NT.Name));
            Fab_Body.N ("_Lists.List_Access := Self.L");
            Fab_Body.N (Positive (NT.Index));
            Fab_Body.P ("'Access;");
            Fab_Body.N ("      Int    : constant Gela.Nodes.");
            Fab_Body.N (Plural (NT.Name));
            Fab_Body.P (".List_Access :=");
            Fab_Body.N ("        Gela.Nodes.");
            Fab_Body.N (Plural (NT.Name));
            Fab_Body.P (".List_Access (Object);");
            Fab_Body.N ("      Result : constant Gela.Nodes.");
            Fab_Body.N (To_Ada (NT.Name));
            Fab_Body.P ("_List :=");
            Fab_Body.N
              ("        (Payload => Self.Create_List (Last_Production + ");
            Fab_Body.N (Lists);
            Fab_Body.P ("),");
            Fab_Body.N ("         Object  => Gela.Nodes.");
            Fab_Body.N (To_Ada (NT.Name));
            Fab_Body.P ("_List_Access (Int));");
            Fab_Body.P ("   begin");
            Fab_Body.P ("      return Result;");
            Fab_Body.N ("   end ");
            Fab_Body.N (To_Ada (NT.Name));
            Fab_Body.P ("_List;");
            Fabrics.P ("", Fab_Body);
         end if;

         for Prod of Plain.Production (NT.First .. NT.Last) loop
            declare
               Name : League.Strings.Universal_String;
               M : constant Gela.Grammars.Non_Terminal_Count :=
                 Macro_Reference (Prod);
            begin
               if M = 0 and not NT.Is_List then
                  Fabrics.N ("   function ", Fab_Body);
                  if Is_Macro (NT.Index) then
                     Name := To_Ada (NT.Name);
                  else
                     Name := To_Ada (Prod.Name);
                  end if;

                  Fabrics.P (Name, Fab_Body);
                  Fabrics.N
                    ("     (Self : access Base_Fabric'Class", Fab_Body);

                  for Part of Plain.Part (Prod.First .. Prod.Last) loop
                     Fabrics.P (";", Fab_Body);
                     Fabrics.N ("      ", Fab_Body);
                     Fabrics.N (To_Ada (Part.Name), Fab_Body);
                     Fabrics.N (" : Gela.Nodes.", Fab_Body);
                     Fabrics.N (Return_Type (Part), Fab_Body);
                  end loop;

                  Fabrics.P (")", Fab_Body);
                  Fabrics.N ("      return Gela.Nodes.", Fab_Body);
                  Fabrics.N (To_Ada (NT.Name), Fab_Body);
                  Fabrics.P (";");
                  Fabrics.P ("", Fab_Body);

                  Body_With.N ("with Gela.Nodes.");
                  Body_With.N (Plural (NT.Name));
                  Body_With.P (";");

                  Fab_Body.P ("   is");
                  Fab_Body.N ("      Object : constant ");
                  Fab_Body.N (Plural (NT.Name));
                  Fab_Body.N (".Object_Access := Self.P");
                  Fab_Body.N (Natural (Prod.Index));
                  Fab_Body.P ("'Access;");
                  Fab_Body.N ("      Int    : constant Gela.Nodes.");
                  Fab_Body.N (Plural (NT.Name));
                  Fab_Body.P (".Object_Access :=");
                  Fab_Body.N ("        Gela.Nodes.");
                  Fab_Body.N (Plural (NT.Name));
                  Fab_Body.P (".Object_Access (Object);");
                  Fab_Body.N ("      Result : constant Gela.Nodes.");
                  Fab_Body.N (To_Ada (NT.Name));
                  Fab_Body.P (" :=");
                  Fab_Body.N ("        (Payload => Self.Create_Production (");
                  Fab_Body.N (Natural (Prod.Index));
                  Fab_Body.P ("),");
                  Fab_Body.N ("         Object  => Gela.Nodes.");
                  Fab_Body.N (To_Ada (NT.Name));
                  Fab_Body.P ("_Access (Int));");
                  Fab_Body.P ("   begin");

                  Offset := Positive (Prod.First) - 1;

                  for Part of Plain.Part (Prod.First .. Prod.Last) loop
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
      Fabrics.N (Positive (Plain.Last_Production));
      Fabrics.P (";");
      Fabrics.P;

      Fabrics.P ("   procedure Initialize (Self : access Base_Fabric);");
      Fabrics.P;
      Fabrics.N ("end Gela.Stores.Base_Fabrics;");

      if Ambiguous_Count /= Positive (Plain.Last_Non_Terminal) then
         Fab_Init.P (",");
         Fab_Init.N ("         others => null");
      end if;

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

   --------------------
   -- Generate_Nodes --
   --------------------

   procedure Generate_Nodes is
      Nodes_With  : Writer;
      Nodes       : Writer;
   begin
      Nodes_With.P ("with Gela.Types;");
      Nodes_With.P ("limited with Gela.Nodes.Tokens;");

      Nodes.P ("package Gela.Nodes is");
      Nodes.P ("   pragma Preelaborate;");
      Nodes.P;
      Nodes.P ("   type Node is interface;");
      Nodes.P ("   type Node_Access is access all Node'Class;");
      Nodes.P;
      Nodes.P ("   type Element is record");
      Nodes.P ("      Object  : access Node'Class;");
      Nodes.P ("      Payload : Gela.Types.Payload;");
      Nodes.P ("   end record;");

      for NT of Plain.Non_Terminal loop
         if not NT.Is_List then
            Nodes_With.N ("limited with Gela.Nodes.");
            Nodes_With.N (Plural (NT.Name));
            Nodes_With.P (";");
            Nodes.P;
            Nodes.N ("   type ");
            Nodes.N (To_Ada (NT.Name));
            Nodes.N ("_Access is");

            if NT.Name.Length > 11 then
               Nodes.P;
               Nodes.N ("    ");
            end if;

            Nodes.N (" access all Gela.Nodes.");
            Nodes.N (Plural (NT.Name));
            Nodes.P (".Object'Class;");
            Nodes.N ("   type ");
            Nodes.N (To_Ada (NT.Name));
            Nodes.P (" is record");
            Nodes.N ("      Object  : ");
            Nodes.N (To_Ada (NT.Name));
            Nodes.P ("_Access;");
            Nodes.N ("      Payload : Gela.Types.Payload;");
            Nodes.P;
            Nodes.P ("   end record;");

            if Has_List (NT.Index) then
               Nodes.N ("   type ");
               Nodes.N (To_Ada (NT.Name));
               Nodes.P ("_List_Access is");
               Nodes.N ("     access all Gela.Nodes.");
               Nodes.N (Plural (NT.Name));
               Nodes.P (".List'Class;");
               Nodes.N ("   type ");
               Nodes.N (To_Ada (NT.Name));
               Nodes.P ("_List is record");
               Nodes.N ("      Object  : ");
               Nodes.N (To_Ada (NT.Name));
               Nodes.P ("_List_Access;");
               Nodes.N ("      Payload : Gela.Types.Payload;");
               Nodes.P;
               Nodes.P ("   end record;");
            end if;

            if Is_Ambiguous (NT) then
               Nodes.P;
               Nodes.N ("   type ");
               Nodes.N (To_Ada (NT.Name));
               Nodes.N ("_Switch_Access is access all Gela.Nodes.");
               Nodes.N (Plural (NT.Name));
               Nodes.P (".Switch'Class;");
               Nodes.N ("   type ");
               Nodes.N (To_Ada (NT.Name));
               Nodes.P ("_Switch is record");
               Nodes.N ("      Object  : ");
               Nodes.N (To_Ada (NT.Name));
               Nodes.P ("_Switch_Access;");
               Nodes.N ("      Payload : Gela.Nodes.Payload;");
               Nodes.P;
               Nodes.P ("   end record;");
               Nodes.P;
            end if;
         end if;
      end loop;

      Nodes.P;
      Nodes.P ("   type Token_Access is access all" &
                 " Gela.Nodes.Tokens.Object'Class;");
      Nodes.P ("   type Token is record");
      Nodes.P ("      Object  : Token_Access;");
      Nodes.P ("      Payload : Gela.Types.Payload;");
      Nodes.P ("   end record;");
      Nodes.P;
      Nodes.N ("end Gela.Nodes;");

      Ada.Text_IO.Put_Line (Nodes_With.Text.To_UTF_8_String);
      Ada.Text_IO.Put_Line (Nodes.Text.To_UTF_8_String);
   end Generate_Nodes;

   -----------------------
   -- Generate_Nodes_NT --
   -----------------------

   procedure Generate_Nodes_NT (NT : Gela.Grammars.Non_Terminal) is
      Nodes_NT    : Writer;
   begin
      Write_Nodes_NT (NT, Nodes_NT);

      for Prod of Plain.Production (NT.First .. NT.Last) loop
         if NT.First /= NT.Last and Macro_Reference (Prod) = 0 then
            Nodes_NT.N ("package Gela.Nodes.");
            Nodes_NT.N (Plural (NT.Name));
            Nodes_NT.N (".");
            Nodes_NT.N (Plural (Prod.Name));
            Nodes_NT.P (" is");
            Nodes_NT.P ("   pragma Preelaborate;");
            Nodes_NT.P;
            Nodes_NT.N ("   type Object is interface and Gela.Nodes.");
            Nodes_NT.N (Plural (NT.Name));
            Nodes_NT.P (".Object;");
            Nodes_NT.P;
            Nodes_NT.P ("   type Object_Access is access all Object'Class;");
            Nodes_NT.P;

            Write_Parts (Prod, Nodes_NT);

            Nodes_NT.N ("end Gela.Nodes.");
            Nodes_NT.N (Plural (NT.Name));
            Nodes_NT.N (".");
            Nodes_NT.N (Plural (Prod.Name));
            Nodes_NT.P (";");
         end if;
      end loop;

      Ada.Text_IO.Put_Line (Nodes_NT.Text.To_UTF_8_String);
   end Generate_Nodes_NT;

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
      Output.P ("_Lists is new Gela.Stores.Lists");
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
      Output.N ("   Head         => Gela.Nodes.");
      Output.N (Plural (NT.Name));
      Output.P (".Head,");
      Output.N ("   Append       => Gela.Nodes.");
      Output.N (Plural (NT.Name));
      Output.P (".Append);");
      Output.N ("pragma Preelaborate (Gela.Stores.");
      Output.N (To_Ada (NT.Name));
      Output.P ("_Lists);");
      Ada.Text_IO.Put_Line (Output.Text.To_UTF_8_String);
   end Generate_Stores_List;

   -------------------------------
   -- Generate_Stores_NT_Switch --
   -------------------------------

   procedure Generate_Stores_NT_Switch (NT : Gela.Grammars.Non_Terminal) is
      Switch_NT   : Writer;
      Switch_Body : Writer;
   begin
      Switch_NT.N ("with Gela.Nodes.");
      Switch_NT.N (Plural (NT.Name));
      Switch_NT.P (";");
      Switch_NT.P ("with Gela.Stores.Productions;");
      Switch_NT.P ("with Gela.Types;");
      Switch_NT.P ("with Gela.Stores.Nodes;");
      Switch_NT.P;

      Switch_Body.P ("with Gela.Mutables.Compilations;");
      Switch_Body.P ("pragma Unreferenced (Gela.Mutables.Compilations);");

      Switch_Body.P;
      Switch_Body.N ("package body ");

      Switch_NT.N ("package ");

      Switch_Body.N ("Gela.Stores.", Switch_NT);
      Switch_Body.N (To_Ada (NT.Name), Switch_NT);
      Switch_Body.N ("_Switches.", Switch_NT);
      Switch_Body.P (" is", Switch_NT);
      Switch_Body.P ("", Switch_NT);

      Switch_Body.P ("   pragma Style_Checks (""-o"");");
      Switch_Body.P ("   pragma Warnings (""F"");");
      Switch_Body.P;
      Switch_Body.P ("   Size_Offset : constant := 2;");
      Switch_Body.P;

      Switch_NT.N ("   type Object is ");
      Switch_NT.P ("abstract new Gela.Stores.Productions.Production");
      Switch_NT.N ("       and Gela.Nodes.");
      Switch_NT.N (Plural (NT.Name));
      Switch_NT.N (".Object");
      Switch_NT.P (" with null record;");
      Switch_NT.P;

      Switch_NT.P ("   overriding function Element", Switch_Body);
      Switch_NT.P ("     (Self    : access Object;", Switch_Body);
      Switch_NT.P ("      Payload : Gela.Types.Payload;", Switch_Body);
      Switch_NT.P ("      Index   : Positive)", Switch_Body);
      Switch_NT.N ("      return Gela.Nodes.", Switch_Body);
      Switch_NT.N (To_Ada (NT.Name), Switch_Body);
      Switch_NT.N (";");
      Switch_NT.P ("", Switch_Body);
      Switch_Body.P ("   is");
      Switch_Body.P ("      pragma Unreferenced (Index);");
      Switch_Body.P ("   begin");
      Switch_Body.N ("      return (Gela.Nodes.");
      Switch_Body.N (To_Ada (NT.Name));
      Switch_Body.P ("_Access (Self), Payload);");
      Switch_Body.P ("   end Element;");
      Switch_NT.P ("", Switch_Body);

      Switch_NT.P ("   overriding function Length", Switch_Body);
      Switch_NT.P ("     (Self    : access Object;", Switch_Body);
      Switch_NT.P ("      Payload : Gela.Types.Payload)", Switch_Body);
      Switch_NT.N ("      return Natural", Switch_Body);
      Switch_NT.N (";");
      Switch_NT.P ("", Switch_Body);
      Switch_Body.P ("   is");
      Switch_Body.P ("      pragma Unreferenced (Self);");
      Switch_Body.P ("      pragma Unreferenced (Payload);");
      Switch_Body.P ("   begin");
      Switch_Body.P ("      return 1;");
      Switch_Body.P ("   end Length;");
      Switch_NT.P ("", Switch_Body);

      Switch_NT.P ("   type Switch is new Gela.Stores.Nodes.Node");
      Switch_NT.N ("       and Gela.Nodes.");
      Switch_NT.N (Plural (NT.Name));
      Switch_NT.P (".Switch with null record;");
      Switch_NT.P;
      Switch_NT.P ("   overriding function Size", Switch_Body);
      Switch_NT.P ("     (Self    : access Switch;", Switch_Body);
      Switch_NT.N ("      Payload : Gela.Types.Payload) return Natural",
                   Switch_Body);
      Switch_NT.P (";");
      Switch_NT.P ("", Switch_Body);

      Switch_Body.P ("   is");
      Switch_Body.P ("      Size_Index : constant Stores.Index :=");
      Switch_Body.P ("        Stores.Index (Payload) + Size_Offset;");
      Switch_Body.P ("      Data : constant Gela.Stores.Element :=");
      Switch_Body.P ("        Self.Compilation.Store.Get (Size_Index);");
      Switch_Body.P ("   begin");
      Switch_Body.P ("      return Natural (Data);");
      Switch_Body.P ("   end Size;");
      Switch_Body.P;

      Switch_NT.P ("   overriding function Last_Child", Switch_Body);
      Switch_NT.P ("     (Self    : access Switch;", Switch_Body);
      Switch_NT.N ("      Payload : Gela.Types.Payload) return Natural",
                   Switch_Body);
      Switch_NT.P (";");
      Switch_Body.P (" is");
      Switch_Body.P ("   begin");
      Switch_Body.N ("      return Size (Self, Payload) - ");
      Switch_Body.N (Reserved);
      Switch_Body.P (";");
      Switch_Body.P ("   end Last_Child;");

      Switch_NT.P ("", Switch_Body);
      Switch_NT.P ("   overriding function Length", Switch_Body);
      Switch_NT.P ("     (Self    : access Switch;", Switch_Body);
      Switch_NT.P ("      Payload : Gela.Types.Payload)", Switch_Body);
      Switch_NT.N ("      return Natural", Switch_Body);
      Switch_NT.P (";");

      Switch_Body.P (" is");
      Switch_Body.P ("   begin");
      Switch_Body.P ("      return Last_Child (Self, Payload);");
      Switch_Body.P ("   end Length;");

      Switch_NT.P ("", Switch_Body);
      Switch_NT.P ("   overriding function Element", Switch_Body);
      Switch_NT.P ("     (Self    : access Switch;", Switch_Body);
      Switch_NT.P ("      Payload : Gela.Types.Payload;", Switch_Body);
      Switch_NT.P ("      Index   : Positive)", Switch_Body);
      Switch_NT.N ("      return Gela.Nodes.", Switch_Body);
      Switch_NT.N (To_Ada (NT.Name), Switch_Body);
      Switch_NT.P (";");

      Switch_Body.P;
      Switch_Body.P ("   is");
      Switch_Body.P ("      Id   : constant Gela.Types.Payload :=" &
                       " Self.Child (Payload, Index);");
      Switch_Body.P ("      Node : constant Gela.Nodes.Node_Access := " &
                       "Self.To_Node (Id);");
      Switch_Body.P ("   begin");
      Switch_Body.N ("      return (Gela.Nodes.");
      Switch_Body.N (To_Ada (NT.Name));
      Switch_Body.P ("_Access (Node), Id);");
      Switch_Body.P ("   end Element;");

      Switch_NT.P ("", Switch_Body);

      Switch_Body.N ("end Gela.Stores.");
      Switch_Body.N (Plural (NT.Name));
      Switch_Body.P (";");

      Switch_NT.N ("end Gela.Stores.");
      Switch_NT.N (Plural (NT.Name));
      Switch_NT.P (";");
      Ada.Text_IO.Put_Line (Switch_NT.Text.To_UTF_8_String);
      Ada.Text_IO.Put_Line (Switch_Body.Text.To_UTF_8_String);
   end Generate_Stores_NT_Switch;

   --------------------------
   -- Generate_Stores_Prod --
   --------------------------

   procedure Generate_Stores_Prod
     (NT   : Gela.Grammars.Non_Terminal;
      Prod : Gela.Grammars.Production)
   is
      Store_Each  : Writer;
      Store_Body  : Writer;
      Store_Each_Name : League.Strings.Universal_String;
   begin
      if Macro_Reference (Prod) /= 0 then
         return;
      end if;

      Store_Each_Name.Clear;
      Store_Each_Name.Append ("Gela.Stores.");

      if Is_Macro (NT.Index) then
         Store_Each_Name.Append (Plural (NT.Name));
      else
         Store_Each_Name.Append (To_Ada (NT.Name));
         Store_Each_Name.Append ("_");
         Store_Each_Name.Append (Plural (Prod.Name));
      end if;

      Store_Each.P ("with Gela.Stores.Productions;");
      Store_Each.P ("with Gela.Types;");
      Store_Each.N ("with ");
      Store_Each.N (Production_Unit (Prod));
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
      Store_Each.N (Production_Unit (Prod));
      Store_Each.P (".Object with null record;");
      Store_Each.P;
      Store_Each.P
        ("   type Object_Access is access all Object;");
      Store_Each.P;
      Store_Each.P ("   overriding function Size", Store_Body);
      Store_Each.P ("     (Self    : access Object;", Store_Body);
      Store_Each.N ("      Payload : Gela.Types.Payload) return Natural",
                    Store_Body);

      Store_Body.P (" is");
      Store_Body.P ("   begin");
      Store_Body.N ("      return ");
      Store_Body.N
        (Reserved + Natural (Prod.Last - Prod.First) + 1);
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

      for Part of Plain.Part (Prod.First .. Prod.Last) loop
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
         Store_Body.P ("_Access (Node), Id);");
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

   ------------------
   -- Is_Ambiguous --
   ------------------

   function Is_Ambiguous (NT : Gela.Grammars.Non_Terminal) return Boolean is
      pragma Unreferenced (NT);
   begin
      return False;  --  True;
   end Is_Ambiguous;

   -------------
   -- Is_List --
   -------------

   function List_Reference
     (Part : Gela.Grammars.Part)
      return Gela.Grammars.Non_Terminal_Count is
   begin
      if Part.Is_Option then
         if Part.First = Part.Last then
            declare
               Prod : Gela.Grammars.Production renames
                 Plain.Production (Part.First);
               Part : Gela.Grammars.Part renames Plain.Part (Prod.First);
            begin
               if Prod.First = Prod.Last and then
                 Part.Is_List_Reference
               then
                  declare
                     NT    : Gela.Grammars.Non_Terminal renames
                       Plain.Non_Terminal (Part.Denote);
                     Inner : Gela.Grammars.Production renames
                       Plain.Production (NT.First);
                  begin
                     return Plain.Part (Inner.First).Denote;
                  end;
               end if;
            end;
         end if;
      end if;

      return 0;
   end List_Reference;

   -------------------
   -- Look_For_List --
   -------------------

   procedure Look_For_List (Prod : Gela.Grammars.Production) is
      List : Gela.Grammars.Non_Terminal_Count;
   begin
      for Part of Plain.Part (Prod.First .. Prod.Last) loop
         List := List_Reference (Part);
         if List /= 0 then
            Has_List (List) := True;
         end if;
      end loop;
   end Look_For_List;

   ---------------------
   -- Macro_Reference --
   ---------------------

   function Macro_Reference
     (Prod : Gela.Grammars.Production)
      return Gela.Grammars.Non_Terminal_Count is
   begin
      if Prod.First /= Prod.Last then
         return 0;
      end if;

      declare
         Part : Gela.Grammars.Part renames Plain.Part (Prod.First);
      begin
         if not Part.Is_Non_Terminal_Reference then
            return 0;
         elsif Plain.Non_Terminal (Part.Denote).Name /= Part.Name then
            return 0;
         else
            return Part.Denote;
         end if;
      end;
   end Macro_Reference;

   ------------
   -- Plural --
   ------------

   function Plural
     (Text : League.Strings.Universal_String)
      return League.Strings.Universal_String is
   begin
      return To_Ada (Text) & "s";
   end Plural;

   ---------------------
   -- Production_Unit --
   ---------------------

   function Production_Unit
     (Prod : Gela.Grammars.Production)
      return League.Strings.Universal_String
   is
      Result : League.Strings.Universal_String;
      NT : constant Gela.Grammars.Non_Terminal_Index := Prod.Parent;
   begin
      Result.Append ("Gela.Nodes.");
      Result.Append (Plural (Plain.Non_Terminal (NT).Name));

      if not Is_Macro (NT) then
         Result.Append (".");
         Result.Append (Plural (Prod.Name));
      end if;

      return Result;
   end Production_Unit;

   -----------------
   -- Return_Type --
   -----------------

   function Return_Type
     (Part : Gela.Grammars.Part)
      return League.Strings.Universal_String
   is
      Result : League.Strings.Universal_String;
   begin
      if Part.Is_Option then
         if Part.First /= Part.Last then
            raise Constraint_Error;
         end if;

         declare
            Prod : Gela.Grammars.Production renames
              Plain.Production (Part.First);
         begin
            if Prod.First /= Prod.Last then
               raise Constraint_Error;
            end if;

            return Return_Type (Plain.Part (Prod.First));
         end;

      elsif Part.Is_List_Reference then
         declare
            NT : Gela.Grammars.Non_Terminal renames
              Plain.Non_Terminal (Part.Denote);
            Prod : Gela.Grammars.Production renames
              Plain.Production (NT.First);
         begin
            Result := Return_Type (Plain.Part (Prod.First));
            Result.Append ("_List");
            return Result;
         end;
      elsif Part.Is_Terminal_Reference then
         Result.Append ("Token");
      elsif Is_Ambiguous (Plain.Non_Terminal (Part.Denote)) then
         Result := To_Ada (Plain.Non_Terminal (Part.Denote).Name);
         Result.Append ("_Switch");
      else
         Result := To_Ada (Plain.Non_Terminal (Part.Denote).Name);
      end if;

      return Result;
   end Return_Type;

   ------------
   -- To_Ada --
   ------------

   function To_Ada
     (Text : League.Strings.Universal_String)
     return League.Strings.Universal_String
   is
      List : League.String_Vectors.Universal_String_Vector;
      Piece : League.Strings.Universal_String;
   begin
      List := Text.Split ('_');
      for J in 1 .. List.Length loop
         Piece := List.Element (J);
         Piece.Replace (1, 1, Piece.Slice (1, 1).To_Uppercase);
         List.Replace (J, Piece);
      end loop;

      return List.Join ('_');
   end To_Ada;

   --------------------
   -- Write_Nodes_NT --
   --------------------

   procedure Write_Nodes_NT
     (NT       : Gela.Grammars.Non_Terminal;
      Nodes_NT : in out Writer)
   is
      Impl_Count : Natural := 0;
   begin
      for J in Implement'Range (2) loop
         if Implement (NT.Index, J) then
            Nodes_NT.N ("with Gela.Nodes.");
            Nodes_NT.N (Plural (Plain.Non_Terminal (J).Name));
            Nodes_NT.P (";");
            Impl_Count := Impl_Count + 1;
         end if;
      end loop;

      Nodes_NT.N ("package Gela.Nodes.");
      Nodes_NT.N (Plural (NT.Name));
      Nodes_NT.P (" is");
      Nodes_NT.P;
      Nodes_NT.P ("   pragma Preelaborate;");

      if Is_Ambiguous (NT) then
         Nodes_NT.P ("   type Switch is interface and Node;");
         Nodes_NT.P;
         Nodes_NT.P ("   function Length");
         Nodes_NT.P ("     (Self    : access Switch;");
         Nodes_NT.P ("      Payload : Gela.Types.Payload)");
         Nodes_NT.P ("      return Natural is abstract;");
         Nodes_NT.P;
         Nodes_NT.P ("   function Element");
         Nodes_NT.P ("     (Self    : access Switch;");
         Nodes_NT.P ("      Payload : Gela.Types.Payload;");
         Nodes_NT.P ("      Index   : Positive)");
         Nodes_NT.N ("      return Gela.Nodes.");
         Nodes_NT.N (To_Ada (NT.Name));
         Nodes_NT.P (" is abstract;");
         Nodes_NT.P;
         Nodes_NT.P ("   type Object is interface and Switch;");
      elsif Impl_Count > 0 then
         Nodes_NT.N ("   type Object is interface and Node");

         for J in Implement'Range (2) loop
            if Implement (NT.Index, J) then
               Nodes_NT.P;
               Nodes_NT.N ("     and Gela.Nodes.");
               Nodes_NT.N (Plural (Plain.Non_Terminal (J).Name));
               Nodes_NT.N (".Object");
            end if;
         end loop;

         Nodes_NT.P (";");
      else
         Nodes_NT.P ("   type Object is interface and Node;");
      end if;

      Nodes_NT.P;
      Nodes_NT.P ("   type Object_Access is access all Object'Class;");
      Nodes_NT.P;

      if Is_Macro (NT.Index) then
         Write_Parts (Plain.Production (NT.First), Nodes_NT);
      end if;

      if Has_List (NT.Index) then
         Nodes_NT.P ("   type List is interface and Node;");
         Nodes_NT.P ("   type List_Access is access all List'Class;");
         Nodes_NT.P;
         Nodes_NT.P ("   not overriding function Head");
         Nodes_NT.P ("     (Self    : access List;");
         Nodes_NT.P ("      Payload : Gela.Types.Payload)");
         Nodes_NT.N ("      return Gela.Nodes.");
         Nodes_NT.N (To_Ada (NT.Name));
         Nodes_NT.P (" is abstract;");
         Nodes_NT.P;
         Nodes_NT.P ("   not overriding procedure Append");
         Nodes_NT.P ("     (Self     : access List;");
         Nodes_NT.P ("      Payload  : Gela.Types.Payload;");
         Nodes_NT.P ("      Item     : Gela.Nodes.Element) is abstract;");
         Nodes_NT.P;
         Nodes_NT.P ("   function Get_Payload");
         Nodes_NT.N ("     (Object  : Gela.Nodes.");
         Nodes_NT.N (To_Ada (NT.Name));
         Nodes_NT.P (") return Gela.Types.Payload is");
         Nodes_NT.P ("       (Object.Payload);");
      end if;

      Nodes_NT.N ("end Gela.Nodes.");
      Nodes_NT.N (Plural (NT.Name));
      Nodes_NT.P (";");
   end Write_Nodes_NT;

   -----------------
   -- Write_Parts --
   -----------------

   procedure Write_Parts
     (Prod     : Gela.Grammars.Production;
      Nodes_NT : in out Writer) is
   begin
      for Part of Plain.Part (Prod.First .. Prod.Last) loop
         Nodes_NT.N ("   function ");
         Nodes_NT.P (To_Ada (Part.Name));
         Nodes_NT.P ("     (Self    : access Object;");
         Nodes_NT.P ("      Payload : Gela.Types.Payload)");
         Nodes_NT.N ("      return Gela.Nodes.");

         Nodes_NT.N (Return_Type (Part));

         Nodes_NT.P (" is abstract;");

         Nodes_NT.P;
      end loop;
   end Write_Parts;

--  --     Token : constant Gela.Grammars.Production_Index :=
--  --       Plain.Last_Production + 1;

begin
--   Gela.Grammars_Debug.Print_Conflicts (Plain);

   for NT of Plain.Non_Terminal loop
      if NT.First = NT.Last then
         if Macro_Reference (Plain.Production (NT.First)) = 0 then
            Is_Macro (NT.Index) := True;
         end if;
      end if;
   end loop;

   for NT of Plain.Non_Terminal loop
      if not NT.Is_List then
         for Prod of Plain.Production (NT.First .. NT.Last) loop
            declare
               Ref : constant Gela.Grammars.Non_Terminal_Count :=
                 Macro_Reference (Prod);
            begin
               if Ref /= 0 then
                  Implement (Ref, NT.Index) := True;
               end if;
               Look_For_List (Prod);
            end;
         end loop;
      end if;
   end loop;

   Generate_Nodes;

   for NT of Plain.Non_Terminal loop
      if Has_List (NT.Index) then
         Generate_Stores_List (NT);
      end if;

      if not NT.Is_List then
         Generate_Nodes_NT (NT);

         if Is_Ambiguous (NT) then
            Generate_Stores_NT_Switch (NT);
         end if;

         for Prod of Plain.Production (NT.First .. NT.Last) loop
            Offset := Positive (Prod.First) - 1;
            Generate_Stores_Prod (NT, Prod);
         end loop;
      end if;
   end loop;

   Generate_Conv;
   Generate_Fabric;
end AG_Driver;
