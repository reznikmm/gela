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

with AG_Tools; use AG_Tools;
with AG_Tools.Writers; use AG_Tools.Writers;
--  with Gela.Grammars_Debug;
with AG_Tools.Check_Ordered;

procedure AG_Driver is

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

   procedure Write_Parts
     (Prod     : Gela.Grammars.Production;
      Nodes_NT : in out Writer);

   procedure Generate_Nodes_NT
     (NT       : Gela.Grammars.Non_Terminal);

   function Macro_Reference
     (Prod : Gela.Grammars.Production)
      return Gela.Grammars.Non_Terminal_Count;
   --  If Prod is  just reference to another NT, return NT.Index.
   --  Return 0 otherwise.

   function List_Reference
     (Part : Gela.Grammars.Part)
      return Gela.Grammars.Non_Terminal_Count;
   --  If Part is reference to list, return NT.Index of the list
   --  Return 0 otherwise

   procedure Look_For_List (Prod : Gela.Grammars.Production);
   --  If any pert of Prod is reference to list of some NT then
   --  mark Has_List (NT) as True

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

   procedure Generate_Nodes;
   procedure Generate_Fabric;
   procedure Generate_Conv;
   procedure Generate_Stores_Prod
     (NT   : Gela.Grammars.Non_Terminal;
      Prod : Gela.Grammars.Production);
   procedure Generate_Stores_List (NT : Gela.Grammars.Non_Terminal);
   procedure Generate_Visiter;

   Name  : constant String := Ada.Command_Line.Argument (1);
   G     : constant Gela.Grammars.Grammar := Gela.Grammars.Reader.Read (Name);

   use AG_Tools.Check_Ordered;

   Is_Concrete : NT_List (G.Non_Terminal'Range) := (others => False);
   --  This is in form NT ::= child_1 child_2 ..;
   --  It has single production

   Has_List : array (G.Non_Terminal'Range) of Boolean := (others => False);
   --  If there are references to {NT} then Has_List (NT) = True

   Implement : NT_Map (G.Non_Terminal'Range, G.Non_Terminal'Range) :=
     (others => (others => False));
   --  If Implement (X, Y) then X implement Y

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

      for NT of G.Non_Terminal loop
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
                           " (X.its, X.Payload);");
            Conv_Spec.P;
            Conv_Spec.P ("   function ""-""");
            Conv_Spec.P ("     (X : Gela.Nodes.Element)");
            Conv_Spec.N ("      return Gela.Nodes.");
            Conv_Spec.N (To_Ada (NT.Name));
            Conv_Spec.N (" is");

            if NT.Name.Length > 29 then
               Conv_Spec.P;
               Conv_Spec.N ("        ");
            end if;

            Conv_Spec.P (" (X.its, X.Payload);");
            Conv_Spec.P;

            if Has_List (NT.Index) then
               Conv_Spec.P ("   function ""+""");
               Conv_Spec.N ("     (X : Gela.Nodes.");
               Conv_Spec.N (To_Ada (NT.Name));
               Conv_Spec.P ("_Sequence)");
               Conv_Spec.P ("      return Gela.Nodes.Element is" &
                              " (X.its, X.Payload);");
               Conv_Spec.P;
               Conv_Spec.P ("   function ""-""");
               Conv_Spec.P ("     (X : Gela.Nodes.Element)");
               Conv_Spec.N ("      return Gela.Nodes.");
               Conv_Spec.N (To_Ada (NT.Name));
               Conv_Spec.N ("_Sequence is");

               if NT.Name.Length > 20 then
                  Conv_Spec.P;
                  Conv_Spec.N ("        ");
               end if;

               Conv_Spec.P (" (X.its, X.Payload);");
               Conv_Spec.P;
            end if;
         end if;
      end loop;

      Conv_Spec.P ("   function ""+""");
      Conv_Spec.P ("     (X : Gela.Nodes.Token)");
      Conv_Spec.P ("      return Gela.Nodes.Element is" &
                     " (X.its, X.Payload);");
      Conv_Spec.P;
      Conv_Spec.P ("   function ""-""");
      Conv_Spec.P ("     (X : Gela.Nodes.Element)");
      Conv_Spec.N ("      return Gela.Nodes.Token");
      Conv_Spec.P (" is (X.its, X.Payload);");
      Conv_Spec.P;
      Conv_Spec.P ("end Gela.Nodes.Convertions;");
      Ada.Text_IO.Put_Line (Conv_With.Text.To_UTF_8_String);
      Ada.Text_IO.Put (Conv_Spec.Text.To_UTF_8_String);
   end Generate_Conv;

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

   --------------------
   -- Generate_Nodes --
   --------------------

   procedure Generate_Nodes is
      Nodes_With  : Writer;
      Nodes       : Writer;
   begin
      Nodes_With.P ("with Gela.Types;");
      Nodes_With.P ("limited with Gela.Nodes.Visiters;");
      Nodes_With.P ("limited with Gela.Nodes.Tokens;");

      Nodes.P ("package Gela.Nodes is");
      Nodes.P ("   pragma Preelaborate;");
      Nodes.P;
      Nodes.P ("   type Node is interface;");
      Nodes.P;
      Nodes.P ("   not overriding function Compilation");
      Nodes.P ("     (Self : Node) return Gela.Types.Compilation_Access" &
                 " is abstract;");
      Nodes.P;
      Nodes.P ("   type Element is record");
      Nodes.P ("      its     : access Node'Class;");
      Nodes.P ("      Payload : Gela.Types.Payload;");
      Nodes.P ("   end record;");
      Nodes.P ("   type Node_Access is access all Node'Class;");
      Nodes.P;
      Nodes.P ("   type Visitable_Node is interface and Node;");
      Nodes.P ("   type Visitable_Node_Access is " &
                 "access all Visitable_Node'Class;");
      Nodes.P ("   type Visitable_Element is record");
      Nodes.P ("      its     : access Visitable_Node'Class;");
      Nodes.P ("      Payload : Gela.Types.Payload;");
      Nodes.P ("   end record;");
      Nodes.P;
      Nodes.P ("   not overriding procedure Visit");
      Nodes.P ("     (Self    : access Visitable_Node;");
      Nodes.P ("      Payload : Gela.Types.Payload;");
      Nodes.P ("      Visiter : in out Gela.Nodes.Visiters.Visiter'Class)" &
                 " is abstract;");

      Nodes.P ("   type List_Node is interface and Node;");
      Nodes.P;
      Nodes.P ("   not overriding procedure Visit_Each");
      Nodes.P ("     (Self    : access List_Node;");
      Nodes.P ("      Payload : Gela.Types.Payload;");
      Nodes.P ("      Visiter : in out Gela.Nodes.Visiters.Visiter'Class)" &
                 " is abstract;");

      for NT of G.Non_Terminal loop
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

            if NT.Name.Length > 35 then
               Nodes.N (" access all ");
            else
               Nodes.N (" access all Gela.Nodes.");
            end if;

            Nodes.N (Plural (NT.Name));
            Nodes.P (".Object'Class;");
            Nodes.N ("   type ");
            Nodes.N (To_Ada (NT.Name));
            Nodes.P (" is record");
            Nodes.N ("      its     : ");
            Nodes.N (To_Ada (NT.Name));
            Nodes.P ("_Access;");
            Nodes.N ("      Payload : Gela.Types.Payload;");
            Nodes.P;
            Nodes.P ("   end record;");

            if Has_List (NT.Index) then
               Nodes.P;
               Nodes.N ("   type ");
               Nodes.N (To_Ada (NT.Name));
               Nodes.P ("_Sequence_Access is");
               Nodes.N ("     access all Gela.Nodes.");
               Nodes.N (Plural (NT.Name));
               Nodes.P (".List'Class;");
               Nodes.N ("   type ");
               Nodes.N (To_Ada (NT.Name));
               Nodes.P ("_Sequence is record");
               Nodes.N ("      its     : ");
               Nodes.N (To_Ada (NT.Name));
               Nodes.P ("_Sequence_Access;");
               Nodes.N ("      Payload : Gela.Types.Payload;");
               Nodes.P;
               Nodes.P ("   end record;");
            end if;
         end if;
      end loop;

      Nodes.P;
      Nodes.P ("   type Token_Access is access all" &
                 " Gela.Nodes.Tokens.Object'Class;");
      Nodes.P ("   type Token is record");
      Nodes.P ("      its     : Token_Access;");
      Nodes.P ("      Payload : Gela.Types.Payload;");
      Nodes.P ("   end record;");
      Nodes.P;
      Nodes.N ("end Gela.Nodes;");

      Ada.Text_IO.Put_Line (Nodes_With.Text.To_UTF_8_String);
      Ada.Text_IO.Put_Line (Nodes.Text.To_UTF_8_String);
   end Generate_Nodes;

   -----------------
   -- Return_Type --
   -----------------

   function Return_Type
     (Part : Gela.Grammars.Part)
      return League.Strings.Universal_String is
   begin
      return Return_Type (G, Part);
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

   -----------------------
   -- Generate_Nodes_NT --
   -----------------------

   procedure Generate_Nodes_NT
     (NT       : Gela.Grammars.Non_Terminal)
   is
      Nodes_NT   : Writer;
      Impl_Count : Natural := 0;
      Type_List  : League.String_Vectors.Universal_String_Vector;
   begin
      for J in Implement'Range (2) loop
         if Implement (NT.Index, J) then
            Nodes_NT.N ("with Gela.Nodes.");
            Nodes_NT.N (Plural (G.Non_Terminal (J).Name));
            Nodes_NT.P (";");
            Impl_Count := Impl_Count + 1;
         end if;
      end loop;

      Write_Attr_With (NT, Nodes_NT, Type_List);

      Nodes_NT.N ("package Gela.Nodes.");
      Nodes_NT.N (Plural (NT.Name));
      Nodes_NT.P (" is");
      Nodes_NT.P ("   pragma Preelaborate;");
      Nodes_NT.P;

      if Impl_Count > 0 then
         Nodes_NT.N ("   type Object is interface and Visitable_Node");

         for J in Implement'Range (2) loop
            if Implement (NT.Index, J) then
               Nodes_NT.P;
               Nodes_NT.N ("     and Gela.Nodes.");
               Nodes_NT.N (Plural (G.Non_Terminal (J).Name));
               Nodes_NT.N (".Object");
            end if;
         end loop;

         Nodes_NT.P (";");
      else
         Nodes_NT.P ("   type Object is interface and Visitable_Node;");
      end if;

      Nodes_NT.P;
      Nodes_NT.P ("   type Object_Access is access all Object'Class;");
      Nodes_NT.P;

      Type_List.Clear;

      for A in NT.First_Attribute .. NT.Last_Attribute loop
         Write_Attr_Get (G.Declaration (A), Nodes_NT, Impl => False);
         Nodes_NT.P (" is abstract;");
         Nodes_NT.P;
         Write_Attr_Set (G.Declaration (A), Nodes_NT, Impl => False);
         Nodes_NT.P (" is abstract;");
         Nodes_NT.P;
      end loop;

      if Is_Concrete (NT.Index) then
         Write_Parts (G.Production (NT.First), Nodes_NT);
      end if;

      if Has_List (NT.Index) then
         Nodes_NT.P ("   type List is interface and List_Node;");
         Nodes_NT.P ("   type List_Access is access all List'Class;");
         Nodes_NT.P;
         Nodes_NT.P ("   not overriding function Head");
         Nodes_NT.P ("     (Self    : access List;");
         Nodes_NT.P ("      Payload : Gela.Types.Payload)");
         Nodes_NT.N ("      return Gela.Nodes.");
         Nodes_NT.N (To_Ada (NT.Name));
         Nodes_NT.P (" is abstract;");
         Nodes_NT.P;
         Nodes_NT.P ("   not overriding procedure Next");
         Nodes_NT.P ("     (Self     : access List;");
         Nodes_NT.P ("      Payload  : Gela.Types.Payload;");
         Nodes_NT.N ("      Position : in out Gela.Nodes.");
         Nodes_NT.N (To_Ada (NT.Name));
         Nodes_NT.P (")");
         Nodes_NT.P ("     is abstract;");
         Nodes_NT.P;
         Nodes_NT.P ("   not overriding procedure Append");
         Nodes_NT.P ("     (Self     : access List;");
         Nodes_NT.P ("      Payload  : Gela.Types.Payload;");
         Nodes_NT.P ("      Item     : Gela.Nodes.Element) is abstract;");
         Nodes_NT.P;
         Nodes_NT.P ("   not overriding procedure Prepend");
         Nodes_NT.P ("     (Self     : access List;");
         Nodes_NT.P ("      Payload  : Gela.Types.Payload;");
         Nodes_NT.P ("      Item     : Gela.Nodes.Element) is abstract;");
         Nodes_NT.P;
         Nodes_NT.P ("   function Get_Payload");
         Nodes_NT.N ("     (Object  : Gela.Nodes.");
         Nodes_NT.N (To_Ada (NT.Name));
         Nodes_NT.P (")");
         Nodes_NT.P ("      return Gela.Types.Payload is (Object.Payload);");
      end if;

      Nodes_NT.N ("end Gela.Nodes.");
      Nodes_NT.N (Plural (NT.Name));
      Nodes_NT.P (";");

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

      for J in Implement'Range (2) loop
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

      for J in Implement'Range (2) loop
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
      Spec : Writer;
      Name : League.Strings.Universal_String;
   begin
      Spec.P ("package Gela.Nodes.Visiters is");
      Spec.P ("   pragma Preelaborate;");
      Spec.P;
      Spec.P ("   type Visiter is limited interface;");

      for NT of G.Non_Terminal loop
         for Prod of G.Production (NT.First .. NT.Last) loop
            if Is_Concrete (NT.Index) and not NT.Is_List then
               Name := To_Ada (NT.Name);
               Spec.P;
               Spec.N ("   not overriding procedure ");
               Spec.P (To_Ada (Name));
               Spec.P ("     (Self    : in out Visiter;");
               Spec.N ("      Node    : Gela.Nodes.");
               Spec.N (To_Ada (Name));
               Spec.P (")");
               Spec.P ("        is null;");
            end if;
         end loop;
      end loop;

      Spec.P;
      Spec.P ("end Gela.Nodes.Visiters;");
      Ada.Text_IO.Put_Line (Spec.Text.To_UTF_8_String);
   end Generate_Visiter;

   -------------
   -- Is_List --
   -------------

   function List_Reference
     (Part : Gela.Grammars.Part)
      return Gela.Grammars.Non_Terminal_Count is
   begin
      if Part.Is_List_Reference then
         declare
            NT   : Gela.Grammars.Non_Terminal renames
              G.Non_Terminal (Part.Denote);
            Prod : Gela.Grammars.Production renames
              G.Production (NT.First);
            Part : Gela.Grammars.Part renames G.Part (Prod.Last);
         begin
            return Part.Denote;
         end;
      end if;

      return 0;
   end List_Reference;

   -------------------
   -- Look_For_List --
   -------------------

   procedure Look_For_List (Prod : Gela.Grammars.Production) is
      List : Gela.Grammars.Non_Terminal_Count;
   begin
      for Part of G.Part (Prod.First .. Prod.Last) loop
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
         Part : Gela.Grammars.Part renames G.Part (Prod.First);
      begin
         if not Part.Is_Non_Terminal_Reference then
            return 0;
         elsif G.Non_Terminal (Part.Denote).Name /= Part.Name then
            return 0;
         else
            return Part.Denote;
         end if;
      end;
   end Macro_Reference;

   ---------------------
   -- Production_Unit --
   ---------------------

   function Non_Terminal_Unit
     (NT : Gela.Grammars.Non_Terminal)
      return League.Strings.Universal_String is
   begin
      return "Gela.Nodes." & Plural (NT.Name);
   end Non_Terminal_Unit;

   -----------------
   -- Write_Parts --
   -----------------

   procedure Write_Parts
     (Prod     : Gela.Grammars.Production;
      Nodes_NT : in out Writer) is
   begin
      for Part of G.Part (Prod.First .. Prod.Last) loop
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
--   Gela.Grammars_Debug.Print (G);

   for NT of G.Non_Terminal loop
      if NT.First = NT.Last then
         if Macro_Reference (G.Production (NT.First)) = 0 then
            Is_Concrete (NT.Index) := True;
         end if;
      else
         for Prod of G.Production (NT.First .. NT.Last) loop
            if Macro_Reference (G.Production (NT.First)) = 0 then
               Ada.Text_IO.Put_Line (NT.Name.To_UTF_8_String);
               raise Constraint_Error;
            end if;
         end loop;
      end if;
   end loop;

   for NT of G.Non_Terminal loop
      if not NT.Is_List then
         for Prod of G.Production (NT.First .. NT.Last) loop
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

   Check (G, Implement, Is_Concrete);

   Generate_Nodes;

   for NT of G.Non_Terminal loop
      if Has_List (NT.Index) then
         Generate_Stores_List (NT);
      end if;

      if not NT.Is_List then
         Generate_Nodes_NT (NT);

         for Prod of G.Production (NT.First .. NT.Last) loop
            Offset := Positive (Prod.First) - 1;
            Generate_Stores_Prod (NT, Prod);
         end loop;
      end if;
   end loop;

   Generate_Conv;
   Generate_Fabric;
   Generate_Visiter;
end AG_Driver;
