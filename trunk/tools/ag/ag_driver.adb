
with Ada.Command_Line;
with Ada.Text_IO;

with League.Strings;
with League.String_Vectors;

with Gela.Grammars;
with Gela.Grammars.Reader;
with Gela.Grammars_Convertors;

procedure AG_Driver is

   use type Gela.Grammars.Production_Index;
   use type League.Strings.Universal_String;

   package Writers is

      type Writer is tagged record
         Text : League.Strings.Universal_String;
      end record;

      procedure P
        (Self   : in out Writer;
         Text   : Wide_Wide_String := "");

      procedure N
        (Self : in out Writer;
         Text : Wide_Wide_String);

      procedure P
        (Self   : in out Writer;
         Text   : League.Strings.Universal_String);

      procedure N
        (Self : in out Writer;
         Text : League.Strings.Universal_String);

      procedure P
        (Self : in out Writer;
         Text : Wide_Wide_String := "";
         Copy : in out Writer'Class);

      procedure N
        (Self : in out Writer;
         Text : Wide_Wide_String;
         Copy : in out Writer'Class);

      procedure P
        (Self   : in out Writer;
         Text   : League.Strings.Universal_String;
         Copy : in out Writer'Class);

      procedure N
        (Self : in out Writer;
         Text : League.Strings.Universal_String;
         Copy : in out Writer'Class);

   end Writers;

   function To_Ada
     (Text : League.Strings.Universal_String)
      return League.Strings.Universal_String;

   function Plural
     (Text : League.Strings.Universal_String)
      return League.Strings.Universal_String;

   function Is_Ambiguous (NT : Gela.Grammars.Non_Terminal) return Boolean;

   function Image (X : Integer) return Wide_Wide_String;

   -----------
   -- Image --
   -----------

   function Image (X : Integer) return Wide_Wide_String is
      Img : constant Wide_Wide_String := Integer'Wide_Wide_Image (X);
   begin
      return Img (2 .. Img'Last);
   end Image;

   package body Writers is

      New_Line : constant Wide_Wide_Character := Wide_Wide_Character'Val (10);

      procedure N (Self : in out Writer; Text : Wide_Wide_String) is
      begin
         Self.Text.Append (Text);
      end N;

      procedure N
        (Self : in out Writer;
         Text : League.Strings.Universal_String) is
      begin
         Self.N (Text.To_Wide_Wide_String);
      end N;

      procedure N
        (Self : in out Writer;
         Text : Wide_Wide_String;
         Copy : in out Writer'Class) is
      begin
         Self.N (Text);
         Copy.N (Text);
      end N;

      procedure N
        (Self : in out Writer;
         Text : League.Strings.Universal_String;
         Copy : in out Writer'Class) is
      begin
         Self.N (Text);
         Copy.N (Text);
      end N;

      procedure P
        (Self : in out Writer;
         Text : Wide_Wide_String := "";
         Copy : in out Writer'Class) is
      begin
         Self.P (Text);
         Copy.P (Text);
      end P;

      procedure P
        (Self   : in out Writer;
         Text   : League.Strings.Universal_String;
         Copy : in out Writer'Class) is
      begin
         Self.P (Text);
         Copy.P (Text);
      end P;

      procedure P
        (Self   : in out Writer;
         Text   : League.Strings.Universal_String) is
      begin
         Self.P (Text.To_Wide_Wide_String);
      end P;

      procedure P
        (Self   : in out Writer;
         Text   : Wide_Wide_String := "") is
      begin
         Self.Text.Append (Text);
         Self.Text.Append (New_Line);
      end P;

   end Writers;

   ------------------
   -- Is_Ambiguous --
   ------------------

   function Is_Ambiguous (NT : Gela.Grammars.Non_Terminal) return Boolean is
      pragma Unreferenced (NT);
   begin
      return True;
   end Is_Ambiguous;

   ------------
   -- Plural --
   ------------

   function Plural
     (Text : League.Strings.Universal_String)
      return League.Strings.Universal_String is
   begin
      return To_Ada (Text) & "s";
   end Plural;

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

   use Writers;

   Name  : constant String := Ada.Command_Line.Argument (1);
   G     : constant Gela.Grammars.Grammar := Gela.Grammars.Reader.Read (Name);
   Plain : constant Gela.Grammars.Grammar :=
     Gela.Grammars_Convertors.Convert_With_Empty (G);

--  --     Token : constant Gela.Grammars.Production_Index :=
--  --       Plain.Last_Production + 1;

   Reserved : constant := 3;  --  Tag, Count

   Return_Type : League.Strings.Universal_String;
   Offset      : Natural;

   With_Clause : Writer;
   Types       : Writer;
   Stores_NT   : Writer;
   Store_Each  : Writer;
   Store_Body  : Writer;
   Nodes_NT    : Writer;
   Fab_With    : Writer;
   Fabrics     : Writer;

   use type Gela.Grammars.Part_Count;
begin
   With_Clause.P ("with Gela.Types;");
   With_Clause.P ("limited with Gela.Tokens;");

   Types.P ("package Gela.Nodes is");
   Types.P ("");
   Types.P ("   type Node is interface;");
   Types.P ("   type Node_Access is access all Node'Class;");

   Fabrics.P ("with Gela.Mutables;");
   Fabrics.P ("with Gela.Stores.Tokens;");
   Fabrics.P;
   Fabrics.P ("package Gela.Stores.Base_Fabrics is");
   Fabrics.P;
   Fabrics.P ("   type Base_Fabric (Compilation : " &
                "Gela.Mutables.Mutable_Compilation_Access) is");
   Fabrics.P ("   tagged limited record");
   Fabrics.P ("      Token : aliased Tokens.Token (Compilation);");

   for NT of Plain.Non_Terminal loop
      With_Clause.N ("limited with Gela.Nodes.");
      With_Clause.N (Plural (NT.Name));
      With_Clause.P (";");
      Types.P;
      Types.N ("   type ");
      Types.N (To_Ada (NT.Name));
      Types.N ("_Access is access all Gela.Nodes.");
      Types.N (Plural (NT.Name));
      Types.P (".Object;");
      Types.N ("   type ");
      Types.N (To_Ada (NT.Name));
      Types.P (" is record");
      Types.N ("      Object  : ");
      Types.N (To_Ada (NT.Name));
      Types.P ("_Access;");
      Types.N ("      Payload : Gela.Types.Payload;");
      Types.P;
      Types.P ("   end record;");

      Nodes_NT.N ("package Gela.Nodes.");
      Nodes_NT.N (Plural (NT.Name));
      Nodes_NT.P (" is");
      Nodes_NT.P ("");

      if Is_Ambiguous (NT) then
         Types.P;
         Types.N ("   type ");
         Types.N (To_Ada (NT.Name));
         Types.N ("_Switch_Access is access all Gela.Nodes.");
         Types.N (Plural (NT.Name));
         Types.P (".Switch;");
         Types.N ("   type ");
         Types.N (To_Ada (NT.Name));
         Types.P ("_Switch is record");
         Types.N ("      Object  : ");
         Types.N (To_Ada (NT.Name));
         Types.P ("_Switch_Access;");
         Types.N ("      Payload : Gela.Types.Payload;");
         Types.P;
         Types.P ("   end record;");
         Types.P;
         Nodes_NT.P ("   type Switch is interface and Node;");
         Nodes_NT.P ("");
         Nodes_NT.P ("   function Length");
         Nodes_NT.P ("     (Self    : access Switch;");
         Nodes_NT.P ("      Payload : Gela.Types.Payload)");
         Nodes_NT.P ("      return Natural is abstract;");
         Nodes_NT.P ("");
         Nodes_NT.P ("   function Element");
         Nodes_NT.P ("     (Self    : access Switch;");
         Nodes_NT.P ("      Payload : Gela.Types.Payload;");
         Nodes_NT.P ("      Index   : Positive)");
         Nodes_NT.N ("      return Gela.Nodes.");
         Nodes_NT.N (To_Ada (NT.Name));
         Nodes_NT.P (" is abstract;");
         Nodes_NT.P ("");
         Nodes_NT.P ("   type Object is interface and Switch;");

         Store_Body.P ("with Gela.Mutables.Compilations;");
         Store_Body.P;
         Store_Body.N ("package body Gela.Stores.");
         Store_Body.N (Plural (NT.Name));
         Store_Body.P (" is");
         Store_Body.P;
         Store_Body.P ("   Size_Offset : constant := 2;");
         Store_Body.P;

      else
         Nodes_NT.P ("   type Object is interface and Node;");
      end if;

      Nodes_NT.P ("");
      Nodes_NT.N ("end Gela.Nodes.");
      Nodes_NT.N (Plural (NT.Name));
      Nodes_NT.P (";");

      Stores_NT.N ("with Gela.Nodes.");
      Stores_NT.N (Plural (NT.Name));
      Stores_NT.P (";");
      Stores_NT.P ("with Gela.Stores.Productions;");
      Stores_NT.P ("with Gela.Types;");

      if Is_Ambiguous (NT) then
         Stores_NT.P ("with Gela.Stores.Nodes;");
      end if;

      Stores_NT.P;
      Stores_NT.N ("package Gela.Stores.");
      Stores_NT.N (Plural (NT.Name));
      Stores_NT.P (" is");
      Stores_NT.P ("");
      Stores_NT.N ("   type Object is ");
      Stores_NT.P ("abstract new Gela.Stores.Productions.Production");
      Stores_NT.N ("       and Gela.Nodes.");
      Stores_NT.N (Plural (NT.Name));
      Stores_NT.N (".Object");
      Stores_NT.P (" with null record;");
      Stores_NT.P ("");

      if Is_Ambiguous (NT) then
         Stores_NT.P ("   overriding function Element", Store_Body);
         Stores_NT.P ("     (Self    : access Object;", Store_Body);
         Stores_NT.P ("      Payload : Gela.Types.Payload;", Store_Body);
         Stores_NT.P ("      Index   : Positive)", Store_Body);
         Stores_NT.N ("      return Gela.Nodes.", Store_Body);
         Stores_NT.N (To_Ada (NT.Name), Store_Body);
         Stores_NT.N (";");
         Stores_NT.P ("", Store_Body);
         Store_Body.P ("   is");
         Store_Body.P ("      pragma Unreferenced (Index);");
         Store_Body.P ("   begin");
         Store_Body.N ("      return (Gela.Nodes.");
         Store_Body.N (To_Ada (NT.Name));
         Store_Body.P ("_Access (Self), Payload);");
         Store_Body.P ("   end Element;");
         Stores_NT.P ("", Store_Body);

         Stores_NT.P ("   overriding function Length", Store_Body);
         Stores_NT.P ("     (Self    : access Object;", Store_Body);
         Stores_NT.P ("      Payload : Gela.Types.Payload)", Store_Body);
         Stores_NT.N ("      return Natural", Store_Body);
         Stores_NT.N (";");
         Stores_NT.P ("", Store_Body);
         Store_Body.P ("   is");
         Store_Body.P ("      pragma Unreferenced (Self);");
         Store_Body.P ("      pragma Unreferenced (Payload);");
         Store_Body.P ("   begin");
         Store_Body.P ("      return 1;");
         Store_Body.P ("   end Length;");
         Stores_NT.P ("", Store_Body);

         Stores_NT.P ("   type Switch is new Gela.Stores.Nodes.Node");
         Stores_NT.N ("       and Gela.Nodes.");
         Stores_NT.N (Plural (NT.Name));
         Stores_NT.P (".Switch with null record;");
         Stores_NT.P;
         Stores_NT.P ("   overriding function Size", Store_Body);
         Stores_NT.P ("     (Self    : access Switch;", Store_Body);
         Stores_NT.N ("      Payload : Gela.Types.Payload) return Natural",
                      Store_Body);
         Stores_NT.P (";");
         Stores_NT.P ("", Store_Body);

         Store_Body.P ("   is");
         Store_Body.P ("      Size_Index : constant Stores.Index :=");
         Store_Body.P ("        Stores.Index (Payload) + Size_Offset;");
         Store_Body.P ("      Data : constant Gela.Stores.Element :=");
         Store_Body.P ("        Self.Compilation.Store.Get (Size_Index);");
         Store_Body.P ("   begin");
         Store_Body.P ("      return Natural (Data);");
         Store_Body.P ("   end Size;");
         Store_Body.P;

         Stores_NT.P ("   overriding function Last_Child", Store_Body);
         Stores_NT.P ("     (Self    : access Switch;", Store_Body);
         Stores_NT.N ("      Payload : Gela.Types.Payload) return Natural",
                      Store_Body);
         Stores_NT.P (";");
         Store_Body.P (" is");
         Store_Body.P ("   begin");
         Store_Body.N ("      return Size (Self, Payload) - ");
         Store_Body.N (Image (Reserved));
         Store_Body.P (";");
         Store_Body.P ("   end Last_Child;");

         Stores_NT.P ("", Store_Body);
         Stores_NT.P ("   overriding function Length", Store_Body);
         Stores_NT.P ("     (Self    : access Switch;", Store_Body);
         Stores_NT.P ("      Payload : Gela.Types.Payload)", Store_Body);
         Stores_NT.N ("      return Natural", Store_Body);
         Stores_NT.P (";");

         Store_Body.P (" is");
         Store_Body.P ("   begin");
         Store_Body.P ("      return Last_Child (Self, Payload);");
         Store_Body.P ("   end Length;");

         Stores_NT.P ("", Store_Body);
         Stores_NT.P ("   overriding function Element", Store_Body);
         Stores_NT.P ("     (Self    : access Switch;", Store_Body);
         Stores_NT.P ("      Payload : Gela.Types.Payload;", Store_Body);
         Stores_NT.P ("      Index   : Positive)", Store_Body);
         Stores_NT.N ("      return Gela.Nodes.", Store_Body);
         Stores_NT.N (To_Ada (NT.Name), Store_Body);
         Stores_NT.P (";");

         Store_Body.P;
         Store_Body.P ("   is");
         Store_Body.P ("      Id   : constant Gela.Types.Payload :=" &
                         " Self.Child (Payload, Index);");
         Store_Body.P ("      Node : constant Gela.Nodes.Node_Access :=");
         Store_Body.P ("        Self.Compilation.Fabric.To_Node (Id);");
         Store_Body.P ("   begin");
         Store_Body.N ("      return (Gela.Nodes.");
         Store_Body.N (To_Ada (NT.Name));
         Store_Body.P ("_Access (Node), Id);");
         Store_Body.P ("   end Element;");

         Stores_NT.P ("", Store_Body);

         Fabrics.N ("      S");
         Fabrics.N (Image (Positive (NT.Index)));
         Fabrics.N (" : aliased ");
         Fabrics.N (Plural (NT.Name));
         Fabrics.P (".Switch (Compilation);");

         Store_Body.N ("end Gela.Stores.");
         Store_Body.N (Plural (NT.Name));
         Store_Body.P (";");
      end if;

      Stores_NT.N ("end Gela.Stores.");
      Stores_NT.N (Plural (NT.Name));
      Stores_NT.P (";");

      for Prod of Plain.Production (NT.First .. NT.Last) loop
         Offset := Positive (Prod.First) - 1;

         Fab_With.N ("with Gela.Stores.");
         Fab_With.N (Plural (NT.Name));
         Fab_With.N (".");
         Fab_With.N (Plural (Prod.Name));
         Fab_With.P (";");

         Fabrics.N ("      P");
         Fabrics.N (Image (Positive (Prod.Index)));
         Fabrics.N (" : aliased ");
         Fabrics.N (Plural (NT.Name));
         Fabrics.N (".");
         Fabrics.N (Plural (Prod.Name));
         Fabrics.P (".Object (Compilation);");

         Nodes_NT.N ("package Gela.Nodes.");
         Nodes_NT.N (Plural (NT.Name));
         Nodes_NT.N (".");
         Nodes_NT.N (Plural (Prod.Name));
         Nodes_NT.P (" is");
         Nodes_NT.P ("");
         Nodes_NT.N ("   type Object is interface and Gela.Nodes.");
         Nodes_NT.N (Plural (NT.Name));
         Nodes_NT.P (".Object;");
         Nodes_NT.P ("");

         Store_Each.P ("with Gela.Types;");
         Store_Each.N ("with Gela.Nodes.");
         Store_Each.N (Plural (NT.Name));
         Store_Each.N (".");
         Store_Each.N (Plural (Prod.Name));
         Store_Each.P (";");
         Store_Each.P;
         Store_Each.N ("package Gela.Stores.");

         Store_Body.P ("with Gela.Mutables.Compilations;");
         Store_Body.P;

         Store_Body.N ("package body Gela.Stores.");
         Store_Each.N (Plural (NT.Name), Store_Body);
         Store_Each.N (".", Store_Body);
         Store_Each.N (Plural (Prod.Name), Store_Body);
         Store_Each.P (" is", Store_Body);
         Store_Each.P ("", Store_Body);

         Store_Each.N ("   type Object is new Gela.Stores.");
         Store_Each.N (Plural (NT.Name));
         Store_Each.P (".Object");
         Store_Each.N ("     and Gela.Nodes.");
         Store_Each.N (Plural (NT.Name));
         Store_Each.N (".");
         Store_Each.N (Plural (Prod.Name));
         Store_Each.P (".Object with null record;");
         Store_Each.P;
         Store_Each.P ("   overriding function Size", Store_Body);
         Store_Each.P ("     (Self    : access Object;", Store_Body);
         Store_Each.N ("      Payload : Gela.Types.Payload) return Natural",
                       Store_Body);

         Store_Body.P (" is");
         Store_Body.P ("   begin");
         Store_Body.N ("      return ");
         Store_Body.N
           (Image (Reserved + Natural (Prod.Last - Prod.First) + 1));
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
         Store_Body.N (Image (Natural (Prod.Last - Prod.First) + 1));
         Store_Body.P (";");
         Store_Body.N ("   end Last_Child");
         Store_Each.P (";", Store_Body);
         Store_Each.P ("", Store_Body);

         for Part of Plain.Part (Prod.First .. Prod.Last) loop
            Nodes_NT.N ("   function ");
            Nodes_NT.P (To_Ada (Part.Name));
            Nodes_NT.P ("     (Self    : access Object;");
            Nodes_NT.P ("      Payload : Gela.Types.Payload)");
            Nodes_NT.N ("      return Gela.Nodes.");

            Return_Type.Clear;

            if Part.Is_Terminal_Reference then
               Return_Type.Append ("Token");
            elsif Is_Ambiguous (Plain.Non_Terminal (Part.Denote)) then
               Return_Type := To_Ada (Plain.Non_Terminal (Part.Denote).Name);
               Return_Type.Append ("_Switch");
            else
               Return_Type := To_Ada (Plain.Non_Terminal (Part.Denote).Name);
            end if;

            Nodes_NT.N (Return_Type);

            Nodes_NT.P (" is abstract;");

            Nodes_NT.P ("");

            Store_Each.N ("   overriding function ", Store_Body);
            Store_Each.P (To_Ada (Part.Name), Store_Body);
            Store_Each.P ("     (Self    : access Object;", Store_Body);
            Store_Each.P ("      Payload : Gela.Types.Payload)", Store_Body);
            Store_Each.N ("      return Gela.Nodes.", Store_Body);
            Store_Each.N (Return_Type, Store_Body);
            Store_Each.P (";");
            Store_Body.P;
            Store_Body.P ("   is");
            Store_Body.N ("      Id   : constant Gela.Types.Payload " &
                            ":= Self.Child (Payload, ");
            Store_Body.N (Image (Positive (Part.Index) - Offset));
            Store_Body.P (");");
            Store_Body.P ("      Node : constant Gela.Nodes.Node_Access :=");
            Store_Body.P ("        Self.Compilation.Fabric.To_Node (Id);");
            Store_Body.P ("   begin");
            Store_Body.N ("      return (Gela.Nodes.");
            Store_Body.N (Return_Type);
            Store_Body.P ("_Access (Node), Id);");
            Store_Body.N ("   end ");
            Store_Body.N (To_Ada (Part.Name));
            Store_Body.P (";");

            Store_Each.P ("", Store_Body);
         end loop;

         Nodes_NT.N ("end Gela.Nodes.");
         Nodes_NT.N (Plural (NT.Name));
         Nodes_NT.N (".");
         Nodes_NT.N (Plural (Prod.Name));
         Nodes_NT.P (";");

         Store_Each.N ("end Gela.Stores.", Store_Body);
         Store_Each.N (Plural (NT.Name), Store_Body);
         Store_Each.N (".", Store_Body);
         Store_Each.N (Plural (Prod.Name), Store_Body);
         Store_Each.N (";", Store_Body);
      end loop;
   end loop;

   Fabrics.P ("   end record;");
   Fabrics.P;
   Fabrics.N ("end Gela.Stores.Base_Fabrics;");

   Types.P ("   type Token_Access is access all" &
                           " Gela.Tokens.Token'Class;");
   Types.P ("   type Token is record");
   Types.P ("      Object  : Token_Access;");
   Types.P ("      Payload : Gela.Types.Payload;");
   Types.P ("   end record;");
   Types.P ("");
   Types.N ("end Gela.Nodes;");

   Ada.Text_IO.Put (Stores_NT.Text.To_UTF_8_String);
   Ada.Text_IO.Put (Nodes_NT.Text.To_UTF_8_String);
--   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line (With_Clause.Text.To_UTF_8_String);
   Ada.Text_IO.Put (Types.Text.To_UTF_8_String);
   Ada.Text_IO.Put (Store_Each.Text.To_UTF_8_String);
   Ada.Text_IO.Put (Store_Body.Text.To_UTF_8_String);
   Ada.Text_IO.Put (Fab_With.Text.To_UTF_8_String);
   Ada.Text_IO.Put (Fabrics.Text.To_UTF_8_String);
end AG_Driver;
