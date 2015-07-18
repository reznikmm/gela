with Ada.Text_IO;

with League.String_Vectors;

with AG_Tools.Writers;
with AG_Tools.Input;

package body AG_Tools.Element_Generators is

   type NT_Map is array (Gela.Grammars.Non_Terminal_Index range <>)
     of Boolean;

   procedure Write_Parts
     (G        : Gela.Grammars.Grammar_Access;
      Prod     : Gela.Grammars.Production;
      Nodes_NT : in out Writers.Writer;
      Withes   : in out Writers.Writer;
      Withed   : in out League.String_Vectors.Universal_String_Vector);

   procedure Generate_Element
     (G  : Gela.Grammars.Grammar_Access;
      NT : Gela.Grammars.Non_Terminal);

   procedure Generate_Node
     (G  : Gela.Grammars.Grammar_Access;
      NT : Gela.Grammars.Non_Terminal);

   procedure Generate_Node_Factory (G  : Gela.Grammars.Grammar_Access);

   procedure Generate_Node_Sequence
     (NT : Gela.Grammars.Non_Terminal);

   procedure Generate_Constructor
     (G    : Gela.Grammars.Grammar_Access;
      NT   : Gela.Grammars.Non_Terminal;
      Prod : Gela.Grammars.Production;
      Spec : in out AG_Tools.Writers.Writer;
      Used : in out NT_Map);

   procedure Generate_Sequence_Constructor
     (G    : Gela.Grammars.Grammar_Access;
      NT   : Gela.Grammars.Non_Terminal;
      Spec : in out AG_Tools.Writers.Writer);

   function Return_Type_Image
     (G    : Gela.Grammars.Grammar_Access;
      Part : Gela.Grammars.Part)
         return League.Strings.Universal_String;

   procedure Write_Attr_With
     (G      : Gela.Grammars.Grammar_Access;
      NT     : Gela.Grammars.Non_Terminal;
      Output : in out AG_Tools.Writers.Writer;
      Done   : in out League.String_Vectors.Universal_String_Vector);

   procedure Write_Attr_Get
     (Decl   : Gela.Grammars.Attribute_Declaration;
      Output : in out AG_Tools.Writers.Writer;
      Name   : League.Strings.Universal_String;
      Impl   : Boolean);

   procedure Write_Attr_Set
     (Decl   : Gela.Grammars.Attribute_Declaration;
      Output : in out AG_Tools.Writers.Writer;
      Name   : League.Strings.Universal_String;
      Impl   : Boolean);

   --------------------------
   -- Generate_Constructor --
   --------------------------

   procedure Generate_Constructor
     (G    : Gela.Grammars.Grammar_Access;
      NT   : Gela.Grammars.Non_Terminal;
      Prod : Gela.Grammars.Production;
      Spec : in out AG_Tools.Writers.Writer;
      Used : in out NT_Map) is
   begin
      Spec.N ("overriding function ");
      Spec.P (To_Ada (NT.Name));
      Spec.N ("     (Self : in out Element_Factory");

      for Part of G.Part (Prod.First .. Prod.Last) loop
         Spec.P (";");
         Spec.N ("      ");
         Spec.N (To_Ada (Part.Name));
         Spec.N (" : ");
         Spec.N (Return_Type (G.all, Part));
         if not Part.Is_Terminal_Reference then
            Used (Part.Denote) := True;
            Spec.N ("_Access");
         end if;
      end loop;

      Spec.P (")");
      Spec.N ("      return ");
      Spec.N (Return_Type (G.all, NT));
      Spec.N ("_Access");
   end Generate_Constructor;

   ----------------------
   -- Generate_Element --
   ----------------------

   procedure Generate_Element
     (G  : Gela.Grammars.Grammar_Access;
      NT : Gela.Grammars.Non_Terminal)
   is
      use AG_Tools.Input;
      use type League.Strings.Universal_String;

      procedure Write_Attr
        (NT    : Gela.Grammars.Non_Terminal;
         Attrs : in out AG_Tools.Writers.Writer);

      Name   : constant League.Strings.Universal_String := To_Ada (NT.Name);

      ----------------
      -- Write_Attr --
      ----------------

      procedure Write_Attr
        (NT    : Gela.Grammars.Non_Terminal;
         Attrs : in out AG_Tools.Writers.Writer) is
      begin
         for A in NT.First_Attribute .. NT.Last_Attribute loop
            Write_Attr_Get (G.Declaration (A), Attrs, Name, False);
            Attrs.P (" is abstract;");
            Attrs.P;
            Write_Attr_Set (G.Declaration (A), Attrs, Name, False);
            Attrs.P (" is abstract;");
            Attrs.P;
         end loop;
      end Write_Attr;

      Withed : League.String_Vectors.Universal_String_Vector;
      Withes : AG_Tools.Writers.Writer;
      Nodes  : AG_Tools.Writers.Writer;
   begin
      for J in G.Non_Terminal'Range loop
         if Implement (NT.Index, J) then
            Withes.N ("with Gela.Elements.");
            Withes.N (Plural (G.Non_Terminal (J).Name));
            Withes.P (";");
            Withed.Append
              ("Gela.Elements." & Plural (G.Non_Terminal (J).Name));
         end if;
      end loop;

      Nodes.N ("package Gela.Elements.");
      Nodes.N (Plural (NT.Name));
      Nodes.P (" is");
      Nodes.P ("   pragma Preelaborate;");
      Nodes.P;
      Nodes.N ("   type ");
      Nodes.N (Name);
      Nodes.P (" is limited interface");
      Nodes.N ("     and Gela.Elements.Element");
      for J in G.Non_Terminal'Range loop
         if Implement (NT.Index, J) then
            Nodes.P;
            Nodes.N ("     and Gela.Elements.");
            Nodes.N (Plural (G.Non_Terminal (J).Name));
            Nodes.N (".");
            if G.Non_Terminal (J).Name.Length > 26 then
               Nodes.P;
               Nodes.N ("        ");
            end if;
            Nodes.N (To_Ada (G.Non_Terminal (J).Name));
         end if;
      end loop;

      Nodes.P (";");
      Nodes.P;
      Nodes.N ("   type ");
      Nodes.N (Name);
      Nodes.P ("_Access is");
      Nodes.N ("     access all ");
      Nodes.N (Name);
      Nodes.P ("'Class;");
      Nodes.N ("   for ");
      Nodes.N (Name);
      Nodes.P ("_Access'Storage_Size use 0;");
      Nodes.P;

      if Is_Concrete (NT.Index) then
         Write_Parts (G, G.Production (NT.First), Nodes, Withes, Withed);
      end if;

      Write_Attr_With (G, NT, Withes, Withed);
      Write_Attr (NT, Nodes);

      if Has_List (NT.Index) then
         Nodes.N ("   package ");
         Nodes.N (Name);
         Nodes.P ("_Sequences is");
         Nodes.P ("     new Generic_Element_Sequences");
         Nodes.N ("       (");
         Nodes.N (Name);
         Nodes.N (",");
         if Name.Length > 28 then
            Nodes.P;
            Nodes.N ("        ");
         else
            Nodes.N (" ");
         end if;

         Nodes.N (Name);
         Nodes.P ("_Access);");
         Nodes.P;
         Nodes.N ("   type ");
         Nodes.N (Name);
         Nodes.P ("_Sequence is limited interface");
         Nodes.N ("     and ");
         Nodes.N (Name);
         Nodes.P ("_Sequences.Sequence;");
         Nodes.N ("   type ");
         Nodes.N (Name);
         Nodes.P ("_Sequence_Access is");
         Nodes.N ("     access all ");
         Nodes.N (Name);
         Nodes.P ("_Sequence'Class;");
         Nodes.N ("   for ");
         Nodes.N (Name);
         Nodes.P ("_Sequence_Access'Storage_Size use 0;");
         Nodes.N ("   subtype ");
         Nodes.N (Name);
         Nodes.P ("_Sequence_Cursor is");
         Nodes.N ("     ");
         Nodes.N (Name);
         Nodes.P ("_Sequences.Sequence_Cursor'Class;");
         Nodes.P;
      end if;

      Nodes.N ("end Gela.Elements.");
      Nodes.N (Plural (NT.Name));
      Nodes.P (";");
      Nodes.P;

      Ada.Text_IO.Put_Line (Withes.Text.To_UTF_8_String);
      Ada.Text_IO.Put_Line (Nodes.Text.To_UTF_8_String);
   end Generate_Element;

   -----------------------
   -- Generate_Elements --
   -----------------------

   procedure Generate_Elements (G : Gela.Grammars.Grammar_Access) is
   begin
      for NT of G.Non_Terminal loop
         if not NT.Is_List then
            Generate_Element (G, NT);
            if Input.Is_Concrete (NT.Index) then
               Generate_Node (G, NT);
            end if;
            if Input.Has_List (NT.Index) then
               Generate_Node_Sequence (NT);
            end if;
         end if;
      end loop;
   end Generate_Elements;

   ---------------------
   -- Generate_Factory --
   ---------------------

   procedure Generate_Factory
     (G : Gela.Grammars.Grammar_Access)
   is
      Spec   : AG_Tools.Writers.Writer;
      Withes : AG_Tools.Writers.Writer;
      Used   : NT_Map (G.Non_Terminal'Range) := (others => False);
   begin
      Spec.P ("with Gela.Lexical_Types;");
      Spec.P;

      Spec.P ("package Gela.Element_Factories is");
      Spec.P ("   pragma Preelaborate;");
      Spec.P;
      Spec.P ("   type Element_Factory is limited interface;");
      Spec.P ("   type Element_Factory_Access is " &
                "access all Element_Factory'Class;");
      Spec.P ("   for Element_Factory_Access'Storage_Size use 0;");
      Spec.P;

      for NT of G.Non_Terminal loop
         if not NT.Is_List then
            for Prod of G.Production (NT.First .. NT.Last) loop
               if Input.Is_Concrete (NT.Index) then
                  Used (NT.Index) := True;
                  Spec.N ("   not ");
                  Generate_Constructor (G, NT, Prod, Spec, Used);
                  Spec.P;
                  Spec.P ("        is abstract;");
                  Spec.P;
               end if;
            end loop;
         end if;
      end loop;

      for NT of G.Non_Terminal loop
         if not NT.Is_List and then Input.Has_List (NT.Index) then
            Used (NT.Index) := True;
            Spec.N ("   not ");
            Generate_Sequence_Constructor (G, NT, Spec);
            Spec.P;
            Spec.P ("        is abstract;");
            Spec.P;
         end if;
      end loop;

      Spec.P ("end Gela.Element_Factories;");

      for NT of G.Non_Terminal loop
         if not NT.Is_List and Used (NT.Index) then
            Withes.N ("with Gela.Elements.");
            Withes.N (Plural (NT.Name));
            Withes.P (";");
         end if;
      end loop;

      Ada.Text_IO.Put_Line (Withes.Text.To_UTF_8_String);
      Ada.Text_IO.Put_Line (Spec.Text.To_UTF_8_String);
      Generate_Node_Factory (G);
   end Generate_Factory;

   -------------------
   -- Generate_Node --
   -------------------

   procedure Generate_Node
     (G  : Gela.Grammars.Grammar_Access;
      NT : Gela.Grammars.Non_Terminal)
   is
      use type League.Strings.Universal_String;
      use type Gela.Grammars.Part_Count;

      procedure Write_Attr
        (NT    : Gela.Grammars.Non_Terminal;
         Nodes : in out AG_Tools.Writers.Writer;
         Impl  : in out AG_Tools.Writers.Writer;
         Attrs : in out AG_Tools.Writers.Writer);

      Name   : constant League.Strings.Universal_String := To_Ada (NT.Name);
      Withed : League.String_Vectors.Universal_String_Vector;
      Withes : AG_Tools.Writers.Writer;

      ----------------
      -- Write_Attr --
      ----------------

      procedure Write_Attr
        (NT    : Gela.Grammars.Non_Terminal;
         Nodes : in out AG_Tools.Writers.Writer;
         Impl  : in out AG_Tools.Writers.Writer;
         Attrs : in out AG_Tools.Writers.Writer)
      is
         Pkg : League.Strings.Universal_String;
      begin
         for A in NT.First_Attribute .. NT.Last_Attribute loop
            declare
               Decl : Gela.Grammars.Attribute_Declaration renames
                 G.Declaration (A);
            begin
               Pkg := Package_Name (Decl.Type_Name);
               if Withed.Index (Pkg) = 0 then
                  Withed.Append (Pkg);
                  Withes.N ("with ");
                  Withes.N (Pkg);
                  Withes.P (";");
               end if;

               Nodes.N ("      ");
               Nodes.N (To_Ada (Decl.Name));
               Nodes.N (" : ");
               Nodes.N (To_Ada (Decl.Type_Name));
               Nodes.P (";");
               Write_Attr_Get (G.Declaration (A), Attrs, Name, True);
               Attrs.P (";");
               Attrs.P;
               Write_Attr_Get (G.Declaration (A), Impl, Name, True);
               Impl.P (" is");
               Impl.P ("   begin");
               Impl.N ("      return Self.");
               Impl.N (To_Ada (Decl.Name));
               Impl.P (";");
               Impl.N ("   end ");
               Impl.N (To_Ada (Decl.Name));
               Impl.P (";");
               Impl.P;
               Write_Attr_Set (G.Declaration (A), Attrs, Name, True);
               Attrs.P (";");
               Attrs.P;
               Write_Attr_Set (G.Declaration (A), Impl, Name, True);
               Impl.P (" is");
               Impl.P ("   begin");
               Impl.N ("      Self.");
               Impl.N (To_Ada (Decl.Name));
               Impl.P (" := Value;");
               Impl.N ("   end Set_");
               Impl.N (To_Ada (Decl.Name));
               Impl.P (";");
               Impl.P;
            end;
         end loop;
      end Write_Attr;

      Attrs  : AG_Tools.Writers.Writer;
      Nodes  : AG_Tools.Writers.Writer;
      Impl   : AG_Tools.Writers.Writer;
      Pkg    : constant League.Strings.Universal_String :=
        "Gela.Elements." & Plural (NT.Name);
      Prod   : Gela.Grammars.Production renames G.Production (NT.First);

      Part_Type    : League.Strings.Universal_String;
      Package_Name : League.Strings.Universal_String;
      Index : Positive := 1;
      Option : Boolean;
   begin
      Withed.Append (Pkg);
      Withes.N ("with ");
      Withes.N (Pkg);
      Withes.P (";");
      Withes.P ("with Gela.Element_Visiters;");

      Impl.P ("with Gela.LARL_Parsers_Nodes; use Gela.LARL_Parsers_Nodes;");
      Impl.P;
      Impl.N ("package body Gela.Nodes.");
      Nodes.N ("package Gela.Nodes.");
      Nodes.N (Plural (NT.Name), Impl);
      Nodes.P (" is", Impl);
      Nodes.P ("   pragma Preelaborate;");
      Nodes.P;
      Impl.P;
      Nodes.N ("   type ");
      Nodes.N (Name);
      Nodes.P (" is limited new");
      Nodes.N ("     Gela.Elements.");
      Nodes.N (Plural (NT.Name));
      Nodes.N (".");
      Nodes.P (Name);
      Nodes.N ("     and Gela.Elements.Set_Enclosing.Element");
      Nodes.P (" with private;");

      Nodes.P;
      Nodes.N ("   type ");
      Nodes.N (Name);
      Nodes.P ("_Access is");
      Nodes.N ("     access all ");
      Nodes.N (Name);
      Nodes.P (";");
      Nodes.P;
      Nodes.P ("   function Create", Impl);
      Nodes.N
        ("     (Comp   : Gela.Compilations.Compilation_Access", Impl);

      for Part of G.Part (Prod.First .. Prod.Last) loop
         Part_Type := Return_Type_Image (G, Part);
         Package_Name := AG_Tools.Package_Name (Part_Type);
         Nodes.P (";", Impl);
         Nodes.N ("     ", Impl);
         Nodes.N (To_Ada (Part.Name), Impl);
         Nodes.N (" : ", Impl);
         Nodes.N (Part_Type, Impl);
      end loop;

      Nodes.P (")", Impl);
      Nodes.N ("      return ", Impl);
      Nodes.N (Name, Impl);
      Nodes.P (";");
      Nodes.P;
      Impl.P (" is");
      Impl.P ("   begin");

      for Part of G.Part (Prod.First .. Prod.Last) loop
         if Part.Is_Non_Terminal_Reference or Part.Is_List_Reference then
            Index := Index + 1;
         end if;
      end loop;

      Impl.N ("      return Result : aliased ");

      if Index = 1 then
         Impl.N ("constant");
         if Name.Length >= 38 then
            Impl.P;
            Impl.N ("        ");
         else
            Impl.N (" ");
         end if;
      end if;

      Impl.N (Name);

      if Index = 1 and Name.Length >= 38 then
         Impl.P;
         Impl.N ("       ");
      end if;

      Impl.P (" :=");
      Impl.N ("        (Length                => ");
      Impl.N (Natural (Prod.Last - Prod.First + 1));
      Impl.P (",");
      Impl.P ("         Enclosing_Element     => null,");
      Impl.P ("         Enclosing_Compilation => Comp,");
      Impl.P ("         Is_Part_Of_Implicit   => False,");
      Impl.P ("         Is_Part_Of_Inherited  => False,");
      Impl.P ("         Is_Part_Of_Instance   => False,");
      Impl.P ("         Children              =>");
      Impl.N ("           (");

      Index := 1;

      for Part of G.Part (Prod.First .. Prod.Last) loop
         Part_Type := Return_Type_Image (G, Part);
         Package_Name := AG_Tools.Package_Name (Part_Type);
         if Index /= 1 then
            Impl.P (",");
            Impl.N ("            ");
         end if;

         Impl.N (Index);
         Impl.N (" => +");
         Impl.N (To_Ada (Part.Name));
         Index := Index + 1;
      end loop;

      Impl.P ("),");
      Impl.P ("         others => <>)");
      Impl.P ("      do");

      for Part of G.Part (Prod.First .. Prod.Last) loop
         if Part.Is_Non_Terminal_Reference or Part.Is_List_Reference then
            Option := AG_Tools.Input.Is_Option (G.all, Part);

            if Option then
               Impl.N ("         if ");
               Impl.N (To_Ada (Part.Name));
               Impl.P (".Assigned then");
               Impl.N ("   ");
            end if;

            Impl.P ("         Gela.Elements.Set_Enclosing.Element'Class");

            if Option then
               Impl.N ("   ");
            end if;

            Impl.N ("           (");
            Impl.N (To_Ada (Part.Name));
            Impl.P (".all).Set_Enclosing_Element");

            if Option then
               Impl.N ("   ");
            end if;

            Impl.P ("             (Result'Unchecked_Access);");

            if Option then
               Impl.P ("         end if;");
            end if;
         end if;
      end loop;

      Impl.P ("         null;");
      Impl.P ("      end return;");
      Impl.P ("   end Create;");
      Impl.P;

      Nodes.P ("private");
      Nodes.P;

      Nodes.N ("   type ");
      Nodes.N (Name);
      Nodes.P (" is");
      Nodes.N ("     limited new Node (Length => ");
      Nodes.N (Natural (Prod.Last - Prod.First + 1));
      Nodes.P (")");
      Nodes.N ("     and Gela.Elements.");
      Nodes.N (Plural (NT.Name));
      Nodes.N (".");
      Nodes.N (Name);
      Nodes.P (" with");
      Nodes.P ("   record");
      Write_Attr (NT, Nodes, Impl, Attrs);

      if Attrs.Text.Is_Empty then
         Nodes.P ("      null;");
      end if;

      Nodes.P ("   end record;");
      Nodes.P;

      for Part of G.Part (Prod.First .. Prod.Last) loop
         Part_Type := Return_Type_Image (G, Part);
         Package_Name := AG_Tools.Package_Name (Part_Type);

         if not Package_Name.Starts_With ("Gela.Lexical_Types")
           and Withed.Index (Package_Name) = 0
         then
            Withed.Append (Package_Name);
            Withes.N ("with ");
            Withes.N (Package_Name);
            Withes.P (";");
         end if;

         Nodes.N ("   overriding function ", Impl);
         Nodes.P (To_Ada (Part.Name), Impl);
         Nodes.N ("     (Self    : ", Impl);
         Nodes.N (Name, Impl);
         Nodes.P (")", Impl);
         Nodes.N ("      return ", Impl);
         Nodes.N (Part_Type, Impl);
         Nodes.P (";");
         Impl.P (" is");
         Impl.P ("   begin");
         Impl.N ("      return -Self.Children (");
         Impl.N (Natural (Part.Index - Prod.First + 1));
         Impl.P (");");
         Impl.N ("   end ");
         Impl.N (To_Ada (Part.Name));
         Impl.P (";");
         Impl.P;

         Nodes.P;
      end loop;

      Nodes.P ("   overriding procedure Visit", Impl);
      Nodes.N ("     (Self    : access ", Impl);
      Nodes.N (Name, Impl);
      Nodes.P (";", Impl);
      Nodes.N ("      Visiter : in out Gela.Element_Visiters.Visiter'Class)",
               Impl);
      Nodes.P (";");
      Nodes.P;
      Impl.P (" is");
      Impl.P ("   begin");
      Impl.N ("      Visiter.");
      Impl.N (Name);
      Impl.P (" (Self);");
      Impl.P ("   end Visit;");
      Impl.P;

      Attrs.P ("   overriding function Nested", Impl);
      Attrs.N ("     (Self : ", Impl);
      Attrs.N (Name, Impl);
      Attrs.P (")", Impl);
      Attrs.N ("      return Nested_Kind_Array", Impl);
      Impl.P;
      Impl.P ("   is");
      Impl.P ("      pragma Unreferenced (Self);");
      Impl.P ("   begin");
      Impl.N ("      return (");

      for Part of G.Part (Prod.First .. Prod.Last) loop
         Part_Type := Return_Type_Image (G, Part);

         if Part.Index /= Prod.First then
            Impl.P (",");
            Impl.N ("              ");
         end if;

         Impl.N (Natural (Part.Index - Prod.First + 1));
         Impl.N (" => ");

         if Part_Type.Ends_With ("Sequence_Access") then
            Impl.N ("Gela.Elements.Nested_Sequence");
         elsif Part_Type.Ends_With ("Token_Count") then
            Impl.N ("Gela.Elements.Nested_Token");
         else
            Impl.N ("Gela.Elements.Nested_Element");
         end if;
      end loop;

      Impl.P (");");
      Impl.P ("   end Nested;");
      Impl.P;

      Attrs.P (";");
      Attrs.P;

      Attrs.N ("end Gela.Nodes.", Impl);
      Attrs.N (Plural (NT.Name), Impl);
      Attrs.P (";", Impl);
      Ada.Text_IO.Put_Line (Withes.Text.To_UTF_8_String);
      Ada.Text_IO.Put_Line (Nodes.Text.To_UTF_8_String);
      Ada.Text_IO.Put_Line (Attrs.Text.To_UTF_8_String);
      Ada.Text_IO.Put_Line (Impl.Text.To_UTF_8_String);
   end Generate_Node;

   --------------------------
   -- Generate_Node_Factory --
   --------------------------

   procedure Generate_Node_Factory (G  : Gela.Grammars.Grammar_Access) is
      Spec   : AG_Tools.Writers.Writer;
      Withes : AG_Tools.Writers.Writer;
      Impl   : AG_Tools.Writers.Writer;
      Impl_With : AG_Tools.Writers.Writer;
      Used   : NT_Map (G.Non_Terminal'Range) := (others => False);
   begin
      Spec.P ("with Gela.Lexical_Types;");
      Spec.P ("with Gela.Element_Factories;");
      Spec.P ("with Gela.Compilations;");
      Spec.P;

      Impl.P ("package body Gela.Node_Factories is");
      Impl.P;
      Spec.P ("package Gela.Node_Factories is");
      Spec.P ("   pragma Preelaborate;");
      Spec.P;
      Spec.P ("   type Element_Factory " &
                "(Comp : Gela.Compilations.Compilation_Access) is");
      Spec.P ("      limited new Gela.Element_Factories.Element_Factory " &
                "with null record;");
      Spec.P ("   type Element_Factory_Access is " &
                "access all Element_Factory'Class;");
      Spec.P;

      for NT of G.Non_Terminal loop
         if not NT.Is_List then
            for Prod of G.Production (NT.First .. NT.Last) loop
               if Input.Is_Concrete (NT.Index) then
                  Used (NT.Index) := True;
                  Spec.N ("   ");
                  Generate_Constructor (G, NT, Prod, Spec, Used);
                  Spec.P (";");
                  Spec.P;
                  Impl.N ("   ");
                  Generate_Constructor (G, NT, Prod, Impl, Used);
                  Impl.P;
                  Impl.P ("   is");
                  Impl.N ("      Result : constant Gela.Nodes.");
                  Impl.N (Plural (NT.Name));
                  Impl.N (".");
                  Impl.N (To_Ada (NT.Name));
                  Impl.P ("_Access :=");
                  Impl.N ("        new Gela.Nodes.");
                  Impl.N (Plural (NT.Name));
                  Impl.N (".");
                  Impl.N (To_Ada (NT.Name));
                  Impl.P ("'");
                  Impl.N ("          (Gela.Nodes.");
                  Impl.N (Plural (NT.Name));
                  Impl.N (".Create (Self.Comp");

                  for Part of G.Part (Prod.First .. Prod.Last) loop
                     Impl.P (",");
                     Impl.N ("            ");
                     Impl.N (To_Ada (Part.Name));
                  end loop;

                  Impl.P ("));");
                  Impl.P ("   begin");
                  Impl.N ("      return ");
                  Impl.N (Return_Type (G.all, NT));
                  Impl.P ("_Access (Result);");
                  Impl.N ("   end ");
                  Impl.N (To_Ada (NT.Name));
                  Impl.P (";");
                  Impl.P;
                  Impl_With.N ("with Gela.Nodes.");
                  Impl_With.N (Plural (NT.Name));
                  Impl_With.P (";");
               end if;
            end loop;
         end if;
      end loop;

      for NT of G.Non_Terminal loop
         if not NT.Is_List and then Input.Has_List (NT.Index) then
            Used (NT.Index) := True;
            Spec.N ("   ");
            Generate_Sequence_Constructor (G, NT, Spec);
            Impl.N ("   ");
            Generate_Sequence_Constructor (G, NT, Impl);
            Impl.P;
            Impl.P ("   is");
            Impl.P ("      pragma Unreferenced (Self);");
            Impl.N ("      Result : constant Gela.Nodes.");
            Impl.N (To_Ada (NT.Name));
            Impl.P ("_Sequences.Sequence_Access :=");
            Impl.N ("        new Gela.Nodes.");
            Impl.N (To_Ada (NT.Name));
            Impl.P ("_Sequences.Sequence;");
            Impl.P ("   begin");
            Impl.N ("      return ");
            Impl.N (Return_Type (G.all, NT));
            Impl.P ("_Sequence_Access (Result);");
            Impl.N ("   end ");
            Impl.N (To_Ada (NT.Name));
            Impl.N ("_Sequence");
            Impl.P (";");
            Impl.P;
            Impl_With.N ("with Gela.Nodes.");
            Impl_With.N (To_Ada (NT.Name));
            Impl_With.P ("_Sequences;");

            Spec.P (";");
            Spec.P;
         end if;
      end loop;

      Spec.P ("end Gela.Node_Factories;", Impl);

      for NT of G.Non_Terminal loop
         if not NT.Is_List and Used (NT.Index) then
            Withes.N ("with Gela.Elements.");
            Withes.N (Plural (NT.Name));
            Withes.P (";");
         end if;
      end loop;

      Ada.Text_IO.Put_Line (Withes.Text.To_UTF_8_String);
      Ada.Text_IO.Put_Line (Spec.Text.To_UTF_8_String);
      Ada.Text_IO.Put_Line (Impl_With.Text.To_UTF_8_String);
      Ada.Text_IO.Put_Line (Impl.Text.To_UTF_8_String);
   end Generate_Node_Factory;

   ----------------------------
   -- Generate_Node_Sequence --
   ----------------------------

   procedure Generate_Node_Sequence
     (NT : Gela.Grammars.Non_Terminal)
   is
      Nodes  : AG_Tools.Writers.Writer;
   begin
      Nodes.N ("with Gela.Elements.");
      Nodes.N (Plural (NT.Name));
      Nodes.P (";");
      Nodes.N ("package Gela.Nodes.");
      Nodes.N (To_Ada (NT.Name));
      Nodes.P ("_Sequences is");
      Nodes.P ("  new Gela.Nodes.Node_Sequences");
      Nodes.N ("    (Gela.Elements.");
      Nodes.N (Plural (NT.Name));
      Nodes.N (".");
      Nodes.N (To_Ada (NT.Name));
      Nodes.P ("_Sequences,");
      Nodes.N ("     Gela.Elements.");
      Nodes.N (Plural (NT.Name));
      Nodes.N (".");
      Nodes.N (To_Ada (NT.Name));
      Nodes.P ("_Sequence);");
      Nodes.N ("pragma Preelaborate (Gela.Nodes.");
      Nodes.N (To_Ada (NT.Name));
      Nodes.P ("_Sequences);");

      Ada.Text_IO.Put_Line (Nodes.Text.To_UTF_8_String);
   end Generate_Node_Sequence;

   -----------------------------------
   -- Generate_Sequence_Constructor --
   -----------------------------------

   procedure Generate_Sequence_Constructor
     (G    : Gela.Grammars.Grammar_Access;
      NT   : Gela.Grammars.Non_Terminal;
      Spec : in out AG_Tools.Writers.Writer) is
   begin
      Spec.N ("overriding function ");
      Spec.N (To_Ada (NT.Name));
      Spec.P ("_Sequence");
      Spec.P ("     (Self : in out Element_Factory)");
      Spec.N ("      return ");
      Spec.N (Return_Type (G.all, NT));
      Spec.N ("_Sequence_Access");
   end Generate_Sequence_Constructor;

   -----------------------
   -- Return_Type_Image --
   -----------------------

   function Return_Type_Image
     (G    : Gela.Grammars.Grammar_Access;
      Part : Gela.Grammars.Part)
         return League.Strings.Universal_String
   is
      use type League.Strings.Universal_String;


      R : constant League.Strings.Universal_String :=
        Return_Type (G.all, Part);
   begin
      if Part.Is_Terminal_Reference then
         return R;
      else
         return R & "_Access";
      end if;
   end Return_Type_Image;

   ---------------------
   -- Write_Attr_With --
   ---------------------

   procedure Write_Attr_With
     (G     : Gela.Grammars.Grammar_Access;
      NT     : Gela.Grammars.Non_Terminal;
      Output : in out AG_Tools.Writers.Writer;
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
      Output : in out AG_Tools.Writers.Writer;
      Name   : League.Strings.Universal_String;
      Impl   : Boolean) is
   begin
      Output.N ("   ");
      if Impl then
         Output.N ("overriding ");
      end if;
      Output.N ("function ");
      Output.P (To_Ada (Decl.Name));
      Output.N ("     (Self    : ");
      Output.N (Name);
      Output.P (")");
      Output.N ("     return ");
      Output.N (To_Ada (Decl.Type_Name));
   end Write_Attr_Get;

   --------------------
   -- Write_Attr_Set --
   --------------------

   procedure Write_Attr_Set
     (Decl   : Gela.Grammars.Attribute_Declaration;
      Output : in out AG_Tools.Writers.Writer;
      Name   : League.Strings.Universal_String;
      Impl   : Boolean) is
   begin
      Output.N ("   ");
      if Impl then
         Output.N ("overriding ");
      end if;
      Output.N ("procedure Set_");
      Output.P (To_Ada (Decl.Name));
      Output.N ("     (Self    : in out ");
      Output.N (Name);
      Output.P (";");
      Output.N ("      Value   : ");
      Output.N (To_Ada (Decl.Type_Name));
      Output.N (")");
   end Write_Attr_Set;

   -----------------
   -- Write_Parts --
   -----------------

   procedure Write_Parts
     (G        : Gela.Grammars.Grammar_Access;
      Prod     : Gela.Grammars.Production;
      Nodes_NT : in out Writers.Writer;
      Withes   : in out Writers.Writer;
      Withed   : in out League.String_Vectors.Universal_String_Vector)
   is
      Name : constant League.Strings.Universal_String :=
        To_Ada (G.Non_Terminal (Prod.Parent).Name);
      Part_Type    : League.Strings.Universal_String;
      Package_Name : League.Strings.Universal_String;
   begin
      for Part of G.Part (Prod.First .. Prod.Last) loop
         Part_Type := Return_Type_Image (G, Part);
         Package_Name := AG_Tools.Package_Name (Part_Type);

         if not Package_Name.Starts_With ("Gela.Lexical_Types")
           and Withed.Index (Package_Name) = 0
         then
            Withed.Append (Package_Name);
            Withes.N ("limited with ");
            Withes.N (Package_Name);
            Withes.P (";");
         end if;

         Nodes_NT.N ("   function ");
         Nodes_NT.P (To_Ada (Part.Name));
         Nodes_NT.N ("     (Self    : ");
         Nodes_NT.N (Name);
         Nodes_NT.P (")");
         Nodes_NT.N ("      return ");
         Nodes_NT.N (Part_Type);
         Nodes_NT.P (" is abstract;");

         Nodes_NT.P;
      end loop;
   end Write_Parts;

end AG_Tools.Element_Generators;
