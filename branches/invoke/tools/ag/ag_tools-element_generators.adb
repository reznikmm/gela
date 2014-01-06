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
      Spec.N ("   not overriding function ");
      Spec.P (To_Ada (NT.Name));
      Spec.N ("     (Self : in out Element_Fabric");

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
      Spec.P ("_Access");
      Spec.P ("        is abstract;");
      Spec.P;
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
      Withed : League.String_Vectors.Universal_String_Vector;
      Withes : AG_Tools.Writers.Writer;
      Nodes  : AG_Tools.Writers.Writer;
      Name   : constant League.Strings.Universal_String := To_Ada (NT.Name);
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
         end if;
      end loop;
   end Generate_Elements;

   ---------------------
   -- Generate_Fabric --
   ---------------------

   procedure Generate_Fabric
     (G : Gela.Grammars.Grammar_Access)
   is
      Spec   : AG_Tools.Writers.Writer;
      Withes : AG_Tools.Writers.Writer;
      Used   : NT_Map (G.Non_Terminal'Range) := (others => False);
   begin
      Spec.P ("with Gela.Lexical_Types;");
      Spec.P;

      Spec.P ("package Gela.Element_Fabrics is");
      Spec.P ("   pragma Preelaborate;");
      Spec.P;
      Spec.P ("   type Element_Fabric is limited interface;");
      Spec.P
        ("   type Element_Fabric_Access is access all Element_Fabric'Class;");
      Spec.P ("   for Element_Fabric_Access'Storage_Size use 0;");
      Spec.P;

      for NT of G.Non_Terminal loop
         if not NT.Is_List then
            for Prod of G.Production (NT.First .. NT.Last) loop
               if Input.Is_Concrete (NT.Index) then
                  Used (NT.Index) := True;
                  Generate_Constructor (G, NT, Prod, Spec, Used);
               end if;
            end loop;
         end if;
      end loop;

      for NT of G.Non_Terminal loop
         if not NT.Is_List and then Input.Has_List (NT.Index) then
            Used (NT.Index) := True;
            Generate_Sequence_Constructor (G, NT, Spec);
         end if;
      end loop;

      Spec.P ("end Gela.Element_Fabrics;");

      for NT of G.Non_Terminal loop
         if not NT.Is_List and Used (NT.Index) then
            Withes.N ("with Gela.Elements.");
            Withes.N (Plural (NT.Name));
            Withes.P (";");
         end if;
      end loop;

      Ada.Text_IO.Put_Line (Withes.Text.To_UTF_8_String);
      Ada.Text_IO.Put_Line (Spec.Text.To_UTF_8_String);
   end Generate_Fabric;

   -----------------------------------
   -- Generate_Sequence_Constructor --
   -----------------------------------

   procedure Generate_Sequence_Constructor
     (G    : Gela.Grammars.Grammar_Access;
      NT   : Gela.Grammars.Non_Terminal;
      Spec : in out AG_Tools.Writers.Writer) is
   begin
      Spec.N ("   not overriding function ");
      Spec.N (To_Ada (NT.Name));
      Spec.P ("_Sequence");
      Spec.P ("     (Self : in out Element_Fabric)");
      Spec.N ("      return ");
      Spec.N (Return_Type (G.all, NT));
      Spec.P ("_Sequence_Access");
      Spec.P ("        is abstract;");
      Spec.P;
   end Generate_Sequence_Constructor;

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
      function Return_Type
        (Part : Gela.Grammars.Part)
         return League.Strings.Universal_String;

      -----------------
      -- Return_Type --
      -----------------

      function Return_Type
        (Part : Gela.Grammars.Part)
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
      end Return_Type;

      Name : constant League.Strings.Universal_String :=
        To_Ada (G.Non_Terminal (Prod.Parent).Name);
      Part_Type    : League.Strings.Universal_String;
      Package_Name : League.Strings.Universal_String;
   begin
      for Part of G.Part (Prod.First .. Prod.Last) loop
         Part_Type := Return_Type (Part);
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
