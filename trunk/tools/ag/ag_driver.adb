
with Ada.Command_Line;
with Ada.Strings.Fixed;
with Ada.Text_IO;

with League.Strings;
with League.String_Vectors;

with Gela.Grammars;
with Gela.Grammars.Reader;
with Gela.Grammars_Convertors;

procedure AG_Driver is

   use type Gela.Grammars.Production_Index;

   procedure N (Text : String);
   procedure P (Text : String := ""; Indent : Integer := -1);
   function To_Ada (Text : League.Strings.Universal_String) return String;
   function Image (X : Integer) return String;

   Current_Indent : Natural := 0;
   New_Line       : Boolean := True;

   -----------
   -- Image --
   -----------

   function Image (X : Integer) return String is
      Img : constant String := Integer'Image (X);
   begin
      return Img (2 .. Img'Last);
   end Image;

   -------
   -- N --
   -------

   procedure N (Text : String) is
      use Ada.Strings.Fixed;
   begin
      if New_Line and Current_Indent /= 0 then
         Ada.Text_IO.Put (Current_Indent * ' ');
      end if;

      Ada.Text_IO.Put (Text);
      New_Line := False;
   end N;

   -------
   -- P --
   -------

   procedure P (Text : String := ""; Indent : Integer := -1) is
      use Ada.Strings.Fixed;
   begin
      if Text /= "" and New_Line and Current_Indent /= 0 then
         Ada.Text_IO.Put (Current_Indent * ' ');
      end if;

      Ada.Text_IO.Put_Line (Text);
      New_Line := True;

      if Indent /= -1 then
         Current_Indent := Indent;
      end if;
   end P;

   ------------
   -- To_Ada --
   ------------

   function To_Ada (Text : League.Strings.Universal_String) return String is
      List : League.String_Vectors.Universal_String_Vector;
      Piece : League.Strings.Universal_String;
   begin
      List := Text.Split ('_');
      for J in 1 .. List.Length loop
         Piece := List.Element (J);
         Piece.Replace (1, 1, Piece.Slice (1, 1).To_Uppercase);
         List.Replace (J, Piece);
      end loop;

      return List.Join ('_').To_UTF_8_String;
   end To_Ada;

   Name  : constant String := Ada.Command_Line.Argument (1);
   G     : constant Gela.Grammars.Grammar := Gela.Grammars.Reader.Read (Name);
   Plain : constant Gela.Grammars.Grammar :=
     Gela.Grammars_Convertors.Convert_With_Empty (G);

--     Token : constant Gela.Grammars.Production_Index :=
--       Plain.Last_Production + 1;

   Reserved : constant := 2;  --  Tag, Count

   use type Gela.Grammars.Part_Count;
begin
   P ("with Gela.Relocatable_Arrays;");
   P ("with Gela.Grammars;");
   P;
   P ("package Gela.Properties is", 3);
   P;
   P ("subtype X is Gela.Relocatable_Arrays.Index;");
   P;
   N ("Reserved : constant := ");
   N (Image (Reserved));
   P (";");
   P;

   for Prod of Plain.Production loop
      N ("package ");
      N (To_Ada (Prod.Name));
      P (" is", 6);

      for Part of Plain.Part (Prod.First .. Prod.Last) loop
         N (To_Ada (Part.Name));
         N (" : constant X := ");
         N (Image (Integer (Part.Index - Prod.First + Reserved)));
         P (";");
      end loop;

      N ("Size : constant X := ");
      N (Image (Integer (Prod.Last - Prod.First + 1 + Reserved)));
      P (";");
      P ("", 3);
      N ("end ");
      N (To_Ada (Prod.Name));
      P (";");
   end loop;

   P ("");
   N ("Size : constant array (Gela.Grammars.Production_Index range 1 ..");
   N (Gela.Grammars.Production_Index'Image (Plain.Last_Production));
   N (") of X :=");
   P ("", 5);
   N ("(");

   for Prod of Plain.Production loop
      N (Image (Integer (Prod.Index)));
      N (" => ");
      N (Image (Integer (Prod.Last - Prod.First + 1 + Reserved)));

      if Prod.Index = Plain.Last_Production then
         P (");", 3);
      else
         P (",", 6);
      end if;
   end loop;

   P ("", 0);

   P ("end Gela.Properties;");

end AG_Driver;
