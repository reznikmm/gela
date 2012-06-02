------------------------------------------------------------------------------
--                        G E L A   G R A M M A R S                         --
--          Library for dealing with tests for for Gela project,            --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with Ada.Directories;
with Ada.Wide_Wide_Text_IO;
with Gela.Conv;
with Gela.Host;
with XML.SAX.Attributes;
with XML.SAX.Pretty_Writers;
with League.String_Vectors;
with League.Characters;

package body Gela.Bitten_Coverage is

   use type League.Strings.Universal_String;

   function "+"
     (Item : Wide_Wide_String)
     return League.Strings.Universal_String
     renames League.Strings.To_Universal_String;

   LF : constant Wide_Wide_Character := Wide_Wide_Character'Val (10);

   package C is
      Name       : constant League.Strings.Universal_String := +"name";
      File       : constant League.Strings.Universal_String := +"file";
      Percentage : constant League.Strings.Universal_String := +"percentage";
      Lines      : constant League.Strings.Universal_String := +"lines";
      Line_Hits  : constant League.Strings.Universal_String := +"line_hits";
      Report     : constant League.Strings.Universal_String := +"report";
      Source     : constant League.Strings.Universal_String := +"Source";
      Coverage   : constant League.Strings.Universal_String := +"coverage";
   end C;

   function Image (Value : Natural) return League.Strings.Universal_String;

   procedure Mkdir (Gcov_Root : League.Strings.Universal_String);

   procedure Make_GCNO_List
     (Build   : League.Strings.Universal_String;
      List    : League.Strings.Universal_String);

   procedure Run_Gcov (Root, List : League.Strings.Universal_String);

   procedure Find_Gcov_Files
     (Root : League.Strings.Universal_String;
      List : out League.String_Vectors.Universal_String_Vector);

   -----------
   -- Mkdir --
   -----------

   procedure Mkdir (Gcov_Root : League.Strings.Universal_String) is
      Path : constant String := Gela.Conv.To_String (Gcov_Root);
   begin
      if not Ada.Directories.Exists (Path) then
         Ada.Directories.Create_Directory (Path);
      end if;
   end Mkdir;

   --------------------
   -- Make_GCNO_List --
   --------------------

   procedure Make_GCNO_List
     (Build   : League.Strings.Universal_String;
      List    : League.Strings.Universal_String)
   is
      Output    : League.Strings.Universal_String;
      Code      : Integer;
      Arguments : League.String_Vectors.Universal_String_Vector;
   begin
      Arguments.Append (Build);
      Arguments.Append (+"-name");
      Arguments.Append (+"*.gcno");

      Gela.Host.Execute
        (Command     =>  +"find",
         Arguments   => Arguments,
         Exit_Code   => Code,
         Output      => Output,
         Output_File => List);

      if Code /= 0 then
         raise Constraint_Error;
      end if;
   end Make_GCNO_List;

   procedure Run_Gcov (Root, List : League.Strings.Universal_String) is
      Output    : League.Strings.Universal_String;
      Code      : Integer;
      Arguments : League.String_Vectors.Universal_String_Vector;
   begin
      Arguments.Append ("@" & List);

      Gela.Host.Execute
        (Command     =>  +"gcov",
         Arguments   => Arguments,
         Exit_Code   => Code,
         Output      => Output,
         Directory   => Root);

      if Code /= 0 then
         raise Constraint_Error;
      end if;
   end Run_Gcov;

   procedure Find_Gcov_Files
     (Root : League.Strings.Universal_String;
      List : out League.String_Vectors.Universal_String_Vector)
   is
      Output    : League.Strings.Universal_String;
      Code      : Integer;
      Arguments : League.String_Vectors.Universal_String_Vector;
   begin
      Arguments.Append (Root);
      Arguments.Append (+"-name");
      Arguments.Append (+"*.gcov");

      Gela.Host.Execute
        (Command     =>  +"find",
         Arguments   => Arguments,
         Exit_Code   => Code,
         Output      => Output);

      if Code /= 0 then
         raise Constraint_Error;
      end if;

      List := Output.Split (LF, League.Strings.Skip_Empty);
   end Find_Gcov_Files;

   procedure Parse_Gcov_File
     (Input   : League.Strings.Universal_String;
      Name    : out League.Strings.Universal_String;
      File    : out League.Strings.Universal_String;
      Percent : out Natural;
      Total   : out Natural;
      Hits    : out League.Strings.Universal_String);

   procedure Parse_Gcov_File
     (Input   : League.Strings.Universal_String;
      Name    : out League.Strings.Universal_String;
      File    : out League.Strings.Universal_String;
      Percent : out Natural;
      Total   : out Natural;
      Hits    : out League.Strings.Universal_String)
   is
      Line   : constant League.String_Vectors.Universal_String_Vector :=
        Gela.Conv.Read_File (Input).Split (LF, League.Strings.Skip_Empty);

      Covered : Natural := 0;
      Length  : Natural := 0;
   begin
      Hits.Clear;
      Total := 0;

      for J in 1 .. Line.Length loop
         declare
            use type League.Characters.Universal_Character;

            Part : constant League.String_Vectors.Universal_String_Vector :=
              Line.Element (J).Split (':');

            Cov : constant League.Strings.Universal_String := Part.Element (1);

            Line_Number : constant Natural :=
              Natural'Wide_Wide_Value (Part.Element (2).To_Wide_Wide_String);

         begin
            if Line_Number = 0 then
               if Part.Element (3) = C.Source then
                  File := Part.Element (4);
               end if;
            else
               if Cov.Element (Cov.Length) = '-' then
                  Hits.Append (" -");
                  Length := Length + 2;
               elsif Cov.Element (Cov.Length) = '#' then
                  Hits.Append (" 0");
                  Length := Length + 2;
                  Total := Total + 1;
               else
                  Hits.Append (Cov);
                  Length := Length + Cov.Length;
                  Total := Total + 1;
                  Covered := Covered + 1;
               end if;

               if Length > 70 then
                  Length := 0;
                  Hits.Append (LF);
               end if;

            end if;
         end;
      end loop;

      Name := File;

      if Covered = 0 then
         Percent := 0;
      else
         Percent := Covered * 100 / Total;
      end if;
   end Parse_Gcov_File;

   --------------
   -- Generate --
   --------------

   procedure Generate
     (Build    : League.Strings.Universal_String;
      Result   : out League.Strings.Universal_String)
   is
      Source    : constant League.Strings.Universal_String :=
        Gela.Host.Source_Root;
      Gcov_Root : constant League.Strings.Universal_String := Build & "/gcov";
      GCNO_List : constant League.Strings.Universal_String := Gcov_Root & "/x";
      List   : League.String_Vectors.Universal_String_Vector;
      Writer : XML.SAX.Pretty_Writers.SAX_Pretty_Writer;
   begin
      Mkdir (Gcov_Root);
      Make_GCNO_List (Build, GCNO_List);
      Run_Gcov (Gcov_Root, GCNO_List);

      Find_Gcov_Files (Gcov_Root, List);

      Writer.Start_Document;

      declare
         Attrs : XML.SAX.Attributes.SAX_Attributes;
      begin
         Attrs.Set_Value (+"category", C.Coverage);

         Writer.Start_Element
           (Qualified_Name => C.Report,
            Attributes     => Attrs);
      end;

      for J in 1 .. List.Length loop
         declare
            Attrs   : XML.SAX.Attributes.SAX_Attributes;
            Name    : League.Strings.Universal_String;
            File    : League.Strings.Universal_String;
            Total   : Natural;
            Percent : Natural;
            Hits    : League.Strings.Universal_String;
         begin
            Ada.Wide_Wide_Text_IO.Put_Line
              ("Reading " & List.Element (J).To_Wide_Wide_String);

            Parse_Gcov_File
              (Input   => List.Element (J),
               Name    => Name,
               File    => File,
               Percent => Percent,
               Total   => Total,
               Hits    => Hits);

            if File.Starts_With (Source) then
               File.Slice (Source.Length + 1, File.Length);
            elsif File.Starts_With (Build) then
               File.Slice (Build.Length + 1, File.Length);
            end if;

            if File.Starts_With ("/") then
               File.Slice (2, File.Length);
            end if;

            declare
               Parts : constant League.String_Vectors.Universal_String_Vector
                 := Name.Split ('/');
            begin
               Name := Parts.Element (Parts.Length);
            end;

            Attrs.Set_Value (C.Name, Name);
            Attrs.Set_Value (C.File, File);
            Attrs.Set_Value (C.Percentage, Image (Percent));
            Attrs.Set_Value (C.Lines, Image (Total));

            Writer.Start_Element
              (Qualified_Name => C.Coverage,
               Attributes     => Attrs);

            Writer.Start_Element (Qualified_Name => C.Line_Hits);
            Writer.Characters (Hits);
            Writer.End_Element (Qualified_Name => C.Line_Hits);
            Writer.End_Element (Qualified_Name => C.Coverage);
         end;
      end loop;

      Writer.End_Element (Qualified_Name => C.Report);

      Result := Writer.Text;
   end Generate;

   -----------
   -- Image --
   -----------

   function Image (Value : Natural) return League.Strings.Universal_String is
      Text : constant Wide_Wide_String := Natural'Wide_Wide_Image (Value);
   begin
      return +Text (2 .. Text'Last);
   end Image;

end Gela.Bitten_Coverage;
