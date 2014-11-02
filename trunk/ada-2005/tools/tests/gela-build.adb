with Ada.Directories;
with Ada.Wide_Wide_Text_IO;
with Gela.Test_Tools;
with Gela.Host;
with Gela.XSLT;
with League.Strings;
with League.String_Vectors;
with UAFlex;
with Xml_To_Y;

with Ayacc;
with command_line_interface;

procedure Gela.Build is
   use League.Strings;

   procedure Make_Dirs;
   --  Make all needed directories

   procedure Make_ASIS_Sources;
   --  Generate ASIS sources: asis.ad[sb], asis elements
   procedure Make_ASIS_ADS;
   procedure Make_ASIS_ADB;
   procedure Make_ASIS_Elements;
   procedure Make_Parser;
   procedure Make_Fixed_Syntax;
   procedure Add_Gela_Prefix (Name : Wide_Wide_String);
   procedure Make_Directory (Dir  : Universal_String);
   procedure Make_Buffer;

   LF : constant Wide_Wide_Character := Wide_Wide_Character'Val (10);

   package Directory is
      Build        : constant Universal_String := Gela.Host.Build_Root;
      Build_Ada    : constant Universal_String := Build & "/ada";
      Source       : constant Universal_String :=
        Gela.Host.Source_Root & "/source";
      Source_ASIS  : constant Universal_String := Source & "/asis";
      Source_Model : constant Universal_String := Source_ASIS & "/model";
      Source_XSLT  : constant Universal_String := Source_ASIS & "/xslt";
   end Directory;

   procedure Add_Gela_Prefix (Name : Wide_Wide_String) is
      Source : constant Universal_String := Directory.Build_Ada & "/" & Name;
      Text   : constant Universal_String := Gela.Test_Tools.Read_File (Source);
      Line   : Universal_String;
      Lines  : League.String_Vectors.Universal_String_Vector :=
        Text.Split (LF);
   begin
      for J in 1 .. Lines.Length loop
         Line := Lines.Element (J);

         if Line.Starts_With ("package Parser") then
            Line.Replace (9, 8, "Asis.Gela.");
         elsif Line.Starts_With ("package body Parser") then
            Line.Replace (14, 13, "Asis.Gela.");
         elsif Line.Starts_With ("end Parser") then
            Line.Replace (5, 4, "Asis.Gela.");
         elsif Line.Starts_With ("with Parser") then
            Line.Replace (6, 5, "Asis.Gela.");
         elsif Line.Starts_With ("use  Parser") then
            Line.Replace (6, 5, "Asis.Gela.");
         end if;

         Lines.Replace (J, Line);
      end loop;

      Gela.Test_Tools.Write_File
        (Directory.Build_Ada & "/asis-gela-" & Name, Lines.Join (LF));

      Ada.Directories.Delete_File (Source.To_UTF_8_String);
   end Add_Gela_Prefix;

   procedure Make_ASIS_ADB is
      Head : constant Universal_String :=
        Gela.Test_Tools.Read_File (Directory.Source_ASIS & "/asis.body");
      Text : constant Universal_String := Gela.XSLT.Transform
        (XML => Directory.Source_Model & "/asis_hier.xml",
         XSL => Directory.Source_XSLT & "/asis-adb.xsl");
   begin
      Gela.Test_Tools.Write_File
        (Directory.Build_Ada & "/asis.adb", Head & Text);
   end Make_ASIS_ADB;

   procedure Make_ASIS_ADS is
      Head : constant Universal_String :=
        Gela.Test_Tools.Read_File (Directory.Source_ASIS & "/asis.spec");
      Text : constant Universal_String := Gela.XSLT.Transform
        (XML => Directory.Source_Model & "/asis_hier.xml",
         XSL => Directory.Source_XSLT & "/asis-ads.xsl");
   begin
      Gela.Test_Tools.Write_File
        (Directory.Build_Ada & "/asis.ads", Head & Text);
   end Make_ASIS_ADS;

   procedure Make_ASIS_Elements is
      Text : constant Universal_String := Gela.XSLT.Transform
        (XML => Directory.Source_Model & "/asis_hier.xml",
         XSL => Directory.Source_XSLT & "/asis-gela-elements.xsl");
      List : constant League.String_Vectors.Universal_String_Vector :=
        Text.Split ('#', Skip_Empty);
   begin
      for J in 1 .. List.Length / 2 loop
         Gela.Test_Tools.Write_File
           (Directory.Build_Ada & "/" & List.Element (J * 2 - 1),
            List.Element (J * 2));
      end loop;
   end Make_ASIS_Elements;

   procedure Make_ASIS_Sources is
   begin
      Make_ASIS_ADS;
      Make_ASIS_ADB;
      Make_ASIS_Elements;
   end Make_ASIS_Sources;

   procedure Make_Buffer is
      Text : Universal_String;
   begin
      Text.Append ("with Gela.Source_Buffers.Portable;" & LF);
      Text.Append ("package Gela.Source_Buffers.Current renames " &
                     "Gela.Source_Buffers.Portable;" & LF);
      Gela.Test_Tools.Write_File
        (Directory.Build_Ada & "/gela-source_buffers-current.ads", Text);
   end Make_Buffer;

   procedure Make_Directory (Dir  : Universal_String) is
      Name : constant String := Dir.To_UTF_8_String;
   begin
      if Ada.Directories.Exists (Name) then
         return;
      end if;

      Ada.Wide_Wide_Text_IO.Put_Line ("mkdir " & Dir.To_Wide_Wide_String);
      Ada.Directories.Create_Directory (Name);
   end Make_Directory;

   procedure Make_Dirs is
   begin
      Make_Directory (Directory.Build_Ada);
   end Make_Dirs;

   procedure Make_Fixed_Syntax is
      Code : constant Universal_String := Gela.XSLT.Transform
        (XML => Directory.Source_Model & "/syntax.xml",
         XSL => Directory.Source_XSLT & "/fixed.xsl");
   begin
      Gela.Test_Tools.Write_File (Directory.Build & "/fixed.xml", Code);
   end Make_Fixed_Syntax;

   procedure Make_Parser is
      Parser_Y : constant Universal_String :=
        Directory.Build_Ada & "/parser.y";

      Prefix : Universal_String;
      Text   : Universal_String;
   begin
      Prefix.Append ("%token New_Line_Token");
      Prefix.Append (LF);
      Prefix.Append ("%token Separator_Token");
      Prefix.Append (LF);
      Prefix.Append ("%token Comment_Token");
      Prefix.Append (LF);

      Gela.Test_Tools.Write_File (Parser_Y, Text);

      Xml_To_Y.Run
        (ASIS_File   =>
           Directory.Source_Model.To_UTF_8_String & "/asis_hier.xml",
         Tokens_File => Directory.Source_Model.To_UTF_8_String & "/tokens.xml",
         Syntax_File => Directory.Build.To_UTF_8_String & "/fixed.xml",
         Code_File   => Directory.Source_Model.To_UTF_8_String & "/code.xml",
         Output      => Parser_Y.To_UTF_8_String);

      Text := Gela.Test_Tools.Read_File (Parser_Y);
      Prefix.Append (Text);
      Text := Gela.Test_Tools.Read_File
        (Directory.Source_ASIS & "/asis-gela-parser.adt");
      Prefix.Append (Text);
      Gela.Test_Tools.Write_File (Parser_Y, Prefix);

      command_line_interface.Push_Arguments (Parser_Y.To_UTF_8_String);
      Ayacc;
      Add_Gela_Prefix ("parser.ads");
      Add_Gela_Prefix ("parser.adb");
      Add_Gela_Prefix ("parser-tokens.ads");
      Add_Gela_Prefix ("parser-shift_reduce.ads");
      Add_Gela_Prefix ("parser-goto_table.ads");
   end Make_Parser;

begin
   Make_Dirs;
   Make_ASIS_Sources;
   UAFlex.Run
     (Input_File => Directory.Source_ASIS.To_UTF_8_String & "/ada.uaflex",
      Pkg_Name   => "Asis.Gela.Scanner_Tables",
      Output_Dir => Directory.Build_Ada.To_UTF_8_String);
   Make_Fixed_Syntax;
   Make_Parser;
   Make_Buffer;
end Gela.Build;
