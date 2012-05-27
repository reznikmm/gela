with Ada.Directories;
with Ada.Wide_Wide_Text_IO;
with Gela.Conv;
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

   procedure Write (File : Universal_String; Text : Universal_String);

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
      Text   : constant Universal_String := Gela.Conv.Read_File (Source);
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

      Write (Directory.Build_Ada & "/asis-gela-" & Name, Lines.Join (LF));
      Ada.Directories.Delete_File (Gela.Conv.To_String (Source));
   end Add_Gela_Prefix;

   procedure Make_ASIS_ADB is
      Head : constant Universal_String :=
        Gela.Conv.Read_File (Directory.Source_ASIS & "/asis.body");
      Text : constant Universal_String := Gela.XSLT.Transform
        (XML => Directory.Source_Model & "/asis_hier.xml",
         XSL => Directory.Source_XSLT & "/asis-adb.xsl");
   begin
      Write (Directory.Build_Ada & "/asis.adb", Head & Text);
   end Make_ASIS_ADB;

   procedure Make_ASIS_ADS is
      Head : constant Universal_String :=
        Gela.Conv.Read_File (Directory.Source_ASIS & "/asis.spec");
      Text : constant Universal_String := Gela.XSLT.Transform
        (XML => Directory.Source_Model & "/asis_hier.xml",
         XSL => Directory.Source_XSLT & "/asis-ads.xsl");
   begin
      Write (Directory.Build_Ada & "/asis.ads", Head & Text);
   end Make_ASIS_ADS;

   procedure Make_ASIS_Elements is
      Text : constant Universal_String := Gela.XSLT.Transform
        (XML => Directory.Source_Model & "/asis_hier.xml",
         XSL => Directory.Source_XSLT & "/asis-gela-elements.xsl");
      List : constant League.String_Vectors.Universal_String_Vector :=
        Text.Split ('#', Skip_Empty);
   begin
      for J in 1 .. List.Length / 2 loop
         Write
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
      Write (Directory.Build_Ada & "/gela-source_buffers-current.ads", Text);
   end Make_Buffer;

   procedure Make_Directory (Dir  : Universal_String) is
      Name : constant String := Gela.Conv.To_String (Dir);
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
      Write (Directory.Build & "/fixed.xml", Code);
   end Make_Fixed_Syntax;

   procedure Make_Parser is
      use Gela.Conv;

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

      Write (Parser_Y, Text);

      Xml_To_Y.Run
        (ASIS_File   => To_String (Directory.Source_Model & "/asis_hier.xml"),
         Tokens_File => To_String (Directory.Source_Model & "/tokens.xml"),
         Syntax_File => To_String (Directory.Build & "/fixed.xml"),
         Code_File   => To_String (Directory.Source_Model & "/code.xml"),
         Output      => To_String (Parser_Y));

      Text := Read_File (Parser_Y);
      Prefix.Append (Text);
      Text := Read_File (Directory.Source_ASIS & "/asis-gela-parser.adt");
      Prefix.Append (Text);
      Write (Parser_Y, Prefix);

      command_line_interface.Push_Arguments (To_String (Parser_Y));
      Ayacc;
      Add_Gela_Prefix ("parser.ads");
      Add_Gela_Prefix ("parser.adb");
      Add_Gela_Prefix ("parser-tokens.ads");
      Add_Gela_Prefix ("parser-shift_reduce.ads");
      Add_Gela_Prefix ("parser-goto_table.ads");
   end Make_Parser;

   procedure Write (File : Universal_String; Text : Universal_String) is
      Output : Ada.Wide_Wide_Text_IO.File_Type;
      Name : constant String := Gela.Conv.To_String (File);
   begin
      Ada.Wide_Wide_Text_IO.Put_Line ("Writing " & File.To_Wide_Wide_String);
      Ada.Wide_Wide_Text_IO.Create (Output, Name => Name);
      Ada.Wide_Wide_Text_IO.Put_Line (Output, Text.To_Wide_Wide_String);
      Ada.Wide_Wide_Text_IO.Close (Output);
   end Write;

begin
   Make_Dirs;
   Make_ASIS_Sources;
   UAFlex.Run
     (Input_File => Conv.To_String (Directory.Source_ASIS & "/ada.uaflex"),
      Pkg_Name   => "Asis.Gela.Scanner_Tables",
      Output_Dir => Conv.To_String (Directory.Build_Ada));
   Make_Fixed_Syntax;
   Make_Parser;
   Make_Buffer;
end Gela.Build;
