------------------------------------------------------------------------------
--                        G E L A   G R A M M A R S                         --
--          Library for dealing with tests for for Gela project,            --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with Gela.Test_Cases;
with League.Calendars.ISO_8601;
with XML.SAX.Attributes;
with XML.SAX.Pretty_Writers;
with Ada.Wide_Wide_Text_IO;

package body Gela.Bitten_Report is

   function "+"
     (Item : Wide_Wide_String)
     return League.Strings.Universal_String
     renames League.Strings.To_Universal_String;

   function Time_Image
     (Value : League.Calendars.Time)
     return League.Strings.Universal_String;

   Duration  : constant League.Strings.Universal_String := +"duration";
   Name      : constant League.Strings.Universal_String := +"name";
   Fixture   : constant League.Strings.Universal_String := +"fixture";
   File      : constant League.Strings.Universal_String := +"file";
   Report    : constant League.Strings.Universal_String := +"report";
   Status    : constant League.Strings.Universal_String := +"status";
   Stdout    : constant League.Strings.Universal_String := +"stdout";
   Test      : constant League.Strings.Universal_String := +"test";
   Traceback : constant League.Strings.Universal_String := +"traceback";

   Image : constant array (Test_Cases.Status_Kind) of
     League.Strings.Universal_String :=
       (Test_Cases.Success => +"success",
        Test_Cases.Failure => +"failure",
        Test_Cases.Error   => +"error",
        Test_Cases.Ignore  => +"ignore");

   --------------
   -- Generate --
   --------------

   procedure Generate
     (Iterator : in out Test_Iterators.Iterator'Class;
      Result   : out League.Strings.Universal_String)
   is
      Test   : Gela.Test_Cases.Test_Case_Access;
      Writer : XML.SAX.Pretty_Writers.SAX_Pretty_Writer;
   begin
      Iterator.Start;

      Writer.Start_Document;

      declare
         Attrs : XML.SAX.Attributes.SAX_Attributes;
      begin
         Attrs.Set_Value (+"category", +"test");

         Writer.Start_Element
           (Qualified_Name => Report,
            Attributes     => Attrs);
      end;

      while Iterator.Has_More_Tests loop
         declare
            Attrs : XML.SAX.Attributes.SAX_Attributes;
         begin
            Iterator.Next (Test);

            Attrs.Set_Value (Duration, Time_Image (Test.Duration));
            Attrs.Set_Value (Status, Image (Test.Status));
            Attrs.Set_Value (Name, Test.Name);
            Attrs.Set_Value (Fixture, Test.Name);
            --  Attrs.Set_Value (Fixture, Test.Fixture);
            Attrs.Set_Value (File, Test.File);

            Writer.Start_Element
              (Qualified_Name => Bitten_Report.Test,
               Attributes     => Attrs);

            if not Test.Output.Is_Empty then
               Writer.Start_Element (Qualified_Name => Stdout);
               Writer.Characters (Test.Output);
               Writer.End_Element (Qualified_Name => Stdout);
            end if;

            if not Test.Traceback.Is_Empty then
               Writer.Start_Element (Qualified_Name => Traceback);
               Writer.Characters (Test.Traceback);
               Writer.End_Element (Qualified_Name => Traceback);
            end if;

            Writer.End_Element (Qualified_Name => Bitten_Report.Test);
         end;
      end loop;

      Writer.End_Element (Qualified_Name => Report);

      Result := Writer.Text;
   end Generate;

   ----------------
   -- Time_Image --
   ----------------

   function Time_Image
     (Value : League.Calendars.Time)
     return League.Strings.Universal_String
   is
      use League.Calendars.ISO_8601;
      package IO is new Ada.Wide_Wide_Text_IO.Integer_IO
        (Nanosecond_100_Number);

      S : constant Second_Number := Second (Value);
      N : constant Nanosecond_100_Number := Nanosecond_100 (Value);
      S_Image : constant Wide_Wide_String := Second_Number'Wide_Wide_Image (S);
      N_Image : Wide_Wide_String (1 .. 8);
   begin
      IO.Put (N_Image, N);

      for J in N_Image'Range loop
         if N_Image (J) = ' ' then
            N_Image (J) := '0';
         end if;
      end loop;

      return +(S_Image (2 .. S_Image'Last) & "."
                 & N_Image (2 .. N_Image'Last));
   end Time_Image;

end Gela.Bitten_Report;
