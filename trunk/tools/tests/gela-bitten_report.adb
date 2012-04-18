------------------------------------------------------------------------------
--                        G E L A   G R A M M A R S                         --
--          Library for dealing with tests for for Gela project,            --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with Gela.Test_Cases;
with XML.SAX.Attributes;
with XML.SAX.Pretty_Writers;

package body Gela.Bitten_Report is

   function "+"
     (Item : Wide_Wide_String)
     return League.Strings.Universal_String
     renames League.Strings.To_Universal_String;

   Duration  : constant League.Strings.Universal_String := +"duration";
   Name      : constant League.Strings.Universal_String := +"name";
--   Fixture   : constant League.Strings.Universal_String := +"fixture";
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
     (Iterator : in out Test_Iterators.Iterator;
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

            Attrs.Set_Value (Duration, +"0");
            Attrs.Set_Value (Status, Image (Test.Status));
            Attrs.Set_Value (Name, Test.Name);
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

end Gela.Bitten_Report;
