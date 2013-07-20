--                        G E L A   G R A M M A R S                         --
--          Library for dealing with grammars for Gela project,             --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with Ada.Command_Line;
with Ada.Text_IO;

with Gela.Grammars;
with Gela.Grammars.Reader;
with Gela.Grammars_Convertors;
with Gela.Grammars.Rule_Templates;
with Gela.Grammars_Debug;
with Gela.Grammars.LR_Tables;
with Gela.Grammars.LR.LALR;
with Gela.Grammars.Constructors;
with Gela.Grammars.Conflicts;
with AG_Tools.Writers;                 use AG_Tools.Writers;

with League.String_Vectors;
with League.Strings;

procedure YACC_Driver is

   use type Gela.Grammars.Rule_Count;

   procedure Put_Rule
     (Prod : Gela.Grammars.Production;
      Rule : Gela.Grammars.Rule);

   function Image (X : Integer) return Wide_Wide_String;

   File  : constant String := Ada.Command_Line.Argument (1);
   G     : constant Gela.Grammars.Grammar := Gela.Grammars.Reader.Read (File);
   Plain : constant Gela.Grammars.Grammar :=
     Gela.Grammars_Convertors.Convert (G, Left => False);
   AG    : constant Gela.Grammars.Grammar :=
     Gela.Grammars.Constructors.To_Augmented (Plain);
   Table : constant Gela.Grammars.LR_Tables.Table_Access :=
     Gela.Grammars.LR.LALR.Build
       (Input        => AG,
        Right_Nulled => False);
   Resolver : Gela.Grammars.Conflicts.Resolver;
   Output : Writer;

   -----------
   -- Image --
   -----------

   function Image (X : Integer) return Wide_Wide_String is
      Img : constant Wide_Wide_String := Integer'Wide_Wide_Image (X);
   begin
      return Img (2 .. Img'Last);
   end Image;

   --------------
   -- Put_Rule --
   --------------

   procedure Put_Rule
     (Prod : Gela.Grammars.Production;
      Rule : Gela.Grammars.Rule)
   is
      use Gela.Grammars.Rule_Templates;
      use type League.Strings.Universal_String;
      Template : constant Rule_Template := Create (Rule.Text);
      Args     : League.String_Vectors.Universal_String_Vector;
      Value    : League.Strings.Universal_String;
   begin
      for J in 1 .. Template.Count loop
         Value.Clear;

         if Plain.Non_Terminal (Prod.Parent).Name = Template.Part_Name (J) then
            Value.Append ("Nodes (1)");
         else
            declare
               Index : Positive := 1;
            begin
               for Part of Plain.Part (Prod.First .. Prod.Last) loop
                  if Part.Name = Template.Part_Name (J) then
                     Value.Append ("Nodes (");
                     Value.Append (Image (Index));
                     Value.Append (")");
                  end if;

                  Index := Index + 1;
               end loop;

               if Value.Is_Empty then
                  if Template.Has_Default (J) then
                     Value := Template.Default (J);
                  else
                     Ada.Text_IO.Put_Line
                       ("Wrong part " &
                          Template.Part_Name (J).To_UTF_8_String &
                          " in rule for production " &
                          Plain.Non_Terminal (Prod.Parent).Name.To_UTF_8_String
                          & "." & Prod.Name.To_UTF_8_String);

                     Ada.Text_IO.Put_Line (Rule.Text.To_UTF_8_String);

                     raise Constraint_Error;
                  end if;
               end if;
            end;
         end if;

         Args.Append (Value);
      end loop;

      Output.P (Template.Substitute (Args));
   end Put_Rule;

begin
   Output.P ("with Gela.Nodes.Convertions; use Gela.Nodes.Convertions;");
   Output.P;
   Output.P ("separate (Gela.Mutables.LALR_Parsers)");
   Output.P ("procedure On_Reduce");
   Output.P ("     (Self  : access Parser;");
   Output.P ("      Prod  : Gela.Grammars.Production_Index;");
   Output.P ("      Nodes : in out Node_Array)");
   Output.P ("is");
   Output.P ("begin");
   Output.P ("   case Prod is");

   for Prod of Plain.Production loop
      Output.N ("      when");
      Output.N (Gela.Grammars.Production_Index'Wide_Wide_Image (Prod.Index));
      Output.P (" =>");

      for Rule of Plain.Rule (Prod.First_Rule .. Prod.Last_Rule) loop
         Put_Rule (Prod, Rule);
      end loop;

      if Prod.First_Rule > Prod.Last_Rule then
         Output.P ("         null;");
      end if;
   end loop;

   Output.P ("      when others =>");
   Output.P ("         null;");
   Output.P ("   end case;");
   Output.N ("end On_Reduce;");

   Ada.Text_IO.Put_Line (Output.Text.To_UTF_8_String);

   Resolver.Resolve (AG, Table.all);
   Gela.Grammars_Debug.Print_Conflicts (AG, Table.all);

   if Ada.Command_Line.Argument_Count > 1 then
      Gela.Grammars_Debug.Print (G);
   end if;
end YACC_Driver;
