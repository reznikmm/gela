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
   use type Gela.Grammars.LR.State_Count;
   use type Gela.Grammars.Non_Terminal_Count;

   procedure Put_Rule
     (Prod : Gela.Grammars.Production;
      Rule : Gela.Grammars.Rule);

   function Image (X : Integer) return Wide_Wide_String;

   procedure Print_Go_To;
   procedure Print_Action;

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

   ------------------
   -- Print_Action --
   ------------------

   procedure Print_Action is
      use type Gela.Grammars.Production_Count;
      use type Gela.Grammars.Part_Count;
      type Action_Code is mod 2 ** 16;

      Count  : Natural;
      Code : Action_Code;
   begin
      Output.P ("   type Action_Code is mod 2 ** 16;");
      Output.P ("   for Action_Code'Size use 16;");
      Output.P;
      Output.P ("   Action_Table : constant array");
      Output.N ("     (State_Index range 1 .. ");
      Output.N (Natural (Gela.Grammars.LR_Tables.Last_State (Table.all)));
      Output.P (",");
      Output.N ("      Gela.Grammars.Terminal_Count range 0 .. ");
      Output.N (Natural (Plain.Last_Terminal));
      Output.P (") of Action_Code :=");

      for State in 1 .. Gela.Grammars.LR_Tables.Last_State (Table.all) loop
         if State = 1 then
            Output.N ("     (");
         else
            Output.P (",");
            Output.N ("      ");
         end if;
         Output.N (Natural (State));
         Output.P (" =>");
         Output.N ("        (");
         Count := 0;

         for T in 0 .. Plain.Last_Terminal loop
            declare
               use Gela.Grammars.LR_Tables;
               S : constant Gela.Grammars.LR.State_Count :=
                 Shift (Table.all, State, T);
               R : constant Reduce_Iterator := Reduce (Table.all, State, T);
            begin
               if S /= 0 then
                  Code := Action_Code (S) + 16#80_00#;
               elsif not Is_Empty (R) then
                  Code := Action_Code (Production (R));
               else
                  Code := 0;
               end if;

               if Code /= 0 then
                  Output.N (Natural (T));
                  Output.N (" => ");
                  Output.N (Natural (Code));
                  Count := Count + 1;
                  if Count < 4 then
                     Output.N (", ");
                  else
                     Count := 0;
                     Output.P (",");
                     Output.N ("         ");
                  end if;
               end if;
            end;
         end loop;
         Output.N ("others => 0)");
      end loop;

      Output.P (");");
      Output.P;
      Output.P ("   type Production_Record is record");
      Output.P ("      NT    : Gela.Grammars.Non_Terminal_Index;");
      Output.P ("      Parts : Natural;");
      Output.P ("   end record;");
      Output.P;
      Output.N ("   Prods : constant array (Action_Code range 1 .. ");
      Output.N (Natural (Plain.Last_Production));
      Output.P (") of");
      Output.P ("     Production_Record :=");

      Count := 0;
      for J in 1 .. Plain.Last_Production loop
         if J = 1 then
            Output.N ("       (");
         elsif Count > 5 then
            Count := 0;
            Output.P (",");
            Output.N ("        ");
         else
            Output.N (", ");
         end if;

         Output.N ("(");
         Output.N (Natural (Plain.Production (J).Parent));
         Output.N (", ");
         Output.N (Natural (Plain.Production (J).Last
                             - Plain.Production (J).First + 1));
         Output.N (")");
         Count := Count + 1;
      end loop;

      Output.P (");");
      Output.P;

      Output.P ("   procedure Next_Action");
      Output.P ("     (State : Gela.Grammars.LR_Parsers.State_Index;");
      Output.P ("      Token : Gela.Grammars.Terminal_Count;");
      Output.P ("      Value : out Gela.Grammars.LR_Parsers.Action)");
      Output.P ("   is");
      Output.P ("      Code : constant Action_Code := " &
                  "Action_Table (State, Token);");
      Output.P ("   begin");
      Output.P ("      if (Code and 16#80_00#) /= 0 then");
      Output.P ("         Value := (Kind => Shift, " &
                  "State => State_Index (Code and 16#7F_FF#));");
      Output.P ("      elsif Code /= 0 then");
      Output.P ("         Value := (Kind  => Reduce,");
      Output.P ("                   Prod  => " &
                  "Gela.Grammars.Production_Index (Code),");
      Output.P ("                   NT    => Prods (Code).NT,");
      Output.P ("                   Parts => Prods (Code).Parts);");

      for State in 1 .. Gela.Grammars.LR_Tables.Last_State (Table.all) loop
         if Gela.Grammars.LR_Tables.Finish (Table.all, State) then
            Output.N ("      elsif State = ");
            Output.N (Natural (State));
            Output.P (" then");
            Output.P ("         Value := (Kind => Finish);");
         end if;
      end loop;

      Output.P ("      else");
      Output.P ("         Value := (Kind => Error);");
      Output.P ("      end if;");
      Output.P ("   end Next_Action;");
      Output.P;
   end Print_Action;

   -----------------
   -- Print_Go_To --
   -----------------

   procedure Print_Go_To is
      Count  : Natural;
   begin
      Output.P ("   Go_To_Table : constant array");
      Output.N ("     (Gela.Grammars.LR_Parsers.State_Index range 1 .. ");
      Output.N (Natural (Gela.Grammars.LR_Tables.Last_State (Table.all)));
      Output.P (",");
      Output.N ("      Gela.Grammars.Non_Terminal_Index range 1 .. ");
      Output.N (Natural (Plain.Last_Non_Terminal));
      Output.P (") of State_Index :=");

      for State in 1 .. Gela.Grammars.LR_Tables.Last_State (Table.all) loop
         if State = 1 then
            Output.N ("     (");
         else
            Output.P (",");
            Output.N ("      ");
         end if;
         Output.N (Natural (State));
         Output.P (" =>");
         Output.N ("        (");
         Count := 0;

         for NT in 1 .. Plain.Last_Non_Terminal loop
            declare
               use Gela.Grammars.LR;
               Next : constant State_Count :=
                 Gela.Grammars.LR_Tables.Shift (Table.all, State, NT);
            begin
               if Next /= 0 then
                  Output.N (Natural (NT));
                  Output.N (" => ");
                  Output.N (Natural (Next));
                  Count := Count + 1;
                  if Count < 4 then
                     Output.N (", ");
                  else
                     Count := 0;
                     Output.P (",");
                     Output.N ("         ");
                  end if;
               end if;
            end;
         end loop;
         Output.N ("others => 0)");
      end loop;
      Output.P (");");
      Output.P;

      Output.P ("   function Go_To");
      Output.P ("     (State : Gela.Grammars.LR_Parsers.State_Index;");
      Output.P ("      NT    : Gela.Grammars.Non_Terminal_Index)");
      Output.P ("      return Gela.Grammars.LR_Parsers.State_Index");
      Output.P ("   is");
      Output.P ("   begin");
      Output.P ("      return Go_To_Table (State, NT);");
      Output.P ("   end Go_To;");
      Output.P;
   end Print_Go_To;

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
   Output.P ("with Gela.Grammars;");
   Output.P ("with Gela.Grammars.LR_Parsers;");
   Output.P;
   Output.P ("package Gela.Mutables.Parser_Data is");
   Output.P ("   pragma Preelaborate (Gela.Mutables.Parser_Data);");
   Output.P;
   Output.P ("   procedure Next_Action");
   Output.P ("     (State : Gela.Grammars.LR_Parsers.State_Index;");
   Output.P ("      Token : Gela.Grammars.Terminal_Count;");
   Output.P ("      Value : out Gela.Grammars.LR_Parsers.Action);");
   Output.P;
   Output.P ("   function Go_To");
   Output.P ("     (State : Gela.Grammars.LR_Parsers.State_Index;");
   Output.P ("      NT    : Gela.Grammars.Non_Terminal_Index)");
   Output.P ("      return Gela.Grammars.LR_Parsers.State_Index;");
   Output.P;
   Output.P ("end Gela.Mutables.Parser_Data;");

   Output.P;
   Output.P ("package body Gela.Mutables.Parser_Data is");
   Output.P ("   use Gela.Grammars.LR_Parsers;");
   Output.P;

   Print_Go_To;
   Print_Action;

   Output.P ("end Gela.Mutables.Parser_Data;");
   Output.P;
   Output.P ("with Gela.Grammars;");
   Output.P ("with Gela.Mutables.Parsers;");
   Output.P ("procedure Gela.Mutables.On_Reduce");
   Output.P ("  (Self  : access Gela.Mutables.Parsers.Parser;");
   Output.P ("   Prod  : Gela.Grammars.Production_Index;");
   Output.P ("   Nodes : in out " &
               "Gela.Mutables.Parsers.Node_Array);");
   Output.P ("pragma Preelaborate (Gela.Mutables.On_Reduce);");

   Output.P ("with Gela.Nodes.Convertions; use Gela.Nodes.Convertions;");
   Output.P ("with Gela.Mutables.Compilations;");
   Output.P ("pragma Unreferenced (Gela.Mutables.Compilations);");
   Output.P ("pragma Style_Checks (""N"");");
   Output.P;
   Output.P ("procedure Gela.Mutables.On_Reduce");
   Output.P ("  (Self  : access Gela.Mutables.Parsers.Parser;");
   Output.P ("   Prod  : Gela.Grammars.Production_Index;");
   Output.P ("   Nodes : in out " &
               "Gela.Mutables.Parsers.Node_Array) is");
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
   Output.P ("end Gela.Mutables.On_Reduce;");

   Ada.Text_IO.Put_Line (Output.Text.To_UTF_8_String);

   Resolver.Resolve (AG, Table.all);
   Gela.Grammars_Debug.Print_Conflicts (AG, Table.all);

   if Ada.Command_Line.Argument_Count > 1 then
      Gela.Grammars_Debug.Print (G);
   end if;
end YACC_Driver;
