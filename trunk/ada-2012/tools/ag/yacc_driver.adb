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

   procedure Put_Proc_Decl
     (Output : in out Writer;
      Suffix : Wide_Wide_String);

   procedure Put_Piece
     (Piece : in out Writer;
      From  : Gela.Grammars.Production_Index;
      To    : Gela.Grammars.Production_Index);

   procedure Put_Rule
     (Output : in out Writer;
      Prod   : Gela.Grammars.Production;
      Rule   : Gela.Grammars.Rule);

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
     (Output : in out Writer;
      Prod   : Gela.Grammars.Production;
      Rule   : Gela.Grammars.Rule)
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

   procedure Put_Proc_Decl
     (Output : in out Writer;
      Suffix : Wide_Wide_String) is
   begin
      Output.N ("procedure Gela.LARL_Parsers.On_Reduce");
      Output.P (Suffix);
      Output.P ("  (Self  : access Parser_Context;");
      Output.P ("   Prod  : Gela.Grammars.Production_Index;");
      Output.N ("   Nodes : in out " &
                  "Gela.LARL_Parsers_Nodes.Node_Array)");
   end Put_Proc_Decl;

   procedure Put_Piece
     (Piece : in out Writer;
      From  : Gela.Grammars.Production_Index;
      To    : Gela.Grammars.Production_Index)
   is
      Suffix : Wide_Wide_String :=
        Gela.Grammars.Production_Index'Wide_Wide_Image (From);
   begin
      Suffix (1) := '_';

      Piece.P ("with Gela.Grammars;");
      Piece.P ("with Gela.LARL_Parsers_Nodes;");
      Piece.N ("private ");
      Put_Proc_Decl (Piece, Suffix);
      Piece.P (";");
      Piece.N ("pragma Preelaborate (Gela.LARL_Parsers.On_Reduce");
      Piece.N (Suffix);
      Piece.P (");");
      Piece.P;

      Piece.P ("pragma Warnings (""U"");");
      Piece.P ("with Gela.Elements.Aspect_Specifications;");
      Piece.P ("with Gela.Elements.Associations;");
      Piece.P ("with Gela.Elements.Basic_Declarative_Items;");
      Piece.P ("with Gela.Elements.Case_Expression_Paths;");
      Piece.P ("with Gela.Elements.Case_Paths;");
      Piece.P ("with Gela.Elements.Clause_Or_Pragmas;");
      Piece.P ("with Gela.Elements.Compilation_Units;");
      Piece.P ("with Gela.Elements.Component_Items;");
      Piece.P ("with Gela.Elements.Context_Items;");
      Piece.P ("with Gela.Elements.Declarative_Items;");
      Piece.P ("with Gela.Elements.Defining_Identifiers;");
      Piece.P ("with Gela.Elements.Discrete_Choices;");
      Piece.P ("with Gela.Elements.Discrete_Subtype_Definitions;");
      Piece.P ("with Gela.Elements.Discriminant_Specifications;");
      Piece.P ("with Gela.Elements.Enumeration_Literal_Specifications;");
      Piece.P ("with Gela.Elements.Exception_Choices;");
      Piece.P ("with Gela.Elements.Exception_Handlers;");
      Piece.P ("with Gela.Elements.Generic_Associations;");
      Piece.P ("with Gela.Elements.Generic_Formals;");
      Piece.P ("with Gela.Elements.If_Else_Expression_Paths;");
      Piece.P ("with Gela.Elements.If_Elsif_Else_Paths;");
      Piece.P ("with Gela.Elements.Membership_Choices;");
      Piece.P ("with Gela.Elements.Names;");
      Piece.P ("with Gela.Elements.Parameter_Specifications;");
      Piece.P ("with Gela.Elements.Pragma_Argument_Associations;");
      Piece.P ("with Gela.Elements.Program_Unit_Names;");
      Piece.P ("with Gela.Elements.Protected_Element_Declarations;");
      Piece.P ("with Gela.Elements.Protected_Operation_Declarations;");
      Piece.P ("with Gela.Elements.Protected_Operation_Items;");
      Piece.P ("with Gela.Elements.Select_Or_Else_Paths;");
      Piece.P ("with Gela.Elements.Select_Then_Abort_Paths;");
      Piece.P ("with Gela.Elements.Statements;");
      Piece.P ("with Gela.Elements.Subtype_Marks;");
      Piece.P ("with Gela.Elements.Task_Items;");
      Piece.P ("with Gela.Elements.Variants;");

      Piece.P ("with Gela.LARL_Parsers_Nodes;");
      Piece.P ("use Gela.LARL_Parsers_Nodes;");
      Piece.P ("pragma Style_Checks (""N"");");

      Put_Proc_Decl (Piece, Suffix);
      Piece.P (" is");
      Piece.P ("begin");
      Piece.P ("   case Prod is");

      for Prod of Plain.Production (From .. To) loop
         Piece.N ("      when");
         Piece.N (Gela.Grammars.Production_Index'Wide_Wide_Image (Prod.Index));
         Piece.P (" =>");

         for Rule of Plain.Rule (Prod.First_Rule .. Prod.Last_Rule) loop
            Put_Rule (Piece, Prod, Rule);
         end loop;

         if Prod.First_Rule > Prod.Last_Rule then
            Piece.P ("         null;");
         end if;
      end loop;

      Piece.P ("      when others =>");
      Piece.P ("         raise Constraint_Error;");
      Piece.P ("   end case;");
      Piece.N ("end Gela.LARL_Parsers.On_Reduce");
      Piece.N (Suffix);
      Piece.P (";");
   end Put_Piece;

   use type Gela.Grammars.Production_Count;

   Piece_Length : constant Gela.Grammars.Production_Count := 500;

   Piece : Writer;
begin
   Resolver.Resolve (AG, Table.all);

   Output.P ("with Gela.Grammars;");
   Output.P ("with Gela.Grammars.LR_Parsers;");
   Output.P;
   Output.P ("package Gela.LARL_Parsers.Data is");
   Output.P ("   pragma Preelaborate;");
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
   Output.P ("end Gela.LARL_Parsers.Data;");

   Output.P;
   Output.P ("package body Gela.LARL_Parsers.Data is");
   Output.P ("   use Gela.Grammars.LR_Parsers;");
   Output.P;

   Print_Go_To;
   Print_Action;

   Output.P ("end Gela.LARL_Parsers.Data;");
   Output.P;
   Output.P ("with Gela.Grammars;");
   Output.P ("with Gela.LARL_Parsers_Nodes;");
   Output.N ("private ");
   Put_Proc_Decl (Output, "");
   Output.P (";");
   Output.P ("pragma Preelaborate (Gela.LARL_Parsers.On_Reduce);");

   Output.P;

   for Piece_Index in 0 .. (Plain.Last_Production - 1) / Piece_Length loop
      declare
         From  : constant Gela.Grammars.Production_Index :=
           Piece_Index * Piece_Length + 1;
      begin
         Output.N ("with Gela.LARL_Parsers.On_Reduce_");
         Output.N (Natural (From));
         Output.P (";");
      end;
   end loop;

   Put_Proc_Decl (Output, "");
   Output.P (" is");
   Output.P ("begin");
   Output.P ("   case Prod is");

   for Piece_Index in 0 .. (Plain.Last_Production - 1) / Piece_Length loop
      declare
         From  : constant Gela.Grammars.Production_Index :=
           Piece_Index * Piece_Length + 1;
         To    : constant Gela.Grammars.Production_Index :=
           Gela.Grammars.Production_Index'Min
             (Plain.Last_Production, (Piece_Index + 1) * Piece_Length);
      begin
         Output.N ("      when ");
         Output.N (Natural (From));
         Output.N (" .. ");
         Output.N (Natural (To));
         Output.P (" =>");
         Output.N ("         On_Reduce_");
         Output.N (Natural (From));
         Output.P (" (Self, Prod, Nodes);");

         Put_Piece
           (Piece => Piece,
            From  => From,
            To    => To);
      end;
   end loop;

   Output.P ("      when others =>");
   Output.P ("         raise Constraint_Error;");
   Output.P ("   end case;");
   Output.P ("end Gela.LARL_Parsers.On_Reduce;");

   Ada.Text_IO.Put_Line (Output.Text.To_UTF_8_String);
   Ada.Text_IO.Put_Line (Piece.Text.To_UTF_8_String);

   Gela.Grammars_Debug.Print_Conflicts (AG, Table.all);

   if Ada.Command_Line.Argument_Count > 1 then
      Gela.Grammars_Debug.Print (G);
   end if;
end YACC_Driver;
