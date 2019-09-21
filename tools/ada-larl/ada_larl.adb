--                        G E L A   G R A M M A R S                         --
--          Library for dealing with grammars for Gela project,             --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with Ada.Command_Line;
with Ada.Text_IO;

with Anagram.Grammars;
with Anagram.Grammars.Reader;
with Anagram.Grammars_Convertors;
with Anagram.Grammars.Rule_Templates;
with Anagram.Grammars_Debug;
with Anagram.Grammars.LR_Tables;
with Anagram.Grammars.LR.LALR;
with Anagram.Grammars.Constructors;
with Anagram.Grammars.Conflicts;
with Writers;                 use Writers;

with League.String_Vectors;
with League.Strings;

procedure Ada_LARL is

   use type Anagram.Grammars.Rule_Count;
   use type Anagram.Grammars.LR.State_Count;

   procedure Put_Proc_Decl
     (Output : in out Writer;
      Suffix : Wide_Wide_String);

   procedure Put_Piece
     (Piece : in out Writer;
      From  : Anagram.Grammars.Production_Index;
      To    : Anagram.Grammars.Production_Index);

   procedure Put_Rule
     (Output : in out Writer;
      Prod   : Anagram.Grammars.Production;
      Rule   : Anagram.Grammars.Rule);

   function Image (X : Integer) return Wide_Wide_String;

   procedure Print_Go_To;
   procedure Print_Action;

   File  : constant String := Ada.Command_Line.Argument (1);
   G     : constant Anagram.Grammars.Grammar :=
     Anagram.Grammars.Reader.Read (File);
   Plain : constant Anagram.Grammars.Grammar :=
     Anagram.Grammars_Convertors.Convert (G, Left => False);
   AG    : constant Anagram.Grammars.Grammar :=
     Anagram.Grammars.Constructors.To_Augmented (Plain);
   Table : constant Anagram.Grammars.LR_Tables.Table_Access :=
     Anagram.Grammars.LR.LALR.Build
       (Input        => AG,
        Right_Nulled => False);
   Resolver : Anagram.Grammars.Conflicts.Resolver;
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
      use type Anagram.Grammars.Production_Count;
      use type Anagram.Grammars.Part_Count;
      type Action_Code is mod 2 ** 16;

      Count  : Natural;
      Code : Action_Code;
   begin
      Output.P ("   type Action_Code is mod 2 ** 16;");
      Output.P ("   for Action_Code'Size use 16;");
      Output.P;
      Output.P ("   Action_Table : constant array");
      Output.N ("     (State_Index range 1 .. ");
      Output.N (Natural (Anagram.Grammars.LR_Tables.Last_State (Table.all)));
      Output.P (",");
      Output.N ("      Anagram.Grammars.Terminal_Count range 0 .. ");
      Output.N (Natural (Plain.Last_Terminal));
      Output.P (") of Action_Code :=");

      for State in 1 .. Anagram.Grammars.LR_Tables.Last_State (Table.all) loop
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
               use Anagram.Grammars.LR_Tables;
               S : constant Anagram.Grammars.LR.State_Count :=
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
      Output.P ("      NT    : Anagram.Grammars.Non_Terminal_Index;");
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
      Output.P ("     (State : Anagram.Grammars.LR_Parsers.State_Index;");
      Output.P ("      Token : Anagram.Grammars.Terminal_Count;");
      Output.P ("      Value : out Anagram.Grammars.LR_Parsers.Action)");
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
                  "Anagram.Grammars.Production_Index (Code),");
      Output.P ("                   NT    => Prods (Code).NT,");
      Output.P ("                   Parts => Prods (Code).Parts);");

      for State in 1 .. Anagram.Grammars.LR_Tables.Last_State (Table.all) loop
         if Anagram.Grammars.LR_Tables.Finish (Table.all, State) then
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
      Output.N ("     (Anagram.Grammars.LR_Parsers.State_Index range 1 .. ");
      Output.N (Natural (Anagram.Grammars.LR_Tables.Last_State (Table.all)));
      Output.P (",");
      Output.N ("      Anagram.Grammars.Non_Terminal_Index range 1 .. ");
      Output.N (Natural (Plain.Last_Non_Terminal));
      Output.P (") of State_Index :=");

      for State in 1 .. Anagram.Grammars.LR_Tables.Last_State (Table.all) loop
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
               use Anagram.Grammars.LR;
               Next : constant State_Count :=
                 Anagram.Grammars.LR_Tables.Shift (Table.all, State, NT);
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
      Output.P ("     (State : Anagram.Grammars.LR_Parsers.State_Index;");
      Output.P ("      NT    : Anagram.Grammars.Non_Terminal_Index)");
      Output.P ("      return Anagram.Grammars.LR_Parsers.State_Index");
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
      Prod   : Anagram.Grammars.Production;
      Rule   : Anagram.Grammars.Rule)
   is
      use Anagram.Grammars.Rule_Templates;
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
      Output.N ("procedure Program.Parsers.On_Reduce");
      Output.P (Suffix);
      Output.P ("  (Self  : access Parse_Context;");
      Output.P ("   Prod  : Anagram.Grammars.Production_Index;");
      Output.N ("   Nodes : in out " &
                  "Program.Parsers.Nodes.Node_Array)");
   end Put_Proc_Decl;

   procedure Put_Piece
     (Piece : in out Writer;
      From  : Anagram.Grammars.Production_Index;
      To    : Anagram.Grammars.Production_Index)
   is
      Suffix : Wide_Wide_String :=
        Anagram.Grammars.Production_Index'Wide_Wide_Image (From);
   begin
      Suffix (1) := '_';

      Piece.P ("with Anagram.Grammars;");
      Piece.P ("with Program.Parsers.Nodes;");
      Piece.N ("private ");
      Put_Proc_Decl (Piece, Suffix);
      Piece.P (";");
      Piece.N ("pragma Preelaborate (Program.Parsers.On_Reduce");
      Piece.N (Suffix);
      Piece.P (");");
      Piece.P;

--      Piece.P ("pragma Warnings (""U"");");

      Piece.P ("with Program.Parsers.Nodes;");
      Piece.P ("use Program.Parsers.Nodes;");
      Piece.P ("pragma Style_Checks (""N"");");

      Put_Proc_Decl (Piece, Suffix);
      Piece.P (" is");
      Piece.P ("begin");
      Piece.P ("   case Prod is");

      for Prod of Plain.Production (From .. To) loop
         Piece.N ("      when");
         Piece.N
           (Anagram.Grammars.Production_Index'Wide_Wide_Image (Prod.Index));
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
      Piece.N ("end Program.Parsers.On_Reduce");
      Piece.N (Suffix);
      Piece.P (";");
   end Put_Piece;

   use type Anagram.Grammars.Production_Count;

   Piece_Length : constant Anagram.Grammars.Production_Count := 500;

   Piece : Writer;
begin
   Resolver.Resolve (AG, Table.all);

   Output.P ("with Anagram.Grammars;");
   Output.P ("with Anagram.Grammars.LR_Parsers;");
   Output.P;
   Output.P ("package Program.Parsers.Data is");
   Output.P ("   pragma Preelaborate;");
   Output.P;
   Output.P ("   procedure Next_Action");
   Output.P ("     (State : Anagram.Grammars.LR_Parsers.State_Index;");
   Output.P ("      Token : Anagram.Grammars.Terminal_Count;");
   Output.P ("      Value : out Anagram.Grammars.LR_Parsers.Action);");
   Output.P;
   Output.P ("   function Go_To");
   Output.P ("     (State : Anagram.Grammars.LR_Parsers.State_Index;");
   Output.P ("      NT    : Anagram.Grammars.Non_Terminal_Index)");
   Output.P ("      return Anagram.Grammars.LR_Parsers.State_Index;");
   Output.P;
   Output.P ("end Program.Parsers.Data;");

   Output.P;
   Output.P ("package body Program.Parsers.Data is");
   Output.P ("   use Anagram.Grammars.LR_Parsers;");
   Output.P;

   Print_Go_To;
   Print_Action;

   Output.P ("end Program.Parsers.Data;");
   Output.P;
   Output.P ("with Anagram.Grammars;");
   Output.P ("with Program.Parsers.Nodes;");
   Output.N ("private ");
   Put_Proc_Decl (Output, "");
   Output.P (";");
   Output.P ("pragma Preelaborate (Program.Parsers.On_Reduce);");

   Output.P;

   for Piece_Index in 0 .. (Plain.Last_Production - 1) / Piece_Length loop
      declare
         From  : constant Anagram.Grammars.Production_Index :=
           Piece_Index * Piece_Length + 1;
      begin
         Output.N ("with Program.Parsers.On_Reduce_");
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
         From  : constant Anagram.Grammars.Production_Index :=
           Piece_Index * Piece_Length + 1;
         To    : constant Anagram.Grammars.Production_Index :=
           Anagram.Grammars.Production_Index'Min
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
   Output.P ("end Program.Parsers.On_Reduce;");

   Ada.Text_IO.Put_Line (Output.Text.To_UTF_8_String);
   Ada.Text_IO.Put_Line (Piece.Text.To_UTF_8_String);

   Anagram.Grammars_Debug.Print_Conflicts (AG, Table.all);

   if Ada.Command_Line.Argument_Count > 1 then
      Anagram.Grammars_Debug.Print (G);
   end if;
end Ada_LARL;
