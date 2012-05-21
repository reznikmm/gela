with Nodes;
with Tokens;
with Gela.Containers.Lists;
with Ada.Text_IO;
with Gramar_Items;           use Gramar_Items;
with Gramar_Items.Code;
with Ada.Strings.Unbounded;

package body Generate is

   procedure Print_Token (Name : in String);
   procedure Print_Rule (Item : in Rule);
   procedure Print_Token_Rule (Name : in String);

   Max_Position : constant := 20;

   subtype Item_Position is Positive range 1 .. Max_Position;
   subtype Iteration is Natural range 0 .. Max_Position;
   type Positions_List is array (Item_Position) of Iteration;

   function "&"
     (Left  : Positions_List;
      Right : Item_Position) return Positions_List;

   Empty_List : constant Positions_List := (others => 0);

   procedure Print_Sequence
     (Item                : in Sequence;
      Rule_Name           : in String;
      Name_Of_List        : in String := "");

   procedure Print_Sequence
     (Item                : in Sequence;
      Rule_Name           : in String;
      Start_From          : in Item_Position;
      Processed_Positions : in Positions_List;
      Name_Of_List        : in String;
      First_In_Rule       : in boolean);

   procedure Print_Sequence_Code
     (Item                : in Sequence;
      Processed_Positions : in Positions_List;
      Name_Of_List        : in String);

   procedure Print_List_Code
     (Item                : in Sequence;
      Rule_Name           : in String;
      Processed_Positions : in Positions_List;
      Name_Of_List        : in String);

   procedure Print_Infix_Code
     (Seq                 : in Sequence;
      Rule_Name           : in String;
      Processed_Positions : in Positions_List;
      Name_Of_List        : in String);

   procedure Print_Infix_Code1
     (Seq                 : in Sequence;
      Rule_Name           : in String;
      Processed_Positions : in Positions_List;
      Name_Of_List        : in String);

   procedure Print_Infix_Code2
     (Processed_Positions : in Positions_List);

   procedure Print_Infix_Code3
     (Seq                 : in Sequence;
      Name_Of_List        : in String);

   procedure Print_Infix_Code4
     (Processed_Positions : in Positions_List;
      List_Index          : in Positive := 2);

   procedure Print_Infix_Code5
     (Seq                 : in Sequence;
      Rule_Name           : in String;
      Processed_Positions : in Positions_List;
      Name_Of_List        : in String);

   procedure Print_Infix_Code6
     (Seq                 : in Sequence;
      Name_Of_List        : in String);

   procedure Print_Infix_Code7
     (Seq                 : in Sequence;
      Name_Of_List        : in String);

   procedure Create_Alt_Nodes
     (Seq                 : in Sequence;
      Processed_Positions : in Positions_List);

   function Count_Total_Positions (Item : Sequence) return Natural;

   function Include
     (Positions : Positions_List;
      Position  : Item_Position) return Boolean;

   function Include
     (Positions : Positions_List;
      Position  : Item_Position) return Iteration;

   procedure Put_Line (Text : String) renames Ada.Text_IO.Put_Line;
   procedure New_Line;
   procedure Put (Text : String) renames Ada.Text_IO.Put;
   procedure Options_And_Lists_In_Sequence (Item : Sequence);
   procedure Print_List (Item : in List);
   procedure Print_Option (Item : in Option);

   procedure Create_Node_Code
     (Item                : in Sequence;
      Processed_Positions : in Positions_List;
      Name_Of_List        : in String;
      Node_Type_Name      : in String);

   procedure Print_Code
     (Child       : Item'Class;
      Parent_Type : String;
      Parent_Name : String;
      Index       : Positive;
      Force       : Boolean);

   procedure Print_Wrap_Code
     (Child : Wrapper;
      Last  : Boolean);

   function Find_Attribute
     (Child          : Item'Class;
      Node_Type_Name : String) return String;

   function Find_Attribute
     (Name      : String;
      Type_Name : String;
      User_Attr : String;
      Node_Type_Name : String) return String;

   function Find_Procedure
     (Node_Name : String;
      Attr_Name : String) return String;

   function Indent return String;

   procedure Step_Indent (Step : Integer);

   --  Append "_r" for pragma and range names
   function Translate_Name (Name : String) return String;
   function Is_List (Item : Sequence) return Boolean;
   function Is_List_Type (Node_Name : String) return Boolean;
   function Quote (Text : String; Double : Boolean) return String;

   function Processed
     (Seq                 : in Sequence;
      Processed_Positions : in Positions_List;
      Index               : Positive) return Boolean;

   function Real_Index
     (Seq                 : in Sequence;
      Processed_Positions : in Positions_List;
      Index               : in Positive;
      Name_Of_List        : in String) return Positive;

   function New_Node (Type_Name : String) return String;

   procedure Set_Name_Image_Code
     (Name     : in String;
      Child    : in Item'Class;
      Index    : in Positive := 1;
      Proc     : in String   := "Name_Image";
      In_Quote : in Boolean  := True);

   function Is_First
     (Seq                 : in Sequence;
      Index               : in Positive;
      Processed_Positions : in Positions_List) return Boolean;

   function Is_Last
     (Seq                 : in Sequence;
      Index               : in Positive;
      Processed_Positions : in Positions_List) return Boolean;

   function Just_Keywords (Item : in Sequence) return Boolean;

   package U renames Ada.Strings.Unbounded;
   use type U.Unbounded_String;

   package Unbounded_String_Lists is
      new Gela.Containers.Lists (U.Unbounded_String);

   Printed_Lists   : Unbounded_String_Lists.List;
   Printed_Options : Unbounded_String_Lists.List;
   List_Phase      : Boolean := False;

   ---------
   -- "&" --
   ---------

   function "&"
     (Left  : Positions_List;
      Right : Item_Position) return Positions_List
   is
      Result : Positions_List := Left;
   begin
      Result (Right) := Result (Right) + 1;
      return Result;
   end "&";

   procedure All_Tokens is
      procedure Print_Tokens is new Tokens.For_Each_Name (Print_Token);
   begin
      Print_Tokens;
   end All_Tokens;

   procedure Print_Token (Name : in String) is
   begin
      Put_Line ("%token " & Name & "_Token");
   end Print_Token;

   procedure Start_Rule is
   begin
      New_Line;
      Put_Line ("%start compilation");
      New_Line;
      Put_Line ("%with Gela.Source_Buffers;");
      Put_Line ("%use Gela;");
      New_Line;
      Put_Line ("{");
      Put_Line ("   type YYSTYPE is record");
      Put_Line ("      Index                    : Asis.ASIS_Natural;");
      Put_Line ("      Start_Line               : Asis.ASIS_Natural;");
      Put_Line ("      Start_Column, End_Column : Asis.ASIS_Natural;");
      Put_Line ("      From, To                 : Source_Buffers.Cursor;");
      Put_Line ("   end record;");
      New_Line;
      Put_Line ("   subtype T       is YYSTYPE;");
      Put_Line ("}");
      New_Line;
      Put_Line ("%%");
      New_Line;
   end Start_Rule;

   procedure All_Rules is
   begin
      for I in 1 .. Rule_Count loop
         Print_Rule (Get_Rule (I));
      end loop;
   end All_Rules;

   procedure Print_Rule (Item : in Rule) is
      Rule_Name : constant String := Translate_Name (Name (Item));
   begin
      if Code.Skip_Rule (Rule_Name) then
         return;
      end if;

      Put_Line (Rule_Name & " :");
      for I in 1 .. Count (Item) loop
         if I > 1 then
            Put ("   |");
         else
            Put ("    ");
         end if;
         Print_Sequence (Get_Alternative (Item, I), Rule_Name);
      end loop;
      Put_Line (";");
      New_Line;
   end Print_Rule;

   procedure Print_Sequence
     (Item                : in Sequence;
      Rule_Name           : in String;
      Name_Of_List        : in String := "") is
   begin
      Print_Sequence
        (Item, Rule_Name, 1, Empty_List, Name_Of_List, True);
   end Print_Sequence;

   procedure Print_Sequence
     (Item                : in Sequence;
      Rule_Name           : in String;
      Start_From          : in Item_Position;
      Processed_Positions : in Positions_List;
      Name_Of_List        : in String;
      First_In_Rule       : in Boolean)
   is
      Item_Count : constant Natural := Count (Item);

      procedure Expand_Option
        (Item   : Option;
         Nested : Boolean)
      is
         Seq : constant Sequence := Items (Item);
      begin
         for I in 1 .. Count (Seq) loop
            declare
               Child : Gramar_Items.Item'Class renames Get_Item (Seq, I).all;
            begin
               if Child in Reference then
                  Put(" " & Translate_Name (Item_Name (Child)));
               elsif Child in Option then
                  if Nested then
                     Put(" " & Item_Name (Child));
                  end if;
               elsif Child in Keyword or Child in Delimiter then
                  Put(" " & Item_Name (Child));
               else
                  Put (Rule_Name &
                       "!!!! List not supported in inlined option !!!");
                  raise Constraint_Error;
               end if;
            end;
         end loop;
      end Expand_Option;

      use type Ada.Text_IO.Count;
      First : Boolean := First_In_Rule;
   begin
      for I in Start_From .. Item_Count loop
         declare
            Child : Gramar_Items.Item'Class renames Get_Item (Item, I).all;
         begin
            --  Expand nested option
            if Child in Option then
               if Count (Option (Child)) > Include (Processed_Positions, I)
               then
                  Print_Sequence         --  recursive call
                    (Item                => Item,
                     Rule_Name           => Rule_Name,
                     Start_From          => I,
                     Processed_Positions => Processed_Positions & I,
                     Name_Of_List        => Name_Of_List,
                     First_In_Rule       => First);

                  First := False;
               elsif Inline_Option (Option (Child)) then
                  case Count_Total_Positions (Items (Option (Child))) is
                     when 0 =>
                        null;
                     when 1 =>
                        Print_Sequence         --  recursive call
                          (Item                => Item,
                           Rule_Name           => Rule_Name,
                           Start_From          => I + 1,
                           Processed_Positions => Processed_Positions & I & I,
                           Name_Of_List        => Name_Of_List,
                           First_In_Rule       => First);

                        First := False;
                     when others =>
                        raise Constraint_Error;
                  end case;
               end if;
            elsif Child in List then
               Print_Sequence         --  recursive call
                 (Item                => Item,
                  Rule_Name           => Rule_Name,
                  Start_From          => I + 1,
                  Processed_Positions => Processed_Positions & I,
                  Name_Of_List        => Name_Of_List,
                  First_In_Rule       => First);

               First := False;
            end if;
         end;
      end loop;

      if First then
         First := False;
      else
         Put ("   |");
      end if;

      if Name_Of_List /= "" then
         Put(" " & Name_Of_List);
      end if;

      for I in 1 .. Item_Count loop
         declare
            Child : Gramar_Items.Item'Class renames Get_Item (Item, I).all;
            Print : Boolean := False;
         begin
            if (Child in List or Child in Option) then
               if Include (Processed_Positions, I) then
                  Print := True;
               end if;
            else
               Print := True;
            end if;
            if Print then
               if Ada.Text_IO.Col > 55 then
                  New_Line;
                  Put ("    ");
               end if;
               if Child in Reference then
                  Put(" " & Translate_Name (Item_Name (Child)));
               elsif Child in Option then
                  if Count (Option (Child)) > 1 then
                     declare
                        Index : constant Positive :=
                          Include (Processed_Positions, I);
                        Seq : constant Sequence :=
                          Items (Option (Child), Index);
                     begin

                        if Count (Seq) /= 1 then
                           Put (Rule_Name &
                                "!!!!  Multiple items in option alternative");
                           raise Constraint_Error;
                        end if;

                        Put(" " & Item_Name (Get_Item (Seq, 1).all));
                     end;
                  elsif Inline_Option (Option (Child)) then
                     Expand_Option
                       (Option (Child),
                        Include (Processed_Positions, I) > 1);
                  else
                     Put(" " & Item_Name (Child));
                  end if;
               else
                  Put(" " & Item_Name (Child));
               end if;
            end  if;
         end;
      end loop;

      New_Line;

      Print_Sequence_Code (Item, Processed_Positions, Name_Of_List);

   end Print_Sequence;

   function Count_Total_Positions (Item : Sequence) return Natural is
      Counted : Natural := 0;
   begin
      for I in 1 .. Count (Item) loop
         declare
            Child  : Gramar_Items.Item'Class renames Get_Item (Item, I).all;
            --  No more 1 nested inline options supported
            Nested : Integer range 0 .. 1;
         begin
            if Child in List then
               Counted := Counted + 1;
            elsif Child in Option then
               if Inline_Option (Option (Child)) then
                  Nested  := Count_Total_Positions (Items (Option (Child)));
                  Counted := Counted + Nested;
               end if;

               Counted := Counted + 1;
            end if;
         end;
      end loop;
      return Counted;
   end Count_Total_Positions;

   function Include
     (Positions : Positions_List;
      Position  : Item_Position) return Boolean is
   begin
      return Positions (Position) > 0;
   end Include;

   function Include
     (Positions : Positions_List;
      Position  : Item_Position) return Iteration is
   begin
      return Positions (Position);
   end Include;

   procedure New_Line is
   begin
      Ada.Text_IO.New_Line;
   end New_Line;

   procedure Options_And_Lists is
   begin
      List_Phase := True;
      for I in 1 .. Rule_Count loop
         declare
            Current_Rule : constant Rule := Get_Rule (I);
         begin
            for J in 1 .. Count (Current_Rule) loop
               Options_And_Lists_In_Sequence
                 (Get_Alternative (Current_Rule, J));
            end loop;
         end;
      end loop;
      --  Put_Line ("%%");
   end Options_And_Lists;

   procedure Options_And_Lists_In_Sequence (Item : Sequence) is
   begin
      for I in 1 .. Count (Item) loop
         declare
            Child : Gramar_Items.Item'Class renames Get_Item (Item, I).all;
         begin
            if Child in List then
               Print_List (List (Child));
               Options_And_Lists_In_Sequence (Items (List (Child)));
            elsif Child in Option then
               if not Inline_Option (Option (Child)) then
                  Print_Option (Option (Child));
                  Options_And_Lists_In_Sequence (Items (Option (Child)));
               end if;
            end if;
         end;
      end loop;
   end Options_And_Lists_In_Sequence;

   procedure Print_List (Item : in List) is
      use Unbounded_String_Lists;
      Name : constant String := Item_Name (Item);
   begin
      if not Contains (Printed_Lists, U.To_Unbounded_String (Name)) then
         Put_Line (Name & " :");
         Put ("    ");
         Print_Sequence (Items (Item), Name, Name);
         Put ("   |");
         Print_Sequence (Items (Item), Name);
         Put_Line (";");
         New_Line;
         Append (Printed_Lists, U.To_Unbounded_String (Name));
      end if;
   end Print_List;

   procedure Print_Option (Item : in Option) is
      use Unbounded_String_Lists;
      Name : constant String := Item_Name (Item);
   begin
      if Separate_Option (Item) then
         if not Contains (Printed_Options, U.To_Unbounded_String (Name)) then
            Put_Line (Name & " :");
            Put ("    ");
            Print_Sequence (Items (Item), Name);

            for J in 2 .. Count (Item) loop
               Put ("    |");
               Print_Sequence (Items (Item, J), Name);
            end loop;

            Put_Line (";");
            New_Line;
            Append (Printed_Options, U.To_Unbounded_String (Name));
         end if;
      end if;
   end Print_Option;

   function Is_List (Item : Sequence) return Boolean is
      R_Name : constant String := Rule_Name (Item);
      N_Name : constant String := Node_Name (Item);
   begin
      if R_Name'Length > 4 and then
        R_Name (R_Name'Last - 4 .. R_Name'Last) = "_list" then

         return True;
      end if;
      if N_Name'Length > 4 and then
        N_Name (N_Name'Last - 4 .. N_Name'Last) = ".List" then

         return True;
      end if;
      return False;
   end Is_List;

   procedure Print_Sequence_Code
     (Item                : in Sequence;
      Processed_Positions : in Positions_List;
      Name_Of_List        : in String)
   is
      R_Name : constant String := Rule_Name (Item);
   begin

      if Infix (Item) /= "" then

         Print_Infix_Code (Item, R_Name, Processed_Positions, Name_Of_List);
         return;

      end if;

      if Pass_Through (Item) then

         declare
            First_Reference : constant Natural := Find_First_Reference (Item);
         begin
            if First_Reference = 0 and Count (Item) > 1 then
               declare
                  Child : Gramar_Items.Item'Class
                    renames Get_Item (Item, 2).all;
               begin
                  if Child in Option and Include (Processed_Positions, 2) then
                     Put_Line ("{$$ := $2;}");
                  end if;
               end;
            elsif First_Reference /= 0 then
               declare
                  Index : constant Positive := Real_Index (Item,
                     Processed_Positions, First_Reference, Name_Of_List);
               begin
                  Put_Line ("{ $$ := $" & To_String (Index) & ";}");
               end;
            end if;
         end;
         return;

      end if;

      if True_Node (Item) /= "" then
         declare
            Index : constant Natural := Choise_Item_Index (Item);
            Child : Gramar_Items.Item'Class renames Get_Item (Item, Index).all;
         begin
            if Child not in Option or else
              Include (Processed_Positions, Index) then

               Create_Alt_Nodes (Item, Processed_Positions);
               return;
            end if;
         end;
      end if;


      if R_Name = "statement" then
         if Include (Processed_Positions, 1) then
            Put_Line ("{");
            Put_Line (Indent & "Set (C, $2.Index, Label_Names, $1.Index);");
            Put_Line (Indent & "$$ := $2;");
            Put_Line ("}");
         end if;
      elsif Node_Name (Item) /= "" then
         Put_Line ("{");
         Create_Node_Code (Item, Processed_Positions, Name_Of_List, "");
         Put_Line ("}");
      end if;

   end Print_Sequence_Code;

   procedure Create_Node_Code
     (Item                : in Sequence;
      Processed_Positions : in Positions_List;
      Name_Of_List        : in String;
      Node_Type_Name      : in String)
   is

      function Get_Node_Name
        (Wrap  : Wrapper) return String is
      begin
         if Top (Wrap) and Node_Type_Name /= "" then
            return Node_Type_Name;
         end if;
         for I in 1 .. Count (Item) loop
            declare
               Child : Gramar_Items.Item'Class renames Get_Item (Item, I).all;
            begin
               if Child in Option and then
                 Include (Processed_Positions, I) and then
                 Alternative_Node_Name (Option (Child)) /= "" and then
                 Parent (Child) = Wrap then

                 return Alternative_Node_Name (Option (Child));

               end if;
            end;
         end loop;
         return Node_Name (Wrap);
      end Get_Node_Name;

      procedure Expand_Option_Code
        (Item        : in     Option;
         Parent_Node : in     String;
         Parent_Name : in     String;
         Index       : in out Positive;
         With_Nested : in     Boolean)
      is
         Seq : Sequence := Items (Item);
      begin
         for I in 1 .. Count (Seq) loop
            declare
               Child : Gramar_Items.Item'Class renames Get_Item (Seq, I).all;
            begin
               if Child not in Option or With_Nested then
                  Print_Code (Child, Parent_Node, Parent_Name,
                              Index, Force => False);
                  Index := Index + 1;
               end if;
            end;
         end loop;

         --  Back to last element
         Index := Index - 1;
      end Expand_Option_Code;

      Index : Positive := 1;
      Force : Boolean := Just_Keywords (Item);
      Last  : array (0 .. Wrap_Count (Item)) of Natural := (others => 0);
   begin
      Put_Line (Indent & "declare");
      Step_Indent (+1);

      for I in 1 .. Wrap_Count (Item) loop
         declare
            Wrap : constant Wrapper := Get_Wrapper (Item, I);
            Node : constant String  := Get_Node_Name (Wrap);
            Ind  : constant Natural := Item_Index (Wrap);
         begin
            if Node /= "" then
               Put_Line (Indent  & Object_Name (Wrap) &
                         " : constant Element_Index :=");

               if Ind > 0 and then
                 Processed (Item, Processed_Positions, Ind) then

                  Put_Line (Indent & "  $" &
                            To_String (Real_Index (Item,
                                                   Processed_Positions,
                                                   Ind,
                                                   Name_Of_List)) &
                            ".Index;");

               elsif I = 1 and Name_Of_List /= "" then
                  Put_Line (Indent & "  $1.Index;");
               else
                  Put_Line (Indent & "  " & New_Node (Node) & ";");
               end if;
            end if;
         end;
      end loop;
      Step_Indent (-1);
      Put_Line (Indent & "begin");

      Step_Indent (+1);

      declare
         Wrap : constant Wrapper := Get_Wrapper (Item, 1);
      begin
         if Rule_Name (Item) = "compilation" then
            Put_Line (Indent & "Last_Compilation := " &
                      Object_Name (Wrap) & ";");
         else
            Put_Line (Indent & "$$.Index := " & Object_Name (Wrap) & ";");
         end if;
      end;

      if Name_Of_List /= "" then
         Index := Index + 1;
      end if;

      for I in 1 .. Count (Item) loop
         declare
            Child       : Gramar_Items.Item'Class
              renames Get_Item (Item, I).all;
            Wrap        : constant Wrapper := Parent (Child);
            Parent_Node : constant String := Get_Node_Name (Wrap);
            Parent_Name : constant String := Object_Name (Wrap);
            Wrap_Index  : constant Natural := Item_Index (Wrap);
         begin
            if Processed (Item, Processed_Positions, I) then

               if I /= Wrap_Index then
                  if Parent_Node /= "Primary_Token_Lists.List" then
                     Force := False;
                  end if;

                  if (not Is_List_Type (Parent_Node) or Child in Keyword)
                    and then Is_First (Item, I, Processed_Positions)
                    and then Parent_Node /= ""
                  then

                     Put_Line (Indent & "Set_Start_Position (" & Parent_Name &
                               ", $" & To_String (Index) & ");");

                  end if;

                  if Parent_Node /= "Primary_Token_Lists.List" then
                     Force := False;
                  end if;

                  if Child in Option and then
                    (Inline_Option (Option (Child)) or
                     Count (Option (Child)) > 1)
                  then
                     if Inline_Option (Option (Child)) then
                        Expand_Option_Code
                          (Option (Child),
                           Parent_Node,
                           Parent_Name,
                           Index,
                           Include (Processed_Positions, I) > 1);
                     else
                        declare
                           Ind : constant Positive :=
                             Include (Processed_Positions, I);
                           Seq : constant Sequence :=
                             Items (Option (Child), Ind);
                           Cld : Gramar_Items.Item'Class
                             renames Get_Item (Seq, 1).all;
                        begin
                           Print_Code
                             (Cld, Parent_Node, Parent_Name, Index, Force);
                        end;
                     end if;
                  else
                     Print_Code
                       (Child, Parent_Node, Parent_Name, Index, Force);
                  end if;

                  if (not Is_List_Type (Parent_Node) or Child in Keyword)
                    and then Is_Last (Item, I, Processed_Positions)
                    and then Parent_Node /= ""
                  then

                     Put_Line (Indent & "Set_End_Position (" & Parent_Name &
                               ", $" & To_String (Index) & ");");
                     Last (Wrapper_Index (Wrap)) := Index;
                  end if;
               else
                  Last (Wrapper_Index (Wrap)) := Index;
               end if;

               Index := Index + 1;

            end  if;
         end;
      end loop;

      for I in reverse 2 .. Wrap_Count (Item) loop
         declare
            Wrap : constant Wrapper := Get_Wrapper (Item, I);
            Up   : constant Wrapper := Parent (Wrap);
         begin
            Print_Wrap_Code (Wrap, Last (I) > Last (Wrapper_Index (Up)));
         end;
      end loop;

      Step_Indent (-1);
      Put_Line (Indent & "end;");

   end Create_Node_Code;

   procedure Print_Code
     (Child       : Item'Class;
      Parent_Type : String;
      Parent_Name : String;
      Index       : Positive;
      Force       : Boolean)
   is
      use Nodes;
      Attr        : constant String := Find_Attribute (Child, Parent_Type);
      Proc        : constant String := Find_Procedure (Parent_Type, Attr);
      Trait_Attr  : constant String :=
         Find_Attribute (Parent_Type, "Trait_Kind");
   begin

      if Trait_Attr /= "" then
         declare
            Trait : constant String := Trait_Name (Child);
         begin
            if Trait /= "" then
               Put_Line (Indent & "Set (C, " & Parent_Name & ", Properties." &
                         Trait_Attr &
                         ", Trait_Kinds'Pos (" & Trait & "));");

            end if;
         end;
      end if;

      if not Force and Attr = "" and
        (Child in Keyword or Child in Delimiter) then

         return;

      end if;

      if Attr /= "" then
         if Attribute_Type (Parent_Type, Attr) = "Source_String" then

            Set_Name_Image_Code (Parent_Name, Child, Index, Attr,
                In_Quote => Parent_Type = "Operator_Symbol_Node");

         elsif Attribute_Type (Parent_Type, Attr) = "Boolean" then

            Put_Line (Indent & "Set (C, " & Parent_Name & ", Properties." &
                      Attr & ", 1);");

--         elsif Attribute_Type (Parent_Type, Attr) = "Mode_Kinds" then

--            Put_Line (Indent & Proc & " (" & Parent_Name &
--                      ".all, Modes.$" & To_String (Index) &
--                      ");");
         elsif Value (Child) /= "" then
            Put_Line (Indent & "Set (C, " & Parent_Name & ", Properties." &
                      Attr & ", " & Value (Child) & ");");
         else
            Put_Line (Indent & "Set (C, " & Parent_Name & ", Properties." &
                      Attr & ", $" & To_String (Index) & ".Index);");
         end if;
      elsif Is_List_Type (Parent_Type) then
         Put_Line (Indent & Proc & Parent_Name &
                   ", $" & To_String (Index) & ".Index);");
      end if;
   end Print_Code;

   Current_Indent : Positive := 3;

   function Indent return String is
      Spaces : constant String := "                                    ";
   begin
      return Spaces (1 .. Current_Indent);
   end;

   procedure Step_Indent (Step : Integer) is
   begin
      Current_Indent := Current_Indent + 3 * Step;
   end Step_Indent;

   procedure Token_Rules is
      procedure Print_Token_Rules is
         new Tokens.For_Each_Name (Print_Token_Rule);
   begin
      Print_Token_Rules;
   end Token_Rules;

   procedure Print_Token_Rule (Name : in String) is
      Length : constant Natural := Tokens.Token_Length (Name);
      Cap    : constant String  := Nodes.Capitalise (Name);
   begin
      Put_Line (Name & " : " & Name & "_Token");
      Put_Line ("{ $$ := $1; };");
      New_Line;
   end Print_Token_Rule;

   function Find_Attribute
     (Child          : Item'Class;
      Node_Type_Name : String) return String
   is
      Name      : constant String := Item_Name (Child);
      Type_Name : constant String := Node_Name (Child);
      U_Attr    : constant String := User_Attr (Child);
   begin
      return Find_Attribute (Name, Type_Name, U_Attr, Node_Type_Name);
   end Find_Attribute;

   function Find_Attribute
     (Name      : String;
      Type_Name : String;
      User_Attr : String;
      Node_Type_Name : String) return String
   is
   begin
--      Put_Line ("Find_Attribute: " & Name);
--      Put_Line ("Type_Name: " & Type_Name);

      if User_Attr /= "" and then
        Nodes.Find_Attribute (Node_Type_Name, User_Attr) /= "" then

         return User_Attr;

      end if;

      if User_Attr /= "" then
         declare
            By_Type  : constant String := Nodes.Find_Attr_By_Type
              (Node_Type_Name, User_Attr);
         begin
            if By_Type /= "" then
               return By_Type;
            end if;
         end;
      end if;

      if Type_Name /= "" then
         declare
            Stripped : String renames Nodes.From_Kind (Type_Name);
            By_Name  : constant String := Nodes.Find_Attribute
              (Node_Type_Name, Stripped);
            By_Type  : constant String := Nodes.Find_Attr_By_Type
              (Node_Type_Name, Stripped);
         begin
            if By_Name /= "" then
               return By_Name;
            end if;
            if By_Type /= "" then
               return By_Type;
            end if;
         end;
      end if;

      declare
         Attr_Type : constant String := Nodes.Capitalise (Name);
         By_Name   : constant String := Nodes.Find_Attribute
           (Node_Type_Name, Attr_Type);
         By_Type   : constant String := Nodes.Find_Attr_By_Type
           (Node_Type_Name, Attr_Type);
      begin
         if By_Name /= "" then
            return By_Name;
         end if;
         if By_Type /= "" then
            return By_Type;
         end if;
      end;

      if Name = "identifier" then
         declare
            Attr_Name : constant String := Nodes.Find_Attr_By_Type
              (Node_Type_Name, "Source_String");
         begin
            if Attr_Name /= "" then
               return Attr_Name;
            end if;
         end;
      end if;

      if Name = "defining_identifier" or Name = "defining_identifier_list" then
         declare
            Attr_Name : constant String := Nodes.Find_Attribute
              (Node_Type_Name, "Name");
         begin
            if Attr_Name /= "" then
               return Attr_Name;
            end if;
         end;

      end if;

      return "";
   end Find_Attribute;

   --  Append "_r" for pragma and range names
   function Translate_Name (Name : String) return String is
   begin
      if Name = "pragma" or Name = "range" or Name = "body" then
         return Name & "_r";
      else
         return Name;
      end if;
   end Translate_Name;

   function Just_Keywords (Item : in Sequence) return Boolean is
   begin
      for I in 1 .. Count (Item) loop
         declare
            Child : Gramar_Items.Item'Class renames Get_Item (Item, I).all;
         begin
            if Child in Option then
               if not Just_Keywords (Items (Option (Child))) then
                  return False;
               end if;
            elsif not (Child in Delimiter or Child in Keyword) then
               return False;
            end if;
         end;
      end loop;

      return Count (Item) /= 0;
   end Just_Keywords;

   procedure Print_List_Code
     (Item                : in Sequence;
      Rule_Name           : in String;
      Processed_Positions : in Positions_List;
      Name_Of_List        : in String)
   is

      Index     : Positive := 1;
      Node      : constant String := Node_Name (Item);
      Node_Ptr  : constant String := Nodes.Get_Pointer_Name (Node);
      Create    : constant String := Create_Node (Get_Item (Item, 1).all);
      Crt_Ptr   : constant String := Nodes.Get_Pointer_Name (Create);
      Proc      : constant String := Find_Procedure (Node, "");
   begin
      if Node = "" then
         return;
      end if;

      Put_Line ("{");

      Put_Line (Indent & "declare");
      if Name_Of_List /= "" then
         Put_Line (Indent & "   New_Node : constant " & Node_Ptr & " :=");
         Put_Line (Indent & "     " & Node_Ptr & " ($1);");
         Index := Index + 1;
      else
         Put_Line (Indent & "   New_Node : constant " & Node_Ptr & " :=");
         Put_Line (Indent & "     " & New_Node (Node) & ";");
      end if;

      if Create /= "" then
         Put_Line (Indent & "   Crt_Node : constant " & Crt_Ptr & " :=");
         Put_Line (Indent & "     " & New_Node (Create) & ";");
      end if;

      Put_Line (Indent & "begin");
      Step_Indent (+1);
      Put_Line (Indent & "$$.Index := New_Node;");

      if Create /= "" then
         Put_Line (Indent & "Add (C, New_Node.all, Crt_Node);");
      end if;

      for I in 1 .. Count (Item) loop
         declare
            Child : Gramar_Items.Item'Class renames Get_Item (Item, I).all;
            Print : Boolean := True;
         begin
            if (Child in List or Child in Option) then
               if not Include (Processed_Positions, I) then
                  Print := False;
               end if;
            elsif (Child in Delimiter or Child in Keyword)
              and then not Just_Keywords (Item)
            then
               Index := Index + 1;
               Print := False;
            end if;
            if Print then
               if Proc /= "" then
                  Put_Line (Indent & Proc & "New_Node.all, $"
                            & To_String (Index) & ");");
               end if;
               Index := Index + 1;
            end  if;
         end;
      end loop;

      Step_Indent (-1);
      Put_Line (Indent & "end;");
      Put_Line ("}");

   end Print_List_Code;

   procedure Print_Infix_Code
     (Seq                 : in Sequence;
      Rule_Name           : in String;
      Processed_Positions : in Positions_List;
      Name_Of_List        : in String)
   is
      First : Item'Class renames Get_Item (Seq, 1).all;
   begin
      if Count (Seq) = 5 then
         Print_Infix_Code5 (Seq, Rule_Name, Processed_Positions, Name_Of_List);
      elsif Count (Seq) = 4 then
         Print_Infix_Code1 (Seq, Rule_Name, Processed_Positions, Name_Of_List);
      elsif First in Option then
         Print_Infix_Code2 (Processed_Positions);
      elsif Count (Seq) = 2 and (First in Keyword or First in Delimiter) then
         Print_Infix_Code3 (Seq, Name_Of_List);
      elsif First in Keyword then
         Print_Infix_Code6 (Seq, Name_Of_List);
      else
         declare
            Second : Item'Class renames Get_Item (Seq, 2).all;
         begin
            if Second in Reference then
               Print_Infix_Code7 (Seq, Name_Of_List);
            else
               Print_Infix_Code4 (Processed_Positions);
            end if;
         end;
      end if;
   end Print_Infix_Code;

   --  ref keyword ref list

   procedure Print_Infix_Code1
     (Seq                 : in Sequence;
      Rule_Name           : in String;
      Processed_Positions : in Positions_List;
      Name_Of_List        : in String)
   is
      Child : Item'Class renames Get_Item (Seq, 2).all;
   begin
      if Include (Processed_Positions, 4) then
         Put_Line ("{");
         Put_Line (Indent & "declare");
         Put_Line (Indent & "   Call_Node1 : constant Element_Index :=");
         Put_Line (Indent & "     New_Element (C, A_Function_Call);");
         Put_Line (Indent & "   Arg_List   : constant Element_Index :=");
         Put_Line (Indent & "     New_Element (C, Primary_List);");
         Put_Line (Indent & "   Arg_Node1  : constant Element_Index :=");
         Put_Line (Indent & "     New_Element (C, A_Parameter_Association);");
         Put_Line (Indent & "   Arg_Node2  : constant Element_Index :=");
         Put_Line (Indent & "     New_Element (C, A_Parameter_Association);");
         Put_Line (Indent & "   Call_Node2 : constant Element_Index :=");
         Put_Line (Indent & "     $4.Index;");
         Put_Line (Indent & "   Sym_Node   : constant Element_Index :=");
         Put_Line (Indent & "     New_Element (C, An_Operator_Symbol);");
         Put_Line (Indent & "begin");
         Step_Indent (+1);
         Put_Line (Indent & "Set (C, Arg_Node1, Actual_Parameter, $1.Index);");
         Put_Line (Indent & "Set_Start_Position (Arg_Node1, $1);");
         Put_Line (Indent & "Set_End_Position (Arg_Node1, $1);");
         Put_Line (Indent & "Set (C, Arg_Node2, Actual_Parameter, $3.Index);");
         Put_Line (Indent & "Set_Start_Position (Arg_Node2, $3);");
         Put_Line (Indent & "Set_End_Position (Arg_Node2, $3);");
         Put_Line (Indent & "Primary_Association_Lists.Add (C, Arg_List, Arg_Node1);");
         Put_Line (Indent & "Primary_Association_Lists.Add (C, Arg_List, Arg_Node2);");
         Put_Line (Indent & "Set (C, Call_Node1, Function_Call_Parameters, Arg_List);");
         Put_Line (Indent & "Set_Start_Position (Call_Node1, $1);");
         Put_Line (Indent & "Set_End_Position (Call_Node1, $3);");
         Set_Name_Image_Code ("Sym_Node", Child, 2);
         Put_Line (Indent & "Set_Start_Position (Sym_Node, $2);");
         Put_Line (Indent & "Set_End_Position (Sym_Node, $2);");
         Put_Line (Indent & "Set (C, Call_Node1, Prefix, Sym_Node);");
         Put_Line (Indent & "Set (C, Call_Node1, Is_Prefix_Call, 0);");
         Put_Line (Indent & "Push_Argument (C, Call_Node2, Call_Node1);");
         Put_Line (Indent & "Set_Start_Position (Call_Node2, $1);");
         Put_Line (Indent & "$$.Index := Call_Node2;");
         Step_Indent (-1);
         Put_Line (Indent & "end;");
         Put_Line ("}");
      else
         Put_Line ("{");
         Put_Line (Indent & "declare");
         Put_Line (Indent & "   Call_Node1 : constant Element_Index :=");
         Put_Line (Indent & "     New_Element (C, A_Function_Call);");
         Put_Line (Indent & "   Arg_List   : constant Element_Index :=");
         Put_Line (Indent & "     New_Element (C, Primary_List);");
         Put_Line (Indent & "   Arg_Node1  : constant Element_Index :=");
         Put_Line (Indent & "     New_Element (C, A_Parameter_Association);");
         Put_Line (Indent & "   Arg_Node2  : constant Element_Index :=");
         Put_Line (Indent & "     New_Element (C, A_Parameter_Association);");
         Put_Line (Indent & "   Sym_Node   : constant Element_Index :=");
         Put_Line (Indent & "     New_Element (C, An_Operator_Symbol);");
         Put_Line (Indent & "begin");
         Step_Indent (+1);
         Put_Line (Indent & "Set (C, Arg_Node1, Actual_Parameter, $1.Index);");
         Put_Line (Indent & "Set_Start_Position (Arg_Node1, $1);");
         Put_Line (Indent & "Set_End_Position (Arg_Node1, $1);");
         Put_Line (Indent & "Set (C, Arg_Node2, Actual_Parameter, $3.Index);");
         Put_Line (Indent & "Set_Start_Position (Arg_Node2, $3);");
         Put_Line (Indent & "Set_End_Position (Arg_Node2, $3);");
         Put_Line (Indent & "Primary_Association_Lists.Add (C, Arg_List, Arg_Node1);");
         Put_Line (Indent & "Primary_Association_Lists.Add (C, Arg_List, Arg_Node2);");
         Put_Line (Indent & "Set (C, Call_Node1, Function_Call_Parameters, Arg_List);");
         Put_Line (Indent & "Set_Start_Position (Call_Node1, $1);");
         Put_Line (Indent & "Set_End_Position (Call_Node1, $3);");
         Put_Line (Indent & "Set (C, Call_Node1, Prefix, Sym_Node);");
         Set_Name_Image_Code ("Sym_Node", Child, 2);
         Put_Line (Indent & "Set_Start_Position (Sym_Node, $2);");
         Put_Line (Indent & "Set_End_Position (Sym_Node, $2);");
         Put_Line (Indent & "Set (C, Call_Node1, Is_Prefix_Call, 0);");
         Put_Line (Indent & "$$.Index := Call_Node1;");
         Step_Indent (-1);
         Put_Line (Indent & "end;");
         Put_Line ("}");
      end if;
   end Print_Infix_Code1;

   --  opt(unar_op) ref list

   procedure Print_Infix_Code2
     (Processed_Positions : in Positions_List)
   is
   begin
      if not Include (Processed_Positions, 1) then
         Print_Infix_Code4 (Processed_Positions, 3);
         return;
      end if;
      if Include (Processed_Positions, 3) then
         Put_Line ("{");
         Put_Line (Indent & "declare");
         Put_Line (Indent & "   Call_Node1 : constant Element_Index :=");
         Put_Line (Indent & "     New_Element (C, A_Function_Call);");
         Put_Line (Indent & "   Arg_List   : constant Element_Index :=");
         Put_Line (Indent & "     New_Element (C, Primary_List);");
         Put_Line (Indent & "   Arg_Node1  : constant Element_Index :=");
         Put_Line (Indent & "     New_Element (C, A_Parameter_Association);");
         Put_Line (Indent & "   Call_Node2 : constant Element_Index :=");
         Put_Line (Indent & "     $3.Index;");
         Put_Line (Indent & "begin");
         Put_Line (Indent & "   Set (C, Arg_Node1, Actual_Parameter, $2.Index);");
         Put_Line (Indent & "   Set_Start_Position (Arg_Node1, $2);");
         Put_Line (Indent & "   Set_End_Position (Arg_Node1, $2);");
         Put_Line (Indent & "   Primary_Association_Lists.Add (C, Arg_List, Arg_Node1);");
         Put_Line (Indent & "   Set (C, Call_Node1, Function_Call_Parameters, Arg_List);");
         Put_Line (Indent & "   Set (C, Call_Node1, Prefix, $1.Index);");
         Put_Line (Indent & "   Set_Start_Position (Call_Node1, $1);");
         Put_Line (Indent & "   Set_End_Position (Call_Node1, $2);");
         Put_Line (Indent & "   Set (C, Call_Node1, Is_Prefix_Call, 0);");
         Put_Line (Indent & "   Push_Argument (C, Call_Node2, Call_Node1);");
         Put_Line (Indent & "   Set_Start_Position (Call_Node2, $1);");
         Put_Line (Indent & "   $$.Index := Call_Node2;");
         Put_Line (Indent & "end;");
         Put_Line ("}");
      else
         Put_Line ("{");
         Put_Line (Indent & "declare");
         Put_Line (Indent & "   Call_Node : constant Element_Index :=");
         Put_Line (Indent & "     New_Element (C, A_Function_Call);");
         Put_Line (Indent & "   Arg_List  : constant Element_Index :=");
         Put_Line (Indent & "     New_Element (C, Primary_List);");
         Put_Line (Indent & "   Arg_Node  : constant Element_Index :=");
         Put_Line (Indent & "     New_Element (C, A_Parameter_Association);");
         Put_Line (Indent & "begin");
         Put_Line (Indent & "   Set (C, Arg_Node, Actual_Parameter, $2.Index);");
         Put_Line (Indent & "   Set_Start_Position (Arg_Node, $2);");
         Put_Line (Indent & "   Set_End_Position (Arg_Node, $2);");
         Put_Line (Indent & "   Primary_Association_Lists.Add (C, Arg_List, Arg_Node);");
         Put_Line (Indent & "   Set (C, Call_Node, Function_Call_Parameters, Arg_List);");
         Put_Line (Indent & "   Set (C, Call_Node, Prefix, $1.Index);");
         Put_Line (Indent & "   Set_Start_Position (Call_Node, $1);");
         Put_Line (Indent & "   Set_End_Position (Call_Node, $2);");
         Put_Line (Indent & "   Set (C, Call_Node, Is_Prefix_Call, 0);");
         Put_Line (Indent & "   $$.Index := Call_Node;");
         Put_Line (Indent & "end;");
         Put_Line ("}");
      end if;
   end Print_Infix_Code2;

   --  keyword ref
   --  delimiter ref

   procedure Print_Infix_Code3
     (Seq                 : in Sequence;
      Name_Of_List        : in String)
   is
      First : Item'Class renames Get_Item (Seq, 1).all;
      Name  : constant String := Item_Name (First);
   begin
      if Name_Of_List = "" then
         Put_Line ("{");
         Put_Line (Indent & "declare");
         Put_Line (Indent & "   Call_Node  : constant Element_Index :=");
         Put_Line (Indent & "     New_Element (C, A_Function_Call);");
         Put_Line (Indent & "   Arg_List   : constant Element_Index :=");
         Put_Line (Indent & "     New_Element (C, Primary_List);");
         Put_Line (Indent & "   Arg_Node   : constant Element_Index :=");
         Put_Line (Indent & "     New_Element (C, A_Parameter_Association);");
         Put_Line (Indent & "   Sym_Node   : constant Element_Index :=");
         Put_Line (Indent & "     New_Element (C, An_Operator_Symbol);");
         if Name = "and" or Name = "or" or Name = "xor" or Name = "double_star" then
            Put_Line (Indent & "   Arg_2_Node : constant Element_Index :=");
            Put_Line (Indent & "     New_Element (C, A_Parameter_Association);");
            Put_Line (Indent & "begin");
            Step_Indent (+1);
            -- make placeholder for first argument
            Put_Line (Indent & "Primary_Association_Lists.Add (C, Arg_List, Arg_2_Node);");
         else
            Put_Line (Indent & "begin");
            Step_Indent (+1);
         end if;
         Put_Line (Indent & "Set (C, Arg_Node, Actual_Parameter, $2.Index);");
         Put_Line (Indent & "Set_Start_Position (Arg_Node, $2);");
         Put_Line (Indent & "Set_End_Position (Arg_Node, $2);");
         Put_Line (Indent & "Primary_Association_Lists.Add (C, Arg_List, Arg_Node);");
         Put_Line (Indent & "Set (C, Call_Node, Function_Call_Parameters, Arg_List);");
         Put_Line (Indent & "Set_Start_Position (Arg_Node, $1);");
         Put_Line (Indent & "Set_End_Position (Arg_Node, $2);");
         Put_Line (Indent & "Set (C, Call_Node, Prefix, Sym_Node);");
         Set_Name_Image_Code ("Sym_Node", First);
         Put_Line (Indent & "Set_Start_Position (Sym_Node, $1);");
         Put_Line (Indent & "Set_End_Position (Sym_Node, $1);");
         Put_Line (Indent & "Set_Start_Position (Call_Node, $1);");
         Put_Line (Indent & "Set_End_Position (Call_Node, $2);");
         Put_Line (Indent & "Set (C, Call_Node, Is_Prefix_Call, 0);");
         Put_Line (Indent & "$$.Index := Call_Node;");
         Step_Indent (-1);
         Put_Line (Indent & "end;");
         Put_Line ("}");
      else
         Put_Line ("{");
         Put_Line (Indent & "declare");
         Put_Line (Indent & "   Arg_Node1  : constant Element_Index :=");
         Put_Line (Indent & "     New_Element (C, A_Parameter_Association);");
         Put_Line (Indent & "   Arg_Node2  : constant Element_Index :=");
         Put_Line (Indent & "     New_Element (C, A_Parameter_Association);");
         Put_Line (Indent & "   Arg_List   : constant Element_Index :=");
         Put_Line (Indent & "     New_Element (C, Primary_List);");
         Put_Line (Indent & "   Call_Node : constant Element_Index :=");
         Put_Line (Indent & "     New_Element (C, A_Function_Call);");
         Put_Line (Indent & "   Sym_Node   : constant Element_Index :=");
         Put_Line (Indent & "     New_Element (C, An_Operator_Symbol);");
         Put_Line (Indent & "begin");
         Step_Indent (+1);
         Put_Line (Indent & "Set (C, Arg_Node1, Actual_Parameter, $1.Index);");
         Put_Line (Indent & "Set_Start_Position (Arg_Node1, $1);");
         Put_Line (Indent & "Set_End_Position (Arg_Node1, $1);");
         Put_Line (Indent & "Set (C, Arg_Node2, Actual_Parameter, $3.Index);");
         Put_Line (Indent & "Set_Start_Position (Arg_Node2, $3);");
         Put_Line (Indent & "Set_End_Position (Arg_Node2, $3);");
         Put_Line (Indent & "Primary_Association_Lists.Add (C, Arg_List, Arg_Node1);");
         Put_Line (Indent & "Primary_Association_Lists.Add (C, Arg_List, Arg_Node2);");
         Put_Line (Indent & "Set (C, Call_Node, Function_Call_Parameters, Arg_List);");
         Put_Line (Indent & "Set_Start_Position (Call_Node, $1);");
         Put_Line (Indent & "Set_End_Position (Call_Node, $3);");
         Set_Name_Image_Code ("Sym_Node", First, 2);
         Put_Line (Indent & "Set_Start_Position (Sym_Node, $2);");
         Put_Line (Indent & "Set_End_Position (Sym_Node, $2);");
         Put_Line (Indent & "Set (C, Call_Node, Prefix, Sym_Node);");
         Put_Line (Indent & "Set (C, Call_Node, Is_Prefix_Call, 0);");
         Put_Line (Indent & "$$.Index := Call_Node;");
         Step_Indent (-1);
         Put_Line (Indent & "end;");
         Put_Line ("}");
      end if;
   end Print_Infix_Code3;

   --  ref (list|option)

   procedure Print_Infix_Code4
     (Processed_Positions : in Positions_List;
      List_Index          : in Positive := 2)
   is
   begin
      if not Include (Processed_Positions, List_Index) then
         return;
      end if;
      Put_Line ("{");
      Put_Line (Indent & "declare");
      Put_Line (Indent & "   Call_Node : constant Element_Index :=");
      Put_Line (Indent & "     $2.Index;");
      Put_Line (Indent & "begin");
      Put_Line (Indent & "   Push_Argument (C, Call_Node, $1.Index);");
      Put_Line (Indent & "   Set_Start_Position (Call_Node, $1);");
      Put_Line (Indent & "   $$.Index := Call_Node;");
      Put_Line (Indent & "end;");
      Put_Line ("}");
   end Print_Infix_Code4;

   --  ref keyword keyword ref list

   procedure Print_Infix_Code5
     (Seq                 : in Sequence;
      Rule_Name           : in String;
      Processed_Positions : in Positions_List;
      Name_Of_List        : in String)
   is
      Node     : constant String := Node_Name (Seq);
   begin
      if Include (Processed_Positions, 5) then
         Put_Line ("{");
         Put_Line (Indent & "declare");
         Put_Line (Indent & "   Node1 : constant Element_Index :=");
         Put_Line (Indent & "     " & New_Node (Node) & ";");
         Put_Line (Indent & "   Node2 : constant Element_Index :=");
         Put_Line (Indent & "     $5.Index;");
         Put_Line (Indent & "begin");
         Put_Line (Indent & "   Set (C, Node1, Short_Circuit_Operation_Left_Expression, $1.Index);");
         Put_Line (Indent & "   Set_Start_Position (Node1, $1);");
         Put_Line (Indent & "   Set_End_Position (Node1, $4);");
         Put_Line (Indent & "   Set (C, Node1, Short_Circuit_Operation_Right_Expression, $4.Index);");
         Put_Line (Indent & "   Push_Argument (C, Node2, Node1);");
         Put_Line (Indent & "   Set_Start_Position (Node2, $1);");
         Put_Line (Indent & "   $$.Index := Node2;");
         Put_Line (Indent & "end;");
         Put_Line ("}");
      else
         Put_Line ("{");
         Put_Line (Indent & "declare");
         Put_Line (Indent & "   Node1 : constant Element_Index :=");
         Put_Line (Indent & "     " & New_Node (Node) & ";");
         Put_Line (Indent & "begin");
         Put_Line (Indent & "   Set (C, Node1, Short_Circuit_Operation_Left_Expression, $1.Index);");
         Put_Line (Indent & "   Set (C, Node1, Short_Circuit_Operation_Right_Expression, $4.Index);");
         Put_Line (Indent & "   Set_Start_Position (Node1, $1);");
         Put_Line (Indent & "   Set_End_Position (Node1, $4);");
         Put_Line (Indent & "   $$.Index := Node1;");
         Put_Line (Indent & "end;");
         Put_Line ("}");
      end if;
   end Print_Infix_Code5;

   -- list : keyword keyword ref

   procedure Print_Infix_Code6
     (Seq                 : in Sequence;
      Name_Of_List        : in String)
   is
      Node     : constant String := Node_Name (Seq);
   begin
      if Name_Of_List /= "" then
         Put_Line ("{");
         Put_Line (Indent & "declare");
         Put_Line (Indent & "   Node1 : constant Element_Index :=");
         Put_Line (Indent & "     " & New_Node (Node) & ";");
         Put_Line (Indent & "begin");
         Put_Line (Indent & "   Set (C, Node1, Short_Circuit_Operation_Left_Expression, $1.Index);");
         Put_Line (Indent & "   Set (C, Node1, Short_Circuit_Operation_Right_Expression, $4.Index);");
         Put_Line (Indent & "   Set_Start_Position (Node1, $1);");
         Put_Line (Indent & "   Set_End_Position (Node1, $4);");
         Put_Line (Indent & "   $$.Index := Node1;");
         Put_Line (Indent & "end;");
         Put_Line ("}");
      else
         Put_Line ("{");
         Put_Line (Indent & "declare");
         Put_Line (Indent & "   Node1 : constant Element_Index :=");
         Put_Line (Indent & "     " & New_Node (Node) & ";");
         Put_Line (Indent & "begin");
         Put_Line (Indent & "   Set (C, Node1, Short_Circuit_Operation_Right_Expression, $3.Index);");
         Put_Line (Indent & "   Set_Start_Position (Node1, $1);");
         Put_Line (Indent & "   Set_End_Position (Node1, $3);");
         Put_Line (Indent & "   $$.Index := Node1;");
         Put_Line (Indent & "end;");
         Put_Line ("}");
      end if;
   end Print_Infix_Code6;

   -- list: ref (oper) ref (term)

   procedure Print_Infix_Code7
     (Seq                 : in Sequence;
      Name_Of_List        : in String) is
   begin
      if Name_Of_List /= "" then
         Put_Line ("{");
         Put_Line (Indent & "declare");
         Put_Line (Indent & "   Arg_Node1  : constant Element_Index :=");
         Put_Line (Indent & "     New_Element (C, A_Parameter_Association);");
         Put_Line (Indent & "   Arg_Node2  : constant Element_Index :=");
         Put_Line (Indent & "     New_Element (C, A_Parameter_Association);");
         Put_Line (Indent & "   Arg_List   : constant Element_Index :=");
         Put_Line (Indent & "     New_Element (C, Primary_List);");
         Put_Line (Indent & "   Call_Node : constant Element_Index :=");
         Put_Line (Indent & "     New_Element (C, A_Function_Call);");
         Put_Line (Indent & "begin");
         Put_Line (Indent & "   Set (C, Arg_Node1, Actual_Parameter, $1.Index);");
         Put_Line (Indent & "   Set_Start_Position (Arg_Node1, $1);");
         Put_Line (Indent & "   Set_End_Position (Arg_Node1, $1);");
         Put_Line (Indent & "   Set (C, Arg_Node2, Actual_Parameter, $3.Index);");
         Put_Line (Indent & "   Set_Start_Position (Arg_Node2, $3);");
         Put_Line (Indent & "   Set_End_Position (Arg_Node2, $3);");
         Put_Line (Indent & "   Primary_Association_Lists.Add (C, Arg_List, Arg_Node1);");
         Put_Line (Indent & "   Primary_Association_Lists.Add (C, Arg_List, Arg_Node2);");
         Put_Line (Indent & "   Set (C, Call_Node, Function_Call_Parameters, Arg_List);");
         Put_Line (Indent & "   Set_Start_Position (Call_Node, $1);");
         Put_Line (Indent & "   Set_End_Position (Call_Node, $3);");
         Put_Line (Indent & "   Set (C, Call_Node, Prefix, $2.Index);");
         Put_Line (Indent & "   Set (C, Call_Node, Is_Prefix_Call, 0);");
         Put_Line (Indent & "   $$.Index := Call_Node;");
         Put_Line (Indent & "end;");
         Put_Line ("}");
      else
         Put_Line ("{");
         Put_Line (Indent & "declare");
         Put_Line (Indent & "   Arg_Node1  : constant Element_Index :=");
         Put_Line (Indent & "     New_Element (C, A_Parameter_Association);");
         Put_Line (Indent & "   Arg_Node2  : constant Element_Index :=");
         Put_Line (Indent & "     New_Element (C, A_Parameter_Association);");
         Put_Line (Indent & "   Arg_List   : constant Element_Index :=");
         Put_Line (Indent & "     New_Element (C, Primary_List);");
         Put_Line (Indent & "   Call_Node : constant Element_Index :=");
         Put_Line (Indent & "     New_Element (C, A_Function_Call);");
         Put_Line (Indent & "begin");
         Put_Line (Indent & "   Set (C, Arg_Node2, Actual_Parameter, $2.Index);");
         Put_Line (Indent & "   Set_Start_Position (Arg_Node2, $2);");
         Put_Line (Indent & "   Set_End_Position (Arg_Node2, $2);");
         Put_Line (Indent & "   Primary_Association_Lists.Add (C, Arg_List, Arg_Node1);");
         Put_Line (Indent & "   Primary_Association_Lists.Add (C, Arg_List, Arg_Node2);");
         Put_Line (Indent & "   Set (C, Call_Node, Function_Call_Parameters, Arg_List);");
         Put_Line (Indent & "   Set_Start_Position (Call_Node, $1);");
         Put_Line (Indent & "   Set_End_Position (Call_Node, $2);");
         Put_Line (Indent & "   Set (C, Call_Node, Prefix, $1.Index);");
         Put_Line (Indent & "   Set (C, Call_Node, Is_Prefix_Call, 0);");
         Put_Line (Indent & "   $$.Index := Call_Node;");
         Put_Line (Indent & "end;");
         Put_Line ("}");
      end if;
   end Print_Infix_Code7;

   procedure Create_Alt_Nodes
     (Seq                 : in Sequence;
      Processed_Positions : in Positions_List)
   is
      Index   : constant Natural := Choise_Item_Index (Seq);
      R_Ind   : constant Natural :=
        Real_Index (Seq, Processed_Positions, Index, "");
      Ch_Name : constant String := Choise (Get_Item (Seq, Index).all);
   begin

      Put_Line ("{");
      Put_Line (Indent & "if Global_Kind (C, $" & To_String (R_Ind) &
                ".Index) " & Ch_Name & " then");
      Step_Indent (+1);
      Create_Node_Code (Seq, Processed_Positions,
                        "", True_Node (Seq));
      Step_Indent (-1);
      Put_Line (Indent & "else");
      Step_Indent (+1);
      Create_Node_Code (Seq, Processed_Positions,
                        "", False_Node (Seq));
      Step_Indent (-1);
      Put_Line (Indent & "end if;");
      Put_Line ("}");
   end Create_Alt_Nodes;

   function Processed
     (Seq                 : in Sequence;
      Processed_Positions : in Positions_List;
      Index               : Positive) return Boolean
   is
      Child : Item'Class renames Get_Item (Seq, Index).all;
   begin
      if (Child in Option or Child in List) and then
         not Include (Processed_Positions, Index) then

         return False;

      else
         return True;
      end if;
   end Processed;

   function Real_Index
     (Seq                 : in Sequence;
      Processed_Positions : in Positions_List;
      Index               : in Positive;
      Name_Of_List        : in String) return Positive
   is
      Result : Positive := Index;
   begin
      if Name_Of_List /= "" then
         Result := Result + 1;
      end if;
      for I in 1 .. Index - 1 loop
         if not Processed (Seq, Processed_Positions, I) then
            Result := Result - 1;
         end if;
      end loop;
      return Result;
   end Real_Index;

   procedure Print_Wrap_Code
     (Child : Wrapper;
      Last  : Boolean)
   is
      Child_Name  : constant String  := Object_Name (Child);
      Child_Node  : constant String  := Node_Name (Child);
      Child_Pos   : constant String  := Position (Child);
      Wrap        : constant Wrapper := Parent (Child);
      Attr_Name   : constant String  := User_Attr_Name (Child);
      Parent_Node : constant String  := Node_Name (Wrap);
      Parent_Name : constant String  := Object_Name (Wrap);
      Attr        : constant String :=
        Find_Attribute (Child_Name, Child_Node, Attr_Name, Parent_Node);
      Proc : constant String := Find_Procedure (Parent_Node, Attr);
   begin
      if Child_Pos = "start" or Child_Pos = "both" then
         Put_Line (Indent & "Set_Start_Position (" & Parent_Name &
                   ", " & Child_Name & ");");
      end if;
      if Attr /= "" then
         Put_Line (Indent & "Set (C, " & Parent_Name & ", Properties." &
                   Attr &", " & Child_Name & ");");
      elsif Child_Node /= "" then
         Put_Line (Indent & Proc & Parent_Name & ", " &
                   Child_Name & ");");
      end if;
      if Last and (Child_Pos = "end" or Child_Pos = "both") then
         Put_Line (Indent & "Set_End_Position (" & Parent_Name &
                   ", " & Child_Name & ");");
      end if;
   end Print_Wrap_Code;

   function Find_Procedure
     (Node_Name : String;
      Attr_Name : String) return String
   is
   begin
      if Is_List_Type (Node_Name) then
         return Node_Name (1 .. Node_Name'Last - 4) & "Add (C, ";
      end if;
      return "####";
   end Find_Procedure;

   function New_Node (Type_Name : String) return String is
   begin
      if Type_Name = "Any_Compilation_Unit_Node" then
         return
           "Any_Compilation_Unit_Ptr (New_Compilation_Unit (The_Context))";
      elsif Is_List_Type (Type_Name) then
         return "New_Element (C, Primary_List)";
      else
         return "New_Element (C, " & Type_Name & ")";
      end if;
   end New_Node;

   function Is_List_Type (Node_Name : String) return Boolean is
   begin
      return Node_Name'Length > 4 and then
        Node_Name (Node_Name'Last - 4 .. Node_Name'Last) = ".List";
   end Is_List_Type;

   function Quote (Text : String; Double : Boolean) return String is
   begin
      if Double then
         return """""""" & Text & """""""";
      else
         return """" & Text & """";
      end if;
   end Quote;

   function To_Operator_Kind (Name : String) return String is
   begin
      if Name = "and" then
         return "An_And_Operator";
      elsif Name = "or" then
         return "An_Or_Operator";
      elsif Name = "xor" then
         return "An_Xor_Operator";
      elsif Name = "=" then
         return "An_Equal_Operator";
      elsif Name = "/=" then
         return "A_Not_Equal_Operator";
      elsif Name = "<" then
         return "A_Less_Than_Operator";
      elsif Name = "<=" then
         return "A_Less_Than_Or_Equal_Operator";
      elsif Name = ">" then
         return "A_Greater_Than_Operator";
      elsif Name = ">=" then
         return "A_Greater_Than_Or_Equal_Operator";
      elsif Name = "+" then
         return "A_Plus_Operator";
      elsif Name = "-" then
         return "A_Minus_Operator";
      elsif Name = "&" then
         return "A_Concatenate_Operator";
      elsif Name = "*" then
         return "A_Multiply_Operator";
      elsif Name = "/" then
         return "A_Divide_Operator";
      elsif Name = "mod" then
         return "A_Mod_Operator";
      elsif Name = "rem" then
         return "A_Rem_Operator";
      elsif Name = "**" then
         return "An_Exponentiate_Operator";
      elsif Name = "abs" then
         return "An_Abs_Operator";
      elsif Name = "not" then
         return "A_Not_Operator";
      else
         Put_Line ("#### wrong operator:" & Name);
         return "";
      end if;
   end To_Operator_Kind;

   procedure Set_Name_Image_Code
     (Name     : in     String;
      Child    : in     Item'Class;
      Index    : in     Positive := 1;
      Proc     : in     String := "Name_Image";
      In_Quote : in     Boolean  := True)
   is
   begin
      if Child in Delimiter then
         Put_Line (Indent & "Set_Image (" & Name & ", " & Proc &
                   ", " & To_Operator_Kind
                   (Text (Delimiter (Child))) & ");");
      elsif Child in Reference then
         declare
            Node : constant String := Node_Name (Child);
            Attr : constant String := Nodes.Find_Attr_By_Type
              (Node, "Source_String");
         begin
            Put_Line (Indent & "Set_Image (" & Name & ", " & Proc &
                      ", $" & To_String (Index) & ", " & Attr & ");");
         end;
      elsif In_Quote then
         Put_Line (Indent & "Set_Image (" & Name & ", " & Proc &
                   ", " & To_Operator_Kind (Item_Name (Child)) & ");");
      else
         Put_Line (Indent & "Set_Image (" & Name & ", " & Proc &
                   ", $" & To_String (Index) & ", Name_Image);");
      end if;
   end Set_Name_Image_Code;

   function Is_First
     (Seq                 : in Sequence;
      Index               : in Positive;
      Processed_Positions : in Positions_List) return Boolean
   is
      Child : Item'Class renames Get_Item (Seq, Index).all;
      Wrap  : Wrapper := Parent (Child);
   begin
      for I in 1 .. Index - 1 loop
         if Processed (Seq, Processed_Positions, I) then
            declare
               Child_I : Item'Class renames Get_Item (Seq, I).all;
               Wrap_I  : Wrapper := Parent (Child_I);
            begin
               if Wrap_I = Wrap then
                  return False;
               end if;
            end;
         end if;
      end loop;
      return True;
   end Is_First;

   function Is_Last
     (Seq                 : in Sequence;
      Index               : in Positive;
      Processed_Positions : in Positions_List) return Boolean
   is
      Child : Item'Class renames Get_Item (Seq, Index).all;
      Wrap  : Wrapper := Parent (Child);
   begin
      for I in Index + 1 .. Count (Seq) loop
         if Processed (Seq, Processed_Positions, I) then
            declare
               Child_I : Item'Class renames Get_Item (Seq, I).all;
               Wrap_I  : Wrapper := Parent (Child_I);
            begin
               if Wrap_I = Wrap then
                  return False;
               end if;
            end;
         end if;
      end loop;
      return True;
   end Is_Last;

end Generate;



------------------------------------------------------------------------------
--  Copyright (c) 2006, Maxim Reznik
--  All rights reserved.
--
--  Redistribution and use in source and binary forms, with or without
--  modification, are permitted provided that the following conditions are met:
--
--     * Redistributions of source code must retain the above copyright notice,
--     * this list of conditions and the following disclaimer.
--     * Redistributions in binary form must reproduce the above copyright
--     * notice, this list of conditions and the following disclaimer in the
--     * documentation and/or other materials provided with the distribution.
--
--  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
--  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
--  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
--  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
--  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
--  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
--  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
--  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
--  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
--  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
--  POSSIBILITY OF SUCH DAMAGE.
------------------------------------------------------------------------------
