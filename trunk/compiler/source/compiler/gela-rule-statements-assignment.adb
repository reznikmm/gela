with Asis.Statements;

package body Gela.Rule.Statements.Assignment is

   ----------
   -- Code --
   ----------

   function Code
     (Engine   : access Gela.Engines.Engine;
      Element  : Asis.Element;
      Property : Gela.Properties.Property_Name)
      return Gela.Properties.Text.Text
   is
      pragma Unreferenced (Property);

      procedure Write_Length
        (Result : in out Gela.Properties.Text.Text;
         Value  : Gela.Properties.Text.Text;
         Output : out Positive);

      procedure Write_Load
        (Result : in out Gela.Properties.Text.Text;
         Value  : Gela.Properties.Text.Text;
         Index  : Natural;
         Tipe   : String;
         Output : out Positive);

      procedure Write_Check
        (Result : in out Gela.Properties.Text.Text;
         Left   : Positive;
         Right  : Positive);

      procedure Write_Move
        (Result : in out Gela.Properties.Text.Text;
         Left   : Positive;
         Right  : Positive;
         Length : Positive);

      ------------------
      -- Write_Length --
      ------------------

      procedure Write_Length
        (Result : in out Gela.Properties.Text.Text;
         Value  : Gela.Properties.Text.Text;
         Output : out Positive)
      is
         Lower_Value   : Positive;
         Upper_Value   : Positive;
         Diff_Value    : Positive;
      begin
         Write_Load (Result, Value, 1, "i32*", Lower_Value);
         Write_Load (Result, Value, 2, "i32*", Upper_Value);
         Diff_Value := Engine.Unique;

         Result := Engine.Text_Container.Join (Result, " %");
         Result := Engine.Text_Container.Join (Result, Diff_Value);
         Result := Engine.Text_Container.Join (Result, " = sub i32 %");
         Result := Engine.Text_Container.Join (Result, Upper_Value);
         Result := Engine.Text_Container.Join (Result, ", %");
         Result := Engine.Text_Container.Join (Result, Lower_Value);
         Result := Engine.Text_Container.Join
           (Result, Engine.Text_Container.New_Line);

         Output := Engine.Unique;

         Result := Engine.Text_Container.Join (Result, " %");
         Result := Engine.Text_Container.Join (Result, Output);
         Result := Engine.Text_Container.Join (Result, " = add i32 %");
         Result := Engine.Text_Container.Join (Result, Diff_Value);
         Result := Engine.Text_Container.Join (Result, ", 1");
         Result := Engine.Text_Container.Join
           (Result, Engine.Text_Container.New_Line);
      end Write_Length;

      ----------------
      -- Write_Load --
      ----------------

      procedure Write_Load
        (Result : in out Gela.Properties.Text.Text;
         Value  : Gela.Properties.Text.Text;
         Index  : Natural;
         Tipe   : String;
         Output : out Positive)
      is
         Item_Address : constant Positive := Engine.Unique;
         Item_Value   : constant Positive := Engine.Unique;
      begin
         Result := Engine.Text_Container.Join (Result, " %");
         Result := Engine.Text_Container.Join (Result, Item_Address);
         Result := Engine.Text_Container.Join
           (Result, " = getelementptr inbounds %_ada_string* ");
         Result := Engine.Text_Container.Join (Result, Value);
         Result := Engine.Text_Container.Join (Result, ", i32 0, i32 ");
         Result := Engine.Text_Container.Join (Result, Index);
         Result := Engine.Text_Container.Join
           (Result, Engine.Text_Container.New_Line);

         Result := Engine.Text_Container.Join (Result, " %");
         Result := Engine.Text_Container.Join (Result, Item_Value);
         Result := Engine.Text_Container.Join (Result, " = load ");
         Result := Engine.Text_Container.Join (Result, Tipe);
         Result := Engine.Text_Container.Join (Result, " %");
         Result := Engine.Text_Container.Join (Result, Item_Address);
         Result := Engine.Text_Container.Join
           (Result, Engine.Text_Container.New_Line);

         Output := Item_Value;
      end Write_Load;

      -----------------
      -- Write_Check --
      -----------------

      procedure Write_Check
        (Result : in out Gela.Properties.Text.Text;
         Left   : Positive;
         Right  : Positive)
      is
         Condition : constant Positive := Engine.Unique;
         Fail      : constant Positive := Engine.Unique;
         Success   : constant Positive := Engine.Unique;
      begin
         Result := Engine.Text_Container.Join (Result, " %");
         Result := Engine.Text_Container.Join (Result, Condition);
         Result := Engine.Text_Container.Join (Result, "= icmp ne i32 %");
         Result := Engine.Text_Container.Join (Result, Left);
         Result := Engine.Text_Container.Join (Result, ", %");
         Result := Engine.Text_Container.Join (Result, Right);
         Result := Engine.Text_Container.Join
           (Result, Engine.Text_Container.New_Line);

         Result := Engine.Text_Container.Join (Result, " br i1 %");
         Result := Engine.Text_Container.Join (Result, Condition);
         Result := Engine.Text_Container.Join (Result, ", label %");
         Result := Engine.Text_Container.Join (Result, Fail);
         Result := Engine.Text_Container.Join (Result, ", label %");
         Result := Engine.Text_Container.Join (Result, Success);
         Result := Engine.Text_Container.Join
           (Result, Engine.Text_Container.New_Line);

         Result := Engine.Text_Container.Join (Result, "; label ");
         Result := Engine.Text_Container.Join (Result, Fail);
         Result := Engine.Text_Container.Join (Result, ":");
         Result := Engine.Text_Container.Join
           (Result, Engine.Text_Container.New_Line);

         Result := Engine.Text_Container.Join
           (Result, " call void @llvm.trap() unreachable");
         Result := Engine.Text_Container.Join
           (Result, Engine.Text_Container.New_Line);

         Result := Engine.Text_Container.Join (Result, "; label ");
         Result := Engine.Text_Container.Join (Result, Success);
         Result := Engine.Text_Container.Join (Result, ":");
         Result := Engine.Text_Container.Join
           (Result, Engine.Text_Container.New_Line);
      end Write_Check;

      ----------------
      -- Write_Move --
      ----------------

      procedure Write_Move
        (Result : in out Gela.Properties.Text.Text;
         Left   : Positive;
         Right  : Positive;
         Length : Positive) is
      begin
         Result := Engine.Text_Container.Join
           (Result, " call void @llvm.memmove.p0i8.p0i8.i32 (i8* %");
         Result := Engine.Text_Container.Join (Result, Left);
         Result := Engine.Text_Container.Join (Result, ", i8* %");
         Result := Engine.Text_Container.Join (Result, Right);
         Result := Engine.Text_Container.Join (Result, ", i32 %");
         Result := Engine.Text_Container.Join (Result, Length);
         Result := Engine.Text_Container.Join (Result, ", i32 0, i1 0)");
         Result := Engine.Text_Container.Join
           (Result, Engine.Text_Container.New_Line);
      end Write_Move;

      Result : Gela.Properties.Text.Text;
      Right  : constant Asis.Expression :=
        Asis.Statements.Assignment_Expression (Element);
      Right_Text : Gela.Properties.Text.Text;
      Right_Length : Positive;
      Right_Data : Positive;
      Left  : constant Asis.Expression :=
        Asis.Statements.Assignment_Variable_Name (Element);
      Left_Text : Gela.Properties.Text.Text;
      Left_Length : Positive;
      Left_Data : Positive;
   begin
      Right_Text := Engine.Get (Right, Gela.Properties.Value);
      Left_Text := Engine.Get (Left, Gela.Properties.Value);
      Result := Engine.Text_Container.Literal ("");
      Write_Length (Result, Right_Text, Right_Length);
      Write_Length (Result, Left_Text, Left_Length);
      Write_Check (Result, Left_Length, Right_Length);
      Write_Load (Result, Left_Text, 0, "i8**", Left_Data);
      Write_Load (Result, Right_Text, 0, "i8**", Right_Data);
      Write_Move (Result, Left_Data, Right_Data, Left_Length);

      Result := Engine.Text_Container.Join
        (Result, Engine.Text_Container.New_Line);

      return Result;
   end Code;

end Gela.Rule.Statements.Assignment;
