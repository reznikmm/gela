with League.Strings.Hash;

with Gela.Element_Visiters;
with Gela.Elements.Defining_Designators;
with Gela.Elements.Defining_Operator_Symbols;
with Gela.Elements.Function_Declarations;
with Gela.Lexical_Types;

package body Gela.Plain_Value_Sets is

   ------------
   -- Concat --
   ------------

   overriding procedure Apply
     (Self  : in out Value_Set;
      Name  : Gela.Semantic_Types.Value_Index;
      Args  : Gela.Semantic_Types.Value_Index;
      Value : out Gela.Semantic_Types.Value_Index)
   is
      use type League.Strings.Universal_String;
      use type Gela.Semantic_Types.Value_Index;
      Op : Gela.Semantic_Types.Static_Operator;
   begin
      Value := 0;

      if Args = 0 or Name = 0 then
         return;
      end if;

      declare
         Item : constant Gela.Plain_Value_Sets.Value :=
           Self.Vector.Element (Name);
      begin
         if Item.Kind = Denote_Function then
            Op := Item.Op;
         else
            return;
         end if;
      end;

      declare
         use type Gela.Arithmetic.Integers.Value;

         Item : constant Gela.Plain_Value_Sets.Value :=
           Self.Vector.Element (Args);
         Left  : Gela.Plain_Value_Sets.Value;
         Right : Gela.Plain_Value_Sets.Value;
      begin
         if Item.Kind /= List_Value then
            Self.String_Literal
              (League.Strings.To_Universal_String ("???"),
               Value);
            return;
         end if;

         Left := Self.Vector.Element (Item.Head);
         Right := Self.Vector.Element (Item.Tail);
         case Op is
            when Gela.Semantic_Types.Ampersand_Operator =>
               if Left.Kind = String_Value and then
                 Right.Kind = String_Value
               then
                  Self.String_Literal
                    (Left.String & Right.String,
                     Value);
               end if;
            when Gela.Semantic_Types.Hyphen_Operator =>
               if Left.Kind = Integer_Value and then
                 Right.Kind = Integer_Value
               then
                  Self.Put_Value
                    ((Integer_Value, Left.Integer - Right.Integer), Value);
               end if;
            when others =>
               raise Constraint_Error with "unimplemeneted";
         end case;
      end;
   end Apply;

   ----------
   -- List --
   ----------

   overriding procedure List
     (Self  : in out Value_Set;
      Head  : Gela.Semantic_Types.Value_Index;
      Tail  : Gela.Semantic_Types.Value_Index;
      Value : out Gela.Semantic_Types.Value_Index)
   is
      use type Gela.Semantic_Types.Value_Index;
   begin
      if Tail = 0 then
         Value := Head;
      elsif Head = 0 then
         Value := 0;
      else
         Self.Put_Value ((List_Value, Head, Tail), Value);
      end if;
   end List;

   ----------
   -- Name --
   ----------

   overriding procedure Name
     (Self  : in out Value_Set;
      Name  : Gela.Elements.Defining_Names.Defining_Name_Access;
      Value : out Gela.Semantic_Types.Value_Index)
   is

      package Get is
         type Visiter is new Gela.Element_Visiters.Visiter with record
            Result : Gela.Semantic_Types.Value_Index := 0;
         end record;

         overriding procedure Defining_Operator_Symbol
           (V    : in out Visiter;
            Node : not null Gela.Elements.Defining_Operator_Symbols.
              Defining_Operator_Symbol_Access);

         overriding procedure Function_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Function_Declarations.
              Function_Declaration_Access);

      end Get;

      package body Get is

         overriding procedure Defining_Operator_Symbol
           (V    : in out Visiter;
            Node : not null Gela.Elements.Defining_Operator_Symbols.
              Defining_Operator_Symbol_Access)
         is
            use type Gela.Lexical_Types.Symbol;

            Symbol : constant Gela.Lexical_Types.Symbol := Node.Full_Name;
            Op : constant Gela.Semantic_Types.Static_Operator :=
              Gela.Semantic_Types.Static_Operator'Val (Symbol - 1);
            Item  : constant Gela.Plain_Value_Sets.Value :=
              (Denote_Function, Op);
         begin
            Put_Value (Self  => Self,
                       Item  => Item,
                       Value => V.Result);
         end Defining_Operator_Symbol;

         overriding procedure Function_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Function_Declarations.
              Function_Declaration_Access)
         is
            Name : constant Gela.Elements.Defining_Designators.
              Defining_Designator_Access := Node.Names;
         begin
            Name.Visit (Self);
         end Function_Declaration;

      end Get;

      use type Gela.Elements.Element_Access;
      use type Gela.Elements.Defining_Names.Defining_Name_Access;

      V : aliased Get.Visiter;
   begin
      if Name /= null and then Name.Enclosing_Element /= null then
         Name.Enclosing_Element.Visit (V);
      else
         --  FIXME stub until name resolution ready
         declare
            Item  : constant Gela.Plain_Value_Sets.Value :=
              (Denote_Function, Gela.Semantic_Types.Ampersand_Operator);
         begin
            Put_Value (Self  => Self,
                       Item  => Item,
                       Value => V.Result);
         end;
      end if;

      Value := V.Result;
   end Name;

   ----------
   -- Hash --
   ----------

   function Hash (X : Value) return Ada.Containers.Hash_Type is
      use type Ada.Containers.Hash_Type;
   begin
      case X.Kind is
         when Denote_Function =>
            return Gela.Semantic_Types.Static_Operator'Pos (X.Op);
         when Integer_Value =>
            return Gela.Arithmetic.Integers.Hash (X.Integer);
         when String_Value =>
            return League.Strings.Hash (X.String);
         when List_Value =>
            return 65_213 * Ada.Containers.Hash_Type (X.Head) +
              Ada.Containers.Hash_Type (X.Tail);
      end case;
   end Hash;

   -----------
   -- Image --
   -----------

   overriding function Image
     (Self  : Value_Set;
      Value : Gela.Semantic_Types.Value_Index)
      return League.Strings.Universal_String
   is
      Item : constant Gela.Plain_Value_Sets.Value :=
        Self.Vector.Element (Value);
   begin
      case Item.Kind is
         when String_Value =>
            return Item.String;
         when Integer_Value =>
            return League.Strings.From_UTF_8_String
              (Gela.Arithmetic.Integers.Image (Item.Integer));
         when others =>
            raise Constraint_Error;
      end case;
   end Image;

   ---------------
   -- Is_String --
   ---------------

   overriding function Is_String
     (Self  : Value_Set;
      Value : Gela.Semantic_Types.Value_Index) return Boolean is
   begin
      return Self.Vector.Element (Value).Kind = String_Value;
   end Is_String;

   ---------------------
   -- Numeric_Literal --
   ---------------------

   overriding procedure Numeric_Literal
     (Self  : in out Value_Set;
      Image : League.Strings.Universal_String;
      Value : out Gela.Semantic_Types.Value_Index)
   is
      X : constant Gela.Arithmetic.Integers.Value :=
        Gela.Arithmetic.Integers.Literal (Image.To_UTF_8_String);
      Item : constant Gela.Plain_Value_Sets.Value := (Integer_Value, X);
   begin
      Self.Put_Value (Item, Value);
   end Numeric_Literal;

   ---------------
   -- Put_Value --
   ---------------

   not overriding procedure Put_Value
     (Self  : in out Value_Set;
      Item  : Value;
      Value : out Gela.Semantic_Types.Value_Index)
   is
      Pos  : constant Hash_Maps.Cursor := Self.Map.Find (Item);
   begin
      if Hash_Maps.Has_Element (Pos) then
         Value := Hash_Maps.Element (Pos);
      else
         Self.Vector.Append (Item);
         Value := Self.Vector.Last_Index;
         Self.Map.Insert (Item, Value);
      end if;
   end Put_Value;

   --------------------
   -- String_Literal --
   --------------------

   overriding procedure String_Literal
     (Self  : in out Value_Set;
      Image : League.Strings.Universal_String;
      Value : out Gela.Semantic_Types.Value_Index)
   is
      Item : constant Gela.Plain_Value_Sets.Value := (String_Value, Image);
   begin
      Self.Put_Value (Item, Value);
   end String_Literal;


end Gela.Plain_Value_Sets;
