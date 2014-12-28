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

      case Op is
         when Gela.Semantic_Types.Concat =>
            declare
               Item : constant Gela.Plain_Value_Sets.Value :=
                 Self.Vector.Element (Args);
            begin
               if Item.Kind = List_Value and then
                 Self.Vector.Element (Item.Head).Kind = String_Value and then
                 Self.Vector.Element (Item.Tail).Kind = String_Value
               then
                  Self.String_Literal
                    (Self.Image (Item.Head) & Self.Image (Item.Tail),
                     Value);
               end if;
            end;
      end case;
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
            Symbol : constant Gela.Lexical_Types.Symbol := Node.Full_Name;
         begin
            case Symbol is
               when Gela.Lexical_Types.Operators.Ampersand_Operator =>
                  declare
                     Item  : constant Gela.Plain_Value_Sets.Value :=
                       (Denote_Function, Gela.Semantic_Types.Concat);
                  begin
                     Put_Value (Self  => Self,
                                Item  => Item,
                                Value => V.Result);
                  end;
               when others =>
                  null;
            end case;
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
      if Name /= null and then Name.Enclosing_Element = null then
         Name.Enclosing_Element.Visit (V);
      else
         --  FIXME stub until name resolution ready
         declare
            Item  : constant Gela.Plain_Value_Sets.Value :=
              (Denote_Function, Gela.Semantic_Types.Concat);
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
      return League.Strings.Universal_String is
   begin
      return Self.Vector.Element (Value).String;
   end Image;

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
