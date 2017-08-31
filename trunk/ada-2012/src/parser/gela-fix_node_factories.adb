with Gela.Nodes.Fixed_Operator_Symbols;
with Gela.Element_Visiters;
with Gela.Elements.Association_Lists;
with Gela.Elements.Associations;
with Gela.Elements.Function_Calls;
with Gela.Elements.Parenthesized_Expressions;
with Gela.Elements.Prefixes;

package body Gela.Fix_Node_Factories is

   ---------------------
   -- Operator_Symbol --
   ---------------------

   overriding function Operator_Symbol
     (Self : in out Element_Factory;
      Operator_Symbol_Token : Gela.Lexical_Types.Token_Count)
      return Gela.Elements.Operator_Symbols.Operator_Symbol_Access
   is
      Result : constant Gela.Nodes.Fixed_Operator_Symbols
        .Operator_Symbol_Access :=
           new Gela.Nodes.Fixed_Operator_Symbols.Operator_Symbol'
            (Gela.Nodes.Fixed_Operator_Symbols.Create
              (Self.Comp,
               Operator_Symbol_Token));
   begin
      Result.Set_Full_Name (Gela.Lexical_Types.No_Symbol);

      return Gela.Elements.Operator_Symbols.Operator_Symbol_Access (Result);
   end Operator_Symbol;

   overriding function Procedure_Call_Statement
     (Self : in out Element_Factory;
      Function_Call : Gela.Elements.Names.Name_Access;
      Semicolon_Token : Gela.Lexical_Types.Token_Count)
      return Gela.Elements.Procedure_Call_Statements.
               Procedure_Call_Statement_Access
   is
      package Get is
         type Visiter is new Gela.Element_Visiters.Visiter with record
            Result : Gela.Elements.Function_Calls.Function_Call_Access;
         end record;

         overriding procedure Function_Call
           (Self : in out Visiter;
            Node : not null Gela.Elements.Function_Calls.
              Function_Call_Access);
      end Get;

      package body Get is

         overriding procedure Function_Call
           (Self : in out Visiter;
            Node : not null Gela.Elements.Function_Calls.
              Function_Call_Access)
         is
         begin
            Self.Result := Node;
         end Function_Call;
      end Get;

      use type Gela.Elements.Function_Calls.Function_Call_Access;

      V      : Get.Visiter;
      Parent : Gela.Node_Factories.Element_Factory renames
        Gela.Node_Factories.Element_Factory (Self);
   begin
      Function_Call.Visit (V);

      if V.Result /= null then
         return Parent. Procedure_Call_Statement
           (Function_Call, Semicolon_Token);
      end if;

      declare
         Prefix : constant Gela.Elements.Prefixes.Prefix_Access :=
           Gela.Elements.Prefixes.Prefix_Access (Function_Call);
         Args : constant Gela.Elements.Associations.Association_Sequence_Access
           := Self.Association_Sequence;
         RA   : constant Gela.Elements.Association_Lists.
           Association_List_Access :=
             Self.Association_List
               (Left_Token                    => 0,
                Record_Component_Associations => Args,
                Right_Token                   => 0);
         Call : constant Gela.Elements.Function_Calls.Function_Call_Access
           := Self.Function_Call
             (Prefix                   => Prefix,
              Function_Call_Parameters => RA);
         Name : constant Gela.Elements.Names.Name_Access :=
           Gela.Elements.Names.Name_Access (Call);
      begin
         return Parent.Procedure_Call_Statement (Name, Semicolon_Token);
      end;
   end Procedure_Call_Statement;

   --------------------------
   -- Qualified_Expression --
   --------------------------

   overriding function Qualified_Expression
     (Self : in out Element_Factory;
      Converted_Or_Qualified_Subtype_Mark : Gela.Elements.Subtype_Marks.
        Subtype_Mark_Access;
      Apostrophe_Token : Gela.Lexical_Types.Token_Count;
      Left_Parenthesis_Token : Gela.Lexical_Types.Token_Count;
      Converted_Or_Qualified_Expression : Gela.Elements.Expressions.
        Expression_Access;
      Right_Parenthesis_Token : Gela.Lexical_Types.Token_Count)
      return Gela.Elements.Qualified_Expressions.Qualified_Expression_Access
   is
      pragma Unreferenced (Left_Parenthesis_Token);
      pragma Unreferenced (Right_Parenthesis_Token);

      package Get is
         type Visiter is new Gela.Element_Visiters.Visiter with record
            Result : Gela.Elements.Expressions.Expression_Access;
            Left_Parenthesis_Token : Gela.Lexical_Types.Token_Count := 0;
            Right_Parenthesis_Token : Gela.Lexical_Types.Token_Count := 0;
         end record;

         overriding procedure Parenthesized_Expression
           (Self : in out Visiter;
            Node : not null Gela.Elements.Parenthesized_Expressions.
              Parenthesized_Expression_Access);
      end Get;

      package body Get is

         overriding procedure Parenthesized_Expression
           (Self : in out Visiter;
            Node : not null Gela.Elements.Parenthesized_Expressions.
              Parenthesized_Expression_Access) is
         begin
            Self.Result := Node.Expression_Parenthesized;
         end Parenthesized_Expression;
      end Get;

      V      : Get.Visiter :=
        (Result => Converted_Or_Qualified_Expression, others => <>);
      Parent : Gela.Node_Factories.Element_Factory renames
        Gela.Node_Factories.Element_Factory (Self);
   begin
      Converted_Or_Qualified_Expression.Visit (V);

      return Parent.Qualified_Expression
        (Converted_Or_Qualified_Subtype_Mark,
         Apostrophe_Token,
         V.Left_Parenthesis_Token,
         V.Result,
         V.Right_Parenthesis_Token);
   end Qualified_Expression;

end Gela.Fix_Node_Factories;
