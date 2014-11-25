with Gela.Nodes.Fixed_Operator_Symbols;
with Gela.Element_Visiters;
with Gela.Elements.Auxiliary_Applies;
with Gela.Elements.Associations;
with Gela.Elements.Record_Aggregates;
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
            Result : Gela.Elements.Auxiliary_Applies.Auxiliary_Apply_Access;
         end record;

         overriding procedure Auxiliary_Apply
           (Self : in out Visiter;
            Node : not null Gela.Elements.Auxiliary_Applies.
              Auxiliary_Apply_Access);
      end Get;

      package body Get is

         overriding procedure Auxiliary_Apply
           (Self : in out Visiter;
            Node : not null Gela.Elements.Auxiliary_Applies.
              Auxiliary_Apply_Access)
         is
         begin
            Self.Result := Node;
         end Auxiliary_Apply;
      end Get;

      use type Gela.Elements.Auxiliary_Applies.Auxiliary_Apply_Access;

      V    : Get.Visiter;
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
         RA   : constant Gela.Elements.Record_Aggregates.
           Record_Aggregate_Access :=
             Self.Record_Aggregate
               (Left_Token                    => 0,
                Record_Component_Associations => Args,
                Right_Token                   => 0);
         Call : constant Gela.Elements.Auxiliary_Applies.Auxiliary_Apply_Access
           := Self.Auxiliary_Apply
             (Prefix                   => Prefix,
              Function_Call_Parameters => RA);
         Name : constant Gela.Elements.Names.Name_Access :=
           Gela.Elements.Names.Name_Access (Call);
      begin
         return Parent. Procedure_Call_Statement (Name, Semicolon_Token);
      end;
   end Procedure_Call_Statement;

end Gela.Fix_Node_Factories;
