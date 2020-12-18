--  SPDX-FileCopyrightText: 2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

package body Program.Nodes.Proxy_Associations is

   ----------------------
   -- Actual_Parameter --
   ----------------------

   overriding function Actual_Parameter (Self : Proxy_Association)
     return not null Program.Elements.Expressions.Expression_Access is
   begin
      return Self.Expression;
   end Actual_Parameter;

   -----------------
   -- Arrow_Token --
   -----------------

   overriding function Arrow_Token (Self : Proxy_Association)
     return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Arrow_Token;
   end Arrow_Token;

   ---------------
   -- Box_Token --
   ---------------

   overriding function Box_Token (Self : Proxy_Association)
     return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Box_Token;
   end Box_Token;

   -------------
   -- Choices --
   -------------

   overriding function Choices (Self : Proxy_Association)
     return Program.Element_Vectors.Element_Vector_Access
   is
   begin
      return Self.Choices;
   end Choices;

   ------------
   -- Create --
   ------------

   function Create
    (Choices     : Program.Element_Vectors.Element_Vector_Access;
     Arrow_Token : Program.Lexical_Elements.Lexical_Element_Access;
     Expression  : Program.Elements.Expressions.Expression_Access;
     Box_Token   : Program.Lexical_Elements.Lexical_Element_Access)
      return Proxy_Association is
   begin
      return Result : aliased Proxy_Association :=
        (Choices     => Choices,
         Arrow_Token => Arrow_Token,
         Expression  => Expression,
         Box_Token   => Box_Token,
         Enclosing_Element => null,
         Current => A_Record_Component_Association)
      do
         for Item in Result.Choices.Each_Element loop
            Set_Enclosing_Element (Item.Element, Result'Unchecked_Access);
         end loop;

         if Result.Expression.Assigned then
            Set_Enclosing_Element (Result.Expression, Result'Unchecked_Access);
         end if;
      end return;
   end Create;

   ----------------
   -- Expression --
   ----------------

   overriding function Expression (Self : Proxy_Association)
     return Program.Elements.Expressions.Expression_Access is
   begin
      return Self.Expression;
   end Expression;

   ----------------------
   -- Formal_Parameter --
   ----------------------

   overriding function Formal_Parameter (Self : Proxy_Association)
     return Program.Elements.Expressions.Expression_Access is
   begin
      if Self.Choices.Length = 1 then
         return Self.Choices.Element (1).To_Expression;
      else
         return null;
      end if;
   end Formal_Parameter;

   --------------------
   -- Is_Association --
   --------------------

   overriding function Is_Association (Self : Proxy_Association)
     return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Association;

   ------------------------------
   -- Is_Parameter_Association --
   ------------------------------

   overriding function Is_Parameter_Association (Self : Proxy_Association)
     return Boolean is
   begin
      return Self.Current = A_Parameter_Association;
   end Is_Parameter_Association;

   -------------------------------------
   -- Is_Record_Component_Association --
   -------------------------------------

   overriding function Is_Record_Component_Association
    (Self : Proxy_Association) return Boolean is
   begin
      return Self.Current = A_Record_Component_Association;
   end Is_Record_Component_Association;

   ------------------------------------------
   -- To_Record_Component_Association_Text --
   ------------------------------------------

   overriding function To_Record_Component_Association_Text
    (Self : aliased in out Proxy_Association)
      return Program.Elements.Record_Component_Associations
          .Record_Component_Association_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Record_Component_Association_Text;

   -----------------------------------
   -- To_Parameter_Association_Text --
   -----------------------------------

   overriding function To_Parameter_Association_Text
    (Self : aliased in out Proxy_Association)
      return Program.Elements.Parameter_Associations
          .Parameter_Association_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Parameter_Association_Text;

   -----------------------
   -- Turn_To_Parameter --
   -----------------------

   procedure Turn_To_Parameter (Self : in out Proxy_Association'Class) is
   begin
      Self.Current := A_Parameter_Association;
   end Turn_To_Parameter;

   -----------
   -- Visit --
   -----------

   overriding procedure Visit
    (Self    : not null access Proxy_Association;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      case Self.Current is
         when A_Parameter_Association =>
            Visitor.Parameter_Association (Self);
         when A_Record_Component_Association =>
            Visitor.Record_Component_Association (Self);
      end case;
   end Visit;

end Program.Nodes.Proxy_Associations;
