--  SPDX-FileCopyrightText: 2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Program.Element_Vectors;
with Program.Element_Visitors;
with Program.Elements.Expressions;
with Program.Elements.Parameter_Associations;
with Program.Elements.Record_Component_Associations;
with Program.Lexical_Elements;

package Program.Nodes.Proxy_Associations is

   pragma Preelaborate;

   type Proxy_Association is new Program.Nodes.Node
     and Program.Elements.Parameter_Associations.Parameter_Association
     and Program.Elements.Parameter_Associations.Parameter_Association_Text
     and Program.Elements.Record_Component_Associations
           .Record_Component_Association
     and Program.Elements.Record_Component_Associations
           .Record_Component_Association_Text
     with private;
   --  Internal common representation of
   --  * Parameter_Association
   --  * Record_Component_Association

   function Create
    (Choices     : Program.Element_Vectors.Element_Vector_Access;
     Arrow_Token : Program.Lexical_Elements.Lexical_Element_Access;
     Expression  : Program.Elements.Expressions.Expression_Access;
     Box_Token   : Program.Lexical_Elements.Lexical_Element_Access)
      return Proxy_Association;

   type Proxy_Association_Access is access all Proxy_Association;

   procedure Turn_To_Parameter (Self : in out Proxy_Association'Class);

private

   type Kind is (A_Parameter_Association, A_Record_Component_Association);

   type Proxy_Association is new Program.Nodes.Node
     and Program.Elements.Parameter_Associations.Parameter_Association
     and Program.Elements.Record_Component_Associations
         .Record_Component_Association
     and Program.Elements.Parameter_Associations.Parameter_Association_Text
     and Program.Elements.Record_Component_Associations
         .Record_Component_Association_Text
     with record
        Current     : Kind;
        Choices     : Program.Element_Vectors.Element_Vector_Access;
        Expression  : Program.Elements.Expressions.Expression_Access;
        Arrow_Token : Program.Lexical_Elements.Lexical_Element_Access;
        Box_Token   : Program.Lexical_Elements.Lexical_Element_Access;
     end record;

   overriding procedure Visit
    (Self    : not null access Proxy_Association;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Choices (Self : Proxy_Association)
     return Program.Element_Vectors.Element_Vector_Access;

   overriding function Expression (Self : Proxy_Association)
     return Program.Elements.Expressions.Expression_Access;

   overriding function Is_Record_Component_Association
    (Self : Proxy_Association)
     return Boolean;

   overriding function Is_Association (Self : Proxy_Association)
     return Boolean;

   overriding function Formal_Parameter (Self : Proxy_Association)
     return Program.Elements.Expressions.Expression_Access;

   overriding function Actual_Parameter (Self : Proxy_Association)
     return not null Program.Elements.Expressions.Expression_Access;

   overriding function Is_Parameter_Association (Self : Proxy_Association)
     return Boolean;

   overriding function To_Record_Component_Association_Text
    (Self : aliased in out Proxy_Association)
      return Program.Elements.Record_Component_Associations
          .Record_Component_Association_Text_Access;

   overriding function Arrow_Token (Self : Proxy_Association)
     return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Box_Token (Self : Proxy_Association)
     return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function To_Parameter_Association_Text
    (Self : aliased in out Proxy_Association)
      return Program.Elements.Parameter_Associations
          .Parameter_Association_Text_Access;

end Program.Nodes.Proxy_Associations;
