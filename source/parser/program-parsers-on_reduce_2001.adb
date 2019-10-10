
with Program.Parsers.Nodes;
use Program.Parsers.Nodes;
pragma Style_Checks ("N");
procedure Program.Parsers.On_Reduce_2001
  (Self  : access Parse_Context;
   Prod  : Anagram.Grammars.Production_Index;
   Nodes : in out Program.Parsers.Nodes.Node_Array) is
begin
   case Prod is
      when 2001 =>
         null;
      when 2002 =>
         null;
      when 2003 =>
         null;
      when 2004 =>
         null;
      when 2005 =>
         null;
      when 2006 =>

   declare
      List : Node := Nodes (1);
   begin
      Self.Factory.Append_Protected_Operation_Item
        (List, Nodes (2));
      Nodes (1) := List;
   end;

      when 2007 =>

   declare
      List : Node := Self.
        Factory.Protected_Operation_Item_Sequence;
   begin
      Self.Factory.Append_Protected_Operation_Item
        (List, Nodes (1));
      Nodes (1) := List;
   end;

      when 2008 =>

   Nodes (1) :=
      Self.Factory.Protected_Type_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          Nodes (9),
          Nodes (10),
          Nodes (11));

      when 2009 =>

   Nodes (1) :=
      Self.Factory.Protected_Type_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          No_Token,
          (Self.Factory.Subtype_Mark_Sequence),
          No_Token,
          Nodes (7),
          Nodes (8));

      when 2010 =>

   Nodes (1) :=
      Self.Factory.Protected_Type_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          Nodes (9),
          Nodes (10));

      when 2011 =>

   Nodes (1) :=
      Self.Factory.Protected_Type_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (5),
          No_Token,
          (Self.Factory.Subtype_Mark_Sequence),
          No_Token,
          Nodes (6),
          Nodes (7));

      when 2012 =>

   Nodes (1) :=
      Self.Factory.Protected_Type_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          None,
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          Nodes (9),
          Nodes (10));

      when 2013 =>

   Nodes (1) :=
      Self.Factory.Protected_Type_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          None,
          Nodes (4),
          Nodes (5),
          No_Token,
          (Self.Factory.Subtype_Mark_Sequence),
          No_Token,
          Nodes (6),
          Nodes (7));

      when 2014 =>

   Nodes (1) :=
      Self.Factory.Protected_Type_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          None,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          Nodes (9));

      when 2015 =>

   Nodes (1) :=
      Self.Factory.Protected_Type_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          None,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (4),
          No_Token,
          (Self.Factory.Subtype_Mark_Sequence),
          No_Token,
          Nodes (5),
          Nodes (6));

      when 2016 =>
  Nodes (1) := Self.Factory.Qualified_Expression
     (Nodes (1), Nodes (2), No_Token, Nodes (3), No_Token);

      when 2017 =>

   Nodes (1) := Self.Factory.Quantified_Expression
       (Nodes (1),
        Nodes (2),
        Nodes (3),
        Nodes (4),
        Nodes (5));

      when 2018 =>

   Nodes (1) := Self.Factory.Quantified_Expression
       (Nodes (1),
        Nodes (2),
        Nodes (3),
        Nodes (4),
        Nodes (5));

      when 2019 =>
         null;
      when 2020 =>
         null;
      when 2021 =>

   Nodes (1) :=
      Self.Factory.Raise_Statement
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5));

      when 2022 =>

   Nodes (1) :=
      Self.Factory.Raise_Statement
         (Nodes (1),
          Nodes (2),
          No_Token,
          None,
          Nodes (3));

      when 2023 =>

   Nodes (1) :=
      Self.Factory.Raise_Statement
         (Nodes (1),
          None,
          No_Token,
          None,
          Nodes (2));

      when 2024 =>
  Nodes (1) := Self.Factory.Range_Attribute_Reference
     (Nodes (1));

      when 2025 =>
  Nodes (1) := Self.Factory.Simple_Expression_Range
     (Nodes (1), Nodes (2), Nodes (3));

      when 2026 =>
  Nodes (1) := Self.Factory.Attribute_Reference
     (Nodes (1),
      Nodes (2),
      Self.Factory.Identifier (Nodes (3)),
      Nodes (5));

      when 2027 =>
  Nodes (1) := Self.Factory.Attribute_Reference
     (Nodes (1),
      Nodes (2),
      Self.Factory.Identifier (Nodes (3)),
      None);

      when 2028 =>
  Nodes (1) := Nodes (2);

      when 2029 =>
 Nodes (1) := Self.Factory.Real_Range_Specification
     (Nodes (1), Nodes (2), Nodes (3), Nodes (4));

      when 2030 =>
 Nodes (1) := Self.Factory.To_Aggregate_Or_Expression
     (Nodes (1));

      when 2031 =>
   Nodes (1) := Self.Factory.Association
     (Nodes (1),
      Nodes (2),
      Nodes (3));

      when 2032 =>
   Nodes (1) := Self.Factory.Association
     ((Self.Factory.Discrete_Choice_Sequence),
      No_Token,
      Nodes (1));

      when 2033 =>

   declare
      Box : constant Node :=
        Self.Factory.Box (Nodes (3));
   begin
     Nodes (1) := Self.Factory.Association
       (Nodes (1), Nodes (2), Box);
   end;

      when 2034 =>

   declare
      List : Node :=
        Self.Factory.Discrete_Choice_Sequence;
   begin
      Self.Factory.Prepend_Discrete_Choice
        (List, Nodes (1));
      Nodes (1) := Self.Factory.Association
        (List, No_Token, None);
   end;

      when 2035 =>

   declare
      List : Node :=
          Nodes (2);
   begin
      Self.Factory.Prepend_Association
        (List, Nodes (1));
      Nodes (1) := List;
   end;

      when 2036 =>

   declare
      List : Node :=
          Self.Factory.Association_Sequence;
   begin
      Self.Factory.Prepend_Association
        (List, Nodes (1));
      Nodes (1) := List;
   end;

      when 2037 =>

   declare
      List : constant Node := Self.Factory.Association_Sequence;
   begin
      Nodes (1) := List;
   end;

      when 2038 =>

   Nodes (1) := Self.Factory.
     Record_Definition
       (Nodes (1),
        Nodes (2),
        Nodes (3),
        Nodes (4));

      when 2039 =>

   Nodes (1) := Self.Factory.
     Null_Record_Definition
       (Nodes (1),
        Nodes (2));

      when 2040 =>

   Nodes (1) := Self.Factory.
     Record_Representation_Clause
       (Nodes (1),
        Nodes (2),
        Nodes (3),
        Nodes (4),
        Nodes (5),
        Nodes (6),
        Nodes (7),
        Nodes (8),
        Nodes (9),
        Nodes (10),
        Nodes (11),
        Nodes (12));

      when 2041 =>

   Nodes (1) := Self.Factory.
     Record_Representation_Clause
       (Nodes (1),
        Nodes (2),
        Nodes (3),
        Nodes (4),
        Nodes (5),
        Nodes (6),
        Nodes (7),
        Nodes (8),
        (Self.Factory.Clause_Or_Pragma_Sequence),
        Nodes (9),
        Nodes (10),
        Nodes (11));

      when 2042 =>

   Nodes (1) := Self.Factory.
     Record_Representation_Clause
       (Nodes (1),
        Nodes (2),
        Nodes (3),
        Nodes (4),
        No_Token,
        No_Token,
        None,
        No_Token,
        Nodes (5),
        Nodes (6),
        Nodes (7),
        Nodes (8));

      when 2043 =>

   Nodes (1) := Self.Factory.
     Record_Representation_Clause
       (Nodes (1),
        Nodes (2),
        Nodes (3),
        Nodes (4),
        No_Token,
        No_Token,
        None,
        No_Token,
        (Self.Factory.Clause_Or_Pragma_Sequence),
        Nodes (5),
        Nodes (6),
        Nodes (7));

      when 2044 =>

   Nodes (1) :=
     Self.Factory.Record_Type_Definition
      (Nodes (1),
       Nodes (2),
       Nodes (3),
       Nodes (4));

      when 2045 =>

   Nodes (1) :=
     Self.Factory.Record_Type_Definition
      (Nodes (1),
       Nodes (2),
       No_Token,
       Nodes (3));

      when 2046 =>

   Nodes (1) :=
     Self.Factory.Record_Type_Definition
      (No_Token,
       Nodes (1),
       Nodes (2),
       Nodes (3));

      when 2047 =>

   Nodes (1) :=
     Self.Factory.Record_Type_Definition
      (No_Token,
       Nodes (1),
       No_Token,
       Nodes (2));

      when 2048 =>

   Nodes (1) :=
     Self.Factory.Record_Type_Definition
      (No_Token,
       No_Token,
       Nodes (1),
       Nodes (2));

      when 2049 =>

   Nodes (1) :=
     Self.Factory.Record_Type_Definition
      (No_Token,
       No_Token,
       No_Token,
       Nodes (1));

      when 2050 =>
  Nodes (1) := Self.Factory.Infix_Call
     (Nodes (2), Nodes (1), Nodes (3));

      when 2051 =>
         null;
      when 2052 =>
  Nodes (1) := Self.Factory.Membership_Test
     (Nodes (1),
      Nodes (2),
      Nodes (3),
      Nodes (4));

      when 2053 =>
  Nodes (1) := Self.Factory.Membership_Test
     (Nodes (1),
      No_Token,
      Nodes (2),
      Nodes (3));

      when 2054 =>
         null;
      when 2055 =>
         null;
      when 2056 =>
         null;
      when 2057 =>
         null;
      when 2058 =>
         null;
      when 2059 =>
         null;
      when 2060 =>

   Nodes (1) :=
      Self.Factory.Requeue_Statement
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5));

      when 2061 =>

   Nodes (1) :=
      Self.Factory.Requeue_Statement
         (Nodes (1),
          Nodes (2),
          No_Token,
          No_Token,
          Nodes (3));

      when 2062 =>
         null;
      when 2063 =>
         null;
      when 2064 =>
         null;
      when 2065 =>
         null;
      when 2066 =>
         null;
      when 2067 =>
         null;
      when 2068 =>
         null;
      when 2069 =>

   declare
      Item : constant Node :=
        Self.Factory.Select_Or_Path
          (Nodes (2),
           Nodes (3),
           Nodes (4),
           Nodes (5),
           Nodes (6));
      List : Node :=
        Nodes (1);
   begin
      Self.Factory.Append_Select_Or_Else_Path
        (List, Item);
      Nodes (1) := List;
   end;

      when 2070 =>

   declare
      Item : constant Node :=
        Self.Factory.Select_Or_Path
          (Nodes (2),
           No_Token,
           None,
           No_Token,
           Nodes (3));
      List : Node :=
        Nodes (1);
   begin
      Self.Factory.Append_Select_Or_Else_Path
        (List, Item);
      Nodes (1) := List;
   end;

      when 2071 =>

   declare
      Item : constant Node :=
        Self.Factory.Select_Or_Path
          (Nodes (1),
           Nodes (2),
           Nodes (3),
           Nodes (4),
           Nodes (5));
      List : Node :=
        Self.Factory.Select_Or_Else_Path_Sequence;
   begin
      Self.Factory.Append_Select_Or_Else_Path
        (List, Item);
      Nodes (1) := List;
   end;

      when 2072 =>

   declare
      Item : constant Node :=
        Self.Factory.Select_Or_Path
          (Nodes (1),
           No_Token,
           None,
           No_Token,
           Nodes (2));
      List : Node :=
        Self.Factory.Select_Or_Else_Path_Sequence;
   begin
      Self.Factory.Append_Select_Or_Else_Path
        (List, Item);
      Nodes (1) := List;
   end;

      when 2073 =>
  Nodes (1) := Self.Factory.Selected_Component
     (Nodes (1), Nodes (2), Nodes (3));

      when 2074 =>

   Nodes (1) :=
      Self.Factory.Selected_Identifier
         (Nodes (1),
          Nodes (2),
          Nodes (3));

      when 2075 =>

   declare
      Item : constant Node :=
        Self.Factory.Select_Or_Path
          (No_Token,  --  Or_Token
           Nodes (2),
           Nodes (3),
           Nodes (4),
           Nodes (5));
      Else_Item : constant Node :=
        Self.Factory.Else_Path
          (Nodes (7), Nodes (8));
      List : Node :=
        Nodes (6);
   begin
      Self.Factory.Prepend_Select_Or_Else_Path
        (List, Item);
      Self.Factory.Append_Select_Or_Else_Path
        (List, Else_Item);
      Nodes (1) := Self.Factory.Selective_Accept
        (Nodes (1),
         List,
         Nodes (9),
         Nodes (10),
         Nodes (11));
   end;

      when 2076 =>

   declare
      Item : constant Node :=
        Self.Factory.Select_Or_Path
          (No_Token,  --  Or_Token
           Nodes (2),
           Nodes (3),
           Nodes (4),
           Nodes (5));
      List : Node :=
        Nodes (6);
   begin
      Self.Factory.Prepend_Select_Or_Else_Path
        (List, Item);
      Nodes (1) := Self.Factory.Selective_Accept
        (Nodes (1),
         List,
         Nodes (7),
         Nodes (8),
         Nodes (9));
   end;

      when 2077 =>

   declare
      Item : constant Node :=
        Self.Factory.Select_Or_Path
          (No_Token,  --  Or_Token
           Nodes (2),
           Nodes (3),
           Nodes (4),
           Nodes (5));
      Else_Item : constant Node :=
        Self.Factory.Else_Path
          (Nodes (6), Nodes (7));
      List : Node :=
        Self.Factory.Select_Or_Else_Path_Sequence;
   begin
      Self.Factory.Prepend_Select_Or_Else_Path
        (List, Item);
      Self.Factory.Append_Select_Or_Else_Path
        (List, Else_Item);
      Nodes (1) := Self.Factory.Selective_Accept
        (Nodes (1),
         List,
         Nodes (8),
         Nodes (9),
         Nodes (10));
   end;

      when 2078 =>

   declare
      Item : constant Node :=
        Self.Factory.Select_Or_Path
          (No_Token,  --  Or_Token
           Nodes (2),
           Nodes (3),
           Nodes (4),
           Nodes (5));
      List : Node :=
        Self.Factory.Select_Or_Else_Path_Sequence;
   begin
      Self.Factory.Prepend_Select_Or_Else_Path
        (List, Item);
      Nodes (1) := Self.Factory.Selective_Accept
        (Nodes (1),
         List,
         Nodes (6),
         Nodes (7),
         Nodes (8));
   end;

      when 2079 =>

   declare
      Item : constant Node :=
        Self.Factory.Select_Or_Path
          (No_Token,  --  Or_Token
           No_Token,
           None,
           No_Token,
           Nodes (2));
      Else_Item : constant Node :=
        Self.Factory.Else_Path
          (Nodes (4), Nodes (5));
      List : Node :=
        Nodes (3);
   begin
      Self.Factory.Prepend_Select_Or_Else_Path
        (List, Item);
      Self.Factory.Append_Select_Or_Else_Path
        (List, Else_Item);
      Nodes (1) := Self.Factory.Selective_Accept
        (Nodes (1),
         List,
         Nodes (6),
         Nodes (7),
         Nodes (8));
   end;

      when 2080 =>

   declare
      Item : constant Node :=
        Self.Factory.Select_Or_Path
          (No_Token,  --  Or_Token
           No_Token,
           None,
           No_Token,
           Nodes (2));
      List : Node :=
        Nodes (3);
   begin
      Self.Factory.Prepend_Select_Or_Else_Path
        (List, Item);
      Nodes (1) := Self.Factory.Selective_Accept
        (Nodes (1),
         List,
         Nodes (4),
         Nodes (5),
         Nodes (6));
   end;

      when 2081 =>

   declare
      Item : constant Node :=
        Self.Factory.Select_Or_Path
          (No_Token,  --  Or_Token
           No_Token,
           None,
           No_Token,
           Nodes (2));
      Else_Item : constant Node :=
        Self.Factory.Else_Path
          (Nodes (3), Nodes (4));
      List : Node :=
        Self.Factory.Select_Or_Else_Path_Sequence;
   begin
      Self.Factory.Prepend_Select_Or_Else_Path
        (List, Item);
      Self.Factory.Append_Select_Or_Else_Path
        (List, Else_Item);
      Nodes (1) := Self.Factory.Selective_Accept
        (Nodes (1),
         List,
         Nodes (5),
         Nodes (6),
         Nodes (7));
   end;

      when 2082 =>

   declare
      Item : constant Node :=
        Self.Factory.Select_Or_Path
          (No_Token,  --  Or_Token
           No_Token,
           None,
           No_Token,
           Nodes (2));
      List : Node :=
        Self.Factory.Select_Or_Else_Path_Sequence;
   begin
      Self.Factory.Prepend_Select_Or_Else_Path
        (List, Item);
      Nodes (1) := Self.Factory.Selective_Accept
        (Nodes (1),
         List,
         Nodes (3),
         Nodes (4),
         Nodes (5));
   end;

      when 2083 =>
         null;
      when 2084 =>
  Nodes (1) := Self.Factory.Character_Literal
     (Nodes (1));

      when 2085 =>
         null;
      when 2086 =>

   declare
      List : Node :=
        Nodes (2);
   begin
      Self.Factory.Prepend_Exception_Handler
        (List, Nodes (1));
      Nodes (1) := List;
   end;

      when 2087 =>

   declare
      List : Node :=
        Self.Factory.
        Exception_Handler_Sequence;
   begin
      Self.Factory.Prepend_Exception_Handler
        (List, Nodes (1));
      Nodes (1) := List;
   end;

      when 2088 =>

   declare
      List : Node :=
        Nodes (2);
      Dummy : constant Node :=
        Self.Factory.Label_Decorator
          (Nodes (3), None);
   begin
      Self.Factory.Prepend_Statement (List, Nodes (1));
      Self.Factory.Append_Statement
        (List, Dummy);
      Nodes (1) := List;
   end;

      when 2089 =>

   declare
      List : Node :=
        Nodes (2);
   begin
      Self.Factory.Prepend_Statement
        (List, Nodes (1));
      Nodes (1) := List;
   end;

      when 2090 =>

   declare
      List : Node :=
        Self.Factory.
        Statement_Sequence;
      Dummy : constant Node :=
        Self.Factory.Label_Decorator
          (Nodes (2), None);
   begin
      Self.Factory.Prepend_Statement (List, Nodes (1));
      Self.Factory.Append_Statement
        (List, Dummy);
      Nodes (1) := List;
   end;

      when 2091 =>

   declare
      List : Node :=
        Self.Factory.
        Statement_Sequence;
   begin
      Self.Factory.Prepend_Statement
        (List, Nodes (1));
      Nodes (1) := List;
   end;

      when 2092 =>
  Nodes (1) :=
     Self.Factory.Signed_Integer_Type_Definition
      (Nodes (1),
       Nodes (2),
       Nodes (3),
       Nodes (4));

      when 2093 =>
  Nodes (1) := Self.Factory.Numeric_Literal
     (Nodes (1));

      when 2094 =>
  Nodes (1) := Self.Factory.Null_Literal
     (Nodes (1));

      when 2095 =>
         null;
      when 2096 =>
         null;
      when 2097 =>
         null;
      when 2098 =>
   Nodes (1) := Nodes (2);

      when 2099 =>
   Nodes (1) := Nodes (2);

      when 2100 =>
   Nodes (1) := Self.Factory.Infix_Call
     (Nodes (1), None, Nodes (2));

      when 2101 =>
   Nodes (1) := Self.Factory.Infix_Call
     (Nodes (1), None, Nodes (2));

      when 2102 =>
   Nodes (1) := Self.Factory.Infix_Call
     (Nodes (2), Nodes (1), Nodes (3));

      when 2103 =>
   Nodes (1) := Self.Factory.Infix_Call
     (Nodes (2), Nodes (1), Nodes (3));

      when 2104 =>
   Nodes (1) := Self.Factory.Infix_Call
     (Nodes (2), Nodes (1), Nodes (3));

      when 2105 =>
   Nodes (1) := Self.Factory.Infix_Call
     (Nodes (2), Nodes (1), Nodes (3));

      when 2106 =>
   Nodes (1) := Self.Factory.Infix_Call
     (Nodes (2), Nodes (1), Nodes (3));

      when 2107 =>
  Nodes (1) := Self.Factory.Infix_Call
     (Nodes (1), None, Nodes (2));

      when 2108 =>
   Nodes (1) := Self.Factory.Infix_Call
     (Nodes (1), None, Nodes (2));

      when 2109 =>
 Nodes (1) := Self.Factory.Infix_Call
     (Nodes (2), Nodes (1), Nodes (3));

      when 2110 =>
  Nodes (1) := Self.Factory.Infix_Call
     (Nodes (2), Nodes (1), Nodes (3));

      when 2111 =>
  Nodes (1) := Self.Factory.Infix_Call
     (Nodes (2), Nodes (1), Nodes (3));

      when 2112 =>

   Nodes (1) :=
      Self.Factory.Simple_Return_Statement
         (Nodes (1),
          Nodes (2),
          Nodes (3));

      when 2113 =>

   Nodes (1) :=
      Self.Factory.Simple_Return_Statement
         (Nodes (1),
          None,
          Nodes (2));

      when 2114 =>

   Nodes (1) :=
      Self.Factory.Single_Protected_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          Nodes (9));

      when 2115 =>

   Nodes (1) :=
      Self.Factory.Single_Protected_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token,
          (Self.Factory.Subtype_Mark_Sequence),
          No_Token,
          Nodes (5),
          Nodes (6));

      when 2116 =>

   Nodes (1) :=
      Self.Factory.Single_Protected_Declaration
         (Nodes (1),
          Nodes (2),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8));

      when 2117 =>

   Nodes (1) :=
      Self.Factory.Single_Protected_Declaration
         (Nodes (1),
          Nodes (2),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (3),
          No_Token,
          (Self.Factory.Subtype_Mark_Sequence),
          No_Token,
          Nodes (4),
          Nodes (5));

      when 2118 =>

   Nodes (1) :=
      Self.Factory.Single_Task_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          Nodes (9));

      when 2119 =>

   Nodes (1) :=
      Self.Factory.Single_Task_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token,
          (Self.Factory.Subtype_Mark_Sequence),
          No_Token,
          Nodes (5),
          Nodes (6));

      when 2120 =>

   Nodes (1) :=
      Self.Factory.Single_Task_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          No_Token,
          (Self.Factory.Subtype_Mark_Sequence),
          No_Token,
          None,
          Nodes (4));

      when 2121 =>

   Nodes (1) :=
      Self.Factory.Single_Task_Declaration
         (Nodes (1),
          Nodes (2),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8));

      when 2122 =>

   Nodes (1) :=
      Self.Factory.Single_Task_Declaration
         (Nodes (1),
          Nodes (2),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (3),
          No_Token,
          (Self.Factory.Subtype_Mark_Sequence),
          No_Token,
          Nodes (4),
          Nodes (5));

      when 2123 =>

   Nodes (1) :=
      Self.Factory.Single_Task_Declaration
         (Nodes (1),
          Nodes (2),
          (Self.Factory.Aspect_Specification_Sequence),
          No_Token,
          No_Token,
          (Self.Factory.Subtype_Mark_Sequence),
          No_Token,
          None,
          Nodes (3));

      when 2124 =>

   Nodes (1) := Self.Factory.Label_Decorator
          (Nodes (1), Nodes (2));

      when 2125 =>
         null;
      when 2126 =>

   declare
      List : Node := Nodes (1);
   begin
      Self.Factory.Append_Statement
        (List, Nodes (2));
      Nodes (1) := List;
   end;

      when 2127 =>

   declare
      List : Node := Self.
        Factory.Statement_Sequence;
   begin
      Self.Factory.Append_Statement
        (List, Nodes (1));
      Nodes (1) := List;
   end;

      when 2128 =>

   Nodes (1) :=
      Self.Factory.Subtype_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6));

      when 2129 =>

   Nodes (1) :=
      Self.Factory.Subtype_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (5));

      when 2130 =>

   Nodes (1) := Self.Factory.To_Subtype_Indication
       (Nodes (1),
        Nodes (2),
        Nodes (3),
        Nodes (4));

      when 2131 =>

   Nodes (1) := Self.Factory.To_Subtype_Indication
       (Nodes (1),
        Nodes (2),
        Nodes (3),
        None);

      when 2132 =>

   Nodes (1) := Self.Factory.To_Subtype_Indication
       (No_Token,
        No_Token,
        Nodes (1),
        Nodes (2));

      when 2133 =>

   Nodes (1) := Self.Factory.To_Subtype_Indication
       (No_Token,
        No_Token,
        Nodes (1),
        None);

      when 2134 =>
         null;
      when 2135 =>

   declare
      List : Node := Nodes (1);
   begin
      Self.Factory.Append_Subtype_Mark
        (List, Nodes (3));
      Nodes (1) := List;
   end;

      when 2136 =>

   declare
      List : Node := Self.
        Factory.Subtype_Mark_Sequence;
   begin
      Self.Factory.Append_Subtype_Mark
        (List, Nodes (2));
      Nodes (1) := List;
   end;

      when 2137 =>

   Nodes (1) :=
      Self.Factory.Subunit
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6));

      when 2138 =>

   Nodes (1) :=
      Self.Factory.Subunit
         ((Self.Factory.Context_Item_Sequence),
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5));

      when 2139 =>

   Nodes (1) :=
      Self.Factory.Task_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          Nodes (9),
          Nodes (10),
          Nodes (11),
          Nodes (12),
          Nodes (13));

      when 2140 =>

   Nodes (1) :=
      Self.Factory.Task_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          Nodes (9),
          Nodes (10),
          Nodes (11),
          No_Token,
          Nodes (12));

      when 2141 =>

   Nodes (1) :=
      Self.Factory.Task_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (9),
          Nodes (10),
          Nodes (11));

      when 2142 =>

   Nodes (1) :=
      Self.Factory.Task_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (9),
          No_Token,
          Nodes (10));

      when 2143 =>

   Nodes (1) :=
      Self.Factory.Task_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          Nodes (9),
          Nodes (10),
          Nodes (11),
          Nodes (12));

      when 2144 =>

   Nodes (1) :=
      Self.Factory.Task_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          Nodes (9),
          Nodes (10),
          No_Token,
          Nodes (11));

      when 2145 =>

   Nodes (1) :=
      Self.Factory.Task_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (6),
          Nodes (7),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (8),
          Nodes (9),
          Nodes (10));

      when 2146 =>

   Nodes (1) :=
      Self.Factory.Task_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (6),
          Nodes (7),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (8),
          No_Token,
          Nodes (9));

      when 2147 =>

   Nodes (1) :=
      Self.Factory.Task_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          Nodes (9),
          Nodes (10),
          Nodes (11),
          Nodes (12));

      when 2148 =>

   Nodes (1) :=
      Self.Factory.Task_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          Nodes (9),
          Nodes (10),
          No_Token,
          Nodes (11));

      when 2149 =>

   Nodes (1) :=
      Self.Factory.Task_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (8),
          Nodes (9),
          Nodes (10));

      when 2150 =>

   Nodes (1) :=
      Self.Factory.Task_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (8),
          No_Token,
          Nodes (9));

      when 2151 =>

   Nodes (1) :=
      Self.Factory.Task_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (4),
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          Nodes (9),
          Nodes (10),
          Nodes (11));

      when 2152 =>

   Nodes (1) :=
      Self.Factory.Task_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (4),
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          Nodes (9),
          No_Token,
          Nodes (10));

      when 2153 =>

   Nodes (1) :=
      Self.Factory.Task_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (4),
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (5),
          Nodes (6),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (7),
          Nodes (8),
          Nodes (9));

      when 2154 =>

   Nodes (1) :=
      Self.Factory.Task_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (4),
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (5),
          Nodes (6),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (7),
          No_Token,
          Nodes (8));

      when 2155 =>

   Nodes (1) :=
      Self.Factory.Task_Body_Stub
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7));

      when 2156 =>

   Nodes (1) :=
      Self.Factory.Task_Body_Stub
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (6));

      when 2157 =>

   Nodes (1) :=
      Self.Factory.Task_Definition
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5));

      when 2158 =>

   Nodes (1) :=
      Self.Factory.Task_Definition
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token);

      when 2159 =>

   Nodes (1) :=
      Self.Factory.Task_Definition
         (Nodes (1),
          Nodes (2),
          (Self.Factory.Task_Item_Sequence),
          Nodes (3),
          Nodes (4));

      when 2160 =>

   Nodes (1) :=
      Self.Factory.Task_Definition
         (Nodes (1),
          Nodes (2),
          (Self.Factory.Task_Item_Sequence),
          Nodes (3),
          No_Token);

      when 2161 =>

   Nodes (1) :=
      Self.Factory.Task_Definition
         (Nodes (1),
          No_Token,
          (Self.Factory.Task_Item_Sequence),
          Nodes (2),
          Nodes (3));

      when 2162 =>

   Nodes (1) :=
      Self.Factory.Task_Definition
         (Nodes (1),
          No_Token,
          (Self.Factory.Task_Item_Sequence),
          Nodes (2),
          No_Token);

      when 2163 =>

   Nodes (1) :=
      Self.Factory.Task_Definition
         ((Self.Factory.Task_Item_Sequence),
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4));

      when 2164 =>

   Nodes (1) :=
      Self.Factory.Task_Definition
         ((Self.Factory.Task_Item_Sequence),
          Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token);

      when 2165 =>

   Nodes (1) :=
      Self.Factory.Task_Definition
         ((Self.Factory.Task_Item_Sequence),
          Nodes (1),
          (Self.Factory.Task_Item_Sequence),
          Nodes (2),
          Nodes (3));

      when 2166 =>

   Nodes (1) :=
      Self.Factory.Task_Definition
         ((Self.Factory.Task_Item_Sequence),
          Nodes (1),
          (Self.Factory.Task_Item_Sequence),
          Nodes (2),
          No_Token);

      when 2167 =>

   Nodes (1) :=
      Self.Factory.Task_Definition
         ((Self.Factory.Task_Item_Sequence),
          No_Token,
          (Self.Factory.Task_Item_Sequence),
          Nodes (1),
          Nodes (2));

      when 2168 =>

   Nodes (1) :=
      Self.Factory.Task_Definition
         ((Self.Factory.Task_Item_Sequence),
          No_Token,
          (Self.Factory.Task_Item_Sequence),
          Nodes (1),
          No_Token);

      when 2169 =>
         null;
      when 2170 =>
         null;
      when 2171 =>

   declare
      List : Node := Nodes (1);
   begin
      Self.Factory.Prepend_Task_Item
        (List, Nodes (2));
      Nodes (1) := List;
   end;

      when 2172 =>

   declare
      List : Node := Self.
        Factory.Task_Item_Sequence;
   begin
      Self.Factory.Prepend_Task_Item
        (List, Nodes (1));
      Nodes (1) := List;
   end;

      when 2173 =>

   Nodes (1) :=
      Self.Factory.Task_Type_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          Nodes (9),
          Nodes (10),
          Nodes (11));

      when 2174 =>

   Nodes (1) :=
      Self.Factory.Task_Type_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          No_Token,
          (Self.Factory.Subtype_Mark_Sequence),
          No_Token,
          Nodes (7),
          Nodes (8));

      when 2175 =>

   Nodes (1) :=
      Self.Factory.Task_Type_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          No_Token,
          No_Token,
          (Self.Factory.Subtype_Mark_Sequence),
          No_Token,
          None,
          Nodes (6));

      when 2176 =>

   Nodes (1) :=
      Self.Factory.Task_Type_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          Nodes (9),
          Nodes (10));

      when 2177 =>

   Nodes (1) :=
      Self.Factory.Task_Type_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (5),
          No_Token,
          (Self.Factory.Subtype_Mark_Sequence),
          No_Token,
          Nodes (6),
          Nodes (7));

      when 2178 =>

   Nodes (1) :=
      Self.Factory.Task_Type_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          (Self.Factory.Aspect_Specification_Sequence),
          No_Token,
          No_Token,
          (Self.Factory.Subtype_Mark_Sequence),
          No_Token,
          None,
          Nodes (5));

      when 2179 =>

   Nodes (1) :=
      Self.Factory.Task_Type_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          None,
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          Nodes (9),
          Nodes (10));

      when 2180 =>

   Nodes (1) :=
      Self.Factory.Task_Type_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          None,
          Nodes (4),
          Nodes (5),
          No_Token,
          (Self.Factory.Subtype_Mark_Sequence),
          No_Token,
          Nodes (6),
          Nodes (7));

      when 2181 =>

   Nodes (1) :=
      Self.Factory.Task_Type_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          None,
          Nodes (4),
          No_Token,
          No_Token,
          (Self.Factory.Subtype_Mark_Sequence),
          No_Token,
          None,
          Nodes (5));

      when 2182 =>

   Nodes (1) :=
      Self.Factory.Task_Type_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          None,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          Nodes (9));

      when 2183 =>

   Nodes (1) :=
      Self.Factory.Task_Type_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          None,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (4),
          No_Token,
          (Self.Factory.Subtype_Mark_Sequence),
          No_Token,
          Nodes (5),
          Nodes (6));

      when 2184 =>

   Nodes (1) :=
      Self.Factory.Task_Type_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          None,
          (Self.Factory.Aspect_Specification_Sequence),
          No_Token,
          No_Token,
          (Self.Factory.Subtype_Mark_Sequence),
          No_Token,
          None,
          Nodes (4));

      when 2185 =>

   declare
      Item : constant Node :=
        Self.Factory.Terminate_Alternative_Statement
          (Nodes (1), Nodes (2));
      List : Node := Self.Factory.Statement_Sequence;
   begin
      Self.Factory.Append_Statement
        (List, Item);
      Nodes (1) := List;
   end;

      when 2186 =>
         null;
      when 2187 =>
         null;
      when 2188 =>
         null;
      when 2189 =>
         null;
      when 2190 =>
         null;
      when 2191 =>
         null;
      when 2192 =>
         null;
      when 2193 =>
         null;
      when 2194 =>
         null;
      when 2195 =>
         null;
      when 2196 =>
         null;
      when 2197 =>

   declare
      List : Node :=
        Nodes (4);
   begin
      Self.Factory.Prepend_Subtype_Mark
        (List, Nodes (3));
      Nodes (1) := Self.Factory.
        Unconstrained_Array_Definition
          (Nodes (1),
           Nodes (2),
           List,
           Nodes (5),
           Nodes (6),
           Nodes (7));
   end;

      when 2198 =>

   declare
      List : Node :=
        Self.Factory.
        Subtype_Mark_Sequence;
   begin
      Self.Factory.Prepend_Subtype_Mark
        (List, Nodes (3));
      Nodes (1) := Self.Factory.
        Unconstrained_Array_Definition
          (Nodes (1),
           Nodes (2),
           List,
           Nodes (4),
           Nodes (5),
           Nodes (6));
   end;

      when 2199 =>
  Nodes (1) := Self.Factory.Unknown_Discriminant_Part
     (Nodes (1), Nodes (2), Nodes (3));

      when 2200 =>
         null;
      when 2201 =>
         null;
      when 2202 =>
         null;
      when 2203 =>
         null;
      when 2204 =>
         null;
      when 2205 =>
         null;
      when 2206 =>
         null;
      when 2207 =>
         null;
      when 2208 =>
         null;
      when 2209 =>
         null;
      when 2210 =>
         null;
      when 2211 =>
         null;
      when 2212 =>
         null;
      when 2213 =>
         null;
      when 2214 =>
         null;
      when 2215 =>
         null;
      when 2216 =>
         null;
      when 2217 =>
         null;
      when 2218 =>
         null;
      when 2219 =>
         null;
      when 2220 =>
         null;
      when 2221 =>

   declare
      List : Node := Nodes (3);
   begin
      Self.Factory.Prepend_Program_Unit_Name
        (List, Nodes (2));
      Nodes (1) :=
        Self.Factory.Use_Package_Clause
          (Nodes (1),
           List,
           Nodes (4));
   end;

      when 2222 =>

   declare
      List : Node := Self.
        Factory.Program_Unit_Name_Sequence;
   begin
      Self.Factory.Prepend_Program_Unit_Name
        (List, Nodes (2));
      Nodes (1) :=
        Self.Factory.Use_Package_Clause
          (Nodes (1),
           List,
           Nodes (3));
   end;

      when 2223 =>

   declare
      List : Node := Nodes (5);
   begin
      Self.Factory.Prepend_Subtype_Mark
        (List, Nodes (4));
      Nodes (1) :=
        Self.Factory.Use_Type_Clause
          (Nodes (1),
           Nodes (2),
           Nodes (3),
           List,
           Nodes (6));
   end;

      when 2224 =>

   declare
      List : Node := Self.
        Factory.Subtype_Mark_Sequence;
   begin
      Self.Factory.Prepend_Subtype_Mark
        (List, Nodes (4));
      Nodes (1) :=
        Self.Factory.Use_Type_Clause
          (Nodes (1),
           Nodes (2),
           Nodes (3),
           List,
           Nodes (5));
   end;

      when 2225 =>

   declare
      List : Node := Nodes (4);
   begin
      Self.Factory.Prepend_Subtype_Mark
        (List, Nodes (3));
      Nodes (1) :=
        Self.Factory.Use_Type_Clause
          (Nodes (1),
           No_Token,
           Nodes (2),
           List,
           Nodes (5));
   end;

      when 2226 =>

   declare
      List : Node := Self.
        Factory.Subtype_Mark_Sequence;
   begin
      Self.Factory.Prepend_Subtype_Mark
        (List, Nodes (3));
      Nodes (1) :=
        Self.Factory.Use_Type_Clause
          (Nodes (1),
           No_Token,
           Nodes (2),
           List,
           Nodes (4));
   end;

      when 2227 =>

   Nodes (1) := Self.Factory.Variant
     (Nodes (1),
      Nodes (2),
      Nodes (3),
      Nodes (4));

      when 2228 =>

   declare
      List : Node := Nodes (1);
   begin
      Self.Factory.Append_Variant
        (List, Nodes (2));
      Nodes (1) := List;
   end;

      when 2229 =>

   declare
      List : Node := Self.
        Factory.Variant_Sequence;
   begin
      Self.Factory.Append_Variant
        (List, Nodes (1));
      Nodes (1) := List;
   end;

      when 2230 =>

   declare
      List : Node :=
        Nodes (5);
   begin
      Self.Factory.Prepend_Variant
        (List, Nodes (4));
      Nodes (1) := Self.Factory.Variant_Part
        (Nodes (1),
         Nodes (2),
         Nodes (3),
         List,
         Nodes (6),
         Nodes (7),
         Nodes (8));
   end;

      when 2231 =>

   declare
      List : Node :=
        Self.Factory.Variant_Sequence;
   begin
      Self.Factory.Prepend_Variant
        (List, Nodes (4));
      Nodes (1) := Self.Factory.Variant_Part
        (Nodes (1),
         Nodes (2),
         Nodes (3),
         List,
         Nodes (5),
         Nodes (6),
         Nodes (7));
   end;

      when 2232 =>

   declare
      List : Node := Nodes (5);
   begin
      Self.Factory.Prepend_Program_Unit_Name
        (List, Nodes (4));
      Nodes (1) :=
        Self.Factory.With_Clause
          (Nodes (1),
           Nodes (2),
           Nodes (3),
           List,
           Nodes (6));
   end;

      when 2233 =>

   declare
      List : Node := Self.
        Factory.Program_Unit_Name_Sequence;
   begin
      Self.Factory.Prepend_Program_Unit_Name
        (List, Nodes (4));
      Nodes (1) :=
        Self.Factory.With_Clause
          (Nodes (1),
           Nodes (2),
           Nodes (3),
           List,
           Nodes (5));
   end;

      when 2234 =>

   declare
      List : Node := Nodes (4);
   begin
      Self.Factory.Prepend_Program_Unit_Name
        (List, Nodes (3));
      Nodes (1) :=
        Self.Factory.With_Clause
          (Nodes (1),
           No_Token,
           Nodes (2),
           List,
           Nodes (5));
   end;

      when 2235 =>

   declare
      List : Node := Self.
        Factory.Program_Unit_Name_Sequence;
   begin
      Self.Factory.Prepend_Program_Unit_Name
        (List, Nodes (3));
      Nodes (1) :=
        Self.Factory.With_Clause
          (Nodes (1),
           No_Token,
           Nodes (2),
           List,
           Nodes (4));
   end;

      when 2236 =>

   declare
      List : Node := Nodes (4);
   begin
      Self.Factory.Prepend_Program_Unit_Name
        (List, Nodes (3));
      Nodes (1) :=
        Self.Factory.With_Clause
          (No_Token,
           Nodes (1),
           Nodes (2),
           List,
           Nodes (5));
   end;

      when 2237 =>

   declare
      List : Node := Self.
        Factory.Program_Unit_Name_Sequence;
   begin
      Self.Factory.Prepend_Program_Unit_Name
        (List, Nodes (3));
      Nodes (1) :=
        Self.Factory.With_Clause
          (No_Token,
           Nodes (1),
           Nodes (2),
           List,
           Nodes (4));
   end;

      when 2238 =>

   declare
      List : Node := Nodes (3);
   begin
      Self.Factory.Prepend_Program_Unit_Name
        (List, Nodes (2));
      Nodes (1) :=
        Self.Factory.With_Clause
          (No_Token,
           No_Token,
           Nodes (1),
           List,
           Nodes (4));
   end;

      when 2239 =>

   declare
      List : Node := Self.
        Factory.Program_Unit_Name_Sequence;
   begin
      Self.Factory.Prepend_Program_Unit_Name
        (List, Nodes (2));
      Nodes (1) :=
        Self.Factory.With_Clause
          (No_Token,
           No_Token,
           Nodes (1),
           List,
           Nodes (3));
   end;

      when others =>
         raise Constraint_Error;
   end case;
end Program.Parsers.On_Reduce_2001;

