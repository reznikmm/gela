
with Program.Parsers.Nodes;
use Program.Parsers.Nodes;
pragma Style_Checks ("N");
procedure Program.Parsers.On_Reduce_1
  (Self  : access Parse_Context;
   Prod  : Anagram.Grammars.Production_Index;
   Nodes : in out Program.Parsers.Nodes.Node_Array) is
begin
   case Prod is
      when 1 =>

   declare
      List : Node :=
        Nodes (2);
   begin
      Self.Factory.Prepend_Compilation_Unit
        (List, Nodes (1));
      Nodes (1) := Self.Factory.Compilation
         (List, Nodes (3));
   end;

      when 2 =>

   declare
      List : Node :=
        Nodes (2);
   begin
      Self.Factory.Prepend_Compilation_Unit
        (List, Nodes (1));
      Nodes (1) := Self.Factory.Compilation
         (List, (Self.Factory.Context_Item_Sequence));
   end;

      when 3 =>

   declare
      List : Node :=
        Self.Factory.Compilation_Unit_Sequence;
   begin
      Self.Factory.Prepend_Compilation_Unit
        (List, Nodes (1));
      Nodes (1) := Self.Factory.Compilation
         (List, Nodes (2));
   end;

      when 4 =>

   declare
      List : Node :=
        Self.Factory.Compilation_Unit_Sequence;
   begin
      Self.Factory.Prepend_Compilation_Unit
        (List, Nodes (1));
      Nodes (1) := Self.Factory.Compilation
         (List, (Self.Factory.Context_Item_Sequence));
   end;

      when 5 =>

   declare
      List : Node :=
        Nodes (3);
   begin
      Self.Factory.Prepend_Name (List, Nodes (2));
      Nodes (1) :=
        Self.Factory.Abort_Statement
          (Nodes (1), List, Nodes (4));
   end;

      when 6 =>

   declare
      List : Node :=
        Self.Factory.Name_Sequence;
   begin
      Self.Factory.Prepend_Name (List, Nodes (2));
      Nodes (1) :=
        Self.Factory.Abort_Statement
          (Nodes (1), List, Nodes (3));
   end;

      when 7 =>

   declare
      List : Node :=
        Nodes (2);
   begin
      Self.Factory.Prepend_Statement
        (List, Nodes (1));
      Nodes (1) := List;
   end;

      when 8 =>

   declare
      List : Node :=
        Self.Factory.Statement_Sequence;
   begin
      Self.Factory.Prepend_Statement
        (List, Nodes (1));
      Nodes (1) := List;
   end;

      when 9 =>

   Nodes (1) :=
      Self.Factory.Accept_Statement
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
          Nodes (13),
          Nodes (14),
          Nodes (15));

      when 10 =>

   Nodes (1) :=
      Self.Factory.Accept_Statement
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
          Nodes (13),
          No_Token,
          Nodes (14));

      when 11 =>

   Nodes (1) :=
      Self.Factory.Accept_Statement
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
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (11),
          Nodes (12),
          Nodes (13));

      when 12 =>

   Nodes (1) :=
      Self.Factory.Accept_Statement
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
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (11),
          No_Token,
          Nodes (12));

      when 13 =>

   Nodes (1) :=
      Self.Factory.Accept_Statement
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          No_Token,
          No_Token,
          Nodes (9));

      when 14 =>

   Nodes (1) :=
      Self.Factory.Accept_Statement
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (6),
          Nodes (7),
          Nodes (8),
          Nodes (9),
          Nodes (10),
          Nodes (11),
          Nodes (12));

      when 15 =>

   Nodes (1) :=
      Self.Factory.Accept_Statement
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (6),
          Nodes (7),
          Nodes (8),
          Nodes (9),
          Nodes (10),
          No_Token,
          Nodes (11));

      when 16 =>

   Nodes (1) :=
      Self.Factory.Accept_Statement
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (6),
          Nodes (7),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (8),
          Nodes (9),
          Nodes (10));

      when 17 =>

   Nodes (1) :=
      Self.Factory.Accept_Statement
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (6),
          Nodes (7),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (8),
          No_Token,
          Nodes (9));

      when 18 =>

   Nodes (1) :=
      Self.Factory.Accept_Statement
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          No_Token,
          No_Token,
          Nodes (6));

      when 19 =>

   Nodes (1) :=
      Self.Factory.Accept_Statement
         (Nodes (1),
          Nodes (2),
          No_Token,
          None,
          No_Token,
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

      when 20 =>

   Nodes (1) :=
      Self.Factory.Accept_Statement
         (Nodes (1),
          Nodes (2),
          No_Token,
          None,
          No_Token,
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          Nodes (9),
          Nodes (10),
          No_Token,
          Nodes (11));

      when 21 =>

   Nodes (1) :=
      Self.Factory.Accept_Statement
         (Nodes (1),
          Nodes (2),
          No_Token,
          None,
          No_Token,
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (8),
          Nodes (9),
          Nodes (10));

      when 22 =>

   Nodes (1) :=
      Self.Factory.Accept_Statement
         (Nodes (1),
          Nodes (2),
          No_Token,
          None,
          No_Token,
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (8),
          No_Token,
          Nodes (9));

      when 23 =>

   Nodes (1) :=
      Self.Factory.Accept_Statement
         (Nodes (1),
          Nodes (2),
          No_Token,
          None,
          No_Token,
          Nodes (3),
          Nodes (4),
          Nodes (5),
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          No_Token,
          No_Token,
          Nodes (6));

      when 24 =>

   Nodes (1) :=
      Self.Factory.Accept_Statement
         (Nodes (1),
          Nodes (2),
          No_Token,
          None,
          No_Token,
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          Nodes (9));

      when 25 =>

   Nodes (1) :=
      Self.Factory.Accept_Statement
         (Nodes (1),
          Nodes (2),
          No_Token,
          None,
          No_Token,
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          No_Token,
          Nodes (8));

      when 26 =>

   Nodes (1) :=
      Self.Factory.Accept_Statement
         (Nodes (1),
          Nodes (2),
          No_Token,
          None,
          No_Token,
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (3),
          Nodes (4),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (5),
          Nodes (6),
          Nodes (7));

      when 27 =>

   Nodes (1) :=
      Self.Factory.Accept_Statement
         (Nodes (1),
          Nodes (2),
          No_Token,
          None,
          No_Token,
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (3),
          Nodes (4),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (5),
          No_Token,
          Nodes (6));

      when 28 =>

   Nodes (1) :=
      Self.Factory.Accept_Statement
         (Nodes (1),
          Nodes (2),
          No_Token,
          None,
          No_Token,
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          No_Token,
          No_Token,
          Nodes (3));

      when 29 =>

   Nodes (1) := Self.Factory.
     Anonymous_Access_To_Object_Definition
       (Nodes (1),
        Nodes (2),
        Nodes (3),
        Nodes (4),
        Nodes (5));

      when 30 =>

   Nodes (1) := Self.Factory.
     Anonymous_Access_To_Object_Definition
       (Nodes (1),
        Nodes (2),
        Nodes (3),
        No_Token,
        Nodes (4));

      when 31 =>

   Nodes (1) := Self.Factory.
     Anonymous_Access_To_Object_Definition
       (No_Token,
        No_Token,
        Nodes (1),
        Nodes (2),
        Nodes (3));

      when 32 =>

   Nodes (1) := Self.Factory.
     Anonymous_Access_To_Object_Definition
       (No_Token,
        No_Token,
        Nodes (1),
        No_Token,
        Nodes (2));

      when 33 =>

   Nodes (1) := Self.Factory.
     Anonymous_Access_To_Procedure_Definition
       (Nodes (1),
        Nodes (2),
        Nodes (3),
        Nodes (4),
        Nodes (5),
        Nodes (6),
        Nodes (7),
        Nodes (8));

      when 34 =>

   Nodes (1) := Self.Factory.
     Anonymous_Access_To_Procedure_Definition
       (Nodes (1),
        Nodes (2),
        Nodes (3),
        Nodes (4),
        Nodes (5),
        No_Token,
        (Self.Factory.Parameter_Specification_Sequence),
        No_Token);

      when 35 =>

   Nodes (1) := Self.Factory.
     Anonymous_Access_To_Procedure_Definition
       (Nodes (1),
        Nodes (2),
        Nodes (3),
        No_Token,
        Nodes (4),
        Nodes (5),
        Nodes (6),
        Nodes (7));

      when 36 =>

   Nodes (1) := Self.Factory.
     Anonymous_Access_To_Procedure_Definition
       (Nodes (1),
        Nodes (2),
        Nodes (3),
        No_Token,
        Nodes (4),
        No_Token,
        (Self.Factory.Parameter_Specification_Sequence),
        No_Token);

      when 37 =>

   Nodes (1) := Self.Factory.
     Anonymous_Access_To_Procedure_Definition
       (No_Token,
        No_Token,
        Nodes (1),
        Nodes (2),
        Nodes (3),
        Nodes (4),
        Nodes (5),
        Nodes (6));

      when 38 =>

   Nodes (1) := Self.Factory.
     Anonymous_Access_To_Procedure_Definition
       (No_Token,
        No_Token,
        Nodes (1),
        Nodes (2),
        Nodes (3),
        No_Token,
        (Self.Factory.Parameter_Specification_Sequence),
        No_Token);

      when 39 =>

   Nodes (1) := Self.Factory.
     Anonymous_Access_To_Procedure_Definition
       (No_Token,
        No_Token,
        Nodes (1),
        No_Token,
        Nodes (2),
        Nodes (3),
        Nodes (4),
        Nodes (5));

      when 40 =>

   Nodes (1) := Self.Factory.
     Anonymous_Access_To_Procedure_Definition
       (No_Token,
        No_Token,
        Nodes (1),
        No_Token,
        Nodes (2),
        No_Token,
        (Self.Factory.Parameter_Specification_Sequence),
        No_Token);

      when 41 =>

   Nodes (1) := Self.Factory.Anonymous_Access_To_Function_Definition
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

      when 42 =>

   Nodes (1) := Self.Factory.Anonymous_Access_To_Function_Definition
       (Nodes (1),
        Nodes (2),
        Nodes (3),
        Nodes (4),
        Nodes (5),
        Nodes (6),
        Nodes (7),
        Nodes (8),
        Nodes (9),
        No_Token,
        No_Token,
        Nodes (10));

      when 43 =>

   Nodes (1) := Self.Factory.Anonymous_Access_To_Function_Definition
       (Nodes (1),
        Nodes (2),
        Nodes (3),
        Nodes (4),
        Nodes (5),
        No_Token,
        (Self.Factory.Parameter_Specification_Sequence),
        No_Token,
        Nodes (6),
        Nodes (7),
        Nodes (8),
        Nodes (9));

      when 44 =>

   Nodes (1) := Self.Factory.Anonymous_Access_To_Function_Definition
       (Nodes (1),
        Nodes (2),
        Nodes (3),
        Nodes (4),
        Nodes (5),
        No_Token,
        (Self.Factory.Parameter_Specification_Sequence),
        No_Token,
        Nodes (6),
        No_Token,
        No_Token,
        Nodes (7));

      when 45 =>

   Nodes (1) := Self.Factory.Anonymous_Access_To_Function_Definition
       (Nodes (1),
        Nodes (2),
        Nodes (3),
        No_Token,
        Nodes (4),
        Nodes (5),
        Nodes (6),
        Nodes (7),
        Nodes (8),
        Nodes (9),
        Nodes (10),
        Nodes (11));

      when 46 =>

   Nodes (1) := Self.Factory.Anonymous_Access_To_Function_Definition
       (Nodes (1),
        Nodes (2),
        Nodes (3),
        No_Token,
        Nodes (4),
        Nodes (5),
        Nodes (6),
        Nodes (7),
        Nodes (8),
        No_Token,
        No_Token,
        Nodes (9));

      when 47 =>

   Nodes (1) := Self.Factory.Anonymous_Access_To_Function_Definition
       (Nodes (1),
        Nodes (2),
        Nodes (3),
        No_Token,
        Nodes (4),
        No_Token,
        (Self.Factory.Parameter_Specification_Sequence),
        No_Token,
        Nodes (5),
        Nodes (6),
        Nodes (7),
        Nodes (8));

      when 48 =>

   Nodes (1) := Self.Factory.Anonymous_Access_To_Function_Definition
       (Nodes (1),
        Nodes (2),
        Nodes (3),
        No_Token,
        Nodes (4),
        No_Token,
        (Self.Factory.Parameter_Specification_Sequence),
        No_Token,
        Nodes (5),
        No_Token,
        No_Token,
        Nodes (6));

      when 49 =>

   Nodes (1) := Self.Factory.Anonymous_Access_To_Function_Definition
       (No_Token,
        No_Token,
        Nodes (1),
        Nodes (2),
        Nodes (3),
        Nodes (4),
        Nodes (5),
        Nodes (6),
        Nodes (7),
        Nodes (8),
        Nodes (9),
        Nodes (10));

      when 50 =>

   Nodes (1) := Self.Factory.Anonymous_Access_To_Function_Definition
       (No_Token,
        No_Token,
        Nodes (1),
        Nodes (2),
        Nodes (3),
        Nodes (4),
        Nodes (5),
        Nodes (6),
        Nodes (7),
        No_Token,
        No_Token,
        Nodes (8));

      when 51 =>

   Nodes (1) := Self.Factory.Anonymous_Access_To_Function_Definition
       (No_Token,
        No_Token,
        Nodes (1),
        Nodes (2),
        Nodes (3),
        No_Token,
        (Self.Factory.Parameter_Specification_Sequence),
        No_Token,
        Nodes (4),
        Nodes (5),
        Nodes (6),
        Nodes (7));

      when 52 =>

   Nodes (1) := Self.Factory.Anonymous_Access_To_Function_Definition
       (No_Token,
        No_Token,
        Nodes (1),
        Nodes (2),
        Nodes (3),
        No_Token,
        (Self.Factory.Parameter_Specification_Sequence),
        No_Token,
        Nodes (4),
        No_Token,
        No_Token,
        Nodes (5));

      when 53 =>

   Nodes (1) := Self.Factory.Anonymous_Access_To_Function_Definition
       (No_Token,
        No_Token,
        Nodes (1),
        No_Token,
        Nodes (2),
        Nodes (3),
        Nodes (4),
        Nodes (5),
        Nodes (6),
        Nodes (7),
        Nodes (8),
        Nodes (9));

      when 54 =>

   Nodes (1) := Self.Factory.Anonymous_Access_To_Function_Definition
       (No_Token,
        No_Token,
        Nodes (1),
        No_Token,
        Nodes (2),
        Nodes (3),
        Nodes (4),
        Nodes (5),
        Nodes (6),
        No_Token,
        No_Token,
        Nodes (7));

      when 55 =>

   Nodes (1) := Self.Factory.Anonymous_Access_To_Function_Definition
       (No_Token,
        No_Token,
        Nodes (1),
        No_Token,
        Nodes (2),
        No_Token,
        (Self.Factory.Parameter_Specification_Sequence),
        No_Token,
        Nodes (3),
        Nodes (4),
        Nodes (5),
        Nodes (6));

      when 56 =>

   Nodes (1) := Self.Factory.Anonymous_Access_To_Function_Definition
       (No_Token,
        No_Token,
        Nodes (1),
        No_Token,
        Nodes (2),
        No_Token,
        (Self.Factory.Parameter_Specification_Sequence),
        No_Token,
        Nodes (3),
        No_Token,
        No_Token,
        Nodes (4));

      when 57 =>

   Nodes (1) := Self.Factory.Anonymous_Access_To_Function_Definition
       (Nodes (1),
        Nodes (2),
        Nodes (3),
        Nodes (4),
        Nodes (5),
        Nodes (6),
        Nodes (7),
        Nodes (8),
        Nodes (9),
        No_Token,
        No_Token,
        Nodes (1));

      when 58 =>

   Nodes (1) := Self.Factory.Anonymous_Access_To_Function_Definition
       (Nodes (1),
        Nodes (2),
        Nodes (3),
        Nodes (4),
        Nodes (5),
        No_Token,
        (Self.Factory.Parameter_Specification_Sequence),
        No_Token,
        Nodes (6),
        No_Token,
        No_Token,
        Nodes (1));

      when 59 =>

   Nodes (1) := Self.Factory.Anonymous_Access_To_Function_Definition
       (Nodes (1),
        Nodes (2),
        Nodes (3),
        No_Token,
        Nodes (4),
        Nodes (5),
        Nodes (6),
        Nodes (7),
        Nodes (8),
        No_Token,
        No_Token,
        Nodes (1));

      when 60 =>

   Nodes (1) := Self.Factory.Anonymous_Access_To_Function_Definition
       (Nodes (1),
        Nodes (2),
        Nodes (3),
        No_Token,
        Nodes (4),
        No_Token,
        (Self.Factory.Parameter_Specification_Sequence),
        No_Token,
        Nodes (5),
        No_Token,
        No_Token,
        Nodes (1));

      when 61 =>

   Nodes (1) := Self.Factory.Anonymous_Access_To_Function_Definition
       (No_Token,
        No_Token,
        Nodes (1),
        Nodes (2),
        Nodes (3),
        Nodes (4),
        Nodes (5),
        Nodes (6),
        Nodes (7),
        No_Token,
        No_Token,
        Nodes (1));

      when 62 =>

   Nodes (1) := Self.Factory.Anonymous_Access_To_Function_Definition
       (No_Token,
        No_Token,
        Nodes (1),
        Nodes (2),
        Nodes (3),
        No_Token,
        (Self.Factory.Parameter_Specification_Sequence),
        No_Token,
        Nodes (4),
        No_Token,
        No_Token,
        Nodes (1));

      when 63 =>

   Nodes (1) := Self.Factory.Anonymous_Access_To_Function_Definition
       (No_Token,
        No_Token,
        Nodes (1),
        No_Token,
        Nodes (2),
        Nodes (3),
        Nodes (4),
        Nodes (5),
        Nodes (6),
        No_Token,
        No_Token,
        Nodes (1));

      when 64 =>

   Nodes (1) := Self.Factory.Anonymous_Access_To_Function_Definition
       (No_Token,
        No_Token,
        Nodes (1),
        No_Token,
        Nodes (2),
        No_Token,
        (Self.Factory.Parameter_Specification_Sequence),
        No_Token,
        Nodes (3),
        No_Token,
        No_Token,
        Nodes (1));

      when 65 =>

   Nodes (1) := Self.Factory.
     Access_To_Object_Definition
       (Nodes (1),
        Nodes (2),
        Nodes (3),
        Nodes (4),
        Nodes (5));

      when 66 =>

   Nodes (1) := Self.Factory.
     Access_To_Object_Definition
       (Nodes (1),
        Nodes (2),
        Nodes (3),
        No_Token,
        Nodes (4));

      when 67 =>

   Nodes (1) := Self.Factory.
     Access_To_Object_Definition
       (No_Token,
        No_Token,
        Nodes (1),
        Nodes (2),
        Nodes (3));

      when 68 =>

   Nodes (1) := Self.Factory.
     Access_To_Object_Definition
       (No_Token,
        No_Token,
        Nodes (1),
        No_Token,
        Nodes (2));

      when 69 =>

   Nodes (1) := Self.Factory.
     Access_To_Procedure_Definition
       (Nodes (1),
        Nodes (2),
        Nodes (3),
        Nodes (4),
        Nodes (5),
        Nodes (6),
        Nodes (7),
        Nodes (8));

      when 70 =>

   Nodes (1) := Self.Factory.
     Access_To_Procedure_Definition
       (Nodes (1),
        Nodes (2),
        Nodes (3),
        Nodes (4),
        Nodes (5),
        No_Token,
        (Self.Factory.Parameter_Specification_Sequence),
        No_Token);

      when 71 =>

   Nodes (1) := Self.Factory.
     Access_To_Procedure_Definition
       (Nodes (1),
        Nodes (2),
        Nodes (3),
        No_Token,
        Nodes (4),
        Nodes (5),
        Nodes (6),
        Nodes (7));

      when 72 =>

   Nodes (1) := Self.Factory.
     Access_To_Procedure_Definition
       (Nodes (1),
        Nodes (2),
        Nodes (3),
        No_Token,
        Nodes (4),
        No_Token,
        (Self.Factory.Parameter_Specification_Sequence),
        No_Token);

      when 73 =>

   Nodes (1) := Self.Factory.
     Access_To_Procedure_Definition
       (No_Token,
        No_Token,
        Nodes (1),
        Nodes (2),
        Nodes (3),
        Nodes (4),
        Nodes (5),
        Nodes (6));

      when 74 =>

   Nodes (1) := Self.Factory.
     Access_To_Procedure_Definition
       (No_Token,
        No_Token,
        Nodes (1),
        Nodes (2),
        Nodes (3),
        No_Token,
        (Self.Factory.Parameter_Specification_Sequence),
        No_Token);

      when 75 =>

   Nodes (1) := Self.Factory.
     Access_To_Procedure_Definition
       (No_Token,
        No_Token,
        Nodes (1),
        No_Token,
        Nodes (2),
        Nodes (3),
        Nodes (4),
        Nodes (5));

      when 76 =>

   Nodes (1) := Self.Factory.
     Access_To_Procedure_Definition
       (No_Token,
        No_Token,
        Nodes (1),
        No_Token,
        Nodes (2),
        No_Token,
        (Self.Factory.Parameter_Specification_Sequence),
        No_Token);

      when 77 =>

   Nodes (1) := Self.Factory.Access_To_Function_Definition
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

      when 78 =>

   Nodes (1) := Self.Factory.Access_To_Function_Definition
       (Nodes (1),
        Nodes (2),
        Nodes (3),
        Nodes (4),
        Nodes (5),
        Nodes (6),
        Nodes (7),
        Nodes (8),
        Nodes (9),
        No_Token,
        No_Token,
        Nodes (10));

      when 79 =>

   Nodes (1) := Self.Factory.Access_To_Function_Definition
       (Nodes (1),
        Nodes (2),
        Nodes (3),
        Nodes (4),
        Nodes (5),
        No_Token,
        (Self.Factory.Parameter_Specification_Sequence),
        No_Token,
        Nodes (6),
        Nodes (7),
        Nodes (8),
        Nodes (9));

      when 80 =>

   Nodes (1) := Self.Factory.Access_To_Function_Definition
       (Nodes (1),
        Nodes (2),
        Nodes (3),
        Nodes (4),
        Nodes (5),
        No_Token,
        (Self.Factory.Parameter_Specification_Sequence),
        No_Token,
        Nodes (6),
        No_Token,
        No_Token,
        Nodes (7));

      when 81 =>

   Nodes (1) := Self.Factory.Access_To_Function_Definition
       (Nodes (1),
        Nodes (2),
        Nodes (3),
        No_Token,
        Nodes (4),
        Nodes (5),
        Nodes (6),
        Nodes (7),
        Nodes (8),
        Nodes (9),
        Nodes (10),
        Nodes (11));

      when 82 =>

   Nodes (1) := Self.Factory.Access_To_Function_Definition
       (Nodes (1),
        Nodes (2),
        Nodes (3),
        No_Token,
        Nodes (4),
        Nodes (5),
        Nodes (6),
        Nodes (7),
        Nodes (8),
        No_Token,
        No_Token,
        Nodes (9));

      when 83 =>

   Nodes (1) := Self.Factory.Access_To_Function_Definition
       (Nodes (1),
        Nodes (2),
        Nodes (3),
        No_Token,
        Nodes (4),
        No_Token,
        (Self.Factory.Parameter_Specification_Sequence),
        No_Token,
        Nodes (5),
        Nodes (6),
        Nodes (7),
        Nodes (8));

      when 84 =>

   Nodes (1) := Self.Factory.Access_To_Function_Definition
       (Nodes (1),
        Nodes (2),
        Nodes (3),
        No_Token,
        Nodes (4),
        No_Token,
        (Self.Factory.Parameter_Specification_Sequence),
        No_Token,
        Nodes (5),
        No_Token,
        No_Token,
        Nodes (6));

      when 85 =>

   Nodes (1) := Self.Factory.Access_To_Function_Definition
       (No_Token,
        No_Token,
        Nodes (1),
        Nodes (2),
        Nodes (3),
        Nodes (4),
        Nodes (5),
        Nodes (6),
        Nodes (7),
        Nodes (8),
        Nodes (9),
        Nodes (10));

      when 86 =>

   Nodes (1) := Self.Factory.Access_To_Function_Definition
       (No_Token,
        No_Token,
        Nodes (1),
        Nodes (2),
        Nodes (3),
        Nodes (4),
        Nodes (5),
        Nodes (6),
        Nodes (7),
        No_Token,
        No_Token,
        Nodes (8));

      when 87 =>

   Nodes (1) := Self.Factory.Access_To_Function_Definition
       (No_Token,
        No_Token,
        Nodes (1),
        Nodes (2),
        Nodes (3),
        No_Token,
        (Self.Factory.Parameter_Specification_Sequence),
        No_Token,
        Nodes (4),
        Nodes (5),
        Nodes (6),
        Nodes (7));

      when 88 =>

   Nodes (1) := Self.Factory.Access_To_Function_Definition
       (No_Token,
        No_Token,
        Nodes (1),
        Nodes (2),
        Nodes (3),
        No_Token,
        (Self.Factory.Parameter_Specification_Sequence),
        No_Token,
        Nodes (4),
        No_Token,
        No_Token,
        Nodes (5));

      when 89 =>

   Nodes (1) := Self.Factory.Access_To_Function_Definition
       (No_Token,
        No_Token,
        Nodes (1),
        No_Token,
        Nodes (2),
        Nodes (3),
        Nodes (4),
        Nodes (5),
        Nodes (6),
        Nodes (7),
        Nodes (8),
        Nodes (9));

      when 90 =>

   Nodes (1) := Self.Factory.Access_To_Function_Definition
       (No_Token,
        No_Token,
        Nodes (1),
        No_Token,
        Nodes (2),
        Nodes (3),
        Nodes (4),
        Nodes (5),
        Nodes (6),
        No_Token,
        No_Token,
        Nodes (7));

      when 91 =>

   Nodes (1) := Self.Factory.Access_To_Function_Definition
       (No_Token,
        No_Token,
        Nodes (1),
        No_Token,
        Nodes (2),
        No_Token,
        (Self.Factory.Parameter_Specification_Sequence),
        No_Token,
        Nodes (3),
        Nodes (4),
        Nodes (5),
        Nodes (6));

      when 92 =>

   Nodes (1) := Self.Factory.Access_To_Function_Definition
       (No_Token,
        No_Token,
        Nodes (1),
        No_Token,
        Nodes (2),
        No_Token,
        (Self.Factory.Parameter_Specification_Sequence),
        No_Token,
        Nodes (3),
        No_Token,
        No_Token,
        Nodes (4));

      when 93 =>

   Nodes (1) := Self.Factory.Access_To_Function_Definition
       (Nodes (1),
        Nodes (2),
        Nodes (3),
        Nodes (4),
        Nodes (5),
        Nodes (6),
        Nodes (7),
        Nodes (8),
        Nodes (9),
        No_Token,
        No_Token,
        Nodes (10));

      when 94 =>

   Nodes (1) := Self.Factory.Access_To_Function_Definition
       (Nodes (1),
        Nodes (2),
        Nodes (3),
        Nodes (4),
        Nodes (5),
        No_Token,
        (Self.Factory.Parameter_Specification_Sequence),
        No_Token,
        Nodes (6),
        No_Token,
        No_Token,
        Nodes (7));

      when 95 =>

   Nodes (1) := Self.Factory.Access_To_Function_Definition
       (Nodes (1),
        Nodes (2),
        Nodes (3),
        No_Token,
        Nodes (4),
        Nodes (5),
        Nodes (6),
        Nodes (7),
        Nodes (8),
        No_Token,
        No_Token,
        Nodes (9));

      when 96 =>

   Nodes (1) := Self.Factory.Access_To_Function_Definition
       (Nodes (1),
        Nodes (2),
        Nodes (3),
        No_Token,
        Nodes (4),
        No_Token,
        (Self.Factory.Parameter_Specification_Sequence),
        No_Token,
        Nodes (5),
        No_Token,
        No_Token,
        Nodes (6));

      when 97 =>

   Nodes (1) := Self.Factory.Access_To_Function_Definition
       (No_Token,
        No_Token,
        Nodes (1),
        Nodes (2),
        Nodes (3),
        Nodes (4),
        Nodes (5),
        Nodes (6),
        Nodes (7),
        No_Token,
        No_Token,
        Nodes (8));

      when 98 =>

   Nodes (1) := Self.Factory.Access_To_Function_Definition
       (No_Token,
        No_Token,
        Nodes (1),
        Nodes (2),
        Nodes (3),
        No_Token,
        (Self.Factory.Parameter_Specification_Sequence),
        No_Token,
        Nodes (4),
        No_Token,
        No_Token,
        Nodes (5));

      when 99 =>

   Nodes (1) := Self.Factory.Access_To_Function_Definition
       (No_Token,
        No_Token,
        Nodes (1),
        No_Token,
        Nodes (2),
        Nodes (3),
        Nodes (4),
        Nodes (5),
        Nodes (6),
        No_Token,
        No_Token,
        Nodes (7));

      when 100 =>

   Nodes (1) := Self.Factory.Access_To_Function_Definition
       (No_Token,
        No_Token,
        Nodes (1),
        No_Token,
        Nodes (2),
        No_Token,
        (Self.Factory.Parameter_Specification_Sequence),
        No_Token,
        Nodes (3),
        No_Token,
        No_Token,
        Nodes (4));

      when 101 =>
         null;
      when 102 =>
         null;
      when 103 =>
  Nodes (1) := Self.Factory.Allocator
   (Nodes (1),
    Nodes (2),
    Nodes (3),
    Nodes (4),
    Nodes (5));

      when 104 =>
  Nodes (1) := Self.Factory.Allocator
   (Nodes (1),
    No_Token,
    None,
    No_Token,
    Nodes (2));

      when 105 =>

   declare
      List : Node := Nodes (1);
   begin
      Self.Factory.Append_Subtype_Mark
        (List, Nodes (3));
      Nodes (1) := List;
   end;

      when 106 =>

   declare
      List : Node := Self.
        Factory.Subtype_Mark_Sequence;
   begin
      Self.Factory.Append_Subtype_Mark
        (List, Nodes (2));
      Nodes (1) := List;
   end;

      when 107 =>
         null;
      when 108 =>
         null;
      when 109 =>
         null;
      when 110 =>
         null;
      when 111 =>

   declare
      Comp : constant Node :=
        Self.Factory.Aspect_Specification
          (Nodes (3),
           Nodes (4),
           Nodes (5));
      List : Node := Nodes (1);
   begin
      Self.Factory.Append_Aspect_Specification
        (List, Comp);
      Nodes (1) := List;
   end;

      when 112 =>

   declare
      Comp : constant Node :=
        Self.Factory.Aspect_Specification
          (Nodes (3),
           No_Token,
           None);
      List : Node := Nodes (1);
   begin
      Self.Factory.Append_Aspect_Specification
        (List, Comp);
      Nodes (1) := List;
   end;

      when 113 =>

   declare
      Comp : constant Node :=
        Self.Factory.Aspect_Specification
          (Nodes (2),
           Nodes (3),
           Nodes (4));
      List : Node := Self.
        Factory.Aspect_Specification_Sequence;
   begin
      Self.Factory.Append_Aspect_Specification
        (List, Comp);
      Nodes (1) := List;
   end;

      when 114 =>

   declare
      Comp : constant Node :=
        Self.Factory.Aspect_Specification
          (Nodes (2),
           No_Token,
           None);
      List : Node := Self.
        Factory.Aspect_Specification_Sequence;
   begin
      Self.Factory.Append_Aspect_Specification
        (List, Comp);
      Nodes (1) := List;
   end;

      when 115 =>

   declare
      Comp : constant Node :=
        Self.Factory.Aspect_Specification
          (Nodes (2),
           Nodes (3),
           Nodes (4));
      List : Node := Nodes (5);
   begin
      Self.Factory.Prepend_Aspect_Specification
        (List, Comp);
      Nodes (1) := List;
   end;

      when 116 =>

   declare
      Comp : constant Node :=
        Self.Factory.Aspect_Specification
          (Nodes (2),
           Nodes (3),
           Nodes (4));
      List : Node := Self.
        Factory.Aspect_Specification_Sequence;
   begin
      Self.Factory.Prepend_Aspect_Specification
        (List, Comp);
      Nodes (1) := List;
   end;

      when 117 =>

   declare
      Comp : constant Node :=
        Self.Factory.Aspect_Specification
          (Nodes (2),
           No_Token,
           None);
      List : Node := Nodes (3);
   begin
      Self.Factory.Prepend_Aspect_Specification
        (List, Comp);
      Nodes (1) := List;
   end;

      when 118 =>

   declare
      Comp : constant Node :=
        Self.Factory.Aspect_Specification
          (Nodes (2),
           No_Token,
           None);
      List : Node := Self.
        Factory.Aspect_Specification_Sequence;
   begin
      Self.Factory.Prepend_Aspect_Specification
        (List, Comp);
      Nodes (1) := List;
   end;

      when 119 =>

   Nodes (1) :=
      Self.Factory.Assignment_Statement
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4));

      when 120 =>

   Nodes (1) :=
     Self.Factory.Association_List
       (Nodes (1),
        Nodes (2),
        Nodes (3));

      when 121 =>

   declare
      Item : constant Node :=
        Self.Factory.Select_Or_Path
          (No_Token, No_Token, None, No_Token,
           Nodes (2));
      Then_Item : constant Node :=
        Self.Factory.Then_Abort_Path
          (Nodes (3),
           Nodes (4),
           Nodes (5));
      List : Node :=
        Self.Factory.Select_Then_Abort_Path_Sequence;
   begin
      Self.Factory.Append_Select_Then_Abort_Path
        (List, Item);
      Self.Factory.Append_Select_Then_Abort_Path
        (List, Then_Item);
      Nodes (1) := Self.Factory.Asynchronous_Select
        (Nodes (1),
         List,
         Nodes (6),
         Nodes (7),
         Nodes (8));
   end;

      when 122 =>

   Nodes (1) := Self.Factory.
     At_Clause
       (Nodes (1),
        Nodes (2),
        Nodes (3),
        Nodes (4),
        Nodes (5),
        Nodes (6));

      when 123 =>

   Nodes (1) := Self.Factory.
     Attribute_Definition_Clause
       (Nodes (1),
        Nodes (2),
        Nodes (3),
        Nodes (4),
        Nodes (5));

      when 124 =>
  Nodes (1) := Self.Factory.Attribute_Reference
   (Nodes (1), Nodes (2), Nodes (3), None);

      when 125 =>
         null;
      when 126 =>
         null;
      when 127 =>
         null;
      when 128 =>
         null;
      when 129 =>
         null;
      when 130 =>
         null;
      when 131 =>
         null;
      when 132 =>
         null;
      when 133 =>
         null;
      when 134 =>
         null;
      when 135 =>
         null;
      when 136 =>
         null;
      when 137 =>
         null;
      when 138 =>
         null;
      when 139 =>
         null;
      when 140 =>
         null;
      when 141 =>
         null;
      when 142 =>
         null;
      when 143 =>
         null;
      when 144 =>
         null;
      when 145 =>
         null;
      when 146 =>
         null;
      when 147 =>
         null;
      when 148 =>
         null;
      when 149 =>

   declare
      List : Node := Nodes (1);
   begin
      Self.Factory.Append_Basic_Declarative_Item
        (List, Nodes (2));
      Nodes (1) := List;
   end;

      when 150 =>

   declare
      List : Node := Self.
        Factory.Basic_Declarative_Item_Sequence;
   begin
      Self.Factory.Append_Basic_Declarative_Item
        (List, Nodes (1));
      Nodes (1) := List;
   end;

      when 151 =>

   Nodes (1) :=
      Self.Factory.Block_Statement
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

      when 152 =>

   Nodes (1) :=
      Self.Factory.Block_Statement
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          Nodes (9),
          No_Token,
          Nodes (10));

      when 153 =>

   Nodes (1) :=
      Self.Factory.Block_Statement
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (7),
          Nodes (8),
          Nodes (9));

      when 154 =>

   Nodes (1) :=
      Self.Factory.Block_Statement
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (7),
          No_Token,
          Nodes (8));

      when 155 =>

   Nodes (1) :=
      Self.Factory.Block_Statement
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          Nodes (9),
          Nodes (10));

      when 156 =>

   Nodes (1) :=
      Self.Factory.Block_Statement
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          No_Token,
          Nodes (9));

      when 157 =>

   Nodes (1) :=
      Self.Factory.Block_Statement
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (4),
          Nodes (5),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (6),
          Nodes (7),
          Nodes (8));

      when 158 =>

   Nodes (1) :=
      Self.Factory.Block_Statement
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (4),
          Nodes (5),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (6),
          No_Token,
          Nodes (7));

      when 159 =>

   Nodes (1) :=
      Self.Factory.Block_Statement
         (Nodes (1),
          Nodes (2),
          No_Token,
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          Nodes (9));

      when 160 =>

   Nodes (1) :=
      Self.Factory.Block_Statement
         (Nodes (1),
          Nodes (2),
          No_Token,
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          No_Token,
          Nodes (8));

      when 161 =>

   Nodes (1) :=
      Self.Factory.Block_Statement
         (Nodes (1),
          Nodes (2),
          No_Token,
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (3),
          Nodes (4),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (5),
          Nodes (6),
          Nodes (7));

      when 162 =>

   Nodes (1) :=
      Self.Factory.Block_Statement
         (Nodes (1),
          Nodes (2),
          No_Token,
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (3),
          Nodes (4),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (5),
          No_Token,
          Nodes (6));

      when 163 =>

   Nodes (1) :=
      Self.Factory.Block_Statement
         (None,
          No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          Nodes (9));

      when 164 =>

   Nodes (1) :=
      Self.Factory.Block_Statement
         (None,
          No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          No_Token,
          Nodes (8));

      when 165 =>

   Nodes (1) :=
      Self.Factory.Block_Statement
         (None,
          No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (5),
          Nodes (6),
          Nodes (7));

      when 166 =>

   Nodes (1) :=
      Self.Factory.Block_Statement
         (None,
          No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (5),
          No_Token,
          Nodes (6));

      when 167 =>

   Nodes (1) :=
      Self.Factory.Block_Statement
         (None,
          No_Token,
          Nodes (1),
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8));

      when 168 =>

   Nodes (1) :=
      Self.Factory.Block_Statement
         (None,
          No_Token,
          Nodes (1),
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          No_Token,
          Nodes (7));

      when 169 =>

   Nodes (1) :=
      Self.Factory.Block_Statement
         (None,
          No_Token,
          Nodes (1),
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (4),
          Nodes (5),
          Nodes (6));

      when 170 =>

   Nodes (1) :=
      Self.Factory.Block_Statement
         (None,
          No_Token,
          Nodes (1),
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (4),
          No_Token,
          Nodes (5));

      when 171 =>

   Nodes (1) :=
      Self.Factory.Block_Statement
         (None,
          No_Token,
          No_Token,
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7));

      when 172 =>

   Nodes (1) :=
      Self.Factory.Block_Statement
         (None,
          No_Token,
          No_Token,
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          No_Token,
          Nodes (6));

      when 173 =>

   Nodes (1) :=
      Self.Factory.Block_Statement
         (None,
          No_Token,
          No_Token,
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (1),
          Nodes (2),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (3),
          Nodes (4),
          Nodes (5));

      when 174 =>

   Nodes (1) :=
      Self.Factory.Block_Statement
         (None,
          No_Token,
          No_Token,
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (1),
          Nodes (2),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (3),
          No_Token,
          Nodes (4));

      when 175 =>
         null;
      when 176 =>
         null;
      when 177 =>
         null;
      when 178 =>
         null;
      when 179 =>
         null;
      when 180 =>

   declare
      List : Node :=
        Nodes (1);
   begin
      Self.Factory.Append_Case_Expression_Path
        (List, Nodes (3));
      Nodes (1) := List;
   end;

      when 181 =>

   declare
      List : Node :=
        Self.Factory.Case_Expression_Path_Sequence;
   begin
      Self.Factory.Append_Case_Expression_Path
        (List, Nodes (2));
      Nodes (1) := List;
   end;

      when 182 =>

   declare
      List : Node :=
        Nodes (5);
   begin
      Self.Factory.Prepend_Case_Expression_Path
        (List, Nodes (4));
      Nodes (1) := Self.Factory.Case_Expression
        (Nodes (1),
         Nodes (2),
         Nodes (3),
         List);
   end;

      when 183 =>

   declare
      List : Node :=
        Self.Factory.Case_Expression_Path_Sequence;
   begin
      Self.Factory.Prepend_Case_Expression_Path
        (List, Nodes (4));
      Nodes (1) := Self.Factory.Case_Expression
        (Nodes (1),
         Nodes (2),
         Nodes (3),
         List);
   end;

      when 184 =>
  Nodes (1) := Self.Factory.Case_Expression_Path
   (Nodes (1),
    Nodes (2),
    Nodes (3),
    Nodes (4));

      when 185 =>

   declare
      List : Node :=
        Nodes (5);
   begin
      Self.Factory.Prepend_Case_Path
        (List, Nodes (4));
      Nodes (1) := Self.Factory.Case_Statement
        (Nodes (1),
         Nodes (2),
         Nodes (3),
         List,
         Nodes (6),
         Nodes (7),
         Nodes (8));
   end;

      when 186 =>

   declare
      List : Node :=
        Self.Factory.Case_Path_Sequence;
   begin
      Self.Factory.Prepend_Case_Path
        (List, Nodes (4));
      Nodes (1) := Self.Factory.Case_Statement
        (Nodes (1),
         Nodes (2),
         Nodes (3),
         List,
         Nodes (5),
         Nodes (6),
         Nodes (7));
   end;

      when 187 =>

   Nodes (1) :=
      Self.Factory.Case_Path
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4));

      when 188 =>

  declare
     List : Node :=
       Nodes (1);
  begin
     Self.Factory.Append_Case_Path
        (List, Nodes (2));
     Nodes (1) := List;
  end;

      when 189 =>

  declare
     List : Node :=
       Self.Factory.Case_Path_Sequence;
  begin
     Self.Factory.Append_Case_Path
        (List, Nodes (1));
     Nodes (1) := List;
  end;

      when 190 =>

   declare
      List : Node :=
        Nodes (1);
   begin
      Self.Factory.Append_Discrete_Choice
        (List, Nodes (3));
      Nodes (1) := List;
   end;

      when 191 =>

   declare
      List : Node :=
        Self.Factory.Discrete_Choice_Sequence;
   begin
      Self.Factory.Append_Discrete_Choice
        (List, Nodes (2));
      Nodes (1) := List;
   end;

      when 192 =>

   Nodes (1) :=
      Self.Factory.Choice_Parameter_Specification
         (Nodes (1));

      when 193 =>
         null;
      when 194 =>
         null;
      when 195 =>

   declare
      List : Node := Nodes (1);
   begin
      Self.Factory.Append_Compilation_Unit
        (List, Nodes (2));
      Nodes (1) := List;
   end;

      when 196 =>

   declare
      List : Node := Self.
        Factory.Compilation_Unit_Sequence;
   begin
      Self.Factory.Append_Compilation_Unit
        (List, Nodes (1));
      Nodes (1) := List;
   end;

      when 197 =>

   declare
      List : Node :=
          Nodes (1);
   begin
      Self.Factory.Append_Association
        (List, Nodes (3));
      Nodes (1) := List;
   end;

      when 198 =>

   declare
      List : Node :=
          Self.Factory.Association_Sequence;
   begin
      Self.Factory.Append_Association
        (List, Nodes (2));
      Nodes (1) := List;
   end;

      when 199 =>

   Nodes (1) := Self.Factory.
     Component_Clause
       (Nodes (1),
        Nodes (2),
        Nodes (3),
        Nodes (4),
        Nodes (5),
        Nodes (6));

      when 200 =>
         null;
      when 201 =>

   declare
      List : Node :=
        Nodes (1);
   begin
      Self.Factory.Append_Clause_Or_Pragma
        (List, Nodes (2));
      Nodes (1) := List;
   end;

      when 202 =>

   declare
      List : Node :=
        Self.Factory.Clause_Or_Pragma_Sequence;
   begin
      Self.Factory.Append_Clause_Or_Pragma
        (List, Nodes (1));
      Nodes (1) := List;
   end;

      when 203 =>

   Nodes (1) := Self.Factory.
     Component_Declaration
       (Nodes (1),
        Nodes (2),
        Nodes (3),
        Nodes (4),
        Nodes (5),
        Nodes (6),
        Nodes (7));

      when 204 =>

   Nodes (1) := Self.Factory.
     Component_Declaration
       (Nodes (1),
        Nodes (2),
        Nodes (3),
        Nodes (4),
        Nodes (5),
        (Self.Factory.Aspect_Specification_Sequence),
        Nodes (6));

      when 205 =>

   Nodes (1) := Self.Factory.
     Component_Declaration
       (Nodes (1),
        Nodes (2),
        Nodes (3),
        No_Token,
        None,
        Nodes (4),
        Nodes (5));

      when 206 =>

   Nodes (1) := Self.Factory.
     Component_Declaration
       (Nodes (1),
        Nodes (2),
        Nodes (3),
        No_Token,
        None,
        (Self.Factory.Aspect_Specification_Sequence),
        Nodes (4));

      when 207 =>

   Nodes (1) := Self.Factory.Component_Definition
       (Nodes (1),
        Nodes (2));

      when 208 =>

   Nodes (1) := Self.Factory.Component_Definition
       (No_Token,
        Nodes (1));

      when 209 =>

   Nodes (1) := Self.Factory.Component_Definition
       (Nodes (1),
        Nodes (2));

      when 210 =>

   Nodes (1) := Self.Factory.Component_Definition
       (No_Token,
        Nodes (1));

      when 211 =>
         null;
      when 212 =>
         null;
      when 213 =>

   declare
      List : Node :=
        Nodes (1);
   begin
      Self.Factory.Append_Component_Item
        (List, Nodes (2));
      Nodes (1) := List;
   end;

      when 214 =>

   declare
      List : Node :=
        Self.Factory.Component_Item_Sequence;
   begin
      Self.Factory.Append_Component_Item
        (List, Nodes (1));
      Nodes (1) := List;
   end;

      when 215 =>

  declare
     List : Node :=
       Nodes (2);
  begin
      Self.Factory.Prepend_Component_Item
        (List, Nodes (1));
      Self.Factory.Append_Component_Item
        (List, Nodes (3));
      Nodes (1) := List;
  end;

      when 216 =>

   declare
      List : Node :=
        Nodes (2);
   begin
      Self.Factory.Prepend_Component_Item
        (List, Nodes (1));
      Nodes (1) := List;
   end;

      when 217 =>

  declare
     List : Node :=
       Self.Factory.
       Component_Item_Sequence;
  begin
      Self.Factory.Prepend_Component_Item
        (List, Nodes (1));
      Self.Factory.Append_Component_Item
        (List, Nodes (2));
      Nodes (1) := List;
  end;

      when 218 =>

   declare
      List : Node :=
        Self.Factory.
        Component_Item_Sequence;
   begin
      Self.Factory.Prepend_Component_Item
        (List, Nodes (1));
      Nodes (1) := List;
   end;

      when 219 =>

   declare
      List : Node :=
        Self.Factory.Component_Item_Sequence;
   begin
      Self.Factory.Prepend_Component_Item
        (List, Nodes (1));
      Nodes (1) := List;
   end;

      when 220 =>

   declare
      Comp : constant Node :=
        Self.Factory.Null_Component
          (Nodes (1), Nodes (2));
      List : Node :=
        Self.Factory.Component_Item_Sequence;
   begin
      Self.Factory.Prepend_Component_Item
        (List, Comp);
      Nodes (1) := List;
   end;

      when 221 =>
         null;
      when 222 =>
         null;
      when 223 =>

   declare
      List : Node :=
        Nodes (4);
   begin
      Self.Factory.Prepend_Discrete_Subtype_Definition
        (List, Nodes (3));
      Nodes (1) := Self.Factory.
        Constrained_Array_Definition
          (Nodes (1),
           Nodes (2),
           List,
           Nodes (5),
           Nodes (6),
           Nodes (7));
   end;

      when 224 =>

   declare
      List : Node :=
        Self.Factory.
        Discrete_Subtype_Definition_Sequence;
   begin
      Self.Factory.Prepend_Discrete_Subtype_Definition
        (List, Nodes (3));
      Nodes (1) := Self.Factory.
        Constrained_Array_Definition
          (Nodes (1),
           Nodes (2),
           List,
           Nodes (4),
           Nodes (5),
           Nodes (6));
   end;

      when 225 =>
         null;
      when 226 =>
         null;
      when 227 =>
         null;
      when 228 =>

   declare
      List : Node := Nodes (1);
   begin
      Self.Factory.Append_Context_Item
        (List, Nodes (2));
      Nodes (1) := List;
   end;

      when 229 =>

   declare
      List : Node := Self.
        Factory.Context_Item_Sequence;
   begin
      Self.Factory.Append_Context_Item
        (List, Nodes (1));
      Nodes (1) := List;
   end;

      when 230 =>
  Nodes (1) :=
     Self.Factory.Decimal_Fixed_Point_Definition
      (Nodes (1),
       Nodes (2),
       Nodes (3),
       Nodes (4),
       Nodes (5));

      when 231 =>
  Nodes (1) :=
     Self.Factory.Decimal_Fixed_Point_Definition
      (Nodes (1),
       Nodes (2),
       Nodes (3),
       Nodes (4),
       None);

      when 232 =>
         null;
      when 233 =>
         null;
      when 234 =>

   declare
      List : Node := Nodes (1);
   begin
      Self.Factory.Append_Declarative_Item
        (List, Nodes (2));
      Nodes (1) := List;
   end;

      when 235 =>

   declare
      List : Node := Self.
        Factory.Declarative_Item_Sequence;
   begin
      Self.Factory.Append_Declarative_Item
        (List, Nodes (1));
      Nodes (1) := List;
   end;

      when 236 =>

   declare
      List : Node :=
       Nodes (2);
   begin
      Self.Factory.Prepend_Declarative_Item
        (List, Nodes (1));
      Nodes (1) := List;
   end;

      when 237 =>

   declare
      List : Node :=
       Self.
         Factory.Declarative_Item_Sequence;
   begin
      Self.Factory.Prepend_Declarative_Item
        (List, Nodes (1));
      Nodes (1) := List;
   end;

      when 238 =>
  Nodes (1) := Self.Factory.Defining_Character_Literal
     (Nodes (1));

      when 239 =>
         null;
      when 240 =>
  Nodes (1) := Self.Factory.Defining_Operator_Symbol
     (Nodes (1));

      when 241 =>
  Nodes (1) :=
     Self.Factory.Defining_Enumeration_Literal (Nodes (1));

      when 242 =>

   declare
      List : Node :=
        Nodes (1);
   begin
      Self.Factory.Append_Defining_Identifier
        (List, Nodes (3));
      Nodes (1) := List;
   end;

      when 243 =>

   declare
      List : Node :=
        Self.Factory.Defining_Identifier_Sequence;
   begin
      Self.Factory.Append_Defining_Identifier
        (List, Nodes (2));
      Nodes (1) := List;
   end;

      when 244 =>
  Nodes (1) := Self.Factory.Defining_Identifier
     (Nodes (1));

      when 245 =>

   declare
      List : Node :=
        Nodes (2);
   begin
      Self.Factory.Prepend_Defining_Identifier
        (List, Nodes (1));
      Nodes (1) := List;
   end;

      when 246 =>

   declare
      List : Node :=
        Self.Factory.Defining_Identifier_Sequence;
   begin
      Self.Factory.Prepend_Defining_Identifier
        (List, Nodes (1));
      Nodes (1) := List;
   end;

      when 247 =>
  Nodes (1) := Self.Factory.Defining_Identifier
     (Nodes (1));

      when 248 =>

   Nodes (1) := Self.Factory.To_Defining_Program_Unit_Name
     (Nodes (1));

      when 249 =>

   declare
      List : Node :=
        Nodes (2);
   begin
      Self.Factory.Prepend_Statement
        (List, Nodes (1));
      Nodes (1) := List;
   end;

      when 250 =>

   declare
      List : Node :=
        Self.Factory.Statement_Sequence;
   begin
      Self.Factory.Prepend_Statement
        (List, Nodes (1));
      Nodes (1) := List;
   end;

      when 251 =>

   Nodes (1) :=
      Self.Factory.Delay_Statement
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4));

      when 252 =>

   Nodes (1) :=
      Self.Factory.Delay_Statement
         (Nodes (1),
          No_Token,
          Nodes (2),
          Nodes (3));

      when 253 =>
  Nodes (1) := Self.Factory.Delta_Constraint
     (Nodes (1), Nodes (2), Nodes (3));

      when 254 =>
  Nodes (1) := Self.Factory.Delta_Constraint
     (Nodes (1), Nodes (2), None);

      when 255 =>
         null;
      when 256 =>
         null;
      when 257 =>
  Nodes (1) := Self.Factory.Digits_Constraint
     (Nodes (1), Nodes (2), Nodes (3));

      when 258 =>
  Nodes (1) := Self.Factory.Digits_Constraint
     (Nodes (1), Nodes (2), None);

      when 259 =>
         null;
      when 260 =>
         null;
      when 261 =>
         null;
      when 262 =>
         null;
      when 263 =>
         null;
      when 264 =>

   declare
      List : Node :=
        Nodes (2);
   begin
      Self.Factory.Prepend_Discrete_Choice
        (List, Nodes (1));
      Nodes (1) := List;
   end;

      when 265 =>

   declare
      List : Node :=
        Self.Factory.Discrete_Choice_Sequence;
   begin
      Self.Factory.Prepend_Discrete_Choice
        (List, Nodes (1));
      Nodes (1) := List;
   end;

      when 266 =>
   Nodes (1) := Self.Factory.Discrete_Subtype_Indication_Dr
     (Nodes (1), Nodes (2));

      when 267 =>
   Nodes (1) := Self.Factory.Range_Attribute_Reference_Dr
     (Nodes (1));

      when 268 =>
   Nodes (1) := Self.Factory.Simple_Expression_Range_Dr
     (Nodes (1), Nodes (2), Nodes (3));

      when 269 =>
   Nodes (1) := Self.Factory.Discrete_Simple_Expression_Range
     (Nodes (1), Nodes (2), Nodes (3));

      when 270 =>
   Nodes (1) := Self.Factory.Discrete_Subtype_Indication
     (Nodes (1), Nodes (2));

      when 271 =>
   Nodes (1) := Self.Factory.Discrete_Subtype_Indication
     (Nodes (1), None);

      when 272 =>
  Nodes (1) := Self.Factory.Discrete_Range_Attribute_Reference
     (Nodes (1));

      when 273 =>
         null;
      when 274 =>

   declare
      List : Node := Nodes (1);
   begin
      Self.Factory.Append_Discrete_Subtype_Definition
        (List, Nodes (3));
      Nodes (1) := List;
   end;

      when 275 =>

   declare
      List : Node := Self.
        Factory.Discrete_Subtype_Definition_Sequence;
   begin
      Self.Factory.Append_Discrete_Subtype_Definition
        (List, Nodes (2));
      Nodes (1) := List;
   end;

      when 276 =>
         null;
      when 277 =>
         null;
      when 278 =>

   declare
      List : Node := Nodes (1);
   begin
      Self.Factory.Append_Discriminant_Specification
        (List, Nodes (3));
      Nodes (1) := List;
   end;

      when 279 =>

   declare
      List : Node := Self.
        Factory.Discriminant_Specification_Sequence;
   begin
      Self.Factory.Append_Discriminant_Specification
        (List, Nodes (2));
      Nodes (1) := List;
   end;

      when 280 =>

   Nodes (1) :=
      Self.Factory.Discriminant_Specification
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7));

      when 281 =>

   Nodes (1) :=
      Self.Factory.Discriminant_Specification
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          No_Token,
          None);

      when 282 =>

   Nodes (1) :=
      Self.Factory.Discriminant_Specification
         (Nodes (1),
          Nodes (2),
          No_Token,
          No_Token,
          Nodes (3),
          Nodes (4),
          Nodes (5));

      when 283 =>

   Nodes (1) :=
      Self.Factory.Discriminant_Specification
         (Nodes (1),
          Nodes (2),
          No_Token,
          No_Token,
          Nodes (3),
          No_Token,
          None);

      when 284 =>

   Nodes (1) :=
      Self.Factory.Discriminant_Specification
         (Nodes (1),
          Nodes (2),
          No_Token,
          No_Token,
          Nodes (3),
          Nodes (4),
          Nodes (5));

      when 285 =>

   Nodes (1) :=
      Self.Factory.Discriminant_Specification
         (Nodes (1),
          Nodes (2),
          No_Token,
          No_Token,
          Nodes (3),
          No_Token,
          None);

      when 286 =>

   declare
      Path : constant Node :=
        Self.Factory.Elsif_Expression_Path
          (Nodes (2),
           Nodes (3),
           Nodes (4),
           Nodes (5));
      List : Node :=
        Nodes (1);
   begin
      Self.Factory.Append_If_Else_Expression_Path
        (List, Path);
      Nodes (1) := List;
   end;

      when 287 =>

   declare
      Path : constant Node :=
        Self.Factory.Elsif_Expression_Path
          (Nodes (1),
           Nodes (2),
           Nodes (3),
           Nodes (4));
      List : Node :=
        Self.Factory.If_Else_Expression_Path_Sequence;
   begin
      Self.Factory.Append_If_Else_Expression_Path
        (List, Path);
      Nodes (1) := List;
   end;

      when 288 =>

   declare
      Item : constant Node :=
        Self.Factory.Elsif_Path
          (Nodes (2),
           Nodes (3),
           Nodes (4),
           Nodes (5));
      List : Node :=
        Nodes (1);
   begin
      Self.Factory.Append_If_Elsif_Else_Path
        (List, Item);
      Nodes (1) := List;
   end;

      when 289 =>

   declare
      Item : constant Node :=
        Self.Factory.Elsif_Path
          (Nodes (1),
           Nodes (2),
           Nodes (3),
           Nodes (4));
      List : Node :=
        Self.Factory.If_Elsif_Else_Path_Sequence;
   begin
      Self.Factory.Append_If_Elsif_Else_Path
        (List, Item);
      Nodes (1) := List;
   end;

      when 290 =>

   Nodes (1) :=
      Self.Factory.Entry_Body
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
          Nodes (13),
          Nodes (14),
          Nodes (15),
          Nodes (16),
          Nodes (17),
          Nodes (18),
          Nodes (19));

      when 291 =>

   Nodes (1) :=
      Self.Factory.Entry_Body
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
          Nodes (13),
          Nodes (14),
          Nodes (15),
          Nodes (16),
          Nodes (17),
          No_Token,
          Nodes (18));

      when 292 =>

   Nodes (1) :=
      Self.Factory.Entry_Body
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
          Nodes (13),
          Nodes (14),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (15),
          Nodes (16),
          Nodes (17));

      when 293 =>

   Nodes (1) :=
      Self.Factory.Entry_Body
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
          Nodes (13),
          Nodes (14),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (15),
          No_Token,
          Nodes (16));

      when 294 =>

   Nodes (1) :=
      Self.Factory.Entry_Body
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
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (12),
          Nodes (13),
          Nodes (14),
          Nodes (15),
          Nodes (16),
          Nodes (17),
          Nodes (18));

      when 295 =>

   Nodes (1) :=
      Self.Factory.Entry_Body
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
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (12),
          Nodes (13),
          Nodes (14),
          Nodes (15),
          Nodes (16),
          No_Token,
          Nodes (17));

      when 296 =>

   Nodes (1) :=
      Self.Factory.Entry_Body
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
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (12),
          Nodes (13),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (14),
          Nodes (15),
          Nodes (16));

      when 297 =>

   Nodes (1) :=
      Self.Factory.Entry_Body
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
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (12),
          Nodes (13),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (14),
          No_Token,
          Nodes (15));

      when 298 =>

   Nodes (1) :=
      Self.Factory.Entry_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (6),
          Nodes (7),
          Nodes (8),
          Nodes (9),
          Nodes (10),
          Nodes (11),
          Nodes (12),
          Nodes (13),
          Nodes (14),
          Nodes (15),
          Nodes (16));

      when 299 =>

   Nodes (1) :=
      Self.Factory.Entry_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (6),
          Nodes (7),
          Nodes (8),
          Nodes (9),
          Nodes (10),
          Nodes (11),
          Nodes (12),
          Nodes (13),
          Nodes (14),
          No_Token,
          Nodes (15));

      when 300 =>

   Nodes (1) :=
      Self.Factory.Entry_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (6),
          Nodes (7),
          Nodes (8),
          Nodes (9),
          Nodes (10),
          Nodes (11),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (12),
          Nodes (13),
          Nodes (14));

      when 301 =>

   Nodes (1) :=
      Self.Factory.Entry_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (6),
          Nodes (7),
          Nodes (8),
          Nodes (9),
          Nodes (10),
          Nodes (11),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (12),
          No_Token,
          Nodes (13));

      when 302 =>

   Nodes (1) :=
      Self.Factory.Entry_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (6),
          Nodes (7),
          Nodes (8),
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (9),
          Nodes (10),
          Nodes (11),
          Nodes (12),
          Nodes (13),
          Nodes (14),
          Nodes (15));

      when 303 =>

   Nodes (1) :=
      Self.Factory.Entry_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (6),
          Nodes (7),
          Nodes (8),
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (9),
          Nodes (10),
          Nodes (11),
          Nodes (12),
          Nodes (13),
          No_Token,
          Nodes (14));

      when 304 =>

   Nodes (1) :=
      Self.Factory.Entry_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (6),
          Nodes (7),
          Nodes (8),
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (9),
          Nodes (10),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (11),
          Nodes (12),
          Nodes (13));

      when 305 =>

   Nodes (1) :=
      Self.Factory.Entry_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (6),
          Nodes (7),
          Nodes (8),
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (9),
          Nodes (10),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (11),
          No_Token,
          Nodes (12));

      when 306 =>

   Nodes (1) :=
      Self.Factory.Entry_Body
         (Nodes (1),
          Nodes (2),
          No_Token,
          None,
          No_Token,
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
          Nodes (13),
          Nodes (14),
          Nodes (15),
          Nodes (16));

      when 307 =>

   Nodes (1) :=
      Self.Factory.Entry_Body
         (Nodes (1),
          Nodes (2),
          No_Token,
          None,
          No_Token,
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
          Nodes (13),
          Nodes (14),
          No_Token,
          Nodes (15));

      when 308 =>

   Nodes (1) :=
      Self.Factory.Entry_Body
         (Nodes (1),
          Nodes (2),
          No_Token,
          None,
          No_Token,
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
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (12),
          Nodes (13),
          Nodes (14));

      when 309 =>

   Nodes (1) :=
      Self.Factory.Entry_Body
         (Nodes (1),
          Nodes (2),
          No_Token,
          None,
          No_Token,
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
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (12),
          No_Token,
          Nodes (13));

      when 310 =>

   Nodes (1) :=
      Self.Factory.Entry_Body
         (Nodes (1),
          Nodes (2),
          No_Token,
          None,
          No_Token,
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (9),
          Nodes (10),
          Nodes (11),
          Nodes (12),
          Nodes (13),
          Nodes (14),
          Nodes (15));

      when 311 =>

   Nodes (1) :=
      Self.Factory.Entry_Body
         (Nodes (1),
          Nodes (2),
          No_Token,
          None,
          No_Token,
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (9),
          Nodes (10),
          Nodes (11),
          Nodes (12),
          Nodes (13),
          No_Token,
          Nodes (14));

      when 312 =>

   Nodes (1) :=
      Self.Factory.Entry_Body
         (Nodes (1),
          Nodes (2),
          No_Token,
          None,
          No_Token,
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (9),
          Nodes (10),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (11),
          Nodes (12),
          Nodes (13));

      when 313 =>

   Nodes (1) :=
      Self.Factory.Entry_Body
         (Nodes (1),
          Nodes (2),
          No_Token,
          None,
          No_Token,
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (9),
          Nodes (10),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (11),
          No_Token,
          Nodes (12));

      when 314 =>

   Nodes (1) :=
      Self.Factory.Entry_Body
         (Nodes (1),
          Nodes (2),
          No_Token,
          None,
          No_Token,
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
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

      when 315 =>

   Nodes (1) :=
      Self.Factory.Entry_Body
         (Nodes (1),
          Nodes (2),
          No_Token,
          None,
          No_Token,
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
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

      when 316 =>

   Nodes (1) :=
      Self.Factory.Entry_Body
         (Nodes (1),
          Nodes (2),
          No_Token,
          None,
          No_Token,
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
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

      when 317 =>

   Nodes (1) :=
      Self.Factory.Entry_Body
         (Nodes (1),
          Nodes (2),
          No_Token,
          None,
          No_Token,
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
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

      when 318 =>

   Nodes (1) :=
      Self.Factory.Entry_Body
         (Nodes (1),
          Nodes (2),
          No_Token,
          None,
          No_Token,
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
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

      when 319 =>

   Nodes (1) :=
      Self.Factory.Entry_Body
         (Nodes (1),
          Nodes (2),
          No_Token,
          None,
          No_Token,
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
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

      when 320 =>

   Nodes (1) :=
      Self.Factory.Entry_Body
         (Nodes (1),
          Nodes (2),
          No_Token,
          None,
          No_Token,
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
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

      when 321 =>

   Nodes (1) :=
      Self.Factory.Entry_Body
         (Nodes (1),
          Nodes (2),
          No_Token,
          None,
          No_Token,
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
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

      when 322 =>

   declare
      List : Node :=
        Nodes (2);
   begin
      Self.Factory.Prepend_Statement
        (List, Nodes (1));
      Nodes (1) := List;
   end;

      when 323 =>

   declare
      List : Node :=
        Self.Factory.Statement_Sequence;
   begin
      Self.Factory.Prepend_Statement
        (List, Nodes (1));
      Nodes (1) := List;
   end;

      when 324 =>

   Nodes (1) :=
      Self.Factory.Procedure_Call_Statement
         (Nodes (1), Nodes (2));

      when 325 =>

   Nodes (1) :=
      Self.Factory.Entry_Declaration
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

      when 326 =>

   Nodes (1) :=
      Self.Factory.Entry_Declaration
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
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (11));

      when 327 =>

   Nodes (1) :=
      Self.Factory.Entry_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (8),
          Nodes (9));

      when 328 =>

   Nodes (1) :=
      Self.Factory.Entry_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (8));

      when 329 =>

   Nodes (1) :=
      Self.Factory.Entry_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token,
          None,
          No_Token,
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          Nodes (9));

      when 330 =>

   Nodes (1) :=
      Self.Factory.Entry_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token,
          None,
          No_Token,
          Nodes (5),
          Nodes (6),
          Nodes (7),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (8));

      when 331 =>

   Nodes (1) :=
      Self.Factory.Entry_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token,
          None,
          No_Token,
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (5),
          Nodes (6));

      when 332 =>

   Nodes (1) :=
      Self.Factory.Entry_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token,
          None,
          No_Token,
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (5));

      when 333 =>

   Nodes (1) :=
      Self.Factory.Entry_Declaration
         (No_Token,
          Nodes (1),
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

      when 334 =>

   Nodes (1) :=
      Self.Factory.Entry_Declaration
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          Nodes (9),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (10));

      when 335 =>

   Nodes (1) :=
      Self.Factory.Entry_Declaration
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (7),
          Nodes (8));

      when 336 =>

   Nodes (1) :=
      Self.Factory.Entry_Declaration
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (7));

      when 337 =>

   Nodes (1) :=
      Self.Factory.Entry_Declaration
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          None,
          No_Token,
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8));

      when 338 =>

   Nodes (1) :=
      Self.Factory.Entry_Declaration
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          None,
          No_Token,
          Nodes (4),
          Nodes (5),
          Nodes (6),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (7));

      when 339 =>

   Nodes (1) :=
      Self.Factory.Entry_Declaration
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          None,
          No_Token,
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (4),
          Nodes (5));

      when 340 =>

   Nodes (1) :=
      Self.Factory.Entry_Declaration
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          None,
          No_Token,
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (4));

      when 341 =>

   Nodes (1) :=
      Self.Factory.Entry_Declaration
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          Nodes (9),
          Nodes (10));

      when 342 =>

   Nodes (1) :=
      Self.Factory.Entry_Declaration
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (9));

      when 343 =>

   Nodes (1) :=
      Self.Factory.Entry_Declaration
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (6),
          Nodes (7));

      when 344 =>

   Nodes (1) :=
      Self.Factory.Entry_Declaration
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (6));

      when 345 =>

   Nodes (1) :=
      Self.Factory.Entry_Declaration
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          No_Token,
          None,
          No_Token,
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7));

      when 346 =>

   Nodes (1) :=
      Self.Factory.Entry_Declaration
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          No_Token,
          None,
          No_Token,
          Nodes (3),
          Nodes (4),
          Nodes (5),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (6));

      when 347 =>

   Nodes (1) :=
      Self.Factory.Entry_Declaration
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          No_Token,
          None,
          No_Token,
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (3),
          Nodes (4));

      when 348 =>

   Nodes (1) :=
      Self.Factory.Entry_Declaration
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          No_Token,
          None,
          No_Token,
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (3));

      when 349 =>

   Nodes (1) :=
      Self.Factory.Entry_Index_Specification
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4));

      when 350 =>

   declare
      List : Node := Nodes (1);
   begin
      Self.Factory.Append_Enumeration_Literal_Specification
        (List, Nodes (3));
      Nodes (1) := List;
   end;

      when 351 =>

   declare
      List : Node := Self.
        Factory.Enumeration_Literal_Specification_Sequence;
   begin
      Self.Factory.Append_Enumeration_Literal_Specification
        (List, Nodes (2));
      Nodes (1) := List;
   end;

      when 352 =>
   Nodes (1) := Self.Factory.Enumeration_Literal_Specification
     (Nodes (1));

      when 353 =>
  Nodes (1) := Self.Factory.Enumeration_Literal_Specification
     (Nodes (1));

      when 354 =>

   declare
      List : Node :=
        Nodes (3);
   begin
      Self.Factory.Prepend_Enumeration_Literal_Specification
        (List, Nodes (2));
      Nodes (1) := Self.Factory.
        Enumeration_Type_Definition
          (Nodes (1),
           List,
           Nodes (4));
   end;

      when 355 =>

   declare
      List : Node :=
        Self.
        Factory.Enumeration_Literal_Specification_Sequence;
   begin
      Self.Factory.Prepend_Enumeration_Literal_Specification
        (List, Nodes (2));
      Nodes (1) := Self.Factory.
        Enumeration_Type_Definition
          (Nodes (1),
           List,
           Nodes (3));
   end;

      when 356 =>
         null;
      when 357 =>
         null;
      when 358 =>

   declare
      List : Node := Nodes (1);
   begin
      Self.Factory.Append_Exception_Choice
        (List, Nodes (3));
      Nodes (1) := List;
   end;

      when 359 =>

   declare
      List : Node := Self.
        Factory.Exception_Choice_Sequence;
   begin
      Self.Factory.Append_Exception_Choice
        (List, Nodes (2));
      Nodes (1) := List;
   end;

      when 360 =>

   Nodes (1) :=
      Self.Factory.Exception_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5));

      when 361 =>

   Nodes (1) :=
      Self.Factory.Exception_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (4));

      when 362 =>

   declare
      List : Node :=
        Nodes (5);
   begin
      Self.Factory.Prepend_Exception_Choice
        (List, Nodes (4));
      Nodes (1) :=
        Self.Factory.Exception_Handler
          (Nodes (1),
           Nodes (2),
           Nodes (3),
           List,
           Nodes (6),
           Nodes (7));
   end;

      when 363 =>

   declare
      List : Node :=
        Self.
          Factory.Exception_Choice_Sequence;
   begin
      Self.Factory.Prepend_Exception_Choice
        (List, Nodes (4));
      Nodes (1) :=
        Self.Factory.Exception_Handler
          (Nodes (1),
           Nodes (2),
           Nodes (3),
           List,
           Nodes (5),
           Nodes (6));
   end;

      when 364 =>

   declare
      List : Node :=
        Nodes (3);
   begin
      Self.Factory.Prepend_Exception_Choice
        (List, Nodes (2));
      Nodes (1) :=
        Self.Factory.Exception_Handler
          (Nodes (1),
           None,
           No_Token,
           List,
           Nodes (4),
           Nodes (5));
   end;

      when 365 =>

   declare
      List : Node :=
        Self.
          Factory.Exception_Choice_Sequence;
   begin
      Self.Factory.Prepend_Exception_Choice
        (List, Nodes (2));
      Nodes (1) :=
        Self.Factory.Exception_Handler
          (Nodes (1),
           None,
           No_Token,
           List,
           Nodes (3),
           Nodes (4));
   end;

      when 366 =>

   declare
      List : Node := Nodes (1);
   begin
      Self.Factory.Append_Exception_Handler
        (List, Nodes (2));
      Nodes (1) := List;
   end;

      when 367 =>

   declare
      List : Node := Self.
        Factory.Exception_Handler_Sequence;
   begin
      Self.Factory.Append_Exception_Handler
        (List, Nodes (1));
      Nodes (1) := List;
   end;

      when 368 =>

   Nodes (1) :=
      Self.Factory.Exception_Renaming_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7));

      when 369 =>

   Nodes (1) :=
      Self.Factory.Exception_Renaming_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (6));

      when 370 =>

   Nodes (1) :=
      Self.Factory.Exit_Statement
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5));

      when 371 =>

   Nodes (1) :=
      Self.Factory.Exit_Statement
         (Nodes (1),
          Nodes (2),
          No_Token,
          None,
          Nodes (3));

      when 372 =>

   Nodes (1) :=
      Self.Factory.Exit_Statement
         (Nodes (1),
          None,
          Nodes (2),
          Nodes (3),
          Nodes (4));

      when 373 =>

   Nodes (1) :=
      Self.Factory.Exit_Statement
         (Nodes (1),
          None,
          No_Token,
          None,
          Nodes (2));

      when 374 =>
  Nodes (1) := Self.Factory.Explicit_Dereference
     (Nodes (1), Nodes (2), Nodes (3));

      when 375 =>
         null;
      when 376 =>
  Nodes (1) := Self.Factory.Infix_Call
     (Nodes (2), Nodes (1), Nodes (3));

      when 377 =>
  Nodes (1) := Self.Factory.Short_Circuit
     (Nodes (1), Nodes (2), Nodes (3), Nodes (4));

      when 378 =>
  Nodes (1) := Self.Factory.Infix_Call
     (Nodes (2), Nodes (1), Nodes (3));

      when 379 =>
  Nodes (1) := Self.Factory.Short_Circuit
     (Nodes (1), Nodes (2), Nodes (3), Nodes (4));

      when 380 =>
  Nodes (1) := Self.Factory.Infix_Call
     (Nodes (2), Nodes (1), Nodes (3));

      when 381 =>

   Nodes (1) :=
      Self.Factory.Return_Object_Specification
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7));

      when 382 =>

   Nodes (1) :=
      Self.Factory.Return_Object_Specification
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          No_Token,
          None);

      when 383 =>

   Nodes (1) :=
      Self.Factory.Return_Object_Specification
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          Nodes (4),
          Nodes (5),
          Nodes (6));

      when 384 =>

   Nodes (1) :=
      Self.Factory.Return_Object_Specification
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          Nodes (4),
          No_Token,
          None);

      when 385 =>

   Nodes (1) :=
      Self.Factory.Return_Object_Specification
         (Nodes (1),
          Nodes (2),
          No_Token,
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6));

      when 386 =>

   Nodes (1) :=
      Self.Factory.Return_Object_Specification
         (Nodes (1),
          Nodes (2),
          No_Token,
          Nodes (3),
          Nodes (4),
          No_Token,
          None);

      when 387 =>

   Nodes (1) :=
      Self.Factory.Return_Object_Specification
         (Nodes (1),
          Nodes (2),
          No_Token,
          No_Token,
          Nodes (3),
          Nodes (4),
          Nodes (5));

      when 388 =>

   Nodes (1) :=
      Self.Factory.Return_Object_Specification
         (Nodes (1),
          Nodes (2),
          No_Token,
          No_Token,
          Nodes (3),
          No_Token,
          None);

      when 389 =>

   Nodes (1) :=
      Self.Factory.Extended_Return_Statement
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          Nodes (9));

      when 390 =>

   Nodes (1) :=
      Self.Factory.Extended_Return_Statement
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (5),
          Nodes (6),
          Nodes (7));

      when 391 =>

   Nodes (1) :=
      Self.Factory.Extended_Return_Statement
         (Nodes (1),
          Nodes (2),
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          No_Token,
          No_Token,
          Nodes (3));

      when 392 =>

   Nodes (1) :=
     Self.Factory.Extension_Aggregate
       (Nodes (1),
        Nodes (2),
        Nodes (3),
        Nodes (4),
        Nodes (5));

      when 393 =>
  Nodes (1) := Self.Factory.Floating_Point_Definition
      (Nodes (1),
       Nodes (2),
       Nodes (3));

      when 394 =>
  Nodes (1) := Self.Factory.Floating_Point_Definition
      (Nodes (1),
       Nodes (2),
       None);

      when 395 =>

   Nodes (1) := Self.Factory.
     Formal_Access_To_Object_Definition
       (Nodes (1),
        Nodes (2),
        Nodes (3),
        Nodes (4),
        Nodes (5));

      when 396 =>

   Nodes (1) := Self.Factory.
     Formal_Access_To_Object_Definition
       (Nodes (1),
        Nodes (2),
        Nodes (3),
        No_Token,
        Nodes (4));

      when 397 =>

   Nodes (1) := Self.Factory.
     Formal_Access_To_Object_Definition
       (No_Token,
        No_Token,
        Nodes (1),
        Nodes (2),
        Nodes (3));

      when 398 =>

   Nodes (1) := Self.Factory.
     Formal_Access_To_Object_Definition
       (No_Token,
        No_Token,
        Nodes (1),
        No_Token,
        Nodes (2));

      when 399 =>

   Nodes (1) := Self.Factory.
     Formal_Access_To_Procedure_Definition
       (Nodes (1),
        Nodes (2),
        Nodes (3),
        Nodes (4),
        Nodes (5),
        Nodes (6),
        Nodes (7),
        Nodes (8));

      when 400 =>

   Nodes (1) := Self.Factory.
     Formal_Access_To_Procedure_Definition
       (Nodes (1),
        Nodes (2),
        Nodes (3),
        Nodes (4),
        Nodes (5),
        No_Token,
        (Self.Factory.Parameter_Specification_Sequence),
        No_Token);

      when 401 =>

   Nodes (1) := Self.Factory.
     Formal_Access_To_Procedure_Definition
       (Nodes (1),
        Nodes (2),
        Nodes (3),
        No_Token,
        Nodes (4),
        Nodes (5),
        Nodes (6),
        Nodes (7));

      when 402 =>

   Nodes (1) := Self.Factory.
     Formal_Access_To_Procedure_Definition
       (Nodes (1),
        Nodes (2),
        Nodes (3),
        No_Token,
        Nodes (4),
        No_Token,
        (Self.Factory.Parameter_Specification_Sequence),
        No_Token);

      when 403 =>

   Nodes (1) := Self.Factory.
     Formal_Access_To_Procedure_Definition
       (No_Token,
        No_Token,
        Nodes (1),
        Nodes (2),
        Nodes (3),
        Nodes (4),
        Nodes (5),
        Nodes (6));

      when 404 =>

   Nodes (1) := Self.Factory.
     Formal_Access_To_Procedure_Definition
       (No_Token,
        No_Token,
        Nodes (1),
        Nodes (2),
        Nodes (3),
        No_Token,
        (Self.Factory.Parameter_Specification_Sequence),
        No_Token);

      when 405 =>

   Nodes (1) := Self.Factory.
     Formal_Access_To_Procedure_Definition
       (No_Token,
        No_Token,
        Nodes (1),
        No_Token,
        Nodes (2),
        Nodes (3),
        Nodes (4),
        Nodes (5));

      when 406 =>

   Nodes (1) := Self.Factory.
     Formal_Access_To_Procedure_Definition
       (No_Token,
        No_Token,
        Nodes (1),
        No_Token,
        Nodes (2),
        No_Token,
        (Self.Factory.Parameter_Specification_Sequence),
        No_Token);

      when 407 =>

   Nodes (1) := Self.Factory.
     Formal_Access_To_Function_Definition
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

      when 408 =>

   Nodes (1) := Self.Factory.
     Formal_Access_To_Function_Definition
       (Nodes (1),
        Nodes (2),
        Nodes (3),
        Nodes (4),
        Nodes (5),
        Nodes (6),
        Nodes (7),
        Nodes (8),
        Nodes (9),
        No_Token,
        No_Token,
        Nodes (10));

      when 409 =>

   Nodes (1) := Self.Factory.
     Formal_Access_To_Function_Definition
       (Nodes (1),
        Nodes (2),
        Nodes (3),
        Nodes (4),
        Nodes (5),
        No_Token,
        (Self.Factory.Parameter_Specification_Sequence),
        No_Token,
        Nodes (6),
        Nodes (7),
        Nodes (8),
        Nodes (9));

      when 410 =>

   Nodes (1) := Self.Factory.
     Formal_Access_To_Function_Definition
       (Nodes (1),
        Nodes (2),
        Nodes (3),
        Nodes (4),
        Nodes (5),
        No_Token,
        (Self.Factory.Parameter_Specification_Sequence),
        No_Token,
        Nodes (6),
        No_Token,
        No_Token,
        Nodes (7));

      when 411 =>

   Nodes (1) := Self.Factory.
     Formal_Access_To_Function_Definition
       (Nodes (1),
        Nodes (2),
        Nodes (3),
        No_Token,
        Nodes (4),
        Nodes (5),
        Nodes (6),
        Nodes (7),
        Nodes (8),
        Nodes (9),
        Nodes (10),
        Nodes (11));

      when 412 =>

   Nodes (1) := Self.Factory.
     Formal_Access_To_Function_Definition
       (Nodes (1),
        Nodes (2),
        Nodes (3),
        No_Token,
        Nodes (4),
        Nodes (5),
        Nodes (6),
        Nodes (7),
        Nodes (8),
        No_Token,
        No_Token,
        Nodes (9));

      when 413 =>

   Nodes (1) := Self.Factory.
     Formal_Access_To_Function_Definition
       (Nodes (1),
        Nodes (2),
        Nodes (3),
        No_Token,
        Nodes (4),
        No_Token,
        (Self.Factory.Parameter_Specification_Sequence),
        No_Token,
        Nodes (5),
        Nodes (6),
        Nodes (7),
        Nodes (8));

      when 414 =>

   Nodes (1) := Self.Factory.
     Formal_Access_To_Function_Definition
       (Nodes (1),
        Nodes (2),
        Nodes (3),
        No_Token,
        Nodes (4),
        No_Token,
        (Self.Factory.Parameter_Specification_Sequence),
        No_Token,
        Nodes (5),
        No_Token,
        No_Token,
        Nodes (6));

      when 415 =>

   Nodes (1) := Self.Factory.
     Formal_Access_To_Function_Definition
       (No_Token,
        No_Token,
        Nodes (1),
        Nodes (2),
        Nodes (3),
        Nodes (4),
        Nodes (5),
        Nodes (6),
        Nodes (7),
        Nodes (8),
        Nodes (9),
        Nodes (10));

      when 416 =>

   Nodes (1) := Self.Factory.
     Formal_Access_To_Function_Definition
       (No_Token,
        No_Token,
        Nodes (1),
        Nodes (2),
        Nodes (3),
        Nodes (4),
        Nodes (5),
        Nodes (6),
        Nodes (7),
        No_Token,
        No_Token,
        Nodes (8));

      when 417 =>

   Nodes (1) := Self.Factory.
     Formal_Access_To_Function_Definition
       (No_Token,
        No_Token,
        Nodes (1),
        Nodes (2),
        Nodes (3),
        No_Token,
        (Self.Factory.Parameter_Specification_Sequence),
        No_Token,
        Nodes (4),
        Nodes (5),
        Nodes (6),
        Nodes (7));

      when 418 =>

   Nodes (1) := Self.Factory.
     Formal_Access_To_Function_Definition
       (No_Token,
        No_Token,
        Nodes (1),
        Nodes (2),
        Nodes (3),
        No_Token,
        (Self.Factory.Parameter_Specification_Sequence),
        No_Token,
        Nodes (4),
        No_Token,
        No_Token,
        Nodes (5));

      when 419 =>

   Nodes (1) := Self.Factory.
     Formal_Access_To_Function_Definition
       (No_Token,
        No_Token,
        Nodes (1),
        No_Token,
        Nodes (2),
        Nodes (3),
        Nodes (4),
        Nodes (5),
        Nodes (6),
        Nodes (7),
        Nodes (8),
        Nodes (9));

      when 420 =>

   Nodes (1) := Self.Factory.
     Formal_Access_To_Function_Definition
       (No_Token,
        No_Token,
        Nodes (1),
        No_Token,
        Nodes (2),
        Nodes (3),
        Nodes (4),
        Nodes (5),
        Nodes (6),
        No_Token,
        No_Token,
        Nodes (7));

      when 421 =>

   Nodes (1) := Self.Factory.
     Formal_Access_To_Function_Definition
       (No_Token,
        No_Token,
        Nodes (1),
        No_Token,
        Nodes (2),
        No_Token,
        (Self.Factory.Parameter_Specification_Sequence),
        No_Token,
        Nodes (3),
        Nodes (4),
        Nodes (5),
        Nodes (6));

      when 422 =>

   Nodes (1) := Self.Factory.
     Formal_Access_To_Function_Definition
       (No_Token,
        No_Token,
        Nodes (1),
        No_Token,
        Nodes (2),
        No_Token,
        (Self.Factory.Parameter_Specification_Sequence),
        No_Token,
        Nodes (3),
        No_Token,
        No_Token,
        Nodes (4));

      when 423 =>

   Nodes (1) := Self.Factory.Formal_Access_To_Function_Definition
       (Nodes (1),
        Nodes (2),
        Nodes (3),
        Nodes (4),
        Nodes (5),
        Nodes (6),
        Nodes (7),
        Nodes (8),
        Nodes (9),
        No_Token,
        No_Token,
        Nodes (10));

      when 424 =>

   Nodes (1) := Self.Factory.Formal_Access_To_Function_Definition
       (Nodes (1),
        Nodes (2),
        Nodes (3),
        Nodes (4),
        Nodes (5),
        No_Token,
        (Self.Factory.Parameter_Specification_Sequence),
        No_Token,
        Nodes (6),
        No_Token,
        No_Token,
        Nodes (7));

      when 425 =>

   Nodes (1) := Self.Factory.Formal_Access_To_Function_Definition
       (Nodes (1),
        Nodes (2),
        Nodes (3),
        No_Token,
        Nodes (4),
        Nodes (5),
        Nodes (6),
        Nodes (7),
        Nodes (8),
        No_Token,
        No_Token,
        Nodes (9));

      when 426 =>

   Nodes (1) := Self.Factory.Formal_Access_To_Function_Definition
       (Nodes (1),
        Nodes (2),
        Nodes (3),
        No_Token,
        Nodes (4),
        No_Token,
        (Self.Factory.Parameter_Specification_Sequence),
        No_Token,
        Nodes (5),
        No_Token,
        No_Token,
        Nodes (6));

      when 427 =>

   Nodes (1) := Self.Factory.Formal_Access_To_Function_Definition
       (No_Token,
        No_Token,
        Nodes (1),
        Nodes (2),
        Nodes (3),
        Nodes (4),
        Nodes (5),
        Nodes (6),
        Nodes (7),
        No_Token,
        No_Token,
        Nodes (8));

      when 428 =>

   Nodes (1) := Self.Factory.Formal_Access_To_Function_Definition
       (No_Token,
        No_Token,
        Nodes (1),
        Nodes (2),
        Nodes (3),
        No_Token,
        (Self.Factory.Parameter_Specification_Sequence),
        No_Token,
        Nodes (4),
        No_Token,
        No_Token,
        Nodes (5));

      when 429 =>

   Nodes (1) := Self.Factory.Formal_Access_To_Function_Definition
       (No_Token,
        No_Token,
        Nodes (1),
        No_Token,
        Nodes (2),
        Nodes (3),
        Nodes (4),
        Nodes (5),
        Nodes (6),
        No_Token,
        No_Token,
        Nodes (7));

      when 430 =>

   Nodes (1) := Self.Factory.Formal_Access_To_Function_Definition
       (No_Token,
        No_Token,
        Nodes (1),
        No_Token,
        Nodes (2),
        No_Token,
        (Self.Factory.Parameter_Specification_Sequence),
        No_Token,
        Nodes (3),
        No_Token,
        No_Token,
        Nodes (4));

      when 431 =>

   Nodes (1) :=
      Self.Factory.Formal_Type_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7));

      when 432 =>

   Nodes (1) :=
      Self.Factory.Formal_Type_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (6));

      when 433 =>

   Nodes (1) :=
      Self.Factory.Formal_Type_Declaration
         (Nodes (1),
          Nodes (2),
          None,
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6));

      when 434 =>

   Nodes (1) :=
      Self.Factory.Formal_Type_Declaration
         (Nodes (1),
          Nodes (2),
          None,
          Nodes (3),
          Nodes (4),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (5));

      when 435 =>

   Nodes (1) :=
      Self.Factory.Formal_Type_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Self.Factory.Aspect_Specification_Sequence,
          Nodes (6));

      when 436 =>

   Nodes (1) :=
      Self.Factory.Formal_Type_Declaration
         (Nodes (1),
          Nodes (2),
          None,
          Nodes (3),
          Nodes (4),
          Self.Factory.Aspect_Specification_Sequence,
          Nodes (5));

      when 437 =>

   declare
      List : Node :=
        Nodes (4);
   begin
      Self.Factory.Prepend_Discrete_Subtype_Definition
        (List, Nodes (3));
      Nodes (1) := Self.Factory.
        Formal_Constrained_Array_Definition
          (Nodes (1),
           Nodes (2),
           List,
           Nodes (5),
           Nodes (6),
           Nodes (7));
   end;

      when 438 =>

   declare
      List : Node :=
        Self.Factory.
        Discrete_Subtype_Definition_Sequence;
   begin
      Self.Factory.Prepend_Discrete_Subtype_Definition
        (List, Nodes (3));
      Nodes (1) := Self.Factory.
        Formal_Constrained_Array_Definition
          (Nodes (1),
           Nodes (2),
           List,
           Nodes (4),
           Nodes (5),
           Nodes (6));
   end;

      when 439 =>

   Nodes (1) :=
      Self.Factory.Formal_Decimal_Fixed_Point_Definition
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4));

      when 440 =>

   Nodes (1) :=
      Self.Factory.Formal_Derived_Type_Definition
         (Nodes (1),
          Nodes (2),
          No_Token,
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          Nodes (9));

      when 441 =>

   Nodes (1) :=
      Self.Factory.Formal_Derived_Type_Definition
         (Nodes (1),
          Nodes (2),
          No_Token,
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          (Self.Factory.Aspect_Specification_Sequence));

      when 442 =>

   Nodes (1) :=
      Self.Factory.Formal_Derived_Type_Definition
         (Nodes (1),
          Nodes (2),
          No_Token,
          Nodes (3),
          Nodes (4),
          No_Token,
          (Self.Factory.Subtype_Mark_Sequence),
          Nodes (5),
          Nodes (6),
          Nodes (7));

      when 443 =>

   Nodes (1) :=
      Self.Factory.Formal_Derived_Type_Definition
         (Nodes (1),
          Nodes (2),
          No_Token,
          Nodes (3),
          Nodes (4),
          No_Token,
          (Self.Factory.Subtype_Mark_Sequence),
          Nodes (5),
          Nodes (6),
          (Self.Factory.Aspect_Specification_Sequence));

      when 444 =>

   Nodes (1) :=
      Self.Factory.Formal_Derived_Type_Definition
         (Nodes (1),
          Nodes (2),
          No_Token,
          Nodes (3),
          Nodes (4),
          No_Token,
          (Self.Factory.Subtype_Mark_Sequence),
          No_Token,
          No_Token,
          Nodes (5));

      when 445 =>

   Nodes (1) :=
      Self.Factory.Formal_Derived_Type_Definition
         (Nodes (1),
          Nodes (2),
          No_Token,
          Nodes (3),
          Nodes (4),
          No_Token,
          (Self.Factory.Subtype_Mark_Sequence),
          No_Token,
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence));

      when 446 =>

   Nodes (1) :=
      Self.Factory.Formal_Derived_Type_Definition
         (Nodes (1),
          No_Token,
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          Nodes (9));

      when 447 =>

   Nodes (1) :=
      Self.Factory.Formal_Derived_Type_Definition
         (Nodes (1),
          No_Token,
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          (Self.Factory.Aspect_Specification_Sequence));

      when 448 =>

   Nodes (1) :=
      Self.Factory.Formal_Derived_Type_Definition
         (Nodes (1),
          No_Token,
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token,
          (Self.Factory.Subtype_Mark_Sequence),
          Nodes (5),
          Nodes (6),
          Nodes (7));

      when 449 =>

   Nodes (1) :=
      Self.Factory.Formal_Derived_Type_Definition
         (Nodes (1),
          No_Token,
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token,
          (Self.Factory.Subtype_Mark_Sequence),
          Nodes (5),
          Nodes (6),
          (Self.Factory.Aspect_Specification_Sequence));

      when 450 =>

   Nodes (1) :=
      Self.Factory.Formal_Derived_Type_Definition
         (Nodes (1),
          No_Token,
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token,
          (Self.Factory.Subtype_Mark_Sequence),
          No_Token,
          No_Token,
          Nodes (5));

      when 451 =>

   Nodes (1) :=
      Self.Factory.Formal_Derived_Type_Definition
         (Nodes (1),
          No_Token,
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token,
          (Self.Factory.Subtype_Mark_Sequence),
          No_Token,
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence));

      when 452 =>

   Nodes (1) :=
      Self.Factory.Formal_Derived_Type_Definition
         (Nodes (1),
          No_Token,
          No_Token,
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8));

      when 453 =>

   Nodes (1) :=
      Self.Factory.Formal_Derived_Type_Definition
         (Nodes (1),
          No_Token,
          No_Token,
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          (Self.Factory.Aspect_Specification_Sequence));

      when 454 =>

   Nodes (1) :=
      Self.Factory.Formal_Derived_Type_Definition
         (Nodes (1),
          No_Token,
          No_Token,
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Subtype_Mark_Sequence),
          Nodes (4),
          Nodes (5),
          Nodes (6));

      when 455 =>

   Nodes (1) :=
      Self.Factory.Formal_Derived_Type_Definition
         (Nodes (1),
          No_Token,
          No_Token,
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Subtype_Mark_Sequence),
          Nodes (4),
          Nodes (5),
          (Self.Factory.Aspect_Specification_Sequence));

      when 456 =>

   Nodes (1) :=
      Self.Factory.Formal_Derived_Type_Definition
         (Nodes (1),
          No_Token,
          No_Token,
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Subtype_Mark_Sequence),
          No_Token,
          No_Token,
          Nodes (4));

      when 457 =>

   Nodes (1) :=
      Self.Factory.Formal_Derived_Type_Definition
         (Nodes (1),
          No_Token,
          No_Token,
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Subtype_Mark_Sequence),
          No_Token,
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence));

      when 458 =>

   Nodes (1) :=
      Self.Factory.Formal_Derived_Type_Definition
         (No_Token,
          Nodes (1),
          No_Token,
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8));

      when 459 =>

   Nodes (1) :=
      Self.Factory.Formal_Derived_Type_Definition
         (No_Token,
          Nodes (1),
          No_Token,
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          (Self.Factory.Aspect_Specification_Sequence));

      when 460 =>

   Nodes (1) :=
      Self.Factory.Formal_Derived_Type_Definition
         (No_Token,
          Nodes (1),
          No_Token,
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Subtype_Mark_Sequence),
          Nodes (4),
          Nodes (5),
          Nodes (6));

      when 461 =>

   Nodes (1) :=
      Self.Factory.Formal_Derived_Type_Definition
         (No_Token,
          Nodes (1),
          No_Token,
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Subtype_Mark_Sequence),
          Nodes (4),
          Nodes (5),
          (Self.Factory.Aspect_Specification_Sequence));

      when 462 =>

   Nodes (1) :=
      Self.Factory.Formal_Derived_Type_Definition
         (No_Token,
          Nodes (1),
          No_Token,
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Subtype_Mark_Sequence),
          No_Token,
          No_Token,
          Nodes (4));

      when 463 =>

   Nodes (1) :=
      Self.Factory.Formal_Derived_Type_Definition
         (No_Token,
          Nodes (1),
          No_Token,
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Subtype_Mark_Sequence),
          No_Token,
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence));

      when 464 =>

   Nodes (1) :=
      Self.Factory.Formal_Derived_Type_Definition
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8));

      when 465 =>

   Nodes (1) :=
      Self.Factory.Formal_Derived_Type_Definition
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          (Self.Factory.Aspect_Specification_Sequence));

      when 466 =>

   Nodes (1) :=
      Self.Factory.Formal_Derived_Type_Definition
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Subtype_Mark_Sequence),
          Nodes (4),
          Nodes (5),
          Nodes (6));

      when 467 =>

   Nodes (1) :=
      Self.Factory.Formal_Derived_Type_Definition
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Subtype_Mark_Sequence),
          Nodes (4),
          Nodes (5),
          (Self.Factory.Aspect_Specification_Sequence));

      when 468 =>

   Nodes (1) :=
      Self.Factory.Formal_Derived_Type_Definition
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Subtype_Mark_Sequence),
          No_Token,
          No_Token,
          Nodes (4));

      when 469 =>

   Nodes (1) :=
      Self.Factory.Formal_Derived_Type_Definition
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Subtype_Mark_Sequence),
          No_Token,
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence));

      when 470 =>

   Nodes (1) :=
      Self.Factory.Formal_Derived_Type_Definition
         (No_Token,
          No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7));

      when 471 =>

   Nodes (1) :=
      Self.Factory.Formal_Derived_Type_Definition
         (No_Token,
          No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          (Self.Factory.Aspect_Specification_Sequence));

      when 472 =>

   Nodes (1) :=
      Self.Factory.Formal_Derived_Type_Definition
         (No_Token,
          No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          No_Token,
          (Self.Factory.Subtype_Mark_Sequence),
          Nodes (3),
          Nodes (4),
          Nodes (5));

      when 473 =>

   Nodes (1) :=
      Self.Factory.Formal_Derived_Type_Definition
         (No_Token,
          No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          No_Token,
          (Self.Factory.Subtype_Mark_Sequence),
          Nodes (3),
          Nodes (4),
          (Self.Factory.Aspect_Specification_Sequence));

      when 474 =>

   Nodes (1) :=
      Self.Factory.Formal_Derived_Type_Definition
         (No_Token,
          No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          No_Token,
          (Self.Factory.Subtype_Mark_Sequence),
          No_Token,
          No_Token,
          Nodes (3));

      when 475 =>

   Nodes (1) :=
      Self.Factory.Formal_Derived_Type_Definition
         (No_Token,
          No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          No_Token,
          (Self.Factory.Subtype_Mark_Sequence),
          No_Token,
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence));

      when 476 =>

   Nodes (1) :=
      Self.Factory.Formal_Discrete_Type_Definition
         (Nodes (1),
          Nodes (2),
          Nodes (3));

      when 477 =>

   Nodes (1) :=
      Self.Factory.Formal_Floating_Point_Definition
         (Nodes (1),
          Nodes (2));

      when 478 =>

   Nodes (1) :=
      Self.Factory.Formal_Function_Declaration
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
          Nodes (13),
          No_Token,
          Nodes (14),
          Nodes (15));

      when 479 =>

   Nodes (1) :=
      Self.Factory.Formal_Function_Declaration
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
          Nodes (13),
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (14));

      when 480 =>

   Nodes (1) :=
      Self.Factory.Formal_Function_Declaration
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
          None,
          Nodes (13),
          Nodes (14),
          Nodes (15));

      when 481 =>

   Nodes (1) :=
      Self.Factory.Formal_Function_Declaration
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
          None,
          Nodes (13),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (14));

      when 482 =>

   Nodes (1) :=
      Self.Factory.Formal_Function_Declaration
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
          None,
          No_Token,
          Nodes (13),
          Nodes (14));

      when 483 =>

   Nodes (1) :=
      Self.Factory.Formal_Function_Declaration
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
          None,
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (13));

      when 484 =>

   Nodes (1) :=
      Self.Factory.Formal_Function_Declaration
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
          Nodes (12),
          No_Token,
          Nodes (13),
          Nodes (14));

      when 485 =>

   Nodes (1) :=
      Self.Factory.Formal_Function_Declaration
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
          Nodes (12),
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (13));

      when 486 =>

   Nodes (1) :=
      Self.Factory.Formal_Function_Declaration
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
          None,
          Nodes (12),
          Nodes (13),
          Nodes (14));

      when 487 =>

   Nodes (1) :=
      Self.Factory.Formal_Function_Declaration
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
          None,
          Nodes (12),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (13));

      when 488 =>

   Nodes (1) :=
      Self.Factory.Formal_Function_Declaration
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
          No_Token,
          No_Token,
          None,
          No_Token,
          Nodes (11),
          Nodes (12));

      when 489 =>

   Nodes (1) :=
      Self.Factory.Formal_Function_Declaration
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
          No_Token,
          No_Token,
          None,
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (11));

      when 490 =>

   Nodes (1) :=
      Self.Factory.Formal_Function_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          No_Token,
          No_Token,
          Nodes (8),
          Nodes (9),
          Nodes (10),
          Nodes (11),
          No_Token,
          Nodes (12),
          Nodes (13));

      when 491 =>

   Nodes (1) :=
      Self.Factory.Formal_Function_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          No_Token,
          No_Token,
          Nodes (8),
          Nodes (9),
          Nodes (10),
          Nodes (11),
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (12));

      when 492 =>

   Nodes (1) :=
      Self.Factory.Formal_Function_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          No_Token,
          No_Token,
          Nodes (8),
          Nodes (9),
          Nodes (10),
          None,
          Nodes (11),
          Nodes (12),
          Nodes (13));

      when 493 =>

   Nodes (1) :=
      Self.Factory.Formal_Function_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          No_Token,
          No_Token,
          Nodes (8),
          Nodes (9),
          Nodes (10),
          None,
          Nodes (11),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (12));

      when 494 =>

   Nodes (1) :=
      Self.Factory.Formal_Function_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          No_Token,
          No_Token,
          Nodes (8),
          Nodes (9),
          Nodes (10),
          None,
          No_Token,
          Nodes (11),
          Nodes (12));

      when 495 =>

   Nodes (1) :=
      Self.Factory.Formal_Function_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          No_Token,
          No_Token,
          Nodes (8),
          Nodes (9),
          Nodes (10),
          None,
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (11));

      when 496 =>

   Nodes (1) :=
      Self.Factory.Formal_Function_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          No_Token,
          No_Token,
          Nodes (8),
          Nodes (9),
          No_Token,
          Nodes (10),
          No_Token,
          Nodes (11),
          Nodes (12));

      when 497 =>

   Nodes (1) :=
      Self.Factory.Formal_Function_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          No_Token,
          No_Token,
          Nodes (8),
          Nodes (9),
          No_Token,
          Nodes (10),
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (11));

      when 498 =>

   Nodes (1) :=
      Self.Factory.Formal_Function_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          No_Token,
          No_Token,
          Nodes (8),
          Nodes (9),
          No_Token,
          None,
          Nodes (10),
          Nodes (11),
          Nodes (12));

      when 499 =>

   Nodes (1) :=
      Self.Factory.Formal_Function_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          No_Token,
          No_Token,
          Nodes (8),
          Nodes (9),
          No_Token,
          None,
          Nodes (10),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (11));

      when 500 =>

   Nodes (1) :=
      Self.Factory.Formal_Function_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          No_Token,
          No_Token,
          Nodes (8),
          No_Token,
          No_Token,
          None,
          No_Token,
          Nodes (9),
          Nodes (10));

      when others =>
         raise Constraint_Error;
   end case;
end Program.Parsers.On_Reduce_1;
