
with Program.Parsers.Nodes;
use Program.Parsers.Nodes;
pragma Style_Checks ("N");
procedure Program.Parsers.On_Reduce_1001
  (Self  : access Parse_Context;
   Prod  : Anagram.Grammars.Production_Index;
   Nodes : in out Program.Parsers.Nodes.Node_Array) is
begin
   case Prod is
      when 1001 =>

   Nodes (1) := Self.Factory.Function_Body
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (3),
          No_Token,
          No_Token,
          Nodes (4),
          Nodes (5),
          Nodes (6),
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (7),
          Nodes (8),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (9),
          None,
          Nodes (10));

      when 1002 =>

   Nodes (1) := Self.Factory.Function_Body
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (3),
          No_Token,
          No_Token,
          Nodes (4),
          Nodes (5),
          Nodes (6),
          (Self.Factory.Declarative_Item_Sequence),
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (7),
          Nodes (8),
          Nodes (9));

      when 1003 =>

   Nodes (1) := Self.Factory.Function_Body
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (3),
          No_Token,
          No_Token,
          Nodes (4),
          Nodes (5),
          Nodes (6),
          (Self.Factory.Declarative_Item_Sequence),
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (7),
          None,
          Nodes (8));

      when 1004 =>

   Nodes (1) := Self.Factory.Function_Body
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (3),
          No_Token,
          No_Token,
          Nodes (4),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          Nodes (9),
          Nodes (10),
          Nodes (11),
          Nodes (12),
          Nodes (13));

      when 1005 =>

   Nodes (1) := Self.Factory.Function_Body
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (3),
          No_Token,
          No_Token,
          Nodes (4),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          Nodes (9),
          Nodes (10),
          Nodes (11),
          None,
          Nodes (12));

      when 1006 =>

   Nodes (1) := Self.Factory.Function_Body
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (3),
          No_Token,
          No_Token,
          Nodes (4),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (9),
          Nodes (10),
          Nodes (11));

      when 1007 =>

   Nodes (1) := Self.Factory.Function_Body
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (3),
          No_Token,
          No_Token,
          Nodes (4),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (9),
          None,
          Nodes (10));

      when 1008 =>

   Nodes (1) := Self.Factory.Function_Body
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (3),
          No_Token,
          No_Token,
          Nodes (4),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (5),
          Nodes (6),
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (7),
          Nodes (8),
          Nodes (9));

      when 1009 =>

   Nodes (1) := Self.Factory.Function_Body
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (3),
          No_Token,
          No_Token,
          Nodes (4),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (5),
          Nodes (6),
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (7),
          None,
          Nodes (8));

      when 1010 =>

   Nodes (1) := Self.Factory.Function_Body
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (3),
          No_Token,
          No_Token,
          Nodes (4),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (5),
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          Nodes (9),
          Nodes (10),
          Nodes (11),
          Nodes (12));

      when 1011 =>

   Nodes (1) := Self.Factory.Function_Body
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (3),
          No_Token,
          No_Token,
          Nodes (4),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (5),
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          Nodes (9),
          Nodes (10),
          None,
          Nodes (11));

      when 1012 =>

   Nodes (1) := Self.Factory.Function_Body
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (3),
          No_Token,
          No_Token,
          Nodes (4),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (5),
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (6),
          Nodes (7),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (8),
          Nodes (9),
          Nodes (10));

      when 1013 =>

   Nodes (1) := Self.Factory.Function_Body
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (3),
          No_Token,
          No_Token,
          Nodes (4),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (5),
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (6),
          Nodes (7),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (8),
          None,
          Nodes (9));

      when 1014 =>

   Nodes (1) := Self.Factory.Function_Body
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (3),
          No_Token,
          No_Token,
          Nodes (4),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (5),
          (Self.Factory.Declarative_Item_Sequence),
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (6),
          Nodes (7),
          Nodes (8));

      when 1015 =>

   Nodes (1) := Self.Factory.Function_Body
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (3),
          No_Token,
          No_Token,
          Nodes (4),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (5),
          (Self.Factory.Declarative_Item_Sequence),
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (6),
          None,
          Nodes (7));

      when 1016 =>

   Nodes (1) := Self.Factory.Function_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          No_Token,
          No_Token,
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

      when 1017 =>

   Nodes (1) := Self.Factory.Function_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          No_Token,
          No_Token,
          Nodes (9),
          Nodes (10),
          Nodes (11),
          Nodes (12),
          Nodes (13),
          Nodes (14),
          Nodes (15),
          Nodes (16),
          Nodes (17),
          None,
          Nodes (18));

      when 1018 =>

   Nodes (1) := Self.Factory.Function_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          No_Token,
          No_Token,
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

      when 1019 =>

   Nodes (1) := Self.Factory.Function_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          No_Token,
          No_Token,
          Nodes (9),
          Nodes (10),
          Nodes (11),
          Nodes (12),
          Nodes (13),
          Nodes (14),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (15),
          None,
          Nodes (16));

      when 1020 =>

   Nodes (1) := Self.Factory.Function_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          No_Token,
          No_Token,
          Nodes (9),
          Nodes (10),
          Nodes (11),
          Nodes (12),
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (13),
          Nodes (14),
          Nodes (15));

      when 1021 =>

   Nodes (1) := Self.Factory.Function_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          No_Token,
          No_Token,
          Nodes (9),
          Nodes (10),
          Nodes (11),
          Nodes (12),
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (13),
          None,
          Nodes (14));

      when 1022 =>

   Nodes (1) := Self.Factory.Function_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          No_Token,
          No_Token,
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

      when 1023 =>

   Nodes (1) := Self.Factory.Function_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          No_Token,
          No_Token,
          Nodes (9),
          Nodes (10),
          Nodes (11),
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (12),
          Nodes (13),
          Nodes (14),
          Nodes (15),
          Nodes (16),
          None,
          Nodes (17));

      when 1024 =>

   Nodes (1) := Self.Factory.Function_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          No_Token,
          No_Token,
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

      when 1025 =>

   Nodes (1) := Self.Factory.Function_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          No_Token,
          No_Token,
          Nodes (9),
          Nodes (10),
          Nodes (11),
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (12),
          Nodes (13),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (14),
          None,
          Nodes (15));

      when 1026 =>

   Nodes (1) := Self.Factory.Function_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          No_Token,
          No_Token,
          Nodes (9),
          Nodes (10),
          Nodes (11),
          (Self.Factory.Declarative_Item_Sequence),
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (12),
          Nodes (13),
          Nodes (14));

      when 1027 =>

   Nodes (1) := Self.Factory.Function_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          No_Token,
          No_Token,
          Nodes (9),
          Nodes (10),
          Nodes (11),
          (Self.Factory.Declarative_Item_Sequence),
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (12),
          None,
          Nodes (13));

      when 1028 =>

   Nodes (1) := Self.Factory.Function_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          No_Token,
          No_Token,
          Nodes (9),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (10),
          Nodes (11),
          Nodes (12),
          Nodes (13),
          Nodes (14),
          Nodes (15),
          Nodes (16),
          Nodes (17),
          Nodes (18));

      when 1029 =>

   Nodes (1) := Self.Factory.Function_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          No_Token,
          No_Token,
          Nodes (9),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (10),
          Nodes (11),
          Nodes (12),
          Nodes (13),
          Nodes (14),
          Nodes (15),
          Nodes (16),
          None,
          Nodes (17));

      when 1030 =>

   Nodes (1) := Self.Factory.Function_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          No_Token,
          No_Token,
          Nodes (9),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (10),
          Nodes (11),
          Nodes (12),
          Nodes (13),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (14),
          Nodes (15),
          Nodes (16));

      when 1031 =>

   Nodes (1) := Self.Factory.Function_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          No_Token,
          No_Token,
          Nodes (9),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (10),
          Nodes (11),
          Nodes (12),
          Nodes (13),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (14),
          None,
          Nodes (15));

      when 1032 =>

   Nodes (1) := Self.Factory.Function_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          No_Token,
          No_Token,
          Nodes (9),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (10),
          Nodes (11),
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (12),
          Nodes (13),
          Nodes (14));

      when 1033 =>

   Nodes (1) := Self.Factory.Function_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          No_Token,
          No_Token,
          Nodes (9),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (10),
          Nodes (11),
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (12),
          None,
          Nodes (13));

      when 1034 =>

   Nodes (1) := Self.Factory.Function_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          No_Token,
          No_Token,
          Nodes (9),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (10),
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (11),
          Nodes (12),
          Nodes (13),
          Nodes (14),
          Nodes (15),
          Nodes (16),
          Nodes (17));

      when 1035 =>

   Nodes (1) := Self.Factory.Function_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          No_Token,
          No_Token,
          Nodes (9),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (10),
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (11),
          Nodes (12),
          Nodes (13),
          Nodes (14),
          Nodes (15),
          None,
          Nodes (16));

      when 1036 =>

   Nodes (1) := Self.Factory.Function_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          No_Token,
          No_Token,
          Nodes (9),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (10),
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (11),
          Nodes (12),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (13),
          Nodes (14),
          Nodes (15));

      when 1037 =>

   Nodes (1) := Self.Factory.Function_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          No_Token,
          No_Token,
          Nodes (9),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (10),
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (11),
          Nodes (12),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (13),
          None,
          Nodes (14));

      when 1038 =>

   Nodes (1) := Self.Factory.Function_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          No_Token,
          No_Token,
          Nodes (9),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (10),
          (Self.Factory.Declarative_Item_Sequence),
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (11),
          Nodes (12),
          Nodes (13));

      when 1039 =>

   Nodes (1) := Self.Factory.Function_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          No_Token,
          No_Token,
          Nodes (9),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (10),
          (Self.Factory.Declarative_Item_Sequence),
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (11),
          None,
          Nodes (12));

      when 1040 =>

   Nodes (1) := Self.Factory.Function_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (5),
          No_Token,
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

      when 1041 =>

   Nodes (1) := Self.Factory.Function_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (5),
          No_Token,
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
          None,
          Nodes (15));

      when 1042 =>

   Nodes (1) := Self.Factory.Function_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (5),
          No_Token,
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

      when 1043 =>

   Nodes (1) := Self.Factory.Function_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (5),
          No_Token,
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
          None,
          Nodes (13));

      when 1044 =>

   Nodes (1) := Self.Factory.Function_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (5),
          No_Token,
          No_Token,
          Nodes (6),
          Nodes (7),
          Nodes (8),
          Nodes (9),
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (10),
          Nodes (11),
          Nodes (12));

      when 1045 =>

   Nodes (1) := Self.Factory.Function_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (5),
          No_Token,
          No_Token,
          Nodes (6),
          Nodes (7),
          Nodes (8),
          Nodes (9),
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (10),
          None,
          Nodes (11));

      when 1046 =>

   Nodes (1) := Self.Factory.Function_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (5),
          No_Token,
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

      when 1047 =>

   Nodes (1) := Self.Factory.Function_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (5),
          No_Token,
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
          None,
          Nodes (14));

      when 1048 =>

   Nodes (1) := Self.Factory.Function_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (5),
          No_Token,
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

      when 1049 =>

   Nodes (1) := Self.Factory.Function_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (5),
          No_Token,
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
          None,
          Nodes (12));

      when 1050 =>

   Nodes (1) := Self.Factory.Function_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (5),
          No_Token,
          No_Token,
          Nodes (6),
          Nodes (7),
          Nodes (8),
          (Self.Factory.Declarative_Item_Sequence),
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (9),
          Nodes (10),
          Nodes (11));

      when 1051 =>

   Nodes (1) := Self.Factory.Function_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (5),
          No_Token,
          No_Token,
          Nodes (6),
          Nodes (7),
          Nodes (8),
          (Self.Factory.Declarative_Item_Sequence),
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (9),
          None,
          Nodes (10));

      when 1052 =>

   Nodes (1) := Self.Factory.Function_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (5),
          No_Token,
          No_Token,
          Nodes (6),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (7),
          Nodes (8),
          Nodes (9),
          Nodes (10),
          Nodes (11),
          Nodes (12),
          Nodes (13),
          Nodes (14),
          Nodes (15));

      when 1053 =>

   Nodes (1) := Self.Factory.Function_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (5),
          No_Token,
          No_Token,
          Nodes (6),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (7),
          Nodes (8),
          Nodes (9),
          Nodes (10),
          Nodes (11),
          Nodes (12),
          Nodes (13),
          None,
          Nodes (14));

      when 1054 =>

   Nodes (1) := Self.Factory.Function_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (5),
          No_Token,
          No_Token,
          Nodes (6),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (7),
          Nodes (8),
          Nodes (9),
          Nodes (10),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (11),
          Nodes (12),
          Nodes (13));

      when 1055 =>

   Nodes (1) := Self.Factory.Function_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (5),
          No_Token,
          No_Token,
          Nodes (6),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (7),
          Nodes (8),
          Nodes (9),
          Nodes (10),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (11),
          None,
          Nodes (12));

      when 1056 =>

   Nodes (1) := Self.Factory.Function_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (5),
          No_Token,
          No_Token,
          Nodes (6),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (7),
          Nodes (8),
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (9),
          Nodes (10),
          Nodes (11));

      when 1057 =>

   Nodes (1) := Self.Factory.Function_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (5),
          No_Token,
          No_Token,
          Nodes (6),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (7),
          Nodes (8),
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (9),
          None,
          Nodes (10));

      when 1058 =>

   Nodes (1) := Self.Factory.Function_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (5),
          No_Token,
          No_Token,
          Nodes (6),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (7),
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (8),
          Nodes (9),
          Nodes (10),
          Nodes (11),
          Nodes (12),
          Nodes (13),
          Nodes (14));

      when 1059 =>

   Nodes (1) := Self.Factory.Function_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (5),
          No_Token,
          No_Token,
          Nodes (6),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (7),
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (8),
          Nodes (9),
          Nodes (10),
          Nodes (11),
          Nodes (12),
          None,
          Nodes (13));

      when 1060 =>

   Nodes (1) := Self.Factory.Function_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (5),
          No_Token,
          No_Token,
          Nodes (6),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (7),
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (8),
          Nodes (9),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (10),
          Nodes (11),
          Nodes (12));

      when 1061 =>

   Nodes (1) := Self.Factory.Function_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (5),
          No_Token,
          No_Token,
          Nodes (6),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (7),
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (8),
          Nodes (9),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (10),
          None,
          Nodes (11));

      when 1062 =>

   Nodes (1) := Self.Factory.Function_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (5),
          No_Token,
          No_Token,
          Nodes (6),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (7),
          (Self.Factory.Declarative_Item_Sequence),
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (8),
          Nodes (9),
          Nodes (10));

      when 1063 =>

   Nodes (1) := Self.Factory.Function_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (5),
          No_Token,
          No_Token,
          Nodes (6),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (7),
          (Self.Factory.Declarative_Item_Sequence),
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (8),
          None,
          Nodes (9));

      when 1064 =>

   Nodes (1) := Self.Factory.Function_Body
         (No_Token,
          Nodes (1),
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
          Nodes (12),
          Nodes (13),
          Nodes (14),
          Nodes (15),
          Nodes (16),
          Nodes (17),
          Nodes (18));

      when 1065 =>

   Nodes (1) := Self.Factory.Function_Body
         (No_Token,
          Nodes (1),
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
          Nodes (12),
          Nodes (13),
          Nodes (14),
          Nodes (15),
          Nodes (16),
          None,
          Nodes (17));

      when 1066 =>

   Nodes (1) := Self.Factory.Function_Body
         (No_Token,
          Nodes (1),
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
          Nodes (12),
          Nodes (13),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (14),
          Nodes (15),
          Nodes (16));

      when 1067 =>

   Nodes (1) := Self.Factory.Function_Body
         (No_Token,
          Nodes (1),
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
          Nodes (12),
          Nodes (13),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (14),
          None,
          Nodes (15));

      when 1068 =>

   Nodes (1) := Self.Factory.Function_Body
         (No_Token,
          Nodes (1),
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
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (12),
          Nodes (13),
          Nodes (14));

      when 1069 =>

   Nodes (1) := Self.Factory.Function_Body
         (No_Token,
          Nodes (1),
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
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (12),
          None,
          Nodes (13));

      when 1070 =>

   Nodes (1) := Self.Factory.Function_Body
         (No_Token,
          Nodes (1),
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
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (11),
          Nodes (12),
          Nodes (13),
          Nodes (14),
          Nodes (15),
          Nodes (16),
          Nodes (17));

      when 1071 =>

   Nodes (1) := Self.Factory.Function_Body
         (No_Token,
          Nodes (1),
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
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (11),
          Nodes (12),
          Nodes (13),
          Nodes (14),
          Nodes (15),
          None,
          Nodes (16));

      when 1072 =>

   Nodes (1) := Self.Factory.Function_Body
         (No_Token,
          Nodes (1),
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
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (11),
          Nodes (12),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (13),
          Nodes (14),
          Nodes (15));

      when 1073 =>

   Nodes (1) := Self.Factory.Function_Body
         (No_Token,
          Nodes (1),
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
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (11),
          Nodes (12),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (13),
          None,
          Nodes (14));

      when 1074 =>

   Nodes (1) := Self.Factory.Function_Body
         (No_Token,
          Nodes (1),
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
          (Self.Factory.Declarative_Item_Sequence),
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (11),
          Nodes (12),
          Nodes (13));

      when 1075 =>

   Nodes (1) := Self.Factory.Function_Body
         (No_Token,
          Nodes (1),
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
          (Self.Factory.Declarative_Item_Sequence),
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (11),
          None,
          Nodes (12));

      when 1076 =>

   Nodes (1) := Self.Factory.Function_Body
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          No_Token,
          No_Token,
          Nodes (8),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (9),
          Nodes (10),
          Nodes (11),
          Nodes (12),
          Nodes (13),
          Nodes (14),
          Nodes (15),
          Nodes (16),
          Nodes (17));

      when 1077 =>

   Nodes (1) := Self.Factory.Function_Body
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          No_Token,
          No_Token,
          Nodes (8),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (9),
          Nodes (10),
          Nodes (11),
          Nodes (12),
          Nodes (13),
          Nodes (14),
          Nodes (15),
          None,
          Nodes (16));

      when 1078 =>

   Nodes (1) := Self.Factory.Function_Body
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          No_Token,
          No_Token,
          Nodes (8),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (9),
          Nodes (10),
          Nodes (11),
          Nodes (12),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (13),
          Nodes (14),
          Nodes (15));

      when 1079 =>

   Nodes (1) := Self.Factory.Function_Body
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          No_Token,
          No_Token,
          Nodes (8),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (9),
          Nodes (10),
          Nodes (11),
          Nodes (12),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (13),
          None,
          Nodes (14));

      when 1080 =>

   Nodes (1) := Self.Factory.Function_Body
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          No_Token,
          No_Token,
          Nodes (8),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (9),
          Nodes (10),
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (11),
          Nodes (12),
          Nodes (13));

      when 1081 =>

   Nodes (1) := Self.Factory.Function_Body
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          No_Token,
          No_Token,
          Nodes (8),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (9),
          Nodes (10),
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (11),
          None,
          Nodes (12));

      when 1082 =>

   Nodes (1) := Self.Factory.Function_Body
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          No_Token,
          No_Token,
          Nodes (8),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (9),
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (10),
          Nodes (11),
          Nodes (12),
          Nodes (13),
          Nodes (14),
          Nodes (15),
          Nodes (16));

      when 1083 =>

   Nodes (1) := Self.Factory.Function_Body
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          No_Token,
          No_Token,
          Nodes (8),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (9),
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (10),
          Nodes (11),
          Nodes (12),
          Nodes (13),
          Nodes (14),
          None,
          Nodes (15));

      when 1084 =>

   Nodes (1) := Self.Factory.Function_Body
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          No_Token,
          No_Token,
          Nodes (8),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (9),
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (10),
          Nodes (11),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (12),
          Nodes (13),
          Nodes (14));

      when 1085 =>

   Nodes (1) := Self.Factory.Function_Body
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          No_Token,
          No_Token,
          Nodes (8),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (9),
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (10),
          Nodes (11),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (12),
          None,
          Nodes (13));

      when 1086 =>

   Nodes (1) := Self.Factory.Function_Body
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          No_Token,
          No_Token,
          Nodes (8),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (9),
          (Self.Factory.Declarative_Item_Sequence),
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (10),
          Nodes (11),
          Nodes (12));

      when 1087 =>

   Nodes (1) := Self.Factory.Function_Body
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          No_Token,
          No_Token,
          Nodes (8),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (9),
          (Self.Factory.Declarative_Item_Sequence),
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (10),
          None,
          Nodes (11));

      when 1088 =>

   Nodes (1) := Self.Factory.Function_Body
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (4),
          No_Token,
          No_Token,
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

      when 1089 =>

   Nodes (1) := Self.Factory.Function_Body
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (4),
          No_Token,
          No_Token,
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          Nodes (9),
          Nodes (10),
          Nodes (11),
          Nodes (12),
          Nodes (13),
          None,
          Nodes (14));

      when 1090 =>

   Nodes (1) := Self.Factory.Function_Body
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (4),
          No_Token,
          No_Token,
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

      when 1091 =>

   Nodes (1) := Self.Factory.Function_Body
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (4),
          No_Token,
          No_Token,
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          Nodes (9),
          Nodes (10),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (11),
          None,
          Nodes (12));

      when 1092 =>

   Nodes (1) := Self.Factory.Function_Body
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (4),
          No_Token,
          No_Token,
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (9),
          Nodes (10),
          Nodes (11));

      when 1093 =>

   Nodes (1) := Self.Factory.Function_Body
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (4),
          No_Token,
          No_Token,
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (9),
          None,
          Nodes (10));

      when 1094 =>

   Nodes (1) := Self.Factory.Function_Body
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (4),
          No_Token,
          No_Token,
          Nodes (5),
          Nodes (6),
          Nodes (7),
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (8),
          Nodes (9),
          Nodes (10),
          Nodes (11),
          Nodes (12),
          Nodes (13),
          Nodes (14));

      when 1095 =>

   Nodes (1) := Self.Factory.Function_Body
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (4),
          No_Token,
          No_Token,
          Nodes (5),
          Nodes (6),
          Nodes (7),
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (8),
          Nodes (9),
          Nodes (10),
          Nodes (11),
          Nodes (12),
          None,
          Nodes (13));

      when 1096 =>

   Nodes (1) := Self.Factory.Function_Body
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (4),
          No_Token,
          No_Token,
          Nodes (5),
          Nodes (6),
          Nodes (7),
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (8),
          Nodes (9),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (10),
          Nodes (11),
          Nodes (12));

      when 1097 =>

   Nodes (1) := Self.Factory.Function_Body
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (4),
          No_Token,
          No_Token,
          Nodes (5),
          Nodes (6),
          Nodes (7),
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (8),
          Nodes (9),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (10),
          None,
          Nodes (11));

      when 1098 =>

   Nodes (1) := Self.Factory.Function_Body
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (4),
          No_Token,
          No_Token,
          Nodes (5),
          Nodes (6),
          Nodes (7),
          (Self.Factory.Declarative_Item_Sequence),
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (8),
          Nodes (9),
          Nodes (10));

      when 1099 =>

   Nodes (1) := Self.Factory.Function_Body
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (4),
          No_Token,
          No_Token,
          Nodes (5),
          Nodes (6),
          Nodes (7),
          (Self.Factory.Declarative_Item_Sequence),
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (8),
          None,
          Nodes (9));

      when 1100 =>

   Nodes (1) := Self.Factory.Function_Body
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (4),
          No_Token,
          No_Token,
          Nodes (5),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          Nodes (9),
          Nodes (10),
          Nodes (11),
          Nodes (12),
          Nodes (13),
          Nodes (14));

      when 1101 =>

   Nodes (1) := Self.Factory.Function_Body
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (4),
          No_Token,
          No_Token,
          Nodes (5),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          Nodes (9),
          Nodes (10),
          Nodes (11),
          Nodes (12),
          None,
          Nodes (13));

      when 1102 =>

   Nodes (1) := Self.Factory.Function_Body
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (4),
          No_Token,
          No_Token,
          Nodes (5),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          Nodes (9),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (10),
          Nodes (11),
          Nodes (12));

      when 1103 =>

   Nodes (1) := Self.Factory.Function_Body
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (4),
          No_Token,
          No_Token,
          Nodes (5),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          Nodes (9),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (10),
          None,
          Nodes (11));

      when 1104 =>

   Nodes (1) := Self.Factory.Function_Body
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (4),
          No_Token,
          No_Token,
          Nodes (5),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (6),
          Nodes (7),
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (8),
          Nodes (9),
          Nodes (10));

      when 1105 =>

   Nodes (1) := Self.Factory.Function_Body
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (4),
          No_Token,
          No_Token,
          Nodes (5),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (6),
          Nodes (7),
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (8),
          None,
          Nodes (9));

      when 1106 =>

   Nodes (1) := Self.Factory.Function_Body
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (4),
          No_Token,
          No_Token,
          Nodes (5),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (6),
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (7),
          Nodes (8),
          Nodes (9),
          Nodes (10),
          Nodes (11),
          Nodes (12),
          Nodes (13));

      when 1107 =>

   Nodes (1) := Self.Factory.Function_Body
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (4),
          No_Token,
          No_Token,
          Nodes (5),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (6),
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (7),
          Nodes (8),
          Nodes (9),
          Nodes (10),
          Nodes (11),
          None,
          Nodes (12));

      when 1108 =>

   Nodes (1) := Self.Factory.Function_Body
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (4),
          No_Token,
          No_Token,
          Nodes (5),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (6),
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (7),
          Nodes (8),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (9),
          Nodes (10),
          Nodes (11));

      when 1109 =>

   Nodes (1) := Self.Factory.Function_Body
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (4),
          No_Token,
          No_Token,
          Nodes (5),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (6),
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (7),
          Nodes (8),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (9),
          None,
          Nodes (10));

      when 1110 =>

   Nodes (1) := Self.Factory.Function_Body
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (4),
          No_Token,
          No_Token,
          Nodes (5),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (6),
          (Self.Factory.Declarative_Item_Sequence),
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (7),
          Nodes (8),
          Nodes (9));

      when 1111 =>

   Nodes (1) := Self.Factory.Function_Body
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (4),
          No_Token,
          No_Token,
          Nodes (5),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (6),
          (Self.Factory.Declarative_Item_Sequence),
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (7),
          None,
          Nodes (8));

      when 1112 =>

   Nodes (1) := Self.Factory.Function_Body
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          No_Token,
          No_Token,
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
          Nodes (17));

      when 1113 =>

   Nodes (1) := Self.Factory.Function_Body
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          No_Token,
          No_Token,
          Nodes (7),
          Nodes (8),
          Nodes (9),
          Nodes (10),
          Nodes (11),
          Nodes (12),
          Nodes (13),
          Nodes (14),
          Nodes (15),
          None,
          Nodes (16));

      when 1114 =>

   Nodes (1) := Self.Factory.Function_Body
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          No_Token,
          No_Token,
          Nodes (7),
          Nodes (8),
          Nodes (9),
          Nodes (10),
          Nodes (11),
          Nodes (12),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (13),
          Nodes (14),
          Nodes (15));

      when 1115 =>

   Nodes (1) := Self.Factory.Function_Body
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          No_Token,
          No_Token,
          Nodes (7),
          Nodes (8),
          Nodes (9),
          Nodes (10),
          Nodes (11),
          Nodes (12),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (13),
          None,
          Nodes (14));

      when 1116 =>

   Nodes (1) := Self.Factory.Function_Body
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          No_Token,
          No_Token,
          Nodes (7),
          Nodes (8),
          Nodes (9),
          Nodes (10),
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (11),
          Nodes (12),
          Nodes (13));

      when 1117 =>

   Nodes (1) := Self.Factory.Function_Body
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          No_Token,
          No_Token,
          Nodes (7),
          Nodes (8),
          Nodes (9),
          Nodes (10),
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (11),
          None,
          Nodes (12));

      when 1118 =>

   Nodes (1) := Self.Factory.Function_Body
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          No_Token,
          No_Token,
          Nodes (7),
          Nodes (8),
          Nodes (9),
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (10),
          Nodes (11),
          Nodes (12),
          Nodes (13),
          Nodes (14),
          Nodes (15),
          Nodes (16));

      when 1119 =>

   Nodes (1) := Self.Factory.Function_Body
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          No_Token,
          No_Token,
          Nodes (7),
          Nodes (8),
          Nodes (9),
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (10),
          Nodes (11),
          Nodes (12),
          Nodes (13),
          Nodes (14),
          None,
          Nodes (15));

      when 1120 =>

   Nodes (1) := Self.Factory.Function_Body
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          No_Token,
          No_Token,
          Nodes (7),
          Nodes (8),
          Nodes (9),
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (10),
          Nodes (11),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (12),
          Nodes (13),
          Nodes (14));

      when 1121 =>

   Nodes (1) := Self.Factory.Function_Body
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          No_Token,
          No_Token,
          Nodes (7),
          Nodes (8),
          Nodes (9),
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (10),
          Nodes (11),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (12),
          None,
          Nodes (13));

      when 1122 =>

   Nodes (1) := Self.Factory.Function_Body
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          No_Token,
          No_Token,
          Nodes (7),
          Nodes (8),
          Nodes (9),
          (Self.Factory.Declarative_Item_Sequence),
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (10),
          Nodes (11),
          Nodes (12));

      when 1123 =>

   Nodes (1) := Self.Factory.Function_Body
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          No_Token,
          No_Token,
          Nodes (7),
          Nodes (8),
          Nodes (9),
          (Self.Factory.Declarative_Item_Sequence),
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (10),
          None,
          Nodes (11));

      when 1124 =>

   Nodes (1) := Self.Factory.Function_Body
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          No_Token,
          No_Token,
          Nodes (7),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (8),
          Nodes (9),
          Nodes (10),
          Nodes (11),
          Nodes (12),
          Nodes (13),
          Nodes (14),
          Nodes (15),
          Nodes (16));

      when 1125 =>

   Nodes (1) := Self.Factory.Function_Body
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          No_Token,
          No_Token,
          Nodes (7),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (8),
          Nodes (9),
          Nodes (10),
          Nodes (11),
          Nodes (12),
          Nodes (13),
          Nodes (14),
          None,
          Nodes (15));

      when 1126 =>

   Nodes (1) := Self.Factory.Function_Body
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          No_Token,
          No_Token,
          Nodes (7),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (8),
          Nodes (9),
          Nodes (10),
          Nodes (11),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (12),
          Nodes (13),
          Nodes (14));

      when 1127 =>

   Nodes (1) := Self.Factory.Function_Body
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          No_Token,
          No_Token,
          Nodes (7),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (8),
          Nodes (9),
          Nodes (10),
          Nodes (11),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (12),
          None,
          Nodes (13));

      when 1128 =>

   Nodes (1) := Self.Factory.Function_Body
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          No_Token,
          No_Token,
          Nodes (7),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (8),
          Nodes (9),
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (10),
          Nodes (11),
          Nodes (12));

      when 1129 =>

   Nodes (1) := Self.Factory.Function_Body
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          No_Token,
          No_Token,
          Nodes (7),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (8),
          Nodes (9),
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (10),
          None,
          Nodes (11));

      when 1130 =>

   Nodes (1) := Self.Factory.Function_Body
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          No_Token,
          No_Token,
          Nodes (7),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (8),
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (9),
          Nodes (10),
          Nodes (11),
          Nodes (12),
          Nodes (13),
          Nodes (14),
          Nodes (15));

      when 1131 =>

   Nodes (1) := Self.Factory.Function_Body
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          No_Token,
          No_Token,
          Nodes (7),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (8),
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (9),
          Nodes (10),
          Nodes (11),
          Nodes (12),
          Nodes (13),
          None,
          Nodes (14));

      when 1132 =>

   Nodes (1) := Self.Factory.Function_Body
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          No_Token,
          No_Token,
          Nodes (7),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (8),
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (9),
          Nodes (10),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (11),
          Nodes (12),
          Nodes (13));

      when 1133 =>

   Nodes (1) := Self.Factory.Function_Body
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          No_Token,
          No_Token,
          Nodes (7),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (8),
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (9),
          Nodes (10),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (11),
          None,
          Nodes (12));

      when 1134 =>

   Nodes (1) := Self.Factory.Function_Body
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          No_Token,
          No_Token,
          Nodes (7),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (8),
          (Self.Factory.Declarative_Item_Sequence),
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (9),
          Nodes (10),
          Nodes (11));

      when 1135 =>

   Nodes (1) := Self.Factory.Function_Body
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          No_Token,
          No_Token,
          Nodes (7),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (8),
          (Self.Factory.Declarative_Item_Sequence),
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (9),
          None,
          Nodes (10));

      when 1136 =>

   Nodes (1) := Self.Factory.Function_Body
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (3),
          No_Token,
          No_Token,
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
          Nodes (14));

      when 1137 =>

   Nodes (1) := Self.Factory.Function_Body
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (3),
          No_Token,
          No_Token,
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
          Nodes (13));

      when 1138 =>

   Nodes (1) := Self.Factory.Function_Body
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (3),
          No_Token,
          No_Token,
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          Nodes (9),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (10),
          Nodes (11),
          Nodes (12));

      when 1139 =>

   Nodes (1) := Self.Factory.Function_Body
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (3),
          No_Token,
          No_Token,
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          Nodes (9),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (10),
          None,
          Nodes (11));

      when 1140 =>

   Nodes (1) := Self.Factory.Function_Body
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (3),
          No_Token,
          No_Token,
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (8),
          Nodes (9),
          Nodes (10));

      when 1141 =>

   Nodes (1) := Self.Factory.Function_Body
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (3),
          No_Token,
          No_Token,
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (8),
          None,
          Nodes (9));

      when 1142 =>

   Nodes (1) := Self.Factory.Function_Body
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (3),
          No_Token,
          No_Token,
          Nodes (4),
          Nodes (5),
          Nodes (6),
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (7),
          Nodes (8),
          Nodes (9),
          Nodes (10),
          Nodes (11),
          Nodes (12),
          Nodes (13));

      when 1143 =>

   Nodes (1) := Self.Factory.Function_Body
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (3),
          No_Token,
          No_Token,
          Nodes (4),
          Nodes (5),
          Nodes (6),
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (7),
          Nodes (8),
          Nodes (9),
          Nodes (10),
          Nodes (11),
          None,
          Nodes (12));

      when 1144 =>

   Nodes (1) := Self.Factory.Function_Body
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (3),
          No_Token,
          No_Token,
          Nodes (4),
          Nodes (5),
          Nodes (6),
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (7),
          Nodes (8),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (9),
          Nodes (10),
          Nodes (11));

      when 1145 =>

   Nodes (1) := Self.Factory.Function_Body
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (3),
          No_Token,
          No_Token,
          Nodes (4),
          Nodes (5),
          Nodes (6),
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (7),
          Nodes (8),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (9),
          None,
          Nodes (10));

      when 1146 =>

   Nodes (1) := Self.Factory.Function_Body
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (3),
          No_Token,
          No_Token,
          Nodes (4),
          Nodes (5),
          Nodes (6),
          (Self.Factory.Declarative_Item_Sequence),
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (7),
          Nodes (8),
          Nodes (9));

      when 1147 =>

   Nodes (1) := Self.Factory.Function_Body
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (3),
          No_Token,
          No_Token,
          Nodes (4),
          Nodes (5),
          Nodes (6),
          (Self.Factory.Declarative_Item_Sequence),
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (7),
          None,
          Nodes (8));

      when 1148 =>

   Nodes (1) := Self.Factory.Function_Body
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (3),
          No_Token,
          No_Token,
          Nodes (4),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          Nodes (9),
          Nodes (10),
          Nodes (11),
          Nodes (12),
          Nodes (13));

      when 1149 =>

   Nodes (1) := Self.Factory.Function_Body
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (3),
          No_Token,
          No_Token,
          Nodes (4),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          Nodes (9),
          Nodes (10),
          Nodes (11),
          None,
          Nodes (12));

      when 1150 =>

   Nodes (1) := Self.Factory.Function_Body
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (3),
          No_Token,
          No_Token,
          Nodes (4),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (9),
          Nodes (10),
          Nodes (11));

      when 1151 =>

   Nodes (1) := Self.Factory.Function_Body
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (3),
          No_Token,
          No_Token,
          Nodes (4),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (9),
          None,
          Nodes (10));

      when 1152 =>

   Nodes (1) := Self.Factory.Function_Body
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (3),
          No_Token,
          No_Token,
          Nodes (4),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (5),
          Nodes (6),
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (7),
          Nodes (8),
          Nodes (9));

      when 1153 =>

   Nodes (1) := Self.Factory.Function_Body
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (3),
          No_Token,
          No_Token,
          Nodes (4),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (5),
          Nodes (6),
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (7),
          None,
          Nodes (8));

      when 1154 =>

   Nodes (1) := Self.Factory.Function_Body
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (3),
          No_Token,
          No_Token,
          Nodes (4),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (5),
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          Nodes (9),
          Nodes (10),
          Nodes (11),
          Nodes (12));

      when 1155 =>

   Nodes (1) := Self.Factory.Function_Body
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (3),
          No_Token,
          No_Token,
          Nodes (4),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (5),
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          Nodes (9),
          Nodes (10),
          None,
          Nodes (11));

      when 1156 =>

   Nodes (1) := Self.Factory.Function_Body
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (3),
          No_Token,
          No_Token,
          Nodes (4),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (5),
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (6),
          Nodes (7),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (8),
          Nodes (9),
          Nodes (10));

      when 1157 =>

   Nodes (1) := Self.Factory.Function_Body
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (3),
          No_Token,
          No_Token,
          Nodes (4),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (5),
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (6),
          Nodes (7),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (8),
          None,
          Nodes (9));

      when 1158 =>

   Nodes (1) := Self.Factory.Function_Body
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (3),
          No_Token,
          No_Token,
          Nodes (4),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (5),
          (Self.Factory.Declarative_Item_Sequence),
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (6),
          Nodes (7),
          Nodes (8));

      when 1159 =>

   Nodes (1) := Self.Factory.Function_Body
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (3),
          No_Token,
          No_Token,
          Nodes (4),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (5),
          (Self.Factory.Declarative_Item_Sequence),
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (6),
          None,
          Nodes (7));

      when 1160 =>
  Nodes (1) := Self.Factory.Function_Call
     (Nodes (1), Nodes (2));

      when 1161 =>

   Nodes (1) := Self.Factory.Function_Declaration
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
          None,
          No_Token,
          None,
          No_Token,
          Nodes (14),
          Nodes (15));

      when 1162 =>

   Nodes (1) := Self.Factory.Function_Declaration
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
          None,
          No_Token,
          None,
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (14));

      when 1163 =>

   Nodes (1) := Self.Factory.Function_Declaration
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
          No_Token,
          Nodes (13),
          No_Token,
          None,
          No_Token,
          Nodes (14),
          Nodes (15));

      when 1164 =>

   Nodes (1) := Self.Factory.Function_Declaration
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
          No_Token,
          Nodes (13),
          No_Token,
          None,
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (14));

      when 1165 =>

   Nodes (1) := Self.Factory.Function_Declaration
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
          No_Token,
          None,
          Nodes (12),
          Nodes (13),
          No_Token,
          Nodes (14),
          Nodes (15));

      when 1166 =>

   Nodes (1) := Self.Factory.Function_Declaration
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
          No_Token,
          None,
          Nodes (12),
          Nodes (13),
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (14));

      when 1167 =>

   Nodes (1) := Self.Factory.Function_Declaration
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
          No_Token,
          None,
          No_Token,
          None,
          Nodes (13),
          Nodes (14),
          Nodes (15));

      when 1168 =>

   Nodes (1) := Self.Factory.Function_Declaration
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
          No_Token,
          None,
          No_Token,
          None,
          Nodes (13),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (14));

      when 1169 =>

   Nodes (1) := Self.Factory.Function_Declaration
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
          No_Token,
          None,
          No_Token,
          None,
          No_Token,
          Nodes (12),
          Nodes (13));

      when 1170 =>

   Nodes (1) := Self.Factory.Function_Declaration
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
          No_Token,
          None,
          No_Token,
          None,
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (12));

      when 1171 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          No_Token,
          No_Token,
          Nodes (9),
          Nodes (10),
          Nodes (11),
          None,
          No_Token,
          None,
          No_Token,
          Nodes (12),
          Nodes (13));

      when 1172 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          No_Token,
          No_Token,
          Nodes (9),
          Nodes (10),
          Nodes (11),
          None,
          No_Token,
          None,
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (12));

      when 1173 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          No_Token,
          No_Token,
          Nodes (9),
          Nodes (10),
          No_Token,
          Nodes (11),
          No_Token,
          None,
          No_Token,
          Nodes (12),
          Nodes (13));

      when 1174 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          No_Token,
          No_Token,
          Nodes (9),
          Nodes (10),
          No_Token,
          Nodes (11),
          No_Token,
          None,
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (12));

      when 1175 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          No_Token,
          No_Token,
          Nodes (9),
          No_Token,
          No_Token,
          None,
          Nodes (10),
          Nodes (11),
          No_Token,
          Nodes (12),
          Nodes (13));

      when 1176 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          No_Token,
          No_Token,
          Nodes (9),
          No_Token,
          No_Token,
          None,
          Nodes (10),
          Nodes (11),
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (12));

      when 1177 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          No_Token,
          No_Token,
          Nodes (9),
          Nodes (10),
          No_Token,
          None,
          No_Token,
          None,
          Nodes (11),
          Nodes (12),
          Nodes (13));

      when 1178 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          No_Token,
          No_Token,
          Nodes (9),
          Nodes (10),
          No_Token,
          None,
          No_Token,
          None,
          Nodes (11),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (12));

      when 1179 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          No_Token,
          No_Token,
          Nodes (9),
          No_Token,
          No_Token,
          None,
          No_Token,
          None,
          No_Token,
          Nodes (10),
          Nodes (11));

      when 1180 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          No_Token,
          No_Token,
          Nodes (9),
          No_Token,
          No_Token,
          None,
          No_Token,
          None,
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (10));

      when 1181 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          Nodes (9),
          Nodes (10),
          None,
          No_Token,
          None,
          No_Token,
          Nodes (11),
          Nodes (12));

      when 1182 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          Nodes (9),
          Nodes (10),
          None,
          No_Token,
          None,
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (11));

      when 1183 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          Nodes (9),
          No_Token,
          Nodes (10),
          No_Token,
          None,
          No_Token,
          Nodes (11),
          Nodes (12));

      when 1184 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          Nodes (9),
          No_Token,
          Nodes (10),
          No_Token,
          None,
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (11));

      when 1185 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          No_Token,
          No_Token,
          None,
          Nodes (9),
          Nodes (10),
          No_Token,
          Nodes (11),
          Nodes (12));

      when 1186 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          No_Token,
          No_Token,
          None,
          Nodes (9),
          Nodes (10),
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (11));

      when 1187 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          Nodes (9),
          No_Token,
          None,
          No_Token,
          None,
          Nodes (10),
          Nodes (11),
          Nodes (12));

      when 1188 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          Nodes (9),
          No_Token,
          None,
          No_Token,
          None,
          Nodes (10),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (11));

      when 1189 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          No_Token,
          No_Token,
          None,
          No_Token,
          None,
          No_Token,
          Nodes (9),
          Nodes (10));

      when 1190 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          No_Token,
          No_Token,
          None,
          No_Token,
          None,
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (9));

      when 1191 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (5),
          No_Token,
          No_Token,
          Nodes (6),
          Nodes (7),
          Nodes (8),
          None,
          No_Token,
          None,
          No_Token,
          Nodes (9),
          Nodes (10));

      when 1192 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (5),
          No_Token,
          No_Token,
          Nodes (6),
          Nodes (7),
          Nodes (8),
          None,
          No_Token,
          None,
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (9));

      when 1193 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (5),
          No_Token,
          No_Token,
          Nodes (6),
          Nodes (7),
          No_Token,
          Nodes (8),
          No_Token,
          None,
          No_Token,
          Nodes (9),
          Nodes (10));

      when 1194 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (5),
          No_Token,
          No_Token,
          Nodes (6),
          Nodes (7),
          No_Token,
          Nodes (8),
          No_Token,
          None,
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (9));

      when 1195 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (5),
          No_Token,
          No_Token,
          Nodes (6),
          No_Token,
          No_Token,
          None,
          Nodes (7),
          Nodes (8),
          No_Token,
          Nodes (9),
          Nodes (10));

      when 1196 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (5),
          No_Token,
          No_Token,
          Nodes (6),
          No_Token,
          No_Token,
          None,
          Nodes (7),
          Nodes (8),
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (9));

      when 1197 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (5),
          No_Token,
          No_Token,
          Nodes (6),
          Nodes (7),
          No_Token,
          None,
          No_Token,
          None,
          Nodes (8),
          Nodes (9),
          Nodes (10));

      when 1198 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (5),
          No_Token,
          No_Token,
          Nodes (6),
          Nodes (7),
          No_Token,
          None,
          No_Token,
          None,
          Nodes (8),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (9));

      when 1199 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (5),
          No_Token,
          No_Token,
          Nodes (6),
          No_Token,
          No_Token,
          None,
          No_Token,
          None,
          No_Token,
          Nodes (7),
          Nodes (8));

      when 1200 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (5),
          No_Token,
          No_Token,
          Nodes (6),
          No_Token,
          No_Token,
          None,
          No_Token,
          None,
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (7));

      when 1201 =>

   Nodes (1) := Self.Factory.Function_Declaration
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
          Nodes (11),
          Nodes (12),
          None,
          No_Token,
          None,
          No_Token,
          Nodes (13),
          Nodes (14));

      when 1202 =>

   Nodes (1) := Self.Factory.Function_Declaration
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
          Nodes (11),
          Nodes (12),
          None,
          No_Token,
          None,
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (13));

      when 1203 =>

   Nodes (1) := Self.Factory.Function_Declaration
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
          Nodes (11),
          No_Token,
          Nodes (12),
          No_Token,
          None,
          No_Token,
          Nodes (13),
          Nodes (14));

      when 1204 =>

   Nodes (1) := Self.Factory.Function_Declaration
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
          Nodes (11),
          No_Token,
          Nodes (12),
          No_Token,
          None,
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (13));

      when 1205 =>

   Nodes (1) := Self.Factory.Function_Declaration
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
          No_Token,
          No_Token,
          None,
          Nodes (11),
          Nodes (12),
          No_Token,
          Nodes (13),
          Nodes (14));

      when 1206 =>

   Nodes (1) := Self.Factory.Function_Declaration
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
          No_Token,
          No_Token,
          None,
          Nodes (11),
          Nodes (12),
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (13));

      when 1207 =>

   Nodes (1) := Self.Factory.Function_Declaration
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
          Nodes (11),
          No_Token,
          None,
          No_Token,
          None,
          Nodes (12),
          Nodes (13),
          Nodes (14));

      when 1208 =>

   Nodes (1) := Self.Factory.Function_Declaration
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
          Nodes (11),
          No_Token,
          None,
          No_Token,
          None,
          Nodes (12),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (13));

      when 1209 =>

   Nodes (1) := Self.Factory.Function_Declaration
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
          No_Token,
          No_Token,
          None,
          No_Token,
          None,
          No_Token,
          Nodes (11),
          Nodes (12));

      when 1210 =>

   Nodes (1) := Self.Factory.Function_Declaration
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
          No_Token,
          No_Token,
          None,
          No_Token,
          None,
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (11));

      when 1211 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (No_Token,
          Nodes (1),
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
          None,
          No_Token,
          Nodes (11),
          Nodes (12));

      when 1212 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (No_Token,
          Nodes (1),
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
          None,
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (11));

      when 1213 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (No_Token,
          Nodes (1),
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
          None,
          No_Token,
          Nodes (11),
          Nodes (12));

      when 1214 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (No_Token,
          Nodes (1),
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
          None,
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (11));

      when 1215 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (No_Token,
          Nodes (1),
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
          Nodes (9),
          Nodes (10),
          No_Token,
          Nodes (11),
          Nodes (12));

      when 1216 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (No_Token,
          Nodes (1),
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
          Nodes (9),
          Nodes (10),
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (11));

      when 1217 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (No_Token,
          Nodes (1),
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
          No_Token,
          None,
          Nodes (10),
          Nodes (11),
          Nodes (12));

      when 1218 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (No_Token,
          Nodes (1),
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
          No_Token,
          None,
          Nodes (10),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (11));

      when 1219 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (No_Token,
          Nodes (1),
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
          None,
          No_Token,
          Nodes (9),
          Nodes (10));

      when 1220 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (No_Token,
          Nodes (1),
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
          None,
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (9));

      when 1221 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          Nodes (9),
          None,
          No_Token,
          None,
          No_Token,
          Nodes (10),
          Nodes (11));

      when 1222 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          Nodes (9),
          None,
          No_Token,
          None,
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (10));

      when 1223 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          No_Token,
          Nodes (9),
          No_Token,
          None,
          No_Token,
          Nodes (10),
          Nodes (11));

      when 1224 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          No_Token,
          Nodes (9),
          No_Token,
          None,
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (10));

      when 1225 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          No_Token,
          No_Token,
          None,
          Nodes (8),
          Nodes (9),
          No_Token,
          Nodes (10),
          Nodes (11));

      when 1226 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          No_Token,
          No_Token,
          None,
          Nodes (8),
          Nodes (9),
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (10));

      when 1227 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          No_Token,
          None,
          No_Token,
          None,
          Nodes (9),
          Nodes (10),
          Nodes (11));

      when 1228 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          No_Token,
          None,
          No_Token,
          None,
          Nodes (9),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (10));

      when 1229 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          No_Token,
          No_Token,
          None,
          No_Token,
          None,
          No_Token,
          Nodes (8),
          Nodes (9));

      when 1230 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          No_Token,
          No_Token,
          None,
          No_Token,
          None,
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (8));

      when 1231 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (4),
          No_Token,
          No_Token,
          Nodes (5),
          Nodes (6),
          Nodes (7),
          None,
          No_Token,
          None,
          No_Token,
          Nodes (8),
          Nodes (9));

      when 1232 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (4),
          No_Token,
          No_Token,
          Nodes (5),
          Nodes (6),
          Nodes (7),
          None,
          No_Token,
          None,
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (8));

      when 1233 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (4),
          No_Token,
          No_Token,
          Nodes (5),
          Nodes (6),
          No_Token,
          Nodes (7),
          No_Token,
          None,
          No_Token,
          Nodes (8),
          Nodes (9));

      when 1234 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (4),
          No_Token,
          No_Token,
          Nodes (5),
          Nodes (6),
          No_Token,
          Nodes (7),
          No_Token,
          None,
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (8));

      when 1235 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (4),
          No_Token,
          No_Token,
          Nodes (5),
          No_Token,
          No_Token,
          None,
          Nodes (6),
          Nodes (7),
          No_Token,
          Nodes (8),
          Nodes (9));

      when 1236 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (4),
          No_Token,
          No_Token,
          Nodes (5),
          No_Token,
          No_Token,
          None,
          Nodes (6),
          Nodes (7),
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (8));

      when 1237 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (4),
          No_Token,
          No_Token,
          Nodes (5),
          Nodes (6),
          No_Token,
          None,
          No_Token,
          None,
          Nodes (7),
          Nodes (8),
          Nodes (9));

      when 1238 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (4),
          No_Token,
          No_Token,
          Nodes (5),
          Nodes (6),
          No_Token,
          None,
          No_Token,
          None,
          Nodes (7),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (8));

      when 1239 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (4),
          No_Token,
          No_Token,
          Nodes (5),
          No_Token,
          No_Token,
          None,
          No_Token,
          None,
          No_Token,
          Nodes (6),
          Nodes (7));

      when 1240 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (4),
          No_Token,
          No_Token,
          Nodes (5),
          No_Token,
          No_Token,
          None,
          No_Token,
          None,
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (6));

      when 1241 =>

   Nodes (1) := Self.Factory.Function_Declaration
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
          Nodes (10),
          Nodes (11),
          None,
          No_Token,
          None,
          No_Token,
          Nodes (12),
          Nodes (13));

      when 1242 =>

   Nodes (1) := Self.Factory.Function_Declaration
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
          Nodes (10),
          Nodes (11),
          None,
          No_Token,
          None,
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (12));

      when 1243 =>

   Nodes (1) := Self.Factory.Function_Declaration
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
          Nodes (10),
          No_Token,
          Nodes (11),
          No_Token,
          None,
          No_Token,
          Nodes (12),
          Nodes (13));

      when 1244 =>

   Nodes (1) := Self.Factory.Function_Declaration
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
          Nodes (10),
          No_Token,
          Nodes (11),
          No_Token,
          None,
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (12));

      when 1245 =>

   Nodes (1) := Self.Factory.Function_Declaration
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
          No_Token,
          No_Token,
          None,
          Nodes (10),
          Nodes (11),
          No_Token,
          Nodes (12),
          Nodes (13));

      when 1246 =>

   Nodes (1) := Self.Factory.Function_Declaration
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
          No_Token,
          No_Token,
          None,
          Nodes (10),
          Nodes (11),
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (12));

      when 1247 =>

   Nodes (1) := Self.Factory.Function_Declaration
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
          Nodes (10),
          No_Token,
          None,
          No_Token,
          None,
          Nodes (11),
          Nodes (12),
          Nodes (13));

      when 1248 =>

   Nodes (1) := Self.Factory.Function_Declaration
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
          Nodes (10),
          No_Token,
          None,
          No_Token,
          None,
          Nodes (11),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (12));

      when 1249 =>

   Nodes (1) := Self.Factory.Function_Declaration
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
          No_Token,
          No_Token,
          None,
          No_Token,
          None,
          No_Token,
          Nodes (10),
          Nodes (11));

      when 1250 =>

   Nodes (1) := Self.Factory.Function_Declaration
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
          No_Token,
          No_Token,
          None,
          No_Token,
          None,
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (10));

      when 1251 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          No_Token,
          No_Token,
          Nodes (7),
          Nodes (8),
          Nodes (9),
          None,
          No_Token,
          None,
          No_Token,
          Nodes (10),
          Nodes (11));

      when 1252 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          No_Token,
          No_Token,
          Nodes (7),
          Nodes (8),
          Nodes (9),
          None,
          No_Token,
          None,
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (10));

      when 1253 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          No_Token,
          No_Token,
          Nodes (7),
          Nodes (8),
          No_Token,
          Nodes (9),
          No_Token,
          None,
          No_Token,
          Nodes (10),
          Nodes (11));

      when 1254 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          No_Token,
          No_Token,
          Nodes (7),
          Nodes (8),
          No_Token,
          Nodes (9),
          No_Token,
          None,
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (10));

      when 1255 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          No_Token,
          No_Token,
          Nodes (7),
          No_Token,
          No_Token,
          None,
          Nodes (8),
          Nodes (9),
          No_Token,
          Nodes (10),
          Nodes (11));

      when 1256 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          No_Token,
          No_Token,
          Nodes (7),
          No_Token,
          No_Token,
          None,
          Nodes (8),
          Nodes (9),
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (10));

      when 1257 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          No_Token,
          No_Token,
          Nodes (7),
          Nodes (8),
          No_Token,
          None,
          No_Token,
          None,
          Nodes (9),
          Nodes (10),
          Nodes (11));

      when 1258 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          No_Token,
          No_Token,
          Nodes (7),
          Nodes (8),
          No_Token,
          None,
          No_Token,
          None,
          Nodes (9),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (10));

      when 1259 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          No_Token,
          No_Token,
          Nodes (7),
          No_Token,
          No_Token,
          None,
          No_Token,
          None,
          No_Token,
          Nodes (8),
          Nodes (9));

      when 1260 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          No_Token,
          No_Token,
          Nodes (7),
          No_Token,
          No_Token,
          None,
          No_Token,
          None,
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (8));

      when 1261 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          None,
          No_Token,
          None,
          No_Token,
          Nodes (9),
          Nodes (10));

      when 1262 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          None,
          No_Token,
          None,
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (9));

      when 1263 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          No_Token,
          Nodes (8),
          No_Token,
          None,
          No_Token,
          Nodes (9),
          Nodes (10));

      when 1264 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          No_Token,
          Nodes (8),
          No_Token,
          None,
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (9));

      when 1265 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          No_Token,
          No_Token,
          None,
          Nodes (7),
          Nodes (8),
          No_Token,
          Nodes (9),
          Nodes (10));

      when 1266 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          No_Token,
          No_Token,
          None,
          Nodes (7),
          Nodes (8),
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (9));

      when 1267 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          No_Token,
          None,
          No_Token,
          None,
          Nodes (8),
          Nodes (9),
          Nodes (10));

      when 1268 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          No_Token,
          None,
          No_Token,
          None,
          Nodes (8),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (9));

      when 1269 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          No_Token,
          No_Token,
          None,
          No_Token,
          None,
          No_Token,
          Nodes (7),
          Nodes (8));

      when 1270 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          No_Token,
          No_Token,
          None,
          No_Token,
          None,
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (7));

      when 1271 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (3),
          No_Token,
          No_Token,
          Nodes (4),
          Nodes (5),
          Nodes (6),
          None,
          No_Token,
          None,
          No_Token,
          Nodes (7),
          Nodes (8));

      when 1272 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (3),
          No_Token,
          No_Token,
          Nodes (4),
          Nodes (5),
          Nodes (6),
          None,
          No_Token,
          None,
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (7));

      when 1273 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (3),
          No_Token,
          No_Token,
          Nodes (4),
          Nodes (5),
          No_Token,
          Nodes (6),
          No_Token,
          None,
          No_Token,
          Nodes (7),
          Nodes (8));

      when 1274 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (3),
          No_Token,
          No_Token,
          Nodes (4),
          Nodes (5),
          No_Token,
          Nodes (6),
          No_Token,
          None,
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (7));

      when 1275 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (3),
          No_Token,
          No_Token,
          Nodes (4),
          No_Token,
          No_Token,
          None,
          Nodes (5),
          Nodes (6),
          No_Token,
          Nodes (7),
          Nodes (8));

      when 1276 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (3),
          No_Token,
          No_Token,
          Nodes (4),
          No_Token,
          No_Token,
          None,
          Nodes (5),
          Nodes (6),
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (7));

      when 1277 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (3),
          No_Token,
          No_Token,
          Nodes (4),
          Nodes (5),
          No_Token,
          None,
          No_Token,
          None,
          Nodes (6),
          Nodes (7),
          Nodes (8));

      when 1278 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (3),
          No_Token,
          No_Token,
          Nodes (4),
          Nodes (5),
          No_Token,
          None,
          No_Token,
          None,
          Nodes (6),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (7));

      when 1279 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (3),
          No_Token,
          No_Token,
          Nodes (4),
          No_Token,
          No_Token,
          None,
          No_Token,
          None,
          No_Token,
          Nodes (5),
          Nodes (6));

      when 1280 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (3),
          No_Token,
          No_Token,
          Nodes (4),
          No_Token,
          No_Token,
          None,
          No_Token,
          None,
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (5));

      when 1281 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          No_Token,
          No_Token,
          Nodes (9),
          Nodes (10),
          Nodes (11),
          None,
          No_Token,
          None,
          No_Token,
          Nodes (12),
          Nodes (13));

      when 1282 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          No_Token,
          No_Token,
          Nodes (9),
          Nodes (10),
          Nodes (11),
          None,
          No_Token,
          None,
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (12));

      when 1283 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          No_Token,
          No_Token,
          Nodes (9),
          Nodes (10),
          No_Token,
          Nodes (11),
          No_Token,
          None,
          No_Token,
          Nodes (12),
          Nodes (13));

      when 1284 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          No_Token,
          No_Token,
          Nodes (9),
          Nodes (10),
          No_Token,
          Nodes (11),
          No_Token,
          None,
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (12));

      when 1285 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          No_Token,
          No_Token,
          Nodes (9),
          No_Token,
          No_Token,
          None,
          Nodes (10),
          Nodes (11),
          No_Token,
          Nodes (12),
          Nodes (13));

      when 1286 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          No_Token,
          No_Token,
          Nodes (9),
          No_Token,
          No_Token,
          None,
          Nodes (10),
          Nodes (11),
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (12));

      when 1287 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          No_Token,
          No_Token,
          Nodes (9),
          Nodes (10),
          No_Token,
          None,
          No_Token,
          None,
          Nodes (11),
          Nodes (12),
          Nodes (13));

      when 1288 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          No_Token,
          No_Token,
          Nodes (9),
          Nodes (10),
          No_Token,
          None,
          No_Token,
          None,
          Nodes (11),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (12));

      when 1289 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          No_Token,
          No_Token,
          Nodes (9),
          No_Token,
          No_Token,
          None,
          No_Token,
          None,
          No_Token,
          Nodes (10),
          Nodes (11));

      when 1290 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          No_Token,
          No_Token,
          Nodes (9),
          No_Token,
          No_Token,
          None,
          No_Token,
          None,
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (10));

      when 1291 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (5),
          No_Token,
          No_Token,
          Nodes (6),
          Nodes (7),
          Nodes (8),
          None,
          No_Token,
          None,
          No_Token,
          Nodes (9),
          Nodes (10));

      when 1292 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (5),
          No_Token,
          No_Token,
          Nodes (6),
          Nodes (7),
          Nodes (8),
          None,
          No_Token,
          None,
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (9));

      when 1293 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (5),
          No_Token,
          No_Token,
          Nodes (6),
          Nodes (7),
          No_Token,
          Nodes (8),
          No_Token,
          None,
          No_Token,
          Nodes (9),
          Nodes (10));

      when 1294 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (5),
          No_Token,
          No_Token,
          Nodes (6),
          Nodes (7),
          No_Token,
          Nodes (8),
          No_Token,
          None,
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (9));

      when 1295 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (5),
          No_Token,
          No_Token,
          Nodes (6),
          No_Token,
          No_Token,
          None,
          Nodes (7),
          Nodes (8),
          No_Token,
          Nodes (9),
          Nodes (10));

      when 1296 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (5),
          No_Token,
          No_Token,
          Nodes (6),
          No_Token,
          No_Token,
          None,
          Nodes (7),
          Nodes (8),
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (9));

      when 1297 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (5),
          No_Token,
          No_Token,
          Nodes (6),
          Nodes (7),
          No_Token,
          None,
          No_Token,
          None,
          Nodes (8),
          Nodes (9),
          Nodes (10));

      when 1298 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (5),
          No_Token,
          No_Token,
          Nodes (6),
          Nodes (7),
          No_Token,
          None,
          No_Token,
          None,
          Nodes (8),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (9));

      when 1299 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (5),
          No_Token,
          No_Token,
          Nodes (6),
          No_Token,
          No_Token,
          None,
          No_Token,
          None,
          No_Token,
          Nodes (7),
          Nodes (8));

      when 1300 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (5),
          No_Token,
          No_Token,
          Nodes (6),
          No_Token,
          No_Token,
          None,
          No_Token,
          None,
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (7));

      when 1301 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (No_Token,
          Nodes (1),
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
          None,
          No_Token,
          Nodes (11),
          Nodes (12));

      when 1302 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (No_Token,
          Nodes (1),
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
          None,
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (11));

      when 1303 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (No_Token,
          Nodes (1),
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
          None,
          No_Token,
          Nodes (11),
          Nodes (12));

      when 1304 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (No_Token,
          Nodes (1),
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
          None,
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (11));

      when 1305 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (No_Token,
          Nodes (1),
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
          Nodes (9),
          Nodes (10),
          No_Token,
          Nodes (11),
          Nodes (12));

      when 1306 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (No_Token,
          Nodes (1),
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
          Nodes (9),
          Nodes (10),
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (11));

      when 1307 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (No_Token,
          Nodes (1),
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
          No_Token,
          None,
          Nodes (10),
          Nodes (11),
          Nodes (12));

      when 1308 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (No_Token,
          Nodes (1),
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
          No_Token,
          None,
          Nodes (10),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (11));

      when 1309 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (No_Token,
          Nodes (1),
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
          None,
          No_Token,
          Nodes (9),
          Nodes (10));

      when 1310 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (No_Token,
          Nodes (1),
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
          None,
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (9));

      when 1311 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (4),
          No_Token,
          No_Token,
          Nodes (5),
          Nodes (6),
          Nodes (7),
          None,
          No_Token,
          None,
          No_Token,
          Nodes (8),
          Nodes (9));

      when 1312 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (4),
          No_Token,
          No_Token,
          Nodes (5),
          Nodes (6),
          Nodes (7),
          None,
          No_Token,
          None,
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (8));

      when 1313 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (4),
          No_Token,
          No_Token,
          Nodes (5),
          Nodes (6),
          No_Token,
          Nodes (7),
          No_Token,
          None,
          No_Token,
          Nodes (8),
          Nodes (9));

      when 1314 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (4),
          No_Token,
          No_Token,
          Nodes (5),
          Nodes (6),
          No_Token,
          Nodes (7),
          No_Token,
          None,
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (8));

      when 1315 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (4),
          No_Token,
          No_Token,
          Nodes (5),
          No_Token,
          No_Token,
          None,
          Nodes (6),
          Nodes (7),
          No_Token,
          Nodes (8),
          Nodes (9));

      when 1316 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (4),
          No_Token,
          No_Token,
          Nodes (5),
          No_Token,
          No_Token,
          None,
          Nodes (6),
          Nodes (7),
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (8));

      when 1317 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (4),
          No_Token,
          No_Token,
          Nodes (5),
          Nodes (6),
          No_Token,
          None,
          No_Token,
          None,
          Nodes (7),
          Nodes (8),
          Nodes (9));

      when 1318 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (4),
          No_Token,
          No_Token,
          Nodes (5),
          Nodes (6),
          No_Token,
          None,
          No_Token,
          None,
          Nodes (7),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (8));

      when 1319 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (4),
          No_Token,
          No_Token,
          Nodes (5),
          No_Token,
          No_Token,
          None,
          No_Token,
          None,
          No_Token,
          Nodes (6),
          Nodes (7));

      when 1320 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (4),
          No_Token,
          No_Token,
          Nodes (5),
          No_Token,
          No_Token,
          None,
          No_Token,
          None,
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (6));

      when 1321 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          No_Token,
          No_Token,
          Nodes (7),
          Nodes (8),
          Nodes (9),
          None,
          No_Token,
          None,
          No_Token,
          Nodes (10),
          Nodes (11));

      when 1322 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          No_Token,
          No_Token,
          Nodes (7),
          Nodes (8),
          Nodes (9),
          None,
          No_Token,
          None,
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (10));

      when 1323 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          No_Token,
          No_Token,
          Nodes (7),
          Nodes (8),
          No_Token,
          Nodes (9),
          No_Token,
          None,
          No_Token,
          Nodes (10),
          Nodes (11));

      when 1324 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          No_Token,
          No_Token,
          Nodes (7),
          Nodes (8),
          No_Token,
          Nodes (9),
          No_Token,
          None,
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (10));

      when 1325 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          No_Token,
          No_Token,
          Nodes (7),
          No_Token,
          No_Token,
          None,
          Nodes (8),
          Nodes (9),
          No_Token,
          Nodes (10),
          Nodes (11));

      when 1326 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          No_Token,
          No_Token,
          Nodes (7),
          No_Token,
          No_Token,
          None,
          Nodes (8),
          Nodes (9),
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (10));

      when 1327 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          No_Token,
          No_Token,
          Nodes (7),
          Nodes (8),
          No_Token,
          None,
          No_Token,
          None,
          Nodes (9),
          Nodes (10),
          Nodes (11));

      when 1328 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          No_Token,
          No_Token,
          Nodes (7),
          Nodes (8),
          No_Token,
          None,
          No_Token,
          None,
          Nodes (9),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (10));

      when 1329 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          No_Token,
          No_Token,
          Nodes (7),
          No_Token,
          No_Token,
          None,
          No_Token,
          None,
          No_Token,
          Nodes (8),
          Nodes (9));

      when 1330 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          No_Token,
          No_Token,
          Nodes (7),
          No_Token,
          No_Token,
          None,
          No_Token,
          None,
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (8));

      when 1331 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (3),
          No_Token,
          No_Token,
          Nodes (4),
          Nodes (5),
          Nodes (6),
          None,
          No_Token,
          None,
          No_Token,
          Nodes (7),
          Nodes (8));

      when 1332 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (3),
          No_Token,
          No_Token,
          Nodes (4),
          Nodes (5),
          Nodes (6),
          None,
          No_Token,
          None,
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (7));

      when 1333 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (3),
          No_Token,
          No_Token,
          Nodes (4),
          Nodes (5),
          No_Token,
          Nodes (6),
          No_Token,
          None,
          No_Token,
          Nodes (7),
          Nodes (8));

      when 1334 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (3),
          No_Token,
          No_Token,
          Nodes (4),
          Nodes (5),
          No_Token,
          Nodes (6),
          No_Token,
          None,
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (7));

      when 1335 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (3),
          No_Token,
          No_Token,
          Nodes (4),
          No_Token,
          No_Token,
          None,
          Nodes (5),
          Nodes (6),
          No_Token,
          Nodes (7),
          Nodes (8));

      when 1336 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (3),
          No_Token,
          No_Token,
          Nodes (4),
          No_Token,
          No_Token,
          None,
          Nodes (5),
          Nodes (6),
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (7));

      when 1337 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (3),
          No_Token,
          No_Token,
          Nodes (4),
          Nodes (5),
          No_Token,
          None,
          No_Token,
          None,
          Nodes (6),
          Nodes (7),
          Nodes (8));

      when 1338 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (3),
          No_Token,
          No_Token,
          Nodes (4),
          Nodes (5),
          No_Token,
          None,
          No_Token,
          None,
          Nodes (6),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (7));

      when 1339 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (3),
          No_Token,
          No_Token,
          Nodes (4),
          No_Token,
          No_Token,
          None,
          No_Token,
          None,
          No_Token,
          Nodes (5),
          Nodes (6));

      when 1340 =>

   Nodes (1) := Self.Factory.Function_Declaration
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (3),
          No_Token,
          No_Token,
          Nodes (4),
          No_Token,
          No_Token,
          None,
          No_Token,
          None,
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (5));

      when 1341 =>
         null;
      when 1342 =>
         null;
      when 1343 =>

   Nodes (1) := Self.Factory.Generic_Association
         (Nodes (1),
          Nodes (2),
          Nodes (3),
           No_Token);

      when 1344 =>

   Nodes (1) := Self.Factory.Generic_Association
         (None,
          No_Token,
          Nodes (1),
           No_Token);

      when 1345 =>
         null;
      when 1346 =>
         null;
      when 1347 =>
         null;
      when 1348 =>

  declare
     List : Node := Nodes (1);
  begin
     Self.Factory.Append_Generic_Formal
        (List, Nodes (2));
     Nodes (1) := List;
  end;

      when 1349 =>

  declare
     List : Node := Self.
       Factory.Generic_Formal_Sequence;
  begin
     Self.Factory.Append_Generic_Formal
        (List, Nodes (1));
     Nodes (1) := List;
  end;

      when 1350 =>
         null;
      when 1351 =>
         null;
      when 1352 =>
         null;
      when 1353 =>
         null;
      when 1354 =>
         null;
      when 1355 =>
         null;
      when 1356 =>

   Nodes (1) := Self.Factory.Generic_Function_Declaration
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

      when 1357 =>

   Nodes (1) := Self.Factory.Generic_Function_Declaration
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
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (12));

      when 1358 =>

   Nodes (1) := Self.Factory.Generic_Function_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          No_Token,
          No_Token,
          Nodes (9),
          Nodes (10),
          Nodes (11));

      when 1359 =>

   Nodes (1) := Self.Factory.Generic_Function_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          No_Token,
          No_Token,
          Nodes (9),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (10));

      when 1360 =>

   Nodes (1) := Self.Factory.Generic_Function_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          Nodes (9),
          Nodes (10));

      when 1361 =>

   Nodes (1) := Self.Factory.Generic_Function_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (9));

      when 1362 =>

   Nodes (1) := Self.Factory.Generic_Function_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (5),
          No_Token,
          No_Token,
          Nodes (6),
          Nodes (7),
          Nodes (8));

      when 1363 =>

   Nodes (1) := Self.Factory.Generic_Function_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (5),
          No_Token,
          No_Token,
          Nodes (6),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (7));

      when 1364 =>

   Nodes (1) := Self.Factory.Generic_Function_Declaration
         (Nodes (1),
          (Self.Factory.Generic_Formal_Sequence),
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

      when 1365 =>

   Nodes (1) := Self.Factory.Generic_Function_Declaration
         (Nodes (1),
          (Self.Factory.Generic_Formal_Sequence),
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

      when 1366 =>

   Nodes (1) := Self.Factory.Generic_Function_Declaration
         (Nodes (1),
          (Self.Factory.Generic_Formal_Sequence),
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
          Nodes (10));

      when 1367 =>

   Nodes (1) := Self.Factory.Generic_Function_Declaration
         (Nodes (1),
          (Self.Factory.Generic_Formal_Sequence),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          No_Token,
          No_Token,
          Nodes (8),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (9));

      when 1368 =>

   Nodes (1) := Self.Factory.Generic_Function_Declaration
         (Nodes (1),
          (Self.Factory.Generic_Formal_Sequence),
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          Nodes (9));

      when 1369 =>

   Nodes (1) := Self.Factory.Generic_Function_Declaration
         (Nodes (1),
          (Self.Factory.Generic_Formal_Sequence),
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (8));

      when 1370 =>

   Nodes (1) := Self.Factory.Generic_Function_Declaration
         (Nodes (1),
          (Self.Factory.Generic_Formal_Sequence),
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (4),
          No_Token,
          No_Token,
          Nodes (5),
          Nodes (6),
          Nodes (7));

      when 1371 =>

   Nodes (1) := Self.Factory.Generic_Function_Declaration
         (Nodes (1),
          (Self.Factory.Generic_Formal_Sequence),
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (4),
          No_Token,
          No_Token,
          Nodes (5),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (6));

      when 1372 =>

   Nodes (1) := Self.Factory.Generic_Function_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          No_Token,
          No_Token,
          Nodes (9),
          Nodes (10),
          Nodes (11));

      when 1373 =>

   Nodes (1) := Self.Factory.Generic_Function_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          No_Token,
          No_Token,
          Nodes (9),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (10));

      when 1374 =>

   Nodes (1) := Self.Factory.Generic_Function_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (5),
          No_Token,
          No_Token,
          Nodes (6),
          Nodes (7),
          Nodes (8));

      when 1375 =>

   Nodes (1) := Self.Factory.Generic_Function_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (5),
          No_Token,
          No_Token,
          Nodes (6),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (7));

      when 1376 =>

   Nodes (1) := Self.Factory.Generic_Function_Declaration
         (Nodes (1),
          (Self.Factory.Generic_Formal_Sequence),
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
          Nodes (10));

      when 1377 =>

   Nodes (1) := Self.Factory.Generic_Function_Declaration
         (Nodes (1),
          (Self.Factory.Generic_Formal_Sequence),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          No_Token,
          No_Token,
          Nodes (8),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (9));

      when 1378 =>

   Nodes (1) := Self.Factory.Generic_Function_Declaration
         (Nodes (1),
          (Self.Factory.Generic_Formal_Sequence),
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (4),
          No_Token,
          No_Token,
          Nodes (5),
          Nodes (6),
          Nodes (7));

      when 1379 =>

   Nodes (1) := Self.Factory.Generic_Function_Declaration
         (Nodes (1),
          (Self.Factory.Generic_Formal_Sequence),
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (4),
          No_Token,
          No_Token,
          Nodes (5),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (6));

      when 1380 =>

   Nodes (1) :=
      Self.Factory.Package_Instantiation
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          Nodes (9),
          Nodes (10));

      when 1381 =>

   Nodes (1) :=
      Self.Factory.Package_Instantiation
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (9));

      when 1382 =>

   Nodes (1) :=
      Self.Factory.Package_Instantiation
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          No_Token,
          (Self.Factory.Generic_Association_Sequence),
          No_Token,
          Nodes (6),
          Nodes (7));

      when 1383 =>

   Nodes (1) :=
      Self.Factory.Package_Instantiation
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          No_Token,
          (Self.Factory.Generic_Association_Sequence),
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (6));

      when 1384 =>

   Nodes (1) :=
      Self.Factory.Function_Instantiation
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

      when 1385 =>

   Nodes (1) :=
      Self.Factory.Function_Instantiation
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

      when 1386 =>

   Nodes (1) :=
      Self.Factory.Function_Instantiation
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          No_Token,
          (Self.Factory.Generic_Association_Sequence),
          No_Token,
          Nodes (8),
          Nodes (9));

      when 1387 =>

   Nodes (1) :=
      Self.Factory.Function_Instantiation
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          No_Token,
          (Self.Factory.Generic_Association_Sequence),
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (8));

      when 1388 =>

   Nodes (1) :=
      Self.Factory.Function_Instantiation
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

      when 1389 =>

   Nodes (1) :=
      Self.Factory.Function_Instantiation
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

      when 1390 =>

   Nodes (1) :=
      Self.Factory.Function_Instantiation
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          No_Token,
          (Self.Factory.Generic_Association_Sequence),
          No_Token,
          Nodes (7),
          Nodes (8));

      when 1391 =>

   Nodes (1) :=
      Self.Factory.Function_Instantiation
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          No_Token,
          (Self.Factory.Generic_Association_Sequence),
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (7));

      when 1392 =>

   Nodes (1) :=
      Self.Factory.Function_Instantiation
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

      when 1393 =>

   Nodes (1) :=
      Self.Factory.Function_Instantiation
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

      when 1394 =>

   Nodes (1) :=
      Self.Factory.Function_Instantiation
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          No_Token,
          (Self.Factory.Generic_Association_Sequence),
          No_Token,
          Nodes (6),
          Nodes (7));

      when 1395 =>

   Nodes (1) :=
      Self.Factory.Function_Instantiation
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          No_Token,
          (Self.Factory.Generic_Association_Sequence),
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (6));

      when 1396 =>

   Nodes (1) :=
      Self.Factory.Procedure_Instantiation
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

      when 1397 =>

   Nodes (1) :=
      Self.Factory.Procedure_Instantiation
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

      when 1398 =>

   Nodes (1) :=
      Self.Factory.Procedure_Instantiation
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          No_Token,
          (Self.Factory.Generic_Association_Sequence),
          No_Token,
          Nodes (8),
          Nodes (9));

      when 1399 =>

   Nodes (1) :=
      Self.Factory.Procedure_Instantiation
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          No_Token,
          (Self.Factory.Generic_Association_Sequence),
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (8));

      when 1400 =>

   Nodes (1) :=
      Self.Factory.Procedure_Instantiation
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

      when 1401 =>

   Nodes (1) :=
      Self.Factory.Procedure_Instantiation
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

      when 1402 =>

   Nodes (1) :=
      Self.Factory.Procedure_Instantiation
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          No_Token,
          (Self.Factory.Generic_Association_Sequence),
          No_Token,
          Nodes (7),
          Nodes (8));

      when 1403 =>

   Nodes (1) :=
      Self.Factory.Procedure_Instantiation
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          No_Token,
          (Self.Factory.Generic_Association_Sequence),
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (7));

      when 1404 =>

   Nodes (1) :=
      Self.Factory.Procedure_Instantiation
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

      when 1405 =>

   Nodes (1) :=
      Self.Factory.Procedure_Instantiation
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

      when 1406 =>

   Nodes (1) :=
      Self.Factory.Procedure_Instantiation
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          No_Token,
          (Self.Factory.Generic_Association_Sequence),
          No_Token,
          Nodes (6),
          Nodes (7));

      when 1407 =>

   Nodes (1) :=
      Self.Factory.Procedure_Instantiation
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          No_Token,
          (Self.Factory.Generic_Association_Sequence),
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (6));

      when 1408 =>

   Nodes (1) :=
      Self.Factory.Generic_Package_Declaration
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

      when 1409 =>

   Nodes (1) :=
      Self.Factory.Generic_Package_Declaration
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
          None,
          Nodes (11));

      when 1410 =>

   Nodes (1) :=
      Self.Factory.Generic_Package_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          (Self.Factory.Basic_Declarative_Item_Sequence),
          Nodes (9),
          Nodes (10),
          Nodes (11));

      when 1411 =>

   Nodes (1) :=
      Self.Factory.Generic_Package_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          (Self.Factory.Basic_Declarative_Item_Sequence),
          Nodes (9),
          None,
          Nodes (10));

      when 1412 =>

   Nodes (1) :=
      Self.Factory.Generic_Package_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          No_Token,
          (Self.Factory.Basic_Declarative_Item_Sequence),
          Nodes (8),
          Nodes (9),
          Nodes (10));

      when 1413 =>

   Nodes (1) :=
      Self.Factory.Generic_Package_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          No_Token,
          (Self.Factory.Basic_Declarative_Item_Sequence),
          Nodes (8),
          None,
          Nodes (9));

      when 1414 =>

   Nodes (1) :=
      Self.Factory.Generic_Package_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          (Self.Factory.Basic_Declarative_Item_Sequence),
          Nodes (7),
          Nodes (8),
          Nodes (9),
          Nodes (10),
          Nodes (11));

      when 1415 =>

   Nodes (1) :=
      Self.Factory.Generic_Package_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          (Self.Factory.Basic_Declarative_Item_Sequence),
          Nodes (7),
          Nodes (8),
          Nodes (9),
          None,
          Nodes (10));

      when 1416 =>

   Nodes (1) :=
      Self.Factory.Generic_Package_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          (Self.Factory.Basic_Declarative_Item_Sequence),
          Nodes (7),
          (Self.Factory.Basic_Declarative_Item_Sequence),
          Nodes (8),
          Nodes (9),
          Nodes (10));

      when 1417 =>

   Nodes (1) :=
      Self.Factory.Generic_Package_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          (Self.Factory.Basic_Declarative_Item_Sequence),
          Nodes (7),
          (Self.Factory.Basic_Declarative_Item_Sequence),
          Nodes (8),
          None,
          Nodes (9));

      when 1418 =>

   Nodes (1) :=
      Self.Factory.Generic_Package_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          (Self.Factory.Basic_Declarative_Item_Sequence),
          No_Token,
          (Self.Factory.Basic_Declarative_Item_Sequence),
          Nodes (7),
          Nodes (8),
          Nodes (9));

      when 1419 =>

   Nodes (1) :=
      Self.Factory.Generic_Package_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          (Self.Factory.Basic_Declarative_Item_Sequence),
          No_Token,
          (Self.Factory.Basic_Declarative_Item_Sequence),
          Nodes (7),
          None,
          Nodes (8));

      when 1420 =>

   Nodes (1) :=
      Self.Factory.Generic_Package_Declaration
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
          Nodes (10),
          Nodes (11));

      when 1421 =>

   Nodes (1) :=
      Self.Factory.Generic_Package_Declaration
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
          None,
          Nodes (10));

      when 1422 =>

   Nodes (1) :=
      Self.Factory.Generic_Package_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          (Self.Factory.Basic_Declarative_Item_Sequence),
          Nodes (8),
          Nodes (9),
          Nodes (10));

      when 1423 =>

   Nodes (1) :=
      Self.Factory.Generic_Package_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          (Self.Factory.Basic_Declarative_Item_Sequence),
          Nodes (8),
          None,
          Nodes (9));

      when 1424 =>

   Nodes (1) :=
      Self.Factory.Generic_Package_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (5),
          Nodes (6),
          No_Token,
          (Self.Factory.Basic_Declarative_Item_Sequence),
          Nodes (7),
          Nodes (8),
          Nodes (9));

      when 1425 =>

   Nodes (1) :=
      Self.Factory.Generic_Package_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (5),
          Nodes (6),
          No_Token,
          (Self.Factory.Basic_Declarative_Item_Sequence),
          Nodes (7),
          None,
          Nodes (8));

      when 1426 =>

   Nodes (1) :=
      Self.Factory.Generic_Package_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (5),
          (Self.Factory.Basic_Declarative_Item_Sequence),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          Nodes (9),
          Nodes (10));

      when 1427 =>

   Nodes (1) :=
      Self.Factory.Generic_Package_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (5),
          (Self.Factory.Basic_Declarative_Item_Sequence),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          None,
          Nodes (9));

      when 1428 =>

   Nodes (1) :=
      Self.Factory.Generic_Package_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (5),
          (Self.Factory.Basic_Declarative_Item_Sequence),
          Nodes (6),
          (Self.Factory.Basic_Declarative_Item_Sequence),
          Nodes (7),
          Nodes (8),
          Nodes (9));

      when 1429 =>

   Nodes (1) :=
      Self.Factory.Generic_Package_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (5),
          (Self.Factory.Basic_Declarative_Item_Sequence),
          Nodes (6),
          (Self.Factory.Basic_Declarative_Item_Sequence),
          Nodes (7),
          None,
          Nodes (8));

      when 1430 =>

   Nodes (1) :=
      Self.Factory.Generic_Package_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (5),
          (Self.Factory.Basic_Declarative_Item_Sequence),
          No_Token,
          (Self.Factory.Basic_Declarative_Item_Sequence),
          Nodes (6),
          Nodes (7),
          Nodes (8));

      when 1431 =>

   Nodes (1) :=
      Self.Factory.Generic_Package_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (5),
          (Self.Factory.Basic_Declarative_Item_Sequence),
          No_Token,
          (Self.Factory.Basic_Declarative_Item_Sequence),
          Nodes (6),
          None,
          Nodes (7));

      when 1432 =>

   Nodes (1) :=
      Self.Factory.Generic_Package_Declaration
         (Nodes (1),
          (Self.Factory.Generic_Formal_Sequence),
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

      when 1433 =>

   Nodes (1) :=
      Self.Factory.Generic_Package_Declaration
         (Nodes (1),
          (Self.Factory.Generic_Formal_Sequence),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          Nodes (9),
          None,
          Nodes (10));

      when 1434 =>

   Nodes (1) :=
      Self.Factory.Generic_Package_Declaration
         (Nodes (1),
          (Self.Factory.Generic_Formal_Sequence),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          (Self.Factory.Basic_Declarative_Item_Sequence),
          Nodes (8),
          Nodes (9),
          Nodes (10));

      when 1435 =>

   Nodes (1) :=
      Self.Factory.Generic_Package_Declaration
         (Nodes (1),
          (Self.Factory.Generic_Formal_Sequence),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          (Self.Factory.Basic_Declarative_Item_Sequence),
          Nodes (8),
          None,
          Nodes (9));

      when 1436 =>

   Nodes (1) :=
      Self.Factory.Generic_Package_Declaration
         (Nodes (1),
          (Self.Factory.Generic_Formal_Sequence),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          No_Token,
          (Self.Factory.Basic_Declarative_Item_Sequence),
          Nodes (7),
          Nodes (8),
          Nodes (9));

      when 1437 =>

   Nodes (1) :=
      Self.Factory.Generic_Package_Declaration
         (Nodes (1),
          (Self.Factory.Generic_Formal_Sequence),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          No_Token,
          (Self.Factory.Basic_Declarative_Item_Sequence),
          Nodes (7),
          None,
          Nodes (8));

      when 1438 =>

   Nodes (1) :=
      Self.Factory.Generic_Package_Declaration
         (Nodes (1),
          (Self.Factory.Generic_Formal_Sequence),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          (Self.Factory.Basic_Declarative_Item_Sequence),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          Nodes (9),
          Nodes (10));

      when 1439 =>

   Nodes (1) :=
      Self.Factory.Generic_Package_Declaration
         (Nodes (1),
          (Self.Factory.Generic_Formal_Sequence),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          (Self.Factory.Basic_Declarative_Item_Sequence),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          None,
          Nodes (9));

      when 1440 =>

   Nodes (1) :=
      Self.Factory.Generic_Package_Declaration
         (Nodes (1),
          (Self.Factory.Generic_Formal_Sequence),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          (Self.Factory.Basic_Declarative_Item_Sequence),
          Nodes (6),
          (Self.Factory.Basic_Declarative_Item_Sequence),
          Nodes (7),
          Nodes (8),
          Nodes (9));

      when 1441 =>

   Nodes (1) :=
      Self.Factory.Generic_Package_Declaration
         (Nodes (1),
          (Self.Factory.Generic_Formal_Sequence),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          (Self.Factory.Basic_Declarative_Item_Sequence),
          Nodes (6),
          (Self.Factory.Basic_Declarative_Item_Sequence),
          Nodes (7),
          None,
          Nodes (8));

      when 1442 =>

   Nodes (1) :=
      Self.Factory.Generic_Package_Declaration
         (Nodes (1),
          (Self.Factory.Generic_Formal_Sequence),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          (Self.Factory.Basic_Declarative_Item_Sequence),
          No_Token,
          (Self.Factory.Basic_Declarative_Item_Sequence),
          Nodes (6),
          Nodes (7),
          Nodes (8));

      when 1443 =>

   Nodes (1) :=
      Self.Factory.Generic_Package_Declaration
         (Nodes (1),
          (Self.Factory.Generic_Formal_Sequence),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          (Self.Factory.Basic_Declarative_Item_Sequence),
          No_Token,
          (Self.Factory.Basic_Declarative_Item_Sequence),
          Nodes (6),
          None,
          Nodes (7));

      when 1444 =>

   Nodes (1) :=
      Self.Factory.Generic_Package_Declaration
         (Nodes (1),
          (Self.Factory.Generic_Formal_Sequence),
          Nodes (2),
          Nodes (3),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          Nodes (9),
          Nodes (10));

      when 1445 =>

   Nodes (1) :=
      Self.Factory.Generic_Package_Declaration
         (Nodes (1),
          (Self.Factory.Generic_Formal_Sequence),
          Nodes (2),
          Nodes (3),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          None,
          Nodes (9));

      when 1446 =>

   Nodes (1) :=
      Self.Factory.Generic_Package_Declaration
         (Nodes (1),
          (Self.Factory.Generic_Formal_Sequence),
          Nodes (2),
          Nodes (3),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          (Self.Factory.Basic_Declarative_Item_Sequence),
          Nodes (7),
          Nodes (8),
          Nodes (9));

      when 1447 =>

   Nodes (1) :=
      Self.Factory.Generic_Package_Declaration
         (Nodes (1),
          (Self.Factory.Generic_Formal_Sequence),
          Nodes (2),
          Nodes (3),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          (Self.Factory.Basic_Declarative_Item_Sequence),
          Nodes (7),
          None,
          Nodes (8));

      when 1448 =>

   Nodes (1) :=
      Self.Factory.Generic_Package_Declaration
         (Nodes (1),
          (Self.Factory.Generic_Formal_Sequence),
          Nodes (2),
          Nodes (3),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (4),
          Nodes (5),
          No_Token,
          (Self.Factory.Basic_Declarative_Item_Sequence),
          Nodes (6),
          Nodes (7),
          Nodes (8));

      when 1449 =>

   Nodes (1) :=
      Self.Factory.Generic_Package_Declaration
         (Nodes (1),
          (Self.Factory.Generic_Formal_Sequence),
          Nodes (2),
          Nodes (3),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (4),
          Nodes (5),
          No_Token,
          (Self.Factory.Basic_Declarative_Item_Sequence),
          Nodes (6),
          None,
          Nodes (7));

      when 1450 =>

   Nodes (1) :=
      Self.Factory.Generic_Package_Declaration
         (Nodes (1),
          (Self.Factory.Generic_Formal_Sequence),
          Nodes (2),
          Nodes (3),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (4),
          (Self.Factory.Basic_Declarative_Item_Sequence),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          Nodes (9));

      when 1451 =>

   Nodes (1) :=
      Self.Factory.Generic_Package_Declaration
         (Nodes (1),
          (Self.Factory.Generic_Formal_Sequence),
          Nodes (2),
          Nodes (3),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (4),
          (Self.Factory.Basic_Declarative_Item_Sequence),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          None,
          Nodes (8));

      when 1452 =>

   Nodes (1) :=
      Self.Factory.Generic_Package_Declaration
         (Nodes (1),
          (Self.Factory.Generic_Formal_Sequence),
          Nodes (2),
          Nodes (3),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (4),
          (Self.Factory.Basic_Declarative_Item_Sequence),
          Nodes (5),
          (Self.Factory.Basic_Declarative_Item_Sequence),
          Nodes (6),
          Nodes (7),
          Nodes (8));

      when 1453 =>

   Nodes (1) :=
      Self.Factory.Generic_Package_Declaration
         (Nodes (1),
          (Self.Factory.Generic_Formal_Sequence),
          Nodes (2),
          Nodes (3),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (4),
          (Self.Factory.Basic_Declarative_Item_Sequence),
          Nodes (5),
          (Self.Factory.Basic_Declarative_Item_Sequence),
          Nodes (6),
          None,
          Nodes (7));

      when 1454 =>

   Nodes (1) :=
      Self.Factory.Generic_Package_Declaration
         (Nodes (1),
          (Self.Factory.Generic_Formal_Sequence),
          Nodes (2),
          Nodes (3),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (4),
          (Self.Factory.Basic_Declarative_Item_Sequence),
          No_Token,
          (Self.Factory.Basic_Declarative_Item_Sequence),
          Nodes (5),
          Nodes (6),
          Nodes (7));

      when 1455 =>

   Nodes (1) :=
      Self.Factory.Generic_Package_Declaration
         (Nodes (1),
          (Self.Factory.Generic_Formal_Sequence),
          Nodes (2),
          Nodes (3),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (4),
          (Self.Factory.Basic_Declarative_Item_Sequence),
          No_Token,
          (Self.Factory.Basic_Declarative_Item_Sequence),
          Nodes (5),
          None,
          Nodes (6));

      when 1456 =>

   Nodes (1) :=
      Self.Factory.Generic_Procedure_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          Nodes (9));

      when 1457 =>

   Nodes (1) :=
      Self.Factory.Generic_Procedure_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (8));

      when 1458 =>

   Nodes (1) :=
      Self.Factory.Generic_Procedure_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (5),
          Nodes (6));

      when 1459 =>

   Nodes (1) :=
      Self.Factory.Generic_Procedure_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (5));

      when 1460 =>

   Nodes (1) :=
      Self.Factory.Generic_Procedure_Declaration
         (Nodes (1),
          (Self.Factory.Generic_Formal_Sequence),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8));

      when 1461 =>

   Nodes (1) :=
      Self.Factory.Generic_Procedure_Declaration
         (Nodes (1),
          (Self.Factory.Generic_Formal_Sequence),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (7));

      when 1462 =>

   Nodes (1) :=
      Self.Factory.Generic_Procedure_Declaration
         (Nodes (1),
          (Self.Factory.Generic_Formal_Sequence),
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (4),
          Nodes (5));

      when 1463 =>

   Nodes (1) :=
      Self.Factory.Generic_Procedure_Declaration
         (Nodes (1),
          (Self.Factory.Generic_Formal_Sequence),
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (4));

      when 1464 =>

   Nodes (1) :=
      Self.Factory.Generic_Package_Renaming
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8));

      when 1465 =>

   Nodes (1) :=
      Self.Factory.Generic_Package_Renaming
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (7));

      when 1466 =>

   Nodes (1) :=
      Self.Factory.Generic_Package_Renaming
         (Nodes (1),
          (Self.Factory.Generic_Formal_Sequence),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7));

      when 1467 =>

   Nodes (1) :=
      Self.Factory.Generic_Package_Renaming
         (Nodes (1),
          (Self.Factory.Generic_Formal_Sequence),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (6));

      when 1468 =>

   Nodes (1) :=
      Self.Factory.Generic_Procedure_Renaming
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8));

      when 1469 =>

   Nodes (1) :=
      Self.Factory.Generic_Procedure_Renaming
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (7));

      when 1470 =>

   Nodes (1) :=
      Self.Factory.Generic_Procedure_Renaming
         (Nodes (1),
          (Self.Factory.Generic_Formal_Sequence),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7));

      when 1471 =>

   Nodes (1) :=
      Self.Factory.Generic_Procedure_Renaming
         (Nodes (1),
          (Self.Factory.Generic_Formal_Sequence),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (6));

      when 1472 =>

   Nodes (1) :=
      Self.Factory.Generic_Function_Renaming
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8));

      when 1473 =>

   Nodes (1) :=
      Self.Factory.Generic_Function_Renaming
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (7));

      when 1474 =>

   Nodes (1) :=
      Self.Factory.Generic_Function_Renaming
         (Nodes (1),
          (Self.Factory.Generic_Formal_Sequence),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7));

      when 1475 =>

   Nodes (1) :=
      Self.Factory.Generic_Function_Renaming
         (Nodes (1),
          (Self.Factory.Generic_Formal_Sequence),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (6));

      when 1476 =>

   Nodes (1) :=
      Self.Factory.Goto_Statement
         (Nodes (1),
          Nodes (2),
          Nodes (3));

      when 1477 =>
  Nodes (1) := Self.Factory.Identifier
     (Nodes (1));

      when 1478 =>

   declare
      Path : constant Node :=
        Self.Factory.If_Expression_Path
          (Nodes (1),
           Nodes (2),
           Nodes (3),
           Nodes (4));
      Tail : constant Node :=
        Self.Factory.Else_Expression_Path
          (Nodes (6),
           Nodes (7));
      List : Node :=
        Nodes (5);
   begin
      Self.Factory.Prepend_If_Else_Expression_Path
        (List, Path);
      Self.Factory.Append_If_Else_Expression_Path
        (List, Tail);
      Nodes (1) := Self.Factory.If_Expression (List);
   end;

      when 1479 =>

   declare
      Path : constant Node :=
        Self.Factory.If_Expression_Path
          (Nodes (1),
           Nodes (2),
           Nodes (3),
          Nodes (4));
      List : Node :=
        Nodes (5);
   begin
      Self.Factory.Prepend_If_Else_Expression_Path
        (List, Path);
      Nodes (1) := Self.Factory.If_Expression (List);
   end;

      when 1480 =>

   declare
      Path : constant Node :=
        Self.Factory.If_Expression_Path
          (Nodes (1),
           Nodes (2),
           Nodes (3),
           Nodes (4));
      Tail : constant Node :=
        Self.Factory.Else_Expression_Path
          (Nodes (5),
           Nodes (6));
      List : Node :=
        Self.Factory.If_Else_Expression_Path_Sequence;
   begin
      Self.Factory.Prepend_If_Else_Expression_Path
        (List, Path);
      Self.Factory.Append_If_Else_Expression_Path
        (List, Tail);
      Nodes (1) := Self.Factory.If_Expression (List);
   end;

      when 1481 =>

   declare
      Path : constant Node :=
        Self.Factory.If_Expression_Path
          (Nodes (1),
           Nodes (2),
           Nodes (3),
          Nodes (4));
      List : Node :=
        Self.Factory.If_Else_Expression_Path_Sequence;
   begin
      Self.Factory.Prepend_If_Else_Expression_Path
        (List, Path);
      Nodes (1) := Self.Factory.If_Expression (List);
   end;

      when 1482 =>

   declare
      If_Item : constant Node :=
        Self.Factory.If_Path
          (Nodes (1),
           Nodes (2),
           Nodes (3),
           Nodes (4));
      Else_Item : constant Node :=
        Self.Factory.Else_Path
          (Nodes (6), Nodes (7));
      List : Node :=
        Nodes (5);
   begin
      Self.Factory.Prepend_If_Elsif_Else_Path
        (List, If_Item);
      Self.Factory.Append_If_Elsif_Else_Path
        (List, Else_Item);
      Nodes (1) := Self.Factory.If_Statement
        (List,
         Nodes (8),
         Nodes (9),
         Nodes (10));
   end;

      when 1483 =>

   declare
      Item : constant Node :=
        Self.Factory.If_Path
          (Nodes (1),
           Nodes (2),
           Nodes (3),
           Nodes (4));
      List : Node :=
        Nodes (5);
   begin
      Self.Factory.Prepend_If_Elsif_Else_Path
        (List, Item);
      Nodes (1) := Self.Factory.If_Statement
        (List,
         Nodes (6),
         Nodes (7),
         Nodes (8));
   end;

      when 1484 =>

   declare
      If_Item : constant Node :=
        Self.Factory.If_Path
          (Nodes (1),
           Nodes (2),
           Nodes (3),
           Nodes (4));
      Else_Item : constant Node :=
        Self.Factory.Else_Path
          (Nodes (5), Nodes (6));
      List : Node :=
        Self.Factory.If_Elsif_Else_Path_Sequence;
   begin
      Self.Factory.Prepend_If_Elsif_Else_Path
        (List, If_Item);
      Self.Factory.Append_If_Elsif_Else_Path
        (List, Else_Item);
      Nodes (1) := Self.Factory.If_Statement
        (List,
         Nodes (7),
         Nodes (8),
         Nodes (9));
   end;

      when 1485 =>

   declare
      Item : constant Node :=
        Self.Factory.If_Path
          (Nodes (1),
           Nodes (2),
           Nodes (3),
           Nodes (4));
      List : Node :=
        Self.Factory.If_Elsif_Else_Path_Sequence;
   begin
      Self.Factory.Prepend_If_Elsif_Else_Path
        (List, Item);
      Nodes (1) := Self.Factory.If_Statement
        (List,
         Nodes (5),
         Nodes (6),
         Nodes (7));
   end;

      when 1486 =>

   declare
      Def : constant Node  :=
        Self.Factory.Incomplete_Type_Definition
          (Nodes (5));
   begin
      Nodes (1) :=
         Self.Factory.Incomplete_Type_Declaration
           (Nodes (1),
            Nodes (2),
            Nodes (3),
            Nodes (4),
            Def,
            Nodes (6));
   end;

      when 1487 =>

   declare
      Def : constant Node  :=
        Self.Factory.Incomplete_Type_Definition
          (No_Token);
   begin
      Nodes (1) :=
         Self.Factory.Incomplete_Type_Declaration
           (Nodes (1),
            Nodes (2),
            Nodes (3),
            No_Token,
            Def,
            Nodes (4));
   end;

      when 1488 =>

   declare
      Def : constant Node  :=
        Self.Factory.Incomplete_Type_Definition
          (Nodes (4));
   begin
      Nodes (1) :=
         Self.Factory.Incomplete_Type_Declaration
           (Nodes (1),
            Nodes (2),
            None,
            Nodes (3),
            Def,
            Nodes (5));
   end;

      when 1489 =>

   declare
      Def : constant Node  :=
        Self.Factory.Incomplete_Type_Definition
          (No_Token);
   begin
      Nodes (1) :=
         Self.Factory.Incomplete_Type_Declaration
           (Nodes (1),
            Nodes (2),
            None,
            No_Token,
            Def,
            Nodes (3));
   end;

      when 1490 =>
         null;
      when 1491 =>

   declare
      List : Node := Nodes (1);
   begin
      Self.Factory.Append_Subtype_Mark
        (List, Nodes (3));
      Nodes (1) := List;
   end;

      when 1492 =>

   declare
      List : Node := Self.
        Factory.Subtype_Mark_Sequence;
   begin
      Self.Factory.Append_Subtype_Mark
        (List, Nodes (2));
      Nodes (1) := List;
   end;

      when 1493 =>
         null;
      when 1494 =>
         null;
      when 1495 =>
         null;
      when 1496 =>
         null;
      when 1497 =>

   declare
      List : Node :=
        Nodes (2);
   begin
      Self.Factory.Prepend_Subtype_Mark
        (List, Nodes (1));
      Nodes (1) := List;
   end;

      when 1498 =>

   declare
      List : Node :=
        Self.Factory.
        Subtype_Mark_Sequence;
   begin
      Self.Factory.Prepend_Subtype_Mark
        (List, Nodes (1));
      Nodes (1) := List;
   end;

      when 1499 =>

   Nodes (1) := Self.Factory.
     Interface_Type_Definition
       (Nodes (1),
        Nodes (2),
        Nodes (3));

      when 1500 =>

   Nodes (1) := Self.Factory.
     Interface_Type_Definition
       (Nodes (1),
        Nodes (2),
        (Self.Factory.Subtype_Mark_Sequence));

      when others =>
         raise Constraint_Error;
   end case;
end Program.Parsers.On_Reduce_1001;
