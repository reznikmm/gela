
with Program.Parsers.Nodes;
use Program.Parsers.Nodes;
pragma Style_Checks ("N");
procedure Program.Parsers.On_Reduce_1501
  (Self  : access Parse_Context;
   Prod  : Anagram.Grammars.Production_Index;
   Nodes : in out Program.Parsers.Nodes.Node_Array) is
begin
   case Prod is
      when 1501 =>

   Nodes (1) := Self.Factory.
     Interface_Type_Definition
       (No_Token,
        Nodes (1),
        Nodes (2));

      when 1502 =>

   Nodes (1) := Self.Factory.
     Interface_Type_Definition
       (No_Token,
        Nodes (1),
        (Self.Factory.Subtype_Mark_Sequence));

      when 1503 =>

   Nodes (1) := Self.Factory.
     Element_Iterator_Specification
       (Nodes (1),
        Nodes (2),
        Nodes (3),
        Nodes (4),
        Nodes (5),
        Nodes (6));

      when 1504 =>

   Nodes (1) := Self.Factory.
     Element_Iterator_Specification
       (Nodes (1),
        Nodes (2),
        Nodes (3),
        Nodes (4),
        No_Token,
        Nodes (5));

      when 1505 =>

   Nodes (1) := Self.Factory.
     Element_Iterator_Specification
       (Nodes (1),
        No_Token,
        None,
        Nodes (2),
        Nodes (3),
        Nodes (4));

      when 1506 =>

   Nodes (1) := Self.Factory.
     Element_Iterator_Specification
       (Nodes (1),
        No_Token,
        None,
        Nodes (2),
        No_Token,
        Nodes (3));

      when 1507 =>

   declare
      List : Node := Nodes (3);
   begin
      Self.Factory.Prepend_Discriminant_Specification
        (List, Nodes (2));
      Nodes (1) :=
        Self.Factory.Known_Discriminant_Part
          (Nodes (1),
           List,
           Nodes (4));
   end;

      when 1508 =>

   declare
      List : Node := Self.
        Factory.Discriminant_Specification_Sequence;
   begin
      Self.Factory.Prepend_Discriminant_Specification
        (List, Nodes (2));
      Nodes (1) :=
        Self.Factory.Known_Discriminant_Part
          (Nodes (1),
           List,
           Nodes (3));
   end;

      when 1509 =>

   Nodes (1) := Nodes (2);

      when 1510 =>

   declare
      List : Node := Nodes (1);
   begin
      Self.Factory.Append_Defining_Identifier
        (List, Nodes (2));
      Nodes (1) := List;
   end;

      when 1511 =>

   declare
      List : Node := Self.
        Factory.Defining_Identifier_Sequence;
   begin
      Self.Factory.Append_Defining_Identifier
        (List, Nodes (1));
      Nodes (1) := List;
   end;

      when 1512 =>

   Nodes (1) :=
      Self.Factory.Compilation_Unit_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3));

      when 1513 =>

   Nodes (1) :=
      Self.Factory.Compilation_Unit_Declaration
         (Nodes (1),
          No_Token,
          Nodes (2));

      when 1514 =>

   Nodes (1) :=
      Self.Factory.Compilation_Unit_Declaration
         ((Self.Factory.Context_Item_Sequence),
          Nodes (1),
          Nodes (2));

      when 1515 =>

   Nodes (1) :=
      Self.Factory.Compilation_Unit_Declaration
         ((Self.Factory.Context_Item_Sequence),
          No_Token,
          Nodes (1));

      when 1516 =>

   Nodes (1) :=
      Self.Factory.Compilation_Unit_Body
         (Nodes (1),
          Nodes (2));

      when 1517 =>

   Nodes (1) :=
      Self.Factory.Compilation_Unit_Body
         ((Self.Factory.Context_Item_Sequence),
          Nodes (1));

      when 1518 =>
         null;
      when 1519 =>
         null;
      when 1520 =>
         null;
      when 1521 =>
         null;
      when 1522 =>
         null;
      when 1523 =>
         null;
      when 1524 =>
         null;
      when 1525 =>
         null;
      when 1526 =>
         null;
      when 1527 =>
         null;
      when 1528 =>
         null;
      when 1529 =>
         null;
      when 1530 =>
  Nodes (1) :=
     Self.Factory.Loop_Parameter_Specification
       (Nodes (1),
        Nodes (2),
        Nodes (3),
        Nodes (4));

      when 1531 =>
  Nodes (1) :=
     Self.Factory.Loop_Parameter_Specification
       (Nodes (1),
        Nodes (2),
        No_Token,
        Nodes (3));

      when 1532 =>

   Nodes (1) :=
      Self.Factory.While_Loop_Statement
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

      when 1533 =>

   Nodes (1) :=
      Self.Factory.While_Loop_Statement
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          No_Token,
          Nodes (9));

      when 1534 =>

   Nodes (1) :=
      Self.Factory.For_Loop_Statement
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

      when 1535 =>

   Nodes (1) :=
      Self.Factory.For_Loop_Statement
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          No_Token,
          Nodes (9));

      when 1536 =>

   Nodes (1) :=
      Self.Factory.For_Loop_Statement
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

      when 1537 =>

   Nodes (1) :=
      Self.Factory.For_Loop_Statement
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          No_Token,
          Nodes (9));

      when 1538 =>

   Nodes (1) :=
      Self.Factory.Loop_Statement
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8));

      when 1539 =>

   Nodes (1) :=
      Self.Factory.Loop_Statement
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          No_Token,
          Nodes (7));

      when 1540 =>

   Nodes (1) :=
      Self.Factory.While_Loop_Statement
         (None,
          No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8));

      when 1541 =>

   Nodes (1) :=
      Self.Factory.While_Loop_Statement
         (None,
          No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          No_Token,
          Nodes (7));

      when 1542 =>

   Nodes (1) :=
      Self.Factory.For_Loop_Statement
         (None,
          No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8));

      when 1543 =>

   Nodes (1) :=
      Self.Factory.For_Loop_Statement
         (None,
          No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          No_Token,
          Nodes (7));

      when 1544 =>

   Nodes (1) :=
      Self.Factory.For_Loop_Statement
         (None,
          No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8));

      when 1545 =>

   Nodes (1) :=
      Self.Factory.For_Loop_Statement
         (None,
          No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          No_Token,
          Nodes (7));

      when 1546 =>

   Nodes (1) :=
      Self.Factory.Loop_Statement
         (None,
          No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6));

      when 1547 =>

   Nodes (1) :=
      Self.Factory.Loop_Statement
         (None,
          No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token,
          Nodes (5));

      when 1548 =>

   declare
      List : Node :=
        Nodes (1);
   begin
      Self.Factory.Append_Membership_Choice
        (List, Nodes (3));
      Nodes (1) := List;
   end;

      when 1549 =>

   declare
      List : Node :=
        Self.Factory.Membership_Choice_Sequence;
   begin
      Self.Factory.Append_Membership_Choice
        (List, Nodes (2));
      Nodes (1) := List;
   end;

      when 1550 =>
         null;
      when 1551 =>
         null;
      when 1552 =>

   declare
      List : Node :=
        Nodes (2);
   begin
      Self.Factory.Prepend_Membership_Choice
        (List, Nodes (1));
      Nodes (1) := List;
   end;

      when 1553 =>

   declare
      List : Node :=
        Self.Factory.Membership_Choice_Sequence;
   begin
      Self.Factory.Prepend_Membership_Choice
        (List, Nodes (1));
      Nodes (1) := List;
   end;

      when 1554 =>
  Nodes (1) := Self.Factory.Modular_Type_Definition
     (Nodes (1), Nodes (2));

      when 1555 =>
         null;
      when 1556 =>
         null;
      when 1557 =>
         null;
      when 1558 =>
         null;
      when 1559 =>
         null;
      when 1560 =>
  Nodes (1) := Self.Factory.Character_Literal
     (Nodes (1));

      when 1561 =>
         null;
      when 1562 =>

   declare
      List : Node :=
        Nodes (1);
   begin
      Self.Factory.Append_Name
        (List, Nodes (3));
      Nodes (1) := List;
   end;

      when 1563 =>

   declare
      List : Node :=
        Self.Factory.Name_Sequence;
   begin
      Self.Factory.Append_Name
        (List, Nodes (2));
      Nodes (1) := List;
   end;

      when 1564 =>
         null;
      when 1565 =>
         null;
      when 1566 =>

   Nodes (1) :=
      Self.Factory.Null_Statement
         (Nodes (1), Nodes (2));

      when 1567 =>

   Nodes (1) :=
      Self.Factory.Number_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6));

      when 1568 =>

   Nodes (1) :=
      Self.Factory.Object_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          Nodes (9));

      when 1569 =>

   Nodes (1) :=
      Self.Factory.Object_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (8));

      when 1570 =>

   Nodes (1) :=
      Self.Factory.Object_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          No_Token,
          None,
          Nodes (6),
          Nodes (7));

      when 1571 =>

   Nodes (1) :=
      Self.Factory.Object_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          No_Token,
          None,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (6));

      when 1572 =>

   Nodes (1) :=
      Self.Factory.Object_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8));

      when 1573 =>

   Nodes (1) :=
      Self.Factory.Object_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          Nodes (4),
          Nodes (5),
          Nodes (6),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (7));

      when 1574 =>

   Nodes (1) :=
      Self.Factory.Object_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          Nodes (4),
          No_Token,
          None,
          Nodes (5),
          Nodes (6));

      when 1575 =>

   Nodes (1) :=
      Self.Factory.Object_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          Nodes (4),
          No_Token,
          None,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (5));

      when 1576 =>

   Nodes (1) :=
      Self.Factory.Object_Declaration
         (Nodes (1),
          Nodes (2),
          No_Token,
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8));

      when 1577 =>

   Nodes (1) :=
      Self.Factory.Object_Declaration
         (Nodes (1),
          Nodes (2),
          No_Token,
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (7));

      when 1578 =>

   Nodes (1) :=
      Self.Factory.Object_Declaration
         (Nodes (1),
          Nodes (2),
          No_Token,
          Nodes (3),
          Nodes (4),
          No_Token,
          None,
          Nodes (5),
          Nodes (6));

      when 1579 =>

   Nodes (1) :=
      Self.Factory.Object_Declaration
         (Nodes (1),
          Nodes (2),
          No_Token,
          Nodes (3),
          Nodes (4),
          No_Token,
          None,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (5));

      when 1580 =>

   Nodes (1) :=
      Self.Factory.Object_Declaration
         (Nodes (1),
          Nodes (2),
          No_Token,
          No_Token,
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7));

      when 1581 =>

   Nodes (1) :=
      Self.Factory.Object_Declaration
         (Nodes (1),
          Nodes (2),
          No_Token,
          No_Token,
          Nodes (3),
          Nodes (4),
          Nodes (5),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (6));

      when 1582 =>

   Nodes (1) :=
      Self.Factory.Object_Declaration
         (Nodes (1),
          Nodes (2),
          No_Token,
          No_Token,
          Nodes (3),
          No_Token,
          None,
          Nodes (4),
          Nodes (5));

      when 1583 =>

   Nodes (1) :=
      Self.Factory.Object_Declaration
         (Nodes (1),
          Nodes (2),
          No_Token,
          No_Token,
          Nodes (3),
          No_Token,
          None,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (4));

      when 1584 =>
         null;
      when 1585 =>
         null;
      when 1586 =>
         null;
      when 1587 =>
         null;
      when 1588 =>
         null;
      when 1589 =>
         null;
      when 1590 =>

   Nodes (1) :=
      Self.Factory.Object_Renaming_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          Nodes (9));

      when 1591 =>

   Nodes (1) :=
      Self.Factory.Object_Renaming_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (8));

      when 1592 =>

   Nodes (1) :=
      Self.Factory.Object_Renaming_Declaration
         (Nodes (1),
          Nodes (2),
          No_Token,
          No_Token,
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7));

      when 1593 =>

   Nodes (1) :=
      Self.Factory.Object_Renaming_Declaration
         (Nodes (1),
          Nodes (2),
          No_Token,
          No_Token,
          Nodes (3),
          Nodes (4),
          Nodes (5),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (6));

      when 1594 =>

   Nodes (1) :=
      Self.Factory.Object_Renaming_Declaration
         (Nodes (1),
          Nodes (2),
          No_Token,
          No_Token,
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7));

      when 1595 =>

   Nodes (1) :=
      Self.Factory.Object_Renaming_Declaration
         (Nodes (1),
          Nodes (2),
          No_Token,
          No_Token,
          Nodes (3),
          Nodes (4),
          Nodes (5),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (6));

      when 1596 =>
  Nodes (1) := Self.Factory.Operator_Symbol
     (Nodes (1));

      when 1597 =>
  Nodes (1) := Self.Factory.Ordinary_Fixed_Point_Definition
     (Nodes (1),
      Nodes (2),
      Nodes (3));

      when 1598 =>
 Nodes (1) := Self.Factory.Others_Choice
     (Nodes (1));

      when 1599 =>

   Nodes (1) :=
      Self.Factory.Package_Body
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

      when 1600 =>

   Nodes (1) :=
      Self.Factory.Package_Body
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
          None,
          Nodes (12));

      when 1601 =>

   Nodes (1) :=
      Self.Factory.Package_Body
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

      when 1602 =>

   Nodes (1) :=
      Self.Factory.Package_Body
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
          None,
          Nodes (10));

      when 1603 =>

   Nodes (1) :=
      Self.Factory.Package_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (7),
          Nodes (8),
          Nodes (9));

      when 1604 =>

   Nodes (1) :=
      Self.Factory.Package_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (7),
          None,
          Nodes (8));

      when 1605 =>

   Nodes (1) :=
      Self.Factory.Package_Body
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

      when 1606 =>

   Nodes (1) :=
      Self.Factory.Package_Body
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
          None,
          Nodes (11));

      when 1607 =>

   Nodes (1) :=
      Self.Factory.Package_Body
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

      when 1608 =>

   Nodes (1) :=
      Self.Factory.Package_Body
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
          None,
          Nodes (9));

      when 1609 =>

   Nodes (1) :=
      Self.Factory.Package_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          (Self.Factory.Declarative_Item_Sequence),
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (6),
          Nodes (7),
          Nodes (8));

      when 1610 =>

   Nodes (1) :=
      Self.Factory.Package_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          (Self.Factory.Declarative_Item_Sequence),
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (6),
          None,
          Nodes (7));

      when 1611 =>

   Nodes (1) :=
      Self.Factory.Package_Body
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

      when 1612 =>

   Nodes (1) :=
      Self.Factory.Package_Body
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
          None,
          Nodes (11));

      when 1613 =>

   Nodes (1) :=
      Self.Factory.Package_Body
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

      when 1614 =>

   Nodes (1) :=
      Self.Factory.Package_Body
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
          None,
          Nodes (9));

      when 1615 =>

   Nodes (1) :=
      Self.Factory.Package_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (4),
          Nodes (5),
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (6),
          Nodes (7),
          Nodes (8));

      when 1616 =>

   Nodes (1) :=
      Self.Factory.Package_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (4),
          Nodes (5),
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (6),
          None,
          Nodes (7));

      when 1617 =>

   Nodes (1) :=
      Self.Factory.Package_Body
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

      when 1618 =>

   Nodes (1) :=
      Self.Factory.Package_Body
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
          None,
          Nodes (10));

      when 1619 =>

   Nodes (1) :=
      Self.Factory.Package_Body
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

      when 1620 =>

   Nodes (1) :=
      Self.Factory.Package_Body
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
          None,
          Nodes (8));

      when 1621 =>

   Nodes (1) :=
      Self.Factory.Package_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (4),
          (Self.Factory.Declarative_Item_Sequence),
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (5),
          Nodes (6),
          Nodes (7));

      when 1622 =>

   Nodes (1) :=
      Self.Factory.Package_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (4),
          (Self.Factory.Declarative_Item_Sequence),
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (5),
          None,
          Nodes (6));

      when 1623 =>

   Nodes (1) :=
      Self.Factory.Package_Body_Stub
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7));

      when 1624 =>

   Nodes (1) :=
      Self.Factory.Package_Body_Stub
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (6));

      when 1625 =>

   Nodes (1) :=
      Self.Factory.Package_Declaration
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

      when 1626 =>

   Nodes (1) :=
      Self.Factory.Package_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          None,
          Nodes (9));

      when 1627 =>

   Nodes (1) :=
      Self.Factory.Package_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          (Self.Factory.Basic_Declarative_Item_Sequence),
          Nodes (7),
          Nodes (8),
          Nodes (9));

      when 1628 =>

   Nodes (1) :=
      Self.Factory.Package_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          (Self.Factory.Basic_Declarative_Item_Sequence),
          Nodes (7),
          None,
          Nodes (8));

      when 1629 =>

   Nodes (1) :=
      Self.Factory.Package_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          No_Token,
          (Self.Factory.Basic_Declarative_Item_Sequence),
          Nodes (6),
          Nodes (7),
          Nodes (8));

      when 1630 =>

   Nodes (1) :=
      Self.Factory.Package_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          No_Token,
          (Self.Factory.Basic_Declarative_Item_Sequence),
          Nodes (6),
          None,
          Nodes (7));

      when 1631 =>

   Nodes (1) :=
      Self.Factory.Package_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          (Self.Factory.Basic_Declarative_Item_Sequence),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          Nodes (9));

      when 1632 =>

   Nodes (1) :=
      Self.Factory.Package_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          (Self.Factory.Basic_Declarative_Item_Sequence),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          None,
          Nodes (8));

      when 1633 =>

   Nodes (1) :=
      Self.Factory.Package_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          (Self.Factory.Basic_Declarative_Item_Sequence),
          Nodes (5),
          (Self.Factory.Basic_Declarative_Item_Sequence),
          Nodes (6),
          Nodes (7),
          Nodes (8));

      when 1634 =>

   Nodes (1) :=
      Self.Factory.Package_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          (Self.Factory.Basic_Declarative_Item_Sequence),
          Nodes (5),
          (Self.Factory.Basic_Declarative_Item_Sequence),
          Nodes (6),
          None,
          Nodes (7));

      when 1635 =>

   Nodes (1) :=
      Self.Factory.Package_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          (Self.Factory.Basic_Declarative_Item_Sequence),
          No_Token,
          (Self.Factory.Basic_Declarative_Item_Sequence),
          Nodes (5),
          Nodes (6),
          Nodes (7));

      when 1636 =>

   Nodes (1) :=
      Self.Factory.Package_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          (Self.Factory.Basic_Declarative_Item_Sequence),
          No_Token,
          (Self.Factory.Basic_Declarative_Item_Sequence),
          Nodes (5),
          None,
          Nodes (6));

      when 1637 =>

   Nodes (1) :=
      Self.Factory.Package_Declaration
         (Nodes (1),
          Nodes (2),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          Nodes (9));

      when 1638 =>

   Nodes (1) :=
      Self.Factory.Package_Declaration
         (Nodes (1),
          Nodes (2),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          None,
          Nodes (8));

      when 1639 =>

   Nodes (1) :=
      Self.Factory.Package_Declaration
         (Nodes (1),
          Nodes (2),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          (Self.Factory.Basic_Declarative_Item_Sequence),
          Nodes (6),
          Nodes (7),
          Nodes (8));

      when 1640 =>

   Nodes (1) :=
      Self.Factory.Package_Declaration
         (Nodes (1),
          Nodes (2),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          (Self.Factory.Basic_Declarative_Item_Sequence),
          Nodes (6),
          None,
          Nodes (7));

      when 1641 =>

   Nodes (1) :=
      Self.Factory.Package_Declaration
         (Nodes (1),
          Nodes (2),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (3),
          Nodes (4),
          No_Token,
          (Self.Factory.Basic_Declarative_Item_Sequence),
          Nodes (5),
          Nodes (6),
          Nodes (7));

      when 1642 =>

   Nodes (1) :=
      Self.Factory.Package_Declaration
         (Nodes (1),
          Nodes (2),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (3),
          Nodes (4),
          No_Token,
          (Self.Factory.Basic_Declarative_Item_Sequence),
          Nodes (5),
          None,
          Nodes (6));

      when 1643 =>

   Nodes (1) :=
      Self.Factory.Package_Declaration
         (Nodes (1),
          Nodes (2),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (3),
          (Self.Factory.Basic_Declarative_Item_Sequence),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8));

      when 1644 =>

   Nodes (1) :=
      Self.Factory.Package_Declaration
         (Nodes (1),
          Nodes (2),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (3),
          (Self.Factory.Basic_Declarative_Item_Sequence),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          None,
          Nodes (7));

      when 1645 =>

   Nodes (1) :=
      Self.Factory.Package_Declaration
         (Nodes (1),
          Nodes (2),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (3),
          (Self.Factory.Basic_Declarative_Item_Sequence),
          Nodes (4),
          (Self.Factory.Basic_Declarative_Item_Sequence),
          Nodes (5),
          Nodes (6),
          Nodes (7));

      when 1646 =>

   Nodes (1) :=
      Self.Factory.Package_Declaration
         (Nodes (1),
          Nodes (2),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (3),
          (Self.Factory.Basic_Declarative_Item_Sequence),
          Nodes (4),
          (Self.Factory.Basic_Declarative_Item_Sequence),
          Nodes (5),
          None,
          Nodes (6));

      when 1647 =>

   Nodes (1) :=
      Self.Factory.Package_Declaration
         (Nodes (1),
          Nodes (2),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (3),
          (Self.Factory.Basic_Declarative_Item_Sequence),
          No_Token,
          (Self.Factory.Basic_Declarative_Item_Sequence),
          Nodes (4),
          Nodes (5),
          Nodes (6));

      when 1648 =>

   Nodes (1) :=
      Self.Factory.Package_Declaration
         (Nodes (1),
          Nodes (2),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (3),
          (Self.Factory.Basic_Declarative_Item_Sequence),
          No_Token,
          (Self.Factory.Basic_Declarative_Item_Sequence),
          Nodes (4),
          None,
          Nodes (5));

      when 1649 =>

   Nodes (1) :=
      Self.Factory.Package_Renaming_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6));

      when 1650 =>

   Nodes (1) :=
      Self.Factory.Package_Renaming_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (5));

      when 1651 =>

   Nodes (1) := Self.Factory.Parameter_Specification
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

      when 1652 =>

   Nodes (1) := Self.Factory.Parameter_Specification
       (Nodes (1),
        Nodes (2),
        Nodes (3),
        Nodes (4),
        Nodes (5),
        Nodes (6),
        Nodes (7),
        Nodes (8),
        No_Token,
        None);

      when 1653 =>

   Nodes (1) := Self.Factory.Parameter_Specification
       (Nodes (1),
        Nodes (2),
        Nodes (3),
        Nodes (4),
        Nodes (5),
        No_Token,
        No_Token,
        Nodes (6),
        Nodes (7),
        Nodes (8));

      when 1654 =>

   Nodes (1) := Self.Factory.Parameter_Specification
       (Nodes (1),
        Nodes (2),
        Nodes (3),
        Nodes (4),
        Nodes (5),
        No_Token,
        No_Token,
        Nodes (6),
        No_Token,
        None);

      when 1655 =>

   Nodes (1) := Self.Factory.Parameter_Specification
       (Nodes (1),
        Nodes (2),
        Nodes (3),
        Nodes (4),
        No_Token,
        Nodes (5),
        Nodes (6),
        Nodes (7),
        Nodes (8),
        Nodes (9));

      when 1656 =>

   Nodes (1) := Self.Factory.Parameter_Specification
       (Nodes (1),
        Nodes (2),
        Nodes (3),
        Nodes (4),
        No_Token,
        Nodes (5),
        Nodes (6),
        Nodes (7),
        No_Token,
        None);

      when 1657 =>

   Nodes (1) := Self.Factory.Parameter_Specification
       (Nodes (1),
        Nodes (2),
        Nodes (3),
        Nodes (4),
        No_Token,
        No_Token,
        No_Token,
        Nodes (5),
        Nodes (6),
        Nodes (7));

      when 1658 =>

   Nodes (1) := Self.Factory.Parameter_Specification
       (Nodes (1),
        Nodes (2),
        Nodes (3),
        Nodes (4),
        No_Token,
        No_Token,
        No_Token,
        Nodes (5),
        No_Token,
        None);

      when 1659 =>

   Nodes (1) := Self.Factory.Parameter_Specification
       (Nodes (1),
        Nodes (2),
        Nodes (3),
        No_Token,
        Nodes (4),
        Nodes (5),
        Nodes (6),
        Nodes (7),
        Nodes (8),
        Nodes (9));

      when 1660 =>

   Nodes (1) := Self.Factory.Parameter_Specification
       (Nodes (1),
        Nodes (2),
        Nodes (3),
        No_Token,
        Nodes (4),
        Nodes (5),
        Nodes (6),
        Nodes (7),
        No_Token,
        None);

      when 1661 =>

   Nodes (1) := Self.Factory.Parameter_Specification
       (Nodes (1),
        Nodes (2),
        Nodes (3),
        No_Token,
        Nodes (4),
        No_Token,
        No_Token,
        Nodes (5),
        Nodes (6),
        Nodes (7));

      when 1662 =>

   Nodes (1) := Self.Factory.Parameter_Specification
       (Nodes (1),
        Nodes (2),
        Nodes (3),
        No_Token,
        Nodes (4),
        No_Token,
        No_Token,
        Nodes (5),
        No_Token,
        None);

      when 1663 =>

   Nodes (1) := Self.Factory.Parameter_Specification
       (Nodes (1),
        Nodes (2),
        Nodes (3),
        No_Token,
        No_Token,
        Nodes (4),
        Nodes (5),
        Nodes (6),
        Nodes (7),
        Nodes (8));

      when 1664 =>

   Nodes (1) := Self.Factory.Parameter_Specification
       (Nodes (1),
        Nodes (2),
        Nodes (3),
        No_Token,
        No_Token,
        Nodes (4),
        Nodes (5),
        Nodes (6),
        No_Token,
        None);

      when 1665 =>

   Nodes (1) := Self.Factory.Parameter_Specification
       (Nodes (1),
        Nodes (2),
        Nodes (3),
        No_Token,
        No_Token,
        No_Token,
        No_Token,
        Nodes (4),
        Nodes (5),
        Nodes (6));

      when 1666 =>

   Nodes (1) := Self.Factory.Parameter_Specification
       (Nodes (1),
        Nodes (2),
        Nodes (3),
        No_Token,
        No_Token,
        No_Token,
        No_Token,
        Nodes (4),
        No_Token,
        None);

      when 1667 =>

   Nodes (1) := Self.Factory.Parameter_Specification
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

      when 1668 =>

   Nodes (1) := Self.Factory.Parameter_Specification
       (Nodes (1),
        Nodes (2),
        No_Token,
        Nodes (3),
        Nodes (4),
        Nodes (5),
        Nodes (6),
        Nodes (7),
        No_Token,
        None);

      when 1669 =>

   Nodes (1) := Self.Factory.Parameter_Specification
       (Nodes (1),
        Nodes (2),
        No_Token,
        Nodes (3),
        Nodes (4),
        No_Token,
        No_Token,
        Nodes (5),
        Nodes (6),
        Nodes (7));

      when 1670 =>

   Nodes (1) := Self.Factory.Parameter_Specification
       (Nodes (1),
        Nodes (2),
        No_Token,
        Nodes (3),
        Nodes (4),
        No_Token,
        No_Token,
        Nodes (5),
        No_Token,
        None);

      when 1671 =>

   Nodes (1) := Self.Factory.Parameter_Specification
       (Nodes (1),
        Nodes (2),
        No_Token,
        Nodes (3),
        No_Token,
        Nodes (4),
        Nodes (5),
        Nodes (6),
        Nodes (7),
        Nodes (8));

      when 1672 =>

   Nodes (1) := Self.Factory.Parameter_Specification
       (Nodes (1),
        Nodes (2),
        No_Token,
        Nodes (3),
        No_Token,
        Nodes (4),
        Nodes (5),
        Nodes (6),
        No_Token,
        None);

      when 1673 =>

   Nodes (1) := Self.Factory.Parameter_Specification
       (Nodes (1),
        Nodes (2),
        No_Token,
        Nodes (3),
        No_Token,
        No_Token,
        No_Token,
        Nodes (4),
        Nodes (5),
        Nodes (6));

      when 1674 =>

   Nodes (1) := Self.Factory.Parameter_Specification
       (Nodes (1),
        Nodes (2),
        No_Token,
        Nodes (3),
        No_Token,
        No_Token,
        No_Token,
        Nodes (4),
        No_Token,
        None);

      when 1675 =>

   Nodes (1) := Self.Factory.Parameter_Specification
       (Nodes (1),
        Nodes (2),
        No_Token,
        No_Token,
        Nodes (3),
        Nodes (4),
        Nodes (5),
        Nodes (6),
        Nodes (7),
        Nodes (8));

      when 1676 =>

   Nodes (1) := Self.Factory.Parameter_Specification
       (Nodes (1),
        Nodes (2),
        No_Token,
        No_Token,
        Nodes (3),
        Nodes (4),
        Nodes (5),
        Nodes (6),
        No_Token,
        None);

      when 1677 =>

   Nodes (1) := Self.Factory.Parameter_Specification
       (Nodes (1),
        Nodes (2),
        No_Token,
        No_Token,
        Nodes (3),
        No_Token,
        No_Token,
        Nodes (4),
        Nodes (5),
        Nodes (6));

      when 1678 =>

   Nodes (1) := Self.Factory.Parameter_Specification
       (Nodes (1),
        Nodes (2),
        No_Token,
        No_Token,
        Nodes (3),
        No_Token,
        No_Token,
        Nodes (4),
        No_Token,
        None);

      when 1679 =>

   Nodes (1) := Self.Factory.Parameter_Specification
       (Nodes (1),
        Nodes (2),
        No_Token,
        No_Token,
        No_Token,
        Nodes (3),
        Nodes (4),
        Nodes (5),
        Nodes (6),
        Nodes (7));

      when 1680 =>

   Nodes (1) := Self.Factory.Parameter_Specification
       (Nodes (1),
        Nodes (2),
        No_Token,
        No_Token,
        No_Token,
        Nodes (3),
        Nodes (4),
        Nodes (5),
        No_Token,
        None);

      when 1681 =>

   Nodes (1) := Self.Factory.Parameter_Specification
       (Nodes (1),
        Nodes (2),
        No_Token,
        No_Token,
        No_Token,
        No_Token,
        No_Token,
        Nodes (3),
        Nodes (4),
        Nodes (5));

      when 1682 =>

   Nodes (1) := Self.Factory.Parameter_Specification
       (Nodes (1),
        Nodes (2),
        No_Token,
        No_Token,
        No_Token,
        No_Token,
        No_Token,
        Nodes (3),
        No_Token,
        None);

      when 1683 =>

   Nodes (1) := Self.Factory.Parameter_Specification
       (Nodes (1),
        Nodes (2),
        No_Token,
        No_Token,
        No_Token,
        No_Token,
        No_Token,
        Nodes (3),
        Nodes (4),
        Nodes (5));

      when 1684 =>

   Nodes (1) := Self.Factory.Parameter_Specification
       (Nodes (1),
        Nodes (2),
        No_Token,
        No_Token,
        No_Token,
        No_Token,
        No_Token,
        Nodes (3),
        No_Token,
        None);

      when 1685 =>

   declare
      List : Node :=
        Nodes (1);
   begin
      Self.Factory.Append_Parameter_Specification
        (List, Nodes (3));
      Nodes (1) := List;
   end;

      when 1686 =>

   declare
      List : Node :=
        Self.Factory.Parameter_Specification_Sequence;
   begin
      Self.Factory.Append_Parameter_Specification
        (List, Nodes (2));
      Nodes (1) := List;
   end;

      when 1687 =>

   declare
      List : Node := Nodes (5);
   begin
      Self.Factory.Prepend_Pragma_Argument_Association
        (List, Nodes (4));
      Nodes (1) := Self.Factory.Pragma_Node
        (Nodes (1),
         Nodes (2),
         Nodes (3),
         List,
         Nodes (6),
         Nodes (7));
   end;

      when 1688 =>

   declare
      List : Node := Self.
        Factory.Pragma_Argument_Association_Sequence;
   begin
      Self.Factory.Prepend_Pragma_Argument_Association
        (List, Nodes (4));
      Nodes (1) := Self.Factory.Pragma_Node
        (Nodes (1),
         Nodes (2),
         Nodes (3),
         List,
         Nodes (5),
         Nodes (6));
   end;

      when 1689 =>

   Nodes (1) := Self.Factory.Pragma_Node
     (Nodes (1),
      Nodes (2),
      No_Token,
      Self.Factory.Pragma_Argument_Association_Sequence,
      No_Token,
      Nodes (3));

      when 1690 =>

   Nodes (1) := Self.Factory.
     Pragma_Argument_Association
       (Nodes (1),
        Nodes (2),
        Nodes (3));

      when 1691 =>

   Nodes (1) := Self.Factory.
     Pragma_Argument_Association
       (None,
        No_Token,
        Nodes (1));

      when 1692 =>

   declare
      List : Node := Nodes (1);
   begin
      Self.Factory.Append_Pragma_Argument_Association
        (List, Nodes (3));
      Nodes (1) := List;
   end;

      when 1693 =>

   declare
      List : Node := Self.
        Factory.Pragma_Argument_Association_Sequence;
   begin
      Self.Factory.Append_Pragma_Argument_Association
        (List, Nodes (2));
      Nodes (1) := List;
   end;

      when 1694 =>
         null;
      when 1695 =>

   declare
      Def : constant Node :=
        Self.Factory.Private_Extension_Definition
          (Nodes (5),
           Nodes (6),
           No_Token,
           Nodes (7),
           Nodes (8),
           Nodes (9),
           Nodes (10),
           Nodes (11));
   begin
      Nodes (1) :=
         Self.Factory.Private_Extension_Declaration
           (Nodes (1),
            Nodes (2),
            Nodes (3),
            Nodes (4),
            Def,
            Nodes (12),
            Nodes (13));
   end;

      when 1696 =>

   declare
      Def : constant Node :=
        Self.Factory.Private_Extension_Definition
          (Nodes (5),
           Nodes (6),
           No_Token,
           Nodes (7),
           Nodes (8),
           Nodes (9),
           Nodes (10),
           Nodes (11));
   begin
      Nodes (1) :=
         Self.Factory.Private_Extension_Declaration
           (Nodes (1),
            Nodes (2),
            Nodes (3),
            Nodes (4),
            Def,
            (Self.Factory.Aspect_Specification_Sequence),
            Nodes (12));
   end;

      when 1697 =>

   declare
      Def : constant Node :=
        Self.Factory.Private_Extension_Definition
          (Nodes (5),
           Nodes (6),
           No_Token,
           Nodes (7),
           Nodes (8),
           (Self.Factory.Subtype_Mark_Sequence),
           Nodes (9),
           Nodes (10));
   begin
      Nodes (1) :=
         Self.Factory.Private_Extension_Declaration
           (Nodes (1),
            Nodes (2),
            Nodes (3),
            Nodes (4),
            Def,
            Nodes (11),
            Nodes (12));
   end;

      when 1698 =>

   declare
      Def : constant Node :=
        Self.Factory.Private_Extension_Definition
          (Nodes (5),
           Nodes (6),
           No_Token,
           Nodes (7),
           Nodes (8),
           (Self.Factory.Subtype_Mark_Sequence),
           Nodes (9),
           Nodes (10));
   begin
      Nodes (1) :=
         Self.Factory.Private_Extension_Declaration
           (Nodes (1),
            Nodes (2),
            Nodes (3),
            Nodes (4),
            Def,
            (Self.Factory.Aspect_Specification_Sequence),
            Nodes (11));
   end;

      when 1699 =>

   declare
      Def : constant Node :=
        Self.Factory.Private_Extension_Definition
          (Nodes (5),
           No_Token,
           Nodes (6),
           Nodes (7),
           Nodes (8),
           Nodes (9),
           Nodes (10),
           Nodes (11));
   begin
      Nodes (1) :=
         Self.Factory.Private_Extension_Declaration
           (Nodes (1),
            Nodes (2),
            Nodes (3),
            Nodes (4),
            Def,
            Nodes (12),
            Nodes (13));
   end;

      when 1700 =>

   declare
      Def : constant Node :=
        Self.Factory.Private_Extension_Definition
          (Nodes (5),
           No_Token,
           Nodes (6),
           Nodes (7),
           Nodes (8),
           Nodes (9),
           Nodes (10),
           Nodes (11));
   begin
      Nodes (1) :=
         Self.Factory.Private_Extension_Declaration
           (Nodes (1),
            Nodes (2),
            Nodes (3),
            Nodes (4),
            Def,
            (Self.Factory.Aspect_Specification_Sequence),
            Nodes (12));
   end;

      when 1701 =>

   declare
      Def : constant Node :=
        Self.Factory.Private_Extension_Definition
          (Nodes (5),
           No_Token,
           Nodes (6),
           Nodes (7),
           Nodes (8),
           (Self.Factory.Subtype_Mark_Sequence),
           Nodes (9),
           Nodes (10));
   begin
      Nodes (1) :=
         Self.Factory.Private_Extension_Declaration
           (Nodes (1),
            Nodes (2),
            Nodes (3),
            Nodes (4),
            Def,
            Nodes (11),
            Nodes (12));
   end;

      when 1702 =>

   declare
      Def : constant Node :=
        Self.Factory.Private_Extension_Definition
          (Nodes (5),
           No_Token,
           Nodes (6),
           Nodes (7),
           Nodes (8),
           (Self.Factory.Subtype_Mark_Sequence),
           Nodes (9),
           Nodes (10));
   begin
      Nodes (1) :=
         Self.Factory.Private_Extension_Declaration
           (Nodes (1),
            Nodes (2),
            Nodes (3),
            Nodes (4),
            Def,
            (Self.Factory.Aspect_Specification_Sequence),
            Nodes (11));
   end;

      when 1703 =>

   declare
      Def : constant Node :=
        Self.Factory.Private_Extension_Definition
          (Nodes (5),
           No_Token,
           No_Token,
           Nodes (6),
           Nodes (7),
           Nodes (8),
           Nodes (9),
           Nodes (10));
   begin
      Nodes (1) :=
         Self.Factory.Private_Extension_Declaration
           (Nodes (1),
            Nodes (2),
            Nodes (3),
            Nodes (4),
            Def,
            Nodes (11),
            Nodes (12));
   end;

      when 1704 =>

   declare
      Def : constant Node :=
        Self.Factory.Private_Extension_Definition
          (Nodes (5),
           No_Token,
           No_Token,
           Nodes (6),
           Nodes (7),
           Nodes (8),
           Nodes (9),
           Nodes (10));
   begin
      Nodes (1) :=
         Self.Factory.Private_Extension_Declaration
           (Nodes (1),
            Nodes (2),
            Nodes (3),
            Nodes (4),
            Def,
            (Self.Factory.Aspect_Specification_Sequence),
            Nodes (11));
   end;

      when 1705 =>

   declare
      Def : constant Node :=
        Self.Factory.Private_Extension_Definition
          (Nodes (5),
           No_Token,
           No_Token,
           Nodes (6),
           Nodes (7),
           (Self.Factory.Subtype_Mark_Sequence),
           Nodes (8),
           Nodes (9));
   begin
      Nodes (1) :=
         Self.Factory.Private_Extension_Declaration
           (Nodes (1),
            Nodes (2),
            Nodes (3),
            Nodes (4),
            Def,
            Nodes (10),
            Nodes (11));
   end;

      when 1706 =>

   declare
      Def : constant Node :=
        Self.Factory.Private_Extension_Definition
          (Nodes (5),
           No_Token,
           No_Token,
           Nodes (6),
           Nodes (7),
           (Self.Factory.Subtype_Mark_Sequence),
           Nodes (8),
           Nodes (9));
   begin
      Nodes (1) :=
         Self.Factory.Private_Extension_Declaration
           (Nodes (1),
            Nodes (2),
            Nodes (3),
            Nodes (4),
            Def,
            (Self.Factory.Aspect_Specification_Sequence),
            Nodes (10));
   end;

      when 1707 =>

   declare
      Def : constant Node :=
        Self.Factory.Private_Extension_Definition
          (No_Token,
           Nodes (5),
           No_Token,
           Nodes (6),
           Nodes (7),
           Nodes (8),
           Nodes (9),
           Nodes (10));
   begin
      Nodes (1) :=
         Self.Factory.Private_Extension_Declaration
           (Nodes (1),
            Nodes (2),
            Nodes (3),
            Nodes (4),
            Def,
            Nodes (11),
            Nodes (12));
   end;

      when 1708 =>

   declare
      Def : constant Node :=
        Self.Factory.Private_Extension_Definition
          (No_Token,
           Nodes (5),
           No_Token,
           Nodes (6),
           Nodes (7),
           Nodes (8),
           Nodes (9),
           Nodes (10));
   begin
      Nodes (1) :=
         Self.Factory.Private_Extension_Declaration
           (Nodes (1),
            Nodes (2),
            Nodes (3),
            Nodes (4),
            Def,
            (Self.Factory.Aspect_Specification_Sequence),
            Nodes (11));
   end;

      when 1709 =>

   declare
      Def : constant Node :=
        Self.Factory.Private_Extension_Definition
          (No_Token,
           Nodes (5),
           No_Token,
           Nodes (6),
           Nodes (7),
           (Self.Factory.Subtype_Mark_Sequence),
           Nodes (8),
           Nodes (9));
   begin
      Nodes (1) :=
         Self.Factory.Private_Extension_Declaration
           (Nodes (1),
            Nodes (2),
            Nodes (3),
            Nodes (4),
            Def,
            Nodes (10),
            Nodes (11));
   end;

      when 1710 =>

   declare
      Def : constant Node :=
        Self.Factory.Private_Extension_Definition
          (No_Token,
           Nodes (5),
           No_Token,
           Nodes (6),
           Nodes (7),
           (Self.Factory.Subtype_Mark_Sequence),
           Nodes (8),
           Nodes (9));
   begin
      Nodes (1) :=
         Self.Factory.Private_Extension_Declaration
           (Nodes (1),
            Nodes (2),
            Nodes (3),
            Nodes (4),
            Def,
            (Self.Factory.Aspect_Specification_Sequence),
            Nodes (10));
   end;

      when 1711 =>

   declare
      Def : constant Node :=
        Self.Factory.Private_Extension_Definition
          (No_Token,
           No_Token,
           Nodes (5),
           Nodes (6),
           Nodes (7),
           Nodes (8),
           Nodes (9),
           Nodes (10));
   begin
      Nodes (1) :=
         Self.Factory.Private_Extension_Declaration
           (Nodes (1),
            Nodes (2),
            Nodes (3),
            Nodes (4),
            Def,
            Nodes (11),
            Nodes (12));
   end;

      when 1712 =>

   declare
      Def : constant Node :=
        Self.Factory.Private_Extension_Definition
          (No_Token,
           No_Token,
           Nodes (5),
           Nodes (6),
           Nodes (7),
           Nodes (8),
           Nodes (9),
           Nodes (10));
   begin
      Nodes (1) :=
         Self.Factory.Private_Extension_Declaration
           (Nodes (1),
            Nodes (2),
            Nodes (3),
            Nodes (4),
            Def,
            (Self.Factory.Aspect_Specification_Sequence),
            Nodes (11));
   end;

      when 1713 =>

   declare
      Def : constant Node :=
        Self.Factory.Private_Extension_Definition
          (No_Token,
           No_Token,
           Nodes (5),
           Nodes (6),
           Nodes (7),
           (Self.Factory.Subtype_Mark_Sequence),
           Nodes (8),
           Nodes (9));
   begin
      Nodes (1) :=
         Self.Factory.Private_Extension_Declaration
           (Nodes (1),
            Nodes (2),
            Nodes (3),
            Nodes (4),
            Def,
            Nodes (10),
            Nodes (11));
   end;

      when 1714 =>

   declare
      Def : constant Node :=
        Self.Factory.Private_Extension_Definition
          (No_Token,
           No_Token,
           Nodes (5),
           Nodes (6),
           Nodes (7),
           (Self.Factory.Subtype_Mark_Sequence),
           Nodes (8),
           Nodes (9));
   begin
      Nodes (1) :=
         Self.Factory.Private_Extension_Declaration
           (Nodes (1),
            Nodes (2),
            Nodes (3),
            Nodes (4),
            Def,
            (Self.Factory.Aspect_Specification_Sequence),
            Nodes (10));
   end;

      when 1715 =>

   declare
      Def : constant Node :=
        Self.Factory.Private_Extension_Definition
          (No_Token,
           No_Token,
           No_Token,
           Nodes (5),
           Nodes (6),
           Nodes (7),
           Nodes (8),
           Nodes (9));
   begin
      Nodes (1) :=
         Self.Factory.Private_Extension_Declaration
           (Nodes (1),
            Nodes (2),
            Nodes (3),
            Nodes (4),
            Def,
            Nodes (10),
            Nodes (11));
   end;

      when 1716 =>

   declare
      Def : constant Node :=
        Self.Factory.Private_Extension_Definition
          (No_Token,
           No_Token,
           No_Token,
           Nodes (5),
           Nodes (6),
           Nodes (7),
           Nodes (8),
           Nodes (9));
   begin
      Nodes (1) :=
         Self.Factory.Private_Extension_Declaration
           (Nodes (1),
            Nodes (2),
            Nodes (3),
            Nodes (4),
            Def,
            (Self.Factory.Aspect_Specification_Sequence),
            Nodes (10));
   end;

      when 1717 =>

   declare
      Def : constant Node :=
        Self.Factory.Private_Extension_Definition
          (No_Token,
           No_Token,
           No_Token,
           Nodes (5),
           Nodes (6),
           (Self.Factory.Subtype_Mark_Sequence),
           Nodes (7),
           Nodes (8));
   begin
      Nodes (1) :=
         Self.Factory.Private_Extension_Declaration
           (Nodes (1),
            Nodes (2),
            Nodes (3),
            Nodes (4),
            Def,
            Nodes (9),
            Nodes (10));
   end;

      when 1718 =>

   declare
      Def : constant Node :=
        Self.Factory.Private_Extension_Definition
          (No_Token,
           No_Token,
           No_Token,
           Nodes (5),
           Nodes (6),
           (Self.Factory.Subtype_Mark_Sequence),
           Nodes (7),
           Nodes (8));
   begin
      Nodes (1) :=
         Self.Factory.Private_Extension_Declaration
           (Nodes (1),
            Nodes (2),
            Nodes (3),
            Nodes (4),
            Def,
            (Self.Factory.Aspect_Specification_Sequence),
            Nodes (9));
   end;

      when 1719 =>

   declare
      Def : constant Node :=
        Self.Factory.Private_Extension_Definition
          (Nodes (4),
           Nodes (5),
           No_Token,
           Nodes (6),
           Nodes (7),
           Nodes (8),
           Nodes (9),
           Nodes (10));
   begin
      Nodes (1) :=
         Self.Factory.Private_Extension_Declaration
           (Nodes (1),
            Nodes (2),
            None,
            Nodes (3),
            Def,
            Nodes (11),
            Nodes (12));
   end;

      when 1720 =>

   declare
      Def : constant Node :=
        Self.Factory.Private_Extension_Definition
          (Nodes (4),
           Nodes (5),
           No_Token,
           Nodes (6),
           Nodes (7),
           Nodes (8),
           Nodes (9),
           Nodes (10));
   begin
      Nodes (1) :=
         Self.Factory.Private_Extension_Declaration
           (Nodes (1),
            Nodes (2),
            None,
            Nodes (3),
            Def,
            (Self.Factory.Aspect_Specification_Sequence),
            Nodes (11));
   end;

      when 1721 =>

   declare
      Def : constant Node :=
        Self.Factory.Private_Extension_Definition
          (Nodes (4),
           Nodes (5),
           No_Token,
           Nodes (6),
           Nodes (7),
           (Self.Factory.Subtype_Mark_Sequence),
           Nodes (8),
           Nodes (9));
   begin
      Nodes (1) :=
         Self.Factory.Private_Extension_Declaration
           (Nodes (1),
            Nodes (2),
            None,
            Nodes (3),
            Def,
            Nodes (10),
            Nodes (11));
   end;

      when 1722 =>

   declare
      Def : constant Node :=
        Self.Factory.Private_Extension_Definition
          (Nodes (4),
           Nodes (5),
           No_Token,
           Nodes (6),
           Nodes (7),
           (Self.Factory.Subtype_Mark_Sequence),
           Nodes (8),
           Nodes (9));
   begin
      Nodes (1) :=
         Self.Factory.Private_Extension_Declaration
           (Nodes (1),
            Nodes (2),
            None,
            Nodes (3),
            Def,
            (Self.Factory.Aspect_Specification_Sequence),
            Nodes (10));
   end;

      when 1723 =>

   declare
      Def : constant Node :=
        Self.Factory.Private_Extension_Definition
          (Nodes (4),
           No_Token,
           Nodes (5),
           Nodes (6),
           Nodes (7),
           Nodes (8),
           Nodes (9),
           Nodes (10));
   begin
      Nodes (1) :=
         Self.Factory.Private_Extension_Declaration
           (Nodes (1),
            Nodes (2),
            None,
            Nodes (3),
            Def,
            Nodes (11),
            Nodes (12));
   end;

      when 1724 =>

   declare
      Def : constant Node :=
        Self.Factory.Private_Extension_Definition
          (Nodes (4),
           No_Token,
           Nodes (5),
           Nodes (6),
           Nodes (7),
           Nodes (8),
           Nodes (9),
           Nodes (10));
   begin
      Nodes (1) :=
         Self.Factory.Private_Extension_Declaration
           (Nodes (1),
            Nodes (2),
            None,
            Nodes (3),
            Def,
            (Self.Factory.Aspect_Specification_Sequence),
            Nodes (11));
   end;

      when 1725 =>

   declare
      Def : constant Node :=
        Self.Factory.Private_Extension_Definition
          (Nodes (4),
           No_Token,
           Nodes (5),
           Nodes (6),
           Nodes (7),
           (Self.Factory.Subtype_Mark_Sequence),
           Nodes (8),
           Nodes (9));
   begin
      Nodes (1) :=
         Self.Factory.Private_Extension_Declaration
           (Nodes (1),
            Nodes (2),
            None,
            Nodes (3),
            Def,
            Nodes (10),
            Nodes (11));
   end;

      when 1726 =>

   declare
      Def : constant Node :=
        Self.Factory.Private_Extension_Definition
          (Nodes (4),
           No_Token,
           Nodes (5),
           Nodes (6),
           Nodes (7),
           (Self.Factory.Subtype_Mark_Sequence),
           Nodes (8),
           Nodes (9));
   begin
      Nodes (1) :=
         Self.Factory.Private_Extension_Declaration
           (Nodes (1),
            Nodes (2),
            None,
            Nodes (3),
            Def,
            (Self.Factory.Aspect_Specification_Sequence),
            Nodes (10));
   end;

      when 1727 =>

   declare
      Def : constant Node :=
        Self.Factory.Private_Extension_Definition
          (Nodes (4),
           No_Token,
           No_Token,
           Nodes (5),
           Nodes (6),
           Nodes (7),
           Nodes (8),
           Nodes (9));
   begin
      Nodes (1) :=
         Self.Factory.Private_Extension_Declaration
           (Nodes (1),
            Nodes (2),
            None,
            Nodes (3),
            Def,
            Nodes (10),
            Nodes (11));
   end;

      when 1728 =>

   declare
      Def : constant Node :=
        Self.Factory.Private_Extension_Definition
          (Nodes (4),
           No_Token,
           No_Token,
           Nodes (5),
           Nodes (6),
           Nodes (7),
           Nodes (8),
           Nodes (9));
   begin
      Nodes (1) :=
         Self.Factory.Private_Extension_Declaration
           (Nodes (1),
            Nodes (2),
            None,
            Nodes (3),
            Def,
            (Self.Factory.Aspect_Specification_Sequence),
            Nodes (10));
   end;

      when 1729 =>

   declare
      Def : constant Node :=
        Self.Factory.Private_Extension_Definition
          (Nodes (4),
           No_Token,
           No_Token,
           Nodes (5),
           Nodes (6),
           (Self.Factory.Subtype_Mark_Sequence),
           Nodes (7),
           Nodes (8));
   begin
      Nodes (1) :=
         Self.Factory.Private_Extension_Declaration
           (Nodes (1),
            Nodes (2),
            None,
            Nodes (3),
            Def,
            Nodes (9),
            Nodes (10));
   end;

      when 1730 =>

   declare
      Def : constant Node :=
        Self.Factory.Private_Extension_Definition
          (Nodes (4),
           No_Token,
           No_Token,
           Nodes (5),
           Nodes (6),
           (Self.Factory.Subtype_Mark_Sequence),
           Nodes (7),
           Nodes (8));
   begin
      Nodes (1) :=
         Self.Factory.Private_Extension_Declaration
           (Nodes (1),
            Nodes (2),
            None,
            Nodes (3),
            Def,
            (Self.Factory.Aspect_Specification_Sequence),
            Nodes (9));
   end;

      when 1731 =>

   declare
      Def : constant Node :=
        Self.Factory.Private_Extension_Definition
          (No_Token,
           Nodes (4),
           No_Token,
           Nodes (5),
           Nodes (6),
           Nodes (7),
           Nodes (8),
           Nodes (9));
   begin
      Nodes (1) :=
         Self.Factory.Private_Extension_Declaration
           (Nodes (1),
            Nodes (2),
            None,
            Nodes (3),
            Def,
            Nodes (10),
            Nodes (11));
   end;

      when 1732 =>

   declare
      Def : constant Node :=
        Self.Factory.Private_Extension_Definition
          (No_Token,
           Nodes (4),
           No_Token,
           Nodes (5),
           Nodes (6),
           Nodes (7),
           Nodes (8),
           Nodes (9));
   begin
      Nodes (1) :=
         Self.Factory.Private_Extension_Declaration
           (Nodes (1),
            Nodes (2),
            None,
            Nodes (3),
            Def,
            (Self.Factory.Aspect_Specification_Sequence),
            Nodes (10));
   end;

      when 1733 =>

   declare
      Def : constant Node :=
        Self.Factory.Private_Extension_Definition
          (No_Token,
           Nodes (4),
           No_Token,
           Nodes (5),
           Nodes (6),
           (Self.Factory.Subtype_Mark_Sequence),
           Nodes (7),
           Nodes (8));
   begin
      Nodes (1) :=
         Self.Factory.Private_Extension_Declaration
           (Nodes (1),
            Nodes (2),
            None,
            Nodes (3),
            Def,
            Nodes (9),
            Nodes (10));
   end;

      when 1734 =>

   declare
      Def : constant Node :=
        Self.Factory.Private_Extension_Definition
          (No_Token,
           Nodes (4),
           No_Token,
           Nodes (5),
           Nodes (6),
           (Self.Factory.Subtype_Mark_Sequence),
           Nodes (7),
           Nodes (8));
   begin
      Nodes (1) :=
         Self.Factory.Private_Extension_Declaration
           (Nodes (1),
            Nodes (2),
            None,
            Nodes (3),
            Def,
            (Self.Factory.Aspect_Specification_Sequence),
            Nodes (9));
   end;

      when 1735 =>

   declare
      Def : constant Node :=
        Self.Factory.Private_Extension_Definition
          (No_Token,
           No_Token,
           Nodes (4),
           Nodes (5),
           Nodes (6),
           Nodes (7),
           Nodes (8),
           Nodes (9));
   begin
      Nodes (1) :=
         Self.Factory.Private_Extension_Declaration
           (Nodes (1),
            Nodes (2),
            None,
            Nodes (3),
            Def,
            Nodes (10),
            Nodes (11));
   end;

      when 1736 =>

   declare
      Def : constant Node :=
        Self.Factory.Private_Extension_Definition
          (No_Token,
           No_Token,
           Nodes (4),
           Nodes (5),
           Nodes (6),
           Nodes (7),
           Nodes (8),
           Nodes (9));
   begin
      Nodes (1) :=
         Self.Factory.Private_Extension_Declaration
           (Nodes (1),
            Nodes (2),
            None,
            Nodes (3),
            Def,
            (Self.Factory.Aspect_Specification_Sequence),
            Nodes (10));
   end;

      when 1737 =>

   declare
      Def : constant Node :=
        Self.Factory.Private_Extension_Definition
          (No_Token,
           No_Token,
           Nodes (4),
           Nodes (5),
           Nodes (6),
           (Self.Factory.Subtype_Mark_Sequence),
           Nodes (7),
           Nodes (8));
   begin
      Nodes (1) :=
         Self.Factory.Private_Extension_Declaration
           (Nodes (1),
            Nodes (2),
            None,
            Nodes (3),
            Def,
            Nodes (9),
            Nodes (10));
   end;

      when 1738 =>

   declare
      Def : constant Node :=
        Self.Factory.Private_Extension_Definition
          (No_Token,
           No_Token,
           Nodes (4),
           Nodes (5),
           Nodes (6),
           (Self.Factory.Subtype_Mark_Sequence),
           Nodes (7),
           Nodes (8));
   begin
      Nodes (1) :=
         Self.Factory.Private_Extension_Declaration
           (Nodes (1),
            Nodes (2),
            None,
            Nodes (3),
            Def,
            (Self.Factory.Aspect_Specification_Sequence),
            Nodes (9));
   end;

      when 1739 =>

   declare
      Def : constant Node :=
        Self.Factory.Private_Extension_Definition
          (No_Token,
           No_Token,
           No_Token,
           Nodes (4),
           Nodes (5),
           Nodes (6),
           Nodes (7),
           Nodes (8));
   begin
      Nodes (1) :=
         Self.Factory.Private_Extension_Declaration
           (Nodes (1),
            Nodes (2),
            None,
            Nodes (3),
            Def,
            Nodes (9),
            Nodes (10));
   end;

      when 1740 =>

   declare
      Def : constant Node :=
        Self.Factory.Private_Extension_Definition
          (No_Token,
           No_Token,
           No_Token,
           Nodes (4),
           Nodes (5),
           Nodes (6),
           Nodes (7),
           Nodes (8));
   begin
      Nodes (1) :=
         Self.Factory.Private_Extension_Declaration
           (Nodes (1),
            Nodes (2),
            None,
            Nodes (3),
            Def,
            (Self.Factory.Aspect_Specification_Sequence),
            Nodes (9));
   end;

      when 1741 =>

   declare
      Def : constant Node :=
        Self.Factory.Private_Extension_Definition
          (No_Token,
           No_Token,
           No_Token,
           Nodes (4),
           Nodes (5),
           (Self.Factory.Subtype_Mark_Sequence),
           Nodes (6),
           Nodes (7));
   begin
      Nodes (1) :=
         Self.Factory.Private_Extension_Declaration
           (Nodes (1),
            Nodes (2),
            None,
            Nodes (3),
            Def,
            Nodes (8),
            Nodes (9));
   end;

      when 1742 =>

   declare
      Def : constant Node :=
        Self.Factory.Private_Extension_Definition
          (No_Token,
           No_Token,
           No_Token,
           Nodes (4),
           Nodes (5),
           (Self.Factory.Subtype_Mark_Sequence),
           Nodes (6),
           Nodes (7));
   begin
      Nodes (1) :=
         Self.Factory.Private_Extension_Declaration
           (Nodes (1),
            Nodes (2),
            None,
            Nodes (3),
            Def,
            (Self.Factory.Aspect_Specification_Sequence),
            Nodes (8));
   end;

      when 1743 =>

   declare
      Def : constant Node:=
        Self.Factory.Private_Type_Definition
          (Nodes (5),
           Nodes (6),
           Nodes (7),
           Nodes (8));
   begin
      Nodes (1) :=
         Self.Factory.Private_Type_Declaration
           (Nodes (1),
            Nodes (2),
            Nodes (3),
            Nodes (4),
            Def,
            Nodes (9),
            Nodes (10));
   end;

      when 1744 =>

   declare
      Def : constant Node:=
        Self.Factory.Private_Type_Definition
          (Nodes (5),
           Nodes (6),
           Nodes (7),
           Nodes (8));
   begin
      Nodes (1) :=
         Self.Factory.Private_Type_Declaration
           (Nodes (1),
            Nodes (2),
            Nodes (3),
            Nodes (4),
            Def,
            (Self.Factory.Aspect_Specification_Sequence),
            Nodes (9));
   end;

      when 1745 =>

   declare
      Def : constant Node:=
        Self.Factory.Private_Type_Definition
          (Nodes (5),
           Nodes (6),
           No_Token,
           Nodes (7));
   begin
      Nodes (1) :=
         Self.Factory.Private_Type_Declaration
           (Nodes (1),
            Nodes (2),
            Nodes (3),
            Nodes (4),
            Def,
            Nodes (8),
            Nodes (9));
   end;

      when 1746 =>

   declare
      Def : constant Node:=
        Self.Factory.Private_Type_Definition
          (Nodes (5),
           Nodes (6),
           No_Token,
           Nodes (7));
   begin
      Nodes (1) :=
         Self.Factory.Private_Type_Declaration
           (Nodes (1),
            Nodes (2),
            Nodes (3),
            Nodes (4),
            Def,
            (Self.Factory.Aspect_Specification_Sequence),
            Nodes (8));
   end;

      when 1747 =>

   declare
      Def : constant Node:=
        Self.Factory.Private_Type_Definition
          (No_Token,
           Nodes (5),
           Nodes (6),
           Nodes (7));
   begin
      Nodes (1) :=
         Self.Factory.Private_Type_Declaration
           (Nodes (1),
            Nodes (2),
            Nodes (3),
            Nodes (4),
            Def,
            Nodes (8),
            Nodes (9));
   end;

      when 1748 =>

   declare
      Def : constant Node:=
        Self.Factory.Private_Type_Definition
          (No_Token,
           Nodes (5),
           Nodes (6),
           Nodes (7));
   begin
      Nodes (1) :=
         Self.Factory.Private_Type_Declaration
           (Nodes (1),
            Nodes (2),
            Nodes (3),
            Nodes (4),
            Def,
            (Self.Factory.Aspect_Specification_Sequence),
            Nodes (8));
   end;

      when 1749 =>

   declare
      Def : constant Node:=
        Self.Factory.Private_Type_Definition
          (No_Token,
           Nodes (5),
           No_Token,
           Nodes (6));
   begin
      Nodes (1) :=
         Self.Factory.Private_Type_Declaration
           (Nodes (1),
            Nodes (2),
            Nodes (3),
            Nodes (4),
            Def,
            Nodes (7),
            Nodes (8));
   end;

      when 1750 =>

   declare
      Def : constant Node:=
        Self.Factory.Private_Type_Definition
          (No_Token,
           Nodes (5),
           No_Token,
           Nodes (6));
   begin
      Nodes (1) :=
         Self.Factory.Private_Type_Declaration
           (Nodes (1),
            Nodes (2),
            Nodes (3),
            Nodes (4),
            Def,
            (Self.Factory.Aspect_Specification_Sequence),
            Nodes (7));
   end;

      when 1751 =>

   declare
      Def : constant Node:=
        Self.Factory.Private_Type_Definition
          (No_Token,
           No_Token,
           Nodes (5),
           Nodes (6));
   begin
      Nodes (1) :=
         Self.Factory.Private_Type_Declaration
           (Nodes (1),
            Nodes (2),
            Nodes (3),
            Nodes (4),
            Def,
            Nodes (7),
            Nodes (8));
   end;

      when 1752 =>

   declare
      Def : constant Node:=
        Self.Factory.Private_Type_Definition
          (No_Token,
           No_Token,
           Nodes (5),
           Nodes (6));
   begin
      Nodes (1) :=
         Self.Factory.Private_Type_Declaration
           (Nodes (1),
            Nodes (2),
            Nodes (3),
            Nodes (4),
            Def,
            (Self.Factory.Aspect_Specification_Sequence),
            Nodes (7));
   end;

      when 1753 =>

   declare
      Def : constant Node:=
        Self.Factory.Private_Type_Definition
          (No_Token,
           No_Token,
           No_Token,
           Nodes (5));
   begin
      Nodes (1) :=
         Self.Factory.Private_Type_Declaration
           (Nodes (1),
            Nodes (2),
            Nodes (3),
            Nodes (4),
            Def,
            Nodes (6),
            Nodes (7));
   end;

      when 1754 =>

   declare
      Def : constant Node:=
        Self.Factory.Private_Type_Definition
          (No_Token,
           No_Token,
           No_Token,
           Nodes (5));
   begin
      Nodes (1) :=
         Self.Factory.Private_Type_Declaration
           (Nodes (1),
            Nodes (2),
            Nodes (3),
            Nodes (4),
            Def,
            (Self.Factory.Aspect_Specification_Sequence),
            Nodes (6));
   end;

      when 1755 =>

   declare
      Def : constant Node:=
        Self.Factory.Private_Type_Definition
          (Nodes (4),
           Nodes (5),
           Nodes (6),
           Nodes (7));
   begin
      Nodes (1) :=
         Self.Factory.Private_Type_Declaration
           (Nodes (1),
            Nodes (2),
            None,
            Nodes (3),
            Def,
            Nodes (8),
            Nodes (9));
   end;

      when 1756 =>

   declare
      Def : constant Node:=
        Self.Factory.Private_Type_Definition
          (Nodes (4),
           Nodes (5),
           Nodes (6),
           Nodes (7));
   begin
      Nodes (1) :=
         Self.Factory.Private_Type_Declaration
           (Nodes (1),
            Nodes (2),
            None,
            Nodes (3),
            Def,
            (Self.Factory.Aspect_Specification_Sequence),
            Nodes (8));
   end;

      when 1757 =>

   declare
      Def : constant Node:=
        Self.Factory.Private_Type_Definition
          (Nodes (4),
           Nodes (5),
           No_Token,
           Nodes (6));
   begin
      Nodes (1) :=
         Self.Factory.Private_Type_Declaration
           (Nodes (1),
            Nodes (2),
            None,
            Nodes (3),
            Def,
            Nodes (7),
            Nodes (8));
   end;

      when 1758 =>

   declare
      Def : constant Node:=
        Self.Factory.Private_Type_Definition
          (Nodes (4),
           Nodes (5),
           No_Token,
           Nodes (6));
   begin
      Nodes (1) :=
         Self.Factory.Private_Type_Declaration
           (Nodes (1),
            Nodes (2),
            None,
            Nodes (3),
            Def,
            (Self.Factory.Aspect_Specification_Sequence),
            Nodes (7));
   end;

      when 1759 =>

   declare
      Def : constant Node:=
        Self.Factory.Private_Type_Definition
          (No_Token,
           Nodes (4),
           Nodes (5),
           Nodes (6));
   begin
      Nodes (1) :=
         Self.Factory.Private_Type_Declaration
           (Nodes (1),
            Nodes (2),
            None,
            Nodes (3),
            Def,
            Nodes (7),
            Nodes (8));
   end;

      when 1760 =>

   declare
      Def : constant Node:=
        Self.Factory.Private_Type_Definition
          (No_Token,
           Nodes (4),
           Nodes (5),
           Nodes (6));
   begin
      Nodes (1) :=
         Self.Factory.Private_Type_Declaration
           (Nodes (1),
            Nodes (2),
            None,
            Nodes (3),
            Def,
            (Self.Factory.Aspect_Specification_Sequence),
            Nodes (7));
   end;

      when 1761 =>

   declare
      Def : constant Node:=
        Self.Factory.Private_Type_Definition
          (No_Token,
           Nodes (4),
           No_Token,
           Nodes (5));
   begin
      Nodes (1) :=
         Self.Factory.Private_Type_Declaration
           (Nodes (1),
            Nodes (2),
            None,
            Nodes (3),
            Def,
            Nodes (6),
            Nodes (7));
   end;

      when 1762 =>

   declare
      Def : constant Node:=
        Self.Factory.Private_Type_Definition
          (No_Token,
           Nodes (4),
           No_Token,
           Nodes (5));
   begin
      Nodes (1) :=
         Self.Factory.Private_Type_Declaration
           (Nodes (1),
            Nodes (2),
            None,
            Nodes (3),
            Def,
            (Self.Factory.Aspect_Specification_Sequence),
            Nodes (6));
   end;

      when 1763 =>

   declare
      Def : constant Node:=
        Self.Factory.Private_Type_Definition
          (No_Token,
           No_Token,
           Nodes (4),
           Nodes (5));
   begin
      Nodes (1) :=
         Self.Factory.Private_Type_Declaration
           (Nodes (1),
            Nodes (2),
            None,
            Nodes (3),
            Def,
            Nodes (6),
            Nodes (7));
   end;

      when 1764 =>

   declare
      Def : constant Node:=
        Self.Factory.Private_Type_Definition
          (No_Token,
           No_Token,
           Nodes (4),
           Nodes (5));
   begin
      Nodes (1) :=
         Self.Factory.Private_Type_Declaration
           (Nodes (1),
            Nodes (2),
            None,
            Nodes (3),
            Def,
            (Self.Factory.Aspect_Specification_Sequence),
            Nodes (6));
   end;

      when 1765 =>

   declare
      Def : constant Node:=
        Self.Factory.Private_Type_Definition
          (No_Token,
           No_Token,
           No_Token,
           Nodes (4));
   begin
      Nodes (1) :=
         Self.Factory.Private_Type_Declaration
           (Nodes (1),
            Nodes (2),
            None,
            Nodes (3),
            Def,
            Nodes (5),
            Nodes (6));
   end;

      when 1766 =>

   declare
      Def : constant Node:=
        Self.Factory.Private_Type_Definition
          (No_Token,
           No_Token,
           No_Token,
           Nodes (4));
   begin
      Nodes (1) :=
         Self.Factory.Private_Type_Declaration
           (Nodes (1),
            Nodes (2),
            None,
            Nodes (3),
            Def,
            (Self.Factory.Aspect_Specification_Sequence),
            Nodes (5));
   end;

      when 1767 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
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
          Nodes (17));

      when 1768 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
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
          None,
          Nodes (16));

      when 1769 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
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
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (13),
          Nodes (14),
          Nodes (15));

      when 1770 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
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
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (13),
          None,
          Nodes (14));

      when 1771 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
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
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (11),
          Nodes (12),
          Nodes (13));

      when 1772 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
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
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (11),
          None,
          Nodes (12));

      when 1773 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
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

      when 1774 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
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

      when 1775 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
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

      when 1776 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
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

      when 1777 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
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

      when 1778 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
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

      when 1779 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
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

      when 1780 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
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

      when 1781 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
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

      when 1782 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
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

      when 1783 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
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

      when 1784 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
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

      when 1785 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
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

      when 1786 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
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

      when 1787 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
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

      when 1788 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
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

      when 1789 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
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

      when 1790 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
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

      when 1791 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
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
          Nodes (11),
          Nodes (12),
          Nodes (13),
          Nodes (14));

      when 1792 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
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
          Nodes (11),
          Nodes (12),
          None,
          Nodes (13));

      when 1793 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
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
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (10),
          Nodes (11),
          Nodes (12));

      when 1794 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
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
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (10),
          None,
          Nodes (11));

      when 1795 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
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
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (8),
          Nodes (9),
          Nodes (10));

      when 1796 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
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
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (8),
          None,
          Nodes (9));

      when 1797 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
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

      when 1798 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
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

      when 1799 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
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

      when 1800 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
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

      when 1801 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
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

      when 1802 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
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

      when 1803 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
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

      when 1804 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
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

      when 1805 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
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

      when 1806 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
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

      when 1807 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
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

      when 1808 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
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

      when 1809 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
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

      when 1810 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
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

      when 1811 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
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

      when 1812 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
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

      when 1813 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
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

      when 1814 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
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

      when 1815 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
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
          Nodes (13),
          Nodes (14),
          Nodes (15),
          Nodes (16));

      when 1816 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
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
          Nodes (13),
          Nodes (14),
          None,
          Nodes (15));

      when 1817 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
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
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (12),
          Nodes (13),
          Nodes (14));

      when 1818 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
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
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (12),
          None,
          Nodes (13));

      when 1819 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
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
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (10),
          Nodes (11),
          Nodes (12));

      when 1820 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
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
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (10),
          None,
          Nodes (11));

      when 1821 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
         (No_Token,
          Nodes (1),
          Nodes (2),
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

      when 1822 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
         (No_Token,
          Nodes (1),
          Nodes (2),
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
          None,
          Nodes (14));

      when 1823 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
         (No_Token,
          Nodes (1),
          Nodes (2),
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

      when 1824 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
         (No_Token,
          Nodes (1),
          Nodes (2),
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
          None,
          Nodes (12));

      when 1825 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
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

      when 1826 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
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

      when 1827 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
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

      when 1828 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
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

      when 1829 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
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

      when 1830 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
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

      when 1831 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
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

      when 1832 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
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

      when 1833 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
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

      when 1834 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
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

      when 1835 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
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

      when 1836 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
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

      when 1837 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
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

      when 1838 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
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

      when 1839 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
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
          Nodes (10),
          Nodes (11),
          Nodes (12),
          Nodes (13));

      when 1840 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
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
          Nodes (10),
          Nodes (11),
          None,
          Nodes (12));

      when 1841 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
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
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (9),
          Nodes (10),
          Nodes (11));

      when 1842 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
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
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (9),
          None,
          Nodes (10));

      when 1843 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
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
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (7),
          Nodes (8),
          Nodes (9));

      when 1844 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
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
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (7),
          None,
          Nodes (8));

      when 1845 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
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

      when 1846 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (4),
          Nodes (5),
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          Nodes (9),
          Nodes (10),
          None,
          Nodes (11));

      when 1847 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
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

      when 1848 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (4),
          Nodes (5),
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (6),
          Nodes (7),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (8),
          None,
          Nodes (9));

      when 1849 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (4),
          Nodes (5),
          (Self.Factory.Declarative_Item_Sequence),
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (6),
          Nodes (7),
          Nodes (8));

      when 1850 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (4),
          Nodes (5),
          (Self.Factory.Declarative_Item_Sequence),
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (6),
          None,
          Nodes (7));

      when 1851 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
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

      when 1852 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          Nodes (9),
          Nodes (10),
          None,
          Nodes (11));

      when 1853 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
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

      when 1854 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (8),
          None,
          Nodes (9));

      when 1855 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (4),
          Nodes (5),
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (6),
          Nodes (7),
          Nodes (8));

      when 1856 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (4),
          Nodes (5),
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (6),
          None,
          Nodes (7));

      when 1857 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
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

      when 1858 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (4),
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          Nodes (9),
          None,
          Nodes (10));

      when 1859 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
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

      when 1860 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (4),
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (5),
          Nodes (6),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (7),
          None,
          Nodes (8));

      when 1861 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (4),
          (Self.Factory.Declarative_Item_Sequence),
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (5),
          Nodes (6),
          Nodes (7));

      when 1862 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (4),
          (Self.Factory.Declarative_Item_Sequence),
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (5),
          None,
          Nodes (6));

      when 1863 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
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
          Nodes (12),
          Nodes (13),
          Nodes (14),
          Nodes (15));

      when 1864 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
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
          Nodes (12),
          Nodes (13),
          None,
          Nodes (14));

      when 1865 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
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
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (11),
          Nodes (12),
          Nodes (13));

      when 1866 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
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
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (11),
          None,
          Nodes (12));

      when 1867 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
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
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (9),
          Nodes (10),
          Nodes (11));

      when 1868 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
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
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (9),
          None,
          Nodes (10));

      when 1869 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
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

      when 1870 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
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

      when 1871 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
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

      when 1872 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
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

      when 1873 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
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

      when 1874 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
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

      when 1875 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
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

      when 1876 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
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

      when 1877 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
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

      when 1878 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
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

      when 1879 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
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

      when 1880 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
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

      when 1881 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
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

      when 1882 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
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

      when 1883 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
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

      when 1884 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
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

      when 1885 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
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

      when 1886 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
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

      when 1887 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
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
          Nodes (9),
          Nodes (10),
          Nodes (11),
          Nodes (12));

      when 1888 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
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
          Nodes (9),
          Nodes (10),
          None,
          Nodes (11));

      when 1889 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
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
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (8),
          Nodes (9),
          Nodes (10));

      when 1890 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
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
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (8),
          None,
          Nodes (9));

      when 1891 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
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
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (6),
          Nodes (7),
          Nodes (8));

      when 1892 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
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
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (6),
          None,
          Nodes (7));

      when 1893 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (3),
          Nodes (4),
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          Nodes (9),
          Nodes (10),
          Nodes (11));

      when 1894 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (3),
          Nodes (4),
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          Nodes (9),
          None,
          Nodes (10));

      when 1895 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (3),
          Nodes (4),
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (5),
          Nodes (6),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (7),
          Nodes (8),
          Nodes (9));

      when 1896 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (3),
          Nodes (4),
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (5),
          Nodes (6),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (7),
          None,
          Nodes (8));

      when 1897 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (3),
          Nodes (4),
          (Self.Factory.Declarative_Item_Sequence),
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (5),
          Nodes (6),
          Nodes (7));

      when 1898 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (3),
          Nodes (4),
          (Self.Factory.Declarative_Item_Sequence),
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (5),
          None,
          Nodes (6));

      when 1899 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          Nodes (9),
          Nodes (10),
          Nodes (11));

      when 1900 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          Nodes (9),
          None,
          Nodes (10));

      when 1901 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (7),
          Nodes (8),
          Nodes (9));

      when 1902 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (7),
          None,
          Nodes (8));

      when 1903 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (3),
          Nodes (4),
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (5),
          Nodes (6),
          Nodes (7));

      when 1904 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (3),
          Nodes (4),
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (5),
          None,
          Nodes (6));

      when 1905 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (3),
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          Nodes (9),
          Nodes (10));

      when 1906 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (3),
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          None,
          Nodes (9));

      when 1907 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (3),
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (4),
          Nodes (5),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (6),
          Nodes (7),
          Nodes (8));

      when 1908 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (3),
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (4),
          Nodes (5),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (6),
          None,
          Nodes (7));

      when 1909 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (3),
          (Self.Factory.Declarative_Item_Sequence),
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (4),
          Nodes (5),
          Nodes (6));

      when 1910 =>

   Nodes (1) :=
      Self.Factory.Procedure_Body
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (3),
          (Self.Factory.Declarative_Item_Sequence),
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (4),
          None,
          Nodes (5));

      when 1911 =>

   Nodes (1) :=
      Self.Factory.Procedure_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8), No_Token,
          No_Token,
          None,
          No_Token,
          Nodes (10),
          Nodes (11));

      when 1912 =>

   Nodes (1) :=
      Self.Factory.Procedure_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8), No_Token,
          No_Token,
          None,
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (10));

      when 1913 =>

   Nodes (1) :=
      Self.Factory.Procedure_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          No_Token, No_Token,
          Nodes (8),
          Nodes (9),
          No_Token,
          Nodes (10),
          Nodes (11));

      when 1914 =>

   Nodes (1) :=
      Self.Factory.Procedure_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          No_Token, No_Token,
          Nodes (8),
          Nodes (9),
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (10));

      when 1915 =>

   Nodes (1) :=
      Self.Factory.Procedure_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8), No_Token,
          No_Token,
          None,
          Nodes (9),
          Nodes (10),
          Nodes (11));

      when 1916 =>

   Nodes (1) :=
      Self.Factory.Procedure_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8), No_Token,
          No_Token,
          None,
          Nodes (9),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (10));

      when 1917 =>

   Nodes (1) :=
      Self.Factory.Procedure_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          No_Token, No_Token,
          No_Token,
          None,
          No_Token,
          Nodes (8),
          Nodes (9));

      when 1918 =>

   Nodes (1) :=
      Self.Factory.Procedure_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          No_Token, No_Token,
          No_Token,
          None,
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (8));

      when 1919 =>

   Nodes (1) :=
      Self.Factory.Procedure_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (5), No_Token,
          No_Token,
          None,
          No_Token,
          Nodes (7),
          Nodes (8));

      when 1920 =>

   Nodes (1) :=
      Self.Factory.Procedure_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (5), No_Token,
          No_Token,
          None,
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (7));

      when 1921 =>

   Nodes (1) :=
      Self.Factory.Procedure_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          No_Token, No_Token,
          Nodes (5),
          Nodes (6),
          No_Token,
          Nodes (7),
          Nodes (8));

      when 1922 =>

   Nodes (1) :=
      Self.Factory.Procedure_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          No_Token, No_Token,
          Nodes (5),
          Nodes (6),
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (7));

      when 1923 =>

   Nodes (1) :=
      Self.Factory.Procedure_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (5), No_Token,
          No_Token,
          None,
          Nodes (6),
          Nodes (7),
          Nodes (8));

      when 1924 =>

   Nodes (1) :=
      Self.Factory.Procedure_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (5), No_Token,
          No_Token,
          None,
          Nodes (6),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (7));

      when 1925 =>

   Nodes (1) :=
      Self.Factory.Procedure_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          No_Token, No_Token,
          No_Token,
          None,
          No_Token,
          Nodes (5),
          Nodes (6));

      when 1926 =>

   Nodes (1) :=
      Self.Factory.Procedure_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          No_Token, No_Token,
          No_Token,
          None,
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (5));

      when 1927 =>

   Nodes (1) :=
      Self.Factory.Procedure_Declaration
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7), No_Token,
          No_Token,
          None,
          No_Token,
          Nodes (9),
          Nodes (10));

      when 1928 =>

   Nodes (1) :=
      Self.Factory.Procedure_Declaration
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7), No_Token,
          No_Token,
          None,
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (9));

      when 1929 =>

   Nodes (1) :=
      Self.Factory.Procedure_Declaration
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          No_Token, No_Token,
          Nodes (7),
          Nodes (8),
          No_Token,
          Nodes (9),
          Nodes (10));

      when 1930 =>

   Nodes (1) :=
      Self.Factory.Procedure_Declaration
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          No_Token, No_Token,
          Nodes (7),
          Nodes (8),
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (9));

      when 1931 =>

   Nodes (1) :=
      Self.Factory.Procedure_Declaration
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7), No_Token,
          No_Token,
          None,
          Nodes (8),
          Nodes (9),
          Nodes (10));

      when 1932 =>

   Nodes (1) :=
      Self.Factory.Procedure_Declaration
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7), No_Token,
          No_Token,
          None,
          Nodes (8),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (9));

      when 1933 =>

   Nodes (1) :=
      Self.Factory.Procedure_Declaration
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          No_Token, No_Token,
          No_Token,
          None,
          No_Token,
          Nodes (7),
          Nodes (8));

      when 1934 =>

   Nodes (1) :=
      Self.Factory.Procedure_Declaration
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          No_Token, No_Token,
          No_Token,
          None,
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (7));

      when 1935 =>

   Nodes (1) :=
      Self.Factory.Procedure_Declaration
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (4), No_Token,
          No_Token,
          None,
          No_Token,
          Nodes (6),
          Nodes (7));

      when 1936 =>

   Nodes (1) :=
      Self.Factory.Procedure_Declaration
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (4), No_Token,
          No_Token,
          None,
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (6));

      when 1937 =>

   Nodes (1) :=
      Self.Factory.Procedure_Declaration
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          No_Token, No_Token,
          Nodes (4),
          Nodes (5),
          No_Token,
          Nodes (6),
          Nodes (7));

      when 1938 =>

   Nodes (1) :=
      Self.Factory.Procedure_Declaration
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          No_Token, No_Token,
          Nodes (4),
          Nodes (5),
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (6));

      when 1939 =>

   Nodes (1) :=
      Self.Factory.Procedure_Declaration
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (4), No_Token,
          No_Token,
          None,
          Nodes (5),
          Nodes (6),
          Nodes (7));

      when 1940 =>

   Nodes (1) :=
      Self.Factory.Procedure_Declaration
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (4), No_Token,
          No_Token,
          None,
          Nodes (5),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (6));

      when 1941 =>

   Nodes (1) :=
      Self.Factory.Procedure_Declaration
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          No_Token, No_Token,
          No_Token,
          None,
          No_Token,
          Nodes (4),
          Nodes (5));

      when 1942 =>

   Nodes (1) :=
      Self.Factory.Procedure_Declaration
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          No_Token, No_Token,
          No_Token,
          None,
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (4));

      when 1943 =>

   Nodes (1) :=
      Self.Factory.Procedure_Declaration
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6), No_Token,
          No_Token,
          None,
          No_Token,
          Nodes (8),
          Nodes (9));

      when 1944 =>

   Nodes (1) :=
      Self.Factory.Procedure_Declaration
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6), No_Token,
          No_Token,
          None,
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (8));

      when 1945 =>

   Nodes (1) :=
      Self.Factory.Procedure_Declaration
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          No_Token, No_Token,
          Nodes (6),
          Nodes (7),
          No_Token,
          Nodes (8),
          Nodes (9));

      when 1946 =>

   Nodes (1) :=
      Self.Factory.Procedure_Declaration
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          No_Token, No_Token,
          Nodes (6),
          Nodes (7),
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (8));

      when 1947 =>

   Nodes (1) :=
      Self.Factory.Procedure_Declaration
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6), No_Token,
          No_Token,
          None,
          Nodes (7),
          Nodes (8),
          Nodes (9));

      when 1948 =>

   Nodes (1) :=
      Self.Factory.Procedure_Declaration
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6), No_Token,
          No_Token,
          None,
          Nodes (7),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (8));

      when 1949 =>

   Nodes (1) :=
      Self.Factory.Procedure_Declaration
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          No_Token, No_Token,
          No_Token,
          None,
          No_Token,
          Nodes (6),
          Nodes (7));

      when 1950 =>

   Nodes (1) :=
      Self.Factory.Procedure_Declaration
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          No_Token, No_Token,
          No_Token,
          None,
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (6));

      when 1951 =>

   Nodes (1) :=
      Self.Factory.Procedure_Declaration
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (3), No_Token,
          No_Token,
          None,
          No_Token,
          Nodes (5),
          Nodes (6));

      when 1952 =>

   Nodes (1) :=
      Self.Factory.Procedure_Declaration
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (3), No_Token,
          No_Token,
          None,
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (5));

      when 1953 =>

   Nodes (1) :=
      Self.Factory.Procedure_Declaration
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          No_Token, No_Token,
          Nodes (3),
          Nodes (4),
          No_Token,
          Nodes (5),
          Nodes (6));

      when 1954 =>

   Nodes (1) :=
      Self.Factory.Procedure_Declaration
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          No_Token, No_Token,
          Nodes (3),
          Nodes (4),
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (5));

      when 1955 =>

   Nodes (1) :=
      Self.Factory.Procedure_Declaration
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (3), No_Token,
          No_Token,
          None,
          Nodes (4),
          Nodes (5),
          Nodes (6));

      when 1956 =>

   Nodes (1) :=
      Self.Factory.Procedure_Declaration
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (3), No_Token,
          No_Token,
          None,
          Nodes (4),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (5));

      when 1957 =>

   Nodes (1) :=
      Self.Factory.Procedure_Declaration
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          No_Token, No_Token,
          No_Token,
          None,
          No_Token,
          Nodes (3),
          Nodes (4));

      when 1958 =>

   Nodes (1) :=
      Self.Factory.Procedure_Declaration
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          No_Token, No_Token,
          No_Token,
          None,
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (3));

      when 1959 =>
         null;
      when 1960 =>
         null;
      when 1961 =>

   declare
      List : Node := Nodes (1);
   begin
      Self.Factory.Append_Program_Unit_Name
        (List, Nodes (3));
      Nodes (1) := List;
   end;

      when 1962 =>

   declare
      List : Node := Self.
        Factory.Program_Unit_Name_Sequence;
   begin
      Self.Factory.Append_Program_Unit_Name
        (List, Nodes (2));
      Nodes (1) := List;
   end;

      when 1963 =>
         null;
      when 1964 =>
         null;
      when 1965 =>
         null;
      when 1966 =>
         null;
      when 1967 =>
         null;
      when 1968 =>

   Nodes (1) :=
      Self.Factory.Protected_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          Nodes (9));

      when 1969 =>

   Nodes (1) :=
      Self.Factory.Protected_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          No_Token,
          Nodes (8));

      when 1970 =>

   Nodes (1) :=
      Self.Factory.Protected_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          (Self.Factory.Protected_Operation_Item_Sequence),
          Nodes (6),
          Nodes (7),
          Nodes (8));

      when 1971 =>

   Nodes (1) :=
      Self.Factory.Protected_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          (Self.Factory.Protected_Operation_Item_Sequence),
          Nodes (6),
          No_Token,
          Nodes (7));

      when 1972 =>

   Nodes (1) :=
      Self.Factory.Protected_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8));

      when 1973 =>

   Nodes (1) :=
      Self.Factory.Protected_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          No_Token,
          Nodes (7));

      when 1974 =>

   Nodes (1) :=
      Self.Factory.Protected_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (4),
          (Self.Factory.Protected_Operation_Item_Sequence),
          Nodes (5),
          Nodes (6),
          Nodes (7));

      when 1975 =>

   Nodes (1) :=
      Self.Factory.Protected_Body
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (4),
          (Self.Factory.Protected_Operation_Item_Sequence),
          Nodes (5),
          No_Token,
          Nodes (6));

      when 1976 =>

   Nodes (1) :=
      Self.Factory.Protected_Body_Stub
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7));

      when 1977 =>

   Nodes (1) :=
      Self.Factory.Protected_Body_Stub
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (6));

      when 1978 =>

   Nodes (1) :=
      Self.Factory.Protected_Definition
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5));

      when 1979 =>

   Nodes (1) :=
      Self.Factory.Protected_Definition
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token);

      when 1980 =>

   Nodes (1) :=
      Self.Factory.Protected_Definition
         (Nodes (1),
          Nodes (2),
          (Self.Factory.Protected_Element_Declaration_Sequence),
          Nodes (3),
          Nodes (4));

      when 1981 =>

   Nodes (1) :=
      Self.Factory.Protected_Definition
         (Nodes (1),
          Nodes (2),
          (Self.Factory.Protected_Element_Declaration_Sequence),
          Nodes (3),
          No_Token);

      when 1982 =>

   Nodes (1) :=
      Self.Factory.Protected_Definition
         (Nodes (1),
          No_Token,
          (Self.Factory.Protected_Element_Declaration_Sequence),
          Nodes (2),
          Nodes (3));

      when 1983 =>

   Nodes (1) :=
      Self.Factory.Protected_Definition
         (Nodes (1),
          No_Token,
          (Self.Factory.Protected_Element_Declaration_Sequence),
          Nodes (2),
          No_Token);

      when 1984 =>

   Nodes (1) :=
      Self.Factory.Protected_Definition
         ((Self.Factory.Protected_Operation_Declaration_Sequence),
          Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4));

      when 1985 =>

   Nodes (1) :=
      Self.Factory.Protected_Definition
         ((Self.Factory.Protected_Operation_Declaration_Sequence),
          Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token);

      when 1986 =>

   Nodes (1) :=
      Self.Factory.Protected_Definition
         ((Self.Factory.Protected_Operation_Declaration_Sequence),
          Nodes (1),
          (Self.Factory.Protected_Element_Declaration_Sequence),
          Nodes (2),
          Nodes (3));

      when 1987 =>

   Nodes (1) :=
      Self.Factory.Protected_Definition
         ((Self.Factory.Protected_Operation_Declaration_Sequence),
          Nodes (1),
          (Self.Factory.Protected_Element_Declaration_Sequence),
          Nodes (2),
          No_Token);

      when 1988 =>

   Nodes (1) :=
      Self.Factory.Protected_Definition
         ((Self.Factory.Protected_Operation_Declaration_Sequence),
          No_Token,
          (Self.Factory.Protected_Element_Declaration_Sequence),
          Nodes (1),
          Nodes (2));

      when 1989 =>

   Nodes (1) :=
      Self.Factory.Protected_Definition
         ((Self.Factory.Protected_Operation_Declaration_Sequence),
          No_Token,
          (Self.Factory.Protected_Element_Declaration_Sequence),
          Nodes (1),
          No_Token);

      when 1990 =>
         null;
      when 1991 =>
         null;
      when 1992 =>

   declare
      List : Node := Nodes (1);
   begin
      Self.Factory.Append_Protected_Element_Declaration
        (List, Nodes (2));
      Nodes (1) := List;
   end;

      when 1993 =>

   declare
      List : Node := Self.
        Factory.Protected_Element_Declaration_Sequence;
   begin
      Self.Factory.Append_Protected_Element_Declaration
        (List, Nodes (1));
      Nodes (1) := List;
   end;

      when 1994 =>
         null;
      when 1995 =>
         null;
      when 1996 =>
         null;
      when 1997 =>
         null;
      when 1998 =>

   declare
      List : Node := Nodes (1);
   begin
      Self.Factory.Append_Protected_Operation_Declaration
        (List, Nodes (2));
      Nodes (1) := List;
   end;

      when 1999 =>

   declare
      List : Node := Self.
        Factory.Protected_Operation_Declaration_Sequence;
   begin
      Self.Factory.Append_Protected_Operation_Declaration
        (List, Nodes (1));
      Nodes (1) := List;
   end;

      when 2000 =>
         null;
      when others =>
         raise Constraint_Error;
   end case;
end Program.Parsers.On_Reduce_1501;
