
with Program.Parsers.Nodes;
use Program.Parsers.Nodes;
pragma Style_Checks ("N");
procedure Program.Parsers.On_Reduce_501
  (Self  : access Parse_Context;
   Prod  : Anagram.Grammars.Production_Index;
   Nodes : in out Program.Parsers.Nodes.Node_Array) is
begin
   case Prod is
      when 501 =>

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
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (9));

      when 502 =>

   Nodes (1) :=
      Self.Factory.Formal_Function_Declaration
         (Nodes (1),
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
          No_Token,
          Nodes (11),
          Nodes (12));

      when 503 =>

   Nodes (1) :=
      Self.Factory.Formal_Function_Declaration
         (Nodes (1),
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
          No_Token,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (11));

      when 504 =>

   Nodes (1) :=
      Self.Factory.Formal_Function_Declaration
         (Nodes (1),
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
          Nodes (10),
          Nodes (11),
          Nodes (12));

      when 505 =>

   Nodes (1) :=
      Self.Factory.Formal_Function_Declaration
         (Nodes (1),
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
          Nodes (10),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (11));

      when 506 =>

   Nodes (1) :=
      Self.Factory.Formal_Function_Declaration
         (Nodes (1),
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
          Nodes (10),
          Nodes (11));

      when 507 =>

   Nodes (1) :=
      Self.Factory.Formal_Function_Declaration
         (Nodes (1),
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
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (10));

      when 508 =>

   Nodes (1) :=
      Self.Factory.Formal_Function_Declaration
         (Nodes (1),
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
          Nodes (10),
          Nodes (11));

      when 509 =>

   Nodes (1) :=
      Self.Factory.Formal_Function_Declaration
         (Nodes (1),
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
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (10));

      when 510 =>

   Nodes (1) :=
      Self.Factory.Formal_Function_Declaration
         (Nodes (1),
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
          Nodes (9),
          Nodes (10),
          Nodes (11));

      when 511 =>

   Nodes (1) :=
      Self.Factory.Formal_Function_Declaration
         (Nodes (1),
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
          Nodes (9),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (10));

      when 512 =>

   Nodes (1) :=
      Self.Factory.Formal_Function_Declaration
         (Nodes (1),
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
          Nodes (8),
          Nodes (9));

      when 513 =>

   Nodes (1) :=
      Self.Factory.Formal_Function_Declaration
         (Nodes (1),
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
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (8));

      when 514 =>

   Nodes (1) :=
      Self.Factory.Formal_Function_Declaration
         (Nodes (1),
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
          Nodes (9),
          Nodes (10));

      when 515 =>

   Nodes (1) :=
      Self.Factory.Formal_Function_Declaration
         (Nodes (1),
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
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (9));

      when 516 =>

   Nodes (1) :=
      Self.Factory.Formal_Function_Declaration
         (Nodes (1),
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
          Nodes (8),
          Nodes (9),
          Nodes (10));

      when 517 =>

   Nodes (1) :=
      Self.Factory.Formal_Function_Declaration
         (Nodes (1),
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
          Nodes (8),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (9));

      when 518 =>

   Nodes (1) :=
      Self.Factory.Formal_Function_Declaration
         (Nodes (1),
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
          Nodes (8),
          Nodes (9));

      when 519 =>

   Nodes (1) :=
      Self.Factory.Formal_Function_Declaration
         (Nodes (1),
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
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (8));

      when 520 =>

   Nodes (1) :=
      Self.Factory.Formal_Function_Declaration
         (Nodes (1),
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
          Nodes (8),
          Nodes (9));

      when 521 =>

   Nodes (1) :=
      Self.Factory.Formal_Function_Declaration
         (Nodes (1),
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
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (8));

      when 522 =>

   Nodes (1) :=
      Self.Factory.Formal_Function_Declaration
         (Nodes (1),
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
          Nodes (7),
          Nodes (8),
          Nodes (9));

      when 523 =>

   Nodes (1) :=
      Self.Factory.Formal_Function_Declaration
         (Nodes (1),
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
          Nodes (7),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (8));

      when 524 =>

   Nodes (1) :=
      Self.Factory.Formal_Function_Declaration
         (Nodes (1),
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
          Nodes (6),
          Nodes (7));

      when 525 =>

   Nodes (1) :=
      Self.Factory.Formal_Function_Declaration
         (Nodes (1),
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
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (6));

      when 526 =>

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

      when 527 =>

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

      when 528 =>

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

      when 529 =>

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

      when 530 =>

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

      when 531 =>

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

      when 532 =>

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

      when 533 =>

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

      when 534 =>

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

      when 535 =>

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

      when 536 =>

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

      when 537 =>

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
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (9));

      when 538 =>

   Nodes (1) :=
      Self.Factory.Formal_Function_Declaration
         (Nodes (1),
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
          Nodes (9),
          Nodes (10));

      when 539 =>

   Nodes (1) :=
      Self.Factory.Formal_Function_Declaration
         (Nodes (1),
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
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (9));

      when 540 =>

   Nodes (1) :=
      Self.Factory.Formal_Function_Declaration
         (Nodes (1),
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
          Nodes (8),
          Nodes (9),
          Nodes (10));

      when 541 =>

   Nodes (1) :=
      Self.Factory.Formal_Function_Declaration
         (Nodes (1),
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
          Nodes (8),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (9));

      when 542 =>

   Nodes (1) :=
      Self.Factory.Formal_Function_Declaration
         (Nodes (1),
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
          Nodes (8),
          Nodes (9));

      when 543 =>

   Nodes (1) :=
      Self.Factory.Formal_Function_Declaration
         (Nodes (1),
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
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (8));

      when 544 =>

   Nodes (1) :=
      Self.Factory.Formal_Function_Declaration
         (Nodes (1),
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
          Nodes (8),
          Nodes (9));

      when 545 =>

   Nodes (1) :=
      Self.Factory.Formal_Function_Declaration
         (Nodes (1),
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
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (8));

      when 546 =>

   Nodes (1) :=
      Self.Factory.Formal_Function_Declaration
         (Nodes (1),
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
          Nodes (7),
          Nodes (8),
          Nodes (9));

      when 547 =>

   Nodes (1) :=
      Self.Factory.Formal_Function_Declaration
         (Nodes (1),
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
          Nodes (7),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (8));

      when 548 =>

   Nodes (1) :=
      Self.Factory.Formal_Function_Declaration
         (Nodes (1),
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
          Nodes (6),
          Nodes (7));

      when 549 =>

   Nodes (1) :=
      Self.Factory.Formal_Function_Declaration
         (Nodes (1),
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
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (6));

      when 550 =>

   Nodes (1) :=
      Self.Factory.Formal_Incomplete_Type_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6));

      when 551 =>

   Nodes (1) :=
      Self.Factory.Formal_Incomplete_Type_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          No_Token,
          Nodes (4));

      when 552 =>

   Nodes (1) :=
      Self.Factory.Formal_Incomplete_Type_Declaration
         (Nodes (1),
          Nodes (2),
          None,
          Nodes (3),
          Nodes (4),
          Nodes (5));

      when 553 =>

   Nodes (1) :=
      Self.Factory.Formal_Incomplete_Type_Declaration
         (Nodes (1),
          Nodes (2),
          None,
          No_Token,
          No_Token,
          Nodes (3));

      when 554 =>

   Nodes (1) := Self.Factory.
     Formal_Interface_Type_Definition
       (Nodes (1),
        Nodes (2),
        Nodes (3));

      when 555 =>

   Nodes (1) := Self.Factory.
     Formal_Interface_Type_Definition
       (Nodes (1),
        Nodes (2),
        (Self.Factory.Subtype_Mark_Sequence));

      when 556 =>

   Nodes (1) := Self.Factory.
     Formal_Interface_Type_Definition
       (No_Token,
        Nodes (1),
        Nodes (2));

      when 557 =>

   Nodes (1) := Self.Factory.
     Formal_Interface_Type_Definition
       (No_Token,
        Nodes (1),
        (Self.Factory.Subtype_Mark_Sequence));

      when 558 =>

   Nodes (1) :=
      Self.Factory.Formal_Modular_Type_Definition
         (Nodes (1),
          Nodes (2));

      when 559 =>

   Nodes (1) :=
      Self.Factory.Formal_Object_Declaration
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

      when 560 =>

   Nodes (1) :=
      Self.Factory.Formal_Object_Declaration
         (Nodes (1),
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

      when 561 =>

   Nodes (1) :=
      Self.Factory.Formal_Object_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          No_Token,
          None,
          Nodes (8),
          Nodes (9));

      when 562 =>

   Nodes (1) :=
      Self.Factory.Formal_Object_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          No_Token,
          None,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (8));

      when 563 =>

   Nodes (1) :=
      Self.Factory.Formal_Object_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token,
          No_Token,
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          Nodes (9));

      when 564 =>

   Nodes (1) :=
      Self.Factory.Formal_Object_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token,
          No_Token,
          Nodes (5),
          Nodes (6),
          Nodes (7),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (8));

      when 565 =>

   Nodes (1) :=
      Self.Factory.Formal_Object_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token,
          No_Token,
          Nodes (5),
          No_Token,
          None,
          Nodes (6),
          Nodes (7));

      when 566 =>

   Nodes (1) :=
      Self.Factory.Formal_Object_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token,
          No_Token,
          Nodes (5),
          No_Token,
          None,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (6));

      when 567 =>

   Nodes (1) :=
      Self.Factory.Formal_Object_Declaration
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
          Nodes (10));

      when 568 =>

   Nodes (1) :=
      Self.Factory.Formal_Object_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (9));

      when 569 =>

   Nodes (1) :=
      Self.Factory.Formal_Object_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          Nodes (4),
          Nodes (5),
          Nodes (6),
          No_Token,
          None,
          Nodes (7),
          Nodes (8));

      when 570 =>

   Nodes (1) :=
      Self.Factory.Formal_Object_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          Nodes (4),
          Nodes (5),
          Nodes (6),
          No_Token,
          None,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (7));

      when 571 =>

   Nodes (1) :=
      Self.Factory.Formal_Object_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          No_Token,
          No_Token,
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8));

      when 572 =>

   Nodes (1) :=
      Self.Factory.Formal_Object_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          No_Token,
          No_Token,
          Nodes (4),
          Nodes (5),
          Nodes (6),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (7));

      when 573 =>

   Nodes (1) :=
      Self.Factory.Formal_Object_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          No_Token,
          No_Token,
          Nodes (4),
          No_Token,
          None,
          Nodes (5),
          Nodes (6));

      when 574 =>

   Nodes (1) :=
      Self.Factory.Formal_Object_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          No_Token,
          No_Token,
          Nodes (4),
          No_Token,
          None,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (5));

      when 575 =>

   Nodes (1) :=
      Self.Factory.Formal_Object_Declaration
         (Nodes (1),
          Nodes (2),
          No_Token,
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          Nodes (9),
          Nodes (10));

      when 576 =>

   Nodes (1) :=
      Self.Factory.Formal_Object_Declaration
         (Nodes (1),
          Nodes (2),
          No_Token,
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (9));

      when 577 =>

   Nodes (1) :=
      Self.Factory.Formal_Object_Declaration
         (Nodes (1),
          Nodes (2),
          No_Token,
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          No_Token,
          None,
          Nodes (7),
          Nodes (8));

      when 578 =>

   Nodes (1) :=
      Self.Factory.Formal_Object_Declaration
         (Nodes (1),
          Nodes (2),
          No_Token,
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          No_Token,
          None,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (7));

      when 579 =>

   Nodes (1) :=
      Self.Factory.Formal_Object_Declaration
         (Nodes (1),
          Nodes (2),
          No_Token,
          Nodes (3),
          No_Token,
          No_Token,
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8));

      when 580 =>

   Nodes (1) :=
      Self.Factory.Formal_Object_Declaration
         (Nodes (1),
          Nodes (2),
          No_Token,
          Nodes (3),
          No_Token,
          No_Token,
          Nodes (4),
          Nodes (5),
          Nodes (6),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (7));

      when 581 =>

   Nodes (1) :=
      Self.Factory.Formal_Object_Declaration
         (Nodes (1),
          Nodes (2),
          No_Token,
          Nodes (3),
          No_Token,
          No_Token,
          Nodes (4),
          No_Token,
          None,
          Nodes (5),
          Nodes (6));

      when 582 =>

   Nodes (1) :=
      Self.Factory.Formal_Object_Declaration
         (Nodes (1),
          Nodes (2),
          No_Token,
          Nodes (3),
          No_Token,
          No_Token,
          Nodes (4),
          No_Token,
          None,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (5));

      when 583 =>

   Nodes (1) :=
      Self.Factory.Formal_Object_Declaration
         (Nodes (1),
          Nodes (2),
          No_Token,
          No_Token,
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          Nodes (9));

      when 584 =>

   Nodes (1) :=
      Self.Factory.Formal_Object_Declaration
         (Nodes (1),
          Nodes (2),
          No_Token,
          No_Token,
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (8));

      when 585 =>

   Nodes (1) :=
      Self.Factory.Formal_Object_Declaration
         (Nodes (1),
          Nodes (2),
          No_Token,
          No_Token,
          Nodes (3),
          Nodes (4),
          Nodes (5),
          No_Token,
          None,
          Nodes (6),
          Nodes (7));

      when 586 =>

   Nodes (1) :=
      Self.Factory.Formal_Object_Declaration
         (Nodes (1),
          Nodes (2),
          No_Token,
          No_Token,
          Nodes (3),
          Nodes (4),
          Nodes (5),
          No_Token,
          None,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (6));

      when 587 =>

   Nodes (1) :=
      Self.Factory.Formal_Object_Declaration
         (Nodes (1),
          Nodes (2),
          No_Token,
          No_Token,
          No_Token,
          No_Token,
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7));

      when 588 =>

   Nodes (1) :=
      Self.Factory.Formal_Object_Declaration
         (Nodes (1),
          Nodes (2),
          No_Token,
          No_Token,
          No_Token,
          No_Token,
          Nodes (3),
          Nodes (4),
          Nodes (5),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (6));

      when 589 =>

   Nodes (1) :=
      Self.Factory.Formal_Object_Declaration
         (Nodes (1),
          Nodes (2),
          No_Token,
          No_Token,
          No_Token,
          No_Token,
          Nodes (3),
          No_Token,
          None,
          Nodes (4),
          Nodes (5));

      when 590 =>

   Nodes (1) :=
      Self.Factory.Formal_Object_Declaration
         (Nodes (1),
          Nodes (2),
          No_Token,
          No_Token,
          No_Token,
          No_Token,
          Nodes (3),
          No_Token,
          None,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (4));

      when 591 =>

   Nodes (1) :=
      Self.Factory.Formal_Object_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token,
          No_Token,
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          Nodes (9));

      when 592 =>

   Nodes (1) :=
      Self.Factory.Formal_Object_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token,
          No_Token,
          Nodes (5),
          Nodes (6),
          Nodes (7),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (8));

      when 593 =>

   Nodes (1) :=
      Self.Factory.Formal_Object_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token,
          No_Token,
          Nodes (5),
          No_Token,
          None,
          Nodes (6),
          Nodes (7));

      when 594 =>

   Nodes (1) :=
      Self.Factory.Formal_Object_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          No_Token,
          No_Token,
          Nodes (5),
          No_Token,
          None,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (6));

      when 595 =>

   Nodes (1) :=
      Self.Factory.Formal_Object_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          No_Token,
          No_Token,
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8));

      when 596 =>

   Nodes (1) :=
      Self.Factory.Formal_Object_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          No_Token,
          No_Token,
          Nodes (4),
          Nodes (5),
          Nodes (6),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (7));

      when 597 =>

   Nodes (1) :=
      Self.Factory.Formal_Object_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          No_Token,
          No_Token,
          Nodes (4),
          No_Token,
          None,
          Nodes (5),
          Nodes (6));

      when 598 =>

   Nodes (1) :=
      Self.Factory.Formal_Object_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          No_Token,
          No_Token,
          Nodes (4),
          No_Token,
          None,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (5));

      when 599 =>

   Nodes (1) :=
      Self.Factory.Formal_Object_Declaration
         (Nodes (1),
          Nodes (2),
          No_Token,
          Nodes (3),
          No_Token,
          No_Token,
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8));

      when 600 =>

   Nodes (1) :=
      Self.Factory.Formal_Object_Declaration
         (Nodes (1),
          Nodes (2),
          No_Token,
          Nodes (3),
          No_Token,
          No_Token,
          Nodes (4),
          Nodes (5),
          Nodes (6),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (7));

      when 601 =>

   Nodes (1) :=
      Self.Factory.Formal_Object_Declaration
         (Nodes (1),
          Nodes (2),
          No_Token,
          Nodes (3),
          No_Token,
          No_Token,
          Nodes (4),
          No_Token,
          None,
          Nodes (5),
          Nodes (6));

      when 602 =>

   Nodes (1) :=
      Self.Factory.Formal_Object_Declaration
         (Nodes (1),
          Nodes (2),
          No_Token,
          Nodes (3),
          No_Token,
          No_Token,
          Nodes (4),
          No_Token,
          None,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (5));

      when 603 =>

   Nodes (1) :=
      Self.Factory.Formal_Object_Declaration
         (Nodes (1),
          Nodes (2),
          No_Token,
          No_Token,
          No_Token,
          No_Token,
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7));

      when 604 =>

   Nodes (1) :=
      Self.Factory.Formal_Object_Declaration
         (Nodes (1),
          Nodes (2),
          No_Token,
          No_Token,
          No_Token,
          No_Token,
          Nodes (3),
          Nodes (4),
          Nodes (5),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (6));

      when 605 =>

   Nodes (1) :=
      Self.Factory.Formal_Object_Declaration
         (Nodes (1),
          Nodes (2),
          No_Token,
          No_Token,
          No_Token,
          No_Token,
          Nodes (3),
          No_Token,
          None,
          Nodes (4),
          Nodes (5));

      when 606 =>

   Nodes (1) :=
      Self.Factory.Formal_Object_Declaration
         (Nodes (1),
          Nodes (2),
          No_Token,
          No_Token,
          No_Token,
          No_Token,
          Nodes (3),
          No_Token,
          None,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (4));

      when 607 =>

   Nodes (1) := Self.Factory
     .Formal_Ordinary_Fixed_Point_Definition
         (Nodes (1),
          Nodes (2));

      when 608 =>

   declare
      List : Node :=
        Self.Factory.Generic_Association_Sequence;
      Box  : constant Node :=
        Self.Factory.Generic_Association
          (Nodes (1),
           Nodes (2),
           None,
           Nodes (3));
   begin
      Self.Factory.Append_Generic_Association
        (List, Box);
      Nodes (1) := List;
   end;

      when 609 =>

   declare
      List : Node :=
        Self.Factory.Generic_Association_Sequence;
      Box  : constant Node :=
        Self.Factory.Generic_Association
          (None,
           No_Token,
           None,
           Nodes (1));
   begin
      Self.Factory.Append_Generic_Association
        (List, Box);
      Nodes (1) := List;
   end;

      when 610 =>

   declare
      List : Node :=
        Nodes (2);
      Box  : constant Node :=
        Self.Factory.Generic_Association
          (Nodes (4),
           Nodes (5),
           None,
           Nodes (6));
   begin
      Self.Factory.Prepend_Generic_Association
        (List, Nodes (1));
      Self.Factory.Append_Generic_Association
        (List, Box);
      Nodes (1) := List;
   end;

      when 611 =>

   declare
      List : Node :=
        Nodes (2);
   begin
      Self.Factory.Prepend_Generic_Association
        (List, Nodes (1));
      Nodes (1) := List;
   end;

      when 612 =>

   declare
      List : Node :=
        Self.Factory.Generic_Association_Sequence;
      Box  : constant Node :=
        Self.Factory.Generic_Association
          (Nodes (3),
           Nodes (4),
           None,
           Nodes (5));
   begin
      Self.Factory.Prepend_Generic_Association
        (List, Nodes (1));
      Self.Factory.Append_Generic_Association
        (List, Box);
      Nodes (1) := List;
   end;

      when 613 =>

   declare
      List : Node :=
        Self.Factory.Generic_Association_Sequence;
   begin
      Self.Factory.Prepend_Generic_Association
        (List, Nodes (1));
      Nodes (1) := List;
   end;

      when 614 =>
         null;
      when 615 =>

   Nodes (1) :=
      Self.Factory.Generic_Association
         (Nodes (1),
          Nodes (2),
          None,
          Nodes (3));

      when 616 =>

   declare
      List : Node := Nodes (1);
   begin
      Self.Factory.Append_Generic_Association
        (List, Nodes (3));
      Nodes (1) := List;
   end;

      when 617 =>

   declare
      List : Node := Self.
        Factory.Generic_Association_Sequence;
   begin
      Self.Factory.Append_Generic_Association
        (List, Nodes (2));
      Nodes (1) := List;
   end;

      when 618 =>

   Nodes (1) :=
      Self.Factory.Formal_Package_Declaration
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

      when 619 =>

   Nodes (1) :=
      Self.Factory.Formal_Package_Declaration
         (Nodes (1),
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

      when 620 =>

   Nodes (1) :=
      Self.Factory.Formal_Package_Declaration
         (Nodes (1),
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

      when 621 =>

   Nodes (1) :=
      Self.Factory.Formal_Package_Declaration
         (Nodes (1),
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

      when 622 =>

   declare
      List : Node :=
        Nodes (2);
   begin
      Self.Factory.Prepend_Parameter_Specification
        (List, Nodes (1));
      Nodes (1) := List;
   end;

      when 623 =>

   declare
      List : Node :=
        Self.Factory.Parameter_Specification_Sequence;
   begin
      Self.Factory.Prepend_Parameter_Specification
        (List, Nodes (1));
      Nodes (1) := List;
   end;

      when 624 =>

   Nodes (1) :=
      Self.Factory.Formal_Private_Type_Definition
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4));

      when 625 =>

   Nodes (1) :=
      Self.Factory.Formal_Private_Type_Definition
         (Nodes (1),
          Nodes (2),
          No_Token,
          Nodes (3));

      when 626 =>

   Nodes (1) :=
      Self.Factory.Formal_Private_Type_Definition
         (No_Token,
          Nodes (1),
          Nodes (2),
          Nodes (3));

      when 627 =>

   Nodes (1) :=
      Self.Factory.Formal_Private_Type_Definition
         (No_Token,
          Nodes (1),
          No_Token,
          Nodes (2));

      when 628 =>

   Nodes (1) :=
      Self.Factory.Formal_Private_Type_Definition
         (No_Token,
          No_Token,
          Nodes (1),
          Nodes (2));

      when 629 =>

   Nodes (1) :=
      Self.Factory.Formal_Private_Type_Definition
         (No_Token,
          No_Token,
          No_Token,
          Nodes (1));

      when 630 =>

   Nodes (1) :=
      Self.Factory.Formal_Procedure_Declaration
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

      when 631 =>

   Nodes (1) :=
      Self.Factory.Formal_Procedure_Declaration
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

      when 632 =>

   Nodes (1) :=
      Self.Factory.Formal_Procedure_Declaration
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
          None,
          Nodes (10),
          Nodes (11));

      when 633 =>

   Nodes (1) :=
      Self.Factory.Formal_Procedure_Declaration
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
          None,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (10));

      when 634 =>

   Nodes (1) :=
      Self.Factory.Formal_Procedure_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          No_Token,
          Nodes (9),
          None,
          Nodes (10),
          Nodes (11));

      when 635 =>

   Nodes (1) :=
      Self.Factory.Formal_Procedure_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          Nodes (8),
          No_Token,
          Nodes (9),
          None,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (10));

      when 636 =>

   Nodes (1) :=
      Self.Factory.Formal_Procedure_Declaration
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
          None,
          Nodes (9),
          Nodes (10));

      when 637 =>

   Nodes (1) :=
      Self.Factory.Formal_Procedure_Declaration
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
          None,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (9));

      when 638 =>

   Nodes (1) :=
      Self.Factory.Formal_Procedure_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          No_Token,
          No_Token,
          No_Token,
          Nodes (8),
          Nodes (9),
          Nodes (10));

      when 639 =>

   Nodes (1) :=
      Self.Factory.Formal_Procedure_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          No_Token,
          No_Token,
          No_Token,
          Nodes (8),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (9));

      when 640 =>

   Nodes (1) :=
      Self.Factory.Formal_Procedure_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          No_Token,
          Nodes (8),
          No_Token,
          None,
          Nodes (9),
          Nodes (10));

      when 641 =>

   Nodes (1) :=
      Self.Factory.Formal_Procedure_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7),
          No_Token,
          Nodes (8),
          No_Token,
          None,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (9));

      when 642 =>

   Nodes (1) :=
      Self.Factory.Formal_Procedure_Declaration
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
          None,
          Nodes (9),
          Nodes (10));

      when 643 =>

   Nodes (1) :=
      Self.Factory.Formal_Procedure_Declaration
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
          None,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (9));

      when 644 =>

   Nodes (1) :=
      Self.Factory.Formal_Procedure_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          No_Token,
          No_Token,
          No_Token,
          No_Token,
          None,
          Nodes (7),
          Nodes (8));

      when 645 =>

   Nodes (1) :=
      Self.Factory.Formal_Procedure_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          No_Token,
          No_Token,
          No_Token,
          No_Token,
          None,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (7));

      when 646 =>

   Nodes (1) :=
      Self.Factory.Formal_Procedure_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (4),
          Nodes (5),
          No_Token,
          No_Token,
          Nodes (6),
          Nodes (7),
          Nodes (8));

      when 647 =>

   Nodes (1) :=
      Self.Factory.Formal_Procedure_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (4),
          Nodes (5),
          No_Token,
          No_Token,
          Nodes (6),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (7));

      when 648 =>

   Nodes (1) :=
      Self.Factory.Formal_Procedure_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (4),
          Nodes (5),
          Nodes (6),
          No_Token,
          None,
          Nodes (7),
          Nodes (8));

      when 649 =>

   Nodes (1) :=
      Self.Factory.Formal_Procedure_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (4),
          Nodes (5),
          Nodes (6),
          No_Token,
          None,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (7));

      when 650 =>

   Nodes (1) :=
      Self.Factory.Formal_Procedure_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (4),
          Nodes (5),
          No_Token,
          Nodes (6),
          None,
          Nodes (7),
          Nodes (8));

      when 651 =>

   Nodes (1) :=
      Self.Factory.Formal_Procedure_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (4),
          Nodes (5),
          No_Token,
          Nodes (6),
          None,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (7));

      when 652 =>

   Nodes (1) :=
      Self.Factory.Formal_Procedure_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (4),
          Nodes (5),
          No_Token,
          No_Token,
          None,
          Nodes (6),
          Nodes (7));

      when 653 =>

   Nodes (1) :=
      Self.Factory.Formal_Procedure_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (4),
          Nodes (5),
          No_Token,
          No_Token,
          None,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (6));

      when 654 =>

   Nodes (1) :=
      Self.Factory.Formal_Procedure_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (4),
          No_Token,
          No_Token,
          No_Token,
          Nodes (5),
          Nodes (6),
          Nodes (7));

      when 655 =>

   Nodes (1) :=
      Self.Factory.Formal_Procedure_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (4),
          No_Token,
          No_Token,
          No_Token,
          Nodes (5),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (6));

      when 656 =>

   Nodes (1) :=
      Self.Factory.Formal_Procedure_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (4),
          No_Token,
          Nodes (5),
          No_Token,
          None,
          Nodes (6),
          Nodes (7));

      when 657 =>

   Nodes (1) :=
      Self.Factory.Formal_Procedure_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (4),
          No_Token,
          Nodes (5),
          No_Token,
          None,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (6));

      when 658 =>

   Nodes (1) :=
      Self.Factory.Formal_Procedure_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (4),
          No_Token,
          No_Token,
          Nodes (5),
          None,
          Nodes (6),
          Nodes (7));

      when 659 =>

   Nodes (1) :=
      Self.Factory.Formal_Procedure_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          Nodes (4),
          No_Token,
          No_Token,
          Nodes (5),
          None,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (6));

      when 660 =>

   Nodes (1) :=
      Self.Factory.Formal_Procedure_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          No_Token,
          No_Token,
          No_Token,
          No_Token,
          None,
          Nodes (4),
          Nodes (5));

      when 661 =>

   Nodes (1) :=
      Self.Factory.Formal_Procedure_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          No_Token,
          (Self.Factory.Parameter_Specification_Sequence),
          No_Token,
          No_Token,
          No_Token,
          No_Token,
          No_Token,
          None,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (4));

      when 662 =>

   Nodes (1) := Self.Factory
     .Formal_Signed_Integer_Type_Definition
         (Nodes (1),
          Nodes (2));

      when 663 =>
         null;
      when 664 =>
         null;
      when 665 =>
         null;
      when 666 =>
         null;
      when 667 =>
         null;
      when 668 =>
         null;
      when 669 =>
         null;
      when 670 =>
         null;
      when 671 =>
         null;
      when 672 =>
         null;
      when 673 =>
         null;
      when 674 =>

   declare
      List : Node :=
        Nodes (4);
   begin
      Self.Factory.Prepend_Subtype_Mark
        (List, Nodes (3));
      Nodes (1) := Self.Factory.
        Formal_Unconstrained_Array_Definition
          (Nodes (1),
           Nodes (2),
           List,
           Nodes (5),
           Nodes (6),
           Nodes (7));
   end;

      when 675 =>

   declare
      List : Node :=
        Self.Factory.
        Subtype_Mark_Sequence;
   begin
      Self.Factory.Prepend_Subtype_Mark
        (List, Nodes (3));
      Nodes (1) := Self.Factory.
        Formal_Unconstrained_Array_Definition
          (Nodes (1),
           Nodes (2),
           List,
           Nodes (4),
           Nodes (5),
           Nodes (6));
   end;

      when 676 =>
  Nodes (1) :=
       Self.Factory.Full_Type_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6),
          Nodes (7));

      when 677 =>
  Nodes (1) :=
       Self.Factory.Full_Type_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Nodes (5),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (6));

      when 678 =>
  Nodes (1) :=
       Self.Factory.Full_Type_Declaration
         (Nodes (1),
          Nodes (2),
          None,
          Nodes (3),
          Nodes (4),
          Nodes (5),
          Nodes (6));

      when 679 =>
  Nodes (1) :=
       Self.Factory.Full_Type_Declaration
         (Nodes (1),
          Nodes (2),
          None,
          Nodes (3),
          Nodes (4),
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (5));

      when 680 =>

     declare
        Type_Definition : constant Node :=
          Self.Factory.Derived_Record_Definition
            (Nodes (5),
             Nodes (6),
             Nodes (7),
             Nodes (8),
             Nodes (9),
             Nodes (10),
             Nodes (11));
     begin
        Nodes (1) :=
          Self.Factory.Full_Type_Declaration
            (Nodes (1),
            Nodes (2),
            Nodes (3),
            Nodes (4),
            Type_Definition,
            Nodes (12),
            Nodes (13));
     end;

      when 681 =>

     declare
        Type_Definition : constant Node :=
          Self.Factory.Derived_Record_Definition
            (Nodes (5),
             Nodes (6),
             Nodes (7),
             Nodes (8),
             Nodes (9),
             Nodes (10),
             Nodes (11));
     begin
        Nodes (1) :=
          Self.Factory.Full_Type_Declaration
            (Nodes (1),
            Nodes (2),
            Nodes (3),
            Nodes (4),
            Type_Definition,
            (Self.Factory.Aspect_Specification_Sequence),
            Nodes (12));
     end;

      when 682 =>

     declare
        Type_Definition : constant Node :=
          Self.Factory.Derived_Record_Definition
            (Nodes (5),
             Nodes (6),
             Nodes (7),
             Nodes (8),
             (Self.Factory.Subtype_Mark_Sequence),
             Nodes (9),
             Nodes (10));
     begin
        Nodes (1) :=
          Self.Factory.Full_Type_Declaration
            (Nodes (1),
            Nodes (2),
            Nodes (3),
            Nodes (4),
            Type_Definition,
            Nodes (11),
            Nodes (12));
     end;

      when 683 =>

     declare
        Type_Definition : constant Node :=
          Self.Factory.Derived_Record_Definition
            (Nodes (5),
             Nodes (6),
             Nodes (7),
             Nodes (8),
             (Self.Factory.Subtype_Mark_Sequence),
             Nodes (9),
             Nodes (10));
     begin
        Nodes (1) :=
          Self.Factory.Full_Type_Declaration
            (Nodes (1),
            Nodes (2),
            Nodes (3),
            Nodes (4),
            Type_Definition,
            (Self.Factory.Aspect_Specification_Sequence),
            Nodes (11));
     end;

      when 684 =>

     declare
        Type_Definition : constant Node :=
          Self.Factory.Derived_Type_Definition
            (Nodes (5),
             Nodes (6),
             Nodes (7),
             Nodes (8));
     begin
       Nodes (1) :=
       Self.Factory.Full_Type_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Type_Definition,
          Nodes (9),
          Nodes (10));
     end;

      when 685 =>

     declare
        Type_Definition : constant Node :=
          Self.Factory.Derived_Type_Definition
            (Nodes (5),
             Nodes (6),
             Nodes (7),
             Nodes (8));
     begin
       Nodes (1) :=
       Self.Factory.Full_Type_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Type_Definition,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (9));
     end;

      when 686 =>

     declare
        Type_Definition : constant Node :=
          Self.Factory.Derived_Record_Definition
            (Nodes (5),
             No_Token,
             Nodes (6),
             Nodes (7),
             Nodes (8),
             Nodes (9),
             Nodes (10));
     begin
        Nodes (1) :=
          Self.Factory.Full_Type_Declaration
            (Nodes (1),
            Nodes (2),
            Nodes (3),
            Nodes (4),
            Type_Definition,
            Nodes (11),
            Nodes (12));
     end;

      when 687 =>

     declare
        Type_Definition : constant Node :=
          Self.Factory.Derived_Record_Definition
            (Nodes (5),
             No_Token,
             Nodes (6),
             Nodes (7),
             Nodes (8),
             Nodes (9),
             Nodes (10));
     begin
        Nodes (1) :=
          Self.Factory.Full_Type_Declaration
            (Nodes (1),
            Nodes (2),
            Nodes (3),
            Nodes (4),
            Type_Definition,
            (Self.Factory.Aspect_Specification_Sequence),
            Nodes (11));
     end;

      when 688 =>

     declare
        Type_Definition : constant Node :=
          Self.Factory.Derived_Record_Definition
            (Nodes (5),
             No_Token,
             Nodes (6),
             Nodes (7),
             (Self.Factory.Subtype_Mark_Sequence),
             Nodes (8),
             Nodes (9));
     begin
        Nodes (1) :=
          Self.Factory.Full_Type_Declaration
            (Nodes (1),
            Nodes (2),
            Nodes (3),
            Nodes (4),
            Type_Definition,
            Nodes (10),
            Nodes (11));
     end;

      when 689 =>

     declare
        Type_Definition : constant Node :=
          Self.Factory.Derived_Record_Definition
            (Nodes (5),
             No_Token,
             Nodes (6),
             Nodes (7),
             (Self.Factory.Subtype_Mark_Sequence),
             Nodes (8),
             Nodes (9));
     begin
        Nodes (1) :=
          Self.Factory.Full_Type_Declaration
            (Nodes (1),
            Nodes (2),
            Nodes (3),
            Nodes (4),
            Type_Definition,
            (Self.Factory.Aspect_Specification_Sequence),
            Nodes (10));
     end;

      when 690 =>

     declare
        Type_Definition : constant Node :=
          Self.Factory.Derived_Type_Definition
            (Nodes (5),
             No_Token,
             Nodes (6),
             Nodes (7));
     begin
       Nodes (1) :=
       Self.Factory.Full_Type_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Type_Definition,
          Nodes (8),
          Nodes (9));
     end;

      when 691 =>

     declare
        Type_Definition : constant Node :=
          Self.Factory.Derived_Type_Definition
            (Nodes (5),
             No_Token,
             Nodes (6),
             Nodes (7));
     begin
       Nodes (1) :=
       Self.Factory.Full_Type_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Type_Definition,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (8));
     end;

      when 692 =>

     declare
        Type_Definition : constant Node :=
          Self.Factory.Derived_Record_Definition
            (No_Token,
             Nodes (5),
             Nodes (6),
             Nodes (7),
             Nodes (8),
             Nodes (9),
             Nodes (10));
     begin
        Nodes (1) :=
          Self.Factory.Full_Type_Declaration
            (Nodes (1),
            Nodes (2),
            Nodes (3),
            Nodes (4),
            Type_Definition,
            Nodes (11),
            Nodes (12));
     end;

      when 693 =>

     declare
        Type_Definition : constant Node :=
          Self.Factory.Derived_Record_Definition
            (No_Token,
             Nodes (5),
             Nodes (6),
             Nodes (7),
             Nodes (8),
             Nodes (9),
             Nodes (10));
     begin
        Nodes (1) :=
          Self.Factory.Full_Type_Declaration
            (Nodes (1),
            Nodes (2),
            Nodes (3),
            Nodes (4),
            Type_Definition,
            (Self.Factory.Aspect_Specification_Sequence),
            Nodes (11));
     end;

      when 694 =>

     declare
        Type_Definition : constant Node :=
          Self.Factory.Derived_Record_Definition
            (No_Token,
             Nodes (5),
             Nodes (6),
             Nodes (7),
             (Self.Factory.Subtype_Mark_Sequence),
             Nodes (8),
             Nodes (9));
     begin
        Nodes (1) :=
          Self.Factory.Full_Type_Declaration
            (Nodes (1),
            Nodes (2),
            Nodes (3),
            Nodes (4),
            Type_Definition,
            Nodes (10),
            Nodes (11));
     end;

      when 695 =>

     declare
        Type_Definition : constant Node :=
          Self.Factory.Derived_Record_Definition
            (No_Token,
             Nodes (5),
             Nodes (6),
             Nodes (7),
             (Self.Factory.Subtype_Mark_Sequence),
             Nodes (8),
             Nodes (9));
     begin
        Nodes (1) :=
          Self.Factory.Full_Type_Declaration
            (Nodes (1),
            Nodes (2),
            Nodes (3),
            Nodes (4),
            Type_Definition,
            (Self.Factory.Aspect_Specification_Sequence),
            Nodes (10));
     end;

      when 696 =>

     declare
        Type_Definition : constant Node :=
          Self.Factory.Derived_Type_Definition
            (No_Token,
             Nodes (5),
             Nodes (6),
             Nodes (7));
     begin
       Nodes (1) :=
       Self.Factory.Full_Type_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Type_Definition,
          Nodes (8),
          Nodes (9));
     end;

      when 697 =>

     declare
        Type_Definition : constant Node :=
          Self.Factory.Derived_Type_Definition
            (No_Token,
             Nodes (5),
             Nodes (6),
             Nodes (7));
     begin
       Nodes (1) :=
       Self.Factory.Full_Type_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Type_Definition,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (8));
     end;

      when 698 =>

     declare
        Type_Definition : constant Node :=
          Self.Factory.Derived_Record_Definition
            (No_Token,
             No_Token,
             Nodes (5),
             Nodes (6),
             Nodes (7),
             Nodes (8),
             Nodes (9));
     begin
        Nodes (1) :=
          Self.Factory.Full_Type_Declaration
            (Nodes (1),
            Nodes (2),
            Nodes (3),
            Nodes (4),
            Type_Definition,
            Nodes (10),
            Nodes (11));
     end;

      when 699 =>

     declare
        Type_Definition : constant Node :=
          Self.Factory.Derived_Record_Definition
            (No_Token,
             No_Token,
             Nodes (5),
             Nodes (6),
             Nodes (7),
             Nodes (8),
             Nodes (9));
     begin
        Nodes (1) :=
          Self.Factory.Full_Type_Declaration
            (Nodes (1),
            Nodes (2),
            Nodes (3),
            Nodes (4),
            Type_Definition,
            (Self.Factory.Aspect_Specification_Sequence),
            Nodes (10));
     end;

      when 700 =>

     declare
        Type_Definition : constant Node :=
          Self.Factory.Derived_Record_Definition
            (No_Token,
             No_Token,
             Nodes (5),
             Nodes (6),
             (Self.Factory.Subtype_Mark_Sequence),
             Nodes (7),
             Nodes (8));
     begin
        Nodes (1) :=
          Self.Factory.Full_Type_Declaration
            (Nodes (1),
            Nodes (2),
            Nodes (3),
            Nodes (4),
            Type_Definition,
            Nodes (9),
            Nodes (10));
     end;

      when 701 =>

     declare
        Type_Definition : constant Node :=
          Self.Factory.Derived_Record_Definition
            (No_Token,
             No_Token,
             Nodes (5),
             Nodes (6),
             (Self.Factory.Subtype_Mark_Sequence),
             Nodes (7),
             Nodes (8));
     begin
        Nodes (1) :=
          Self.Factory.Full_Type_Declaration
            (Nodes (1),
            Nodes (2),
            Nodes (3),
            Nodes (4),
            Type_Definition,
            (Self.Factory.Aspect_Specification_Sequence),
            Nodes (9));
     end;

      when 702 =>

     declare
        Type_Definition : constant Node :=
          Self.Factory.Derived_Type_Definition
            (No_Token,
             No_Token,
             Nodes (5),
             Nodes (6));
     begin
       Nodes (1) :=
       Self.Factory.Full_Type_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Type_Definition,
          Nodes (7),
          Nodes (8));
     end;

      when 703 =>

     declare
        Type_Definition : constant Node :=
          Self.Factory.Derived_Type_Definition
            (No_Token,
             No_Token,
             Nodes (5),
             Nodes (6));
     begin
       Nodes (1) :=
       Self.Factory.Full_Type_Declaration
         (Nodes (1),
          Nodes (2),
          Nodes (3),
          Nodes (4),
          Type_Definition,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (7));
     end;

      when 704 =>

     declare
        Type_Definition : constant Node :=
          Self.Factory.Derived_Record_Definition
            (Nodes (4),
             Nodes (5),
             Nodes (6),
             Nodes (7),
             Nodes (8),
             Nodes (9),
             Nodes (10));
     begin
        Nodes (1) :=
          Self.Factory.Full_Type_Declaration
            (Nodes (1),
            Nodes (2),
            None,
            Nodes (3),
            Type_Definition,
            Nodes (11),
            Nodes (12));
     end;

      when 705 =>

     declare
        Type_Definition : constant Node :=
          Self.Factory.Derived_Record_Definition
            (Nodes (4),
             Nodes (5),
             Nodes (6),
             Nodes (7),
             Nodes (8),
             Nodes (9),
             Nodes (10));
     begin
        Nodes (1) :=
          Self.Factory.Full_Type_Declaration
            (Nodes (1),
            Nodes (2),
            None,
            Nodes (3),
            Type_Definition,
            (Self.Factory.Aspect_Specification_Sequence),
            Nodes (11));
     end;

      when 706 =>

     declare
        Type_Definition : constant Node :=
          Self.Factory.Derived_Record_Definition
            (Nodes (4),
             Nodes (5),
             Nodes (6),
             Nodes (7),
             (Self.Factory.Subtype_Mark_Sequence),
             Nodes (8),
             Nodes (9));
     begin
        Nodes (1) :=
          Self.Factory.Full_Type_Declaration
            (Nodes (1),
            Nodes (2),
            None,
            Nodes (3),
            Type_Definition,
            Nodes (10),
            Nodes (11));
     end;

      when 707 =>

     declare
        Type_Definition : constant Node :=
          Self.Factory.Derived_Record_Definition
            (Nodes (4),
             Nodes (5),
             Nodes (6),
             Nodes (7),
             (Self.Factory.Subtype_Mark_Sequence),
             Nodes (8),
             Nodes (9));
     begin
        Nodes (1) :=
          Self.Factory.Full_Type_Declaration
            (Nodes (1),
            Nodes (2),
            None,
            Nodes (3),
            Type_Definition,
            (Self.Factory.Aspect_Specification_Sequence),
            Nodes (10));
     end;

      when 708 =>

     declare
        Type_Definition : constant Node :=
          Self.Factory.Derived_Type_Definition
            (Nodes (4),
             Nodes (5),
             Nodes (6),
             Nodes (7));
     begin
       Nodes (1) :=
       Self.Factory.Full_Type_Declaration
         (Nodes (1),
          Nodes (2),
          None,
          Nodes (3),
          Type_Definition,
          Nodes (8),
          Nodes (9));
     end;

      when 709 =>

     declare
        Type_Definition : constant Node :=
          Self.Factory.Derived_Type_Definition
            (Nodes (4),
             Nodes (5),
             Nodes (6),
             Nodes (7));
     begin
       Nodes (1) :=
       Self.Factory.Full_Type_Declaration
         (Nodes (1),
          Nodes (2),
          None,
          Nodes (3),
          Type_Definition,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (8));
     end;

      when 710 =>

     declare
        Type_Definition : constant Node :=
          Self.Factory.Derived_Record_Definition
            (Nodes (4),
             No_Token,
             Nodes (5),
             Nodes (6),
             Nodes (7),
             Nodes (8),
             Nodes (9));
     begin
        Nodes (1) :=
          Self.Factory.Full_Type_Declaration
            (Nodes (1),
            Nodes (2),
            None,
            Nodes (3),
            Type_Definition,
            Nodes (10),
            Nodes (11));
     end;

      when 711 =>

     declare
        Type_Definition : constant Node :=
          Self.Factory.Derived_Record_Definition
            (Nodes (4),
             No_Token,
             Nodes (5),
             Nodes (6),
             Nodes (7),
             Nodes (8),
             Nodes (9));
     begin
        Nodes (1) :=
          Self.Factory.Full_Type_Declaration
            (Nodes (1),
            Nodes (2),
            None,
            Nodes (3),
            Type_Definition,
            (Self.Factory.Aspect_Specification_Sequence),
            Nodes (10));
     end;

      when 712 =>

     declare
        Type_Definition : constant Node :=
          Self.Factory.Derived_Record_Definition
            (Nodes (4),
             No_Token,
             Nodes (5),
             Nodes (6),
             (Self.Factory.Subtype_Mark_Sequence),
             Nodes (7),
             Nodes (8));
     begin
        Nodes (1) :=
          Self.Factory.Full_Type_Declaration
            (Nodes (1),
            Nodes (2),
            None,
            Nodes (3),
            Type_Definition,
            Nodes (9),
            Nodes (10));
     end;

      when 713 =>

     declare
        Type_Definition : constant Node :=
          Self.Factory.Derived_Record_Definition
            (Nodes (4),
             No_Token,
             Nodes (5),
             Nodes (6),
             (Self.Factory.Subtype_Mark_Sequence),
             Nodes (7),
             Nodes (8));
     begin
        Nodes (1) :=
          Self.Factory.Full_Type_Declaration
            (Nodes (1),
            Nodes (2),
            None,
            Nodes (3),
            Type_Definition,
            (Self.Factory.Aspect_Specification_Sequence),
            Nodes (9));
     end;

      when 714 =>

     declare
        Type_Definition : constant Node :=
          Self.Factory.Derived_Type_Definition
            (Nodes (4),
             No_Token,
             Nodes (5),
             Nodes (6));
     begin
       Nodes (1) :=
       Self.Factory.Full_Type_Declaration
         (Nodes (1),
          Nodes (2),
          None,
          Nodes (3),
          Type_Definition,
          Nodes (7),
          Nodes (8));
     end;

      when 715 =>

     declare
        Type_Definition : constant Node :=
          Self.Factory.Derived_Type_Definition
            (Nodes (4),
             No_Token,
             Nodes (5),
             Nodes (6));
     begin
       Nodes (1) :=
       Self.Factory.Full_Type_Declaration
         (Nodes (1),
          Nodes (2),
          None,
          Nodes (3),
          Type_Definition,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (7));
     end;

      when 716 =>

     declare
        Type_Definition : constant Node :=
          Self.Factory.Derived_Record_Definition
            (No_Token,
             Nodes (4),
             Nodes (5),
             Nodes (6),
             Nodes (7),
             Nodes (8),
             Nodes (9));
     begin
        Nodes (1) :=
          Self.Factory.Full_Type_Declaration
            (Nodes (1),
            Nodes (2),
            None,
            Nodes (3),
            Type_Definition,
            Nodes (10),
            Nodes (11));
     end;

      when 717 =>

     declare
        Type_Definition : constant Node :=
          Self.Factory.Derived_Record_Definition
            (No_Token,
             Nodes (4),
             Nodes (5),
             Nodes (6),
             Nodes (7),
             Nodes (8),
             Nodes (9));
     begin
        Nodes (1) :=
          Self.Factory.Full_Type_Declaration
            (Nodes (1),
            Nodes (2),
            None,
            Nodes (3),
            Type_Definition,
            (Self.Factory.Aspect_Specification_Sequence),
            Nodes (10));
     end;

      when 718 =>

     declare
        Type_Definition : constant Node :=
          Self.Factory.Derived_Record_Definition
            (No_Token,
             Nodes (4),
             Nodes (5),
             Nodes (6),
             (Self.Factory.Subtype_Mark_Sequence),
             Nodes (7),
             Nodes (8));
     begin
        Nodes (1) :=
          Self.Factory.Full_Type_Declaration
            (Nodes (1),
            Nodes (2),
            None,
            Nodes (3),
            Type_Definition,
            Nodes (9),
            Nodes (10));
     end;

      when 719 =>

     declare
        Type_Definition : constant Node :=
          Self.Factory.Derived_Record_Definition
            (No_Token,
             Nodes (4),
             Nodes (5),
             Nodes (6),
             (Self.Factory.Subtype_Mark_Sequence),
             Nodes (7),
             Nodes (8));
     begin
        Nodes (1) :=
          Self.Factory.Full_Type_Declaration
            (Nodes (1),
            Nodes (2),
            None,
            Nodes (3),
            Type_Definition,
            (Self.Factory.Aspect_Specification_Sequence),
            Nodes (9));
     end;

      when 720 =>

     declare
        Type_Definition : constant Node :=
          Self.Factory.Derived_Type_Definition
            (No_Token,
             Nodes (4),
             Nodes (5),
             Nodes (6));
     begin
       Nodes (1) :=
       Self.Factory.Full_Type_Declaration
         (Nodes (1),
          Nodes (2),
          None,
          Nodes (3),
          Type_Definition,
          Nodes (7),
          Nodes (8));
     end;

      when 721 =>

     declare
        Type_Definition : constant Node :=
          Self.Factory.Derived_Type_Definition
            (No_Token,
             Nodes (4),
             Nodes (5),
             Nodes (6));
     begin
       Nodes (1) :=
       Self.Factory.Full_Type_Declaration
         (Nodes (1),
          Nodes (2),
          None,
          Nodes (3),
          Type_Definition,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (7));
     end;

      when 722 =>

     declare
        Type_Definition : constant Node :=
          Self.Factory.Derived_Record_Definition
            (No_Token,
             No_Token,
             Nodes (4),
             Nodes (5),
             Nodes (6),
             Nodes (7),
             Nodes (8));
     begin
        Nodes (1) :=
          Self.Factory.Full_Type_Declaration
            (Nodes (1),
            Nodes (2),
            None,
            Nodes (3),
            Type_Definition,
            Nodes (9),
            Nodes (10));
     end;

      when 723 =>

     declare
        Type_Definition : constant Node :=
          Self.Factory.Derived_Record_Definition
            (No_Token,
             No_Token,
             Nodes (4),
             Nodes (5),
             Nodes (6),
             Nodes (7),
             Nodes (8));
     begin
        Nodes (1) :=
          Self.Factory.Full_Type_Declaration
            (Nodes (1),
            Nodes (2),
            None,
            Nodes (3),
            Type_Definition,
            (Self.Factory.Aspect_Specification_Sequence),
            Nodes (9));
     end;

      when 724 =>

     declare
        Type_Definition : constant Node :=
          Self.Factory.Derived_Record_Definition
            (No_Token,
             No_Token,
             Nodes (4),
             Nodes (5),
             (Self.Factory.Subtype_Mark_Sequence),
             Nodes (6),
             Nodes (7));
     begin
        Nodes (1) :=
          Self.Factory.Full_Type_Declaration
            (Nodes (1),
            Nodes (2),
            None,
            Nodes (3),
            Type_Definition,
            Nodes (8),
            Nodes (9));
     end;

      when 725 =>

     declare
        Type_Definition : constant Node :=
          Self.Factory.Derived_Record_Definition
            (No_Token,
             No_Token,
             Nodes (4),
             Nodes (5),
             (Self.Factory.Subtype_Mark_Sequence),
             Nodes (6),
             Nodes (7));
     begin
        Nodes (1) :=
          Self.Factory.Full_Type_Declaration
            (Nodes (1),
            Nodes (2),
            None,
            Nodes (3),
            Type_Definition,
            (Self.Factory.Aspect_Specification_Sequence),
            Nodes (8));
     end;

      when 726 =>

     declare
        Type_Definition : constant Node :=
          Self.Factory.Derived_Type_Definition
            (No_Token,
             No_Token,
             Nodes (4),
             Nodes (5));
     begin
       Nodes (1) :=
       Self.Factory.Full_Type_Declaration
         (Nodes (1),
          Nodes (2),
          None,
          Nodes (3),
          Type_Definition,
          Nodes (6),
          Nodes (7));
     end;

      when 727 =>

     declare
        Type_Definition : constant Node :=
          Self.Factory.Derived_Type_Definition
            (No_Token,
             No_Token,
             Nodes (4),
             Nodes (5));
     begin
       Nodes (1) :=
       Self.Factory.Full_Type_Declaration
         (Nodes (1),
          Nodes (2),
          None,
          Nodes (3),
          Type_Definition,
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (6));
     end;

      when 728 =>

   Nodes (1) := Self.Factory.Function_Body
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
          Nodes (19),
          Nodes (20),
          Nodes (21));

      when 729 =>

   Nodes (1) := Self.Factory.Function_Body
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
          Nodes (19),
          None,
          Nodes (20));

      when 730 =>

   Nodes (1) := Self.Factory.Function_Body
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
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (17),
          Nodes (18),
          Nodes (19));

      when 731 =>

   Nodes (1) := Self.Factory.Function_Body
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
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (17),
          None,
          Nodes (18));

      when 732 =>

   Nodes (1) := Self.Factory.Function_Body
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
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (15),
          Nodes (16),
          Nodes (17));

      when 733 =>

   Nodes (1) := Self.Factory.Function_Body
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
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (15),
          None,
          Nodes (16));

      when 734 =>

   Nodes (1) := Self.Factory.Function_Body
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
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (14),
          Nodes (15),
          Nodes (16),
          Nodes (17),
          Nodes (18),
          Nodes (19),
          Nodes (20));

      when 735 =>

   Nodes (1) := Self.Factory.Function_Body
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
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (14),
          Nodes (15),
          Nodes (16),
          Nodes (17),
          Nodes (18),
          None,
          Nodes (19));

      when 736 =>

   Nodes (1) := Self.Factory.Function_Body
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
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (14),
          Nodes (15),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (16),
          Nodes (17),
          Nodes (18));

      when 737 =>

   Nodes (1) := Self.Factory.Function_Body
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
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (14),
          Nodes (15),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (16),
          None,
          Nodes (17));

      when 738 =>

   Nodes (1) := Self.Factory.Function_Body
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
          (Self.Factory.Declarative_Item_Sequence),
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (14),
          Nodes (15),
          Nodes (16));

      when 739 =>

   Nodes (1) := Self.Factory.Function_Body
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
          (Self.Factory.Declarative_Item_Sequence),
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (14),
          None,
          Nodes (15));

      when 740 =>

   Nodes (1) := Self.Factory.Function_Body
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
          Nodes (12),
          Nodes (13),
          Nodes (14),
          Nodes (15),
          Nodes (16),
          Nodes (17),
          Nodes (18),
          Nodes (19),
          Nodes (20));

      when 741 =>

   Nodes (1) := Self.Factory.Function_Body
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
          Nodes (12),
          Nodes (13),
          Nodes (14),
          Nodes (15),
          Nodes (16),
          Nodes (17),
          Nodes (18),
          None,
          Nodes (19));

      when 742 =>

   Nodes (1) := Self.Factory.Function_Body
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
          Nodes (12),
          Nodes (13),
          Nodes (14),
          Nodes (15),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (16),
          Nodes (17),
          Nodes (18));

      when 743 =>

   Nodes (1) := Self.Factory.Function_Body
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
          Nodes (12),
          Nodes (13),
          Nodes (14),
          Nodes (15),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (16),
          None,
          Nodes (17));

      when 744 =>

   Nodes (1) := Self.Factory.Function_Body
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
          Nodes (12),
          Nodes (13),
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (14),
          Nodes (15),
          Nodes (16));

      when 745 =>

   Nodes (1) := Self.Factory.Function_Body
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
          Nodes (12),
          Nodes (13),
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (14),
          None,
          Nodes (15));

      when 746 =>

   Nodes (1) := Self.Factory.Function_Body
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
          Nodes (12),
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (13),
          Nodes (14),
          Nodes (15),
          Nodes (16),
          Nodes (17),
          Nodes (18),
          Nodes (19));

      when 747 =>

   Nodes (1) := Self.Factory.Function_Body
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
          Nodes (12),
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (13),
          Nodes (14),
          Nodes (15),
          Nodes (16),
          Nodes (17),
          None,
          Nodes (18));

      when 748 =>

   Nodes (1) := Self.Factory.Function_Body
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
          Nodes (12),
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (13),
          Nodes (14),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (15),
          Nodes (16),
          Nodes (17));

      when 749 =>

   Nodes (1) := Self.Factory.Function_Body
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
          Nodes (12),
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (13),
          Nodes (14),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (15),
          None,
          Nodes (16));

      when 750 =>

   Nodes (1) := Self.Factory.Function_Body
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
          Nodes (12),
          (Self.Factory.Declarative_Item_Sequence),
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (13),
          Nodes (14),
          Nodes (15));

      when 751 =>

   Nodes (1) := Self.Factory.Function_Body
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
          Nodes (12),
          (Self.Factory.Declarative_Item_Sequence),
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (13),
          None,
          Nodes (14));

      when 752 =>

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

      when 753 =>

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

      when 754 =>

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

      when 755 =>

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

      when 756 =>

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

      when 757 =>

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

      when 758 =>

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

      when 759 =>

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

      when 760 =>

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

      when 761 =>

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

      when 762 =>

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

      when 763 =>

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

      when 764 =>

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

      when 765 =>

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

      when 766 =>

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

      when 767 =>

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

      when 768 =>

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

      when 769 =>

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

      when 770 =>

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

      when 771 =>

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

      when 772 =>

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

      when 773 =>

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

      when 774 =>

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

      when 775 =>

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

      when 776 =>

   Nodes (1) := Self.Factory.Function_Body
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
          Nodes (14),
          Nodes (15),
          Nodes (16),
          Nodes (17),
          Nodes (18));

      when 777 =>

   Nodes (1) := Self.Factory.Function_Body
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
          Nodes (14),
          Nodes (15),
          Nodes (16),
          None,
          Nodes (17));

      when 778 =>

   Nodes (1) := Self.Factory.Function_Body
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
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (14),
          Nodes (15),
          Nodes (16));

      when 779 =>

   Nodes (1) := Self.Factory.Function_Body
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
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (14),
          None,
          Nodes (15));

      when 780 =>

   Nodes (1) := Self.Factory.Function_Body
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
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (12),
          Nodes (13),
          Nodes (14));

      when 781 =>

   Nodes (1) := Self.Factory.Function_Body
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
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (12),
          None,
          Nodes (13));

      when 782 =>

   Nodes (1) := Self.Factory.Function_Body
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
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (11),
          Nodes (12),
          Nodes (13),
          Nodes (14),
          Nodes (15),
          Nodes (16),
          Nodes (17));

      when 783 =>

   Nodes (1) := Self.Factory.Function_Body
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
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (11),
          Nodes (12),
          Nodes (13),
          Nodes (14),
          Nodes (15),
          None,
          Nodes (16));

      when 784 =>

   Nodes (1) := Self.Factory.Function_Body
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
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (11),
          Nodes (12),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (13),
          Nodes (14),
          Nodes (15));

      when 785 =>

   Nodes (1) := Self.Factory.Function_Body
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
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (11),
          Nodes (12),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (13),
          None,
          Nodes (14));

      when 786 =>

   Nodes (1) := Self.Factory.Function_Body
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
          (Self.Factory.Declarative_Item_Sequence),
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (11),
          Nodes (12),
          Nodes (13));

      when 787 =>

   Nodes (1) := Self.Factory.Function_Body
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
          (Self.Factory.Declarative_Item_Sequence),
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (11),
          None,
          Nodes (12));

      when 788 =>

   Nodes (1) := Self.Factory.Function_Body
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
          Nodes (9),
          Nodes (10),
          Nodes (11),
          Nodes (12),
          Nodes (13),
          Nodes (14),
          Nodes (15),
          Nodes (16),
          Nodes (17));

      when 789 =>

   Nodes (1) := Self.Factory.Function_Body
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
          Nodes (9),
          Nodes (10),
          Nodes (11),
          Nodes (12),
          Nodes (13),
          Nodes (14),
          Nodes (15),
          None,
          Nodes (16));

      when 790 =>

   Nodes (1) := Self.Factory.Function_Body
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
          Nodes (9),
          Nodes (10),
          Nodes (11),
          Nodes (12),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (13),
          Nodes (14),
          Nodes (15));

      when 791 =>

   Nodes (1) := Self.Factory.Function_Body
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
          Nodes (9),
          Nodes (10),
          Nodes (11),
          Nodes (12),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (13),
          None,
          Nodes (14));

      when 792 =>

   Nodes (1) := Self.Factory.Function_Body
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
          Nodes (9),
          Nodes (10),
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (11),
          Nodes (12),
          Nodes (13));

      when 793 =>

   Nodes (1) := Self.Factory.Function_Body
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
          Nodes (9),
          Nodes (10),
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (11),
          None,
          Nodes (12));

      when 794 =>

   Nodes (1) := Self.Factory.Function_Body
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
          Nodes (9),
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (10),
          Nodes (11),
          Nodes (12),
          Nodes (13),
          Nodes (14),
          Nodes (15),
          Nodes (16));

      when 795 =>

   Nodes (1) := Self.Factory.Function_Body
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
          Nodes (9),
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (10),
          Nodes (11),
          Nodes (12),
          Nodes (13),
          Nodes (14),
          None,
          Nodes (15));

      when 796 =>

   Nodes (1) := Self.Factory.Function_Body
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
          Nodes (9),
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (10),
          Nodes (11),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (12),
          Nodes (13),
          Nodes (14));

      when 797 =>

   Nodes (1) := Self.Factory.Function_Body
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
          Nodes (9),
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (10),
          Nodes (11),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (12),
          None,
          Nodes (13));

      when 798 =>

   Nodes (1) := Self.Factory.Function_Body
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
          Nodes (9),
          (Self.Factory.Declarative_Item_Sequence),
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (10),
          Nodes (11),
          Nodes (12));

      when 799 =>

   Nodes (1) := Self.Factory.Function_Body
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
          Nodes (9),
          (Self.Factory.Declarative_Item_Sequence),
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (10),
          None,
          Nodes (11));

      when 800 =>

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

      when 801 =>

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

      when 802 =>

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

      when 803 =>

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

      when 804 =>

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

      when 805 =>

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

      when 806 =>

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

      when 807 =>

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

      when 808 =>

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

      when 809 =>

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

      when 810 =>

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

      when 811 =>

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

      when 812 =>

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

      when 813 =>

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

      when 814 =>

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

      when 815 =>

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

      when 816 =>

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

      when 817 =>

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

      when 818 =>

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

      when 819 =>

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

      when 820 =>

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

      when 821 =>

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

      when 822 =>

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

      when 823 =>

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

      when 824 =>

   Nodes (1) := Self.Factory.Function_Body
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
          Nodes (16),
          Nodes (17),
          Nodes (18),
          Nodes (19),
          Nodes (20));

      when 825 =>

   Nodes (1) := Self.Factory.Function_Body
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
          Nodes (16),
          Nodes (17),
          Nodes (18),
          None,
          Nodes (19));

      when 826 =>

   Nodes (1) := Self.Factory.Function_Body
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
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (16),
          Nodes (17),
          Nodes (18));

      when 827 =>

   Nodes (1) := Self.Factory.Function_Body
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
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (16),
          None,
          Nodes (17));

      when 828 =>

   Nodes (1) := Self.Factory.Function_Body
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
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (14),
          Nodes (15),
          Nodes (16));

      when 829 =>

   Nodes (1) := Self.Factory.Function_Body
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
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (14),
          None,
          Nodes (15));

      when 830 =>

   Nodes (1) := Self.Factory.Function_Body
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
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (13),
          Nodes (14),
          Nodes (15),
          Nodes (16),
          Nodes (17),
          Nodes (18),
          Nodes (19));

      when 831 =>

   Nodes (1) := Self.Factory.Function_Body
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
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (13),
          Nodes (14),
          Nodes (15),
          Nodes (16),
          Nodes (17),
          None,
          Nodes (18));

      when 832 =>

   Nodes (1) := Self.Factory.Function_Body
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
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (13),
          Nodes (14),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (15),
          Nodes (16),
          Nodes (17));

      when 833 =>

   Nodes (1) := Self.Factory.Function_Body
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
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (13),
          Nodes (14),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (15),
          None,
          Nodes (16));

      when 834 =>

   Nodes (1) := Self.Factory.Function_Body
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
          (Self.Factory.Declarative_Item_Sequence),
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (13),
          Nodes (14),
          Nodes (15));

      when 835 =>

   Nodes (1) := Self.Factory.Function_Body
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
          (Self.Factory.Declarative_Item_Sequence),
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (13),
          None,
          Nodes (14));

      when 836 =>

   Nodes (1) := Self.Factory.Function_Body
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
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (11),
          Nodes (12),
          Nodes (13),
          Nodes (14),
          Nodes (15),
          Nodes (16),
          Nodes (17),
          Nodes (18),
          Nodes (19));

      when 837 =>

   Nodes (1) := Self.Factory.Function_Body
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
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (11),
          Nodes (12),
          Nodes (13),
          Nodes (14),
          Nodes (15),
          Nodes (16),
          Nodes (17),
          None,
          Nodes (18));

      when 838 =>

   Nodes (1) := Self.Factory.Function_Body
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
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (11),
          Nodes (12),
          Nodes (13),
          Nodes (14),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (15),
          Nodes (16),
          Nodes (17));

      when 839 =>

   Nodes (1) := Self.Factory.Function_Body
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
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (11),
          Nodes (12),
          Nodes (13),
          Nodes (14),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (15),
          None,
          Nodes (16));

      when 840 =>

   Nodes (1) := Self.Factory.Function_Body
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
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (11),
          Nodes (12),
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (13),
          Nodes (14),
          Nodes (15));

      when 841 =>

   Nodes (1) := Self.Factory.Function_Body
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
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (11),
          Nodes (12),
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (13),
          None,
          Nodes (14));

      when 842 =>

   Nodes (1) := Self.Factory.Function_Body
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
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (11),
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (12),
          Nodes (13),
          Nodes (14),
          Nodes (15),
          Nodes (16),
          Nodes (17),
          Nodes (18));

      when 843 =>

   Nodes (1) := Self.Factory.Function_Body
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
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (11),
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (12),
          Nodes (13),
          Nodes (14),
          Nodes (15),
          Nodes (16),
          None,
          Nodes (17));

      when 844 =>

   Nodes (1) := Self.Factory.Function_Body
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
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (11),
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (12),
          Nodes (13),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (14),
          Nodes (15),
          Nodes (16));

      when 845 =>

   Nodes (1) := Self.Factory.Function_Body
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
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (11),
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (12),
          Nodes (13),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (14),
          None,
          Nodes (15));

      when 846 =>

   Nodes (1) := Self.Factory.Function_Body
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
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (11),
          (Self.Factory.Declarative_Item_Sequence),
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (12),
          Nodes (13),
          Nodes (14));

      when 847 =>

   Nodes (1) := Self.Factory.Function_Body
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
          (Self.Factory.Aspect_Specification_Sequence),
          Nodes (11),
          (Self.Factory.Declarative_Item_Sequence),
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (12),
          None,
          Nodes (13));

      when 848 =>

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

      when 849 =>

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

      when 850 =>

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

      when 851 =>

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

      when 852 =>

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

      when 853 =>

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

      when 854 =>

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

      when 855 =>

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

      when 856 =>

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

      when 857 =>

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

      when 858 =>

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

      when 859 =>

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

      when 860 =>

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

      when 861 =>

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

      when 862 =>

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

      when 863 =>

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

      when 864 =>

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

      when 865 =>

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

      when 866 =>

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

      when 867 =>

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

      when 868 =>

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

      when 869 =>

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

      when 870 =>

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

      when 871 =>

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

      when 872 =>

   Nodes (1) := Self.Factory.Function_Body
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
          Nodes (13),
          Nodes (14),
          Nodes (15),
          Nodes (16),
          Nodes (17));

      when 873 =>

   Nodes (1) := Self.Factory.Function_Body
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
          Nodes (13),
          Nodes (14),
          Nodes (15),
          None,
          Nodes (16));

      when 874 =>

   Nodes (1) := Self.Factory.Function_Body
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
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (13),
          Nodes (14),
          Nodes (15));

      when 875 =>

   Nodes (1) := Self.Factory.Function_Body
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
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (13),
          None,
          Nodes (14));

      when 876 =>

   Nodes (1) := Self.Factory.Function_Body
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
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (11),
          Nodes (12),
          Nodes (13));

      when 877 =>

   Nodes (1) := Self.Factory.Function_Body
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
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (11),
          None,
          Nodes (12));

      when 878 =>

   Nodes (1) := Self.Factory.Function_Body
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
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (10),
          Nodes (11),
          Nodes (12),
          Nodes (13),
          Nodes (14),
          Nodes (15),
          Nodes (16));

      when 879 =>

   Nodes (1) := Self.Factory.Function_Body
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
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (10),
          Nodes (11),
          Nodes (12),
          Nodes (13),
          Nodes (14),
          None,
          Nodes (15));

      when 880 =>

   Nodes (1) := Self.Factory.Function_Body
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
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (10),
          Nodes (11),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (12),
          Nodes (13),
          Nodes (14));

      when 881 =>

   Nodes (1) := Self.Factory.Function_Body
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
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (10),
          Nodes (11),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (12),
          None,
          Nodes (13));

      when 882 =>

   Nodes (1) := Self.Factory.Function_Body
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
          (Self.Factory.Declarative_Item_Sequence),
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (10),
          Nodes (11),
          Nodes (12));

      when 883 =>

   Nodes (1) := Self.Factory.Function_Body
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
          (Self.Factory.Declarative_Item_Sequence),
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (10),
          None,
          Nodes (11));

      when 884 =>

   Nodes (1) := Self.Factory.Function_Body
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

      when 885 =>

   Nodes (1) := Self.Factory.Function_Body
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

      when 886 =>

   Nodes (1) := Self.Factory.Function_Body
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

      when 887 =>

   Nodes (1) := Self.Factory.Function_Body
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

      when 888 =>

   Nodes (1) := Self.Factory.Function_Body
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

      when 889 =>

   Nodes (1) := Self.Factory.Function_Body
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

      when 890 =>

   Nodes (1) := Self.Factory.Function_Body
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

      when 891 =>

   Nodes (1) := Self.Factory.Function_Body
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

      when 892 =>

   Nodes (1) := Self.Factory.Function_Body
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

      when 893 =>

   Nodes (1) := Self.Factory.Function_Body
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

      when 894 =>

   Nodes (1) := Self.Factory.Function_Body
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

      when 895 =>

   Nodes (1) := Self.Factory.Function_Body
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

      when 896 =>

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

      when 897 =>

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

      when 898 =>

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

      when 899 =>

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

      when 900 =>

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

      when 901 =>

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

      when 902 =>

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

      when 903 =>

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

      when 904 =>

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

      when 905 =>

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

      when 906 =>

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

      when 907 =>

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

      when 908 =>

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

      when 909 =>

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

      when 910 =>

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

      when 911 =>

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

      when 912 =>

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

      when 913 =>

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

      when 914 =>

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

      when 915 =>

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

      when 916 =>

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

      when 917 =>

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

      when 918 =>

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

      when 919 =>

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

      when 920 =>

   Nodes (1) := Self.Factory.Function_Body
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
          Nodes (15),
          Nodes (16),
          Nodes (17),
          Nodes (18),
          Nodes (19));

      when 921 =>

   Nodes (1) := Self.Factory.Function_Body
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
          Nodes (15),
          Nodes (16),
          Nodes (17),
          None,
          Nodes (18));

      when 922 =>

   Nodes (1) := Self.Factory.Function_Body
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
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (15),
          Nodes (16),
          Nodes (17));

      when 923 =>

   Nodes (1) := Self.Factory.Function_Body
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
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (15),
          None,
          Nodes (16));

      when 924 =>

   Nodes (1) := Self.Factory.Function_Body
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
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (13),
          Nodes (14),
          Nodes (15));

      when 925 =>

   Nodes (1) := Self.Factory.Function_Body
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
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (13),
          None,
          Nodes (14));

      when 926 =>

   Nodes (1) := Self.Factory.Function_Body
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
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (12),
          Nodes (13),
          Nodes (14),
          Nodes (15),
          Nodes (16),
          Nodes (17),
          Nodes (18));

      when 927 =>

   Nodes (1) := Self.Factory.Function_Body
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
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (12),
          Nodes (13),
          Nodes (14),
          Nodes (15),
          Nodes (16),
          None,
          Nodes (17));

      when 928 =>

   Nodes (1) := Self.Factory.Function_Body
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
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (12),
          Nodes (13),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (14),
          Nodes (15),
          Nodes (16));

      when 929 =>

   Nodes (1) := Self.Factory.Function_Body
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
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (12),
          Nodes (13),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (14),
          None,
          Nodes (15));

      when 930 =>

   Nodes (1) := Self.Factory.Function_Body
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
          (Self.Factory.Declarative_Item_Sequence),
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (12),
          Nodes (13),
          Nodes (14));

      when 931 =>

   Nodes (1) := Self.Factory.Function_Body
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
          (Self.Factory.Declarative_Item_Sequence),
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (12),
          None,
          Nodes (13));

      when 932 =>

   Nodes (1) := Self.Factory.Function_Body
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

      when 933 =>

   Nodes (1) := Self.Factory.Function_Body
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

      when 934 =>

   Nodes (1) := Self.Factory.Function_Body
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

      when 935 =>

   Nodes (1) := Self.Factory.Function_Body
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

      when 936 =>

   Nodes (1) := Self.Factory.Function_Body
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

      when 937 =>

   Nodes (1) := Self.Factory.Function_Body
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

      when 938 =>

   Nodes (1) := Self.Factory.Function_Body
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

      when 939 =>

   Nodes (1) := Self.Factory.Function_Body
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

      when 940 =>

   Nodes (1) := Self.Factory.Function_Body
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

      when 941 =>

   Nodes (1) := Self.Factory.Function_Body
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

      when 942 =>

   Nodes (1) := Self.Factory.Function_Body
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

      when 943 =>

   Nodes (1) := Self.Factory.Function_Body
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

      when 944 =>

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

      when 945 =>

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

      when 946 =>

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

      when 947 =>

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

      when 948 =>

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

      when 949 =>

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

      when 950 =>

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

      when 951 =>

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

      when 952 =>

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

      when 953 =>

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

      when 954 =>

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

      when 955 =>

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

      when 956 =>

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

      when 957 =>

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

      when 958 =>

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

      when 959 =>

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

      when 960 =>

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

      when 961 =>

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

      when 962 =>

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

      when 963 =>

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

      when 964 =>

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

      when 965 =>

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

      when 966 =>

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

      when 967 =>

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

      when 968 =>

   Nodes (1) := Self.Factory.Function_Body
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
          Nodes (12),
          Nodes (13),
          Nodes (14),
          Nodes (15),
          Nodes (16));

      when 969 =>

   Nodes (1) := Self.Factory.Function_Body
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
          Nodes (12),
          Nodes (13),
          Nodes (14),
          None,
          Nodes (15));

      when 970 =>

   Nodes (1) := Self.Factory.Function_Body
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
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (12),
          Nodes (13),
          Nodes (14));

      when 971 =>

   Nodes (1) := Self.Factory.Function_Body
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
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (12),
          None,
          Nodes (13));

      when 972 =>

   Nodes (1) := Self.Factory.Function_Body
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
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (10),
          Nodes (11),
          Nodes (12));

      when 973 =>

   Nodes (1) := Self.Factory.Function_Body
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
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (10),
          None,
          Nodes (11));

      when 974 =>

   Nodes (1) := Self.Factory.Function_Body
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
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (9),
          Nodes (10),
          Nodes (11),
          Nodes (12),
          Nodes (13),
          Nodes (14),
          Nodes (15));

      when 975 =>

   Nodes (1) := Self.Factory.Function_Body
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
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (9),
          Nodes (10),
          Nodes (11),
          Nodes (12),
          Nodes (13),
          None,
          Nodes (14));

      when 976 =>

   Nodes (1) := Self.Factory.Function_Body
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
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (9),
          Nodes (10),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (11),
          Nodes (12),
          Nodes (13));

      when 977 =>

   Nodes (1) := Self.Factory.Function_Body
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
          (Self.Factory.Declarative_Item_Sequence),
          Nodes (9),
          Nodes (10),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (11),
          None,
          Nodes (12));

      when 978 =>

   Nodes (1) := Self.Factory.Function_Body
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
          (Self.Factory.Declarative_Item_Sequence),
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (9),
          Nodes (10),
          Nodes (11));

      when 979 =>

   Nodes (1) := Self.Factory.Function_Body
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
          (Self.Factory.Declarative_Item_Sequence),
          No_Token,
          (Self.Factory.Statement_Sequence),
          No_Token,
          (Self.Factory.Exception_Handler_Sequence),
          Nodes (9),
          None,
          Nodes (10));

      when 980 =>

   Nodes (1) := Self.Factory.Function_Body
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

      when 981 =>

   Nodes (1) := Self.Factory.Function_Body
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

      when 982 =>

   Nodes (1) := Self.Factory.Function_Body
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

      when 983 =>

   Nodes (1) := Self.Factory.Function_Body
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

      when 984 =>

   Nodes (1) := Self.Factory.Function_Body
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

      when 985 =>

   Nodes (1) := Self.Factory.Function_Body
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

      when 986 =>

   Nodes (1) := Self.Factory.Function_Body
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

      when 987 =>

   Nodes (1) := Self.Factory.Function_Body
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

      when 988 =>

   Nodes (1) := Self.Factory.Function_Body
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

      when 989 =>

   Nodes (1) := Self.Factory.Function_Body
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

      when 990 =>

   Nodes (1) := Self.Factory.Function_Body
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

      when 991 =>

   Nodes (1) := Self.Factory.Function_Body
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

      when 992 =>

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

      when 993 =>

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

      when 994 =>

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

      when 995 =>

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

      when 996 =>

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

      when 997 =>

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

      when 998 =>

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

      when 999 =>

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

      when 1000 =>

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

      when others =>
         raise Constraint_Error;
   end case;
end Program.Parsers.On_Reduce_501;
