------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                          http://gela.ada-ru.org                          --
--                     - - - - - - - - - - - - - - -                        --
--            Read copyright and license at the end of this file            --
------------------------------------------------------------------------------
--  $Revision$ $Date$
--  Purpose:
--  Procedural wrapper over Object-Oriented ASIS implementation

with Gela.Element_Visiters;
with Gela.Elements.Assignment_Statements;
with Gela.Elements.Association_Lists;
with Gela.Elements.Associations;
with Gela.Elements.Auxiliary_Applies;
with Gela.Elements.Expressions;
with Gela.Elements.Names;
with Gela.Elements.Prefixes;
with Gela.Elements.Procedure_Call_Statements;

package body Asis.Statements is

   -------------------
   -- Aborted_Tasks --
   -------------------

   function Aborted_Tasks
     (Statement : in Asis.Statement)
      return Asis.Expression_List
   is
   begin
      Check_Nil_Element (Statement, "Aborted_Tasks");
      Raise_Not_Implemented ("");
      return Asis.Nil_Element_List;
   end Aborted_Tasks;

   ------------------------------------
   -- Accept_Body_Exception_Handlers --
   ------------------------------------

   function Accept_Body_Exception_Handlers
     (Statement       : in Asis.Statement;
      Include_Pragmas : in Boolean := False)
      return Asis.Statement_List
   is
      pragma Unreferenced (Include_Pragmas);
   begin
      Check_Nil_Element (Statement, "Accept_Body_Exception_Handlers");
      Raise_Not_Implemented ("");
      return Asis.Nil_Element_List;
   end Accept_Body_Exception_Handlers;

   ----------------------------
   -- Accept_Body_Statements --
   ----------------------------

   function Accept_Body_Statements
     (Statement       : in Asis.Statement;
      Include_Pragmas : in Boolean := False)
      return Asis.Statement_List
   is
      pragma Unreferenced (Include_Pragmas);
   begin
      Check_Nil_Element (Statement, "Accept_Body_Statements");
      Raise_Not_Implemented ("");
      return Asis.Nil_Element_List;
   end Accept_Body_Statements;

   ------------------------------
   -- Accept_Entry_Direct_Name --
   ------------------------------

   function Accept_Entry_Direct_Name
     (Statement : in Asis.Statement)
      return Asis.Name
   is
   begin
      Check_Nil_Element (Statement, "Accept_Entry_Direct_Name");
      Raise_Not_Implemented ("");
      return Asis.Nil_Element;
   end Accept_Entry_Direct_Name;

   ------------------------
   -- Accept_Entry_Index --
   ------------------------

   function Accept_Entry_Index
     (Statement : in Asis.Statement)
      return Asis.Expression
   is
   begin
      Check_Nil_Element (Statement, "Accept_Entry_Index");
      Raise_Not_Implemented ("");
      return Asis.Nil_Element;
   end Accept_Entry_Index;

   -----------------------
   -- Accept_Parameters --
   -----------------------

   function Accept_Parameters
     (Statement : in Asis.Statement)
      return Asis.Parameter_Specification_List
   is
   begin
      Check_Nil_Element (Statement, "Accept_Parameters");
      Raise_Not_Implemented ("");
      return Asis.Nil_Element_List;
   end Accept_Parameters;

   ---------------------------
   -- Assignment_Expression --
   ---------------------------

   function Assignment_Expression
     (Statement : in Asis.Statement)
      return Asis.Expression
   is
      package Get is
         type Visiter is new Gela.Element_Visiters.Visiter with record
            Result : Gela.Elements.Element_Access;
         end record;

         overriding procedure Assignment_Statement
           (Self : in out Visiter;
            Node : not null Gela.Elements.Assignment_Statements.
              Assignment_Statement_Access);
      end Get;

      package body Get is

         overriding procedure Assignment_Statement
           (Self : in out Visiter;
            Node : not null Gela.Elements.Assignment_Statements.
              Assignment_Statement_Access)
         is
            X : constant Gela.Elements.Expressions.Expression_Access :=
              Node.Assignment_Expression;
         begin
            Self.Result := Gela.Elements.Element_Access (X);
         end Assignment_Statement;
      end Get;

      V : Get.Visiter;
   begin
      Check_Nil_Element (Statement, "Assignment_Expression");
      Statement.Data.Visit (V);
      return (Data => V.Result);
   end Assignment_Expression;

   ------------------------------
   -- Assignment_Variable_Name --
   ------------------------------

   function Assignment_Variable_Name
     (Statement : in Asis.Statement)
      return Asis.Expression
   is
      package Get is
         type Visiter is new Gela.Element_Visiters.Visiter with record
            Result : Gela.Elements.Element_Access;
         end record;

         overriding procedure Assignment_Statement
           (Self : in out Visiter;
            Node : not null Gela.Elements.Assignment_Statements.
              Assignment_Statement_Access);
      end Get;

      package body Get is

         overriding procedure Assignment_Statement
           (Self : in out Visiter;
            Node : not null Gela.Elements.Assignment_Statements.
              Assignment_Statement_Access)
         is
            X : constant Gela.Elements.Names.Name_Access :=
              Node.Assignment_Variable_Name;
         begin
            Self.Result := Gela.Elements.Element_Access (X);
         end Assignment_Statement;
      end Get;

      V : Get.Visiter;
   begin
      Check_Nil_Element (Statement, "Assignment_Variable_Name");
      Statement.Data.Visit (V);
      return (Data => V.Result);
   end Assignment_Variable_Name;

   -----------------------------
   -- Block_Declarative_Items --
   -----------------------------

   function Block_Declarative_Items
     (Statement       : in Asis.Statement;
      Include_Pragmas : in Boolean := False)
      return Asis.Declarative_Item_List
   is
      pragma Unreferenced (Include_Pragmas);
   begin
      Check_Nil_Element (Statement, "Block_Declarative_Items");
      Raise_Not_Implemented ("");
      return Asis.Nil_Element_List;
   end Block_Declarative_Items;

   ------------------------------
   -- Block_Exception_Handlers --
   ------------------------------

   function Block_Exception_Handlers
     (Statement : in Asis.Statement;
      Include_Pragmas : in Boolean := False)
      return Asis.Exception_Handler_List
   is
      pragma Unreferenced (Include_Pragmas);
   begin
      Check_Nil_Element (Statement, "Block_Exception_Handlers");
      Raise_Not_Implemented ("");
      return Asis.Nil_Element_List;
   end Block_Exception_Handlers;

   ----------------------
   -- Block_Statements --
   ----------------------

   function Block_Statements
     (Statement       : in Asis.Statement;
      Include_Pragmas : in Boolean := False)
      return Asis.Statement_List
   is
      pragma Unreferenced (Include_Pragmas);
   begin
      Check_Nil_Element (Statement, "Block_Statements");
      Raise_Not_Implemented ("");
      return Asis.Nil_Element_List;
   end Block_Statements;

   -------------------------------
   -- Call_Statement_Parameters --
   -------------------------------

   function Call_Statement_Parameters
     (Statement  : in Asis.Statement;
      Normalized : in Boolean := False)
      return Asis.Association_List
   is
      package Get is
         type Visiter is new Gela.Element_Visiters.Visiter with record
            Result : Gela.Elements.Element_Sequence_Access;
         end record;

         overriding procedure Auxiliary_Apply
           (Self : in out Visiter;
            Node : not null Gela.Elements.Auxiliary_Applies.
              Auxiliary_Apply_Access);

         overriding procedure Procedure_Call_Statement
           (Self : in out Visiter;
            Node : not null Gela.Elements.Procedure_Call_Statements.
              Procedure_Call_Statement_Access);

      end Get;

      package body Get is

         overriding procedure Procedure_Call_Statement
           (Self : in out Visiter;
            Node : not null Gela.Elements.Procedure_Call_Statements.
              Procedure_Call_Statement_Access)
         is
            Prefix : constant Gela.Elements.Names.Name_Access :=
              Node.Function_Call;
         begin
            Prefix.Visit (Self);
         end Procedure_Call_Statement;

         overriding procedure Auxiliary_Apply
           (Self : in out Visiter;
            Node : not null Gela.Elements.Auxiliary_Applies.
              Auxiliary_Apply_Access)
         is
            RA : constant Gela.Elements.Association_Lists.
              Association_List_Access := Node.Function_Call_Parameters;
            List  : constant Gela.Elements.Associations.
              Association_Sequence_Access :=
                RA.Record_Component_Associations;
         begin
            Self.Result := Gela.Elements.Element_Sequence_Access (List);
         end Auxiliary_Apply;

      end Get;

      V : Get.Visiter;
   begin
      Check_Nil_Element (Statement, "Call_Statement_Parameters");
      Statement.Data.Visit (V);

      if Normalized then
         Raise_Not_Implemented ("");
         return Asis.Nil_Element_List;
      else
         return Asis.To_List (V.Result);
      end if;
   end Call_Statement_Parameters;

   -----------------
   -- Called_Name --
   -----------------

   function Called_Name
     (Statement : in Asis.Statement)
      return Asis.Expression
   is
      package Get is
         type Visiter is new Gela.Element_Visiters.Visiter with record
            Result : Gela.Elements.Element_Access;
         end record;

         overriding procedure Auxiliary_Apply
           (Self : in out Visiter;
            Node : not null Gela.Elements.Auxiliary_Applies.
              Auxiliary_Apply_Access);

         overriding procedure Procedure_Call_Statement
           (Self : in out Visiter;
            Node : not null Gela.Elements.Procedure_Call_Statements.
              Procedure_Call_Statement_Access);

      end Get;

      package body Get is

         overriding procedure Procedure_Call_Statement
           (Self : in out Visiter;
            Node : not null Gela.Elements.Procedure_Call_Statements.
              Procedure_Call_Statement_Access)
         is
            Prefix : constant Gela.Elements.Names.Name_Access :=
              Node.Function_Call;
         begin
            Prefix.Visit (Self);
         end Procedure_Call_Statement;

         overriding procedure Auxiliary_Apply
           (Self : in out Visiter;
            Node : not null Gela.Elements.Auxiliary_Applies.
              Auxiliary_Apply_Access)
         is
            Prefix : constant Gela.Elements.Prefixes.Prefix_Access :=
              Node.Prefix;
         begin
            Self.Result := Gela.Elements.Element_Access (Prefix);
         end Auxiliary_Apply;

      end Get;

      V : Get.Visiter;
   begin
      Check_Nil_Element (Statement, "Called_Name");
      Statement.Data.Visit (V);
      return (Data => V.Result);
   end Called_Name;

   ---------------------
   -- Case_Expression --
   ---------------------

   function Case_Expression
     (Statement : in Asis.Statement)
      return Asis.Expression
   is
   begin
      Check_Nil_Element (Statement, "Case_Expression");
      Raise_Not_Implemented ("");
      return Asis.Nil_Element;
   end Case_Expression;

   ----------------------------------------
   -- Case_Statement_Alternative_Choices --
   ----------------------------------------

   function Case_Statement_Alternative_Choices
     (Path : in Asis.Path)
      return Asis.Element_List
   is
   begin
      Check_Nil_Element (Path, "Case_Statement_Alternative_Choices");
      Raise_Not_Implemented ("");
      return Asis.Nil_Element_List;
   end Case_Statement_Alternative_Choices;

   ------------------------------------
   -- Choice_Parameter_Specification --
   ------------------------------------

   function Choice_Parameter_Specification
     (Handler : in Asis.Exception_Handler)
      return Asis.Declaration
   is
   begin
      Check_Nil_Element (Handler, "Choice_Parameter_Specification");
      Raise_Not_Implemented ("");
      return Asis.Nil_Element;
   end Choice_Parameter_Specification;

   --------------------------
   -- Condition_Expression --
   --------------------------

   function Condition_Expression
     (Path : in Asis.Path)
      return Asis.Expression
   is
   begin
      Check_Nil_Element (Path, "Condition_Expression");
      Raise_Not_Implemented ("");
      return Asis.Nil_Element;
   end Condition_Expression;

   ---------------------------------
   -- Corresponding_Called_Entity --
   ---------------------------------

   function Corresponding_Called_Entity
     (Statement : in Asis.Statement)
      return Asis.Declaration
   is
   begin
      Check_Nil_Element (Statement, "Corresponding_Called_Entity");
      Raise_Not_Implemented ("");
      return Asis.Nil_Element;
   end Corresponding_Called_Entity;

   -----------------------------------------
   -- Corresponding_Destination_Statement --
   -----------------------------------------

   function Corresponding_Destination_Statement
     (Statement : in Asis.Statement)
      return Asis.Statement
   is
   begin
      Check_Nil_Element (Statement, "Corresponding_Destination_Statement");
      Raise_Not_Implemented ("");
      return Asis.Nil_Element;
   end Corresponding_Destination_Statement;

   -------------------------
   -- Corresponding_Entry --
   -------------------------

   function Corresponding_Entry
     (Statement : in Asis.Statement)
      return Asis.Declaration
   is
   begin
      Check_Nil_Element (Statement, "Corresponding_Entry");
      Raise_Not_Implemented ("");
      return Asis.Nil_Element;
   end Corresponding_Entry;

   -------------------------------
   -- Corresponding_Loop_Exited --
   -------------------------------

   function Corresponding_Loop_Exited
     (Statement : in Asis.Statement)
      return Asis.Statement
   is
   begin
      Check_Nil_Element (Statement, "Corresponding_Loop_Exited");
      Raise_Not_Implemented ("");
      return Asis.Nil_Element;
   end Corresponding_Loop_Exited;

   ----------------------
   -- Delay_Expression --
   ----------------------

   function Delay_Expression
     (Statement : in Asis.Statement)
      return Asis.Expression
   is
   begin
      Check_Nil_Element (Statement, "Delay_Expression");
      Raise_Not_Implemented ("");
      return Asis.Nil_Element;
   end Delay_Expression;

   -----------------------
   -- Exception_Choices --
   -----------------------

   function Exception_Choices
     (Handler : in Asis.Exception_Handler)
      return Asis.Element_List
   is
   begin
      Check_Nil_Element (Handler, "Exception_Choices");
      Raise_Not_Implemented ("");
      return Asis.Nil_Element_List;
   end Exception_Choices;

   --------------------
   -- Exit_Condition --
   --------------------

   function Exit_Condition
     (Statement : in Asis.Statement)
      return Asis.Expression
   is
   begin
      Check_Nil_Element (Statement, "Exit_Condition");
      Raise_Not_Implemented ("");
      return Asis.Nil_Element;
   end Exit_Condition;

   --------------------
   -- Exit_Loop_Name --
   --------------------

   function Exit_Loop_Name
     (Statement : in Asis.Statement)
      return Asis.Expression
   is
   begin
      Check_Nil_Element (Statement, "Exit_Loop_Name");
      Raise_Not_Implemented ("");
      return Asis.Nil_Element;
   end Exit_Loop_Name;

   ----------------------------------------
   -- Extended_Return_Exception_Handlers --
   ----------------------------------------

   function Extended_Return_Exception_Handlers
     (Statement       : Asis.Statement;
      Include_Pragmas : Boolean := False)
      return Asis.Exception_Handler_List
   is
      pragma Unreferenced (Include_Pragmas);
   begin
      Check_Nil_Element (Statement, "Extended_Return_Exception_Handlers");
      Raise_Not_Implemented ("");
      return Asis.Nil_Element_List;
   end Extended_Return_Exception_Handlers;

   --------------------------------
   -- Extended_Return_Statements --
   --------------------------------

   function Extended_Return_Statements
     (Statement       : Asis.Statement;
      Include_Pragmas : Boolean := False)
      return Asis.Statement_List
   is
      pragma Unreferenced (Include_Pragmas);
   begin
      Check_Nil_Element (Statement, "Extended_Return_Statements");
      Raise_Not_Implemented ("");
      return Asis.Nil_Element_List;
   end Extended_Return_Statements;

   --------------------------------------
   -- For_Loop_Parameter_Specification --
   --------------------------------------

   function For_Loop_Parameter_Specification
     (Statement : in Asis.Statement)
      return Asis.Declaration
   is
   begin
      Check_Nil_Element (Statement, "For_Loop_Parameter_Specification");
      Raise_Not_Implemented ("");
      return Asis.Nil_Element;
   end For_Loop_Parameter_Specification;

   ----------------
   -- Goto_Label --
   ----------------

   function Goto_Label
     (Statement : in Asis.Statement)
      return Asis.Expression
   is
   begin
      Check_Nil_Element (Statement, "Goto_Label");
      Raise_Not_Implemented ("");
      return Asis.Nil_Element;
   end Goto_Label;

   -----------
   -- Guard --
   -----------

   function Guard
     (Path : in Asis.Path)
      return Asis.Expression
   is
   begin
      Check_Nil_Element (Path, "Guard");
      Raise_Not_Implemented ("");
      return Asis.Nil_Element;
   end Guard;

   ------------------------
   -- Handler_Statements --
   ------------------------

   function Handler_Statements
     (Handler         : in Asis.Exception_Handler;
      Include_Pragmas : in Boolean := False)
      return Asis.Statement_List
   is
      pragma Unreferenced (Include_Pragmas);
   begin
      Check_Nil_Element (Handler, "Handler_Statements");
      Raise_Not_Implemented ("");
      return Asis.Nil_Element_List;
   end Handler_Statements;

   --------------------------------------
   -- Is_Call_On_Dispatching_Operation --
   --------------------------------------

   function Is_Call_On_Dispatching_Operation
     (Call : in Asis.Element)
      return Boolean
   is
   begin
      Check_Nil_Element (Call, "Is_Call_On_Dispatching_Operation");
      Raise_Not_Implemented ("");
      return False;
   end Is_Call_On_Dispatching_Operation;

   ----------------------
   -- Is_Declare_Block --
   ----------------------

   function Is_Declare_Block
     (Statement : in Asis.Statement)
      return Boolean
   is
   begin
      Check_Nil_Element (Statement, "Is_Declare_Block");
      Raise_Not_Implemented ("");
      return False;
   end Is_Declare_Block;

   -------------------------
   -- Is_Dispatching_Call --
   -------------------------

   function Is_Dispatching_Call (Call : in Asis.Element) return Boolean is
   begin
      Check_Nil_Element (Call, "Is_Dispatching_Call");
      Raise_Not_Implemented ("");
      return False;
   end Is_Dispatching_Call;

   ----------------------
   -- Is_Name_Repeated --
   ----------------------

   function Is_Name_Repeated
     (Statement : in Asis.Statement)
      return Boolean
   is
   begin
      Check_Nil_Element (Statement, "Is_Name_Repeated");
      Raise_Not_Implemented ("");
      return False;
   end Is_Name_Repeated;

   -----------------
   -- Label_Names --
   -----------------

   function Label_Names
     (Statement : in Asis.Statement)
      return Asis.Defining_Name_List
   is
   begin
      Check_Nil_Element (Statement, "Label_Names");
      Raise_Not_Implemented ("");
      return Asis.Nil_Element_List;
   end Label_Names;

   ---------------------
   -- Loop_Statements --
   ---------------------

   function Loop_Statements
     (Statement       : in Asis.Statement;
      Include_Pragmas : in Boolean := False)
      return Asis.Statement_List
   is
      pragma Unreferenced (Include_Pragmas);
   begin
      Check_Nil_Element (Statement, "Loop_Statements");
      Raise_Not_Implemented ("");
      return Asis.Nil_Element_List;
   end Loop_Statements;

   --------------------------
   -- Qualified_Expression --
   --------------------------

   function Qualified_Expression
     (Statement : in Asis.Statement)
      return Asis.Expression
   is
   begin
      Check_Nil_Element (Statement, "Qualified_Expression");
      Raise_Not_Implemented ("");
      return Asis.Nil_Element;
   end Qualified_Expression;

   ----------------------
   -- Raised_Exception --
   ----------------------

   function Raised_Exception
     (Statement : in Asis.Statement)
      return Asis.Expression
   is
   begin
      Check_Nil_Element (Statement, "Raised_Exception");
      Raise_Not_Implemented ("");
      return Asis.Nil_Element;
   end Raised_Exception;

   -----------------------------
   -- Raise_Statement_Message --
   -----------------------------

   function Raise_Statement_Message  -- 13.3(2)
     (Statement : Asis.Statement)
     return Asis.Expression is
   begin
      Check_Nil_Element (Statement, "Raise_Statement_Message");
      Raise_Not_Implemented ("");
      return Asis.Nil_Element;
   end Raise_Statement_Message;

   ------------------------
   -- Requeue_Entry_Name --
   ------------------------

   function Requeue_Entry_Name
     (Statement : in Asis.Statement)
      return Asis.Name
   is
   begin
      Check_Nil_Element (Statement, "Requeue_Entry_Name");
      Raise_Not_Implemented ("");
      return Asis.Nil_Element;
   end Requeue_Entry_Name;

   -----------------------
   -- Return_Expression --
   -----------------------

   function Return_Expression
     (Statement : in Asis.Statement)
      return Asis.Expression
   is
   begin
      Check_Nil_Element (Statement, "Return_Expression");
      Raise_Not_Implemented ("");
      return Asis.Nil_Element;
   end Return_Expression;

   ---------------------------------
   -- Return_Object_Specification --
   ---------------------------------

   function Return_Object_Specification
     (Statement : Asis.Statement)
      return Asis.Declaration
   is
   begin
      Check_Nil_Element (Statement, "Return_Object_Specification");
      Raise_Not_Implemented ("");
      return Asis.Nil_Element;
   end Return_Object_Specification;

   ----------------------------
   -- Sequence_Of_Statements --
   ----------------------------

   function Sequence_Of_Statements
     (Path            : in Asis.Path;
      Include_Pragmas : in Boolean := False)
      return Asis.Statement_List
   is
      pragma Unreferenced (Include_Pragmas);
   begin
      Check_Nil_Element (Path, "Sequence_Of_Statements");
      Raise_Not_Implemented ("");
      return Asis.Nil_Element_List;
   end Sequence_Of_Statements;

   --------------------------
   -- Statement_Identifier --
   --------------------------

   function Statement_Identifier
     (Statement : in Asis.Statement)
      return Asis.Defining_Name
   is
   begin
      Check_Nil_Element (Statement, "Statement_Identifier");
      Raise_Not_Implemented ("");
      return Asis.Nil_Element;
   end Statement_Identifier;

   ---------------------
   -- Statement_Paths --
   ---------------------

   function Statement_Paths
     (Statement : in Asis.Statement;
      Include_Pragmas : in Boolean := False)
      return Asis.Path_List
   is
      pragma Unreferenced (Include_Pragmas);
   begin
      Check_Nil_Element (Statement, "Statement_Paths");
      Raise_Not_Implemented ("");
      return Asis.Nil_Element_List;
   end Statement_Paths;

   ---------------------
   -- While_Condition --
   ---------------------

   function While_Condition
     (Statement : in Asis.Statement)
      return Asis.Expression
   is
   begin
      Check_Nil_Element (Statement, "While_Condition");
      Raise_Not_Implemented ("");
      return Asis.Nil_Element;
   end While_Condition;

end Asis.Statements;


------------------------------------------------------------------------------
--  Copyright (c) 2006-2013, Maxim Reznik
--  All rights reserved.
--
--  Redistribution and use in source and binary forms, with or without
--  modification, are permitted provided that the following conditions are met:
--
--     * Redistributions of source code must retain the above copyright notice,
--       this list of conditions and the following disclaimer.
--     * Redistributions in binary form must reproduce the above copyright
--       notice, this list of conditions and the following disclaimer in the
--       documentation and/or other materials provided with the distribution.
--     * Neither the name of the Maxim Reznik, IE nor the names of its
--       contributors may be used to endorse or promote products derived from
--       this software without specific prior written permission.
--
--  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
--  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
--  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
--  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
--  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
--  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
--  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
--  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
--  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
--  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
--  POSSIBILITY OF SUCH DAMAGE.
------------------------------------------------------------------------------
