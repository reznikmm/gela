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
with Gela.Elements.Program_Unit_Names;
with Gela.Elements.Subtype_Marks;
with Gela.Elements.Use_Package_Clauses;
with Gela.Elements.Use_Type_Clauses;
with Gela.Elements.With_Clauses;

package body Asis.Clauses is

   ------------------
   -- Clause_Names --
   ------------------

   function Clause_Names
     (Clause : in Asis.Element)
      return Asis.Name_List
   is
      package Get is
         type Visiter is new Gela.Element_Visiters.Visiter with record
            List : Gela.Elements.Element_Sequence_Access;
         end record;

         overriding procedure Use_Package_Clause
           (Self : in out Visiter;
            Node : not null Gela.Elements.Use_Package_Clauses.
              Use_Package_Clause_Access);

         overriding procedure Use_Type_Clause
           (Self : in out Visiter;
            Node : not null Gela.Elements.Use_Type_Clauses.
              Use_Type_Clause_Access);

         overriding procedure With_Clause
           (Self : in out Visiter;
            Node : not null Gela.Elements.With_Clauses.With_Clause_Access);

      end Get;

      ---------
      -- Get --
      ---------

      package body Get is

         ------------------------
         -- Use_Package_Clause --
         ------------------------

         overriding procedure Use_Package_Clause
           (Self : in out Visiter;
            Node : not null Gela.Elements.Use_Package_Clauses.
              Use_Package_Clause_Access)
         is
            Names : constant Gela.Elements.Program_Unit_Names.
              Program_Unit_Name_Sequence_Access := Node.Clause_Names;
         begin
            Self.List := Gela.Elements.Element_Sequence_Access (Names);
         end Use_Package_Clause;

         ---------------------
         -- Use_Type_Clause --
         ---------------------

         overriding procedure Use_Type_Clause
           (Self : in out Visiter;
            Node : not null Gela.Elements.Use_Type_Clauses.
              Use_Type_Clause_Access)
         is
            Names : constant Gela.Elements.Subtype_Marks.
              Subtype_Mark_Sequence_Access := Node.Type_Clause_Names;
         begin
            Self.List := Gela.Elements.Element_Sequence_Access (Names);
         end Use_Type_Clause;

         -----------------
         -- With_Clause --
         -----------------

         overriding procedure With_Clause
           (Self : in out Visiter;
            Node : not null Gela.Elements.With_Clauses.With_Clause_Access)
         is
            Names : constant Gela.Elements.Program_Unit_Names.
              Program_Unit_Name_Sequence_Access := Node.With_Clause_Names;
         begin
            Self.List := Gela.Elements.Element_Sequence_Access (Names);
         end With_Clause;

      end Get;

      use type Gela.Elements.Element_Sequence_Access;
      V : Get.Visiter;
   begin
      Check_Nil_Element (Clause, "Clause_Names");
      Clause.Data.Visit (V);

      if V.List = null then
         return Asis.Nil_Element_List;
      else
         return Asis.To_List (V.List);
      end if;
   end Clause_Names;

   -------------------------------
   -- Component_Clause_Position --
   -------------------------------

   function Component_Clause_Position
     (Clause : in Asis.Component_Clause)
      return Asis.Expression
   is
   begin
      Check_Nil_Element (Clause, "Component_Clause_Position");
      Raise_Not_Implemented ("Component_Clause_Position");
      return Asis.Nil_Element;
   end Component_Clause_Position;

   ----------------------------
   -- Component_Clause_Range --
   ----------------------------

   function Component_Clause_Range
     (Clause : in Asis.Component_Clause)
      return Asis.Discrete_Range
   is
   begin
      Check_Nil_Element (Clause, "Component_Clause_Range");
      Raise_Not_Implemented ("Component_Clause_Range");
      return Asis.Nil_Element;
   end Component_Clause_Range;

   -----------------------
   -- Component_Clauses --
   -----------------------

   function Component_Clauses
     (Clause : in Asis.Representation_Clause;
      Include_Pragmas : in Boolean := False)
      return Asis.Component_Clause_List
   is
      pragma Unreferenced (Include_Pragmas);
   begin
      Check_Nil_Element (Clause, "Component_Clauses");
      Raise_Not_Implemented ("Component_Clauses");
      return Asis.Nil_Element_List;
   end Component_Clauses;

   ---------------------------
   -- Mod_Clause_Expression --
   ---------------------------

   function Mod_Clause_Expression
     (Clause : in Asis.Representation_Clause)
      return Asis.Expression
   is
   begin
      Check_Nil_Element (Clause, "Mod_Clause_Expression");
      Raise_Not_Implemented ("Mod_Clause_Expression");
      return Asis.Nil_Element;
   end Mod_Clause_Expression;

   --------------------------------------
   -- Representation_Clause_Expression --
   --------------------------------------

   function Representation_Clause_Expression
     (Clause : in Asis.Representation_Clause)
      return Asis.Expression
   is
   begin
      Check_Nil_Element (Clause, "Representation_Clause_Expression");
      Raise_Not_Implemented ("Representation_Clause_Expression");
      return Asis.Nil_Element;
   end Representation_Clause_Expression;

   --------------------------------
   -- Representation_Clause_Name --
   --------------------------------

   function Representation_Clause_Name
     (Clause : in Asis.Clause)
      return Asis.Name
   is
   begin
      Check_Nil_Element (Clause, "Representation_Clause_Name");
      Raise_Not_Implemented ("Representation_Clause_Name");
      return Asis.Nil_Element;
   end Representation_Clause_Name;

end Asis.Clauses;



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
