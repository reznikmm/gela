------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                     http://www.ten15.org/wiki/Ada                        --
--                     - - - - - - - - - - - - - - -                        --
--            Read copyright and license at the end of this file            --
------------------------------------------------------------------------------
--  $TenDRA: $
--  Purpose:
--  Set of functions to convert Global_Kinds to Asis element kinds


with Asis.Gela.Properties;
package Asis.Gela.To is
   package P renames Asis.Gela.Properties;

   function Element_Kinds
     (Kind : P.Global_Kinds) return Asis.Element_Kinds;

   function Defining_Name_Kinds
     (Kind : P.Global_Kinds) return Asis.Defining_Name_Kinds;

   function Declaration_Kinds
     (Kind : P.Global_Kinds) return Asis.Declaration_Kinds;

   function Definition_Kinds
     (Kind : P.Global_Kinds) return Asis.Definition_Kinds;

   function Type_Kinds
     (Kind : P.Global_Kinds) return Asis.Type_Kinds;

   function Formal_Type_Kinds
     (Kind : P.Global_Kinds) return Asis.Formal_Type_Kinds;

   function Access_Type_Kinds
     (Kind : P.Global_Kinds) return Asis.Access_Type_Kinds;

   function Access_Definition_Kinds
     (Kind : P.Global_Kinds) return Asis.Access_Definition_Kinds;

   function Constraint_Kinds
     (Kind : P.Global_Kinds) return Asis.Constraint_Kinds;

   function Discrete_Range_Kinds
     (Kind : P.Global_Kinds) return Asis.Discrete_Range_Kinds;

   function Association_Kinds
     (Kind : P.Global_Kinds) return Asis.Association_Kinds;

   function Expression_Kinds
     (Kind : P.Global_Kinds) return Asis.Expression_Kinds;

   function Statement_Kinds
     (Kind : P.Global_Kinds) return Asis.Statement_Kinds;

   function Path_Kinds
     (Kind : P.Global_Kinds) return Asis.Path_Kinds;

   function Clause_Kinds
     (Kind : P.Global_Kinds) return Asis.Clause_Kinds;

   function Representation_Clause_Kinds
     (Kind : P.Global_Kinds) return Asis.Representation_Clause_Kinds;

private
   pragma Inline (Element_Kinds);
   pragma Inline (Defining_Name_Kinds);
   pragma Inline (Declaration_Kinds);
   pragma Inline (Definition_Kinds);
   pragma Inline (Type_Kinds);
   pragma Inline (Formal_Type_Kinds);
   pragma Inline (Access_Type_Kinds);
   pragma Inline (Access_Definition_Kinds);
   pragma Inline (Constraint_Kinds);
   pragma Inline (Discrete_Range_Kinds);
   pragma Inline (Association_Kinds);
   pragma Inline (Expression_Kinds);
   pragma Inline (Statement_Kinds);
   pragma Inline (Path_Kinds);
   pragma Inline (Clause_Kinds);
   pragma Inline (Representation_Clause_Kinds);

end Asis.Gela.To;


------------------------------------------------------------------------------
--  Copyright (c) 2009, Maxim Reznik
--  All rights reserved.
--
--  Redistribution and use in source and binary forms, with or without
--  modification, are permitted provided that the following conditions are met:
--
--     * Redistributions of source code must retain the above copyright notice,
--     * this list of conditions and the following disclaimer.
--     * Redistributions in binary form must reproduce the above copyright
--     * notice, this list of conditions and the following disclaimer in the
--     * documentation and/or other materials provided with the distribution.
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
