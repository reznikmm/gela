------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                     http://www.ten15.org/wiki/Ada                        --
--                     - - - - - - - - - - - - - - -                        --
--            Read copyright and license at the end of this file            --
------------------------------------------------------------------------------
--  $TenDRA: asis-gela-base_lists.ads 2455 2006-06-24 19:22:06Z maxr $
--  Purpose:
--  Provide Primary_Lists and Secondary_Lists generics.
--  Instances of these generics are constrained with permited Element_Kinds.
--
--  Primary_List owns its elements. It is used to implement syntax properties
--  such as Body_Statements. An Element could be only in one Primary_List.
--
--  Secondary_List contains references to Elements. It is used to implement
--  semantic properties such as Corresponding_Representation_Clauses.

with Asis.Gela.Compilations;

package Asis.Gela.Base_Lists is

   -----------------------
   -- Element_Kind_List --
   -----------------------

   type Element_Kind_List is array (Positive range <>) of Element_Kinds;

   procedure Add
     (C    : in     Compilations.Compilation;
      List : in     Element_Index;
      Item : in     Element_Index);

   function Get
     (C     : Compilations.Compilation;
      List  : Element_Index;
      Index : List_Index) return Element_Index;

   function Length
     (C     : Compilations.Compilation;
      List  : Element_Index) return ASIS_Natural;

   function To_Element_List
     (C                : Compilations.Compilation;
      List             : Element_Index;
      Unit             : Compilation_Unit;
      Include_Pragmas  : Boolean)
     return Asis.Element_List;

   type Element_Index_List is array (ASIS_Positive range <>) of Element_Index;

   function To_Element_List
     (C    : Compilations.Compilation;
      List : Element_Index)
     return Element_Index_List;

   -------------------
   -- Primary_Lists --
   -------------------

   generic
      Allowed : in Element_Kind_List;
   package Primary_Lists is

      subtype List is Element_Index;

      procedure Add
        (C         : Compilations.Compilation;
         Container : in     List;
         Item      : in     Element_Index);

   end Primary_Lists;

   ---------------------
   -- Secondary_Lists --
   ---------------------

end Asis.Gela.Base_Lists;

------------------------------------------------------------------------------
--  Copyright (c) 2006, Maxim Reznik
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
