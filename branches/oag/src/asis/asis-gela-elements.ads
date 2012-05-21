------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                     http://www.ten15.org/wiki/Ada                        --
--                     - - - - - - - - - - - - - - -                        --
--            Read copyright and license at the end of this file            --
------------------------------------------------------------------------------
--  $TenDRA: $
--  Purpose:

with Asis.Gela.Properties;
with Asis.Gela.Compilations;

package Asis.Gela.Elements is
   package P renames Asis.Gela.Properties;

   function New_Element
     (C    : Compilations.Compilation;
      Kind : P.Global_Kinds) return Element_Index;

   function Get
     (C        : Compilations.Compilation;
      Element  : Element_Index;
      Property : P.Property_Kinds) return Property_Value;

   function Get
     (C        : Compilations.Compilation;
      Element  : Element_Index;
      Index    : Element_Index) return Property_Value;

   procedure Get
     (C        : in     Compilations.Compilation;
      Element  : in     Element_Index;
      Property : in     P.Property_Kinds;
      Result   :    out Property_Value;
      Success  :    out Boolean);

   procedure Set
     (C        : Compilations.Compilation;
      Element  : Element_Index;
      Property : P.Property_Kinds;
      Value    : Property_Value);

   function Global_Kind
     (C       : Compilations.Compilation;
      Element : Element_Index) return P.Global_Kinds;

   procedure Push_Argument
     (C    : Compilations.Compilation;
      Call : Element_Index;
      Arg  : Element_Index);
   --  Parser utility procedure

   procedure Set_Start_Position
     (C       : Compilations.Compilation;
      Element : Element_Index;
      Source  : Element_Index);

   procedure Set_End_Position
     (C       : Compilations.Compilation;
      Element : Element_Index;
      Source  : Element_Index);

   function Child_Elements
     (Element  : Asis.Element;
      Property : P.Property_Kinds;
      Include_Pragmas : in Boolean := False) return Asis.Element_List;

   function Child_Element
     (Element  : Asis.Element;
      Property : P.Property_Kinds) return Asis.Element;

   function Get_Image
     (Element  : Asis.Element;
      Property : P.Property_Kinds) return Asis.Program_Text;

   function Get_Boolean
     (Element  : Asis.Element;
      Property : P.Property_Kinds) return Boolean;

   type Global_Kind_List is array (Positive range <>) of P.Global_Kinds;

   function Kind_In_List
     (C    : Compilations.Compilation;
      Item : Element_Index;
      List : Global_Kind_List) return Boolean;

end Asis.Gela.Elements;

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
