------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                          http://gela.ada-ru.org                          --
--                     - - - - - - - - - - - - - - -                        --
--            Read copyright and license at the end of this file            --
------------------------------------------------------------------------------
--  $Revision$ $Date$
--  Purpose:
--  Helper functions to work with text and source buffers

with Gela.Decoders;
with Gela.Source_Buffers;             use Gela;

with Asis.Gela.Lines;

package Asis.Gela.Text_Utils is

   type Source_Buffer_Access is
     access all Source_Buffers.Source_Buffer'Class;

   function New_Buffer (File : in Wide_String) return Source_Buffer_Access;

   procedure Free (Buffer : in out Source_Buffer_Access);

   function Source_Buffer
     (Unit : Asis.Compilation_Unit) return Source_Buffer_Access;

   subtype Decoder_Access is Decoders.Decoder_Access;

   function Decoder
     (Unit : Asis.Compilation_Unit) return Decoder_Access;

   function Get_Line
     (Unit  : Asis.Compilation_Unit;
      Index : Asis.Asis_Positive) return Lines.Line;

   function Compilation_Line_Count
     (Unit : Asis.Compilation_Unit) return Asis.Asis_Natural;

end Asis.Gela.Text_Utils;


------------------------------------------------------------------------------
--  Copyright (c) 2008-2013, Maxim Reznik
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
