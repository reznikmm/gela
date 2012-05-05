<?xml version='1.0'?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version='1.0'>

<xsl:output method="text"/>

<xsl:template match="/">
with Gtk.GEntry;
with Gtk.Combo_Box;
with Asis.Text; use Asis.Text;
with Asis.Clauses; use Asis.Clauses;
with Asis.Elements; use Asis.Elements;
with Asis.Declarations; use Asis.Declarations;
with Asis.Definitions; use Asis.Definitions;
with Asis.Expressions; use Asis.Expressions;
with Asis.Statements; use Asis.Statements;

with Ada.Characters.Handling;  use Ada.Characters.Handling;
with XASIS.Utils;

separate (GTK_Asis_Elements)
   procedure Show
     (Frame   : out GTK_Asis_Element;
      Element : in  Asis.Element)
is
begin
<xsl:apply-templates select= "node[@abstract]"/>
<xsl:for-each select="/node">
<xsl:call-template name="set-attr"/>
</xsl:for-each>
   null;
end;

</xsl:template>

<xsl:template match="node">
case <xsl:call-template name="kind-name"/> (Element) is
<xsl:apply-templates select= "node" mode="when"/>
   when others => null;
end case;
</xsl:template>

<xsl:template match="node" mode="when">
   <xsl:variable name="name" select="@name"/>
   when Asis.<xsl:call-template name="real-kind-name"/> =>
      Frame.Frames.<xsl:value-of select="$name"/>.Show;
<xsl:for-each select="../node[@name!=$name]"
>      Frame.Frames.<xsl:value-of select="@name"/>.Hide;
</xsl:for-each>
<xsl:call-template name="set-attr"/>
   <xsl:if test="@abstract">
     <xsl:apply-templates select= "."/>
   </xsl:if>
</xsl:template>

<xsl:template name="set-attr">
<xsl:for-each select="attr">
begin
  <xsl:choose>
    <xsl:when test="@name = 'Is_Name_Repeated'
                    or @name = 'Is_Private_Present'"
      >Gtk.Gentry.Set_Text (Frame.Frames.<xsl:value-of select="../@name"/>
                            <xsl:text>_</xsl:text>
                            <xsl:value-of select="@name"/>,
        <xsl:value-of select="@type"/>
        <xsl:text>'Image (</xsl:text>
        <xsl:call-template name="statements"/>
        <xsl:value-of select="@name"/> (Element)));
</xsl:when>
    <xsl:when test="substring-before(@type,'_Kinds') !=''
         or @type = 'Boolean'
         or @type='Asis.ASIS_Integer'
         or @type='Asis.Declaration_Origins'"
      >Gtk.Gentry.Set_Text (Frame.Frames.<xsl:value-of select="../@name"/>
                            <xsl:text>_</xsl:text>
                            <xsl:value-of select="@name"/>,
        <xsl:value-of select="@type"/>
        <xsl:text>'Image (</xsl:text>
        <xsl:value-of select="@name"/> (Element)));
</xsl:when>
    <xsl:when test="@type = 'Unbounded_Wide_String'
         or @type ='Asis.Compilation_Unit'"
      >Gtk.Gentry.Set_Text (Frame.Frames.<xsl:value-of select="../@name"/>
                            <xsl:text>_</xsl:text>
                            <xsl:value-of select="@name"/>,
        <xsl:text>To_String (</xsl:text>
          <xsl:value-of select="@name"/> (Element)));
</xsl:when>
    <xsl:when test="@name ='References'"/>
    <xsl:when test="substring-before(@type,'_Lists') !=''">
declare
   List : Asis.Element_List := <xsl:call-template name="attr-name"/> (Element);
   Vector : Vectors.Vector := Vectors.To_Vector (List'Length);
begin
   Clear (Frame.Frames.<xsl:value-of select="../@name"/>
                            <xsl:text>_</xsl:text>
                            <xsl:value-of select="@name"/>);
   for J in List'Range loop
      Gtk.Combo_Box.Append_Text (Frame.Frames.<xsl:value-of select="../@name"/>
                            <xsl:text>_</xsl:text>
                            <xsl:value-of select="@name"/>,
                            Asis.List_Index'Image (J) &amp; " => " &amp;
                            To_String (List (J)));
      Vector.Replace_Element (J, List (J));
   end loop;

   Vector_Data.Set (Frame.Frames.<xsl:value-of select="../@name"/>
                            <xsl:text>_</xsl:text>
                            <xsl:value-of select="@name"/>,
                    Vector);
end;
</xsl:when>
    <xsl:when test="@type ='Asis.Text.Span'"
>Gtk.Gentry.Set_Text (Frame.Frames.<xsl:value-of select="../@name"/>
                            <xsl:text>_</xsl:text>
                            <xsl:value-of select="@name"/>,
        <xsl:text>To_String (</xsl:text>
          <xsl:value-of select="@name"/> (Element)));
</xsl:when>
    <xsl:otherwise
>   Gtk.Gentry.Set_Text (Frame.Frames.<xsl:value-of select="../@name"/>
                            <xsl:text>_</xsl:text>
                            <xsl:value-of select="@name"/>,
        <xsl:text>To_String (</xsl:text>
        <xsl:value-of select="@name"/> (Element)));
   Gtk.Tooltips.Set_Tip (Frame.Tips,
                         Frame.Frames.<xsl:value-of select="../@name"/>
                            <xsl:text>_</xsl:text>
                            <xsl:value-of select="@name"/>,
                         To_String (XASIS.Utils.Debug_Image (<xsl:value-of select="@name"/> (Element))));
   Asis_Data.Set (Frame.Frames.<xsl:value-of select="../@name"/>
                            <xsl:text>_</xsl:text>
                            <xsl:value-of select="@name"/>,
                  <xsl:value-of select="@name"/> (Element));
    </xsl:otherwise>
  </xsl:choose>
null;
exception when others => 
  <xsl:choose>
    <xsl:when test="substring-before(@type,'_Lists') !=''">
      Gtk.Combo_Box.Append_Text (Frame.Frames.<xsl:value-of select="../@name"/>
                            <xsl:text>_</xsl:text>
                            <xsl:value-of select="@name"/>,
                            "[error]");
      Vector_Data.Set (Frame.Frames.<xsl:value-of select="../@name"/>
                            <xsl:text>_</xsl:text>
                            <xsl:value-of select="@name"/>,
                       Vectors.To_Vector (Asis.Nil_Element, 1));

    </xsl:when>
    <xsl:otherwise>
   Gtk.Gentry.Set_Text (Frame.Frames.<xsl:value-of select="../@name"/>
                            <xsl:text>_</xsl:text>
                            <xsl:value-of select="@name"/>, "[error]");
   Asis_Data.Set (Frame.Frames.<xsl:value-of select="../@name"/>
                            <xsl:text>_</xsl:text>
                            <xsl:value-of select="@name"/>,
                  Asis.Nil_Element);
    </xsl:otherwise>
  </xsl:choose>
end;
</xsl:for-each>
</xsl:template>

<xsl:template name="attr-name">
  <xsl:if test="@name = 'Progenitor_List'">Asis.Declarations.</xsl:if>
  <xsl:value-of select="@name"/>
</xsl:template>

<xsl:template name="kind-name">
  <xsl:choose>
    <xsl:when test="@name = 'Discrete_Subtype_Definition_Node'"
      >Discrete_Range</xsl:when>
    <xsl:when test="@name = 'Type_Definition_Node'"
      >Type</xsl:when>
    <xsl:when test="@name = 'Formal_Type_Definition_Node'"
      >Formal_Type</xsl:when>
    <xsl:otherwise>
      <xsl:value-of select="substring-before(@name,'_Node')"/>
    </xsl:otherwise>
  </xsl:choose>
  <xsl:text>_Kind</xsl:text>
</xsl:template>

<xsl:template name="statements">
<xsl:if test="ancestor::node[@name='Statement_Node']"
>Asis.Statements.</xsl:if>
<xsl:if test="ancestor::node[@name='Declaration_Node']"
>Asis.Declarations.</xsl:if>
<xsl:if test="ancestor::node[@name='Definition_Node']"
>Asis.Definitions.</xsl:if>
</xsl:template>

<xsl:template name="real-kind-name">
  <xsl:text>A</xsl:text>
  <xsl:choose>
    <xsl:when test="starts-with(@name,'A')">n</xsl:when>
    <xsl:when test="starts-with(@name,'E')">n</xsl:when>
    <xsl:when test="starts-with(@name,'I')">n</xsl:when>
    <xsl:when test="starts-with(@name,'O')">n</xsl:when>
    <xsl:when test="starts-with(@name,'Use')"></xsl:when>
    <xsl:when test="starts-with(@name,'U')">n</xsl:when>
  </xsl:choose>

  <xsl:text>_</xsl:text>

  <xsl:choose>
    <xsl:when test="starts-with(@name,'S_')">
      <xsl:value-of
         select="substring-after(substring-before(@name,'_Node'),'S_')"/>
    </xsl:when>
    <xsl:otherwise>
      <xsl:value-of select="substring-before(@name,'_Node')"/>
    </xsl:otherwise>
  </xsl:choose>

  <xsl:if test="ancestor::node[@name='Type_Definition_Node' 
                or @name='Formal_Type_Definition_Node']">
    <xsl:text>_Definition</xsl:text>
  </xsl:if>
</xsl:template>

</xsl:stylesheet>
