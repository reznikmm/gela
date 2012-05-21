<?xml version="1.0" encoding="koi8-r"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version='1.0'>

<xsl:output method="text" indent="yes" encoding="utf-8"/>

<xsl:variable name="arrow"> =&gt; </xsl:variable>

<xsl:template match="/">
package body Asis.Gela.Properties is

   Map : constant array (Global_Kinds, Property_Kinds) of Asis.ASIS_Natural :=
     (<xsl:apply-templates select="//node[not (@abstract)]"/>);

   Size_Map : constant array (Global_Kinds) of Asis.ASIS_Positive :=
     (<xsl:apply-templates select="//node[not (@abstract)]" mode="size"/>);

   Child_Map : constant array (Global_Kinds) of Traverse_List :=
     (<xsl:apply-templates select="//node[not (@abstract)]" mode="child"/>);

   --------------------
   -- Property_Index --
   --------------------

   function Property_Index
     (Global_Kind : Global_Kinds;
      Property    : Property_Kinds) return Asis.ASIS_Natural is
   begin
      return Map (Global_Kind, Property);
   end Property_Index;

   ----------
   -- Size --
   ----------

   function Size (Global_Kind : Global_Kinds) return Asis.ASIS_Positive is
   begin
      return Size_Map (Global_Kind);
   end Size;

   --------------
   -- Children --
   --------------

   function Children (Global_Kind : Global_Kinds) return Traverse_List is
   begin
      return Child_Map (Global_Kind);
   end Children;

end Asis.Gela.Properties;

</xsl:template>

<xsl:template match="node">
  <xsl:variable name="attrs"
                select="ancestor-or-self::node/attr[not (@skip)]"/>
  <xsl:variable name="types"
                select="ancestor-or-self::node/attr[not (@skip)]/@type"/>

   <xsl:value-of select="@name"/>
   <xsl:value-of select="$arrow"/>
        (<xsl:for-each select="$attrs">
     <xsl:variable name="i" select="position()"/>

     <xsl:value-of select="@name"/>
     <xsl:value-of select="$arrow"/>

     <xsl:call-template name="calc-index">
       <xsl:with-param name="types" select="$types"/>
       <xsl:with-param name="i" select="$i"/>
     </xsl:call-template>,
         </xsl:for-each
         >others => 0)<xsl:if test="@name!='An_Any_Compilation_Unit'">
     <xsl:text>,

      </xsl:text>
   </xsl:if>
</xsl:template>

<xsl:template match="node" mode="size">
  <xsl:variable name="types"
                select="ancestor-or-self::node/attr[not (@skip)]/@type"/>

   <xsl:value-of select="@name"/>
   <xsl:value-of select="$arrow"/>
     <xsl:variable name="i" select="count($types)+1"/>

     <xsl:call-template name="calc-index">
       <xsl:with-param name="types" select="$types"/>
       <xsl:with-param name="i" select="$i"/>
     </xsl:call-template><xsl:if test="@name!='An_Any_Compilation_Unit'">
     <xsl:text>,
      </xsl:text>
   </xsl:if>
</xsl:template>

<xsl:template match="node" mode="child">
  <xsl:variable name="attrs"
                select="ancestor-or-self::node/attr[not (@skip)]"/>
  <xsl:variable name="types"
                select="ancestor-or-self::node/attr[not (@skip)]/@type"/>

   <xsl:value-of select="@name"/>
   <xsl:value-of select="$arrow"/>
        (<xsl:for-each select="$attrs">
     <xsl:if test="@child">
       <xsl:variable name="i" select="position()"/>

       <xsl:call-template name="child-index">
         <xsl:with-param name="attrs" select="$attrs"/>
         <xsl:with-param name="i" select="@child"/>
       </xsl:call-template>
       <xsl:value-of select="$arrow"/>
       
       <xsl:call-template name="calc-index">
         <xsl:with-param name="types" select="$types"/>
         <xsl:with-param name="i" select="$i"/>
       </xsl:call-template>, </xsl:if></xsl:for-each
         >others => 0)<xsl:if test="@name!='An_Any_Compilation_Unit'">
     <xsl:text>,

      </xsl:text>
   </xsl:if>
</xsl:template>

<xsl:template name="calc-index">
  <xsl:param name="types"/>
  <xsl:param name="i"/>
  <xsl:if test="$i = 1">1</xsl:if>
  <xsl:if test="$i != 1">
    <xsl:variable name="prev">
      <xsl:call-template name="calc-index">
        <xsl:with-param name="types" select="$types"/>
        <xsl:with-param name="i" select="$i -1"/>
      </xsl:call-template>
    </xsl:variable>
    <xsl:variable name="size">
      <xsl:call-template name="size-of">
        <xsl:with-param name="type" select="$types[$i - 1]"/>
      </xsl:call-template>
    </xsl:variable>
    <xsl:value-of select="$prev + $size"/>
  </xsl:if>
</xsl:template>

<xsl:template name="size-of">
  <xsl:param name="type"/>
  <xsl:choose>
    <xsl:when test="$type = 'Asis.Text_Position'">2</xsl:when>
    <xsl:otherwise>1</xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template name="child-index">
  <xsl:param name="attrs"/>
  <xsl:param name="i"/>
  <xsl:value-of select="count($attrs[@child &lt; $i])+1"/>
</xsl:template>

</xsl:stylesheet>
