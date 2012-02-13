<?xml version='1.0'?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version='1.0'>

<xsl:output method="xml" indent='yes'/>

<xsl:template match="/|*|@*">
    <xsl:copy>
      <xsl:apply-templates select="*|@*"/>
    </xsl:copy>
</xsl:template>
  
<xsl:template match="node" >
  <xsl:copy>
    <xsl:apply-templates select="@*"/>
    <xsl:apply-templates select=".." mode='copy-helper'/>
    <xsl:apply-templates select="attr"/>
  </xsl:copy>
    <xsl:apply-templates select="node"/>
</xsl:template>

<xsl:template match="node[@abstract]" >
   <xsl:variable name="kind">
     <xsl:call-template name="kind-name"/>
   </xsl:variable>
  <xsl:copy>
    <xsl:apply-templates select="@*"/>
    <xsl:apply-templates select=".." mode='copy-helper'/>
    <attr name="{$kind}" type="Asis.{$kind}s"/>
    <xsl:apply-templates select="*"/>
  </xsl:copy>
</xsl:template>


<xsl:template match="node[@helper='y']" >
   <xsl:apply-templates select="node"/>
</xsl:template>

<xsl:template match="node" mode='copy-helper'>
</xsl:template>

<xsl:template match="node[@helper or not (@abstract)]" mode='copy-helper'>
  <xsl:apply-templates select="attr"/>
  <xsl:if test="../@helper or not (../@abstract)">
    <xsl:apply-templates select=".." mode='copy-helper'/>
  </xsl:if>
</xsl:template>

<xsl:template match="node[@name='Any_Compilation_Unit_Node']" >
</xsl:template>

<xsl:template match="attr[@defer='y' or starts-with (@name, 'Normalized_')]">
</xsl:template>

<xsl:template match="attr">
  <xsl:variable name="name" select="@name"/>

  <xsl:if test="not (../attr[@name=concat($name,'s')])">
    <xsl:copy>
      <xsl:apply-templates select="*|@*"/>
    </xsl:copy>
  </xsl:if>
</xsl:template>

<xsl:template match="attr[@name='Next_Element']"/>
<xsl:template match="attr[@name='Start_Position']"/>
<xsl:template match="attr[@name='End_Position']"/>

<xsl:template match="attr[@name='Loop_Parameter_Specification']">
    <xsl:copy>
      <xsl:attribute name="name">For_Loop_Parameter_Specification</xsl:attribute>
      <xsl:apply-templates select="*|@*[name()!='name']"/>
    </xsl:copy>
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

</xsl:stylesheet>
