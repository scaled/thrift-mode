//
// Scaled Thrift Mode - a Scaled major mode for editing Thrift code
// http://github.com/scaled/scala-mode/blob/master/LICENSE

package scaled.thrift

import scaled._
import scaled.code.{CodeConfig, Commenter, BlockIndenter}
import scaled.grammar._
import scaled.util.Paragrapher

@Plugin(tag="textmate-grammar")
class ThriftGrammarPlugin extends GrammarPlugin {
  import CodeConfig._

  override def grammars = Map("source.thrift" -> "Thrift.ndf")

  override def effacers = List(
    effacer("comment.line", commentStyle),
    effacer("comment.block", docStyle),
    effacer("constant", constantStyle),
    effacer("invalid", invalidStyle),
    effacer("keyword", keywordStyle),
    effacer("string", stringStyle),

    effacer("variable.other.namespace", moduleStyle),
    effacer("entity.name.type", typeStyle),
    effacer("entity.name.function", functionStyle),
    effacer("entity.other.field-id", preprocessorStyle),

    effacer("storage.type.field", typeStyle),
    // storage.type.field: leaving white for now
    effacer("variable.parameter", variableStyle)
  )

  override def syntaxers = List(
    syntaxer("comment.line", Syntax.LineComment),
    syntaxer("comment.block", Syntax.DocComment),
    syntaxer("constant", Syntax.OtherLiteral),
    syntaxer("string.quoted.triple", Syntax.HereDocLiteral),
    syntaxer("string.quoted.double", Syntax.StringLiteral)
  )
}

@Major(name="thrift",
       tags=Array("code", "project", "thrift"),
       pats=Array(".*\\.thrift"),
       ints=Array("thrift"),
       desc="A major editing mode for Thrift IDL files.")
class ThriftMode (env :Env) extends GrammarCodeMode(env) {
  import CodeConfig._
  import scaled.util.Chars._
  import Syntax.{HereDocLiteral => HD}

  override def langScope = "source.thrift"

  override def mkParagrapher (syntax :Syntax) =
    if (syntax != HD) super.mkParagrapher(syntax)
    else new Paragrapher(syntax, buffer) {
      override def isDelim (row :Int) = super.isDelim(row) || {
        val ln = line(row)
        (ln.syntaxAt(0) != HD) || (ln.syntaxAt(ln.length-1) != HD)
      }
    }

  override protected def createIndenter = new BlockIndenter(config, Seq())

  override protected def canAutoFill (p :Loc) :Boolean =
    super.canAutoFill(p) || (buffer.syntaxNear(p) == HD)

  override val commenter = new Commenter() {
    override def linePrefix  = "//"
    override def blockPrefix = "*"
    override def blockOpen   = "/*"
    override def blockClose  = "*/"
    override def docOpen     = "/**"
  }

  // TODO: more things!
}
