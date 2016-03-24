//
// Scaled Thrift Mode - a Scaled major mode for editing Thrift code
// http://github.com/scaled/scala-mode/blob/master/LICENSE

package scaled.thrift

import scaled._
import scaled.code.{CodeConfig, Commenter}
import scaled.grammar.{Grammar, GrammarConfig, GrammarCodeMode}
import scaled.util.Paragrapher

object ThriftConfig extends Config.Defs {
  import CodeConfig._
  import GrammarConfig._

  // map TextMate grammar scopes to Scaled style definitions
  val effacers = List(
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

    // storage.type.field: leaving white for now
    effacer("variable.parameter", variableStyle)
  )

  // map TextMate grammar scopes to Scaled syntax definitions
  val syntaxers = List(
    syntaxer("comment.line", Syntax.LineComment),
    syntaxer("comment.block", Syntax.DocComment),
    syntaxer("constant", Syntax.OtherLiteral),
    syntaxer("string.quoted.triple", Syntax.HereDocLiteral),
    syntaxer("string.quoted.double", Syntax.StringLiteral)
  )

  val grammars = resource(Seq("Thrift.ndf"))(Grammar.parseNDFs)
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

  override def configDefs = ThriftConfig :: super.configDefs

  override def grammars = ThriftConfig.grammars.get
  override def effacers = ThriftConfig.effacers
  override def syntaxers = ThriftConfig.syntaxers

  override def mkParagrapher (syntax :Syntax) =
    if (syntax != HD) super.mkParagrapher(syntax)
    else new Paragrapher(syntax, buffer) {
      override def isDelim (row :Int) = super.isDelim(row) || {
        val ln = line(row)
        (ln.syntaxAt(0) != HD) || (ln.syntaxAt(ln.length-1) != HD)
      }
    }

  override protected def createIndenter = new ThriftIndenter(buffer, config)

  override protected def canAutoFill (p :Loc) :Boolean =
    super.canAutoFill(p) || (buffer.syntaxNear(p) == HD)

  override val commenter :ThriftCommenter = new ThriftCommenter() {
    // the thrift grammar marks all whitespace leading up to the open doc in comment style, so we
    // have to hack this predicate a bit
    override def inDoc (buffer :BufferV, p :Loc) :Boolean = {
      super.inDoc(buffer, p) && {
        val line = buffer.line(p)
        (line.indexOf(openDocM, p.col) == -1)
      }
    }
  }

  //
  // FNs

  override def electricNewline () {
    // shenanigans to determine whether we should auto-insert the doc prefix (* )
    if (commenter.inDoc(buffer, view.point())) {
      newline()
      view.point() = commenter.insertDocPre(buffer, view.point())
      reindentAtPoint()
    } else super.electricNewline()
  }

  // TODO: more things!
}