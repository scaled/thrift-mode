//
// Scaled Thrift Mode - a Scaled major mode for editing Thrift code
// http://github.com/scaled/thrift-mode/blob/master/LICENSE

package scaled.thrift

import scaled._
import scaled.code.Commenter

/** Extends [[Commenter]] with some Thriftdoc smarts. */
class ThriftCommenter extends Commenter {
  import scaled.code.CodeConfig._

  val openDocM = Matcher.exact("/**")
  val closeDocM = Matcher.exact("*/")
  val atCmdM = Matcher.regexp("@[a-z]+")

  def inDoc (buffer :BufferV, p :Loc) :Boolean = {
    val line = buffer.line(p)
    // we need to be on doc-styled text...
    ((buffer.stylesNear(p) contains docStyle) &&
     // and not on the open doc (/**)
     !line.matches(openDocM, p.col) &&
     // and not on or after the close doc (*/)
     (line.lastIndexOf(closeDocM, p.col) == -1))
  }

  def insertDocPre (buffer :Buffer, p :Loc) :Loc = {
    buffer.insert(p, Line(docPrefix))
    p + (0, docPrefix.length)
  }

  override def linePrefix  = "//"
  override def blockOpen = "/*"
  override def blockClose = "*/"
  override def blockPrefix = "*"
  override def docPrefix   = "*"

  override def mkParagrapher (syn :Syntax, buf :Buffer) = new CommentParagrapher(syn, buf) {
    private def isAtCmdLine (line :LineV) = line.matches(atCmdM, commentStart(line))
    // don't extend paragraph upwards if the current top is an @cmd
    override def canPrepend (row :Int) =
      super.canPrepend(row) && !isAtCmdLine(line(row+1))
    // don't extend paragraph downwards if the new line is at an @cmd
    override def canAppend (row :Int) =
      super.canAppend(row) && !isAtCmdLine(line(row))
  }

  override def commentDelimLen (line :LineV, col :Int) =
    if (line.matches(openDocM, col)) openDocM.matchLength
    else if (line.matches(closeDocM, col)) closeDocM.matchLength
    else super.commentDelimLen(line, col)
}
