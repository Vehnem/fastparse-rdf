package com.github.vehnem.rdf.fastparse.grammar

import com.github.vehnem.rdf.fastparse.model.{RDFModel => MODEL}
import fastparse.NoWhitespace._
import fastparse._

/**
 * @author ${user.name}
 * @see <a href="https://www.w3.org/TR/n-triples/">RDF 1.1 N-Triples</a>
 */
object NTripleGrammar {

  /* GRAMMAR
  ----------------------------------------------------------------------------------------------------------------
   */

  /*
  ntriplesDoc ::= triple? (EOL triple)* EOL?
   */
  def ntriples[_: P]: P[MODEL.TRIPLES] = P(
    (ntriple ~ EOL).rep(1) ~ End
  ).map(MODEL.TRIPLES(_:_*))

  /*
  triple ::= subject predicate object '.'
  TODO whitespaces
   */
  def ntriple[_: P]: P[MODEL.TRIPLE] = P(
    _subject ~ " " ~ _predicate ~ " " ~ _object ~" ."
  ).map(MODEL.TRIPLE)

  /*
  subject ::= IRIREF | BLANK_NODE_LABEL
   */
  def _subject[_: P]: P[MODEL.SUBJECT] = P(
    IRIREF | BLANK_NODE_LABEL
  ).map(MODEL.SUBJECT)

  /*
  predicate ::= IRIREF
   */
  def _predicate[_: P]: P[MODEL.PREDICATE] = P(
    IRIREF
  ).map(MODEL.PREDICATE)

  /*
  object ::= IRIREF | BLANK_NODE_LABEL | literal
   */
  def _object[_: P]: P[MODEL.OBJECT] = P(
    IRIREF | BLANK_NODE_LABEL | _literal
  ).map(MODEL.OBJECT)

  /*
  literal ::= STRING_LITERAL_QUOTE ('^^' IRIREF | LANGTAG)?
   */
  def _literal[_: P]: P[MODEL.LITERAL] = P(
    STRING_LITERAL_QUOTE ~ (("^^" ~ IRIREF) | LANGTAG).?
  ).map(MODEL.LITERAL)

  /* PRODUCTION
  ----------------------------------------------------------------------------------------------------------------
   */

  /*
  LANGTAG ::= '@' [a-zA-Z]+ ('-' [a-zA-Z0-9]+)*
   */
  def LANGTAG[_: P]: P[MODEL.LANG] = P(
    "@" ~ (CharIn("a-zA-Z").rep(1) ~ ("-" ~ CharIn("a-zA-Z0-9").rep(1)).rep).!
  ).map(MODEL.LANG)

  /*
  EOL ::= [#xD#xA]+
   */
  def EOL[_: P]: P[Unit] = P(
    CharIn("\u000D\u000A").rep( 1)
  )

  /*
  IRIREF ::= '<' ([^#x00-#x20<>"{}|^`\] | UCHAR)* '>'
   */
  def IRIREF[_: P]: P[MODEL.IRI] = P(
    "<" ~ ( !CharIn("\u0000\u0020<\\>\"{}^`") ~ ( AnyChar | UCHAR ) ).rep.! ~ ">"
  ).map(MODEL.IRI)

  /*
  STRING_LITERAL_QUOTE	::=	'"' ([^#x22#x5C#xA#xD] | ECHAR | UCHAR)* '"'
   */
  def STRING_LITERAL_QUOTE[_: P]: P[java.lang.String] = P(
    "\"" ~ ( !CharIn("\"\\\u000D\u000A") ~ ( AnyChar | ECHAR | UCHAR)).rep.! ~ "\""
  )

  /*
  BLANK_NODE_LABEL ::= '_:' (PN_CHARS_U | [0-9]) ((PN_CHARS | '.')* PN_CHARS)?
   */
  def BLANK_NODE_LABEL[_: P]: P[MODEL.BLANK_NODE] = P(
    "_:" ~ ( ( PN_CHARS_U | CharIn("0-9") ) ~ ( ".".? ~ PN_CHARS ).rep ).!
  ).map(MODEL.BLANK_NODE)

  /*
  UCHAR ::= \\u HEX HEX HEX HEX | \\U HEX HEX HEX HEX HEX HEX HEX HEX
   */
  def UCHAR[_: P]: P[Unit] = P(
    ("\\u" ~ HEX.rep(exactly =4 )) | ("\\U" ~ HEX.rep(exactly = 6))
  )

  /*
  ECHAR ::= '\' [tbnrf"'\]
   */
  def ECHAR[_: P]: P[Unit] = P(
    "\\" ~ CharIn("tbnrf\"\\'")
  )

  /*
  PN_CHARS_BASE ::= [A-Z] | [a-z] | [#x00C0-#x00D6] | [#x00D8-#x00F6] |
  [#x00F8-#x02FF] | [#x0370-#x037D] | [#x037F-#x1FFF] |
  [#x200C-#x200D] | [#x2070-#x218F] | [#x2C00-#x2FEF] |
  [#x3001-#xD7FF] | [#xF900-#xFDCF] | [#xFDF0-#xFFFD] |
  TODO [#x10000-#xEFFFF] ::= CharIn("\u10000-\uEFFFF")
   */
  def PN_CHARS_BASE[_:P]: P[Unit] = P(
    CharIn("A-Z") | CharIn("a-z") | CharIn("\u00C0-\u00D6") | CharIn("\u00D8-\u00F6") |
      CharIn("\u00F8-\u02FF") | CharIn("\u0370-\u037D") | CharIn("\u037F-\u1FFF") |
      CharIn("\u200C-\u200D") | CharIn("\u2070-\u218F") | CharIn("\u2C00-\u2FEF") |
      CharIn("\u3001-\uD7FF") | CharIn("\uF900-\uFDCF") | CharIn("\uFDF0-\uFFFD")
  )

  /*
  PN_CHARS_U ::= PN_CHARS_BASE | '_' | ':'
   */
  def PN_CHARS_U[_: P]: P[Unit] = P(
    PN_CHARS_BASE | "_" | ":"
  )

  /*
  PN_CHARS ::= PN_CHARS_U | '-' | [0-9] | #x00B7 | [#x0300-#x036F] | [#x203F-#x2040]
   */
  def PN_CHARS[_: P]: P[Unit] = P(
    PN_CHARS_U | "-" | CharIn("0-9") | "\u00B7" | CharIn("\u0300-\u036F") | CharIn("\u203F-\u2040")
  )

  /*
  HEX ::= [0-9] | [A-F] | [a-f]
   */
  def HEX[_: P]: P[Unit] = P(
    CharIn("0-9a-fA-F").rep
  )
}