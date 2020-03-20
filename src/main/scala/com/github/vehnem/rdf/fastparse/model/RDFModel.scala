package com.github.vehnem.rdf.fastparse.model

/**
 * @author ${user.name}
 */
object RDFModel {

  sealed trait Val extends Any {
    def value: Any
  }

  /*
  RDF Triples
   */
  case class TRIPLES(value: Val*) extends AnyVal with Val
  case class TRIPLE(value: (SUBJECT, PREDICATE, OBJECT)) extends AnyVal with Val

  /*
  RDF Nodes
   */
  case class SUBJECT(value: Val) extends AnyVal with Val
  case class PREDICATE(value: Val) extends AnyVal with Val
  case class OBJECT(value: Val) extends AnyVal with Val

  /*
  RDF Terms
   */
  case class IRI(value: java.lang.String) extends AnyVal with Val

  case class BLANK_NODE(value: java.lang.String) extends AnyVal with Val

  case class LITERAL(value: (java.lang.String, Option[Val])) extends AnyVal with Val
  case class LANG(value: java.lang.String) extends AnyVal with Val
  case class LADA(value: Val) extends AnyVal with Val
}
