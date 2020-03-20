package com.github.vehnem

import com.github.vehnem.rdf.fastparse.grammar.NTripleGrammar.ntriples
import fastparse.{Parsed, parse}

/**
  * @author ${user.name}
  */
object NTripleParser {

  val exampleNT : String =
    s"""<http://dbpedia.org/resource/Berlin> <http://dbpedia.org/ontology/country> <http://dbpedia.org/resource/Germany> .
       |<http://dbpedia.org/resource/Berlin> <http://dbpedia.org/ontology/name> "Berlin"@en .
       |<http://dbpedia.org/resource/Berlin> <http://dbpedia.org/ontology/abstract> "Berlin (/brˈlɪn/, German: [bɛɐ̯ˈliːn] ) is..." .
       |<http://dbpedia.org/resource/Berlin> <http://dbpedia.org/ontology/populationTotal> "3610156"^^<http://www.w3.org/2001/XMLSchema#integer> .
       |<http://dbpedia.org/resource/Germany> <http://dbpedia.org/ontology/capital> _:blankNode .
       |_:blankNode <http://dbpedia.org/ontology/blank> <http://dbpedia.org/resource/Berlin> .
       |""".stripMargin

  def main(args: Array[String]): Unit = {

    val input = if (args.isEmpty) {
      println(exampleNT)
      exampleNT
    } else {
      //TODO
      print("TODO")
      ""
    }

    val start = System.currentTimeMillis()

    val Parsed.Success(triples, _) = parse(input, ntriples(_))

    val halt = System.currentTimeMillis()

    triples.value.foreach(println(_))
    println()

    val end = System.currentTimeMillis()

    println("parse time ", halt - start)
    println("print time ", end - halt)
    println("total time ", end - start)
  }
}
