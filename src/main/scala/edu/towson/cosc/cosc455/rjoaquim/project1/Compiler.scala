package edu.towson.cosc.cosc455.rjoaquim.project1
import scala.collection.mutable.Stack

object Compiler{

  var currentToken : String = ""
  var fileContents : String = ""
  var parseTree = Stack[String]()
  var flipedStack = Stack[String]()
  var holder = Stack[String]()

  val Scanner = new MyLexicalAnalyzer
  val Parser = new MySyntaxAnalyzer
  val SemanticAnalyzer = new MySemanticAnalyzer

  def main(args: Array[String]): Unit = {
    checkFile(args)
    readFile(args(0))
    Scanner.getNextToken
    Parser.gittex()
    SemanticAnalyzer.flipStack()
    SemanticAnalyzer.converToHTML()
    /*while(fileContents != ""){
      println(currentToken)
      Scanner.getNextToken()
    }
    println(currentToken)*/
    println("DOne")
  }

  def readFile(file : String) = {
    val source = scala.io.Source.fromFile(file)
    fileContents = try source.mkString finally source.close()
  }

  def checkFile(args : Array[String]) = {
    if (args.length != 1) {
      println("USAGE ERROR: wrong number of args fool!")
      System.exit(1)
    }
    else if (! args(0).endsWith(".gtx")) {
      println("USAGE ERROR: wrong extension fool!")
      System.exit(1)
    }
  }
}