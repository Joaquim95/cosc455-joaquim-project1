package edu.towson.cosc.cosc455.rjoaquim.project1
import scala.collection.mutable.Stack
import java.awt.Desktop
import java.io.{File, IOException}
import java.io._

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
    val writer = new PrintWriter(new File("tester.html" ))

    writer.write(fileContents)
    writer.close()
    openHTMLFileInBrowser("tester.html")
  }

  /**Read file
    * Reads a file and saves it as a string variable
    */
  def readFile(file : String) = {
    val source = scala.io.Source.fromFile(file)
    fileContents = try source.mkString finally source.close()
  }

  /**Check File
    * Checks if a file has the correct number of ars and if it has the .gtx extension
    */
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

  /**Open HTML File In Browser
    * Takes an html file and opens it in a browser
    */
  def openHTMLFileInBrowser(htmlFileStr : String) = {
    val file : File = new File(htmlFileStr.trim)
    println(file.getAbsolutePath)
    if (!file.exists())
      sys.error("File " + htmlFileStr + " does not exist.")

    try {
      Desktop.getDesktop.browse(file.toURI)
    }
    catch {
      case ioe: IOException => sys.error("Failed to open file:  " + htmlFileStr)
      case e: Exception => sys.error("He's dead, Jim!")
    }
  }
}