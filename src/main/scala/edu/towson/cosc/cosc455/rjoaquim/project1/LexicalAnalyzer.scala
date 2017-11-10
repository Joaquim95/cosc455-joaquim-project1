package edu.towson.cosc.cosc455.rjoaquim.project1

trait LexicalAnalyzer {
  def addChar(char: Char) : Unit
  def getChar() : Char
  def getNextToken() : Unit
  def lookup(char: Char) : Boolean
}
