package edu.towson.cosc.cosc455.rjoaquim.project1

class MyLexicalAnalyzer extends LexicalAnalyzer {
  /**addChar
    *
    * This method takes in a Char and adds it to the current token
    */
  override def addChar(char: Char): Unit = {
    Compiler.currentToken += char
  }

  /**getNextToken
    *
    * This method is used for finding the next token
    * It is called by the Syntax analyzer when forming a parse tree
    */
  override def getNextToken(): Unit = {
    Compiler.currentToken = ""
    var c = getChar()
    while(CONSTANTS.whiteSpace contains(c.toString)){
      c = getChar()
    }
    if(CONSTANTS.keyWord contains(c)){
      if(!lookup(c)){
        println("Lexical error- " + Compiler.currentToken + " is not a legal token")
        System.exit(1)
      }
    }
    else if(CONSTANTS.validText contains(c.toString.toLowerCase())) {
      addChar(c)
      while (!(CONSTANTS.keyWord contains Compiler.fileContents.charAt(0))) {
        addChar(getChar())
      }
    }
    else{
      println("Lexical Error- " + c + " is not a legal token")
      System.exit(1)
    }
  }

  /**getChar
    *
    * returns the first char from Compiler.fileContents
    */
  override def getChar(): Char = {
 //   println(Compiler.fileContents)
    if(Compiler.fileContents == ""){
      println("Lexical error- Out of Chars")
      System.exit(1)
    }
    var a = Compiler.fileContents.toList
    Compiler.fileContents = a.tail.mkString
    a.head
  }

  /**lookup
    *
    * Takes in a keyWord char and returns a Boolean
    * true or false depending on if it is a legal token
    */
  override def lookup(c : Char): Boolean = {
    var i = 0
    addChar(c)
    if(Compiler.currentToken.equalsIgnoreCase("\\")){
      addChar(getChar())
      /**must be new line token*/
      if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.NEWLINE)){
        true
      }
      /**must be begin doc token*/
      else if( Compiler.currentToken.equalsIgnoreCase("\\b")){
        for(i<- 0 to 3){
          addChar(getChar())
        }
        Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOCB)
      }
      /**must be end of doc token*/
      else if( Compiler.currentToken.equalsIgnoreCase("\\e")){
        for(i<- 0 to 1){
          addChar(getChar())
        }
        Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOCE)
      }
      /**must be beginning of title token*/
      else if(Compiler.currentToken.equalsIgnoreCase("\\t")){
        for(i<- 0 to 4){
          addChar(getChar())
        }
        Compiler.currentToken.equalsIgnoreCase(CONSTANTS.TITLEB)
      }
      /**must be the beginning of def token*/
      else if(Compiler.currentToken.equalsIgnoreCase("\\d")){
        for(i<- 0 to 2){
          addChar(getChar())
        }
        Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DEFB)
      }
      /**must be the beginning of use token*/
      else if(Compiler.currentToken.equalsIgnoreCase("\\u")){
        for(i<- 0 to 2){
          addChar(getChar())
        }
        Compiler.currentToken.equalsIgnoreCase(CONSTANTS.USEB)
      }
      /**must be the beginning of parab or parae token*/
      else if(Compiler.currentToken.equalsIgnoreCase("\\p")){
        for(i<- 0 to 3){
          addChar(getChar())
        }
        /**must be the beginning of parab token*/
        if(Compiler.currentToken.equalsIgnoreCase("\\parab"))
          Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARAB)
        /**must be the beginning of parae token*/
        else if(Compiler.currentToken.equalsIgnoreCase("\\parae"))
          Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARAE)
        else
          false
      }
      /**must be a \ followed by an illegal char*/
      else
        false
    }
    /**must be the beginning of image token*/
    else if (c == '!'){
      //val a = getChar()
      addChar(getChar())
      if(Compiler.currentToken.equalsIgnoreCase("!["))
        true
      else
        false
    }
    /**must be ], #, *, (, ), or = token*/
    else
      true
  }
}
