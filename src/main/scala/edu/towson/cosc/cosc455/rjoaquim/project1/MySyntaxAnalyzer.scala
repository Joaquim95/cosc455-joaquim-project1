package edu.towson.cosc.cosc455.rjoaquim.project1

class MySyntaxAnalyzer extends SyntaxAnalyzer{
  /**gittex
    *
    * The starting point for parse tree
    * follows the BNF <gittex> ::= DOCB <variable-define> <title> <body> DOCE
    */
  override def gittex(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOCB)){
      // add to parse tree / stack
      Compiler.parseTree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
    }
    else {
      println("Syntax error- " + CONSTANTS.DOCB + " expected, " + Compiler.currentToken + " received")
      System.exit(1)
    }

    variableDefine()

    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.TITLEB)){
      title()
    }
    else{
      println("Syntax error- "+ CONSTANTS.TITLEB + " expected, " + Compiler.currentToken+ " received")
      System.exit(1)
    }
    while(!Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOCE)) {
      body()
    }
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOCE)){
      Compiler.parseTree.push(Compiler.currentToken)
    }
    else{
      println("Syntax error- "+ CONSTANTS.DOCE + " expected, " + Compiler.currentToken+ " received")
      System.exit(1)
    }
    checkEnd()

  }

  /**paragraph
    *
    * Can only be reached by the body method
    * follows the BNF <paragraph> ::= PARAB <variable-define> <inner-text> PARAE
    */
  override def paragraph(): Unit = {
    Compiler.parseTree.push(Compiler.currentToken)
    Compiler.Scanner.getNextToken()

    variableDefine()
    while(!(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARAE))){
      innerText()
    }
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARAE)){
      Compiler.parseTree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
    }
    else {
      println("Syntax error- " + CONSTANTS.PARAE + " expected, " + Compiler.currentToken + " received")
      System.exit(1)
    }
  }

  /**Inner Item
    *
    * Reachable from list item
    * Follows the BNF <inner-item> ::= <variable-use> <inner- item>
    *| <bold> <inner- item>
    *| <link> <inner- item>
    *| REQTEXT <inner- item>
    *| ε
    */
  override def innerItem(): Unit = {
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.USEB)) {
        variableUse()
      }
      else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BOLD)) {
        bold()
      }
      else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LINKB)) {
        link()
      }
      else if(CONSTANTS.validText contains Compiler.currentToken.charAt(0).toString.toLowerCase){
          Compiler.parseTree.push(Compiler.currentToken)
          Compiler.Scanner.getNextToken()
      }
  }

  /**Inner text
    *
    * Reachable through body and paragraph
    * follows the BNF <inner-text> ::= <variable-use> <inner-text>
    *| <heading> <inner-text>
    *| <bold> <inner-text>
    *| <listitem> <inner-text>
    *| <image> <inner-text>
    *| <link> <inner-text>
    *| TEXT <inner-text>
    *| ε
    */
  override def innerText(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.USEB)){
      variableUse()
    }
    else if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LISTITEM)){
      listItem()
    }
    else if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BOLD)){
      bold()
    }
    else if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.HEADING)){
      heading()
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.IMAGEB)){
      image()
    }
    else if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LINKB)){
      link()
    }
    else if(CONSTANTS.keyWord contains(Compiler.currentToken.charAt(0))){
      println(Compiler.currentToken + " cannot be entered here")
      System.exit(1)
    }
    else if(CONSTANTS.validText contains Compiler.currentToken.charAt(0).toString.toLowerCase){
      Compiler.parseTree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
    }
  }

  /**Link
    *
    * Reachable through inner item and inner text
    * follows the BNF <link> ::= LINKB REQTEXT BRACKETE ADDRESSB REQTEXT ADDRESSE | ε
    */
  override def link(): Unit = {
    Compiler.parseTree.push(Compiler.currentToken)
    Compiler.Scanner.getNextToken()

    if(CONSTANTS.validText contains Compiler.currentToken.charAt(0).toString.toLowerCase){
      Compiler.parseTree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
    }
    else{
      println("Sytax error- Text is required")
      System.exit(1)
    }

    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE)) {
      Compiler.parseTree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
    }
    else {
      println("Syntax error- " + CONSTANTS.BRACKETE + " expected, " + Compiler.currentToken + " received")
      System.exit(1)
    }

    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ADDRESSB)) {
      Compiler.parseTree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
    }
    else {
      println("Syntax error- " + CONSTANTS.ADDRESSB + " expected, " + Compiler.currentToken + " received")
      System.exit(1)
    }

    if(CONSTANTS.validText contains Compiler.currentToken.charAt(0).toString.toLowerCase){
      Compiler.parseTree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
    }
    else{
      println("Sytax error- Text is required")
      System.exit(1)
    }

    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ADDRESSE)) {
      Compiler.parseTree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
    }
    else {
      println("Syntax error- " + CONSTANTS.ADDRESSE + " expected, " + Compiler.currentToken + " received")
      System.exit(1)
    }
  }

  /**Body
    *
    * Reachable through gittex
    * Follows the BNF <body> ::= <inner-text> <body>
    *| <paragraph> <body>
    *| <newline> <body>
    *| ε
    */
  override def body(): Unit = {
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARAB)) {
        paragraph()
      }
      else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.NEWLINE)) {
        newline()
      }
      else {
        innerText()
      }
  }

  /**Bold
    *
    * Reachable through inner text and inner item
    * Follows the BNF <bold> ::= BOLD TEXT BOLD | ε
    */
  override def bold(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BOLD)){
      Compiler.parseTree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      if(CONSTANTS.validText contains Compiler.currentToken.charAt(0).toString.toLowerCase){
        Compiler.parseTree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
      }
      else{
        println("Sytax error- Text is required")
        System.exit(1)
      }

      if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BOLD)){
        Compiler.parseTree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
      }
      else {
        println("Syntax error- " + CONSTANTS.BOLD + " expected, " + Compiler.currentToken + " received")
        System.exit(1)
      }
    }
  }

  /**New Line
    *
    * Reachable through body
    * Follows the BNF <newline> ::= NEWLINE | ε
    */
  override def newline(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.NEWLINE)){
      Compiler.parseTree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
    }
  }

  /**Title
    *
    * Reachable through gittex
    * Follows the BNF <title> ::= TITLEB REQTEXT BRACKETE
    */
  override def title(): Unit = {
    Compiler.parseTree.push(Compiler.currentToken)
    Compiler.Scanner.getNextToken()

    if(CONSTANTS.validText contains Compiler.currentToken.charAt(0).toString.toLowerCase){
      Compiler.parseTree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
    }
    else{
      println("Sytax error- Text is required")
      System.exit(1)
    }

    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE)){
      Compiler.parseTree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
    }
    else {
      println("Syntax error- " + CONSTANTS.BRACKETE + " expected, " + Compiler.currentToken + " received")
      System.exit(1)
    }
  }

  /**Define Variable
    *
    * Reachable through gittex and paragraph
    * Follows the BNF <variable-define> ::= DEFB REQTEXT EQSIGN REQTEXT BRACKETE <variable-define> | ε
    */
  override def variableDefine(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DEFB)){
      Compiler.parseTree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      if(CONSTANTS.validText contains Compiler.currentToken.charAt(0).toString.toLowerCase){
        Compiler.parseTree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
      }
      else{
        println("Sytax error- Text is required")
        System.exit(1)
      }

      if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.EQSIGN)){
        Compiler.parseTree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
      }
      else{
        println("Syntax error- " + CONSTANTS.EQSIGN + " expected, " + Compiler.currentToken + " received")
        System.exit(1)
      }

      if(CONSTANTS.validText contains Compiler.currentToken.charAt(0).toString.toLowerCase){
        Compiler.parseTree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
      }
      else{
        println("Sytax error- Text is required")
        System.exit(1)
      }

      if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE)){
        Compiler.parseTree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
      }
      else{
        println("Syntax error- " + CONSTANTS.BRACKETE + " expected, " + Compiler.currentToken + " received")
        System.exit(1)
      }
      variableDefine()
    }
  }

  /**Image
    *
    * Reachable through inner text
    * Follows the BNF <image> ::= IMAGEB REQTEXT BRACKETE ADDRESSB REQTEXT ADDRESSE | ε
    */
  override def image(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.IMAGEB)){
      Compiler.parseTree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()

      if(CONSTANTS.validText contains Compiler.currentToken.charAt(0).toString.toLowerCase){
        Compiler.parseTree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
      }
      else{
        println("Sytax error- Text is required")
        System.exit(1)
      }

      if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE)){
        Compiler.parseTree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
      }
      else{
        println("Syntax error- " + CONSTANTS.BRACKETE + " expected, " + Compiler.currentToken + " received")
        System.exit(1)
      }

      if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ADDRESSB)){
        Compiler.parseTree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
      }
      else{
        println("Syntax error- " + CONSTANTS.ADDRESSB + " expected, " + Compiler.currentToken + " received")
        System.exit(1)
      }

      if(CONSTANTS.validText contains Compiler.currentToken.charAt(0).toString.toLowerCase){
        Compiler.parseTree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
      }
      else{
        println("Sytax error- Text is required")
        System.exit(1)
      }

      if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ADDRESSE)){
        Compiler.parseTree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
      }
      else{
        println("Syntax error- " + CONSTANTS.ADDRESSE + " expected, " + Compiler.currentToken + " received")
        System.exit(1)
      }
    }
  }

  /**Variable Use
    *
    * Reachable through inner text
    * Follows the BNF <variable-use> ::= USEB REQTEXT BRACKETE | ε
    */
  override def variableUse(): Unit = {
    Compiler.parseTree.push(Compiler.currentToken)
    Compiler.Scanner.getNextToken()
    if(CONSTANTS.validText contains Compiler.currentToken.charAt(0).toString.toLowerCase){
      Compiler.parseTree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
    }
    else{
      println("Sytax error- Text is required")
      System.exit(1)
    }

    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE)) {
      Compiler.parseTree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
    }
    else {
      println("Syntax error- " + CONSTANTS.BRACKETE + " expected, " + Compiler.currentToken + " received")
      System.exit(1)
    }
  }

  /**Heading
    *
    * Reachable through inner text
    * Follows the BNF <heading> ::= HEADING REQTEXT | ε
    */
  override def heading(): Unit = {
    Compiler.parseTree.push(Compiler.currentToken)
    Compiler.Scanner.getNextToken()
    if(CONSTANTS.validText contains Compiler.currentToken.charAt(0).toString.toLowerCase){
      Compiler.parseTree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
    }
    else{
      println("Sytax error- Text is required")
      System.exit(1)
    }
  }

  /**List Item
    *
    * Reachable recursively and through inner text
    * Follows the BNF <listitem> ::= LISTITEMB <inner-item> <list-item> | ε
    */
  override def listItem(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LISTITEM)) {
      Compiler.parseTree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      innerItem()
    }
  }
  def checkEnd() : Unit = {
    if(Compiler.fileContents != ""){
      for(i<- 0 to Compiler.fileContents.length -1) {
        if (!(CONSTANTS.whiteSpace contains Compiler.fileContents.charAt(i).toString.toLowerCase)) {
          println("Syntax error- " + Compiler.fileContents + " found after document end")
          System.exit(1)
        }
      }
      Compiler.fileContents = ""
    }
  }
}
