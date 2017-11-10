package edu.towson.cosc.cosc455.rjoaquim.project1

class MySemanticAnalyzer {
  /**Flip Stack
    *
    * This method takes the original parse tree and flips it so that the top of
    * the new parse tree will be the begining of the new document.
    * Also this method takes care of the variable usages and declarations
    */
  def flipStack() : Unit = {
    while (!(Compiler.parseTree.isEmpty)){
      if(Compiler.parseTree.top.equalsIgnoreCase(CONSTANTS.USEB)){
        Compiler.parseTree.pop()
        val find =Compiler.flipedStack.pop()
        Compiler.flipedStack.pop()
        findDef(find)
      }
      else if(Compiler.parseTree.top.equalsIgnoreCase((CONSTANTS.DEFB))){
        Compiler.parseTree.pop()
        for(i<-0 to 2){
          Compiler.flipedStack.pop()
        }
      }
      else
        Compiler.flipedStack.push(Compiler.parseTree.pop())
    }
  }

  /**find a variable being used
    *
    * Receives the variable that is being used and searches through the stack to find
    * where the variable was declared.
    * If found, the variable is added onto the new stack in place of the variable use declaration
    * else an error is thrown out
    */
  def findDef(find:String) : Unit = {
    while(!(Compiler.parseTree.top.equalsIgnoreCase(CONSTANTS.DEFB))){

      if(Compiler.parseTree.top.equalsIgnoreCase(CONSTANTS.PARAE)){
        while(!(Compiler.parseTree.top.equalsIgnoreCase(CONSTANTS.PARAB))) {
          Compiler.holder.push(Compiler.parseTree.pop())
        }
      }
      Compiler.holder.push(Compiler.parseTree.pop())
      if(Compiler.parseTree.isEmpty){
        println("Semantic error- The variable " + find + " is trying to be used but was not declared")
        System.exit(1)
      }
    }
    if(Compiler.holder.top.equals(find)){
      Compiler.parseTree.push(Compiler.holder.pop())
      Compiler.parseTree.push(Compiler.holder.pop())
      Compiler.flipedStack.push(Compiler.holder.top)
      while(!(Compiler.holder.isEmpty)){
        Compiler.parseTree.push(Compiler.holder.pop())
      }
    }
    else {
      Compiler.holder.push(Compiler.parseTree.pop())
      findDef(find)
    }
  }
  /*def removeWhiteSpace(word : String): String ={
    if(CONSTANTS.whiteSpace contains(word.last.toString)){
      removeWhiteSpace(word.dropRight(1))
    }
    word
  }*/

  def converToHTML():Unit ={
    while (!(Compiler.flipedStack.isEmpty)){
      if(Compiler.flipedStack.top.equalsIgnoreCase(CONSTANTS.DOCB)){
        Compiler.fileContents += "<html>"
        Compiler.fileContents += "<head>"
        Compiler.flipedStack.pop()
      }
      else if(Compiler.flipedStack.top.equalsIgnoreCase(CONSTANTS.DOCE)){
        Compiler.fileContents += "</html>"
        Compiler.flipedStack.pop()
      }
      else if(Compiler.flipedStack.top.equalsIgnoreCase(CONSTANTS.TITLEB)){
        Compiler.fileContents += "<title>"
        Compiler.flipedStack.pop()
        Compiler.fileContents += Compiler.flipedStack.pop()
        Compiler.fileContents += "</title>"
        Compiler.flipedStack.pop()
        Compiler.fileContents += "</head>"
      }
      else if(Compiler.flipedStack.top.equalsIgnoreCase(CONSTANTS.HEADING)){
        Compiler.fileContents += "<h1>"
        Compiler.flipedStack.pop()
        Compiler.fileContents += Compiler.flipedStack.pop()
        Compiler.fileContents += "</h1>"
        Compiler.flipedStack.pop()
      }
      else if(Compiler.flipedStack.top.equalsIgnoreCase(CONSTANTS.PARAB)){
        Compiler.fileContents += "<p>"
        Compiler.flipedStack.pop()
      }
      else if(Compiler.flipedStack.top.equalsIgnoreCase(CONSTANTS.PARAE)){
        Compiler.fileContents += "</p>"
        Compiler.flipedStack.pop()
      }
      else if(Compiler.flipedStack.top.equalsIgnoreCase(CONSTANTS.BOLD)){
        Compiler.fileContents += "<b>"
        Compiler.flipedStack.pop()
        Compiler.fileContents += Compiler.flipedStack.pop()
        Compiler.fileContents += "</b>"
        Compiler.flipedStack.pop()
      }
      else if(Compiler.flipedStack.top.equalsIgnoreCase(CONSTANTS.LISTITEM)){
        Compiler.fileContents += "<li>"
        Compiler.flipedStack.pop()
        Compiler.fileContents += Compiler.flipedStack.pop()
        Compiler.fileContents += "</li>"
        Compiler.flipedStack.pop()
      }
      else if(Compiler.flipedStack.top.equalsIgnoreCase(CONSTANTS.NEWLINE)){
        Compiler.fileContents += "<br>"
        Compiler.flipedStack.pop()
      }
      else if(Compiler.flipedStack.top.equalsIgnoreCase(CONSTANTS.LINKB)){
        Compiler.flipedStack.pop()
        val a = Compiler.flipedStack.pop()
        Compiler.flipedStack.pop()
        Compiler.flipedStack.pop()
        Compiler.fileContents += "<a href= " + Compiler.flipedStack.pop() +"> " + a + "</a>"
        Compiler.fileContents += Compiler.flipedStack.pop()

      }
      else if(Compiler.flipedStack.top.equalsIgnoreCase(CONSTANTS.IMAGEB)){
        Compiler.flipedStack.pop()
        val a = Compiler.flipedStack.pop()
        Compiler.flipedStack.pop()
        Compiler.flipedStack.pop()
        Compiler.fileContents += "<img src= " + Compiler.flipedStack.pop() + " alt= " + a + ">"
        Compiler.fileContents += Compiler.flipedStack.pop()
      }
      else{
        Compiler.fileContents += Compiler.flipedStack.pop()
      }
    }
  }
}