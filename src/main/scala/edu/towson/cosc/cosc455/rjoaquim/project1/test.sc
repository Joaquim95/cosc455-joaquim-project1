import com.sun.corba.se.impl.orb.ParserTable.TestAcceptor1
//import com.sun.crypto.provider.AESCipher.AES128_CBC_NoPadding

var list = List('\\','[',']','#','*','+','!','=','(',')')
var list2 = List('A', 'B')
var c = 'A'
println(c + " not")
c = c.toLower
println(c)
println(list)
def lowerlist(list: List[Char]):List[Char] = list.map(e => e.toLower)
list = lowerlist(list)
list2 = lowerlist(list2)
println(list)
println(list2)

var word = "Hello"
/*def getToken(w : String):Unit = {
  val c = get(w)
  println(c)
}
def get(word : String): Char = {
  word.foreach(return _)
}*/
/*def get(word : String): Char ={
  val a = word.toCharArray
  for (i<-0 to a.length) {
    return a[i]
  }
}*/
word += 'C'
println(word)

var word2 = "Hello"
def getToken():Unit = {
  val c = get()
  println(c)
}
def get(): Char = {
  val a = word2.toList
  val c = a.head
   var w = a.tail.mkString
  word2 = w.mkString
  c
}
while(!word2.isEmpty){
  getToken()
}

def equalsIgnoreCase(a:String,b:String) : Boolean = {
  if(a.toLowerCase == b.toLowerCase)
    true
  else
    false
}
val s = "HELLO"
val s2 = "hello"
println(equalsIgnoreCase(s,s2))

var someWord = "Hello"
var newWord = ""
def look(char: Char): Unit ={
  for(i<- 0 to 3){
    var a = getChar()
    addChar(a)
  }
}
def getChar(): Char = {
  val a = someWord.toList
  val c = a.head
  var w = a.tail.mkString
  someWord = w.mkString
  c
}
def addChar(char: Char): Unit ={
  newWord += char

}
look('a')
println(newWord)

var x = "hello"
var y = x.toList
var z = y.head
x = y.tail.mkString

println(y)
println(z)
println(x)

