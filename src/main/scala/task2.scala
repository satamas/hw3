import scala.collection.mutable.ArrayBuffer
import scala.util.parsing.combinator.JavaTokenParsers
import scala.io.Source
import scala.collection.mutable

/**
 * Created by atamas on 14.10.14.
 */

abstract class JSONNode{}

class JSONString(_value: String) extends JSONNode{
  val value = _value

  override def toString() = {
    value
  }
}

class JSONObject extends JSONNode{
  private val fields = mutable.Map[String, JSONNode]()

  def ++=( field : List[(String, JSONNode)] ): JSONObject = {
    fields ++= field
    this
  }

  def apply(fieldName:String): JSONNode = {
    fields(fieldName)
  }

  override def toString() = {
    val res: StringBuilder = new StringBuilder
    res += '{'
    for((name, value) <- fields){
      res ++= name += ':' ++= value.toString += ','
    }
    if(res.endsWith(",")){
      res.deleteCharAt(res.size - 1)
    }
    res +='}'
    res.toString()
  }
}

class JSONArray() extends JSONNode{
  private var elements: ArrayBuffer[JSONNode] = ArrayBuffer[JSONNode]()

  def +=(element: List[JSONNode]): JSONArray ={
    elements ++= element
    this
  }

  def apply(ind: Int): JSONNode ={
    elements(ind)
  }

  override def toString() = {
    val res: StringBuilder = new StringBuilder
    res += '['
    for(element <- elements){
      res ++= element.toString += ','
    }
    if(res.endsWith(",")){
      res.deleteCharAt(res.size - 1)
    }
    res +=']'
    res.toString()
  }
}

object JSONParser extends JavaTokenParsers{
  private def jsonObject: Parser[JSONObject] = "{" ~> repsep(jsonProperty, ",") <~ "}" ^^ (new JSONObject() ++= _)

  private def jsonProperty: Parser[(String, JSONNode)] = stringLiteral ~ ":" ~ (jsonString | jsonObject | jsonArray) ^^ { case name ~ ":" ~ value => (name, value)}

  private def jsonString: Parser[JSONString] = (stringLiteral | floatingPointNumber ^^ (_.toString)) ^^ (new JSONString(_))

  private def jsonArray: Parser[(JSONArray)] = "[" ~> repsep(jsonString | jsonObject, ",") <~ "]" ^^ (new JSONArray() += _)

  def parse(input: String) = parseAll(jsonObject | jsonArray, input)
}

object task2 extends App{
  val json = Source.fromFile("1.json").mkString
  val res = JSONParser.parse(json)
  println(res.get)
}
