abstract class JSON
case class JSeq(elems: List[JSON]) extends JSON
case class JObj(bindings: Map[String, JSON]) extends JSON
case class JNum(num: Double) extends JSON
case class JStr(str: String) extends JSON
case class JBool(b: Boolean) extends JSON
case object JNull extends JSON

val data = JObj(Map(
    "firstName" -> JStr("John"),
    "lastName" -> JStr("Smith"),
    "address" -> JObj(Map(
        "streetAddress" -> JStr("21 2nd Street"),
        "state" -> JStr("NY"),
        "postalCode" -> JNum(10021)
    )),
    "phoneNumbers" -> JSeq(List(
        JObj(Map(
            "type" -> JStr("Home"), "number" -> JStr("212 555-1234")
        )),
        JObj(Map(
            "type" -> JStr("work"), "number" -> JStr("656 555-4321")
        ))
    ))
))

def show(json: JSON): String = json match {
    case JSeq(elems) => "[" + (elems map show mkString ", ") + "]"
    case JObj(bindings) =>
        val assocs = bindings map {
            case (key, value) => "\"" + key + "\": " + show(value)
        }
        "{" + (assocs mkString ", ") + "}"
    case JNum(num) => num.toString
    case JStr(str) => "\"" + str + "\""
    case JBool(b) => b.toString
    case JNull => "null"
}

println(show(data))


for {
    JObj(bindings) <- List(data)
    JSeq(phones) = bindings("phoneNumbers")
    JObj(phone) <- phones
    JStr(digits) = phone("number")
    if digits startsWith "212"
} yield (bindings("firstName"), bindings("lastName"))

