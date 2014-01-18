val f1: PartialFunction[String, String] = { case "ping" => "pong" }
println(f1.isDefinedAt("ping"))
println(f1.isDefinedAt("pong"))


val f: PartialFunction[List[Int], String] = {
    case Nil => "one"
    case x :: y :: rest => "two"
}

println(f.isDefinedAt(List(1,2,3)))

val g: PartialFunction[List[Int], String] = {
    case Nil => "one"
    case x :: rest =>
        rest match {
            case Nil => "two"
        }
}

println(f.isDefinedAt(List(1,2,3)))
println(g.isDefinedAt(List(1,2,3)))
