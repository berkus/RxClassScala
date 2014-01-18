
class Wire {
    private var sigVal = false
    private var actions: List[Action] = List()
    def getSignal: Boolean = sigVal
    def setSignal(s: Boolean): Unit =
        if (s != sigVal) {
            sigVal = s
            actions foreach (_())
        }
    def addAction(a: Action): Unit = {
        actions = a :: actions
        a()
    }
}

trait Simulation {
    type Action = () => Unit
    case class Event(time: Int, action: Action)
    private type Agenda = List[Event]
    private var agenda: Agenda = List()
    private var curtime = 0
    def currentTime: Int = curtime
    def afterDelay(delay: Int)(block: => Unit): Unit = {
        val item = Event(currentTime + delay, () => block)
        agenda = insert(agenda, item)
    }
    private def insert(ag: Agenda, item: Event): Agenda = ag match {
        case first :: rest if first.time <= item.time => first :: insert(rest, item)
        case _ => item :: ag
    }
    private def loop(): Unit = agenda match {
        case first :: rest =>
            agenda = rest
            curtime = first.time
            first.action()
            loop()
        case Nil =>
    }
    def run(): Unit = {
        afterDelay(0) {
            println(s"*** Simulation started, time = $currentTime ***")
        }
        loop()
        println(s"*** Simulation ended, time = $currentTime ***")
    }
    def probe(name: String, wire: Wire): Unit = {
        def probeAction(): Unit = {
            println(s"$name $currentTime value = ${wire.getSignal}");
        }
        wire addAction probeAction
    }
}

trait Gates extends Simulation = {
    def inverter(input: Wire, output: Wire): Unit = {
        def invertAction(): Unit = {
            val inputSig = input.getSignal
            afterDelay(InverterDelay) { output setSignal !inputSig }
        }
        input addAction invertAction
    }

    def andGate(i1: Wire, i2: Wire, output: Wire): Unit = {
        def andAction(): Unit = {
            val in1 = i1.getSignal
            val in2 = i2.getSignal
            afterDelay(AndGateDelay) { output setSignal (in1 & in2) }
        }
        i1 addAction andAction
        i2 addAction andAction
    }

    def orGate(i1: Wire, i2: Wire, output: Wire): Unit = {
        def orAction(): Unit = {
            val in1 = i1.getSignal
            val in2 = i2.getSignal
            afterDelay(OrGateDelay) { output setSignal (in1 | in2) }
        }
        i1 addAction orAction
        i2 addAction orAction
    }
}

trait Circuits extends Gates = {
    def halfAdder(a: Wire, b: Wire, sum: Wire, cout: Wire): Unit = {
        val d, e = new Wire
        orGate(a, b, d)
        andGate(a, b, cout)
        inverter(cout, e)
        andGate(d, e, sum)
    }

    def fullAdder(a: Wire, b: Wire, cin: Wire, sum: Wire, cout: Wire): Unit = {
        val s, c1, c2 = new Wire
        halfAdder(b, cin, s, c1)
        halfAdder(a, s, sum, c2)
        orGate(c1, c2, cout)
    }
}
