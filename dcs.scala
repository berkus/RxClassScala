
abstract class Simulation
{
    type Action = () => Unit

    case class Event(time: Int, action: Action)

    private var curtime = 0
    def currentTime: Int = curtime

    private type Agenda = List[Event]
    private var agenda: Agenda = List()
    private def insert(ag: Agenda, item: Event): Agenda = ag match {
        case first :: rest if first.time <= item.time => first :: insert(rest, item)
        case _ => item :: ag
    }

    def afterDelay(delay: Int)(block: => Unit): Unit = {
        val item = Event(currentTime + delay, () => block)
        agenda = insert(agenda, item)
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
}

abstract class Gates extends Simulation
{
    def InverterDelay: Int
    def AndGateDelay: Int
    def OrGateDelay: Int

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

    // Poor-man's OR gate using only andGate() and inverter()
    def orGateAlt(i1: Wire, i2: Wire, output: Wire): Unit = {
        val notIn1, notIn2, notOut = new Wire
        inverter(i1, notIn1)
        inverter(i2, notIn2)
        andGate(notIn1, notIn2, notOut)
        inverter(notOut, output)
    }

    def probe(name: String, wire: Wire): Unit = {
        def probeAction(): Unit = {
            println(s"$name @$currentTime new-value=${wire.getSignal}");
        }
        wire addAction probeAction
    }
}

abstract class Circuits extends Gates
{
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

trait Parameters {
    def InverterDelay = 2
    def AndGateDelay = 3
    def OrGateDelay = 5
}

object sim extends Circuits with Parameters
import sim._

val in1, in2, sum, carry = new Wire
halfAdder(in1, in2, sum, carry)
probe("sum", sum)
probe("carry", carry)
in1 setSignal true
run()
in2 setSignal true
run()
in1 setSignal false
run()
