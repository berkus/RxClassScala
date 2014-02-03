import scala.concurrent.duration._
import akka.actor.Actor
import akka.pattern.pipe

class WebClient
{
    val client = new AsyncHttpClient
    def get(url: String)(implicit exec: Executor): Future[String] = {
        val f = client.prepareGet(url).execute()
        val p = Promise[String]()
        f.addListener(new Runnable {
            def run = {
                val response = f.get
                if (response.getStatusCode < 400)
                    p.success(response.getResponseBodyExcerpt(131072))
                else
                    p.failure(BadStatus(response.getStatusCode))
            }
        }, exec)
        p.future
    }
}

class Getter(url: String, depth: Int) extends Actor
{
    implicit val exec = context.dispatcher.asInstanceOf[Executor with ExecutionContext]
    // val future = WebClient.get(url)
    // future onComplete {
    //     case Success(body) => self ! body
    //     case Failure(err)  => self ! Status.Failure(err)
    // }
    // future.pipeTo(self)

    WebClient get url pipeTo self

    def receive = {
        case body: String =>
            for (link <- findLinks(body))
                context.parent ! Controller.Check(link, depth)
            context.stop(self)
        case _: Status.Failure => context.stop(self)
    }
}

class Controller extends Actor with ActorLogging
{
    var cache = Set.empty[String]
    context.setReceiveTimeout(10.seconds)

    override val supervisorStrategy = OneForOneStrategy(maxNrOfRetries = 5) {
        case _: Exception => SupervisorStrategy.Restart
    }
    def receive = {
        case Check(url, depth) =>
            log.debug("{} checking {}", depth, url)
            if (!cache(url) && depth > 0) {
                cache += url
                context.watch(context.actorOf(getterProps(url, depth-1)))
            }
        case Terminated(_) =>
            if (context.children.isEmpty) context.parent ! Result(cache)
        case ReceiveTimeout =>
            context.children foreach context.stop
    }
}

class Receptionist extends Actor
{
    def receive = waiting

    override def supervisorStrategy = SupervisorStrategy.stoppingStrategy

    val waiting: Receive = {
        case Get(url) => context.become(runNext(Vector(Job(sender, url))))
    }

    def running(queue: Vector[Job]): Receive = {
        case Controller.Result(links) =>
            val job = queue.head
            job.client ! Result(job.url, links)
            context.stop(context.unwatch(sender))
            context.become(runNext(queue.tail))
        case Terminated(_) =>
            val job = queue.head
            job.client ! Failed(job.url)
            context.become(runNext(queue.tail))
        case Get(url) =>
            context.become(enqueueJob(queue, Job(sender, url)))
    }

    case class Job(client: ActorRef, url: String)
    var reqNo = 0
    def runNext(queue: Vector[Job]): Receive = {
        reqNo += 1
        if (reqNo == 3) context.stop(self) // death pact test
        if (queue.isEmpty) waiting
        else {
            var controller = context.actorOf(Props[Controller], s"c$reqNo")
            context.watch(controller)
            controller ! Controller.Check(queue.head.url, 2)
            running(queue)
        }
    }
    def enqueueJob(queue: Vector[Job], job: Job): Receive = {
        if (queue.size > 3) {
            sender ! Failed(job.url)
            running(queue)
        } else running(queue :+ job)
    }
}

class Main extends Actor
{
    import Receptionist._

    val receptionist = context.actorOf(Props[Receptionist], "receptionist")
    context.watch(receptionist) // sign death pact

    receptionist ! Get("http://exquance.com")

    context.setReceiveTimeout(10.seconds)

    def receive = {
        case Result(url, set) =>
            println(set.toVector.sorted.mkString(s"Results for '$url':\n", "\n", "\n"))
        case Failed(url) =>
            println(s"Failed to fetch '$url'\n")
        case ReceiveTimeout =>
            context.stop(self)
    }

    override def postStop(): Unit = {
        WebClient.shutdown()
    }
}
