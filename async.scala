import scala.async.Async._

def async[T](body: => T)(implicit context: ExecutionContext): Future[T]
def await[T](future: Future[T]): T

async {
    ....
    await { ... }
}


def retry(nTimes: Int)(block: => Future[T]): Future[T] = async { // <<-----!!!
    var i = 0
    var result: Try[T] = Failure(new Exception)
    while (i < nTimes && result.isFailure) {
        result = await { Try(block) }
        i += 1
    }
    result.get
}
