// Recursion is the GOTO of functional programming. Erik Meijer
//
def retry(nTimes: Int)(block: => Future[T]): Future[T] = {
    if (nTimes == 0) {
        Future.failed(new Exception("Sorry"))
    } else {
        block fallbackTo {
            retry(nTimes - 1) { block }
        }
    }
}

// folds
// foldRight(seed)(function)
// foldLeft(seed)(function)

List(a,b,c).foldRight(e)(f) =
f(a, f(b, f(c, e)))
 <--------------                          Northern wind blows from the north. (Richard Bird)

List(a,b,c).foldLeft(e)(f) =
f(f(f(e,a), b), c)
  ------------->

// foldLeft:
// retry(3) { block } =
// ((failed recoverWith block1) recoverWith block2) recoverWith block3

def retry(nTimes: Int)(block: => Future[T]): Future[T] = {
    val ns: Iterator[Int] = (1 to nTimes).iterator
    val attempts: Iterator[Future[T]] = ns.map(_ => ()=>block)
    val failed = Future.failed(new Exception)

    attempts.foldLeft(failed) ((a, block) => a recoverWith { block() })
}

// foldRight:
// retry(3) { block } =
// block1 fallbackTo { block2 fallbackTo { block3 fallbackTo { failed }}}

def retry(nTimes: Int)(block: => Future[T]): Future[T] = {
    val ns: Iterator[Int] = (1 to nTimes).iterator
    val attempts: Iterator[Future[T]] = ns.map(_ => ()=>block) // ()=>xxx function from Unit to Future[T]
    val failed = Future.failed(new Exception)

    // a() reduces function to Future[T] lazily
    attempts.foldRight(() => failed) ((block, a) => ()=>{ block() fallbackTo { a() }}) // subtle
}

