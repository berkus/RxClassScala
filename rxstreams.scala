object Observable {
    def apply[T](subscribe: Observer[T] => Subscription): Observable[T]

    // send only one onError signal and that's it
    def apply[T](error: Throwable): Observable[T] =
        Observable[T](observer => {
            observer.onError(error);
            Subscription {}
        })

    def apply[T](f: Future[T]): Observable[T] = {
        val subject = AsyncSubject[T]()
        f onComplete {
            case Failure(e) => { subject.onError(e) }
            case Success(c) => { subject.onNext(c); subject.onCompleted() }
        }
        subject
    }
}

// never send any signals
def never(): Observable[Nothing] = Observable[Nothing](observer => {
    Subscription {}
})

def startWith(ss: T*): Observable[T] = {
    Observable[T](observer => {
        for (s <- ss) observer.onNext(s)
        subscribe(observer)
    })
}

def filter(p: T => Boolean): Observable[T] = {
    Observable[T](observer => {
        subscribe(
            (t: T) => { if (p(t)) observer.onNext(t) },
            (e: Throwable) => { observer.onError(e) },
            () => { observer.onCompleted() }
        )
    })
}

def map[S](f: T => S): Observable[S] = {
    Observable[S](observer => {
        subscribe(
            (t: T) => { observer.onNext(f(t)) },
            (e: Throwable) => { observer.onError(e) },
            () => { observer.onCompleted() }
        )
    })
}

// Subjects to Observables are what Promises to the Futures
// Subjects make cold Observables hot
PublishSubject[T]
ReplaySubject[T]
BehaviorSubject[T]
AsyncSubject[T]


/// Schedulers

// Runnable is Java equivalent of r: => Unit
trait ExecutionContext {
    def execute(runnable: Runnable): Unit
}

trait Scheduler {
    def schedule(work: => Unit): Subscription
    def schedule(work: Scheduler => Subscription): Subscription
    def schedule(work: (=>Unit) => Unit): Subscription
}

