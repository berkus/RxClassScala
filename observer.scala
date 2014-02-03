when getting a result of a Future[T]

import scala.concurrent._

trait Future[T] {
    def onComplete(callback: Observer[T]): Unit
}

trait Observer[T] {
    def onNext(value: T): Unit
    def onError(error: Throwable): Unit
}
