trait Monad[T] {
    def flatMap[U](f: T => Monad[U]): Monad[U] // bind
}
def unit[T](x: T): Monad[T]


m map f == m flatMap (f andThen unit)

// monoid - simpler monad, doesn't bind anything
// 3 monad laws

// associativity

(m flatMap f) flatMap g == m flatMap (x => f(x) flatMap g)

// left unit

unit(x) flatMap f == f(x)

// right unit

m flatMap unit == m


