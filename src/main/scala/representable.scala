trait Representable[F[_]]:
    def F: Functor[F]
    type Representation
    def index[A](f: F[A]): Representation => A
    def tabulate[A](f: Representation => A): F[A]

type Triple[A] = (A, A, A)
enum OneTwoThree:
    case One
    case Two
    case Three
import OneTwoThree._
given Representable[Triple] with
    def F: Functor[Triple] = ???
    type Representation = OneTwoThree
    def index[A](f: Triple[A]): OneTwoThree => A =
        case One => f._1
        case Two => f._2
        case Three => f._3
    def tabulate[A](f: OneTwoThree => A): Triple[A] = (f(One), f(Two), f(Three))

    /*
trait FunctorK[U[f[_]]]:
    extension [F[_], G[_]] (obj: U[F])
        def mapK (f: [A] => F[A] => G[A]): U[G]
trait PureK[U[f[_]]] extends FunctorK[U]:
    def pureK[F[_]](gen: [A] => () => F[A]): U[F]
trait ApplyK[U[f[_]]] extends FunctorK[U]:
    extension [F[_], G[_], H[_]] (left: U[F])
        def map2K(right: U[G])(f: [A] => (F[A], G[A]) => H[A]): U[H]
    */
// trait RepresentableK[U[f[_]]]:
//     type Representation
//     def index[F[_]](f: U[F]): [A] => Representation => F[A]
//     def tabulate[F[_]](f: [A] => Representation => F[A]): U[F]

// given RepresentableK[PersonOf] with
//     type Representation[G[_]] = PersonOf[G[_]]
//     def index[F[_]](f: PersonOf[F]): [A] => Representation => F[A] = ???
//     def tabulate[F[_]](f: [A] => Representation => F[A]): PersonOf[F] = ???

// say want to go from PersonOf[Option] to PersonOf[Either[Unit, *]]
// [A] => Option[A] => Either[Unit, A]
// type Rep[-U[f[_]], A] = [F[_]] => U[F] => F[A]

/*
type FunctionK[A[_], B[_]] = [Z] => A[Z] => B[Z]
type ~>:[A[_], B[_]] = FunctionK[A, B]

trait RepresentableK[F[_[_], _]]:
  type RepresentationK[_]

  def tabulateK[A[_], C](f: RepresentationK ~>: A): F[A, C]

  extension[A[_], C](fa: F[A, C])
    def indexK: RepresentationK ~>: A

    def mapK[B[_]] (f: A ~>: B): F[B, C] =
      tabulateK([Z] => (r: RepresentationK[Z]) => f(indexK(r)))
*/
// @main
def repmain =
    val indexed: OneTwoThree => String = summon[Representable[Triple]].index(("foo", "bar", "bang"))
    println(indexed(One))
    println(indexed(Two))
    val tabbed = summon[Representable[Triple]].tabulate(indexed)
    println(tabbed)
