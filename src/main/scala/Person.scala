// case class Person(
//     name: String, 
//     age: Int, 
//     postcode: String, 
//     nickname: Option[String]
// )

case class PersonF[F[_]](
    name: F[String], 
    age: F[Int], 
    postcode: F[String], 
    nickname: F[Option[String]]
)

type Identity[A] = A
type Person = PersonF[Identity]
type MaybePerson = PersonF[Option]

trait Functor[F[_]]:
  extension [A](fa: F[A]) 
    def map[B](f: A => B): F[B]

given Functor[Identity] with
  extension [A](fa: Identity[A]) 
    def map[B](f: A => B): Identity[B] =
      f(fa) 

given Functor[Option] with
  extension [A](fa: Option[A])
    def map[B](f: A => B): Option[B] =
      fa.map(f) 

type Validation[T] = T => Either[String, T]
type ValidatedPerson = PersonF[Validation]

val postcodeRegex = "ABC"

val personValidations = PersonF[Validation](
    name => Either.cond(name.startsWith("C"), name, "Name does not begin with 'C'"),
    age => Either.cond(age > 0, age, "Age must be greater than 0"),
    postcode => Either.cond(postcode.matches(postcodeRegex), postcode, "Postcode doesn't look right"),
    nickname => Right(nickname)
)

type Validated[F[_]] = [A] =>> F[Either[String, A]]
def validatePersonFSpecific[F[_]: Functor](personF: PersonF[F]): PersonF[Validated[F]] = 
  PersonF(
    personF.name.map(personValidations.name),
    personF.age.map(personValidations.age),
    personF.postcode.map(personValidations.postcode),
    personF.nickname.map(personValidations.nickname)
  )

def validatePersonFCompleteSpecific[F[_]: Traverse](personF: PersonF[F]): Either[PersonF[Validated[F]], PersonF[F]] = 
  val v = validatePersonFSpecific(personF)
  val p = for 
    name <- v.name.sequence
    age <- v.age.sequence
    postcode <- v.postcode.sequence
    nickname <- v.nickname.sequence
  yield PersonF(name, age, postcode, nickname)
  p.fold(_ => Left(v), Right(_))

trait Traverse[F[_]] extends Functor[F]:
  extension [A](fa: F[A])
    def traverse[G[_]: Applicative, B](f: A => G[B]): G[F[B]]
    def map[B](f: A => B): F[B] = fa.traverse[Identity, B](f)
  extension [G[_]: Applicative, A](fga: F[G[A]])
    def sequence: G[F[A]] = fga.traverse(ga => ga)

trait Applicative[F[_]] extends Functor[F]:
  def pure[A](x: A): F[A]
  extension [A](fa: F[A])
    def ap[B](ff: F[A => B]): F[B]
    def map[B](f: A => B): F[B] =
      fa.ap(pure(f))
    def map2[B, C](fb: F[B])(f: (A, B) => C): F[C] =
      fb.ap(fa.map(a => (b: B) => f(a, b)))

given [A]: Applicative[[B] =>> Either[A, B]] with
  def pure[B](x: B): Either[A, B] = Right(x)
  extension [B](fa: Either[A, B])
    def ap[C](ff: Either[A, B => C]): Either[A, C] =
      ff match 
        case Right(f) => fa.map(f)
        case Left(l) => Left(l)

given Traverse[Identity] with Applicative[Identity] with
  def pure[A](x: A): Identity[A] = x
  extension [A](fa: Identity[A])
    override def map[B](f: A => B): Identity[B] = f(fa)
    def ap[B](ff: Identity[A => B]): Identity[B] = ff(fa)
    def traverse[G[_]: Applicative, B](f: A => G[B]): G[Identity[B]] =
      f(fa)

given Traverse[Option] with Applicative[Option] with
  def pure[A](x: A): Option[A] = Some(x)
  extension [A](fa: Option[A])
    override def map[B](f: A => B): Option[B] = fa.map(f)
    def ap[B](ff: Option[A => B]): Option[B] = ff.fold(None)(fa.map(_))
    def traverse[G[_]: Applicative, B](f: A => G[B]): G[Option[B]] =
      fa match 
        case Some(a) => summon[Applicative[G]].map(f(a))(Some(_))
        case None => summon[Applicative[G]].pure(None)

trait FunctorK[U[f[_]]]:
  extension [F[_]](u: U[F])
    def mapK[G[_]](f: [T] => F[T] => G[T]): U[G]

// given FunctorK[PersonF] with
//   extension [F[_]](u: PersonF[F])
//     def mapK[G[_]](f: [T] => (t: F[T]) => G[T]): PersonF[G] = 
//       PersonF(
//         f(u.name), f(u.age), f(u.postcode), f(u.nickname)
//       )

trait ApplyK[U[_[_]]] extends FunctorK[U]:
  extension [F[_]](u: U[F])
    def mapK[G[_]](f: [T] => F[T] => G[T]): U[G] = 
      u.map2K(u)([T] => (t: F[T], _: F[T]) => f(t))
    def map2K[G[_], H[_]](v: U[G])(f: [T] => (F[T], G[T]) => H[T]): U[H]

// given ApplyK[PersonF] with
//   extension [F[_]](u: PersonF[F])
//     def map2K[G[_], H[_]](v: PersonF[G])(f: [T] => (t: F[T], s: G[T]) => H[T]): PersonF[H] = 
//       PersonF(
//         f(u.name, v.name),
//         f(u.age, v.age),
//         f(u.postcode, v.postcode),
//         f(u.nickname, v.nickname),
//       )

import scala.deriving.Mirror
import scala.compiletime.summonFrom

inline def deriveApplyKForCaseClass[U[_[_]] <: Product]: ApplyK[U] = 
  new ApplyK[U] {
    extension [F[_]](u: U[F])
      def map2K[G[_], H[_]](v: U[G])(f: [T] => (F[T], G[T]) => H[T]): U[H] = summonFrom {
        case p: Mirror.ProductOf[U[F]] =>
          p.fromProduct(new Product {
            def canEqual(that: Any): Boolean = true
            def productArity: Int = u.productArity
            def productElement(n: Int): Any =
              f(u.productElement(n).asInstanceOf[F[Any]], v.productElement(n).asInstanceOf[G[Any]])
          }).asInstanceOf[U[H]]
      }
  }

// given ApplyK[PersonF] = deriveApplyKForCaseClass[PersonF]

def validatePersonF[F[_]: Functor](personF: PersonF[F]): PersonF[Validated[F]] = 
  personF.map2K(personValidations)(
    [T] => (value: F[T], validation: Validation[T]) => value.map(validation)
  )

def validateU[U[_[_]]: ApplyK, F[_]: Functor](u: U[F], validations: U[Validation]): U[Validated[F]] =
  u.map2K(validations)(
    [T] => (value: F[T], validation: Validation[T]) => value.map(validation)
  )


trait TraverseK[U[_[_]]] extends FunctorK[U]:
  extension [F[_]](uf: U[F])
    def traverseK[V[_]: Applicative, G[_]](f: [T] => F[T] => V[G[T]]): V[U[G]]
    def mapK[G[_]](f: [T] => F[T] => G[T]): U[G] = 
      traverseK[Identity, G]([T] => (ft: F[T]) => f(ft))

// given TraverseK[PersonF] with 
//   extension [F[_]](uf: PersonF[F])
//     def traverseK[V[_]: Applicative, G[_]](f: [T] => F[T] => V[G[T]]): V[PersonF[G]] = 
//       val name = f(uf.name)
//       val age = f(uf.age)
//       val postcode = f(uf.postcode)
//       val nickname = f(uf.nickname)
      
//       name.map2(age)((a, b)=> (a, b))
//         .map2(postcode){ case ((a, b), c) => (a, b, c) }
//         .map2(nickname){ case ((a, b, c), d) => (a, b, c, d)}
//         .map(PersonF[G])


inline def deriveTraverseKForCaseClass[U[_[_]] <: Product]: TraverseK[U] = 
  new TraverseK[U] {
    extension [F[_]](u: U[F])
      def traverseK[V[_]: Applicative, G[_]](f: [T] => F[T] => V[G[T]]): V[U[G]] = summonFrom {
        case p: Mirror.ProductOf[U[F]] =>
          val appliedF = u.productIterator.toList.map(t => f(t.asInstanceOf[F[Any]]))
          val vTuple: V[Tuple] = appliedF.foldRight(summon[Applicative[V]].pure(EmptyTuple)) { (vgs, x) =>
            x.map2(vgs)((tuple, a) => (a *: tuple) )
          }
          
          vTuple.map { tuple =>
            p.fromProduct(new Product {
              def canEqual(that: Any): Boolean = true
              def productArity: Int = tuple.productArity
              def productElement(n: Int): Any =
                tuple.productElement(n)
            }).asInstanceOf[U[G]]
          }
      }
  }

// given ApplyK[PersonF] = deriveApplyKForCaseClass[PersonF]
// given TraverseK[PersonF] = deriveTraverseKForCaseClass[PersonF]

def validatePersonFComplete[F[_]: Traverse](personF: PersonF[F]): Either[PersonF[Validated[F]], PersonF[F]] = 
  val v = validatePersonF(personF)
  val e = v.traverseK([T] => (x: F[Either[String, T]]) => x.sequence)
  e.fold(_ => Left(v), Right(_))

def validateUComplete[U[_[_]]: TraverseK: ApplyK, F[_]: Traverse](u: U[F], validations: U[Validation]): Either[U[Validated[F]], U[F]] =
  val v = validateU(u, validations)
  val e = v.traverseK([T] => (x: F[Either[String, T]]) => x.sequence)
  e.fold(_ => Left(v), Right(_))

trait ApplyTraverseK[U[_[_]]] extends ApplyK[U], TraverseK[U]:
  extension [F[_]](u: U[F])
    override def mapK[G[_]](f: [T] => F[T] => G[T]): U[G] = 
      u.traverseK[Identity, G]([T] => (ft: F[T]) => f(ft))
    
given ApplyTraverseK[PersonF] = ApplyTraverseK.derived[PersonF]

object ApplyTraverseK:
  inline def derived[U[_[_]] <: Product]: ApplyTraverseK[U] = new ApplyTraverseK[U] {
    extension [F[_]](u: U[F])
      def traverseK[V[_]: Applicative, G[_]](f: [T] => F[T] => V[G[T]]): V[U[G]] = summonFrom {
        case p: Mirror.ProductOf[U[F]] =>
          val appliedF = u.productIterator.toList.map(t => f(t.asInstanceOf[F[Any]]))
          val vTuple: V[Tuple] = appliedF.foldRight(summon[Applicative[V]].pure(EmptyTuple)) { (vgs, x) =>
            x.map2(vgs)((tuple, a) => (a *: tuple) )
          }
          
          vTuple.map { tuple =>
            p.fromProduct(new Product {
              def canEqual(that: Any): Boolean = true
              def productArity: Int = tuple.productArity
              def productElement(n: Int): Any =
                tuple.productElement(n)
            }).asInstanceOf[U[G]]
          }
        case _ => sys.error("cannot handle")
      }
      def map2K[G[_], H[_]](v: U[G])(f: [T] => (F[T], G[T]) => H[T]): U[H] =
        summonFrom {
          case p: Mirror.ProductOf[U[F]] =>
            p.fromProduct(new Product {
              def canEqual(that: Any): Boolean = true
              def productArity: Int = u.productArity
              def productElement(n: Int): Any =
                f(u.productElement(n).asInstanceOf[F[Any]], v.productElement(n).asInstanceOf[G[Any]])
            }).asInstanceOf[U[H]]
          case _ => sys.error("cannot handle")
        }
  }

case class AnotherPersonF[F[_]](
  name: F[String], 
  age: F[Int]
) derives ApplyTraverseK

@main
def main = 
  val person = PersonF[Identity](
    "Charlie", 102, "invalid postcode", None
  )
  val personOpt = PersonF[Option](
    Some("Crow"), Some(123), Some("ABC"), None
  )
  val specificVal = validatePersonFCompleteSpecific(person)
  println("Using the validate function for PersonF on person: " + specificVal)
  val genericVal = validateUComplete(person, personValidations)
  println("Using the validate function for any U on person: " + genericVal)
  assert(specificVal == genericVal)

  val specificValOpt = validatePersonFCompleteSpecific(personOpt)
  println("Using the validate function for PersonF on personOpt: " + specificValOpt)
  val genericValOpt = validateUComplete(personOpt, personValidations)
  println("Using the validate function for any U on personOpt: " + genericValOpt)
  assert(specificValOpt == genericValOpt)
  
  val anotherPerson = AnotherPersonF[Identity]("Ms S", 151)
  val anotherPersonValidations = AnotherPersonF[Validation](
    name => Either.cond(name.startsWith("M"), name, "Name does not being with M"),
    age => Either.cond(age > 0 && age < 150, age, "Age not in range (0,150)")
  )
  println(anotherPerson)
  println(anotherPerson.mapK([T] => (t: Identity[T]) => Some(t)))
  println("Using the validate function for any U on anotherPerson: " + validateUComplete(anotherPerson, anotherPersonValidations))
