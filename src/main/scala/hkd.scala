// import scala.deriving._
// import scala.compiletime._

// case class Tag(name: String, value: String)

// case class PersonOf[F[_]](
//     firstName: F[String],
//     lastName: F[Option[String]],
//     age: F[Int],
//     tags: F[List[Tag]]
// )

// type Name[A] = String

// type Names[Data[f[_]]] = Data[Name]

// given Names[PersonOf] = names[PersonOf]

// inline def names[Data[f[_]]](using p: Mirror.ProductOf[Data[Name]]) = 
//     p.fromProduct(constValueTuple[p.MirroredElemLabels])

// inline def provision[P](using p: Mirror.ProductOf[P]) =
//     p.fromProduct(summonAll[p.MirroredElemTypes])

// trait Show[A] {
//     def show(a: A): String
// }
// given Show[String] with 
//     def show(s: String) = s
// given Show[Int] with 
//     def show(s: Int) = s.toString
// given Show[Tag] with 
//     def show(t: Tag) = t.toString // TODO
// given [A] (using showA: Show[A]): Show[List[A]] with
//     def show(xs: List[A]) = xs.map(x => showA.show(x)).mkString("[", ", ", "]")
// given [A] (using showA: Show[A]): Show[Option[A]] with 
//     def show(opt: Option[A]) = 
//         opt.fold("None")(s => s"Some(${showA.show(s)})")

// type Provided[Data[f[_]], TC[_]] = Data[TC]
// given PersonOf[Show] = provision[PersonOf[Show]]

// // Is it ApplyK?
// // f: [A] => (F[A], G[A]) => H[A]
// //    [String] => (F[String], String => Either[String, String]) => F[Either[String, String]]

// trait FunctorK[U[f[_]]]:
//     extension [F[_], G[_]] (obj: U[F])
//         def mapK (f: [A] => F[A] => G[A]): U[G]
// trait PureK[U[f[_]]] extends FunctorK[U]:
//     def pureK[F[_]](gen: [A] => () => F[A]): U[F]
// trait ApplyK[U[f[_]]] extends FunctorK[U]:
//     extension [F[_], G[_], H[_]] (left: U[F])
//         def map2K(right: U[G])(f: [A] => (F[A], G[A]) => H[A]): U[H]
// // trait ApplicativeK[U[f[_]]] extends ApplyK[U] with PureK[U]:
// //     extension [F[_], G[_]] (obj: U[F])
// //         override def mapK (f: [A] => F[A] => G[A]): U[G] = 
// //             val pure = pureK[[A] =>> Unit]([A] => () => ())
// //             obj.map2K[F, [A] =>> Unit, G](pure)([A] => (x: F[A], y: Unit) => f(x))
//             // map2K[F, [A] =>> Unit, G](obj, pure, [A] => (x: F[A], y: Unit) => f(x))
//             // obj.map2K[F, [A] =>> Unit, G](pureK([A] => () => ()))([A] => (x: F[A], y: Unit) => f(x))
// // given FunctorK[PersonOf] with
// //     extension [F[_], G[_]] (obj: PersonOf[F])
// //         def mapK (f: [A] => F[A] => G[A]): PersonOf[G] =
// //             PersonOf(
// //                 f(obj.firstName),
// //                 f(obj.lastName),
// //                 f(obj.age),
// //                 f(obj.tags),
// //             )

// inline def deriveApplyKCaseClass[U[f[_]] <: Product]: ApplyK[U] = 
//     new ApplyK[U] {
//         extension [F[_], G[_], H[_]] (left: U[F]) 
//             def map2K(right: U[G])(f: [A] => (F[A], G[A]) => H[A]): U[H] = summonFrom {
//                 case p: Mirror.ProductOf[U[F]] => 
//                     p.fromProduct(new Product {
//                         def canEqual(that: Any): Boolean = true
//                         def productArity: Int = left.productArity
//                         def productElement(n: Int): Any = 
//                             f(left.productElement(n).asInstanceOf[F[Any]], right.productElement(n).asInstanceOf[G[Any]])
//                     }).asInstanceOf[U[H]]
//             }
//         extension [F[_], G[_]] (obj: U[F])
//             def mapK(f: [A] => F[A] => G[A]): U[G] = summonFrom {
//                 case p: Mirror.ProductOf[U[F]] => 
//                     // println("When is this run?")
//                     // val gs = obj.productIterator.map {
//                     //     case fa: F[_] => f(fa).asInstanceOf[Any]
//                     // }.toArray
//                     p.fromProduct(new Product {
//                         def canEqual(that: Any): Boolean = true
//                         def productArity: Int = obj.productArity
//                         def productElement(n: Int): Any = 
//                             f(obj.productElement(n).asInstanceOf[F[Any]])
//                     }).asInstanceOf[U[G]]
//                 case _ => error("")
//             }
//     }

// given ApplyK[PersonOf] = deriveApplyKCaseClass[PersonOf]

// case class Y[F[_]](y: F[String])
// case class X[F[_]](x: F[Y[F]])

// given ApplyK[Y] = deriveApplyKCaseClass[Y]
// given ApplyK[X] = deriveApplyKCaseClass[X]

// // given ApplyK[PersonOf] with
// //     extension [F[_], G[_]] (obj: PersonOf[F])
// //         def mapK (f: [A] => F[A] => G[A]): PersonOf[G] =
// //             PersonOf(
// //                 f(obj.firstName),
// //                 f(obj.lastName),
// //                 f(obj.age),
// //                 f(obj.tags),
// //             )
// //     extension [F[_], G[_], H[_]] (left: PersonOf[F])
// //         def map2K(right: PersonOf[G])(f: [A] => (F[A], G[A]) => H[A]): PersonOf[H] =
// //             PersonOf(
// //                 f(left.firstName, right.firstName),
// //                 f(left.lastName, right.lastName),
// //                 f(left.age, right.age),
// //                 f(left.tags, right.tags)
// //             )
// type Validation2[T] = T => Either[String, T]
// type FError2[F[_]] = [A] =>> F[Either[String, A]]

// @main def main: Unit =
//     println("Hello world!")
//     println(names[PersonOf])

//     val y = Y[Identity]("y")
//     val x = X[Identity](y)
//     println(x.mapK([A] => (a: A) => Some(a)))
    
//     val person = PersonOf[Identity](
//         "leg",
//         Some("Nizhnik"),
//         36,
//         List(Tag("scala", "love"))
//     )
//     val personValidations = PersonOf[Validation2](
//         s => Either.cond(s.startsWith("O"), s, "Bad first name"),
//         s => Either.cond(s.isEmpty || s.exists(_.startsWith("N")), s, "Bad last name"),
//         s => Either.cond(s > 0, s, "Bad age"),
//         s => Either.cond(true, s, "Bad tags"),
//     )
//     val validated = person.map2K(personValidations)(
//         [A] => (value: A, func: A => Either[String, A]) => 
//         func(value)
//     )
//     println(validated)
//     val personOpt: PersonOf[Option] = person.mapK([A] => (a: A) => Some(a))
//     println(personOpt)
//     val validatedOpt: PersonOf[[A] =>> Option[Either[String, A]]] = personOpt.map2K(personValidations)(
//         [A] => (value: Option[A], func: A => Either[String, A]) =>
//             value.map(func)
//     )
//     println(validatedOpt)
    

//     def validatedF[F[_]: Functor](personF: PersonOf[F]): PersonOf[[A] =>> F[Either[String, A]]] = personF.map2K(personValidations)(
//         [A] => (value: F[A], func: A => Either[String, A]) =>
//             value.map(func)
//     )
//     println(validatedF(personOpt))

//     for 
//       a <- Either.cond(true, 1, 2)
//     yield a
