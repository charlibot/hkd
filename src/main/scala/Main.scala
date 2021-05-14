// @main def hello: Unit =
//     println("Hello world!")
//     println(msg)
//     println(createTemplate[Identity](Template("Charlie", "Tomething")))
//     println(updateTemplateOptionQuestion[Identity](Template(None, Some("Something"))))


// def msg = "I was compiled by Scala 3. :)"


// case class Template[F[_]](
//     name: F[String], 
//     text: F[String]
// )

// type Identity[A] = A
// type FError[F[_]] = [A] =>> F[Either[String, A]]

// trait Functor[F[_]]:
//     extension [A](fa: F[A]) def map[B](f: A => B): F[B]
//     def lift[A, B](f: A => B): F[A] => F[B] = map(_)(f)

// trait Apply[F[_]] extends Functor[F]:
//     extension [A](fa: F[A]) 
//         def ap[B](ff: F[A => B]): F[B]
//         def map2[B, C](fb: F[B])(f: (A, B) => C): F[C]

// trait Applicative[F[_]] extends Apply[F]:
//     def pure[A](x: A): F[A]

// trait Functor2[F[_, _]]:
//     extension [E, A](fa: F[E, A]) def map[B](f: A => B): F[E, B]

// trait Apply2[F[_, _]] extends Functor2[F]:
//     extension [E, A](fa: F[E, A]) 
//         def ap[B](ff: F[E, A => B]): F[E, B]

// trait Applicative2[F[_, _]] extends Apply2[F]:
//     def pure[E, A](x: A): F[E, A]

// given Functor[Option] with
//     extension [A](fa: Option[A])
//         def map[B](f: A => B): Option[B] = fa.map(f)

// given Applicative[Identity] with
//     def pure[A](x: A): Identity[A] = x
//     extension [A](fa: Identity[A]) 
//         def map[B](f: A => B): Identity[B] = f(fa)
//         def ap[B](ff: Identity[A => B]): Identity[B] = ff(fa)
//         def map2[B, C](fb: Identity[B])(f: (A, B) => C): Identity[C] = f(fa, fb)

// type Validation[T] = T => Either[String, T]
// val templateValidations = Template[Validation](
//     name = name => Either.cond(name.startsWith("C"), name, "Name does not being with 'C'"),
//     text = text => Either.cond(text.startsWith("T"), text, "Text does not being with 'T'")
// )

// // type ValidationF[F[_], T] = F[T] => F[Either[String, T]]
// // val templateValidationsF = Template[ValidationF[F[_], *]](
// //     name = name => Either.cond(name.startsWith("C"), name, "Name does not being with 'C'"),
// //     text = text => Either.cond(text.startsWith("T"), text, "Text does not being with 'T'")
// // )

// // type MapKV = [K] =>> [V] =>> Map[K,V]

// // given [K]: Functor[MapKV[K]] with 
// //     extension [V1] (map: MapKV[K][V1])
// //         def map2[V2](f: V1 => V2): MapKV[K][V2] = map.view.mapValues(f).toMap


// // trait ZipApply[X <: Tuple, Y <: Tuple]:
// //     def zipApply(x: X, y: Y): Tuple

// // object ZipApply:
// //     given ZipApply[EmptyTuple, EmptyTuple] with
// //         def zipApply(emptyF: EmptyTuple, emptyV: EmptyTuple) =
// //             EmptyTuple
// //     given [T]: ZipApply[(T => Either[String, T]) *: Tuple, T *: Tuple] with 
// //         def zipApply(fst: (T => Either[String, T]) *: Tuple, scd: T *: Tuple) = 
// //             fst.head(scd.head) *: summon[ZipApply[Tuple, Tuple]].zipApply(fst.tail, scd.tail)

// trait ZipApply2[X, Y]:
//     def zipApply(x: X, y: Y): Tuple

// object ZipApply2:
//     given ZipApply2[EmptyTuple, EmptyTuple] with
//         def zipApply(emptyF: EmptyTuple, emptyV: EmptyTuple) =
//             EmptyTuple
//     given [A, H <: A => Either[String, A], G <: A, T <: Tuple, S <: Tuple]: ZipApply2[H *: T, G *: S] with 
//         def zipApply(fst: H *: T, scd: G *: S) = ???
//             // fst.head(scd.head) *: summon[ZipApply2[T, S]].zipApply(fst.tail, scd.tail)
//     given ZipApply2[Tuple, Tuple] with 
//         def zipApply(f: Tuple, s: Tuple) = 
//             ???

// trait DepFn2[T, U] {
//     type Out
//     def apply(t: T, u: U): Out
//   }

// trait ZipApply[FL <: Tuple, AL <: Tuple] extends DepFn2[FL, AL] with Serializable { type Out <: Tuple }

// // object ZipApply {
// //     def apply[FL <: Tuple, AL <: Tuple](implicit zip: ZipApply[FL, AL]): ZipApply[FL, AL] = zip

// //     // type Aux[FL <: HList, AL <: HList, Out0 <: HList] = ZipApply[FL, AL] { type Out = Out0 }

// //     implicit def hnilZipApply: ZipApply[EmptyTuple, EmptyTuple] =
// //         new ZipApply[EmptyTuple, EmptyTuple] {
// //             type Out = EmptyTuple
// //             def apply(fl : EmptyTuple, al : EmptyTuple): Out = EmptyTuple
// //         }

// //     implicit def hconsZipApply[T, R, FLT <: Tuple, ALT <: Tuple, ZttOut <: Tuple]
// //         (implicit ztt : ZipApply[FLT, ALT]): ZipApply[(T => R) *: FLT, T *: ALT] =
// //         new ZipApply[(T => R) *: FLT, T *: ALT] {
// //             type Out = R *: ZttOut
// //             def apply(fl : (T => R) *: FLT, al : T *: ALT): Out = fl.head(al.head) *: ztt(fl.tail, al.tail)
// //         }
// // }

// object ZipApply {
//     def apply[FL <: Tuple, AL <: Tuple](implicit zip: ZipApply[FL, AL]): Aux[FL, AL, zip.Out] = zip

//     type Aux[FL <: Tuple, AL <: Tuple, Out0 <: Tuple] = ZipApply[FL, AL] { type Out = Out0 }

//     implicit def hnilZipApply: Aux[EmptyTuple, EmptyTuple, EmptyTuple] =
//         new ZipApply[EmptyTuple, EmptyTuple] {
//             type Out = EmptyTuple
//             def apply(fl : EmptyTuple, al : EmptyTuple): Out = EmptyTuple
//         }

//     implicit def hconsZipApply[T, R, FLT <: Tuple, ALT <: Tuple, ZttOut <: Tuple]
//         (implicit ztt : ZipApply.Aux[FLT, ALT, ZttOut]): Aux[(T => R) *: FLT, T *: ALT, R *: ZttOut] =
//         new ZipApply[(T => R) *: FLT, T *: ALT] {
//             type Out = R *: ZttOut
//             def apply(fl : (T => R) *: FLT, al : T *: ALT): Out = fl.head(al.head) *: ztt(fl.tail, al.tail)
//         }
// }

// // summon[ZipApply[X, Y]].encodeRow(tuple)

// // def zipApply()

// // trait Va[A <: Product, F[_]] {
// //     def validate(a: A)
// // }

// // inline def getTypeclassInstances[A <: Tuple]: List[PrettyString[Any]] = inline erasedValue[A] match {
// //   case _: EmptyTuple => Nil
// //   case x: (head *: tail) => 
// //     val y = x.head
// //     val headTypeClass = summonInline[PrettyString[head]] 
// //     val tailTypeClasses = getTypeclassInstances[tail]
// //     headTypeClass.asInstanceOf[PrettyString[Any]] :: getTypeclassInstances[tail]
// // }


// private def validateTemplate[F[_] : Functor](template: Template[F]): Template[FError[F]] =
    
//     val x: scala.deriving.Mirror.ProductOf[Template[F]] = ???
//     // Template[F]
//     // x.fromProduct(p)
//     // TODO: instead of this manual approach, somehow zip the template with templateValidations and map
//     // 
//     // is this a macro thing?
//     val t = Tuple.fromProductTyped(template)
//     val templateValidationsTuple = Tuple.fromProductTyped(templateValidations)
//     // val fffed = templateValidationsTuple.map([T] => (t: T) => summon[Functor[F]].lift(t))
//     // F[String] => F[Either[String, String]] which is fmap applied to only one argument
//     val zipApply = ZipApply[(String => Either[String, String], String => Either[String, String]), (String, String)]
//     // val results = zipApply(templateValidationsTuple, t)

//     summon[ZipApply2[Tuple, Tuple]].zipApply(templateValidationsTuple, t)
//     // this is an approach but not great because not type safe.
//     // want something like zipApply from shapeless
//     def doSomething[T](values: Tuple, validations: Tuple): Tuple =
//         (values, validations) match 
//             case (EmptyTuple, EmptyTuple) => EmptyTuple
//             case (x: (F[T] *: Tuple), y: ((T => Either[String, T]) *: Tuple)) => 
//                 x.head.map(y.head) *: doSomething(x.tail, y.tail)
//             case _ => throw new Exception("Oops")
//     summon[scala.deriving.Mirror.Of[Template[FError[F]]]].fromProduct(doSomething(t, templateValidationsTuple))
//     doSomething(t, templateValidationsTuple)
//     // def doIt[T](values: F[T] *: Tuple, validations: (T => Either[String, T]) *: Tuple) =
//     //     values.head.map(validations.head) *: doIt(values.tail, validations.tail)
//     // val zipped: ((F[String], Validation[String]), (F[String], Validation[String])) = t.zip(templateValidationsTuple)
//     // val mapped = (1, 'a').map([T] => (t: T) => Some(t))
//     // val blahed = [T] => (t: T) => t match {
//     //     case (x, y) => (Some)
//     // }
//     val polyf = [T, U] => (t: (T, U)) => (Some(t._1), Some(t._2))
//     val polyf2 = [T] => (t: T) => Some(t)
//     val iuhiuh = ((1, 2), ('a','b')).map(polyf2)
//     ???
//     // // zipped.map()
//     // Template(
//     //     template.name.map(templateValidations.name), 
//     //     template.text.map(templateValidations.text)
//     // )

// case class ServiceError(t: Throwable)
// def createTemplate2[F[_, _]: Applicative2](template: Template[Identity]): F[ServiceError | Template[Either[String, *]], Unit] = 
//     val x = validateTemplate(template)
//     summon[Applicative2[F]].pure(())

// // TODO: The return type should be F[Unit] with a potential validation error. Use bifunctors F[_, _]?
// def createTemplate[F[_]: Applicative](template: Template[Identity]): F[Template[Either[String, *]]] = 
//     val x = validateTemplate(template)
//     // want to iterate through the case class. If any Lefts, then we exit (hello monads, traverse?, essentially sequence).
//     // do some transformation to Either[Template[Either[String, *]], Template[Identity]]
//     // this left item is an error
//     summon[Applicative[F]].pure(x)

// def updateTemplateOptionQuestion[F[_]: Applicative](template: Template[Option]): F[Template[FError[Option]]] =
//     val x = validateTemplate(template)
//     summon[Applicative[F]].pure(x)

// // The * in this case will be Options. I wonder if there's something fancy I can do here. Nah, prefer the one above
// def updateTemplate[F[_]](template: Template[Option]): F[Template[Either[String, *]]] =
//     // validate each Some field
//     // construct a SQL query just from the Some
//     ???

// def streamRevisions[F[_]]: F[Template[LazyList]] = 
//     // with every field, s
//     ???

// def receiveRevisions[F[_]](revisions: Template[LazyList]): F[Unit] = 
//     // update the corresponding HTML field. Use in JS
//     ???