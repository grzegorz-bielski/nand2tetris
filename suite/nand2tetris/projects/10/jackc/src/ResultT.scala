package jackc

import scala.util.control.TailCalls.*

final case class ResultT[G](value: TailRec[Either[Error, G]]) extends AnyVal
object ResultT:
  inline def of[G](g: G): ResultT[G] = ResultT(done(Right(g)))
  inline def error[G](err: Error): ResultT[G] = ResultT(done(Left(err)))

  def matches[G](hint: String, value: G)(fn: PartialFunction[G, Unit]): ResultT[Unit] =
    if fn.isDefinedAt(value) then ResultT.of(())
    else ResultT.error(Error.PredicateFailed(s"$hint predicate failed on $value"))

  extension [G](r: ResultT[G])
    // fails on first error without accumulating
    def flatMap[G2](fn: G => ResultT[G2]): ResultT[G2] = ResultT:
      r.value.flatMap:
        case Left(err) => done(Left(err))
        case Right(g) =>
          fn(g).value.flatMap:
            case Left(err) => done(Left(err))
            case Right(g2) => done(Right(g2))

    def map[G2](fn: G => G2): ResultT[G2] = flatMap(g => ResultT.of(fn(g)))

    def run: Either[Error, G] = r.value.result

    def toOption: Option[G] = run.toOption 

    def withFilter(fn: G => Boolean): ResultT[G] = ResultT:
      r.value.flatMap:
        case Left(err) => done(Left(err))
        case Right(g)  => if fn(g) then done(Right(g)) else done(Left(Error.PredicateFailed(s"`withFilter` Predicate failed on $g")))
