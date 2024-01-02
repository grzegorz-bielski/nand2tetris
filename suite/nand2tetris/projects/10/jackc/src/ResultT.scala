package jackc

import scala.util.control.TailCalls.*

final case class ResultT[G](value: TailRec[Either[Error, G]]) extends AnyVal
object ResultT:
  inline def of[G](g: G): ResultT[G] = ResultT(done(Right(g)))
  inline def error[G](err: Error): ResultT[G] = ResultT(done(Left(err)))

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

    def withFilter(fn: G => Boolean): ResultT[G] = ResultT:
      r.value.flatMap:
        case Left(err) => done(Left(err))
        case Right(g) => if fn(g) then done(Right(g)) else done(Left(Error.PredicateFailed(s"Predicate failed on $g")))
