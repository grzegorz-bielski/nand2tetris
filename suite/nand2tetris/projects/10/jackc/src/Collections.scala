package jackc

import scala.collection.{ AbstractIterator, AbstractView, Factory }
import scala.collection.generic.IsSeq

// from https://docs.scala-lang.org/overviews/core/custom-collection-operations.html
extension [Repr](coll: Repr)(using seq: IsSeq[Repr])
  def intersperse[B >: seq.A, That](sep: B)(using factory: Factory[B, That]): That =
    val seqOps = seq(coll)
    factory.fromSpecific:
      new AbstractView[B]:
        def iterator = new AbstractIterator[B]:
          val it = seqOps.iterator
          var intersperseNext = false
          def hasNext = intersperseNext || it.hasNext
          def next() =
            val elem = if intersperseNext then sep else it.next()
            intersperseNext = !intersperseNext && it.hasNext
            elem
    