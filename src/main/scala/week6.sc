val word: String = "HelloWOrld"

word.groupBy(ch => ch.toLower).map({case (k, v) => (k, v.length)}).toList.sortWith(_._2 > _._2)

val occur = List(('a', 2), ('b', 2))

def comb(bits: List[Int]): List[List[Int]] = {
  if (bits.isEmpty) List(List())
  else
    for {
      seqs <- comb(bits.tail)
      bit <- 0 to bits.head
    } yield bit :: seqs
}

comb(List(2, 2, 2))

def combinations(occs: List[(Char, Int)]): List[List[(Char, Int)]] = {
  occs match {
    case Nil => List(List())
    case (ch, num) :: xs => for {
      seqs <- combinations(xs)
      count <- 0 to num
    } yield if (count == 0) seqs
    else (ch, count) :: seqs
  }
}

combinations(occur)

val occ1 = List(('a', 2))
combinations(occ1)


type Occurrences = List[(Char, Int)]
def subtract(x: Occurrences, y: Occurrences): Occurrences = {
  val xm = x toMap
  val ym = y toMap

  def subOcc(occs: Map[Char, Int], el: (Char, Int)): Map[Char, Int] = {
    val (ch, num) = el
    occs + (ch -> (occs(ch) - num))
  }

  (ym foldLeft xm)(subOcc).toList.filter(pair => pair._2 > 0).sortWith(_._2 > _._2)


}

subtract(List(('a', 4), ('b', 3), ('c', 2)), List(('a', 3), ('b', 3)))
