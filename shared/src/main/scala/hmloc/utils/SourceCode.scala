package hmloc.utils

import hmloc.utils.shorthands._

class SourceLine(val content: Str, indent: Int = 0) {
  def indented: SourceLine = new SourceLine(content, indent + 1)
  def +(that: SourceLine): SourceLine =
    new SourceLine(content + that.content, indent)
  def withPrefix(prefix: Str): SourceLine =
    new SourceLine(prefix + content, indent)
  def withPostfix(postfix: Str): SourceLine =
    new SourceLine(content + postfix, indent)
  def between(prefix: Str, postfix: Str): SourceLine =
    new SourceLine(prefix + content + postfix, indent)
  override def toString: Str = "  " * indent + content
}

object SourceLine {
  def apply(line: Str): SourceLine = new SourceLine(line)
}

/**
  * SourceCode is a list of SourceLines that can be exported to a code file
  * 
  * SourceCode provides a number of helper methods to create SourceCode
  * from strings and manipulate them. It also abstracts common code patterns
  * like block, condition, clause
  *
  * @param lines
  */
class SourceCode(val lines: Ls[SourceLine]) {

  /** Concat two parts of source code vertically. "1 \\ 2" + "3 \\ 4" == "1 \\ 2 \\ 3 \\ 4"
    *
    * @param that
    *   another part of source code
    * @return
    */
  def +(that: SourceCode): SourceCode = new SourceCode(lines ++ that.lines)

  /** Concat two parts of source code horizontally. "1 \\ 2" ++ "3 \\ 4" == "1 \\ 2 3 \\ 4"
    *
    * @param that
    *   another part of source code
    * @return
    */
  def ++(that: SourceCode): SourceCode = that.lines match {
    case head :: next =>
      if (lines.nonEmpty) {
        new SourceCode(lines.init ::: Ls(lines.last + head) ::: next)
      } else {
        that
      }
    case Nil => this
  }

  def isSingleLine: Bool = lines.sizeCompare(1) === 0

  def isMultipleLine: Bool = lines.lengthIs > 1

  def isEmpty: Bool = lines.isEmpty
  def indented: SourceCode = new SourceCode(lines map { _.indented })
  def parenthesized(implicit run: Bool = true): SourceCode =
    if (run) {
      lines.length match {
        case 0 => SourceCode("()")
        case 1 => new SourceCode(lines map { _.between("(", ")") })
        case _ =>
          val head = lines.head
          val middle = lines.tail.dropRight(1)
          val last = lines.last
          new SourceCode(
            head.withPrefix("(") :: middle ::: Ls(last.withPostfix(")"))
          )
      }
    } else {
      this
    }
  def clause: SourceCode =
    lines.length match {
      case 0 | 1 => this
      case _ =>
        val head = lines.head
        val middle = lines.tail.dropRight(1)
        val last = lines.last
        new SourceCode(
          head.withPrefix("(") :: middle ::: Ls(last.withPostfix(")"))
        )
    }
  def condition: SourceCode =
    lines.length match {
      case 0 => this
      case 1 => new SourceCode(lines map { _.between("(", ")") })
      case _ =>
        new SourceCode(
          SourceLine("(")
            :: lines.map({ _.indented })
            ::: Ls(SourceLine(")"))
        )
    }
  // Surround the source code with braces in a block style.
  def block: SourceCode =
    lines.length match {
      case 0 => SourceCode("{}")
      case _ =>
        new SourceCode(
          SourceLine("{")
            :: lines.map({ _.indented })
            ::: Ls(SourceLine("}"))
        )
    }
  override def toString: Str = lines.mkString("\n")

  def toLines: Ls[Str] = lines.map(_.toString)
}

object SourceCode {
  def apply(line: Str): SourceCode = new SourceCode(Ls(new SourceLine(line, 0)))
  def apply(lines: Ls[Str]): SourceCode = new SourceCode(lines map {
    new SourceLine(_, 0)
  })

  val ampersand: SourceCode = SourceCode(" & ")
  val space: SourceCode = SourceCode(" ")
  val semicolon: SourceCode = SourceCode(";")
  val colon: SourceCode = SourceCode(": ")
  val separator: SourceCode = SourceCode(" | ")
  val comma: SourceCode = SourceCode(",")
  val commaSpace: SourceCode = SourceCode(", ")
  val empty: SourceCode = SourceCode(Nil)
  val openCurlyBrace: SourceCode = SourceCode("{")
  val closeCurlyBrace: SourceCode = SourceCode("}")
  val openAngleBracket: SourceCode = SourceCode("<")
  val closeAngleBracket: SourceCode = SourceCode(">")
  val fatArrow: SourceCode = SourceCode(" => ")
  val equalSign: SourceCode = SourceCode(" = ")

  def concat(codes: Ls[SourceCode]): SourceCode =
    codes.foldLeft(SourceCode.empty) { _ + _ }

  // concatenate source codes without intermediate allocations
  def bulkConcat(codes: Iterable[SourceCode]): SourceCode =
    new SourceCode(codes.iterator.map(_.lines).foldRight(List.empty[SourceLine])((lines, accum) => lines ::: accum))

  /**
    * Comma-separate elements of List[SourceCode] and wrap with curly braces.
    * Each element is on a new line.
    * 
    * @param entries
    * @return
    */
  def record(entries: Ls[SourceCode]): SourceCode =
    entries match {
      case Nil         => SourceCode("{}")
      case entry :: Nil => if (entry.isMultipleLine) {
        SourceCode("{") + entry.indented + SourceCode("}")
      } else {
        SourceCode("{ ") ++ entry ++ SourceCode(" }")
      }
      case _ =>
        (entries.zipWithIndex.foldLeft(SourceCode("{")) { case (acc, (entry, index)) =>
          acc + (if (index + 1 === entries.length) { entry }
                 else { entry ++ SourceCode.comma }).indented
        }) + SourceCode("}")
    }


  /**
    * Comma separate elements of List[SourceCode] and wrap with curly braces
    * on the same horizontal line
    * 
    * @param entries
    * @return
    */
    def horizontalRecord(entries: Ls[SourceCode]): SourceCode = {
      entries match {
        case Nil => SourceCode("{}")
        case _ =>
          (entries
            .zipWithIndex.foldLeft(SourceCode("{")) { case (acc, (entry, index)) =>
            acc ++ entry ++ (if (index + 1 === entries.length) SourceCode.closeCurlyBrace else SourceCode.commaSpace)
          })
      }
    }


    def recordWithEntries(entries: List[SourceCode -> SourceCode]): SourceCode = {
      entries match {
        case Nil => SourceCode("{}")
        case _ =>
          (entries
            .map(entry => entry._1 ++ colon ++ entry._2)
            .zipWithIndex.foldLeft(SourceCode("{")) { case (acc, (entry, index)) =>
            acc ++ entry ++ (if (index + 1 === entries.length) SourceCode.closeCurlyBrace else SourceCode.commaSpace)
          })
      }
    }
    
    /** ',' separate and wrap in angled brackets the given source code instances
      * and return empty string if list is empty 
      *
      * @param entries
      * @return
      */
    def paramList(entries: List[SourceCode]): SourceCode = {
      entries match {
        case Nil => SourceCode("")
        case _ =>
          (entries
            .zipWithIndex.foldLeft(SourceCode.openAngleBracket) { case (acc, (entry, index)) =>
            acc ++ entry ++ (if (index + 1 === entries.length) SourceCode.closeAngleBracket else SourceCode.commaSpace)
          })
      }
    }

  /**
    * Surround the source code with braces.
    */
  def array(entries: Ls[SourceCode]): SourceCode =
    entries match {
      case Nil         => SourceCode("[]")
      case entry :: Nil => if (entry.isMultipleLine) {
        SourceCode("[") + entry.indented + SourceCode("]")
      } else {
        SourceCode("[") ++ entry ++ SourceCode("]")
      }
      case _ =>
        (entries.zipWithIndex.foldLeft(SourceCode("[")) { case (acc, (entry, index)) =>
          acc + (if (index + 1 === entries.length) { entry }
                 else { entry ++ SourceCode.comma }).indented
        }) + SourceCode("]")
    }

  /**
    * Surround source code with square brackets concatenating elements
    * horizontally only. Single element is still wrapped in brackets
    *
    * @param entries
    * @return
    */
  def horizontalArray(entries: Ls[SourceCode]): SourceCode =
    (entries.zipWithIndex.foldLeft(SourceCode("[")) { case (acc, (entry, index)) =>
      acc ++ entry ++ (if (index + 1 === entries.length) SourceCode("]") else SourceCode.commaSpace)
    })


  def sepBy(codes: Ls[SourceCode], sep: SourceCode = this.commaSpace): SourceCode =
    codes.zipWithIndex
      .foldLeft(this.empty) { case (x, (y, i)) =>
        x ++ y ++ (if (i === codes.length - 1) this.empty else sep)
      }

}
