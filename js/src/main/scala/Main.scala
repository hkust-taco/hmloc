import scala.util.Try
import scala.scalajs.js.annotation.JSExportTopLevel
import org.scalajs.dom
import org.scalajs.dom.document
import org.scalajs.dom.raw.{Event, HTMLTextAreaElement, TextEvent, UIEvent}
import mlscript.utils._
import mlscript._
import mlscript.utils.shorthands._

import scala.util.matching.Regex
import scala.scalajs.js
import scala.collection.{immutable, mutable}

object Main {
  def main(args: Array[String]): Unit = {
    val source = document.querySelector("#system-input")
    update(source.textContent)
    source.addEventListener("input", typecheck)
  }
  @JSExportTopLevel("typecheck")
  def typecheck(e: dom.UIEvent): Unit = {
    e.target match {
      case elt: dom.HTMLTextAreaElement =>
        update(elt.value)
    }
  }
  @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
  def update(str: String): Unit = {
    // println(s"Input: $str")
    val target = document.querySelector("#system-output")
    val tryRes = Try[Str] {
      import fastparse._
      import fastparse.Parsed.{Success, Failure}
      import mlscript.{OcamlParser, ErrorReport, Origin}
      val lines = str.splitSane('\n').toIndexedSeq
      val processedBlock = OcamlParser.addTopLevelSeparators(lines).mkString
      val fph = new mlscript.FastParseHelpers(str, lines)
      val parser = new OcamlParser(Origin("<input>", 1, fph))
      parse(processedBlock, parser.pgrm(_), verboseFailures = false) match {
        case f: Failure =>
          val Failure(err, index, extra) = f
          val (lineNum, lineStr, _) = fph.getLineColAt(index)
          "Parse error: " + extra.trace().msg +
            s" at line $lineNum:<BLOCKQUOTE>$lineStr</BLOCKQUOTE>"
        case Success(pgrm, index) =>
          println(s"Parsed: $pgrm")
          val (diags, (typeDefs, stmts)) = pgrm.desugared
          // report(diags) // TODO... currently the OcamlParser does not report any in desugaring so this is fine
          val (typeCheckResult, errorResult) = checkProgramType(pgrm)
          errorResult match {
            case Some(typeCheckResult) => typeCheckResult
            case None =>
              val htmlBuilder = new StringBuilder
              htmlBuilder ++= """<table>
                  |  <thead>
                  |    <tr>
                  |       <td>Name</td>
                  |       <td>Type</td>
                  |       <td>Value</td>
                  |    </tr>
                  |  </thead>
                  |""".stripMargin
              // Assemble something like: `val <name>: <type> = <value>`.
              // If error occurred, leave `<no value>`.
              typeCheckResult.zip(pgrm.desugared._2._2) foreach { case ((name, ty), origin) =>
                val value = origin match {
                  // Do not extract from results if its a type declaration.
                  case Def(_, _, R(_), _) => N
                  // Otherwise, `origin` is either a term or a definition.
                  case _ => /* results match {
                    case head :: next =>
                      results = next
                      S(head)
                    case Nil => N
                  } */
                  N
                }
                val valueHtml = value match {
                  // case S(text) => s"<td>$text</td>"
                  case N => "<td class=\"no-value\">no value</td>"
                }
                htmlBuilder ++= s"""<tr>
                  |  <td class="name">${name getOrElse "res"}</td>
                  |  <td class="type">$ty</td>
                  |  $valueHtml
                  |</tr>
                  |""".stripMargin
              }
              htmlBuilder ++= "</table>"
              htmlBuilder.toString
          }
      }
    }
    
    target.innerHTML = tryRes.fold[Str](
      err =>
        s"""
      <font color="Red">
      Unexpected error: ${err}${
          err.printStackTrace
          // err.getStackTrace().map(s"$htmlLineBreak$htmlWhiteSpace$htmlWhiteSpace at " + _).mkString
          ""
        }</font>""",
      identity
    )
  }
  
  private val htmlLineBreak = "<br/>"
  private val htmlWhiteSpace = "&nbsp;"
  private val splitLeadingSpaces: Regex = "^( +)(.+)$".r
  private def underline(fragment: Str): Str =
    s"<u class=\"error-underline\">$fragment</u>"
  
  def checkProgramType(pgrm: Pgrm): Ls[Option[Str] -> Str] -> Option[Str] = {
    val (diags, (typeDefs, stmts)) = pgrm.desugared
    
    val typer = new mlscript.Typer(
      dbg = false,
      verbose = false,
      explainErrors = false
    ) {
      override def reporCollisionErrors: Bool = false
      unifyMode = true
    }
    
    import typer._
    
    val res = new collection.mutable.StringBuilder
    val results = new collection.mutable.ArrayBuffer[Option[Str] -> Str]
    val stopAtFirstError = true
    var errorOccurred = false

    def formatError(culprit: Str, err: ErrorReport): Str = {
      s"""<b><font color="Red">
                Error in <font color="LightGreen">${culprit}</font>: ${err.mainMsg}
                <!--${
                  // The rest of the message may not make sense if we don't also print the provs
                  // For example we'd get things like "Declared at\nDeclared at" for dup type params...
                  err.allMsgs.tail
                    .map(_._1.show + "<br/>")
                    .mkString("&nbsp;&nbsp;&nbsp;&nbsp;")}-->
                </font></b><br/>"""
    }
    
    implicit val raise: Raise = throw _
    
    val (libCtx, libDeclared): (typer.Ctx, Map[Str, typer.PolymorphicType]) = Helpers.loadLibrary(typer)
    // ctx = libCtx
    // declared = libDeclared
    implicit var ctx: Ctx =
      try processTypeDefs(typeDefs)(libCtx, raise)
      catch {
        case err: ErrorReport =>
          res ++= formatError("class definitions", err)
          Ctx.init
      }
    
    val curBlockTypeDefs = typeDefs.flatMap(td => ctx.tyDefs.get(td.nme.name))
    typer.computeVariances(curBlockTypeDefs, ctx)
    
    def getType(ty: typer.TypeScheme): Type = {
      val wty = ty.uninstantiatedBody
      object SimplifyPipeline extends typer.SimplifyPipeline {
        def debugOutput(msg: => Str): Unit = println(msg)
      }
      val sim = SimplifyPipeline(wty)(ctx)
      val exp = typer.expandUnifiedType(sim)
      exp
    }
    def formatBinding(nme: Str, ty: TypeScheme): Str = {
      val exp = getType(ty)
      s"""<b>
              <font color="#93a1a1">val </font>
              <font color="LightGreen">${nme}</font>: 
              <font color="LightBlue">${exp.show}</font>
              </b><br/>"""
    }
    
    def underline(fragment: Str): Str =
      s"<u class=\"error-underline\">$fragment</u>"
    
    var totalTypeErrors = 0
    var totalWarnings = 0
    var outputMarker = ""
    val blockLineNum = 0
    val showRelativeLineNums = false

    def reportUniError(err: UniErrReport,
                       sb: collection.mutable.StringBuilder = new collection.mutable.StringBuilder(),
                       show: Option[ShowCtx] = None
                      ): Str = {
      def output(s: Str): Unit = {
        sb ++= outputMarker
        sb ++= s
        sb ++= htmlLineBreak
        ()
      }

      def outputMsg(info: (Message, Ls[Loc], Bool, Int, Bool), sctx: ShowCtx): Unit = {
        val (msg, locs, dir, level, lastMsg) = info
        val levelOffset = htmlWhiteSpace * (level * 2)
        val msgPre = levelOffset ++ "◉ "
        val msgStr = msgPre ++ msg.showIn(sctx)
        var locPre = levelOffset ++ "│" ++ htmlWhiteSpace
        output(msgStr)

        locs.zipWithIndex.foreach { case (loc, idx) =>
          var termLinePre = levelOffset ++ "│" ++ htmlWhiteSpace
          val lastLoc = idx == locs.length - 1
          if (lastMsg) locPre = levelOffset ++ htmlWhiteSpace * 2
          if (lastMsg) termLinePre = levelOffset ++ htmlWhiteSpace * 2

          val (startLineNum, _, startLineCol) = loc.origin.fph.getLineColAt(loc.spanStart)
          val (endLineNum, _, endLineCol) = loc.origin.fph.getLineColAt(loc.spanEnd)
          val lineNum = loc.origin.startLineNum + startLineNum - blockLineNum
          val lineNumPad = 5
          var lineNumStr = htmlWhiteSpace * lineNumPad // about the same space as if it had a 2 digit line number
          val lineBullet = " - "
          val truncateStr = " ..."

          // single line location and markers
          lineNumStr = if (loc.origin.fileName === "builtin") {
            "lib.".padTo(lineNumPad, ' ')
          } else {
            s"l.$lineNum".padTo(lineNumPad, ' ')
          }
          val fstLine: String = loc.origin.fph.lines(startLineNum - 1)
          if (!dir && idx === 0 && !lastMsg) termLinePre = levelOffset ++ "▲ "
          var linePre = termLinePre ++ lineBullet ++ lineNumStr

          if (endLineNum === startLineNum) {
            val (begin, rest) = fstLine.splitAt(startLineCol - 1)
            val (mid, last) = rest.splitAt(endLineCol - startLineCol)
            val reLine = s"$begin${underline(mid)}$last"
            output(linePre ++ reLine)
          }
          // multi line location print first two lines
          // truncate if message runs past second line
          else {
            // markers for first line cover the line for multi line
            val (start, rest) = fstLine.splitAt(startLineCol - 1)
            val reLine = s"$start${underline(rest)}"
            output(linePre ++ reLine)
            if (!lastLoc) output(locPre)

            val truncate = endLineNum > (startLineNum + 1)
            var sndLine: String = loc.origin.fph.lines(startLineNum)
            if (truncate) sndLine ++= truncateStr
            val whitespace = sndLine.takeWhile(_ == ' ').length
            linePre = htmlWhiteSpace * (lineBullet.length + lineNumStr.length)

            if (truncate) {
              val (begin, rest) = sndLine.splitAt(whitespace)
              val reLine = s"${begin.replaceAll(" ", htmlWhiteSpace)}${underline(rest)}"
              output(locPre ++ linePre ++ reLine)
            } else {
              val (begin, rest) = sndLine.splitAt(whitespace)
              val (mid, last) = rest.splitAt(endLineCol)
              val reLine = s"${begin.replaceAll(" ", htmlWhiteSpace)}${underline(mid)}$last"
              output(locPre ++ linePre ++ reLine)
            }
          }

          if (lastLoc) {
            if (dir && !lastMsg) locPre = levelOffset ++ "▼ "
            if (!lastMsg) output(locPre)
          } else {
            output(locPre)
          }
        }
      }
      val (mainMsg, seqStr, msgs, _, _) = UniErrReport.unapply(err).get
      val sctx = show.getOrElse(Message.mkCtx(err.allMsgs.map(_._1)))

      if (err.level === 0) {
        output("")
        val pre = "<strong style=\"color: #E74C3C\">[ERROR]</strong> "
        output(s"$pre${mainMsg.showIn(sctx)}")
        output("")
      }

      msgs.zipWithIndex.foreach{
        case (L(msg), i) => outputMsg(msg, sctx)
        case (R(report), i) => reportUniError(report, sb, S(sctx))
      }

      // show is empty if it's the first call here we should
      // return the string from the string builder
      if (show.isEmpty) {
        sb.toString
      } else {
        ""
      }
    }

    def report(diag: Diagnostic): Str = {
      var sb = new collection.mutable.StringBuilder
      def output(s: Str): Unit = {
        sb ++= outputMarker
        sb ++= s
        sb ++= htmlLineBreak
        ()
      }
      val sctx = Message.mkCtx(diag.allMsgs.iterator.map(_._1), "?")
      val headStr = diag match {
        case ErrorReport(msg, loco, src) =>
          totalTypeErrors += 1
          s"╔══ <strong style=\"color: #E74C3C\">[ERROR]</strong> "
        case WarningReport(msg, loco, src) =>
          totalWarnings += 1
          s"╔══ <strong style=\"color: #F39C12\">[WARNING]</strong> "
      }
      val lastMsgNum = diag.allMsgs.size - 1
      var globalLineNum =
        blockLineNum // solely used for reporting useful test failure messages
      diag.allMsgs.zipWithIndex.foreach { case ((msg, loco), msgNum) =>
        val isLast = msgNum =:= lastMsgNum
        val msgStr = msg.showIn(sctx)
        if (msgNum =:= 0)
          output(headStr + msgStr)
        else
          output(s"${if (isLast && loco.isEmpty) "╙──" else "╟──"} ${msgStr}")
        if (loco.isEmpty && diag.allMsgs.size =:= 1) output("╙──")
        loco.foreach { loc =>
          val (startLineNum, startLineStr, startLineCol) =
            loc.origin.fph.getLineColAt(loc.spanStart)
          if (globalLineNum =:= 0) globalLineNum += startLineNum - 1
          val (endLineNum, endLineStr, endLineCol) =
            loc.origin.fph.getLineColAt(loc.spanEnd)
          var l = startLineNum
          var c = startLineCol // c starts from 1
          while (l <= endLineNum) {
            val globalLineNum = loc.origin.startLineNum + l - 1
            val relativeLineNum = globalLineNum - blockLineNum + 1
            val shownLineNum =
              if (showRelativeLineNums && relativeLineNum > 0)
                s"l.+$relativeLineNum"
              else "l." + globalLineNum
            val prepre = "║  "
            val pre = s"$shownLineNum: " // Looks like l.\d+
            val curLine = loc.origin.fph.lines(l - 1)
            val lastCol =
              if (l =:= endLineNum) endLineCol else curLine.length + 1
            val front = curLine.slice(0, c - 1)
            val middle = underline(curLine.slice(c - 1, lastCol - 1))
            val back = curLine.slice(lastCol - 1, curLine.size)
            output(s"$prepre$pre\t$front$middle$back")
            c = 1
            l += 1
            if (isLast) output("╙──")
          }
        }
      }
      if (diag.allMsgs.isEmpty) output("╙──")
      sb.toString
    }
    
    var declared: Map[Var, typer.PolymorphicType] = Map.empty
    
    def htmlize(str: Str): Str =
      str.replace("\n", "<br/>").replace("  ", "&emsp;")

    var decls = stmts
    while (decls.nonEmpty) {
      val d = decls.head
      decls = decls.tail
      try d match {
        case d @ Def(isrec, nme, L(rhs), _) =>
          val ty_sch = typeLetRhs(isrec, nme, rhs)(ctx, raise)
          val inst = ty_sch.instantiate(0)
          println(s"Typed `$nme` as: $inst")
          println(s" where: ${inst.showUnified}")
          val exp = getType(ty_sch)
          declared.get(nme) match {
            case S(sign) =>
              typer.uniState.subsume(ty_sch, sign)(ctx, raise, TypeProvenance(d.toLoc, "def definition"))
              // Note: keeping the less precise declared type signature here (no ctx update)
            case N =>
              ctx += nme.name -> VarSymbol(ty_sch, nme)
          }
          res ++= formatBinding(d.nme.name, ty_sch)
          results append S(d.nme.name) -> htmlize(getType(ty_sch).show)
        case d @ Def(isrec, nme, R(PolyType(tps, rhs)), _) =>
          declared.get(nme) match {
            case S(sign) =>
              import Message.MessageContext
              typer.err(msg"illegal redeclaration of ${nme.name}" -> d.toLoc
                :: msg"already defined here:" ->
                  declared.keysIterator.find(_.name === nme.name).flatMap(_.toLoc)
                :: Nil)
            case N => ()
          }
          val ty_sch = PolymorphicType(0, typeType(rhs)(ctx.nextLevel, raise,
            vars = tps.map(tp => tp.name -> freshVar(noProv/*FIXME*/)(1)).toMap))
          ctx += nme.name -> VarSymbol(ty_sch, nme)
          declared += nme -> ty_sch
          results append S(d.nme.name) -> htmlize(getType(ty_sch).show)
        case s: Statement =>
          typer.typeStatement(s, allowPure = true) match {
            case R(binds) =>
              binds.foreach { case (nme, pty) =>
                ctx += nme -> VarSymbol(pty, Var(nme))
                res ++= formatBinding(nme, pty)
                results append S(nme) -> htmlize(getType(pty).show)
              }
            case L(pty) =>
              val exp = getType(pty)
              if (exp =/= TypeName("unit")) {
                val nme = "res"
                ctx += nme -> VarSymbol(pty, Var(nme))
                res ++= formatBinding(nme, pty)
                results append N -> htmlize(getType(pty).show)
              }
          }
      } catch {
        case err: ErrorReport =>
          if (stopAtFirstError) decls = Nil
          val culprit = d match {
            case Def(isrec, nme, rhs, isByname)  => "def " + nme
            case _: Statement => "statement"
          }
          res ++= report(err)
          errorOccurred = true
      }
    }

    // generate unification for the type variables created in the current typing unit.
    typer.uniState.error.toList.sorted
      .foreach(u => res ++= reportUniError(u.createErrorMessage()(ctx, u.sequenceTVs)))

    // results.toList -> (if (errorOccurred) S(res.toString) else N)
    results.toList -> S(res.toString)
  }
}

object Helpers {
  import os.Path
  import fastparse._
  import fastparse.Parsed.{Success, Failure}
  import mlscript.{OcamlParser, ErrorReport, Origin}
  
  // val libPath = dir/"ocaml"/"OcamlLibrary.mls"
  
    /** Load type definitions and function definitions from a file into ctx
      * and declared definitions. This is useful for loading ocaml standard
      * library definitions
      */
    def loadLibrary(typer: Typer): (typer.Ctx, Map[Str, typer.PolymorphicType]) = {
      val fileContents = libStr
      val allLines = fileContents.splitSane('\n').toIndexedSeq
      val block = OcamlParser.libraryTopLevelSeparators(allLines).mkString("\n")
      val fph = new FastParseHelpers(block)
      val globalStartLineNum = 0
      parse(block, p => new OcamlParser(Origin("builtin", globalStartLineNum, fph)).pgrm(p)
        , verboseFailures = true) match {
        case Failure(lbl, index, extra) =>
          val (lineNum, lineStr, col) = fph.getLineColAt(index)
          val globalLineNum = allLines.size + lineNum
          // output("/!\\ Parse error: " + extra.trace().msg +
          //   s" at l.$globalLineNum:$col: $lineStr")
          // output("Failed to parse library")
          (typer.Ctx.init, Map.empty)
        case Success(prog, index) => {
          val (_, (typeDefs, stmts)) = prog.desugared
          var ctx = typer.Ctx.init
          val raise: typer.Raise = d => ()
          var declared: Map[Str, typer.PolymorphicType] = Map.empty
          
          ctx = typer.processTypeDefs(typeDefs)(ctx, raise)
          val curBlockTypeDefs = ctx.tyDefs.iterator.map(_._2).toList
          typer.computeVariances(curBlockTypeDefs, ctx)
          
          stmts.foreach {
            // statement only declares a new term with its type
            // but does not give a body/definition to it
            case Def(isrec, nme, R(PolyType(tps, rhs)), isByname) =>
              val ty_sch = typer.PolymorphicType(0,
                typer.typeType(rhs)(ctx.nextLevel, raise,
                  vars = tps.map(tp => tp.name -> typer.freshVar(typer.noProv/*FIXME*/)(1)).toMap))
              ctx += nme.name -> typer.VarSymbol(ty_sch, nme)
              declared += nme.name -> ty_sch

            // statement is defined and has a body/definition
            case d @ Def(isrec, nme, L(rhs), isByname) =>
              val ty_sch = typer.typeLetRhs(isrec, nme, rhs)(ctx, raise)
              // statement does not have a declared type for the body
              // the inferred type must be used and stored for lookup
              declared.get(nme.name) match {
                // statement has a body but it's type was not declared
                // infer it's type and store it for lookup and type gen
                case N =>
                  ctx += nme.name -> typer.VarSymbol(ty_sch, nme)

                // statement has a body and a declared type
                // both are used to compute a subsumption (What is this??)
                // the inferred type is used to for ts type gen
                case S(sign) =>
                  ctx += nme.name -> typer.VarSymbol(sign, nme)
                  typer.uniState.subsume(ty_sch, sign)(ctx, raise, typer.TypeProvenance(d.toLoc, "def definition"))
              }

            case _ => ()
          }
          (ctx, declared)
        }
      }
    }
    
  val libStr =
"""
// common data types
type 'a list = Cons of 'a * 'a list | Nil
type 'a option = None | Some of 'a
//│ Defined type alias list[+'a]
//│ Defined class Cons[+'a]
//│ Defined class Nil
//│ Defined type alias option[+'a]
//│ Defined class None
//│ Defined class Some[+'a]
//│ Cons: ('a, list['a],) -> list['a]
//│ Nil: list[nothing]
//│ None: option[nothing]
//│ Some: 'a -> option['a]

// helper functions
let raise: 'a -> nothing
let fst: ('a * 'b) -> 'a
let snd: ('a * 'b) -> 'b
let print_int: int -> unit
let print_string: string -> unit
let print_endline: string -> unit
let string_of_int: int -> string
let failwith: string -> 'a
//│ raise: anything -> nothing
//│ fst: ('a, anything,) -> 'a
//│ snd: (anything, 'a,) -> 'a
//│ print_int: int -> unit
//│ print_string: string -> unit
//│ print_endline: string -> unit
//│ string_of_int: int -> string
//│ failwith: string -> nothing

// string
let (^): string -> string -> string
let String_length: string -> int
//│ ^: string -> string -> string
//│ String_length: string -> int

// arithmetic
let (+): int -> int -> int
let (-): int -> int -> int
let ( * ): int -> int -> int
let ( / ): int -> int -> int
let ( % ): int -> int -> int
let abs: int -> int
let mod: int -> int -> int
let succ: int -> int
let pred: int -> int
//│ +: int -> int -> int
//│ -: int -> int -> int
//│ *: int -> int -> int
//│ /: int -> int -> int
//│ %: int -> int -> int
//│ abs: int -> int
//│ mod: int -> int -> int
//│ succ: int -> int
//│ pred: int -> int

// comparison operators
let (<): 'a -> 'a -> bool
let (<=): 'a -> 'a -> bool
let (>): 'a -> 'a -> bool
let (>=): 'a -> 'a -> bool
let (<>): 'a -> 'a -> bool
let (==): 'a -> 'a -> bool
let (!=): 'a -> 'a -> bool
//│ <: anything -> anything -> bool
//│ <=: anything -> anything -> bool
//│ >: anything -> anything -> bool
//│ >=: anything -> anything -> bool
//│ <>: anything -> anything -> bool
//│ ==: anything -> anything -> bool
//│ !=: anything -> anything -> bool

// list
let List.length: 'a list -> int
let List.mem: 'a -> 'a list -> bool
let List.append: 'a list -> 'a list -> 'a list
let (@): 'a list -> 'a list -> 'a list
let List.map: ('a -> 'b) -> 'a list -> 'b list
let List.fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
let List.hd: 'a list -> 'a
let List.rev: 'a list -> 'a list
let List.combine: 'a list -> 'b list -> ('a * 'b) list
//│ List.length: list[?] -> int
//│ List.mem: anything -> list[?] -> bool
//│ List.append: list['a] -> list['a] -> list['a]
//│ @: list['a] -> list['a] -> list['a]
//│ List.map: ('a -> 'b) -> list['a] -> list['b]
//│ List.fold_left: ('a -> 'b -> 'a) -> 'a -> list['b] -> 'a
//│ List.hd: list['a] -> 'a
//│ List.rev: list['a] -> list['a]
//│ List.combine: list['a] -> list['b] -> list[('a, 'b,)]

// float
let (+.): float -> float -> float
let (-.): float -> float -> float
let ( *. ): float -> float -> float
let ( /. ): float -> float -> float
let ( ** ): float -> float -> float
let atan: float -> float
let sin: float -> float
let cos: float -> float
let tan: float -> float
//│ +.: float -> float -> float
//│ -.: float -> float -> float
//│ *.: float -> float -> float
//│ /.: float -> float -> float
//│ **: float -> float -> float
//│ atan: float -> float
//│ sin: float -> float
//│ cos: float -> float
//│ tan: float -> float

let (&&): bool -> bool -> bool
let (||): bool -> bool -> bool
let not: bool -> bool
//│ &&: bool -> bool -> bool
//│ ||: bool -> bool -> bool
//│ not: bool -> bool
"""
}