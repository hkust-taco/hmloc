package mlscript

import fastparse._
import mlscript.utils._, shorthands._
import mlscript._
import scala.annotation.nowarn

class OcamlParserTests extends org.scalatest.funsuite.AnyFunSuite {
  import OcamlParserTests._

  val fileContents = os.read(os.Path(filePath)).splitSane('\n').toList.toIndexedSeq;
  val processedBlock = OcamlParser.addTopLevelSeparators(fileContents);
  val fph = new FastParseHelpers(fileContents);
  val ans =
    parse(processedBlock.mkString, p => new OcamlParser(Origin(filePath, 0, fph)).pgrm(p), verboseFailures = true)
  println(ans)
}

object OcamlParserTests {
  val filePath = "/Users/twitu/Code/mlscript/shared/src/test/diff/analysis/test.ocaml"
}
