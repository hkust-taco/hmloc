package mlscript

import fastparse._
import mlscript.utils._
import fastparse.Parsed.Failure
import fastparse.Parsed.Success

class OcamlParserTests extends org.scalatest.funsuite.AnyFunSuite {
  import OcamlParserTests._

  val fileContents = os.read(os.Path(filePath)).splitSane('\n').toList.toIndexedSeq;
  val processedBlock = OcamlParser.addTopLevelSeparators(fileContents);
  val fph = new FastParseHelpers(fileContents);
  val ans =
    parse(processedBlock.mkString, p => new OcamlParser(Origin(filePath, 0, fph)).pgrm(p), verboseFailures = true)
  ans match {
    case f: Failure => 
      println(f)
    case Success(pgrm, index) =>
      println(pgrm)
      println(PrettyPrintHelper.inspect(pgrm))
  }
}

object OcamlParserTests {
  val filePath = "/Users/twitu/Code/mlscript/shared/src/test/diff/analysis/test-parser.ocaml"
}
