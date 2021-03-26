package vexriscv.plugin

import spinal.core._
import spinal.lib._
import vexriscv.{DecoderService, Stageable, VexRiscv}


case class SHA256(encoding : MaskedLiteral = M"0001000000-------001-----0010011") extends Plugin[VexRiscv]{
  //Define the concept of IS_SHA256 signals, which specify if the current instruction is destined for ths plugin
  object IS_SHA256 extends Stageable(Bool)

  val mapping = new {
      def SEL_ACTION = 20   //Which sha256 operation should be performed
    }

  //Callback to setup the plugin and ask for different services
  override def setup(pipeline: VexRiscv): Unit = {
    import pipeline.config._

    //Retrieve the DecoderService instance
    val decoderService = pipeline.service(classOf[DecoderService])

    //Specify the IS_SHA256 default value when instruction are decoded
    decoderService.addDefault(IS_SHA256, False)

    //Specify the instruction decoding which should be applied when the instruction match the 'key' parttern
    decoderService.add(
      key = encoding,
      List(
        IS_SHA256                -> True,
        REGFILE_WRITE_VALID      -> True, //Enable the register file write
        BYPASSABLE_EXECUTE_STAGE -> True, //Notify the hazard management unit that the instruction result is already accessible in the EXECUTE stage (Bypass ready)
        BYPASSABLE_MEMORY_STAGE  -> True, //Same as above but for the memory stage
        RS1_USE                  -> True //Notify the hazard management unit that this instruction use the RS1 value
      )
    )
  }

  override def build(pipeline: VexRiscv): Unit = {
    import pipeline._
    import pipeline.config._

    //Add a new scope on the execute stage (used to give a name to signals)
    execute plug new Area {
      import execute._
      //Define some signals used internally to the plugin
      val rs1 = execute.input(RS1).asUInt //32 bits UInt value of the regfile[RS1]
      val actionSel = input(INSTRUCTION)(mapping.SEL_ACTION, 2 bits).asUInt
     
     val rd= actionSel.mux(
        0 -> (rs1.rotateRight(2) ^ rs1.rotateRight(13) ^ rs1.rotateRight(22)),  //Sum0
        1 -> (rs1.rotateRight(6) ^ rs1.rotateRight(11) ^ rs1.rotateRight(25)),  //Sum1
        2 -> (rs1.rotateRight(7) ^ rs1.rotateRight(18) ^ (rs1 |>> 3)),          //Sigma0
        3 -> (rs1.rotateRight(17) ^ rs1.rotateRight(19) ^ (rs1 |>> 10))         //Sigma1
    )

	
      //When the instruction is a SHA256 one, then write the result into the register file data path.
      when(execute.input(IS_SHA256)) {
        execute.output(REGFILE_WRITE_DATA) := rd.asBits
      }
    }
  }
}