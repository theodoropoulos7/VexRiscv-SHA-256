package vexriscv.plugin

import spinal.core._
import spinal.lib._
import vexriscv.{DecoderService, Stageable, VexRiscv}

/**
  * The instructions are encoded by default as following :
  * ZZZZZ0SYYYYYXXXXX000DDDDD1010011
  *  Where :
  * - ZZZZZ is the register file source 3 (RS3)
  * - YYYYY is the register file source 2 (RS2)
  * - XXXXX is the register file source 1 (RS1)
  * - DDDDD is the register file destination
  * - S=0 mean choice(x,y,z) , S=1 mean Majority(x,y,z)
*/

case class SHA256ThreeOperands(encoding : MaskedLiteral = M"-----0-----------001-----1010011") extends Plugin[VexRiscv]{
  object IS_SHA256ThreeOperands extends Stageable(Bool)

  val mapping = new {
      def SEL_FUNCTION = 25   //Which sha256_3operand operation should be performed
    }

  //Callback to setup the plugin and ask for different services
  override def setup(pipeline: VexRiscv): Unit = {
    import pipeline.config._

    //Retrieve the DecoderService instance
    val decoderService = pipeline.service(classOf[DecoderService])

    //Specify the IS_SHA256ThreeOperands default value when instruction are decoded
    decoderService.addDefault(IS_SHA256ThreeOperands, False)

    //Specify the instruction decoding which should be applied when the instruction match the 'key' parttern
    decoderService.add(
      key = encoding,
      List(
        IS_SHA256ThreeOperands   -> True,
        REGFILE_WRITE_VALID      -> True, //Enable the register file write
        BYPASSABLE_EXECUTE_STAGE -> True, //Notify the hazard management unit that the instruction result is already accessible in the EXECUTE stage (Bypass ready)
        BYPASSABLE_MEMORY_STAGE  -> True, //Same as above but for the memory stage
        RS1_USE                  -> True, //Notify the hazard management unit that this instruction use the RS1 value
        RS2_USE                  -> True, //Notify the hazard management unit that this instruction use the RS2 value
		    RS3_USE                  -> True  //Notify the hazard management unit that this instruction use the RS3 value
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
      val x = execute.input(RS1).asUInt //32 bits UInt value of the regfile[RS1]
      val y = execute.input(RS2).asUInt
      val z = execute.input(RS3).asUInt
      val actionSel = input(INSTRUCTION)(mapping.SEL_FUNCTION).asUInt
     
     val rd= actionSel.mux(
        0 -> ((x & y) ^ (~x & z)),          //Choice(x,y,z) = ((x & y) ^ (~x & z))
        1 -> ((x & y) ^ (x & z) ^ (y & z))  //Majority(x,y,z) = ((x & y) ^ (x & z) ^ (y & z))
    )

	
      //When the instruction is a SHA256ThreeOperands one, then write the result into the register file data path.
      when(execute.input(IS_SHA256ThreeOperands)) {
        execute.output(REGFILE_WRITE_DATA) := rd.asBits
      }
    }
  }
}