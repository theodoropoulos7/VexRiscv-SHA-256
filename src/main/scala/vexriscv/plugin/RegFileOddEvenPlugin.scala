package vexriscv.plugin

import vexriscv._
import spinal.core._
import spinal.lib._

import scala.collection.mutable

// causes an redefined error if left in, I assume they are reused from RegFilePlugin ?
//trait RegFileReadKind
//object ASYNC extends RegFileReadKind
//object SYNC extends RegFileReadKind


class RegFileOddEvenPlugin(regFileReadyKind : RegFileReadKind,
                    zeroBoot : Boolean = false,
                    x0Init : Boolean = true,
                    writeRfInMemoryStage : Boolean = false,
                    readInExecute : Boolean = false,
                    syncUpdateOnStall : Boolean = true,
                    rv32e : Boolean = false,
                    withShadow : Boolean = false //shadow registers aren't transition hazard free
                   ) extends Plugin[VexRiscv] with RegFileService{
  import Riscv._

  override def readStage(): Stage = if(readInExecute) pipeline.execute else pipeline.decode

  override def setup(pipeline: VexRiscv): Unit = {
    import pipeline.config._
    val decoderService = pipeline.service(classOf[DecoderService])
    decoderService.addDefault(RS1_USE,False)
    decoderService.addDefault(RS2_USE,False)
    decoderService.addDefault(RS3_USE,False)
    decoderService.addDefault(REGFILE_WRITE_VALID,False)
    decoderService.addDefault(REGFILE_WRITE_VALID_ODD,False)
  }

  override def build(pipeline: VexRiscv): Unit = {
    import pipeline._
    import pipeline.config._

    val readStage = if(readInExecute) execute else decode
    val writeStage = if(writeRfInMemoryStage) memory else stages.last

    val numRegisters = if(rv32e) 16 else 32
    def clipRange(that : Range) = if(rv32e) that.tail else that

    val global = pipeline plug new Area{
      val regFileSize = if(withShadow) numRegisters * 2 else numRegisters
      val regFileEven = Mem(Bits(32 bits),regFileSize/2) addAttribute(Verilator.public)
      val regFileOdd = Mem(Bits(32 bits),regFileSize/2) addAttribute(Verilator.public)
      if(zeroBoot) regFileEven.init(List.fill(regFileSize)(B(0, 32 bits)))
      if(zeroBoot) regFileOdd.init(List.fill(regFileSize)(B(0, 32 bits)))

      val shadow = ifGen(withShadow)(new Area{
        val write, read, clear = RegInit(False)

        read  clearWhen(clear && !readStage.arbitration.isStuck)
        write clearWhen(clear && !writeStage.arbitration.isStuck)

        val csrService = pipeline.service(classOf[CsrInterface])
        csrService.w(0x7C0,2 -> clear, 1 -> read, 0 -> write)
      })
    }

    val notAES = (AES32ZKNE =/= decode.input(INSTRUCTION).asBits)
    val rdIndex = ((notAES) ? (decode.input(INSTRUCTION)(rdRange)) | (decode.input(INSTRUCTION)(rs1Range)))

    //Disable rd0 write in decoding stage
    when(rdIndex === 0) {
      decode.input(REGFILE_WRITE_VALID) := False
      decode.input(REGFILE_WRITE_VALID_ODD) := False
    }
    if(rv32e) when(decode.input(INSTRUCTION)(rdRange.head)) { // fixme for AES?
      decode.input(REGFILE_WRITE_VALID) := False
      decode.input(REGFILE_WRITE_VALID_ODD) := False
    }

    //Read register file
    readStage plug new Area{
      import readStage._

      //read register file
      val srcInstruction = regFileReadyKind match{
        case `ASYNC` => input(INSTRUCTION)
        case `SYNC` if !readInExecute =>  input(INSTRUCTION_ANTICIPATED)
        case `SYNC` if readInExecute =>   if(syncUpdateOnStall) Mux(execute.arbitration.isStuck, execute.input(INSTRUCTION), decode.input(INSTRUCTION)) else  decode.input(INSTRUCTION)
      }

      def shadowPrefix(that : Bits) = if(withShadow) global.shadow.read ## that else that
      val regFileReadAddress1 = U(shadowPrefix(srcInstruction(clipRange(Riscv.rs1Range))))
      val regFileReadAddress2 = U(shadowPrefix(srcInstruction(clipRange(Riscv.rs2Range))))
      val regFileReadAddress3 = (srcInstruction(Riscv.opcodeRange) === P_OPCODE) ? U(shadowPrefix(srcInstruction(clipRange(Riscv.rdRange)))) | U(shadowPrefix(srcInstruction(clipRange(Riscv.rs3Range))))
      val rfra1o = regFileReadAddress1 % 2
      val rfra2o = regFileReadAddress2 % 2
      val rfra3o = regFileReadAddress3 % 2

      val (rs1Data,rs2Data,rs3Data) = regFileReadyKind match{
        case `ASYNC` => (((rfra1o === 0) ? global.regFileEven.readAsync(regFileReadAddress1 >> 1) | global.regFileOdd.readAsync(regFileReadAddress1 >> 1)),
	     	     	 ((rfra2o === 0) ? global.regFileEven.readAsync(regFileReadAddress2 >> 1) | global.regFileOdd.readAsync(regFileReadAddress2 >> 1)),
	                 ((rfra3o === 0) ? global.regFileEven.readAsync(regFileReadAddress3 >> 1) | global.regFileOdd.readAsync(regFileReadAddress3 >> 1)))
        case `SYNC` =>
          val enable = if(!syncUpdateOnStall) !readStage.arbitration.isStuck else null
          (((rfra1o === 0) ? global.regFileEven.readSync(regFileReadAddress1 >> 1, enable) | global.regFileOdd.readSync(regFileReadAddress1 >> 1, enable)),
	   ((rfra2o === 0) ? global.regFileEven.readSync(regFileReadAddress2 >> 1, enable) | global.regFileOdd.readSync(regFileReadAddress2 >> 1, enable)),
	   ((rfra3o === 0) ? global.regFileEven.readSync(regFileReadAddress3 >> 1, enable) | global.regFileOdd.readSync(regFileReadAddress3 >> 1, enable)))
      }

      insert(RS1) := rs1Data
      insert(RS2) := rs2Data
      insert(RS3) := rs3Data
    }

    //Write register file
    writeStage plug new Area {
      import writeStage._

      val notAES = (AES32ZKNE =/= output(INSTRUCTION).asBits)
      val rdIndex = ((notAES) ? (output(INSTRUCTION)(clipRange(rdRange))) | (output(INSTRUCTION)(clipRange(rs1Range))))

      def shadowPrefix(that : Bits) = if(withShadow) global.shadow.write ## that else that
      val regFileWriteEven = global.regFileEven.writePort.addAttribute(Verilator.public).setName("lastStageRegFileWrite")
      regFileWriteEven.valid := ((rdIndex(0 downto 0) === B"1'b0") && output(REGFILE_WRITE_VALID)) && arbitration.isFiring
      regFileWriteEven.address := U(shadowPrefix(rdIndex)) >> 1
      regFileWriteEven.data := output(REGFILE_WRITE_DATA)
      
      val regFileWriteOdd = global.regFileOdd.writePort.addAttribute(Verilator.public).setName("lastStageRegFileWrite")
      regFileWriteOdd.valid := (((rdIndex(0 downto 0) === B"1'b1") && output(REGFILE_WRITE_VALID)) || (output(REGFILE_WRITE_VALID_ODD))) && arbitration.isFiring
      regFileWriteOdd.address := U(shadowPrefix(rdIndex)) >> 1
      regFileWriteOdd.data := ((rdIndex(0 downto 0) === B"1'b0") && output(REGFILE_WRITE_VALID_ODD)) ? output(REGFILE_WRITE_DATA_ODD) | output(REGFILE_WRITE_DATA)

      //Ensure no boot glitches modify X0
      if(!x0Init && zeroBoot) when(regFileWriteEven.address === 0){
        regFileWriteEven.valid := False
      }

      //CPU will initialise constant register zero in the first cycle
      if(x0Init) {
        val boot = RegNext(False) init (True)
        regFileWriteEven.valid setWhen (boot)
        when(boot) {
          regFileWriteEven.address := 0
          regFileWriteEven.data := 0
        }
      }
    }
  }
}
