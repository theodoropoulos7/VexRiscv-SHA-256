package vexriscv.plugin

import vexriscv._
import spinal.core._
import spinal.lib._

trait HazardService{
  def hazardOnExecuteRS : Bool
}

class HazardSimplePlugin(bypassExecute : Boolean = false,
                         bypassMemory: Boolean = false,
                         bypassWriteBack: Boolean = false,
                         bypassWriteBackBuffer : Boolean = false,
                         pessimisticUseSrc : Boolean = false,
                         pessimisticWriteRegFile : Boolean = false,
                         pessimisticAddressMatch : Boolean = false) extends Plugin[VexRiscv] with HazardService{
  import Riscv._


  def hazardOnExecuteRS = {
    if(pipeline.service(classOf[RegFileService]).readStage() == pipeline.execute) pipeline.execute.arbitration.isStuckByOthers else False //TODO not so nice
  }

  override def setup(pipeline: VexRiscv): Unit = {
    import pipeline.config._
    val decoderService = pipeline.service(classOf[DecoderService])
    decoderService.addDefault(HAS_SIDE_EFFECT, False) //TODO implement it in each plugin
  }

  override def build(pipeline: VexRiscv): Unit = {
    import pipeline._
    import pipeline.config._
    val src0Hazard = False
    val src1Hazard = False
    val src2Hazard = False

    val readStage = service(classOf[RegFileService]).readStage()

    def trackHazardWithStage(stage : Stage,bypassable : Boolean, runtimeBypassable : Stageable[Bool]): Unit ={
      val notAES = (AES32ZKNE =/= stage.input(INSTRUCTION).asBits) && (SM4ZKS  =/= stage.input(INSTRUCTION).asBits)
      val isPDouble = (P_DOUBLE === stage.input(INSTRUCTION).asBits)
      val rdIndex = ((notAES) ? (stage.input(INSTRUCTION)(rdRange)) | (stage.input(INSTRUCTION)(rs1Range)))
      val rdIndexSecond = isPDouble ? (rdIndex ^ B"5'b00001") | B"5'b00000"
      val regFileReadAddress3 = (readStage.input(INSTRUCTION)(Riscv.opcodeRange) === P_OPCODE) ? readStage.input(INSTRUCTION)(rdRange) | readStage.input(INSTRUCTION)(rs3Range)

      val runtimeBypassableValue = if(runtimeBypassable != null) stage.input(runtimeBypassable) else True
      val addr0Match = if(pessimisticAddressMatch) True else ((rdIndex =/= B"5'b00000") && (rdIndex === readStage.input(INSTRUCTION)(rs1Range)))
      val addr1Match = if(pessimisticAddressMatch) True else ((rdIndex =/= B"5'b00000") && (rdIndex === readStage.input(INSTRUCTION)(rs2Range)))
      val addr2Match = if(pessimisticAddressMatch) True else ((rdIndex =/= B"5'b00000") && (rdIndex === regFileReadAddress3))
      val addr0MatchSecond = if(pessimisticAddressMatch) True else ((rdIndexSecond =/= B"5'b00000") && (rdIndexSecond === readStage.input(INSTRUCTION)(rs1Range)))
      val addr1MatchSecond = if(pessimisticAddressMatch) True else ((rdIndexSecond =/= B"5'b00000") && (rdIndexSecond === readStage.input(INSTRUCTION)(rs2Range)))
      val addr2MatchSecond = if(pessimisticAddressMatch) True else ((rdIndexSecond =/= B"5'b00000") && (rdIndexSecond === regFileReadAddress3))
      when(stage.arbitration.isValid && stage.input(REGFILE_WRITE_VALID)) {
        if (bypassable) {
          when(runtimeBypassableValue) {
            when(addr0Match) {
              readStage.input(RS1) := stage.output(REGFILE_WRITE_DATA)
            }
            when(addr1Match) {
              readStage.input(RS2) := stage.output(REGFILE_WRITE_DATA)
            }
            when(addr2Match) {
              readStage.input(RS3) := stage.output(REGFILE_WRITE_DATA)
            }
          }
        }
      }
      when(stage.arbitration.isValid && isPDouble && stage.input(REGFILE_WRITE_VALID_ODD)) {
        if (bypassable) {
          when(runtimeBypassableValue) {
            when(addr0MatchSecond) {
              readStage.input(RS1) := stage.output(REGFILE_WRITE_DATA_ODD)
            }
            when(addr1MatchSecond) {
              readStage.input(RS2) := stage.output(REGFILE_WRITE_DATA_ODD)
            }
            when(addr2MatchSecond) {
              readStage.input(RS3) := stage.output(REGFILE_WRITE_DATA_ODD)
            }
          }
        }
      }
      // REGFILE_WRITE_VALID should be enough, REGFILE_WRITE_VALID_ODD should never be true unless REGFILE_WRITE_VALID is also true
      when(stage.arbitration.isValid && (if(pessimisticWriteRegFile) True else stage.input(REGFILE_WRITE_VALID))) {
        when((Bool(!bypassable) || !runtimeBypassableValue)) {
          when(addr0Match || addr0MatchSecond) {
            src0Hazard := True
          }
          when(addr1Match || addr1MatchSecond) {
            src1Hazard := True
          }
          when(addr2Match || addr2MatchSecond) {
            src2Hazard := True
          }
        }
      }
    }


    val writeBackWrites = Flow(cloneable(new Bundle{
      val address = Bits(5 bits)
      val data = Bits(32 bits)
    }))
    val notAES = (AES32ZKNE =/= stages.last.output(INSTRUCTION).asBits) && (SM4ZKS =/= stages.last.output(INSTRUCTION).asBits)
    val rdIndex = ((notAES) ? (stages.last.output(INSTRUCTION)(rdRange)) | (stages.last.output(INSTRUCTION)(rs1Range)))
    val regFileReadAddress3 = (readStage.input(INSTRUCTION)(Riscv.opcodeRange) === P_OPCODE) ? readStage.input(INSTRUCTION)(rdRange) | readStage.input(INSTRUCTION)(rs3Range)
    writeBackWrites.valid := stages.last.output(REGFILE_WRITE_VALID) && stages.last.arbitration.isFiring
    writeBackWrites.address := rdIndex
    writeBackWrites.data := stages.last.output(REGFILE_WRITE_DATA)
    val writeBackBuffer = writeBackWrites.stage()

    val addr0Match = if(pessimisticAddressMatch) True else writeBackBuffer.address === readStage.input(INSTRUCTION)(rs1Range)
    val addr1Match = if(pessimisticAddressMatch) True else writeBackBuffer.address === readStage.input(INSTRUCTION)(rs2Range)
    val addr2Match = if(pessimisticAddressMatch) True else writeBackBuffer.address === regFileReadAddress3
    when(writeBackBuffer.valid) {
      if (bypassWriteBackBuffer) {
        when(addr0Match) {
          readStage.input(RS1) := writeBackBuffer.data
        }
        when(addr1Match) {
          readStage.input(RS2) := writeBackBuffer.data
        }
        when(addr2Match) {
          readStage.input(RS3) := writeBackBuffer.data
        }
      } else {
        when(addr0Match) {
          src0Hazard := True
        }
        when(addr1Match) {
          src1Hazard := True
        }
        when(addr2Match) {
          src2Hazard := True
        }
      }
    }

    if(withWriteBackStage) trackHazardWithStage(writeBack,bypassWriteBack,null)
    if(withMemoryStage)    trackHazardWithStage(memory   ,bypassMemory, if(stages.last == memory) null else BYPASSABLE_MEMORY_STAGE)
    if(readStage != execute) trackHazardWithStage(execute  ,bypassExecute , if(stages.last == execute) null else BYPASSABLE_EXECUTE_STAGE)


    if(!pessimisticUseSrc) {
      when(!readStage.input(RS1_USE)) {
        src0Hazard := False
      }
      when(!readStage.input(RS2_USE)) {
        src1Hazard := False
      }
      when(!readStage.input(RS3_USE)) {
        src2Hazard := False
      }
    }

    when(readStage.arbitration.isValid && (src0Hazard || src1Hazard | src2Hazard)){
      readStage.arbitration.haltByOther := True
    }
  }
}


class NoHazardPlugin extends Plugin[VexRiscv] with HazardService {
  override def build(pipeline: VexRiscv): Unit = {}

  def hazardOnExecuteRS = False
}