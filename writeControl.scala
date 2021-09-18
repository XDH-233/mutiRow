package mutiRow

import spinal.core._
import spinal.core.internals.Operator
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._


case class pePorts(outLineNum: Int, dataWidth: Int, Hout: Int, K: Int, channel: Int) extends Bundle with IMasterSlave {
    val data    = Stream(Vec(Bits(Hout * dataWidth bits), outLineNum))
    val row     = UInt(log2Up(K) bits)
    val Channel = UInt(log2Up(channel) bits)
    val switch  = Bool()

    override def asMaster(): Unit = {
        in(row, Channel, switch)
        master(data)
    }
}


case class bufferRamPorts(dataWidth: Int, depth: Int, inLineNum: Int, lineBufferNum: Int) extends Bundle with IMasterSlave {
    val wrEn      = Bool() // 0 -> write, 1 -> read
    val writeData = Vec(Bits(dataWidth bits), inLineNum)
    val address   = UInt(log2Up(depth) bits)
    val sync      = Bool()
    val readData  = Vec(Bits(dataWidth  bits), lineBufferNum)

    override def asMaster(): Unit = {
        in(readData)
        out(wrEn, writeData, address, sync)
    }
}

case class rowBufferWriterPorts(Hout: Int, dataWidth: Int, channel: Int) extends Bundle with IMasterSlave {
    val transferStart = Bool()
    val transferEnd   = Bool()
    val dataToRam     = Bits(Hout * dataWidth bits)
    val wrEn          = Bool()
    val address       = UInt(log2Up(channel) bits)

    override def asMaster(): Unit = {
        out(transferEnd, dataToRam, wrEn, address)
        in(transferStart)
    }
}

case class writeControl(
                           dataWidth: Int = 8,
                           inLineNum: Int = 2,
                           outLineNum: Int = 3,
                           bufferRamCount: Int,
                           Hout: Int = 6,
                           channel: Int,
                           K: Int = 3, // kennel size
                           S: Int = 1, // kernel step
                           firstDelay: Int) extends Component {
    //---------------------------------------io-------------------------------------------------------------------------
    val fromWriter  = Array.fill(inLineNum)(slave(rowBufferWriterPorts(Hout = Hout, dataWidth = dataWidth, channel = channel)))
    val toBufferRam = master(bufferRamPorts(dataWidth = dataWidth * Hout, depth = channel, inLineNum = inLineNum, lineBufferNum = bufferRamCount))
    val toPe        = master(pePorts(outLineNum = outLineNum, dataWidth = dataWidth, Hout = Hout, K = K, channel = channel))

    // These signals of writer can be managed uniformly
    val writerTransferStart = Bool()
    val writerTransferEnd   = Bool()
    val writerAddress       = UInt(log2Up(channel) bits)
    val writerWrEn          = Bool()
    fromWriter.foreach(_.transferStart := writerTransferStart)
    writerTransferEnd := fromWriter(0).transferEnd
    writerAddress := fromWriter(0).address
    writerWrEn := fromWriter(0).wrEn

    // some output ports' default state
    writerTransferStart := False
    toPe.data.valid := False
    toPe.data.payload.foreach(_:=0)
    toBufferRam.wrEn := False // default: read
    toBufferRam.address := 0
    toBufferRam.sync := False
    toBufferRam.writeData.foreach(_ := 0)

    // counters
    val fillCount    = Counter(0, bufferRamCount / inLineNum - 1)
    val writtenCount = RegInit(U(0, log2Up(1000) bits)) //TODO, The upper limit of readOutTimes is to be determined
    val reWriteCount = Counter(0, (outLineNum / inLineNum) - 1)

    val transferEndReg = RegNext(writerTransferEnd)


    //------------------------------------stateMachine------------------------------------------------------------------
    val FSM = new StateMachine {
        val idle           = new State with EntryPoint
        val fillBufferRam  = new State
        val writeBufferRam = new State
        val readBufferRam  = new State
        idle.whenIsActive {
            writerTransferStart := True
            goto(fillBufferRam)
        }
        fillBufferRam.whenIsActive {
            when(writerWrEn) {
                writeLineBuffer()
                when(writerAddress === channel - 1) {
                    toBufferRam.sync := True
                    fillCount.increment()
                    when(fillCount.willOverflowIfInc) { // Filled up all the bufferRam
                        goto(readBufferRam) //TODO Ignore firstDelay for now
                    }
                }
            }elsewhen(transferEndReg){
                writerTransferStart := True
            }
        }
        readBufferRam.whenIsActive {
            toBufferRam.wrEn := False // read
            toBufferRam.address := toPe.Channel
            when(toPe.data.ready) {
                toPe.data.valid := True
                toPe.data.payload.zipWithIndex.foreach {
                    case (b, index) => {
                        b := toBufferRam.readData.read(((writtenCount + index + toPe.row.resize(writtenCount.getWidth)) % bufferRamCount).resize(log2Up(bufferRamCount)))
                    }
                }
            } elsewhen (toPe.switch) {      // Read the effective information of the entire bufferRam, now read the new lineBuffer
                goto(writeBufferRam)
                writtenCount := writtenCount + inLineNum
                toBufferRam.sync := True    // chang the written lineBuffers
                writerTransferStart := True
            }
        }
        writeBufferRam.whenIsActive {
            when(writerWrEn) {
                writeLineBuffer()
                when(writerAddress === channel - 1) {
                    reWriteCount.increment()
                    when(reWriteCount.willOverflowIfInc) {
                        goto(readBufferRam)
                    }
                }
            }
        }
    }


    def writeLineBuffer() = {
        fromWriter.zip(toBufferRam.writeData).foreach { case (writer, wData) => wData := writer.dataToRam }
        toBufferRam.wrEn := writerWrEn
        toBufferRam.address := writerAddress
    }
}


case class ctrlTestTop() extends Component {
    val Ctrl      = writeControl(inLineNum = 2, outLineNum = 5, Hout = 9, dataWidth = 8, bufferRamCount = 8, channel = 7, K = 3, S = 1, firstDelay = 5)
    val writers   = Array.fill(2)(rowBufferWriter(width = 8, Iw = 2, Wh = 2, channel = 7, Hout = 9, colCount = 5, rowCount = 4))
    val BufferRam = bufferRam(width = 8 * 9, num = 4, depth = 7, inLineNum = 2)
    Ctrl.fromWriter.zip(writers).foreach { case (c, w) => c <> w.io.toControl }
    Ctrl.toBufferRam <> BufferRam.io
    val dataIn = Array.fill(2)(slave Stream (Vec(Bits(8 bits), 2 * 2)))
    dataIn.zip(writers).foreach { case (i, w) => i <> w.io.dataIn }
    val dataOut = master(Stream(Vec(Bits(8 * 9 bits), 5)))
    dataOut <> Ctrl.toPe.data
    val row = in UInt(log2Up(3) bits)
    val Channel = in UInt(log2Up(6) bits)
    val switchPE = in Bool()
    row <> Ctrl.toPe.row
    Channel <> Ctrl.toPe.Channel
    switchPE <> Ctrl.toPe.switch
}

object ctrlTestTopRTL extends App {
    SpinalConfig(
        defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = HIGH)
    ).generateVerilog(new ctrlTestTop())
}


object ctrlTestTopSim extends App{
    implicit class ctrlSim(dut: ctrlTestTop){
        def init={
            dut.dataIn.foreach(_.valid#=false)
            dut.dataIn.foreach(_.payload.foreach(_#=0))
            dut.dataOut.ready #= false
            dut.row #= 0
            dut.Channel #= 0
            dut.switchPE #= false
            dut.clockDomain.waitSampling(3)
        }
    }
    import lineBuffer._
    SimConfig.withWave.withConfig(SpinalConfig(
        defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = HIGH),
        defaultClockDomainFrequency = FixedFrequency(100 MHz)
    )).compile{
        val dutCompile = new ctrlTestTop()
        dutCompile.writers(0).io.toControl.transferStart.simPublic()
        dutCompile
    }.doSim { dut =>
        import dut._
        clockDomain.forkStimulus(10)
        val testCases = Array.fill(10)(getLineBuffer(rowNum = 7, colNum = 9, channel = 6, Hout = 8, width = 8))
        dut.init
        for(i <- 0 until 140){
            dataIn.foreach(_.valid #= true)
            dataIn.foreach(_.payload.randomize())
            clockDomain.waitSampling()
        }

        dataOut.ready #= true
        switchPE #= false
        clockDomain.waitSampling()
        for(k <- 0 until 3){
            for(c <- 0 until 7){
                row #= k
                Channel #= c
                clockDomain.waitSampling()
            }
        }
        dataOut.ready #= false
        switchPE #= true
        clockDomain.waitSampling(2)
        for(i <- 0 until 50){
            dataIn.foreach(_.valid #= true)
            dataIn.foreach(_.payload.randomize())
            clockDomain.waitSampling()
        }


    }
}