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
    val readData  = Vec(Bits(dataWidth bits), lineBufferNum)

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
                           N: Int = 2,
                           M: Int = 3,
                           bufferRamCount: Int,
                           Hout: Int = 6,
                           channel: Int,
                           K: Int = 3, // kennel size
                           S: Int = 1, // kernel step
                           firstDelay: Int) extends Component {
    //---------------------------------------io-------------------------------------------------------------------------
    val fromWriter  = Array.fill(N)(slave(rowBufferWriterPorts(Hout = Hout, dataWidth = dataWidth, channel = channel)))
    val toBufferRam = master(bufferRamPorts(dataWidth = dataWidth * Hout, depth = channel, inLineNum = N, lineBufferNum = bufferRamCount))
    val toPe        = master(pePorts(outLineNum = M, dataWidth = dataWidth, Hout = Hout, K = K, channel = channel))

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
    toPe.data.payload.foreach(_ := 0)
    toBufferRam.wrEn := False // default: read
    toBufferRam.address := 0
    toBufferRam.sync := False
    toBufferRam.writeData.foreach(_ := 0)

    // counters
    val fillCount     = Counter(0, bufferRamCount / N - 1)
    val readHeadCount = RegInit(U(0, log2Up(1000) bits)) //TODO, The upper limit of readOutTimes is to be determined
    val reWriteRowCount = Counter(0, (M / N) - 1)
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
                        toBufferRam.sync := False
                    }
                }
            } elsewhen (transferEndReg) {
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
                        b := toBufferRam.readData.read(((readHeadCount + index + toPe.row.resize(readHeadCount.getWidth)) % bufferRamCount).resize(log2Up(bufferRamCount)))
                    }
                }
            } elsewhen (toPe.switch) { // Read the effective information of the entire bufferRam, now read the new lineBuffer
                goto(writeBufferRam)
                readHeadCount := readHeadCount + M
                toBufferRam.sync := True // chang the written lineBuffers
                writerTransferStart := True
            }
        }
        writeBufferRam.whenIsActive {
            when(writerWrEn) {
                writeLineBuffer()
                when(writerAddress === channel - 1) {
                    toBufferRam.sync := True
                    reWriteRowCount.increment()
                    when(reWriteRowCount.willOverflowIfInc) {
                        toBufferRam.sync := False
                        goto(readBufferRam)
                    }
                }
            } elsewhen (transferEndReg) {
                writerTransferStart := True
            }
        }
    }


    def writeLineBuffer() = {
        fromWriter.zip(toBufferRam.writeData).foreach { case (writer, wData) => wData := writer.dataToRam }
        toBufferRam.wrEn := writerWrEn
        toBufferRam.address := writerAddress
    }

    def getWriters(iw: Int, wh: Int, colBlockCount: Int, rowBlockCount: Int): Array[rowBufferWriter] = {
        Array.fill(N)(rowBufferWriter(width = dataWidth, Iw = iw, Wh = wh, channel = channel, Hout = Hout, colCount = colBlockCount, rowCount = rowBlockCount))
    }

    def getBufferRam: bufferRam = {
        bufferRam(width = dataWidth * Hout, writeCount = bufferRamCount / N, depth = channel, inLineNum = N)
    }

}


case class ctrlTestTop(n: Int, m: Int, hout: Int, width: Int, bufferRamCount: Int, channel: Int, k: Int, s: Int, delay: Int, iw: Int, wh: Int, col: Int, row: Int) extends Component {
    val Ctrl      = writeControl(dataWidth = width, N = n, M = m, bufferRamCount = bufferRamCount, Hout = hout, channel = channel, K = k, S = s, firstDelay = delay)
    val writers   = Ctrl.getWriters(iw = iw, wh = wh, colBlockCount = col, rowBlockCount = row)
    val BufferRam = Ctrl.getBufferRam
    writers.zip(Ctrl.fromWriter).foreach { case (w, c) => w.io.toControl <> c }
    BufferRam.io <> Ctrl.toBufferRam
    val dataIn = Array.fill(n)(slave(Stream(Vec(Bits(width bits), iw * wh))))
    dataIn.zip(writers).foreach { case (i, w) => i <> w.io.dataIn }

    val peOut = master(pePorts(outLineNum = m, dataWidth = width, Hout = hout, K = k, channel = channel))
    peOut <> Ctrl.toPe
}

object ctrlTestTopRTL extends App {
    SpinalConfig(
        defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = HIGH)
    ).generateVerilog(new ctrlTestTop(n = 2, m = 5, hout = 9, width = 8, bufferRamCount = 8, channel = 7, k = 3, s = 1, delay = 5, iw = 2, wh = 2, col = 5, row = 4))
}

object ctrlTestTopSim extends App {
    import lineBuffer._

    simNow(n = 2, m = 5, hout = 9, width = 8, bufferRamCount = 8, channel = 7, k = 3, s = 1, delay = 5, iw = 2, wh = 2, col = 5, row = 4)
//    simNow(n = 5, m = 7, k = 5, bufferRamCount = 15, channel = 13, hout = 13, iw=4, wh = 4, col = 4, row = 4, delay = 5, s = 1, width = 8)

    def simNow(n: Int, m: Int, hout: Int, width: Int, bufferRamCount: Int, channel: Int, k: Int, s: Int, delay: Int, iw: Int, wh: Int, col: Int, row: Int) = {
        SimConfig.withWave.withConfig(SpinalConfig(
            defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = HIGH),
            defaultClockDomainFrequency = FixedFrequency(100 MHz)
        )).compile {
            val dut = new ctrlTestTop(n, m, hout, width, bufferRamCount, channel, k, s, delay, iw, wh, col, row)
            dut.writers.foreach(_.io.toControl.dataToRam simPublic())
            dut.Ctrl.readHeadCount.simPublic()
            dut.Ctrl.reWriteRowCount
            dut.BufferRam.counter simPublic()
            dut
        }.doSim { dut =>
            import dut._

            val bufferRamFillData = getRows(colNumOfBlocks = dut.col * dut.iw, rowNumOfBlocks = dut.row * dut.wh, Hout = dut.hout, channel = dut.channel, width = dut.width, rowNum = dut.bufferRamCount)
            val bufferRamData     = Array.fill(dut.bufferRamCount)(Array.fill(dut.channel)(Array.fill(dut.hout)(BigInt(0))))
//            bufferRamData.foreach(print2D(_))
            clockDomain.forkStimulus(10)
            dut.init
            println("--------------------------------fill-------------------------------------------------")
            dut.writeAndRead(bufferRamFillData, bufferRamData)
//            println("assert done! fill -> read right")
            for(i <- 0 until 100){
                val newBufferRamData = getRows(colNumOfBlocks = dut.col * dut.iw, rowNumOfBlocks = dut.row * dut.wh, Hout = dut.hout, channel = dut.channel, width = dut.width, rowNum = dut.m / dut.n * dut.n)
                dut.writeAndRead(newBufferRamData, bufferRamData)
//                println("assert done! fill -> read " + ("-> write -> read" * (i + 1)) + "right")
            }
            clockDomain.waitSampling(10)

        }

    }

    implicit class ctrlSIm(dut: ctrlTestTop) {

        import dut.{dataIn, peOut, clockDomain}
        import lineBuffer._

        def init = {
            dataIn.foreach(_.valid #= false)
            dataIn.foreach(_.payload.foreach(_ #= 0))
            peOut.data.ready #= false
            peOut.switch #= false
            peOut.row #= 0
            peOut.Channel #= 0
            clockDomain.waitSampling()
        }



        def readBufferRam(bufferRamData: Array[Array[Array[BigInt]]]) = {
            peOut.data.ready #= true
            peOut.switch #= false
            val readHeadCount = dut.Ctrl.readHeadCount.toInt
            for (r <- 0 until dut.row) {
                for (c <- 0 until dut.channel) {
                    peOut.row #= r
                    peOut.Channel #= c
                    clockDomain.waitSampling()
                    //sampling the readOut of bufferRam
//                    printf("row:     %2d\n", peOut.row.toBigInt)
//                    printf("channel: %2d\n", peOut.Channel.toBigInt)
                    val bufferRamOut = dut.peOut.data.payload.map(_.toBigInt).map(_.toString(2)).map(s => prefixZero(s, dut.hout * dut.width)).map(_.grouped(dut.width)).map(_.toArray).map(_.map(s => BigInt(s, 2))).toArray
//                    print2D(bufferRamOut)
                    bufferRamOut.zipWithIndex.foreach{case(portData, index)=>
                        portData.reverse.zipWithIndex.foreach{case(num, numIndex)=>
                            assert(bufferRamData((readHeadCount  + index + r) % dut.bufferRamCount)(c)(numIndex) ==  num)
                        }
                    }
                }
            }
            peOut.switch #= true
            peOut.data.ready #= false
            clockDomain.waitSampling()
            peOut.switch #= false
            clockDomain.waitSampling()
        }
        def write(newBufferRemData: Array[Array[Array[BigInt]]], bufferRamData: Array[Array[Array[BigInt]]])={
            newBufferRemData.grouped(dut.n).foreach{ nRows=>
                clockDomain.waitSampling()
                writeRows(nRows, bufferRamData)
            }
        }

        def writeAndRead(newBufferRamData: Array[Array[Array[BigInt]]], bufferRamData: Array[Array[Array[BigInt]]])={
            println("--------------------------------write-------------------------------------------------")
            dut.write(newBufferRamData, bufferRamData)
//            bufferRamData.foreach(print2D(_))
            println("--------------------------------read-------------------------------------------------")
            dut.readBufferRam(bufferRamData)
        }

        def writeRows(nRow: Array[Array[Array[BigInt]]], bufferRamData: Array[Array[Array[BigInt]]]) = {
            val counter = dut.BufferRam.counter.toInt
//            nRow.foreach(i => print2D(i))
            var linesOuted = 0
            for (r <- 0 until dut.row) {
                dut.dataIn.foreach(_.valid #= true)
                for (c <- 0 until dut.col) {
                    // write the block matrix of row
                    nRow.zip(dut.dataIn).foreach { case (rowMatrix, ports)=>
                        ports.payload.zipWithIndex.foreach { case (sig, sigIndex) =>
                            sig #= rowMatrix(r * dut.wh + sigIndex / dut.wh)(c * dut.iw + (sigIndex % dut.iw))
                        }
                    }
                    clockDomain.waitSampling()
                }
                // wait for the wh channels to be read out
                dut.dataIn.foreach(_.valid #= false)
                for (w<- 0 until dut.wh) {
                    clockDomain.waitSampling()
                    // sampling the readData
                    val datatoRam: Array[Array[BigInt]] = dut.writers.map(_.io.toControl.dataToRam.toBigInt.toString(2)).map { d => prefixZero(d, dut.width * dut.hout) }.map(_.grouped(dut.width).map(BigInt(_, 2))).map(_.toArray)
//                    print2D(datatoRam)
                    if(linesOuted < dut.channel){
                        for(i <- 0 until dut.n ){
                            bufferRamData(counter * dut.n + i)(r * dut.wh + w) = datatoRam(i).reverse
                        }
                    }
                    linesOuted += 1
                }
            }
//            println("mem")
//            bufferRamData.foreach(print2D(_))
        }

        def prefixZero(readOut: String, L: Int): String = {
            if (readOut.length < L)
                ("0" * (L - readOut.length)) + readOut
            else
                readOut
        }
    }
}
