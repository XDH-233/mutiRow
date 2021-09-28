package mutiRow

import spinal.core._
import spinal.core.internals.Operator
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._


case class pePorts(M: Int, dataWidth: Int, Hout: Int, K: Int, channel: Int) extends Bundle with IMasterSlave {
    val data    = Stream(Vec(Bits(Hout * dataWidth bits), M))
    val row     = UInt(log2Up(K) bits)
    val Channel = UInt(log2Up(channel) bits)
    val switch  = Bool()

    override def asMaster(): Unit = {
        in(row, Channel, switch)
        master(data)
    }
}

object pePorts {
    def apply(ctrl: writeControl): pePorts = {
        pePorts(ctrl.M, ctrl.dataWidth, ctrl.Hout, ctrl.K, ctrl.channel)
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
                           F: Int) extends Component {
    require(F % N == 0 && F >= scala.math.ceil((K + M - 1) / N).toInt * N && F <= bufferRamCount, "wrong fillRowNum !")
    //---------------------------------------io-------------------------------------------------------------------------
    val fromWriter    = Array.fill(N)(slave(rowBufferWriterPorts(this)))
    val toBufferRamRW = master(bufferRamRWPorts(this))
    val toPe          = master(pePorts(this))

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
    toBufferRamRW.wrEn := False // default: read
    toBufferRamRW.writeAddress := 0
    toBufferRamRW.readAddress := 0
    toBufferRamRW.sync := False
    toBufferRamRW.writeData.foreach(_ := 0)

    // counters
    val fillCount      = Counter(0, F / N - 1)
    val transferEndReg = RegNext(writerTransferEnd)

    val writeHead, readHead, readTail = Reg(UInt(16 bits)) init (0)
    readTail := readHead + K + M - 2
    //-------------------------------------------------new stateMachine-------------------------------------------------
    val FSM = new StateMachine {
        val idle         = new State with EntryPoint // 1
        val fill         = new State // 2
        val readAndWrite = new State // 3
        val Switch       = new State // 4

        idle.whenIsActive {
            writerTransferStart := True
            goto(fill)
        }
        fill.whenIsActive {
            when(writerWrEn) {
                wrightAssign()
                when(writerAddress === channel - 1) {
                    toBufferRamRW.sync := True
                    fillCount.increment()
                    writeHead := writeHead +  N
                    when(fillCount.willOverflowIfInc) {
                        when(readTail < writeHead){
                            writerTransferStart := True
                        }
                        goto(readAndWrite)
                    }
                }
            } elsewhen (transferEndReg) {
                writerTransferStart := True
            }
        }

        readAndWrite.whenIsActive {
            when(readTail < writeHead) {
                readBufferRam()
            }
            when(writeHead + N <= readHead + bufferRamCount) {
                writeBufferRam()
            }

        }
        Switch.whenIsActive {
            writerTransferStart := True
            goto(readAndWrite)
        }

        def readBufferRam() = {
            // read part
            when(toPe.data.ready) {
                toBufferRamRW.readAddress := toPe.Channel
                toPe.data.valid := True
                toPe.data.payload.zipWithIndex.foreach { case (peData, index) =>
                    peData := toBufferRamRW.readData.read(((readHead + toPe.row.resize(readHead.getWidth) + index) % bufferRamCount).resize(log2Up(bufferRamCount)))
                }
            } elsewhen (toPe.switch) {
                readHead := readHead + M
                goto(Switch)
            }
        }

        def wrightAssign() = {
            fromWriter.zip(toBufferRamRW.writeData).foreach { case (writer, wData) => wData := writer.dataToRam }
            toBufferRamRW.wrEn := writerWrEn
            toBufferRamRW.writeAddress := writerAddress
        }

        def writeBufferRam() = {
            when(writerWrEn) {
                wrightAssign()
                when(writerAddress === channel - 1) {
                    toBufferRamRW.sync := True
                    writeHead := writeHead + N
                }
            } elsewhen (transferEndReg) {
                writerTransferStart := True
            }
        }
    }


    def getWriters(iw: Int, wh: Int, colBlockCount: Int, rowBlockCount: Int): Array[rowBufferWriter] = {
        Array.fill(N)(rowBufferWriter(width = dataWidth, Iw = iw, Wh = wh, channel = channel, Hout = Hout, colCount = colBlockCount, rowCount = rowBlockCount))
    }

    def getBufferRam: bufferRam = {
        bufferRam(width = dataWidth * Hout, writeCount = bufferRamCount / N, depth = channel, inLineNum = N)
    }

    def getBufferRamRW: bufferRamRW = {
        bufferRamRW(dataWidth = dataWidth * Hout, inCount = N, bufferRamCount = bufferRamCount, depth = channel)
    }

}


case class ctrlTestTop(n: Int, m: Int, hout: Int, width: Int, bufferRamCount: Int, channel: Int, k: Int, F: Int, iw: Int, wh: Int, col: Int, row: Int) extends Component {
    val Ctrl      = writeControl(dataWidth = width, N = n, M = m, bufferRamCount = bufferRamCount, Hout = hout, channel = channel, K = k, F = F)
    val writers   = Ctrl.getWriters(iw = iw, wh = wh, colBlockCount = col, rowBlockCount = row)
    val BufferRam = Ctrl.getBufferRamRW
    writers.zip(Ctrl.fromWriter).foreach { case (w, c) => w.toControl <> c }
    BufferRam.io <> Ctrl.toBufferRamRW
    val dataIn = Array.fill(n)(slave(Stream(Vec(Bits(width bits), iw * wh))))
    dataIn.zip(writers).foreach { case (i, w) => i <> w.dataIn }

    val peOut = master(pePorts(Ctrl))
    peOut <> Ctrl.toPe
}

object ctrlTestTopRTL extends App {
    SpinalConfig(
        defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = HIGH)
        ).generateVerilog(new ctrlTestTop(n = 2, m = 5, hout = 9, width = 8, bufferRamCount = 12, channel = 7, k = 3, F = 10, iw = 2, wh = 2, col = 5, row = 4))
}

object ctrlTestTopSim extends App {

    import lineBuffer._

    //    simNow(n = 2, m = 5, hout = 9, width = 8, bufferRamCount = 8, channel = 7, k = 3, s = 1, delay = 5, iw = 2, wh = 2, col = 5, row = 4)
    simNow(n = 2, m = 5, k = 3, bufferRamCount = 12, channel = 7, hout = 9, iw = 2, wh = 2, col = 5, row = 4, F = 10, width = 8)

    def simNow(n: Int, m: Int, hout: Int, width: Int, bufferRamCount: Int, channel: Int, k: Int, F: Int, iw: Int, wh: Int, col: Int, row: Int) = {
        SimConfig.withWave.withConfig(SpinalConfig(
            defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = HIGH),
            defaultClockDomainFrequency = FixedFrequency(100 MHz)
            )).compile {
            val dut = new ctrlTestTop(n, m, hout, width, bufferRamCount, channel, k, F, iw, wh, col, row)
            dut.writers.foreach(_.toControl.dataToRam simPublic())
            dut.BufferRam.counter.value simPublic()
            dut.Ctrl.readHead.simPublic()
            dut.Ctrl.writeHead.simPublic()
            dut.Ctrl.writerWrEn.simPublic()
            dut.Ctrl.writerAddress.simPublic()
            dut.Ctrl.FSM.stateReg.simPublic()
            dut
        }.doSimUntilVoid { dut =>
            import dut._

            val bufferRamFillData = getRows(dut, dut.bufferRamCount)
            val bufferRamData     = Array.fill(dut.bufferRamCount)(Array.fill(dut.channel)(Array.fill(dut.hout)(BigInt(0))))
            //            bufferRamData.foreach(print2D(_))

            clockDomain.forkStimulus(10)
            SimTimeout(200000 * 10)
            dut.init
            println("-------------------------------------after fill--------------------------------------------------")
            bufferRamData.foreach(print2D(_))
            val writeThread = fork {
                for (i <- 0 until 20000) {
                    dataIn.foreach(_.valid #= true)
                    dataIn.foreach(_.payload.randomize())
                    clockDomain.waitSampling()
                    if (Ctrl.writerWrEn.toBoolean) {
                        val bufferRamCounter = dut.BufferRam.counter.value.toInt
                        val address          = dut.Ctrl.writerAddress.toInt
                        val writerOutData    = dut.writers.map(_.toControl.dataToRam).map(dut.longBitsCov(_))
                        println(s"---------output ${dut.wh} lines-----------------------")
                        println("address: " + address)
                        print2D(writerOutData)
                        for (i <- 0 until dut.n) {
                            bufferRamData(bufferRamCounter * dut.n + i)(address) = writerOutData(i).reverse
                        }
                        bufferRamData.slice(bufferRamCounter * dut.n, (bufferRamCounter + 1) * dut.n).foreach(print2D(_))
                    }
                }
            }
            val readThread  = fork {
                for (i <- 0 until 100) {
                    for (j <- 0 until 200) {
                        peOut.data.ready #= true
                        peOut.switch #= false
                        peOut.row #= scala.util.Random.nextInt(dut.k)
                        peOut.Channel #= scala.util.Random.nextInt(dut.channel)
                        clockDomain.waitSampling()
                        val state = dut.Ctrl.FSM.stateReg.toBigInt
                        if (state == 4) {
                            println("--------------readAndWrite------------")
                            bufferRamData.foreach {
                                print2D(_)
                            }
                        }
                        if (peOut.data.ready.toBoolean && peOut.data.valid.toBoolean) {
                            val row      = peOut.row.toInt
                            val Channel  = peOut.Channel.toInt
                            val payload  = peOut.data.payload.map(dut.longBitsCov(_)).toArray
                            val readHead = Ctrl.readHead.toInt
                            println("----------------------------read---------------------")
                            println("readHead: " + readHead)
                            println("row: " + row)
                            println("channel: " + Channel)
                            print2D(payload)
                            payload.zipWithIndex.foreach { case (port, index) =>
                                bufferRamData((readHead + row + index) % dut.bufferRamCount)(Channel).zip(port.reverse).foreach { case (b, p) => assert(b == p) }
                            }
                        }
                    }
                    peOut.switch #= true
                    peOut.data.ready #= false
                    clockDomain.waitSampling()
                }
                simSuccess()
            }
            clockDomain.waitSampling(20)
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

        def fill(fillData: Array[Array[Array[BigInt]]], bufferRamData: Array[Array[Array[BigInt]]]) = {
            fillData.grouped(dut.n).foreach { nRows =>
                //                nRows.foreach(print2D(_))
                val counter = dut.BufferRam.counter.value.toInt
                clockDomain.waitSampling()
                var lineOuted = 0
                for (r <- 0 until dut.row) {
                    dut.dataIn.foreach(_.valid #= true)
                    for (c <- 0 until dut.col) {
                        nRows.zip(dut.dataIn).foreach { case (rowMatrix, ports) =>
                            ports.payload.zipWithIndex.foreach { case (sig, sigIndex) =>
                                sig #= rowMatrix(r * dut.wh + sigIndex / dut.wh)(c * dut.iw + (sigIndex % dut.iw))
                            }
                        }
                        clockDomain.waitSampling()
                    }
                    dut.dataIn.foreach(_.valid #= false)
                    for (w <- 0 until dut.wh) {
                        clockDomain.waitSampling()
                        val datatoRam = dut.writers.map(_.toControl.dataToRam.toBigInt.toString(2)).map { d => prefixZero(d, dut.width * dut.hout) }.map(_.grouped(dut.width).map(BigInt(_, 2))).map(_.toArray)
                        //                        print2D(datatoRam)
                        if (lineOuted < dut.channel) {
                            for (i <- 0 until dut.n) {
                                bufferRamData(counter * dut.n + i)(r * dut.wh + w) = datatoRam(i).reverse
                            }
                        }
                        lineOuted += 1
                    }
                }
                //                bufferRamData.foreach(print2D(_))
            }
        }


        def prefixZero(readOut: String, L: Int): String = {
            if (readOut.length < L)
                ("0" * (L - readOut.length)) + readOut
            else
                readOut
        }

        def longBitsCov(bits: Bits) = prefixZero(bits.toBigInt.toString(2), bits.getWidth).grouped(dut.width).map(BigInt(_, 2)).toArray
    }
}
