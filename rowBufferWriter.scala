package mutiRow

import spinal.core._
import spinal.core.internals.Operator
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

case class rowBufferWriter(width: Int, Iw: Int, Wh: Int, channel: Int, Hout: Int) extends Component {
    val io = new Bundle {
        val dataIn        = slave Stream (Vec(Bits(width bits), Iw * Wh))
        val dataOut       = master Stream (Bits(Hout * width bits))
        val transferStart = in Bool()
        val transferEnd   = out Bool()
    }
    noIoPrefix()
    io.dataIn.ready := True
    io.dataOut.valid := False
    io.transferEnd := False
    io.dataOut.payload := 0

    val colTimes        = Hout / Iw + {
        if ((Hout - Hout / Iw * Iw) > 0) 1 else 0
    }
    val rowTimes        = channel / Wh + {
        if (channel - channel / Wh * Wh > 0) 1 else 0
    }
    val blockRegs       = Array.fill(Wh)(Vec(Reg(Bits(width bits)) init (0), Hout))
    val colCounter      = Reg(UInt(log2Up(colTimes) + 1 bits)) init (0)
    val rowCounter      = Reg(UInt(log2Up(Wh) + 1 bits)) init (0)
    val blockRowCounter = Reg(UInt(log2Up(rowTimes) + 1 bits)) init (0)


    val FSM = new StateMachine {
        val idle            = new State with EntryPoint
        val blocksReceiving = new State
        val rowsSending     = new State
        val transferEnd     = new State

        idle.whenIsActive {
            when(io.transferStart) {
                goto(blocksReceiving)
            }otherwise{
                goto(idle)
            }
        }
        blocksReceiving.whenIsActive {
            when(io.dataIn.valid) {
                for (w <- 0 until Wh) {
                    for (i <- 0 until Iw) {
                        switch(colCounter) {
                            for (c <- 0 until colTimes) {
                                is(c) {
                                    blockRegs(w)(c * Iw + i) := io.dataIn.payload(w * Iw + i)
                                }
                            }
                        }
                    }
                }
                when(colCounter === colTimes - 1) {
                    goto(rowsSending)
                    colCounter.clearAll()
                } otherwise {
                    goto(blocksReceiving)
                    colCounter := colCounter + 1
                }
            }
        }

        rowsSending.whenIsActive {
            io.dataIn.ready := False
            io.dataOut.valid := True
            when(io.dataOut.ready) {
                switch(rowCounter) {
                    for (r <- 0 until Wh) {
                        is(r) {
                            blockRegs(r).zipWithIndex.foreach { case (reg, index) => io.dataOut.payload((index + 1) * width - 1 downto index * width) := reg }
                        }
                    }
                }
                when(rowCounter === Wh - 1) {
                    rowCounter.clearAll()
                    when(blockRowCounter === rowTimes - 1){
                        goto(transferEnd)
                        blockRowCounter.clearAll()
                        blockRegs.foreach(_.foreach(_.clearAll()))
                    }otherwise{
                        blockRowCounter := blockRowCounter + 1
                        goto(blocksReceiving)
                    }
                } otherwise {
                    rowCounter := rowCounter + 1
                }
            }
        }

        transferEnd.whenIsActive {
            io.transferEnd := True
            goto(idle)
        }
    }

}

object lineBuffer {
    def getLineBuffer(Hout: Int, channel: Int, width: Int) = List.tabulate(channel, Hout)((i, j) => BigInt(width, scala.util.Random))

    def print2D(buffer: List[List[BigInt]]) = {
        buffer.foreach { seq =>
            seq.foreach(b => printf("%6d ", b))
            println("")
        }
        println("")
    }
}


object rowBufferWriterSim extends App {
    implicit class simMethod(dut: rowBufferWriter) {
        def init = {
            dut.io.dataIn.valid #= false
            dut.io.dataOut.ready #= false
            dut.io.transferStart #= false
            dut.io.dataIn.payload.foreach(_ #= 0)
            dut.clockDomain.waitSampling()
        }

        def writeBuffer(buffer: List[List[BigInt]]) = {
            dut.io.transferStart #= true
            dut.io.dataIn.valid #= false
            dut.clockDomain.waitSampling()
            dut.io.transferStart #= false
            for (c <- 0 until dut.rowTimes) {
                dut.io.dataIn.valid #= true
                dut.io.dataOut.ready #= false
                for (t <- 0 until dut.colTimes) {
                    dut.io.dataIn.payload.zipWithIndex.foreach { case (port, index) => port #= buffer(c * dut.Wh + index / dut.Iw)(t * dut.Iw + index % dut.Iw) }
                    dut.clockDomain.waitSampling()
                    printRegs
                }
                dut.io.dataIn.valid #= false
                dut.clockDomain.waitSampling()
                printRegs
                dut.clockDomain.waitSampling()
                dut.io.dataOut.ready #= true
                dut.io.dataIn.valid #= false
                println("***************************************Out***************************************")
                for (o <- 0 until dut.Wh) {
                    dut.clockDomain.waitSampling()
                    var resString = dut.io.dataOut.payload.toBigInt.toString(2)
                    if (resString.length < dut.width * dut.Hout)
                        resString = ("0" * (dut.Hout * dut.width - resString.length)) + resString
                    resString.grouped(dut.width).map(s => BigInt(s, 2)).foreach(n => printf("%6d ", n))
                    println("")
                }
                dut.io.dataIn.ready #= false
                dut.clockDomain.waitSampling(3)
            }
        }

        def getRegs: Array[IndexedSeq[BigInt]] = dut.blockRegs.map(_.map(_.toBigInt))

        def printRegs = {
            println("---------------------------------------Regs------------------------------------------")
            val cov = getRegs.map(_.toList).toList
            lineBuffer.print2D(cov)
        }


    }

    SimConfig.withWave.withConfig(SpinalConfig(
        defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = HIGH),
        defaultClockDomainFrequency = FixedFrequency(100 MHz)
    )).compile {
        val dut = rowBufferWriter(Hout = 10, channel = 8, width = 8, Iw = 2, Wh = 2)
        dut.blockRegs.foreach(_.simPublic())
        dut
    }.doSim { dut =>
        import dut._
        import lineBuffer._

        clockDomain.forkStimulus(10)
        dut.init
        val testCase1 = getLineBuffer(Hout = dut.Hout, channel = dut.channel, width = dut.width)
        println("-----------------------------------------case1----------------------------------")
        print2D(testCase1)
        dut.writeBuffer(testCase1)
        val testCase2 = getLineBuffer(Hout = dut.Hout, channel = dut.channel, width = dut.width)
        println("-----------------------------------------case2----------------------------------")
        print2D(testCase2)
        dut.writeBuffer(testCase2)
    }
}






