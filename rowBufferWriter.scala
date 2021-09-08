package mutiRow

import spinal.core._
import spinal.core.internals.Operator
import spinal.sim._
import spinal.core.sim._
import spinal.lib._

case class rowBufferWriter(width: Int, Iw: Int, Wh: Int, channel: Int, Hout: Int) extends Component {
    val io = new Bundle {
        val dataIn      = slave Stream (Vec(Bits(width bits), Iw * Wh))
        val dataOut     = master Stream (Bits(Hout * width bits))
        val transferStart = in Bool()
        val transferEnd = out Bool()
    }
    noIoPrefix()
    io.dataIn.ready := True
    io.dataOut.valid := False
    io.dataOut.payload := 0
    io.transferEnd := False
    val colTimes        = Hout / Iw + {
        if ((Hout - Hout / Iw * Iw) > 0) 1 else 0
    }
    val rowTimes        = channel / Wh + {
        if (channel - channel / Wh * Wh > 0) 1 else 0
    }
    val colCounter      = Reg(UInt(log2Up(colTimes) + 1 bits)) init (0)
    val blockRegs       = Array.fill(Wh)(Vec(Reg(Bits(width bits)) init (0), Hout))
    val rowCounter      = Reg(UInt(log2Up(Wh) + 1 bits)) init (0)
    val blockRowCounter = Reg(UInt(log2Up(rowTimes) + 1 bits)) init (0)
    val transmitting    = Reg(Bool()) init(False)

    when(io.transferStart){
        transmitting := True
        blockRegs.foreach(_.foreach(_.clearAll()))
    }

    when(io.dataIn.valid && transmitting) {
        when(colCounter === colTimes) {
            io.dataOut.valid := False
            io.dataIn.ready := False
        } otherwise {
            colCounter := colCounter + 1
            io.dataIn.ready := False
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
        }

    }
    when(io.dataOut.ready && colCounter === colTimes) {
        when(rowCounter === Wh) {
            io.dataOut.valid := False
            io.dataIn.ready := True
            colCounter := 0
            rowCounter := 0
            when(blockRowCounter === rowTimes - 1) {
                io.transferEnd := True
                transmitting := False
                blockRowCounter := 0
            } otherwise {
                blockRowCounter := blockRowCounter + 1
            }
        } otherwise {
            rowCounter := rowCounter + 1
            io.dataOut.valid := True
            io.dataIn.ready := False
            switch(rowCounter) {
                for (r <- 0 until Wh) {
                    is(r) {
                        blockRegs(r).zipWithIndex.foreach { case (reg, index) => io.dataOut.payload((index + 1) * width - 1 downto index * width) := reg }
                    }
                }
            }
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

        def write(buffer: List[List[BigInt]]) = {
            dut.io.transferStart #= true
            dut.io.dataIn.valid #= false
            dut.clockDomain.waitSampling()
            dut.io.transferStart #= false
            for (c <- 0 until dut.rowTimes) {
                dut.io.dataIn.valid #= true
                for (t <- 0 until dut.colTimes) {
                    dut.io.dataIn.payload.zipWithIndex.foreach { case (port, index) => port #= buffer(c * dut.Wh + index / dut.Iw)(t * dut.Iw + index % dut.Iw) }
                    dut.clockDomain.waitSampling()
                    printRegs
                }
                dut.clockDomain.waitSampling()
                printRegs
                dut.io.dataOut.ready #= true
                dut.io.dataIn.valid #= false
                dut.clockDomain.waitSampling(dut.Wh)
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
        print2D(testCase1)
        dut.write(testCase1)
        val testCase2 = getLineBuffer(Hout = dut.Hout, channel = dut.channel, width = dut.width)
        print2D(testCase2)
        dut.write(testCase2)
    }
}






