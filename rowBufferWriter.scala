package mutiRow

import spinal.core._
import spinal.core.internals.Operator
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

case class rowBufferWriter(width: Int, Iw: Int, Wh: Int, channel: Int, Hout: Int, colCount: Int, rowCount: Int) extends Component {
    val io = new Bundle {
        val dataIn        = slave Stream (Vec(Bits(width bits), Iw * Wh))
        val transferStart = in Bool()
        val transferEnd   = out Bool()
        val dataOut       = out Bits (Hout * width bits)
        val en            = out Bool()
        val address       = out UInt (log2Up(channel) bits)
    }
    noIoPrefix()

    val blockRegs            = Array.fill(Wh)(Vec(Reg(Bits(width bits)) init (0), Hout))
    val colCounter           = Reg(UInt(log2Up(colCount) + 1 bits)) init (0)
    val rowCounter           = Reg(UInt(log2Up(Wh) + 1 bits)) init (0)
    val lineBufferRowCounter = Reg(UInt(log2Up(channel) bits)) init (0)
    io.dataOut := 0
    io.dataIn.ready := True
    io.en := False
    io.transferEnd := False
    io.address := 0

    val FSM = new StateMachine {
        val idle            = new State with EntryPoint
        val blocksReceiving = new State
        val rowsSending     = new State
        val transferEnd     = new State

        idle.whenIsActive {
            when(io.transferStart) {
                goto(blocksReceiving)
            } otherwise {
                goto(idle)
            }
        }


        blocksReceiving.whenIsActive {
            when(io.dataIn.valid) {
                for (w <- 0 until Wh) {
                    for (i <- 0 until Iw) {
                        switch(colCounter) {
                            for (c <- 0 until colCount) {
                                if (c * Iw + i < Hout) {
                                    is(c) {
                                        blockRegs(w)(c * Iw + i) := io.dataIn.payload(w * Iw + i)
                                    }
                                }
                            }
                        }
                    }
                }
                when(colCounter === colCount - 1) {
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
            io.en := True
            io.address := lineBufferRowCounter

            switch(rowCounter) {
                for (r <- 0 until Wh) {
                    is(r) {
                        blockRegs(r).zipWithIndex.foreach { case (reg, index) =>
                            io.dataOut((index + 1) * width - 1 downto index * width) := reg
                        }
                    }
                }
            }
            when(lineBufferRowCounter === channel - 1) {
                goto(transferEnd)
                lineBufferRowCounter.clearAll()
                rowCounter.clearAll()
                blockRegs.foreach(_.foreach(_.clearAll()))
            } elsewhen (rowCounter === Wh - 1) {
                goto(blocksReceiving)
                rowCounter.clearAll()
                lineBufferRowCounter := lineBufferRowCounter + 1
            } otherwise {
                rowCounter := rowCounter + 1
                lineBufferRowCounter := lineBufferRowCounter + 1
            }

        }

        transferEnd.whenIsActive {
            io.transferEnd := True
            io.dataIn.ready := False
            goto(idle)
        }
    }


}

object lineBuffer {
    def getLineBuffer(rowNum: Int, colNum: Int, channel: Int, Hout: Int, width: Int) = List.tabulate(rowNum, colNum)((i, j) => if (i > channel - 1 || j > Hout - 1) BigInt(0) else BigInt(width, scala.util.Random))

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
            dut.io.transferStart #= false
            dut.io.dataIn.payload.foreach(_ #= 0)
            dut.clockDomain.waitSampling()
        }

        def writeBuffer(buffer: List[List[BigInt]]) = {
            dut.io.transferStart #= true
            dut.io.dataIn.valid #= false
            dut.clockDomain.waitSampling()
            dut.io.transferStart #= false
            for (c <- 0 until dut.rowCount) {
                dut.io.dataIn.valid #= true
                for (t <- 0 until dut.colCount) {
                    dut.io.dataIn.payload.zipWithIndex.foreach { case (port, index) => port #= buffer(c * dut.Wh + index / dut.Iw)(t * dut.Iw + index % dut.Iw) }
                    dut.clockDomain.waitSampling()
                    printRegs
                }
                dut.io.dataIn.valid #= false
                println("***************************************Out***************************************")
                for (o <- 0 until dut.Wh) {
                    if (c * dut.Wh + o < dut.channel) {
                        dut.clockDomain.waitSampling()
                        if (o == 0) printRegs
                        var resString = dut.io.dataOut.toBigInt.toString(2)
                        if (resString.length < dut.width * dut.Hout)
                            resString = ("0" * (dut.Hout * dut.width - resString.length)) + resString
                        resString.grouped(dut.width).map(s => BigInt(s, 2)).foreach(n => printf("%6d ", n))
                        println("")
                    }
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

    def simNow(h: Int, c: Int, Width: Int, I: Int, W: Int, col: Int, row: Int) = {

        SimConfig.withWave.withConfig(SpinalConfig(
            defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = HIGH),
            defaultClockDomainFrequency = FixedFrequency(100 MHz)
        )).compile {
            val dut = rowBufferWriter(Hout = h, channel = c, width = Width, Iw = I, Wh = W, colCount = col, rowCount = row)
            dut.blockRegs.foreach(_.simPublic())
            dut
        }.doSim { dut =>
            import dut._
            import lineBuffer._

            clockDomain.forkStimulus(10)
            dut.init

            val testCase1 = getLineBuffer(rowNum = W * row, colNum = I * col, Hout = h, channel = c, width = Width)
            if (I * col > h)
                testCase1.foreach(_.zipWithIndex.foreach { case (n, index) => if (index > h - 1) BigInt(0) })
            println("-----------------------------------------case1----------------------------------")
            print2D(testCase1)
            dut.writeBuffer(testCase1)
            val testCase2 = getLineBuffer(rowNum = W * row, colNum = I * col, Hout = h, channel = c, width = Width)
            println("-----------------------------------------case2----------------------------------")
            print2D(testCase2)
            dut.writeBuffer(testCase2)
        }
    }

    simNow(h = 11, c = 9, Width = 8, W = 2, I = 2, col = 6, row = 5)
}






