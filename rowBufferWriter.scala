package mutiRow

import spinal.core._
import spinal.core.internals.Operator
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

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
object rowBufferWriterPorts{
    def apply(ctrl: writeControl): rowBufferWriterPorts={
        rowBufferWriterPorts(ctrl.Hout, ctrl.dataWidth, ctrl.channel)
    }
    def apply(writer: rowBufferWriter): rowBufferWriterPorts={
        rowBufferWriterPorts(writer.Hout, writer.width, writer.channel)
    }
}

case class rowBufferWriter(width: Int, Iw: Int, Ww: Int, channel: Int, Hout: Int, colCount: Int, rowCount: Int) extends Component {
    val dataIn    = slave Stream (Vec(Bits(width bits), Iw * Ww))
    val toControl = master(rowBufferWriterPorts(this))

    val blockRegs            = Array.fill(Ww)(Vec(Reg(Bits(width bits)) init (0), Hout))
    val colCounter           = Reg(UInt(log2Up(colCount) + 1 bits)) init (0)
    val rowCounter           = Reg(UInt(log2Up(Ww) + 1 bits)) init (0)
    val lineBufferRowCounter = Reg(UInt(log2Up(channel) bits)) init (0)
    toControl.dataToRam := 0
    dataIn.ready := False
    toControl.wrEn := False
    toControl.transferEnd := False
    toControl.address := 0

    val FSM = new StateMachine {
        val idle            = new State with EntryPoint
        val blocksReceiving = new State
        val rowsSending     = new State
        val transferEnd     = new State

        idle.whenIsActive {
            dataIn.ready := False
            when(toControl.transferStart) {
                goto(blocksReceiving)
            } otherwise {
                goto(idle)
            }
        }


        blocksReceiving.whenIsActive {
            dataIn.ready := True
            when(dataIn.valid) {
                for (w <- 0 until Ww) {
                    for (i <- 0 until Iw) {
                        switch(colCounter) {
                            for (c <- 0 until colCount) {
                                if (c * Iw + i < Hout) {
                                    is(c) {
                                        blockRegs(w)(c * Iw + i) := dataIn.payload(w * Iw + i)
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
            dataIn.ready := False
            toControl.wrEn := True
            toControl.address := lineBufferRowCounter

            switch(rowCounter) {
                for (r <- 0 until Ww) {
                    is(r) {
                        blockRegs(r).zipWithIndex.foreach { case (reg, index) =>
                            toControl.dataToRam((index + 1) * width - 1 downto index * width) := reg
                        }
                    }
                }
            }
            when(lineBufferRowCounter === channel - 1) {
                goto(transferEnd)
                lineBufferRowCounter.clearAll()
                rowCounter.clearAll()
                blockRegs.foreach(_.foreach(_.clearAll()))
            } elsewhen (rowCounter === Ww - 1) {
                goto(blocksReceiving)
                rowCounter.clearAll()
                lineBufferRowCounter := lineBufferRowCounter + 1
            } otherwise {
                rowCounter := rowCounter + 1
                lineBufferRowCounter := lineBufferRowCounter + 1
            }

        }

        transferEnd.whenIsActive {
            toControl.transferEnd := True
            dataIn.ready := False
            goto(idle)
        }
    }


}

object lineBuffer {
    def getRowMatrix(rowNumOfBlocks: Int, colNumOfBlocks: Int, channel: Int, Hout: Int, width: Int) = List.tabulate(rowNumOfBlocks, colNumOfBlocks)((i, j) => if (i > channel - 1 || j > Hout - 1) BigInt(0) else BigInt(width, scala.util.Random)).map(_.toArray).toArray

    def getRows(dut: lineBufferCtrl, num: Int)= Array.fill(num)(getRowMatrix(dut.row * dut.ww, dut.col * dut.iw, dut.channel, dut.hout, dut.width))

    def print2D(buffer: Array[Array[BigInt]]) = {
        buffer.foreach { seq =>
            seq.foreach(b => printf("%6d ", b))
            println("")
        }
        println("")
    }
}


object rowBufferWriterSim extends App {
    simNow(h = 11, c = 9, Width = 8, W = 2, I = 2, col = 6, row = 5)
    implicit class simMethod(dut: rowBufferWriter) {
        def init = {
            dut.dataIn.valid #= false
            dut.toControl.transferStart #= false
            dut.dataIn.payload.foreach(_ #= 0)
            dut.clockDomain.waitSampling()
        }

        def writeBuffer(buffer: Array[Array[BigInt]]) = {
            dut.toControl.transferStart #= true
            dut.dataIn.valid #= false
            dut.clockDomain.waitSampling()
            dut.toControl.transferStart #= false
            for (c <- 0 until dut.rowCount) {
                dut.dataIn.valid #= true
                for (t <- 0 until dut.colCount) {
                    dut.dataIn.payload.zipWithIndex.foreach { case (port, index) => port #= buffer(c * dut.Ww + index / dut.Iw)(t * dut.Iw + index % dut.Iw) }
                    dut.clockDomain.waitSampling()
                    printRegs
                }
                dut.dataIn.valid #= false
                println("***************************************Out***************************************")
                for (o <- 0 until dut.Ww) {
                    if (c * dut.Ww + o < dut.channel) {
                        dut.clockDomain.waitSampling()
                        if (o == 0) printRegs
                        var resString = dut.toControl.dataToRam.toBigInt.toString(2)
                        if (resString.length < dut.width * dut.Hout)
                            resString = ("0" * (dut.Hout * dut.width - resString.length)) + resString
                        resString.grouped(dut.width).map(s => BigInt(s, 2)).foreach(n => printf("%6d ", n))
                        println("")
                    }
                }
                dut.dataIn.ready #= false
                dut.clockDomain.waitSampling(3)
            }
        }

        def getRegs = dut.blockRegs.map(_.map(_.toBigInt)).map(_.toArray)

        def printRegs = {
            println("---------------------------------------Regs------------------------------------------")
            val cov = getRegs
            lineBuffer.print2D(cov)
        }


    }

    def simNow(h: Int, c: Int, Width: Int, I: Int, W: Int, col: Int, row: Int) = {

        SimConfig.withWave.withConfig(SpinalConfig(
            defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = HIGH),
            defaultClockDomainFrequency = FixedFrequency(100 MHz)
            )).compile {
            val dut = rowBufferWriter(Hout = h, channel = c, width = Width, Iw = I, Ww = W, colCount = col, rowCount = row)
            dut.blockRegs.foreach(_.simPublic())
            dut
        }.doSim { dut =>
            import dut._
            import lineBuffer._

            clockDomain.forkStimulus(10)
            dut.init

            val testCase1 = getRowMatrix(rowNumOfBlocks = W * row, colNumOfBlocks = I * col, Hout = h, channel = c, width = Width)
            if (I * col > h)
                testCase1.foreach(_.zipWithIndex.foreach { case (n, index) => if (index > h - 1) BigInt(0) })
            println("-----------------------------------------case1----------------------------------")
            print2D(testCase1)
            dut.writeBuffer(testCase1)
            val testCase2 = getRowMatrix(rowNumOfBlocks = W * row, colNumOfBlocks = I * col, Hout = h, channel = c, width = Width)
            println("-----------------------------------------case2----------------------------------")
            print2D(testCase2)
            dut.writeBuffer(testCase2)
        }
    }


}






