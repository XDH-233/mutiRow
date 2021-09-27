package mutiRow

import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._


case class rowBufferWriterPoolingPorts(dataWidth: Int, poolingSize: Int, Hout: Int, channel: Int) extends Bundle with IMasterSlave {
    val transferStart = Bool()
    val transferEnd   = Bool()
    val dataOut       = Vec(UInt(dataWidth bits), scala.math.ceil(Hout.toDouble / poolingSize.toDouble).toInt)
    val address       = UInt(log2Up(channel) bits)
    val wrEn          = Bool()

    override def asMaster(): Unit = {
        out(transferEnd, dataOut, wrEn, address)
        in(transferStart)
    }
}

object rowBufferWriterPoolingPorts {
    def apply(writer: rowBufferWriterPooling): rowBufferWriterPoolingPorts = {
        rowBufferWriterPoolingPorts(writer.dataWidth, writer.poolingSize, writer.Hout, writer.channel)
    }
}

case class rowBufferWriterPooling(dataWidth: Int, Iw: Int, Wh: Int, channel: Int, Hout: Int, colCount: Int, rowCount: Int, poolingSize: Int) extends Component {
    val toControl = master(rowBufferWriterPoolingPorts(this))
    val dataIn    = slave Stream (Vec(UInt(dataWidth bits), Iw * Wh))

    val blockRegs            = Array.fill(Wh)(Vec(Reg(UInt(dataWidth bits)) init (0), Hout))
    val colCounter           = Reg(UInt(log2Up(colCount) + 1 bits)) init (0)
    val rowCounter           = Reg(UInt(log2Up(Wh) + 1 bits)) init (0)
    val lineBufferRowCounter = Reg(UInt(log2Up(channel) bits)) init (0)
    toControl.dataOut.foreach(_ := 0)
    dataIn.ready := False
    toControl.wrEn := False
    toControl.transferEnd := False
    toControl.address := 0

    val writerFSM = new StateMachine {
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
                for (w <- 0 until Wh) {
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
                for (r <- 0 until Wh) {
                    is(r) {
                        val c = Vec(Vec(blockRegs(r).grouped(poolingSize).map(Vec(_))).map(compare(_)))
                        toControl.dataOut.zip(c).foreach{case(o, i)=> o :=i}
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
            toControl.transferEnd := True
            dataIn.ready := False
            goto(idle)
        }
    }

    def compare(vec: Vec[UInt]): UInt = vec.reduceBalancedTree((i, j) => comp(i, j))

    def comp(i: UInt, j: UInt): UInt = {
        val r = UInt(i.getWidth bits)
        when(i < j) {
            r := j
        } otherwise {
            r := i
        }
        r
    }
}

object rowBufferWriterPoolingSim extends App{
    SimConfig.withWave.withConfig(SpinalConfig(
        defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = HIGH),
        defaultClockDomainFrequency = FixedFrequency(100 MHz)
    )).compile{
        val dut  = new rowBufferWriterPooling(8, 2,2,7,9,5,4,2)
        dut.blockRegs.foreach(_.simPublic())
        dut
    }.doSim { dut =>
        dut.clockDomain.forkStimulus(10000)
        dut.init
        val rowMatrix = lineBuffer.getRowMatrix(8,10,7,9,8)
        lineBuffer.print2D(rowMatrix)
        dut.writeBuffer(rowMatrix)
    }

    implicit class simMethod(dut: rowBufferWriterPooling) {
        import dut._
        def init = {
            dataIn.valid #= false
            toControl.transferStart #= false
            dataIn.payload.foreach(_ #= 0)
            clockDomain.waitSampling()
        }

        def writeBuffer(buffer: Array[Array[BigInt]]) = {
            toControl.transferStart #= true
            dataIn.valid #= false
            clockDomain.waitSampling()
            toControl.transferStart #= false
            for (c <- 0 until rowCount) {
                dataIn.valid #= true
                for (t <- 0 until colCount) {
                    dataIn.payload.zipWithIndex.foreach { case (port, index) => port #= buffer(c * Wh + index / Iw)(t * Iw + index % Iw) }
                    clockDomain.waitSampling()
                    printRegs
                }
                dataIn.valid #= false
                println("***************************************Out***************************************")
                clockDomain.waitSampling()
                for(i <- 0 until Wh){
                    clockDomain.waitSampling()
                }
                clockDomain.waitSampling(3)
            }
        }

        def getRegs = dut.blockRegs.map(_.map(_.toBigInt)).map(_.toArray)

        def printRegs = {
            println("---------------------------------------Regs------------------------------------------")
            val cov = getRegs
            lineBuffer.print2D(cov)
        }


    }
}