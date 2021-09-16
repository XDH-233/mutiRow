package mutiRow

import spinal.core._
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
    val toBufferRam = master(bufferRamPorts(dataWidth = dataWidth, depth = channel, inLineNum = inLineNum, lineBufferNum = bufferRamCount))
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
    toBufferRam.wrEn := False // default: read
    toBufferRam.address := 0
    toBufferRam.sync := False
    toBufferRam.writeData.foreach(_ := 0)

    // counters
    //    val counterForChannel = Counter(0, channel - 1)
    val switchTimes  = Counter(0, outLineNum - 1)
    val readOutTimes = Counter(0, 100) //TODO, The upper limit of readOutTimes is to be determined

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
                fromWriter.zip(toBufferRam.writeData).foreach { case (writer, wData) => wData := writer.dataToRam }
                toBufferRam.wrEn := writerWrEn
                toBufferRam.address := writerAddress
                when(writerAddress === channel - 1) {
                    toBufferRam.sync := True
                    writerTransferStart := True // transfer next output map
                    switchTimes.increment()
                    when(switchTimes.willOverflowIfInc) { // Filled up all the bufferRam
                        goto(readBufferRam) //TODO Ignore firstDelay for now
                    }
                }
            }
        }
        readBufferRam.whenIsActive {
            toBufferRam.wrEn := False // read
            toBufferRam.address := toPe.Channel
            toPe.data.valid := True
            when(toPe.data.ready) {
                switch(toPe.row) {
                    for (row <- 0 until K) {
                        is(K) {
                            // toPe.data.payload.zipWithIndex.foreach{case(sig, index)=> sig := }
                            toPe.data.payload.zipWithIndex.foreach{case(b, index) => b := }
                        }
                    }
                }
            }
        }

    }

}

