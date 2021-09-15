package mutiRow

import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._


case class pePorts(outLineNum: Int, dataWidth: Int, Hout: Int, K: Int, channel: Int) extends Bundle with IMasterSlave {
    val data    = Vec(Bits(Hout * dataWidth bits), outLineNum)
    val row     = UInt(log2Up(K) bits)
    val Channel = UInt(log2Up(channel) bits)
    val ready   = Bool()
    val valid   = Bool()
    val switch  = Bool()
    override def asMaster(): Unit = {
        out(data, valid)
        in(ready,row, Channel, switch)
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
    val writerPorts   = new Bundle {
        val transferStart = out Bool()
        val transferEnd   = in Bool()
        val dataIn        = in Vec(Bits(Hout * dataWidth bits), inLineNum)
        val wrEn          = in Bool()
        val address       = in UInt (log2Up(channel) bits)
    }
    val bufferRamPort = new Bundle {
        val wrEn    = out Bool()
        val dataOut = out Vec(Bits(Hout * dataWidth bits), inLineNum)
        val address = out UInt (log2Up(channel) bits)
        val sync    = out Bool()
    }

    val peOut  = master(new pePorts(outLineNum = outLineNum, dataWidth = dataWidth, Hout = Hout, K = K, channel = channel))



    val FSM = new StateMachine {
        val idle          = new State with EntryPoint
        val fillMutiRam   = new State
        val writeMultiRam = new State
        val readMultiRam  = new State
    }

}

