package mutiRow

import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._


case class writeControl(
                           dataWidth: Int=8,
                           inLineNum: Int=2,
                           outLineOut: Int=3,
                           bufferRamCount: Int,
                           Hout: Int = 6,
                           channel: Int,
                           K: Int,
                           S: Int,
                           firstDelay: Int) extends Component{
    val io = new Bundle{

    }
    noIoPrefix()


}