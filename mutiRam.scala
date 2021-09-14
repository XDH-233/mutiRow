package mutiRow

import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._


case class mutiRam(width: Int, num: Int, depth: Int, inLineNum: Int) extends Component {
    val io = new Bundle {
        val rdEn    = Vec(in Bool(), inLineNum)
        val data    = Vec(in Bits (width bits), inLineNum)
        val address = Vec(in UInt (log2Up(depth) bits), inLineNum)
        val sync    = in Bool()
        val dataOut = Vec(out Bits (width bits), inLineNum)
    }
    noIoPrefix()

    val Rams    = Array.fill(num * inLineNum)(Mem(Bits(width bits), depth))
    val counter = Reg(UInt(log2Up(num) bits)) init (0) simPublic()

    when(io.sync) {
        when(counter === num) {
            counter := 0
        } otherwise {
            counter := counter + 1
        }
    }

    io.dataOut.foreach(_.clearAll())

    switch(counter) {
        for (n <- 0 until num) {
            is(n) {
                for (i <- 0 until inLineNum) {
                    when(io.rdEn(i)) {
                        io.dataOut(i) := Rams(n * inLineNum + i).readAsync(address = io.address(i))
                    } otherwise {
                        Rams(n * inLineNum + i).write(address = io.address(i), data = io.data(i))
                    }
                }
            }
        }
    }

}

object doubleRamSim extends App {
    SimConfig.withWave.withConfig(SpinalConfig(
        defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = HIGH),
        defaultClockDomainFrequency = FixedFrequency(100 MHz)
    )).compile(new mutiRam(width = 16, num = 8, depth = 128, inLineNum = 3)).doSim { dut =>
        import dut._
        // dataRam Array to store the write date
        val dataRam = Array.ofDim[BigInt](dut.num * dut.inLineNum, dut.depth)

        clockDomain.forkStimulus(10)
        // init
        io.sync #= false
        clockDomain.waitSampling()
        // write the initial content of ram
        for (i <- 0 until dut.num) {
            io.sync #= false
            clockDomain.waitSampling()

            for (j <- 0 until dut.depth) {
                //                io.rdEn(0) #= false
                //                io.rdEn(1) #= false
                io.rdEn.foreach(_ #= false)
                io.data.randomize()
                io.address.foreach(_ #= j)
                //                io.address(0) #= j
                //                io.address(1) #= j
                clockDomain.waitSampling()
                for (k <- i * inLineNum until (i + 1) * inLineNum) {
                    dataRam(k)(j) = io.data(k % inLineNum).toBigInt
                }
                //                dataRam(i * 2)(j) = io.data(0).toBigInt
                //                dataRam(i * 2 + 1)(j) = io.data(1).toBigInt
            }
            io.rdEn.foreach(_ #= true)
            //            io.rdEn(0) #= true
            //            io.rdEn(1) #= true
            io.sync #= true
            clockDomain.waitSampling()
        }

        // pure random operation for the doubleRam
        for (i <- 0 until 50) {
            io.sync.randomize()
            io.address.randomize()
            io.rdEn.randomize()
            io.data.randomize()
            clockDomain.waitSampling()
            println("--------------------")
            println("i: " + i)
            for (j <- 0 until inLineNum) {
                if (!io.rdEn(j).toBoolean) { // write
                    dataRam(counter.toInt * inLineNum + j)(io.address(j).toInt) = io.data(j).toBigInt
                    println(s"write ram ${counter.toInt * inLineNum + j} with data: ${io.data(j).toBigInt} in address: ${io.address(j).toBigInt}")
                } else { // read
                    val gold = dataRam(counter.toInt * inLineNum + j)(io.address(j).toInt)
                    val get  = io.dataOut(j).toBigInt
                    println(s"read ram ${counter.toInt * inLineNum + j}, readData: ${get}")
                    println(s"expected: ${gold}")
                    assert(gold == get)
                }
            }
        }
    }
}