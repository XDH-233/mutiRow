package mutiRow

import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._


case class mutiRam(width: Int, num: Int, depth: Int, inLineNum: Int) extends Component {
    val io = new Bundle {
        val wrEn = in Bool()
        val data = in Vec(Bits (width bits), inLineNum)
        val address = in UInt (log2Up(depth) bits)
        val sync    = in Bool()
        val dataOut = out Vec(Bits (width bits), inLineNum)
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
                    when(io.wrEn) {
                        io.dataOut(i) := Rams(n * inLineNum + i).readAsync(address = io.address)
                    } otherwise {
                        Rams(n * inLineNum + i).write(address = io.address, data = io.data(i))
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
                io.wrEn #= false
                io.data.randomize()
                io.address #= j
                clockDomain.waitSampling()
                for (k <- i * inLineNum until (i + 1) * inLineNum) {
                    dataRam(k)(j) = io.data(k % inLineNum).toBigInt
                }
            }
            io.wrEn #= true
            io.sync #= true
            clockDomain.waitSampling()
        }

        // pure random operation for the doubleRam
        for (i <- 0 until 50) {
            io.sync.randomize()
            io.address.randomize()
            io.wrEn.randomize()
            io.data.randomize()
            clockDomain.waitSampling()
            println("--------------------")
            println("i: " + i)
            for (j <- 0 until inLineNum) {
                if (!io.wrEn.toBoolean) { // write
                    dataRam(counter.toInt * inLineNum + j)(io.address.toInt) = io.data(j).toBigInt
                    println(s"write ram ${counter.toInt * inLineNum + j} with data: ${io.data(j).toBigInt} in address: ${io.address.toBigInt}")
                } else { // read
                    val gold = dataRam(counter.toInt * inLineNum + j)(io.address.toInt)
                    val get  = io.dataOut(j).toBigInt
                    println(s"read ram ${counter.toInt * inLineNum + j}, readData: ${get}")
                    println(s"expected: ${gold}")
                    assert(gold == get)
                }
            }
        }
    }
}