package mutiRow

import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._


case class bufferRam(width: Int, writeCount: Int, depth: Int, inLineNum: Int) extends Component {
    val io = slave(new bufferRamPorts(dataWidth = width, depth = depth, inLineNum = inLineNum, lineBufferNum = writeCount * inLineNum))
    noIoPrefix()
    val Rams    = Array.fill(writeCount * inLineNum)(Mem(Bits(width bits), depth))
    val counter = Reg(UInt(log2Up(writeCount) bits)) init (0) simPublic()

    when(io.sync) {
        when(counter === writeCount) {
            counter := 0
        } otherwise {
            counter := counter + 1
        }
    }

    io.readData.foreach(_.clearAll())

    switch(counter) {
        for (n <- 0 until writeCount) {
            is(n) {
                for (i <- 0 until inLineNum) {
                    when(!io.wrEn) { // 0 -> read, 1 -> write
                        io.readData.zip(Rams).foreach{case(data, ram) => data := ram.readAsync(io.address)}
                    } otherwise {
                        Rams(n * inLineNum + i).write(address = io.address, data = io.writeData(i))
                    }
                }
            }
        }
    }

}

object bufferRamSim extends App {
    SimConfig.withWave.withConfig(SpinalConfig(
        defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = HIGH),
        defaultClockDomainFrequency = FixedFrequency(100 MHz)
    )).compile(new bufferRam(width = 16, writeCount = 8, depth = 128, inLineNum = 3)).doSim { dut =>
        import dut._
        // dataRam Array to store the write date
        val dataRam = Array.ofDim[BigInt](dut.writeCount * dut.inLineNum, dut.depth)

        clockDomain.forkStimulus(10)
        // init
        io.sync #= false
        clockDomain.waitSampling()
        // write the initial content of ram
        for (i <- 0 until dut.writeCount) {
            io.sync #= false
            clockDomain.waitSampling()

            for (j <- 0 until dut.depth) {
                io.wrEn #= true
                io.writeData.randomize()
                io.address #= j
                clockDomain.waitSampling()
                for (k <- i * inLineNum until (i + 1) * inLineNum) {
                    dataRam(k)(j) = io.writeData(k % inLineNum).toBigInt
                }
            }
            io.wrEn #= false
            io.sync #= true
            clockDomain.waitSampling()
        }

        // pure random operation for the doubleRam
        for (i <- 0 until 50) {
            io.sync.randomize()
            io.address.randomize()
            io.wrEn.randomize()
            io.writeData.randomize()
            clockDomain.waitSampling()
            println("--------------------")
            println("i: " + i)
            if(io.wrEn.toBoolean){
                for (j <- 0 until inLineNum){
                    dataRam(counter.toInt * inLineNum + j)(io.address.toInt) = io.writeData(j).toBigInt
                    println(s"write ram ${counter.toInt * inLineNum + j} with data: ${io.writeData(j).toBigInt} in address: ${io.address.toBigInt}")
                }
            }else{
                val gold: Array[BigInt] = dataRam.map(_(io.address.toInt))
                gold.zip(io.readData).foreach{case(g, d)=> assert(g == d.toBigInt)}
                println(gold.mkString(" "))
                println( io.readData.map(_.toBigInt).mkString(" "))
            }

        }
    }
}