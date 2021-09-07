package mutiRow

import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._


case class doubleRam(width: Int, num: Int, depth: Int) extends Component {
    val io = new Bundle {
        val rdEn    = Vec(in Bool(), 2)
        val data    = Vec(in Bits (width bits), 2)
        val address = Vec(in UInt (log2Up(depth) bits), 2)
        val sync    = in Bool()
        val dataOut = Vec(out Bits (width bits), 2)
    }
    noIoPrefix()

    val multiRam = Array.fill(num * 2)(Mem(Bits(width bits), depth))
    val counter  = Reg(UInt(log2Up(num) bits)) init (0) simPublic()

    when(io.sync) {
        when(counter === num) {
            counter := 0
        } otherwise {
            counter := counter + 1
        }
    }

    io.dataOut(0).clearAll()
    io.dataOut(1).clearAll()

    switch(counter){
        for(n <- 0 until num){
            is(n){
                for (i <- 0 until 2) {
                    when(io.rdEn(i)) {
                        io.dataOut(i) := multiRam(n * 2 + i).readAsync(address = io.address(i))
                    } otherwise {
                        multiRam(n * 2 + i).write(address = io.address(i), data = io.data(i))
                    }
                }
            }
        }
    }

}

object doubleRamSim extends App{
    SimConfig.withWave.withConfig(SpinalConfig(
        defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = HIGH),
        defaultClockDomainFrequency = FixedFrequency(100 MHz)
    )).compile(new doubleRam(width = 16, num = 8, depth = 128)).doSim { dut =>
        import dut._
        // dataRam Array to store the write date
        val dataRam = Array.ofDim[BigInt](dut.num * 2, dut.depth)

        clockDomain.forkStimulus(10)
        // init
        io.sync #= false
        clockDomain.waitSampling()
        // write the initial content of ram
        for(i <- 0 until dut.num){
            io.sync #= false
            clockDomain.waitSampling()
            for(j <- 0 until dut.depth){
                io.rdEn(0) #= false
                io.rdEn(1) #= false
                io.data.randomize()
                io.address(0) #= j
                io.address(1) #= j
                clockDomain.waitSampling()
                dataRam(i * 2)(j) = io.data(0).toBigInt
                dataRam(i * 2 + 1)(j) = io.data(1).toBigInt
            }
            io.rdEn(0) #= true
            io.rdEn(1) #= true
            io.sync #= true
            clockDomain.waitSampling()
        }

        // pure random operation for the doubleRam
        for(i <- 0 until 1000){
            io.sync.randomize()
            io.address.randomize()
            io.rdEn.randomize()
            io.data.randomize()
            clockDomain.waitSampling()
            println("--------------------")
            println("i: " + i)
            for(j <- 0 until 2){
                if(! io.rdEn(j).toBoolean){// write
                    dataRam(counter.toInt * 2 + j)(io.address(j).toInt) = io.data(j).toBigInt
                    println(s"write ram ${counter.toInt * 2 + j} with data: ${io.data(j).toBigInt} in address: ${io.address(j).toBigInt}" )
                }else{// read
                    val gold = dataRam(counter.toInt * 2 + j)(io.address(j).toInt)
                    val get = io.dataOut(j).toBigInt
                    println(s"read ram ${counter.toInt * 2 + j}, readData: ${get}")
                    println(s"expected: ${gold}")
                    assert(gold == get)
                }
            }
        }
    }
}