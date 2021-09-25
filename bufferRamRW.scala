package mutiRow

import spinal.core._
import spinal.core.sim._
import spinal.lib._


case class bufferRamRWPorts(dataWidth: Int, depth: Int, inCount: Int, bufferRamCount: Int) extends Bundle with IMasterSlave {
    require(bufferRamCount % inCount == 0, "wrong para!")
    val wrEn                      = Bool()
    val writeData                 = Vec(Bits(dataWidth bits), inCount)
    val readData                  = Vec(Bits(dataWidth bits), bufferRamCount)
    val writeAddress, readAddress = UInt(log2Up(depth) bits)
    val sync                      = Bool()

    override def asMaster(): Unit = {
        out(wrEn, writeData, writeAddress, readAddress, sync)
        in(readData)
    }
}

case class bufferRamRW(dataWidth: Int, inCount: Int, bufferRamCount: Int, depth: Int) extends Component {
    val io = slave(bufferRamRWPorts(dataWidth, depth, inCount, bufferRamCount))
    noIoPrefix()
    val rams    = Array.fill(bufferRamCount)(Mem(Bits(dataWidth bits), depth))
    val counter = Counter(0, bufferRamCount / inCount - 1)
    when(io.sync) {
        counter.increment()
    }
    switch(counter.value) {
        for (n <- 0 until bufferRamCount / inCount) {
            is(n) {
                when(io.wrEn) {
                    for (i <- 0 until inCount) {
                        rams(n * inCount + i).write(io.writeAddress, io.writeData(i))
                        io.readData(n * inCount + i) := 0
                    }
                    io.readData.slice(0, n * inCount).zip(rams.slice(0, n * inCount)).foreach { case (port, ram) => port := ram.readAsync(io.readAddress) }
                    io.readData.slice((n + 1) * inCount, bufferRamCount).zip(rams.slice((n + 1) * inCount, bufferRamCount)).foreach { case (port, ram) => port := ram.readAsync(io.readAddress) }
                } otherwise {
                    io.readData.zip(rams).foreach { case (port, ram) => port := ram.readAsync(io.readAddress) }
                }
            }
            if(!isPow2(bufferRamCount / inCount)){
                default{
                    io.readData.foreach(_:=0)
                }
            }

        }
    }
}

object bufferRamRWRTL extends App {
    SpinalConfig(
        defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = HIGH)
        ).generateVerilog(new bufferRamRW(8, 2, 6, 8))
}

object bufferRamRWSim extends App {
    SimConfig.withWave.withConfig(SpinalConfig(
        defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = HIGH),
        defaultClockDomainFrequency = FixedFrequency(100 MHz)
        )).compile {
        val dut = new bufferRamRW(8, 3, 12, 16)
        dut.counter.value.simPublic()
        dut
    }.doSim { dut =>
        import dut.clockDomain
        import dut.io._

        import scala.util.Random

        dut.clockDomain.forkStimulus(10)
        dut.init
        val fillData = Array.fill(bufferRamCount)(Array.fill(depth)(BigInt(dataWidth, scala.util.Random)))
        mutiRow.lineBuffer.print2D(fillData)
        dut.fill(fillData)
        for (i <- 0 until 1000) {
            val en       = Random.nextBoolean()
            val wData    = Array.fill(inCount)(BigInt(dataWidth, Random))
            val wAddress = BigInt(3, Random).toInt
            val rAddress = BigInt(3, Random).toInt
            wrEn #= en
            writeData.zip(wData).foreach { case (port, data) => port #= data }
            writeAddress #= wAddress
            readAddress #= rAddress
            sync.randomize()
            val rData = fillData.map(i => i(rAddress))
            clockDomain.waitSampling()
            //---------------------------assert-----------------------------------------
            val counter = dut.counter.value.toInt
            println("readAddress" + rAddress)
            println(rData.mkString(" "))
            println(readData.map(_.toBigInt).mkString(" "))
            if (en) {
                readData.slice(0, counter * inCount).zip(rData.slice(0, counter * inCount)).foreach{case(sig, gold) => assert(sig.toBigInt == gold)}
                readData.slice(counter * inCount, (counter + 1) * inCount).foreach(i=> assert(i.toBigInt == 0))
                readData.slice((counter + 1) * inCount, bufferRamCount).zip(rData.slice((counter + 1) * inCount, bufferRamCount)).foreach{case(sig, gold) => assert(sig.toBigInt == gold)}
                println("---------write--------")
                println("counter: " + counter)
                println("writeAddress: " + wAddress)
                println(wData.mkString(" "))
                for (i <- 0 until inCount) {
                    fillData(counter * inCount + i)(wAddress) = wData(i)
                }
                mutiRow.lineBuffer.print2D(fillData)
            }else{
                readData.zip(rData).foreach{case(sig, gold)=> assert(sig.toBigInt == gold)}
            }
        }
    }

    implicit class bufferRamRWSim(dut: bufferRamRW) {

        import dut.clockDomain
        import dut.io._

        def init = {
            wrEn #= false
            writeAddress #= 0
            writeData.foreach(_ #= 0)
            readAddress #= 0
            sync #= false
            clockDomain.waitSampling()
        }

        def fill(data: Array[Array[BigInt]]) = {
            readAddress.randomize()
            for (s <- 0 until bufferRamCount / inCount) {
                wrEn #= true
                sync #= false
                for (k <- 0 until depth) {
                    writeData.zip(data.slice(inCount * s, inCount * (s + 1))).foreach { case (port, datas) => port #= datas(k) }
                    writeAddress #= k
                    clockDomain.waitSampling()
                }
                sync #= true
                wrEn #= false
                clockDomain.waitSampling()
            }
        }
    }

}