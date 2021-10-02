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

object bufferRamRWPorts {
    def apply(ctrl: writeControl): bufferRamRWPorts = {
        bufferRamRWPorts(dataWidth = ctrl.dataWidth * ctrl.Hout, inCount = ctrl.N, bufferRamCount = ctrl.bufferRamCount, depth = ctrl.channel)
    }
}

case class bufferRamRW(dataWidth: Int, inCount: Int, bufferRamCount: Int, depth: Int) extends Component {
    val io = slave(bufferRamRWPorts(dataWidth, depth, inCount, bufferRamCount))
    noIoPrefix()
    // counter
    val counter = Reg(UInt(log2Up(bufferRamCount) bits)) init (0)
    when(io.sync) {
        when(counter === bufferRamCount - inCount) {
            counter := 0
        } otherwise {
            counter := counter + inCount
        }
    }
    // wrEn
    val wrEnVec = Vec(Bool(), bufferRamCount)
    wrEnVec.foreach(_ := False)
    when(io.wrEn) {
        for (i <- 0 until inCount) {
            wrEnVec(counter + i) := True
        }
    }
    // rams
    val rams = Array.fill(bufferRamCount)(ram(dataWidth, depth))
    rams.zipWithIndex.foreach { case (r, i) =>
        r.wrEn := wrEnVec(i)
        r.writeData := io.writeData(i % inCount)
        r.writeAddress := io.writeAddress
        r.readAddress := io.readAddress
    }
    io.readData.zip(rams).foreach { case (d, r) => d := r.readData }
}

object bufferRamRWSim extends App {

    import scala.util.Random._

    SimConfig.withWave.withConfig(SpinalConfig(
        defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = HIGH),
        defaultClockDomainFrequency = FixedFrequency(100 MHz)
        )).compile{
        val dut = new bufferRamRW(8, 2, 8, 8)
        dut.counter simPublic()
        dut
    }.doSim { dut =>
        import dut._
        import dut.io._
        dut.clockDomain.forkStimulus(10)
        dut.init
        // write B input
        val ramData = Array.fill(dut.bufferRamCount)(Array.fill(dut.depth)(BigInt(0)))
        val B = ramInput(ramData = ramData, width = dut.dataWidth, inCount = dut.inCount, depth = dut.depth)
        dut.loadRamInput(B)

        lineBuffer.print2D(ramData)
        for (i <- 0 until 10000) {
            val counterB = counter.toInt
            // write B data to ramData to record
            if(B.en){
                ramData.slice(counterB , counterB +  dut.inCount).zip(B.wData).foreach{case(r, w)=> r(B.wAddress) = w}
                println("counterB: " + counterB)
                println("writeAddressB: " + B.wAddress)
                println(B.wData.mkString(" "))
                lineBuffer.print2D(ramData)
            }
            // write A input
            val A = ramInput(ramData, dut.dataWidth, dut.inCount, depth = dut.depth)
            dut.loadRamInput(A)
            val counterA = dut.counter.toInt

            // assert B readData
            val readDataGetB = readData.map(_.toBigInt).toArray
            println("readAddressB: " + B.rAddress)
            println(readDataGetB.mkString(" "))
            if(B.en){
                readDataGetB.slice(0, counterB).zip(ramData.slice(0, counterB).map(_(B.rAddress))).foreach{case(p, d) => p ==d}
                readDataGetB.slice(counterB + dut.inCount, dut.depth).zip(ramData.slice(counterB + dut.inCount, dut.depth).map(_(B.rAddress))).foreach{case(p, d) => p ==d}
            }else{
                readDataGetB.zip(ramData.map(_(B.rAddress))).foreach{case(p, d) => assert(p == d)}
            }
            // writer A data to ramData to record
            if(A.en){
                ramData.slice(counterA , counterA + dut.inCount).zip(A.wData).foreach{case(r, w)=> r(A.wAddress) = w}
                println("counterA: " + counterA)
                println("writeAddressA: " + A.wAddress)
                println(A.wData.mkString(" "))
                lineBuffer.print2D(ramData)
            }
            // write B input again
            B.refresh
            dut.loadRamInput(B)
            // assert A readData
            val readDataGetA = readData.map(_.toBigInt).toArray
            println("readAddressB: " + A.rAddress)
            println(readDataGetA.mkString(" "))
            if(A.en){
                readDataGetA.slice(0, counterA).zip(ramData.slice(0, counterA).map(_(A.rAddress))).foreach{case(p, d) => p ==d}
                readDataGetA.slice(counterA + dut.inCount, dut.depth).zip(ramData.slice(counterA + dut.inCount, dut.depth).map(_(A.rAddress))).foreach{case(p, d) => p ==d}
            }else{
                readDataGetA.zip(ramData.map(_(A.rAddress))).foreach{case(p, d) => assert(p == d)}
            }

        }
    }

    case class ramInput(ramData: Array[Array[BigInt]], width: Int, inCount: Int, depth: Int) {
        var en           = nextBoolean()
        var wAddress     = nextInt(depth)
        var rAddress     = nextInt(depth)
        var wData        = Array.fill(inCount)(BigInt(width, scala.util.Random))
        var readDataGold = ramData.map(_ (rAddress))

        def refresh = {
            en = nextBoolean()
            wAddress = nextInt(depth)
            rAddress = nextInt(depth)
            wData = Array.fill(inCount)(BigInt(width, scala.util.Random))
            readDataGold = ramData.map(_ (rAddress))
        }
    }

    implicit class bufferRamRWSimMeth(dut: bufferRamRW) {

        import dut._
        import dut.io._

        def init = {
            wrEn #= false
            sync #= false
            writeData.foreach(_ #= 0)
            writeAddress #= 0
            readAddress #= 0
            clockDomain.waitSampling()
        }
        def loadRamInput(a: ramInput)={
            wrEn #= a.en
            readAddress #= a.rAddress
            writeAddress #= a.wAddress
            writeData.zip(a.wData).foreach{case(p,d)=> p#=d}
            sync.randomize()
            clockDomain.waitSampling()
        }
    }
}


case class ram(width: Int, depth: Int) extends Component {
    val readAddress, writeAddress = in UInt (log2Up(depth) bits)
    val writeData                 = in Bits (width bits)
    val readData                  = out Bits (width bits)
    val wrEn                      = in Bool()
    val content                   = Seq.fill(depth)(B(0))
    val Ram                       = Mem(Bits(width bits), content)
    Ram.addAttribute("ram_style", "block")
    Ram.write(writeAddress, writeData, wrEn)
    readData := Ram.readSync(readAddress, !wrEn)
}

object ramSim extends App {
    SimConfig.withWave.withConfig(SpinalConfig(
        defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = HIGH),
        defaultClockDomainFrequency = FixedFrequency(100 MHz)
        )).compile(new ram(8, 8)).doSim { dut =>
        import dut._
        clockDomain.forkStimulus(10)
        wrEn #= false
        writeAddress #= 0
        readAddress #= 0
        writeData #= 0
        clockDomain.waitSampling()
        for (i <- 0 until 100) {
            wrEn.randomize()
            writeAddress.randomize()
            readAddress.randomize()
            writeData.randomize()
            clockDomain.waitSampling()
        }
    }
}