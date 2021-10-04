package mutiRow

import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._


case class peOutBufferPara(Hout: Int = 16, Channel: Int = 9, Iw: Int = 4, Wh: Int = 3, dataWidth: Int = 8)

case class peOutBuffer(para: peOutBufferPara) extends Component {

    import para._

    val IwCount       = scala.math.ceil(Hout.toDouble / Iw.toDouble).toInt
    val WhCount       = scala.math.ceil(Channel.toDouble / Wh.toDouble).toInt
    // --------------------------------IO-------------------------------------------------------------------------------
    val dataIn        = slave Stream (Vec(Bits(dataWidth bits), Iw * Wh))
    val transferStart = in Bool()
    val address       = out Vec(UInt(log2Up(Channel) bits), Wh)
    val dataOut       = out Vec(Bits(dataWidth bits), Hout)
    val wrEn          = out Vec(Bool(), IwCount)
    val transferEnd   = out Bool()
    //-------default value of of combinatorial output ports-------------------------------------------------------------
    dataIn.ready := False
    transferEnd := False
    address.foreach(_ := 0)
    dataOut.foreach(_ := 0)
    wrEn.foreach(_ := False)
    //-----------------------------counters-----------------------------------------------------------------------------
    val ADDR            = Counter(0, IwCount - 1) // 0, 1, 2, 3
    val counter         = Counter(0, 2047) // counter for write times
    val dataWritten     = Reg(UInt(log2Up(IwCount * Channel) bits)) init (0) // counter for the write Iw data
    val decreaseCounter = Reg(UInt(log2Up(Wh) bits)) init (Wh - 1)
    val addressInc      = Vec(Reg(UInt(log2Up(Channel) bits)) init (0), Wh)
    val wrEnRegs        = Vec(Reg(Bool()) init (False), IwCount)
    //------------------------------state machine-----------------------------------------------------------------------
    val FSM             = new StateMachine {
        val idle = new State with EntryPoint
        val rcv  = new State
        val end  = new State

        idle.whenIsActive {
            when(transferStart) {
                goto(rcv)
                wrEnRegs(0) := True
            }
        }
        rcv.whenIsActive {
            dataIn.ready := True
            when(dataIn.valid) {
                counter.increment()
                ADDR.increment()
                // wrEnRegs
                when(ADDR.value === IwCount - 1) {
                    wrEnRegs(0) := True
                } elsewhen (ADDR.value === Wh - 1) {
                    wrEnRegs(0) := False
                }
                for (i <- 0 until IwCount - 1) {
                    wrEnRegs(i + 1) := wrEnRegs(i)
                }
                wrEn.zip(wrEnRegs).foreach { case (w, r) => w := r }
                // ram address
                for (i <- 0 until Wh - 1) {
                    addressInc(i + 1) := addressInc(i) + 1
                }
                when(counter.valueNext % Iw === 0) {
                    addressInc(0) := addressInc(0) + Wh
                }
                address.zip(addressInc).foreach { case (c, r) => c := r }
                // dataOut
                switch(ADDR.value) {
                    for (a <- 0 until IwCount) {
                        is(a) {
                            for (i <- 0 until Wh) {
                                val head = (a - i + IwCount) % IwCount
                                dataOut.slice(head * Iw, (head + 1) * Iw).zip(dataIn.payload.slice(i * Iw, (i + 1) * Iw)).foreach { case (o, i) => o := i }
                            }
                        }
                    }
                }
                when(counter.value < Wh - 1) { // Part of  data in the beginning is invalid
                    dataWritten := dataWritten + counter.value.resize(dataWritten.getWidth) + 1
                } elsewhen (dataWritten >= IwCount * Channel - Wh * (Wh - 1) / 2) { // Part of the data in the last few transfers is invalid
                    dataWritten := dataWritten + decreaseCounter.resize(dataWritten.getWidth)
                    decreaseCounter := decreaseCounter - 1
                    switch(decreaseCounter) {
                        wrEn.foreach(_ := False)
                        for (d <- 1 to Wh - 1) {
                            is(d) {
                                for (i <- 0 until d)
                                    wrEn(IwCount - 1 - i) := True
                            }
                        }
                    }

                    when(dataWritten === IwCount * Channel - 1) {
                        decreaseCounter := Wh - 1
                        counter.clearAll()
                        dataWritten.clearAll()
                        ADDR.clearAll()
                        addressInc.foreach(_.clearAll())
                        wrEnRegs.foreach(_.clear())
                        goto(end)
                    }
                } otherwise {
                    dataWritten := dataWritten + Wh
                }
            }
        }
        end.whenIsActive {
            transferEnd := True
            goto(idle)
        }
    }
}

object peOutBufferSim extends App {
    val para = peOutBufferPara(16, 9, 4, 3, 8)
    SimConfig.withWave.withConfig(SpinalConfig(
        defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = HIGH),
        defaultClockDomainFrequency = FixedFrequency(100 MHz)
        )).compile(new peOutBuffer(para)).doSim { dut =>
        import dut._
        dut.clockDomain.forkStimulus(10)
        dataIn.valid #= false
        transferStart #= false
        dataIn.payload.foreach(_ #= 0)
        clockDomain.waitSampling()
        transferStart #= true
        clockDomain.waitSampling()
        transferStart #= false
        dataIn.valid #= true
        for (i <- 0 until 100) {
            dataIn.payload.randomize()
            clockDomain.waitSampling()
        }
    }
}