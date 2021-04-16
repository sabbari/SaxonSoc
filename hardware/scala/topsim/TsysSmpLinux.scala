package topsim

import saxon._
import saxon.board.digilent.ArtyA7SmpLinuxAbstract
import spinal.core
import spinal.core._
import spinal.core.fiber._
import spinal.lib.blackbox.xilinx.s7.BUFG
import spinal.lib.bus.amba3.ahblite._
import spinal.lib.bus.bmb._
import spinal.lib.bus.misc.{SingleMapping, SizeMapping}
import spinal.lib.bus.bsb.BsbInterconnectGenerator
import spinal.lib.com.jtag.Jtag
import spinal.lib.com.jtag.sim.JtagTcp
import spinal.lib.com.uart.UartCtrlMemoryMappedConfig
import spinal.lib.com.uart.sim.{UartDecoder, UartEncoder}
import spinal.lib.generator._
import spinal.lib.io.InOutWrapper
import spinal.lib.memory.sdram.sdr._
import spinal.lib.memory.sdram.xdr.phy.XilinxS7Phy
import spinal.lib.{LatencyAnalysis, master, slave}
import vexriscv.demo.smp.VexRiscvSmpClusterGen
import vexriscv.ip.fpu.{FpuCore, FpuParameter}
import vexriscv.plugin.{AesPlugin, FpuPlugin}


// Define a SoC abstract enough to be used in simulation (no PLL, no PHY)
class TsysSmpLinuxAbstract(cpuCount: Int) extends VexRiscvClusterGenerator(cpuCount) {



  def withAHB(debugCd: Handle[ClockDomain], systemCd: ClockDomainResetGenerator) = new Area {

    val masterPort = debugCd on BmbOutPortGenerator(0x18000000l)
    masterPort.setName("ahb")
    masterPort.dataWidth.load(32)
    masterPort.size.load(4 KiB)
    interconnect.addConnection(fabric.dBus.bmb, masterPort.ctrl)
  }

  def withAhbRamslave()= new Area {
    val ahbConfig = AhbLite3Config(
      addressWidth = 32, //log2Up(4 KiB),
      dataWidth = 32
    )

    val ram = new AhbLite3Ram(ahbConfig, 4 KiB)

    val ahbPort = slave(AhbLite3Master(ahbConfig)).setName("AhbRamSlave")
    ahbPort <> ram.io.bus
  }
  val fabric = withDefaultFabric()


  val uartA = BmbUartGenerator(0x10000)
  uartA.connectInterrupt(plic, 1)
//  val ahbRam = Handle( new Area{
//    val ahbConfig = AhbLite3Config(
//      addressWidth = 32 ,//log2Up(8*1024*1024),
//      dataWidth    = 32
//    )
//    val ram = new AhbLite3Ram(ahbConfig, 8 MiB)
//    val ahbPort=slave(AhbLite3Master(ahbConfig))
//    ahbPort<>ram.io.bus
//  })

  implicit val bsbInterconnect = BsbInterconnectGenerator()
  val ramA = BmbOnChipRamGenerator(0xA00000l)
  ramA.hexOffset = bmbPeripheral.mapping.lowerBound
  interconnect.addConnection(bmbPeripheral.bmb, ramA.ctrl)


  interconnect.addConnection(
    fabric.iBus.bmb -> List(bmbPeripheral.bmb),
    fabric.dBus.bmb -> List(bmbPeripheral.bmb)
  )

  val fpu = new Area {
    val logic = Handle {
      new FpuCore(
        portCount = cpuCount,
        p = FpuParameter(
          withDouble = true,
          asyncRegFile = false
        )
      )
    }

    val connect = Handle {
      for (i <- 0 until cpuCount;
           vex = cores(i).logic.cpu;
           port = logic.io.port(i)) {
        val plugin = vex.service(classOf[FpuPlugin])
        plugin.port.cmd >> port.cmd
        plugin.port.commit >> port.commit
        plugin.port.completion := port.completion.stage()
        plugin.port.rsp << port.rsp

        if (i == 0) {
          println("cpuDecode to fpuDispatch " + LatencyAnalysis(vex.decode.arbitration.isValid, logic.decode.input.valid))
          println("fpuDispatch to cpuRsp    " + LatencyAnalysis(logic.decode.input.valid, plugin.port.rsp.valid))

          println("cpuWriteback to fpuAdd   " + LatencyAnalysis(vex.writeBack.input(plugin.FPU_COMMIT), logic.commitLogic(0).add.counter))

          println("add                      " + LatencyAnalysis(logic.decode.add.rs1.mantissa, logic.merge.arbitrated.value.mantissa))
          println("mul                      " + LatencyAnalysis(logic.decode.mul.rs1.mantissa, logic.merge.arbitrated.value.mantissa))
          println("fma                      " + LatencyAnalysis(logic.decode.mul.rs1.mantissa, logic.decode.add.rs1.mantissa, logic.merge.arbitrated.value.mantissa))
          println("short                    " + LatencyAnalysis(logic.decode.shortPip.rs1.mantissa, logic.merge.arbitrated.value.mantissa))

        }
      }
    }
  }
}

class TsysSmpLinux(cpuCount: Int) extends Component {
  // Define the clock domains used by the SoC
  val debugCdCtrl = ClockDomainResetGenerator()
  debugCdCtrl.holdDuration.load(4095)
  debugCdCtrl.enablePowerOnReset()


  //  val sdramCdCtrl = ClockDomainResetGenerator()
  //  sdramCdCtrl.holdDuration.load(63)
  //  sdramCdCtrl.asyncReset(debugCdCtrl)

  val systemCdCtrl = ClockDomainResetGenerator()
  systemCdCtrl.holdDuration.load(63)
  //systemCdCtrl.asyncReset(sdramCdCtrl)
  systemCdCtrl.setInput(
    debugCdCtrl.outputClockDomain,
    omitReset = true
  )

  val debugCd = BUFG.onReset(debugCdCtrl.outputClockDomain)
  //  val sdramCd  = BUFG.onReset(sdramCdCtrl.outputClockDomain)
  val systemCd = BUFG.onReset(systemCdCtrl.outputClockDomain)


  val system = systemCd on new TsysSmpLinuxAbstract(cpuCount) {


    // Enable native JTAG debug
    val debugBus = this.withDebugBus(debugCd, debugCdCtrl, 0x10B80000)
    val nativeJtag = debugBus.withBscane2(userId = 2)
    //enable sstap
    //val jtagssTap = this.withSSTAP(debugCd, debugCdCtrl)
    //this.withAHB(debugCd, debugCdCtrl)
  }


  //Manage clocks and PLL
  val clocking = new Area {
    val GCLK100 = in Bool()
    GCLK100.setName("usys_clk")
    val rstb = in Bool()
    rstb.setName("rstb")

    val pll = new BlackBox {
      setDefinitionName("PLLE2_ADV")

      addGenerics(
        "CLKIN1_PERIOD" -> 10.0,
        "CLKFBOUT_MULT" -> 12,
        "CLKOUT0_DIVIDE" -> 12,
        "CLKOUT0_PHASE" -> 0,
        "CLKOUT1_DIVIDE" -> 8,
        "CLKOUT1_PHASE" -> 0,
        "CLKOUT2_DIVIDE" -> 8,
        "CLKOUT2_PHASE" -> 45,
        "CLKOUT3_DIVIDE" -> 4,
        "CLKOUT3_PHASE" -> 0,
        "CLKOUT4_DIVIDE" -> 4,
        "CLKOUT4_PHASE" -> 90,
        "CLKOUT5_DIVIDE" -> 48,
        "CLKOUT5_PHASE" -> 0
      )

      val CLKIN1 = in Bool()

      val CLKFBIN = in Bool()
      val CLKFBOUT = out Bool()
      val CLKOUT0 = out Bool()
      val CLKOUT1 = out Bool()
      val CLKOUT2 = out Bool()
      val CLKOUT3 = out Bool()
      val CLKOUT4 = out Bool()
      val CLKOUT5 = out Bool()

      Clock.syncDrive(CLKIN1, CLKOUT1)
      Clock.syncDrive(CLKIN1, CLKOUT2)
      Clock.syncDrive(CLKIN1, CLKOUT3)
      Clock.syncDrive(CLKIN1, CLKOUT4)
      Clock.syncDrive(CLKIN1, CLKOUT5)
    }

    pll.CLKFBIN := pll.CLKFBOUT
    pll.CLKIN1 := GCLK100

    //    val clk25 = out Bool()
    //    clk25 := pll.CLKOUT5

    debugCdCtrl.setInput(
      ClockDomain(
        clock = pll.CLKOUT0,
        frequency = FixedFrequency(100 MHz),
        reset = rstb
      )
    )
  }
}

object TsysSmpLinuxAbstract {
  def default(g: TsysSmpLinuxAbstract) = g.rework {
    import g._

    // Configure the CPUs
    for ((cpu, coreId) <- cores.zipWithIndex) {
      cpu.config.load(VexRiscvSmpClusterGen.vexRiscvConfig(
        hartId = coreId,
        ioRange = _ (31 downto 28) === 0x1,
        resetVector = 0x10A00000l,
        iBusWidth = 64,
        dBusWidth = 64,
        loadStoreWidth = 64,
        iCacheSize = 4096 * 2,
        dCacheSize = 4096 * 2,
        iCacheWays = 2,
        dCacheWays = 2,
        iBusRelax = true,
        earlyBranch = true,
        withFloat = true,
        withDouble = true,
        externalFpu = true
      ))
      cpu.config.plugins += AesPlugin()
    }

    // Configure the peripherals
    ramA.size.load(8 KiB)
    ramA.hexInit.loadNothing()


    uartA.parameter load UartCtrlMemoryMappedConfig(
      baudrate = 115200,
      txFifoDepth = 128,
      rxFifoDepth = 128
    )


    // Add some interconnect pipelining to improve FMax
    for (cpu <- cores) interconnect.setPipelining(cpu.dBus)(cmdValid = true, invValid = true, ackValid = true, syncValid = true)
    interconnect.setPipelining(fabric.exclusiveMonitor.input)(cmdValid = true, cmdReady = true, rspValid = true)
    interconnect.setPipelining(fabric.invalidationMonitor.output)(cmdValid = true, cmdReady = true, rspValid = true)
    interconnect.setPipelining(fabric.dBus.bmb)(cmdValid = true, cmdReady = true)
    interconnect.setPipelining(bmbPeripheral.bmb)(cmdHalfRate = true, rspHalfRate = true)
    interconnect.setPipelining(fabric.iBus.bmb)(cmdValid = true)


    g
  }
}



class Top_wit_AHb(debugCd:ClockDomainResetGenerator,systemCd : ClockDomainResetGenerator) extends Component {
  val ahbConfig = AhbLite3Config(
    addressWidth = 32,
    dataWidth    = 32
  )

  //tsys is just a simplified version of Arty with some peripheral removed
  // ahbout creates an ahb master port
  val top = systemCd.outputClockDomain on new TsysSmpLinuxAbstract(cpuCount = 1) {
    val ahbOut= withAHB(debugCd.outputClockDomain, debugCd)
    val jtagTap = withDebugBus(debugCd.outputClockDomain, systemCd, address = 0x10B80000).withJtag()
    TsysSmpLinuxAbstract.default(this)
    ramA.hexInit.load("software/standalone/ApbJtag/build/ApbJtag.hex")

  }
}
class top_for_sim extends Component{
  //ios
  val jtag = slave(Jtag())
  val uart_rxd = in Bool
  val uart_txd = out Bool
  //
  val debugCd = ClockDomainResetGenerator()
  debugCd.enablePowerOnReset()
  debugCd.holdDuration.load(63)
  debugCd.makeExternal(
    frequency = FixedFrequency(100 MHz)
  )
  val systemCd = ClockDomainResetGenerator()
  systemCd.holdDuration.load(63)
  systemCd.setInput(debugCd)

  val top= new Top_wit_AHb(debugCd,systemCd)
  //AhbLite3Ram creates a ram accessible via and AHB slave
  val ahb_Ram = new AhbLite3Ram(AhbLite3Config(addressWidth = 32, dataWidth    = 32),
    4 KiB)
  //connecting ports
  top.top.ahbOut.masterPort.testLogic.port<>ahb_Ram.io.bus
  top.top.jtagTap.jtag<>jtag
  top.top.uartA.uart.rxd:=uart_rxd
  uart_txd:=top.top.uartA.uart.txd
}

object Top_connected_Sim {

  import spinal.core.sim._

  def main(args: Array[String]): Unit = {

    case class Config(trace: Boolean, bin: String)
    val parser = new scopt.OptionParser[Config]("SpinalCore") {
      opt[Boolean]("trace") action { (v, c) => c.copy(trace = v) } text ("Store fst wave")
      opt[String]("bin") action { (v, c) => c.copy(bin = v) } text ("Baremetal app")
    }

    val config = parser.parse(args, Config(
      trace = false,
      bin = "software/standalone/timerAndGpioInterruptDemo/build/timerAndGpioInterruptDemo_spinal_sim.bin"
    )) match {
      case Some(config) => config
      case None => ???
    }

    val simConfig = SimConfig
    simConfig.allOptimisation
    simConfig.withFstWave
    simConfig.addSimulatorFlag("-Wno-MULTIDRIVEN -Wno-LATCH")

    simConfig.compile(new top_for_sim().setDefinitionName("miaou2")).doSimUntilVoid("test", 42) { dut =>
      // val debugCd=dut.top.debugCd
      val debugClkPeriod = (1e12 / dut.debugCd.inputClockDomain.frequency.getValue.toDouble).toLong
      val uartBaudRate = 115200
      val uartBaudPeriod = (1e12 / uartBaudRate).toLong

      val clockDomain = dut.debugCd.inputClockDomain.get
      clockDomain.forkStimulus(debugClkPeriod)

      fork {
        val at = 0
        val duration = 0
        while (simTime() < at * 1000000000l) {
          disableSimWave()
          sleep(10000 * 10000)
          enableSimWave()
          sleep(100 * 10000)
        }
        println("\n\n********************")
        sleep(duration * 1000000000l)
        println("********************\n\n")
        while (true) {
          disableSimWave()
          sleep(100000 * 10000)
          enableSimWave()
          sleep(100 * 10000)
        }
      }

      val uartTx = UartDecoder(
        uartPin = dut.uart_txd,
        baudPeriod = uartBaudPeriod
      )

      val uartRx = UartEncoder(
        uartPin = dut.uart_rxd,
        baudPeriod = uartBaudPeriod
      )
    }
  }
}




object TsysSmpLinux {
  //Generate the SoC
  def main(args: Array[String]): Unit = {
    val cpuCount = 1 //sys.env.apply("SAXON_CPU_COUNT").toInt

    val report = SpinalRtlConfig
      .copy(
        defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC),
        inlineRom = true
      ).addStandardMemBlackboxing(blackboxByteEnables)
      .generateVerilog(InOutWrapper(new TsysSmpLinux(cpuCount) {
        TsysSmpLinuxAbstract.default(system)
        //system.withAhbRamslave(debugCd)
        system.withAHB(debugCd, debugCdCtrl)
        system.withAhbRamslave()
        system.ramA.hexInit.load("software/standalone/ApbJtag/build/ApbJtag.hex")
        setDefinitionName("tsys")
      }))
    BspGenerator("digilent/ArtyA7SmpLinux", report.toplevel, report.toplevel.system.cores(0).dBus)
  }
}


object TsysSmpLinuxSystemSim {

  import spinal.core.sim._

  def main(args: Array[String]): Unit = {

    case class Config(trace: Boolean, bin: String)
    val parser = new scopt.OptionParser[Config]("SpinalCore") {
      opt[Boolean]("trace") action { (v, c) => c.copy(trace = v) } text ("Store fst wave")
      opt[String]("bin") action { (v, c) => c.copy(bin = v) } text ("Baremetal app")
    }

    val config = parser.parse(args, Config(
      trace = false,
      bin = "software/standalone/timerAndGpioInterruptDemo/build/timerAndGpioInterruptDemo_spinal_sim.bin"
    )) match {
      case Some(config) => config
      case None => ???
    }

    val simConfig = SimConfig
    simConfig.allOptimisation
    simConfig.withFstWave
    simConfig.addSimulatorFlag("-Wno-MULTIDRIVEN -Wno-LATCH")

    simConfig.compile(new Component {
      val debugCd = ClockDomainResetGenerator()
      debugCd.enablePowerOnReset()
      debugCd.holdDuration.load(63)
      debugCd.makeExternal(
        frequency = FixedFrequency(100 MHz)
      )

      val systemCd = ClockDomainResetGenerator()
      systemCd.holdDuration.load(63)
      systemCd.setInput(debugCd)

      val top = systemCd.outputClockDomain on new TsysSmpLinuxAbstract(1) {
        val jtagTap = withDebugBus(debugCd.outputClockDomain, systemCd, address = 0x10B80000).withJtag()
        TsysSmpLinuxAbstract.default(this)
        ramA.hexInit.load("software/standalone/ApbJtag/build/ApbJtag.hex")

      }


    }.setDefinitionName("miaou2")).doSimUntilVoid("test", 42) { dut =>
     // val debugCd=dut.top.debugCd
      val debugClkPeriod = (1e12 / dut.debugCd.inputClockDomain.frequency.getValue.toDouble).toLong
      val jtagClkPeriod = debugClkPeriod * 4
      val uartBaudRate = 115200
      val uartBaudPeriod = (1e12 / uartBaudRate).toLong

      val clockDomain = dut.debugCd.inputClockDomain.get
      clockDomain.forkStimulus(debugClkPeriod)

      fork {
        val at = 0
        val duration = 0
        while (simTime() < at * 1000000000l) {
          disableSimWave()
          sleep(10000 * 10000)
          enableSimWave()
          sleep(100 * 10000)
        }
        println("\n\n********************")
        sleep(duration * 1000000000l)
        println("********************\n\n")
        while (true) {
          disableSimWave()
          sleep(100000 * 10000)
          enableSimWave()
          sleep(100 * 10000)
        }
      }

      val tcpJtag = JtagTcp(
          jtag = dut.top.jtagTap.jtag,
          jtagClkPeriod = jtagClkPeriod,
          port =7894
          //port =7899
        )
//      dut.top.ahbOut.masterPort.testLogic.port<>dut.topslave.io.bus
//      val tcpJtag2 = JtagTcp(
//        jtag = dut.top.jtagssTap.tap.jtagArea.jtag,
//        jtagClkPeriod = jtagClkPeriod,
//        port=7894      )
      val uartTx = UartDecoder(
        uartPin = dut.top.uartA.uart.txd,
        baudPeriod = uartBaudPeriod
      )

      val uartRx = UartEncoder(
        uartPin = dut.top.uartA.uart.rxd,
        baudPeriod = uartBaudPeriod
      )
    }
  }
}


