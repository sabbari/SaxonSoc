package saxon.board.digilent

import tesic._

import java.awt.image.BufferedImage
import java.awt.{Color, Dimension, Graphics}
import java.io.{ByteArrayOutputStream, FileInputStream, FileOutputStream}
import javax.swing.{JFrame, JPanel, WindowConstants}
import saxon.common.I2cModel
import saxon._
import spinal.core._
import spinal.core.fiber._
import spinal.core.sim._
import spinal.lib.blackbox.xilinx.s7.{BSCANE2, BUFG, STARTUPE2}
import spinal.lib.bus.amba3.apb.{Apb3, Apb3Config, Apb3SlaveFactory}
import spinal.lib.bus.bmb._
import spinal.lib.bus.bsb.BsbInterconnectGenerator
import spinal.lib.bus.misc.{AddressMapping, SingleMapping, SizeMapping}
import spinal.lib.bus.simple.{PipelinedMemoryBus, PipelinedMemoryBusDecoder}
import spinal.lib.com.eth.{MacEthParameter, PhyParameter}
import spinal.lib.com.jtag.sim.JtagTcp
import spinal.lib.com.jtag.{Jtag, JtagTap, JtagTapDebuggerGenerator, JtagTapInstructionCtrl}
import spinal.lib.com.jtag.xilinx.Bscane2BmbMasterGenerator
import spinal.lib.com.spi.ddr.{SpiXdrMasterCtrl, SpiXdrParameter}
import spinal.lib.com.uart.UartCtrlMemoryMappedConfig
import spinal.lib.com.uart.sim.{UartDecoder, UartEncoder}
import spinal.lib.generator._
import spinal.lib.graphic.RgbConfig
import spinal.lib.graphic.vga.{BmbVgaCtrlGenerator, BmbVgaCtrlParameter, Vga}
import spinal.lib.io.{Gpio, InOutWrapper}
import spinal.lib.memory.sdram.sdr._
import spinal.lib.memory.sdram.xdr.CoreParameter
import spinal.lib.memory.sdram.xdr.phy.XilinxS7Phy
import spinal.lib.misc.analog.{BmbBsbToDeltaSigmaGenerator, BsbToDeltaSigmaParameter}
import spinal.lib.{StreamFifo, slave}
import spinal.lib.system.dma.sg.{DmaMemoryLayout, DmaSgGenerator}
import vexriscv.demo.smp.VexRiscvSmpClusterGen
import vexriscv.ip.fpu.{FpuCore, FpuParameter}
import vexriscv.plugin.{AesPlugin, FpuPlugin}


// Define a SoC abstract enough to be used in simulation (no PLL, no PHY)
class ArtyA7SmpLinuxAbstract(cpuCount: Int) extends VexRiscvClusterGenerator(cpuCount) {
  val fabric = withDefaultFabric()

  val gpioA = BmbGpioGenerator(0x00000)

  val uartA = BmbUartGenerator(0x10000)
  uartA.connectInterrupt(plic, 1)

  val spiA = new BmbSpiGenerator(0x20000) {
    val decoder = SpiPhyDecoderGenerator(phy)
    val user = decoder.spiMasterNone()
    val flash = decoder.spiMasterId(0)
    //val sdcard = decoder.spiMasterId(1)
    val md = decoder.mdioMasterId(2) //Ethernet phy
  }


  val myApbBmb = BmbToApb3Generator(SizeMapping(0x50000, BigInt(1) << log2Up(8)))
  myApbBmb.apb3Config.load(Apb3Config(
    addressWidth = 4,
    dataWidth = 32
  ))
  val apbArea = Handle(new Area {
      val myreg = Reg(UInt(32 bit)).init(U"xFFF0F0F0")
      val myreg1 = Reg(UInt(32 bit)).init(U"x0F0F0FFF")
      val busCtrl = Apb3SlaveFactory(myApbBmb.output)
      busCtrl.read(myreg, address = 0)
      busCtrl.write(myreg1, address = 4)

  })



  val ramA = BmbOnChipRamGenerator(0xA00000l)
  ramA.hexOffset = bmbPeripheral.mapping.lowerBound
  ramA.dataWidth.load(32)
  interconnect.addConnection(bmbPeripheral.bmb, ramA.ctrl)

  interconnect.addConnection(
    fabric.iBus.bmb -> List(/*sdramA0.bmb, */ bmbPeripheral.bmb),
    fabric.dBus.bmb -> List(/*sdramA0.bmb,*/ bmbPeripheral.bmb)
  )


  val fpuParameter = FpuParameter(
    withDouble = true
  )

  val fpu = new Generator {
    dependencies ++= cores.map(_.logic)
    val logic = add task new FpuCore(
      portCount = cpuCount,
      p = fpuParameter
    )

    val connect = add task new Area {
      for (i <- 0 until cpuCount;
           vex = cores(i).logic.cpu;
           port = logic.io.port(i)) {
        val plugin = vex.service(classOf[FpuPlugin])
        plugin.port.cmd >> port.cmd
        plugin.port.commit >> port.commit
        plugin.port.completion := port.completion.stage()
        plugin.port.rsp << port.rsp
      }
    }
  }


}

class ArtyA7SmpLinux(cpuCount: Int) extends Component {
  // Define the clock domains used by the SoC
  val debugCd = ClockDomainResetGenerator()
  debugCd.holdDuration.load(4095)
  debugCd.enablePowerOnReset()



  val resetCd = ClockDomainResetGenerator()
  resetCd.holdDuration.load(63)
  resetCd.asyncReset(debugCd)

  val systemCd = ClockDomainResetGenerator()
  systemCd.holdDuration.load(63)
  systemCd.asyncReset(resetCd)
  systemCd.setInput(
    debugCd.outputClockDomain,
    omitReset = true
  )

  // ...
  val system = systemCd.outputClockDomain on new ArtyA7SmpLinuxAbstract(cpuCount) {

  }

  // Enable native JTAG debug
  val debug = system.withDebugBus(debugCd, systemCd, 0x10B80000).withBscane2(userId = 2)
  //val debug = system.withDebugBus(debugCd, systemCd, 0x10B80000).withJtag()
  // TESIC JTAG

  //Manage clocks and PLL
  val clocking = new Area {
    val GCLK100 = in Bool()

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

    val clk25 = out Bool()
    clk25 := pll.CLKOUT5

    debugCd.setInput(
      ClockDomain(
        clock = pll.CLKOUT0,
        frequency = FixedFrequency(100 MHz)
      )
    )
    resetCd.setInput(
      ClockDomain(
        clock = pll.CLKOUT1,
        frequency = FixedFrequency(150 MHz)
      )
    )
    //vgaCd.setInput(ClockDomain(clk25))
    // system.vga.vgaCd.load(vgaCd.outputClockDomain)

    //    sdramDomain.phyA.clk90.load(ClockDomain(pll.CLKOUT2))
    //    sdramDomain.phyA.serdesClk0.load(ClockDomain(pll.CLKOUT3))
    //    sdramDomain.phyA.serdesClk90.load(ClockDomain(pll.CLKOUT4))
  }

  // Allow to access the native SPI flash clock pin
  val startupe2 = system.spiA.flash.produce(
    STARTUPE2.driveSpiClk(system.spiA.flash.sclk.setAsDirectionLess())
  )
  val jtag = slave(Jtag())
  val jtagArea = ClockDomain(jtag.tck, systemCd.outputClockDomain.reset)(new Area {
    val myFifo = StreamFifo(UInt(32 bits), 5)
    val tap = new JtagTapTesic(jtag, 4)
    val idcodeArea = tap.idcode(B"x10005FFF")(1)
    val bypassarea = tap.bypass()(2)
    val h2t = tap.h2t(myFifo.io.push)(3)
    val t2h = tap.t2h(myFifo.io.pop)(4)
  })


}

object ArtyA7SmpLinuxAbstract {
  def default(g: ArtyA7SmpLinuxAbstract) = g {
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

    //    interconnect.lock.retain()

    gpioA.parameter load Gpio.Parameter(
      width = 32,
      interrupt = List(24, 25, 26, 27)
    )
    gpioA.connectInterrupts(plic, 4)

    spiA.parameter load SpiXdrMasterCtrl.MemoryMappingParameters(
      SpiXdrMasterCtrl.Parameters(
        dataWidth = 8,
        timerWidth = 12,
        spi = SpiXdrParameter(
          dataWidth = 2,
          ioRate = 1,
          ssWidth = 3
        )
      ).addFullDuplex(id = 0).addHalfDuplex(id = 1, rate = 1, ddr = false, spiWidth = 1, lateSampling = false),
      cmdFifoDepth = 256,
      rspFifoDepth = 256
    )

    for (cpu <- cores) interconnect.setPipelining(cpu.dBus)(cmdValid = true, invValid = true, ackValid = true, syncValid = true)
    interconnect.setPipelining(fabric.exclusiveMonitor.input)(cmdValid = true, cmdReady = true, rspValid = true)
    interconnect.setPipelining(fabric.invalidationMonitor.output)(cmdValid = true, cmdReady = true, rspValid = true)
    interconnect.setPipelining(fabric.dBus.bmb)(cmdValid = true, cmdReady = true)
    interconnect.setPipelining(bmbPeripheral.bmb)(cmdHalfRate = true, rspHalfRate = true)
    // interconnect.setPipelining(sdramA0.bmb)(cmdValid = true, cmdReady = true, rspValid = true)
    interconnect.setPipelining(fabric.iBus.bmb)(cmdValid = true)
    //interconnect.setPipelining(dma.read)(cmdHalfRate = true)

    //    val myReg=Reg(UInt(32 bit))
    //    val myapb= slave(Apb3(Apb3Config(
    //      addressWidth = 1,
    //      dataWidth    = 32
    //       )))
    //        val busCtrl = Apb3SlaveFactory(myapb)
    //        busCtrl.driveAndRead(myReg,address = 0)
    //
    //    val myApbBmb = BmbToApb3Generator(SingleMapping(0x30000))(interconnect)
    //     bmbPeripheral.bmb<> myApbBmb.input
    ////   interconnect.addConnection(
    ////    bmbPeripheral.bmb           -> List(myApbBmb.input)  )
    //       myApbBmb.output <> myapb
  }
}


object ArtyA7SmpLinux {
  //Generate the SoC
  def main(args: Array[String]): Unit = {
    val cpuCount = 2 //sys.env.apply("SAXON_CPU_COUNT").toInt

    val report = SpinalRtlConfig
      .copy(
        defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC),
        inlineRom = true
      ).addStandardMemBlackboxing(blackboxByteEnables)
      .generateVerilog(InOutWrapper(new ArtyA7SmpLinux(cpuCount) {
        ArtyA7SmpLinuxAbstract.default(system)
        system.ramA.hexInit.load("software/standalone/memoryTest/build/memoryTest.hex")
        setDefinitionName("ArtyA7SmpLinux")
      }))
    BspGenerator("digilent/ArtyA7SmpLinux", report.toplevel, report.toplevel.system.cores(0).dBus)
  }
}

object VgaDisplaySim {
  def apply(vga: Vga, cd: ClockDomain): Unit = {

    var width = 160
    var height = 120
    val image = new BufferedImage(width, height, BufferedImage.TYPE_INT_BGR);

    val frame = new JFrame {
      setPreferredSize(new Dimension(800, 600));

      add(new JPanel {
        this.setPreferredSize(new Dimension(width, height))

        override def paintComponent(g: Graphics): Unit = {
          g.drawImage(image, 0, 0, width * 4, height * 4, null)
        }
      })

      pack();
      setVisible(true);
      setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);
    }

    //    def resize(newWidth : Int, newHeight : Int): Unit ={
    //
    //    }
    var overflow = false
    var x, y = 0
    cd.onSamplings {
      val vsync = vga.vSync.toBoolean
      val hsync = vga.hSync.toBoolean
      val colorEn = vga.colorEn.toBoolean
      if (colorEn) {
        val color = vga.color.r.toInt << (16 + 8 - vga.rgbConfig.rWidth) | vga.color.g.toInt << (8 + 8 - vga.rgbConfig.gWidth) | vga.color.b.toInt << (0 + 8 - vga.rgbConfig.bWidth)
        if (x < width && y < height) {
          image.setRGB(x, y, color)
        }
        x += 1
      }
      if (!vsync) {
        y = 0
      }
      if (!hsync) {
        if (x != 0) {
          y += 1
          frame.repaint()
        }
        x = 0
      }
    }


  }
}


object ArtyA7SmpLinuxSystemSim {

  import spinal.core.sim._

  def main(args: Array[String]): Unit = {

    case class Config(trace: Boolean, bin: String)
    val parser = new scopt.OptionParser[Config]("SpinalCore") {
      opt[Boolean]("trace") action { (v, c) => c.copy(trace = v) } text ("Store fst wave")
      opt[String]("bin") action { (v, c) => c.copy(bin = v) } text ("Baremetal app")
    }

    val config = parser.parse(args, Config(
      trace = true,
      bin = "software/standalone/timerAndGpioInterruptDemo/build/timerAndGpioInterruptDemo_spinal_sim.bin"
    )) match {
      case Some(config) => config
      case None => ???
    }


    val simConfig = SimConfig
    simConfig.allOptimisation
    // simConfig.withFstWave
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


      val top = systemCd.outputClockDomain on new ArtyA7SmpLinuxAbstract(cpuCount = 2) {

        //      val vgaCd = ClockDomainResetGenerator()
        //      vgaCd.holdDuration.load(63)
        //      vgaCd.makeExternal(withResetPin = false)
        //      vgaCd.asyncReset(debugCd)
        //
        //      vga.vgaCd.merge(vgaCd.outputClockDomain)


        val jtag = slave(Jtag()).setName("Tesic")
        val jtagArea = ClockDomain(jtag.tck, debugCd.outputClockDomain.reset)(new Area {
          val myFifo = StreamFifo(UInt(32 bits), 5)
          val tap = new JtagTapTesic(jtag, 4)
          val idcodeArea = tap.idcode(B"x10005FFF")(1)
          val bypassarea = tap.bypass()(2)
          val h2t = tap.h2t(myFifo.io.push)(3)
          val t2h = tap.t2h(myFifo.io.pop)(4)
        })
        val jtagTap = withDebugBus(debugCd, systemCd, address = 0x10B80000).withJtag()
        //        withoutDebug


        ArtyA7SmpLinuxAbstract.default(this)
        ramA.hexInit.load("software/standalone/memoryTest/build/memoryTest.hex") //_spinal_sim.hex")
      }
    }.setDefinitionName("miaou2")).doSimUntilVoid("test", 42) { dut =>
      val debugClkPeriod = (1e12 / dut.debugCd.inputClockDomain.frequency.getValue.toDouble).toLong
      val jtagClkPeriod = debugClkPeriod * 4
      val uartBaudRate = 115200
      val uartBaudPeriod = (1e12 / uartBaudRate).toLong

      val clockDomain = dut.debugCd.inputClockDomain.get
      clockDomain.forkStimulus(debugClkPeriod)

      //      dut.vgaCd.inputClockDomain.get.forkStimulus(40000)
      //      clockDomain.forkSimSpeedPrinter(2.0)

      fork {
        val at = 0
        val duration = 20
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
        //jtag = dut.top.jtag,
        jtagClkPeriod = jtagClkPeriod
      )

      val uartTx = UartDecoder(
        uartPin = dut.top.uartA.uart.txd,
        baudPeriod = uartBaudPeriod
      )

      val uartRx = UartEncoder(
        uartPin = dut.top.uartA.uart.rxd,
        baudPeriod = uartBaudPeriod
      )
      println(jtagClkPeriod)
      println(debugClkPeriod)
      ////      val vga = VgaDisplaySim(dut.vga.output, dut.vgaCd.inputClockDomain)
      //      val vga = VgaDisplaySim(dut.top.vga.output, clockDomain)
      //
      //      dut.top.eth.mii.RX.DV #= false
      //      dut.top.eth.mii.RX.ER #= false
      //      dut.top.eth.mii.RX.CRS #= false
      //      dut.top.eth.mii.RX.COL #= false
      //      dut.top.spiA.sdcard.data.read #= 3

      //      val memoryTraceFile = new FileOutputStream("memoryTrace")
      //      clockDomain.onSamplings{
      //        val cmd = dut.sdramA0.bmb.cmd
      //        if(cmd.valid.toBoolean){
      //          val address = cmd.address.toLong
      //          val opcode = cmd.opcode.toInt
      //          val source = cmd.source.toInt
      //          val bytes = Array[Byte](opcode.toByte, source.toByte, (address >> 0).toByte, (address >> 8).toByte, (address >> 16).toByte, (address >> 24).toByte)
      //          memoryTraceFile.write(bytes)
      //        }
      //      }

      //      val images = "../buildroot-build/images/"
      //
      //      dut.top.phy.logic.loadBin(0x00F80000, images + "fw_jump.bin")
      //      dut.top.phy.logic.loadBin(0x00F00000, images + "u-boot.bin")
      //      dut.phy.logic.loadBin(0x00000000, images + "Image")
      //      dut.phy.logic.loadBin(0x00FF0000, images + "linux.dtb")
      //      dut.phy.logic.loadBin(0x00FFFFC0, images + "rootfs.cpio.uboot")
      //
      //      //Bypass uboot
      //      dut.phy.logic.loadBytes(0x00F00000, Seq(0xb7, 0x0f, 0x00, 0x80, 0xe7, 0x80, 0x0f,0x00).map(_.toByte))  //Seq(0x80000fb7, 0x000f80e7)


      //        dut.top.phy.logic.loadBin(0x00F80000, "software/standalone/fpu/build/fpu.bin")
      //      dut.phy.logic.loadBin(0x00F80000, "software/standalone/audioOut/build/audioOut.bin")
      //dut.phy.logic.loadBin(0x00F80000, "software/standalone/dhrystone/build/dhrystone.bin")
      //      dut.phy.logic.loadBin(0x00F80000, "software/standalone/timerAndGpioInterruptDemo/build/timerAndGpioInterruptDemo_spinal_sim.bin")
      //      dut.phy.logic.loadBin(0x00F80000, "software/standalone/freertosDemo/build/freertosDemo_spinal_sim.bin")
      //println("DRAM loading done")
    }
  }
}


//object MemoryTraceAnalyse extends App{
//  val stream = new FileInputStream("memoryTrace")
//  val data = stream.readAllBytes()
//  val size = data.size
//  println(s"Size : ${size}")
//
//  for( cacheBytes <- List(32 KiB, 64 KiB, 128 KiB, 256 KiB).map(_.toInt);
//       wayCount <- List(1, 2, 4, 8);
//       bytePerLine <- List(64)) {
//    val wayBytes = cacheBytes / wayCount
//    val linesPerWay = wayBytes / bytePerLine
//    val lineAddressShift = log2Up(bytePerLine)
//    val lineAddressMask = linesPerWay - 1
//    val tagMask = -wayBytes
//
//    var wayAllocator = 0
//    val ways = for (wayId <- 0 until wayCount) yield new {
//      val lines = for (lineId <- 0 until linesPerWay) yield new {
//        var address = 0
//        var valid = false
//        var age = 0
//
//        def hit(target: Int) = valid && (target & tagMask) == address
//      }
//    }
//
//
//    var writeThrough = true
//    var readHits = 0
//    var readMiss = 0
//    var writeHits = 0
//    var writeMiss = 0
//    for (i <- 0 until size by 6) {
//      val opcode = data(i + 0)
//      val source = data(i + 1)
//      val address = (data(i + 2) << 0) | (data(i + 3) << 8) | (data(i + 4) << 16) | (data(i + 5) << 24)
//      val lineId = (address >> lineAddressShift) & lineAddressMask
//      val allocate = !writeThrough || opcode == 0
//      ways.exists(_.lines(lineId).hit(address)) match {
//        case false => {
//          if (opcode == 0) readMiss += 1
//          else writeMiss += 1
//          if (allocate) {
//            var line = ways(0).lines(lineId)
//            for(way <- ways){
//              val alternative = way.lines(lineId)
//              if(alternative.age < line.age) line = alternative
//            }
//
//           // val line = ways(wayAllocator).lines(lineId)
//            line.valid = true
//            line.address = address & tagMask
//            line.age = i
//
//            wayAllocator += 1
//            wayAllocator %= wayCount
//          }
//        }
//        case true => {
//          if (opcode == 0) readHits += 1
//          else if(!writeThrough) writeHits += 1
//        }
//      }
//    }
//    println(f"cacheBytes=${cacheBytes/1024} KB wayCount=$wayCount bytePerLine=${bytePerLine} => readMissRate=${readMiss.toFloat/(readMiss+readHits)}%1.3f writeMissRate=${writeMiss.toFloat/(writeMiss+writeHits)}%1.3f readMiss=$readMiss writeMiss=$writeMiss")
//  }
//
//}
