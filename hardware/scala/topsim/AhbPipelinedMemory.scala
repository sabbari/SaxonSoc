package topsim



import spinal.core._
import spinal.core.fiber.{Handle, Unset}
import spinal.lib.bus.amba3.ahblite.{AhbLite3, AhbLite3Config, AhbLite3Master}
import spinal.lib.bus.amba3.apb.Apb3Config
import spinal.lib.bus.bmb.{Bmb, BmbAccessCapabilities, BmbAccessParameter, BmbImplicitPeripheralDecoder, BmbInterconnectGenerator, BmbParameter, BmbToApb3Bridge}
import spinal.lib.bus.misc.{AddressMapping, SizeMapping}
import spinal.lib.bus.simple.{PipelinedMemoryBus, PipelinedMemoryBusConfig}
import spinal.lib.misc.{Clint, HexTools}
import spinal.lib.{CountOne, master, slave}
object BmbToAhbLite3{
  def getAhbLite3Config(p : BmbAccessParameter): AhbLite3Config = AhbLite3Config(
    addressWidth = p.addressWidth,
    dataWidth = p.dataWidth
  )
}
case class BmbForward(p: BmbParameter) extends Component{
  val io = new Bundle {
    val input = slave(Bmb(p))
    val output = master(Bmb(p))
  }

  io.output.cmd << io.input.cmd
  io.input.rsp << io.output.rsp


}

object BmbOutPort{
  def busCapabilities(size : BigInt, dataWidth : Int) = BmbAccessCapabilities(
    addressWidth  = log2Up(size),
    dataWidth     = dataWidth,
    lengthWidthMax   = log2Up(dataWidth/8),
    alignment     = BmbParameter.BurstAlignement.LENGTH
  )
}
case class BmbToAhbLite3(p :BmbParameter, addrMsbWidth: Int) extends Component{
  val config = BmbToAhbLite3.getAhbLite3Config(p.access)
  val config2 = AhbLite3Config(addressWidth = config.addressWidth + addrMsbWidth, dataWidth = config.dataWidth)
  val io = new Bundle {
    val input = slave(Bmb(p))
    val output = master(AhbLite3Master(config2))
    val addrMsb = in Bits( addrMsbWidth bits)
  }

  val beatCounter = Reg(UInt(p.access.beatCounterWidth bits)) init(0)
  val beatLast = beatCounter === io.input.cmd.transferBeatCountMinusOne
  when(io.input.cmd.valid && io.output.HREADY){
    beatCounter := (beatCounter + 1).resized
    when(io.input.cmd.ready && io.input.cmd.last){
      beatCounter := 0
    }
  }
  val cmdSize = CountOne(io.input.cmd.mask)-1
  val new_cmd = io.input.cmd.valid & io.output.HREADY

  val addrLsb = Bmb.addToAddress(io.input.cmd.address, beatCounter << log2Up(p.access.byteCount), p)
  io.output.HADDR     := addrLsb + (io.addrMsb  << addrLsb.getBitsWidth).asUInt
  io.output.HWRITE    := False
  io.output.HWDATA    := RegNextWhen(io.input.cmd.data, new_cmd)
  io.output.HBURST    := 0
  io.output.HMASTLOCK := False
  io.output.HPROT     := B"1111"
  io.output.HTRANS    := AhbLite3.IDLE
  io.output.HSIZE     := 0
  when(new_cmd) {
    io.output.HWRITE  := io.input.cmd.isWrite
    io.output.HTRANS  := AhbLite3.NONSEQ
    io.output.HSIZE   := B(cmdSize, 3 bits)
  }

  val pending_read = RegInit(False) clearWhen(io.output.HREADY) setWhen(new_cmd && !io.input.cmd.isWrite)
  val pending_write = RegInit(False) clearWhen(io.output.HREADY) setWhen(new_cmd && io.input.cmd.isWrite)

  io.input.cmd.ready := io.output.HREADY && (io.input.cmd.isWrite || beatLast)

io.input.rsp.valid     := io.output.HREADY && (pending_read || pending_write)
  io.input.rsp.data    := io.output.HRDATA
  io.input.rsp.source  := RegNext(io.input.cmd.source)
  io.input.rsp.context := RegNext(io.input.cmd.context)
  io.input.rsp.last    := RegNext(beatLast)
  io.input.rsp.setSuccess() //TODO
}
case class BmbToAhbLite3Generator(mapping : Handle[AddressMapping] = Unset,addrMsbWidth: Int)
                             (implicit interconnect: BmbInterconnectGenerator) extends Area {
  val input = Handle(logic.io.input)
  val output = Handle(logic.io.output)
  val addrMsb = Handle(logic.io.addrMsb)

  val accessSource = Handle[BmbAccessCapabilities]
  val accessRequirements = Handle[BmbAccessParameter]
  val logic = Handle(BmbToAhbLite3(
    p = accessRequirements.toBmbParameter(),
    addrMsbWidth = addrMsbWidth
  ))

  interconnect.addSlave(
    accessSource = accessSource,
    accessCapabilities = accessSource.derivate(Clint.getBmbCapabilities),
    accessRequirements = accessRequirements,
    bus = input,
    mapping = mapping
  )


}

case class BmbOutPort(p: BmbParameter, config: AhbLite3Config) extends Component {
  val io = new Bundle {
    val bus = slave(Bmb(p))
    val port = master(AhbLite3Master(config))
  }

  val addrMsbWidth = config.addressWidth - p.access.addressWidth
  val addrMsb = Bits(addrMsbWidth bits)
  addrMsb := 0
  val bridge = BmbToAhbLite3(p, addrMsbWidth).setCompositeName(this, "toAhbLite3", true)
  bridge.io.input << io.bus
  bridge.io.output <> io.port
  bridge.io.addrMsb := addrMsb

}

case class BmbOutPortGenerator(val address: Handle[BigInt] = Unset)
                              (implicit interconnect: BmbInterconnectGenerator) extends Area {
  val size      = Handle[BigInt]
  val dataWidth = Handle[Int]
  val requirements = Handle[BmbAccessParameter]
  val ctrl = Handle(testLogic.logic.io.bus)

  interconnect.addSlave(
    accessCapabilities = Handle(BmbOutPort.busCapabilities(size, dataWidth)),
    accessRequirements = requirements,
    bus = ctrl,
    mapping = Handle(SizeMapping(address, BigInt(1) << log2Up(size)))
  )
  val testLogic = Handle(new Area{
    val config  = AhbLite3Config(addressWidth = 32, dataWidth = 32)
    val logic = Handle(BmbOutPort(p = requirements.toBmbParameter(), config))
    val port = master(AhbLite3Master(config))
    port <> logic.io.port
  })
}

case class AhbLite3ToPipelinedMemoryBusBridge(ahbConfig: AhbLite3Config, pmpConfig : PipelinedMemoryBusConfig) extends Component{
  assert(ahbConfig.dataWidth == pmpConfig.dataWidth)

  val io = new Bundle {
    val ahb = slave(AhbLite3Master(ahbConfig))
    val pmb = master(PipelinedMemoryBus(pmpConfig))
  }

  val ahbReq = io.ahb.HTRANS === AhbLite3.NONSEQ

  val addr_stage_ready = io.pmb.rsp.valid | !io.pmb.cmd.isStall
  val addr_stage_req   = addr_stage_ready & ahbReq
  val pending_read      = RegInit(False) clearWhen(io.pmb.rsp.valid) setWhen(addr_stage_req && !io.ahb.HWRITE)

  io.pmb.cmd.valid := RegInit(False) clearWhen(io.pmb.cmd.ready && !ahbReq) setWhen(ahbReq && !pending_read)

  //assert(io.ahb.HBURST === 0)
  val cmdMask = io.ahb.HSIZE.mux(
    0 -> B"0001",
    1 -> B"0011"          ,
    default -> B"1111"
  ) |<< io.ahb.HADDR(1 downto 0)
  io.pmb.cmd.mask := RegNextWhen(cmdMask, addr_stage_req)
  //assert(io.ahb.HPROT === B"1111")
  //assert(io.ahb.HMASTLOCK === False)
  io.pmb.cmd.write := RegNextWhen(io.ahb.HWRITE, addr_stage_req)
  io.pmb.cmd.address := RegNextWhen(io.ahb.HADDR.resize(pmpConfig.addressWidth), addr_stage_req)
  io.pmb.cmd.data := io.ahb.HWDATA

  val write_accepted = io.pmb.cmd.valid & io.pmb.cmd.write & io.pmb.cmd.ready
  io.ahb.HREADY := write_accepted | io.pmb.rsp.valid | (!io.pmb.cmd.valid & !pending_read)
  io.ahb.HRDATA := io.pmb.rsp.data
  io.ahb.HRESP:=False
}

case class PipelinedMemoryBusRam(onChipRamSize : BigInt, config : PipelinedMemoryBusConfig, onChipRamHexFile : String = null) extends Component{
  val io = new Bundle{
    val bus = slave(PipelinedMemoryBus(config))
  }

  val ram = Mem(Bits(32 bits), onChipRamSize / 4)
  setName("Ahb_slave_memory")
  io.bus.rsp.valid := RegNext(io.bus.cmd.fire && !io.bus.cmd.write) init(False)
  io.bus.rsp.data := ram.readWriteSync(
    address = (io.bus.cmd.address >> 2).resized,
    data  = io.bus.cmd.data,
    enable  = io.bus.cmd.valid,
    write  = io.bus.cmd.write,
    mask  = io.bus.cmd.mask
  )
  io.bus.cmd.ready := True

  if(onChipRamHexFile != null){
    HexTools.initRam(ram, onChipRamHexFile, 0x00000000l)
  }
}

case class AhbLite3Ram(config: AhbLite3Config, size : BigInt, hexFile : String = null) extends Component{
  val io = new Bundle {
    val bus = slave(AhbLite3Master(config))
  }
  val pmpConfig = PipelinedMemoryBusConfig(addressWidth = log2Up(size), dataWidth = 32)
  val bridge = AhbLite3ToPipelinedMemoryBusBridge(config,pmpConfig)
  val ram = PipelinedMemoryBusRam(onChipRamSize = size, config = pmpConfig, onChipRamHexFile = hexFile)
  bridge.io.ahb <> io.bus
  bridge.io.pmb <> ram.io.bus
}

