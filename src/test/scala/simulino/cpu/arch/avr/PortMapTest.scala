package simulino.cpu.arch.avr

import java.io.ByteArrayInputStream

import com.fasterxml.jackson.databind.ObjectMapper
import org.scalatest.path
import org.mockito.Mockito._
import org.mockito.Matchers
import simulino.cpu.Cpu
import simulino.cpu.arch.avr.ATmega.PortType._
import simulino.memory.{UnsignedByte, Memory}
import simulino.utils.TestUtils._

import scala.collection.mutable.ListBuffer
import scala.util.Try

/**
 * Created by dnwiebe on 5/24/15.
 */
class PortMapTest extends path.FunSpec {

  describe ("Regarding handler validation") {
    describe ("A handler that needs multiple ports that don't exist") {
      val handler = new SleepDetector ()
      val ports = Map ("UNNEC" -> Port ("UNNEC", 1000, 0, 8, Input))
      describe ("when validated") {
        val result = Try {PortMap.validatePortHandler (handler, ports)}

        it ("doesn't") {
          fails (result, new IllegalArgumentException ("PortHandler simulino.cpu.arch.avr.SleepDetector requires nonexistent ports EL, ER"))
        }
      }
    }
    describe ("A handler that needs just one port that doesn't exist") {
      val handler = new SleepDetector ()
      val ports = Map ("ER" -> Port ("ER", 1000, 0, 8, Input))
      describe ("when validated") {
        val result = Try {PortMap.validatePortHandler (handler, ports)}

        it ("doesn't") {
          fails (result, new IllegalArgumentException ("PortHandler simulino.cpu.arch.avr.SleepDetector requires nonexistent port EL"))
        }
      }
    }
    describe ("A handler without any problems") {
      val handler = new SleepDetector ()
      val ports = Map ("ER" -> Port ("ER", 1000, 0, 8, Input), "EL" -> Port ("EL", 1001, 0, 8, Input))
      it ("when validated doesn't throw anything") {
        PortMap.validatePortHandler (handler, ports)
      }

      describe ("when directed to show a voltage on a pin") {
        val cpu = mock (classOf[AvrCpu])
        handler.initialize (cpu)
        handler.showVoltageAtPin ("BOBBY", Some (14.5))

        it ("passes the buck to the Cpu") {
          verify (cpu).showVoltageAtPin ("BOBBY", Some (14.5))
        }
      }
    }
  }

  describe ("A JSON stream about port configurations") {
    val json =
      s"""
        |{
        |  "ports": [
        |    {"name": "EBL", "address": 4660, "lowBit": 0, "bitLength": 1, "direction": "O"},
        |    {"name": "EBR", "address": "0x1234", "lowBit": 1, "bitLength": 1, "direction": "O"},
        |    {"name": "EL", "address": 4660, "lowBit": 2, "bitLength": 1, "direction": "B"},
        |    {"name": "ER", "address": "0x1234", "lowBit": 3, "bitLength": 1, "direction": "B"},
        |    {"name": "MTH", "address": 4661, "lowBit": 4, "bitLength": 4, "direction": "O"}
        |  ],
        |  "portHandlers": [
        |    {
        |      "class": "simulino.cpu.arch.avr.ExpressionMaker",
        |      "params": ["fark", 2, "0x4789", "fleeb"]
        |    },
        |    {
        |      "class": "simulino.cpu.arch.avr.SleepDetector"
        |    }
        |  ]
        |}
      """.stripMargin

    describe ("converted to a JsonNode") {
      val mapper = new ObjectMapper ()
      val node = mapper.readTree (new ByteArrayInputStream (json.getBytes))

      describe ("used to initialize a PortConfiguration") {
        val config = PortConfiguration (node)

        it ("produces the correct port locations") {
          assert (config.ports === Seq (
            ("EBL", Port ("EBL", 4660, 0, 1, Output)),
            ("EBR", Port ("EBR", 4660, 1, 1, Output)),
            ("EL", Port ("EL", 4660, 2, 1, Both)),
            ("ER", Port ("ER", 4660, 3, 1, Both)),
            ("MTH", Port ("MTH", 4661, 4, 4, Output))
          ))
        }

        it ("produces the correct port handlers") {
          assert (config.portHandlers.map {_.getClass} === Seq (
            classOf[ExpressionMaker],
            classOf[SleepDetector]
          ))
          val slpd = config.portHandlers.find {_.getClass == classOf[ExpressionMaker]}.get.asInstanceOf[ExpressionMaker]
          assert (slpd.s1 === "fark")
          assert (slpd.s2 === "fleeb")
          assert (slpd.i1 === 2)
          assert (slpd.i2 === 0x4789)
        }

        describe ("which is then used to initialize a PortMap") {
          val cpu = mock (classOf[AvrCpu])
          val subject = new PortMap (cpu, List (config))
          when (cpu.portMap).thenReturn (subject)

          describe ("that sees a memory change that doesn't affect anything") {
            Changes.changes.clear ()
            subject.memoryChange (10, 11, 12)

            it ("produces no changes") {
              assert (Changes.changes.isEmpty === true)
            }
          }

          describe ("on a mocked AvrCpu with mocked memory") {
            val memory = mock (classOf[Memory])
            when (memory (4661)).thenReturn (UnsignedByte (0xCA))
            when (cpu.dataMemory).thenReturn (memory)

            describe ("that is instructed to read from a port") {
              val result = subject.readFromPort ("MTH")

              it ("reads the expected value") {
                assert (result === 0xC)
              }
            }

            describe ("that is instructed to write to a port") {
              subject.writeToPort ("MTH", 0x3)

              it ("makes the appropriate call") {
                verify (memory).update (4661, UnsignedByte (0x3A))
              }
            }
          }

          describe ("that sees a memory change that affects a port handler") {
            Changes.changes.clear ()
            subject.memoryChange (4660, 0x08, 0x00)

            it ("produces two Change events") {
              assert (Changes.changes.toList === List ("Checking left eye", "Checking right eye"))
            }

            it ("results in right eye closed") {
              assert (subject.handler ("SLPD").get.asInstanceOf[SleepDetector].rightEyeClosed === true)
            }
          }

          describe ("that experiences an incoming change from a PortHandler") {
            val memory = mock (classOf[Memory])
            when (memory (Matchers.anyInt())).thenReturn (UnsignedByte (0x00))
            when (cpu.dataMemory).thenReturn (memory)
            val handler = subject.handler ("EXMK").get.asInstanceOf[ExpressionMaker]
            handler.smile ()

            it ("the proper writes to memory are performed") {
              verify (memory).update (4660, UnsignedByte (0x01))
              verify (memory).update (4660, UnsignedByte (0x02))
              verify (memory).update (4660, UnsignedByte (0x04))
              verify (memory).update (4660, UnsignedByte (0x08))
              verify (memory).update (4661, UnsignedByte (0xF0))
            }
          }

          describe ("that is given to a handler") {
            val handler = new PortHandler () {
              override val name = "Bandit"
              override val portNames = Nil
              override def acceptChange (portName: String, oldValue: Int, newValue: Int): Unit = {readFromPort ("ER")}
            }
            handler.initialize (cpu)

            describe ("that reads from an unrequested port") {
              val result = Try {handler.acceptChange ("blah", 0, 0)}

              it ("complains") {
                fails (result, new IllegalArgumentException (s"${handler.getClass.getName} attempted read from unrequested port ER"))
              }
            }

            describe ("that tries to reinitialize") {
              val result = Try {handler.initialize (cpu)}

              it ("complains") {
                fails (result, new IllegalStateException (s"${handler.getClass.getName} may not be reinitialized"))
              }
            }
          }

          describe ("that is given to a handler") {
            val handler = new PortHandler () {
              override val name = "Bandit"
              override val portNames = Nil
              override def acceptChange (portName: String, oldValue: Int, newValue: Int): Unit = {writeToPort ("EL", 0)}
            }
            handler.initialize (cpu)

            describe ("that writes to an unrequested port") {
              val result = Try {handler.acceptChange ("blah", 0, 0)}

              it ("complains") {
                fails (result, new IllegalArgumentException (s"${handler.getClass.getName} attempted write to unrequested port EL"))
              }
            }
          }
        }
      }
    }
  }

  describe ("A JSON stream with port-handler info that doesn't match reality") {
    val json =
      s"""
         |{
         |  "ports": [],
         |  "portHandlers": [
         |    {
         |      "class": "simulino.cpu.arch.avr.ExpressionMaker",
         |      "params": ["fark", 2, "0x4789"]
         |    }
         |  ]
         |}
      """.stripMargin
    val mapper = new ObjectMapper ()
    val node = mapper.readTree (new ByteArrayInputStream (json.getBytes))

    describe ("used to initialize a PortConfiguration") {
      val result = Try {PortConfiguration (node)}

      it ("fails in the expected way") {
        fails (result, new NoSuchMethodException ("simulino.cpu.arch.avr.ExpressionMaker.<init>(java.lang.String, int, int)"))
      }
    }
  }
}

object Changes {
  val changes = new ListBuffer[String] ()
}

class ExpressionMaker (val s1: String, val i1: Int, val i2: Int, val s2: String) extends PortHandler {
  override val name = "EXMK"
  override val portNames = List ("EBL", "EL", "EBR", "ER", "MTH")

  def smile (): Unit = {
    writeToPort ("EBL", 1)
    writeToPort ("EBR", 1)
    writeToPort ("EL", 1)
    writeToPort ("ER", 1)
    writeToPort ("MTH", 0xF)
  }

  def frown (): Unit = {
    writeToPort ("EBL", 0)
    writeToPort ("EBR", 0)
    writeToPort ("EL", 1)
    writeToPort ("ER", 1)
    writeToPort ("MTH", 0x0)
  }
}

class SleepDetector () extends PortHandler {
  var leftEyeClosed = false
  var rightEyeClosed = false
  override val name = "SLPD"
  override val portNames = List ("EL", "ER")
  override def acceptChange (portName: String, oldValue: Int, newValue: Int): Unit = {
    portName match {
      case "EL" => Changes.changes += "Checking left eye"; leftEyeClosed = (newValue == 0)
      case "ER" => Changes.changes += "Checking right eye"; rightEyeClosed = (newValue == 0)
    }
  }

  override def showVoltageAtPin (chipPin: String, voltage: Option[Double]): Unit = {
    super.showVoltageAtPin (chipPin, voltage)
  }

  def isAsleep = leftEyeClosed && rightEyeClosed
}
