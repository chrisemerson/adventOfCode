package uk.co.cemerson.aoc.Day23

import uk.co.cemerson.aoc.Util.IntCode.OutputConsumer
import java.math.BigInteger

class OutputInterface(private val inputInterfaces: Map<BigInteger, InputInterface>, private val nat: NAT) : OutputConsumer {
    private var output1: BigInteger? = null
    private var output2: BigInteger? = null

    override fun consumeOutput(output: BigInteger) {
        if (output1 == null) {
            output1 = output
        } else if (output2 == null) {
            output2 = output
        } else {
            if (output1 == 255.toBigInteger()) {
                nat.addToQueue(output2!!, output)
            } else if (inputInterfaces.containsKey(output1!!)) {
                inputInterfaces[output1!!]!!.addToQueue(output2!!)
                inputInterfaces[output1!!]!!.addToQueue(output)
            }

            output1 = null
            output2 = null
        }
    }

    override fun consumeFinalValueInPositionZero(output: BigInteger) {
    }
}