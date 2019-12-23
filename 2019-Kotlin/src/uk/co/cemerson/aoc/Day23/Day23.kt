package uk.co.cemerson.aoc.Day23

import uk.co.cemerson.aoc.AOCDay
import uk.co.cemerson.aoc.Util.IntCode.Computer
import uk.co.cemerson.aoc.Util.IntCode.ProgramState
import java.math.BigInteger

class Day23 : AOCDay {
    val inputInterfaces = mutableMapOf<BigInteger, InputInterface>()
    val outputInterfaces = mutableMapOf<BigInteger, OutputInterface>()
    val computers = mutableMapOf<Int, Computer>()
    val programStates = mutableMapOf<Int, ProgramState>()
    val nat = NAT()

    override fun part1() {
        val program = getProgram()
        setupNetwork(program)

        while (!nat.isNewPacketReceived()) {
            for (computer in 0..49) {
                programStates[computer] = computers[computer]!!.executeProgramStep(programStates[computer]!!)
            }
        }

        println("Y value of first received packet: " + nat.getFirstReceivedPacket().second)
    }

    override fun part2() {
        val program = getProgram()
        setupNetwork(program)

        val lastYs = mutableListOf<BigInteger>()
        var done = false

        var stepsWithoutInput = 0

        while (!done) {
            for (computer in 0..49) {
                programStates[computer] = computers[computer]!!.executeProgramStep(programStates[computer]!!)
            }

            //If all the inputs are empty, we are idling - send last received packet to NAT to computer 0
            if (nat.isNewPacketReceived() && inputInterfaces.map { it.value.isIdle() }.filter { !it }.count() == 0) {
                stepsWithoutInput++
            }

            if (stepsWithoutInput > 1000) {
                stepsWithoutInput = 0

                val lastRcvdPacket = nat.getLastReceivedPacket()
                inputInterfaces[0.toBigInteger()]!!.addToQueue(lastRcvdPacket.first)
                inputInterfaces[0.toBigInteger()]!!.addToQueue(lastRcvdPacket.second)

                println("Sent value " + lastRcvdPacket.first + ", " + lastRcvdPacket.second + " to computer 0")

                if (lastRcvdPacket.second in lastYs) {
                    println("First Y value delieved to computer 0 twice: " + lastRcvdPacket.second)
                    done = true
                }

                lastYs.add(lastRcvdPacket.second)
            }
        }
    }

    private fun setupNetwork(program: List<BigInteger>) {
        //Create Input queues
        for (i in 0..49) {
            inputInterfaces[i.toBigInteger()] = InputInterface()
            inputInterfaces[i.toBigInteger()]!!.addToQueue(i.toBigInteger())
        }

        //And now output interfaces
        for (i in 0..49) {
            outputInterfaces[i.toBigInteger()] = OutputInterface(inputInterfaces, nat)
            computers[i] = Computer(inputInterfaces[i.toBigInteger()]!!, outputInterfaces[i.toBigInteger()]!!)
            programStates[i] = computers[i]!!.getInitialProgramState(program)
        }
    }

    private fun getProgram(): List<BigInteger> =
            readFileSplitByChar(filename = "Day23/input.txt", splitBy = ',')
                    .map { it.toBigInteger() }
}