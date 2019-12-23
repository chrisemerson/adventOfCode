package uk.co.cemerson.aoc.Day23

import java.math.BigInteger

class NAT {
    private var firstPacketX = 0.toBigInteger()
    private var firstPacketY = 0.toBigInteger()

    private var lastPacketX = 0.toBigInteger()
    private var lastPacketY = 0.toBigInteger()

    private var newPacketReceived = false

    fun addToQueue(x: BigInteger, y: BigInteger) {
        if (!newPacketReceived) {
            firstPacketX = x
            firstPacketY = y
        }

        lastPacketX = x
        lastPacketY = y

        newPacketReceived = true
    }

    fun isNewPacketReceived(): Boolean = newPacketReceived

    fun getFirstReceivedPacket(): Pair<BigInteger, BigInteger> {
        return Pair(firstPacketX, firstPacketY)
    }

    fun getLastReceivedPacket(): Pair<BigInteger, BigInteger> {
        newPacketReceived = false

        return Pair(lastPacketX, lastPacketY)
    }
}