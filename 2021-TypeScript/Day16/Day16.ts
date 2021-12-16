import * as fs from 'fs';

enum PacketType {
    Sum = 0,
    Product = 1,
    Minimum = 2,
    Maximum = 3,
    Literal = 4,
    GreaterThan = 5,
    LessThan = 6,
    EqualTo = 7
}

type Packet = {
    version: number,
    type: PacketType,
    packets: Packet[],
    value: number|null
};

function part1() {
    const packet = parsePacket(hex2bin(getInput()))[0];

    console.log(
        "Total of version numbers in parsed packets: " + getTotalPacketVersionSum(packet)
    );
}

function part2() {
    const packet = parsePacket(hex2bin(getInput()))[0];

    console.log(
        "Evaluation of packet value: : " + getPacketValue(packet)
    );
}

function hex2bin(hex: string): string {
    return hex
        .split('')
        .map(h => parseInt(h, 16).toString(2).padStart(4, '0'))
        .join('');
}

function parsePacket(packetString: string): [Packet, string] {
    let packet: Packet = {
        version: 0,
        type: PacketType.Literal,
        packets: [],
        value: null
    };

    packet.version = parseInt(packetString.substr(0, 3), 2);

    let type = parseInt(packetString.substr(3, 3), 2);
    let remainderString = '';

    if (type === 4) {
        //Literal value packet

        let valueString = '';
        let continueParsing = true;
        let pointer = 6;

        while (continueParsing) {
            continueParsing = false;

            const thisGroup = packetString.substr(pointer, 5);

            if (thisGroup[0] === '1') {
                continueParsing = true;
            }

            pointer += 5;

            valueString += thisGroup.substr(1, 4);
        }

        remainderString = packetString.substr(pointer);

        packet.value = parseInt(valueString, 2);
    } else {
        //Operator packet
        packet.type = type;

        const lengthTypeID = packetString.substr(6, 1);

        let subPackets: Packet[] = [];

        if (lengthTypeID === '0') {
            //Length is a 15 bit number containing length of all sub packets
            let length = parseInt(packetString.substr(7, 15), 2);
            remainderString = packetString.substr(22 + length);

            let subPacketsString = packetString.substr(22, length);
            let stillParsing = true;

            while (stillParsing) {
                stillParsing = false;

                let [thisSubPacket, subRemainderString] = parsePacket(subPacketsString);

                subPackets.push(thisSubPacket);

                if (subRemainderString.length > 0) {
                    subPacketsString = subRemainderString;
                    stillParsing = true;
                }
            }
        } else {
            //Length tells us the number of sub packets
            let counter = parseInt(packetString.substr(7, 11), 2);
            let subPacketsString = packetString.substr(18);

            while (counter > 0) {
                let [thisSubPacket, subRemainderString] = parsePacket(subPacketsString);

                subPackets.push(thisSubPacket);

                subPacketsString = subRemainderString;
                counter--;
            }

            remainderString = subPacketsString;
        }

        packet.packets = subPackets;
    }

    return [packet, remainderString];
}

function getTotalPacketVersionSum(packet: Packet): number {
    return packet.version + packet.packets.reduce((acc, curr) => acc + getTotalPacketVersionSum(curr), 0);
}

function getPacketValue(packet: Packet): number {
    switch (packet.type) {
        case PacketType.Sum:
            return packet.packets.reduce((acc, c) => acc + getPacketValue(c), 0);

        case PacketType.Product:
            return packet.packets.reduce((acc, c) => acc * getPacketValue(c), 1);

        case PacketType.Minimum:
            return Math.min(...packet.packets.map(p => getPacketValue(p)));

        case PacketType.Maximum:
            return Math.max(...packet.packets.map(p => getPacketValue(p)));

        case PacketType.Literal:
            return packet.value ?? 0;

        case PacketType.GreaterThan:
            return getPacketValue(packet.packets[0]) > getPacketValue(packet.packets[1]) ? 1 : 0;

        case PacketType.LessThan:
            return getPacketValue(packet.packets[0]) < getPacketValue(packet.packets[1]) ? 1 : 0;

        case PacketType.EqualTo:
            return getPacketValue(packet.packets[0]) === getPacketValue(packet.packets[1]) ? 1 : 0;
    }
}

function getInput(): string {
    return fs
        .readFileSync(__dirname + "/input.txt", 'utf8')
        .trim();
}

export {part1, part2};
