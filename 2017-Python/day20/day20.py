import re
import sys

particles = {}
particleno = 0
part = int(sys.argv[1])

with open('input.txt', 'r') as fp:
    for line in fp:
        matches = re.search(
            '^p=<(-?\d+),(-?\d+),(-?\d+)>,\s+v=<(-?\d+),(-?\d+),(-?\d+)>,\s+a=<(-?\d+),(-?\d+),(-?\d+)>$',
            line.strip()
        )

        particles[particleno] = {
            'p': {
                'x': int(matches.group(1)),
                'y': int(matches.group(2)),
                'z': int(matches.group(3)),
            },
            'v': {
                'x': int(matches.group(4)),
                'y': int(matches.group(5)),
                'z': int(matches.group(6)),
            },
            'a': {
                'x': int(matches.group(7)),
                'y': int(matches.group(8)),
                'z': int(matches.group(9)),
            },
            'd': int(matches.group(1)) + int(matches.group(2)) + int(matches.group(3))
        }

        particleno += 1

while True:
    if part == 1:
        mindistparticle = 0

        for i in particles.keys():
            particles[i]['v']['x'] += particles[i]['a']['x']
            particles[i]['v']['y'] += particles[i]['a']['y']
            particles[i]['v']['z'] += particles[i]['a']['z']

            particles[i]['p']['x'] += particles[i]['v']['x']
            particles[i]['p']['y'] += particles[i]['v']['y']
            particles[i]['p']['z'] += particles[i]['v']['z']

            particles[i]['d'] = abs(particles[i]['p']['x']) + abs(particles[i]['p']['y']) + abs(particles[i]['p']['z'])

            if particles[i]['d'] < particles[mindistparticle]['d']:
                mindistparticle = i

        print("Min distance particle is " + str(mindistparticle) + " with a distance of " + str(particles[mindistparticle]['d']))

    elif part == 2:
        for i in particles.keys():
            particles[i]['v']['x'] += particles[i]['a']['x']
            particles[i]['v']['y'] += particles[i]['a']['y']
            particles[i]['v']['z'] += particles[i]['a']['z']

            particles[i]['p']['x'] += particles[i]['v']['x']
            particles[i]['p']['y'] += particles[i]['v']['y']
            particles[i]['p']['z'] += particles[i]['v']['z']

        collisions = set()

        for i in particles.keys():
            for j in particles.keys():
                if i != j:
                    if particles[i]['p']['x'] == particles[j]['p']['x'] and particles[i]['p']['y'] == particles[j]['p']['y'] and particles[i]['p']['z'] == particles[j]['p']['z']:
                        collisions.add(i)
                        collisions.add(j)

        for particle in collisions:
            del particles[particle]

        print("There are " + str(len(particles)) + " particles left after collisions")
