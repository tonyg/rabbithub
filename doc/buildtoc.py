import sys

wantcounters = False
lines = sys.stdin.readlines()
toc = []

class Counter:
    def __init__(self):
        self.counters = [0]

    def next(self, depth):
        global wantcounters
        if not wantcounters:
            return ''
        if depth == 1:
            return ''
        depth = depth - 1
        while len(self.counters) < depth:
            self.counters.append(0)
        while len(self.counters) > depth:
            self.counters.pop()
        self.counters[-1] = self.counters[-1] + 1
        return '.'.join(str(x) for x in self.counters)

def depthfor(h):
    depth = 0
    while h[depth] == '#':
        depth = depth + 1
    return depth

def printtoc():
    c = Counter()
    for (depth, h) in toc:
        section = c.next(depth)
        if depth > 1 and depth <= 3:
            label = h[depth:].strip()
            sys.stdout.write('  ' * (depth - 1))
            sys.stdout.write('- %s [%s](#%s)\n' % (section, label, anchorfor(h)))

def anchorfor(line):
    result = ""
    for c in line:
        if c.isalnum():
            result = result + c.lower()
    return result

for line in lines:
    if line.startswith('@TOC@'):
        wantcounters = True
    if line.startswith('#'):
        toc.append((depthfor(line), line))

c = Counter()
for line in lines:
    if line.startswith('@TOC@'):
        printtoc()
    elif line.startswith('#'):
        depth = depthfor(line)
        section = c.next(depth)
        sys.stdout.write('#' * depth)
        sys.stdout.write(' %s <a name="%s"></a>%s' % (section, anchorfor(line), line[depth:]))
    else:
        sys.stdout.write(line)
