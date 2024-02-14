from enum import Enum
from queue import Queue


class Signal(Enum):
    LOW = 0
    HIGH = 1


class Module:
    q = Queue()
    signal_counts = {Signal.LOW: 0, Signal.HIGH: 0}

    def __init__(self, id: str, bcast: list[str]):
        self.id = id
        self.broadcast = bcast

    def broadcast_signal(self, signal):
        for b in self.broadcast:
            Module.q.put((self.id, b, signal))
            Module.signal_counts[signal] += 1


class FlipFlop(Module):
    def __init__(self, id: str, bcast: list[str]):
        super().__init__(id, bcast)
        self.state = False

    def receive_signal(self, _: str, signal: Signal):
        if signal == Signal.LOW:
            send_sig = Signal.LOW if self.state else Signal.HIGH
            self.broadcast_signal(send_sig)
            self.state = not self.state


class Conjunction(Module):
    def __init__(self, id: str, bcast: list[str], inputs: list[str]):
        super().__init__(id, bcast)
        self.feed_memory = {inp: Signal.LOW for inp in inputs}

    def receive_signal(self, source: str, signal: Signal):
        if source != "broadcaster":
            self.feed_memory[source] = signal

        if all(v == Signal.HIGH for v in self.feed_memory.values()):
            send_sig = Signal.LOW
        else:
            send_sig = Signal.HIGH


class Output(Module):
    def __init__(self, id: str):
        super().__init__(id, [])
        self.signals = []

    def receive_signal(self, _, signal):
        self.signals.append(signal)


def load_input(filename: str):
    modules = {}
    lines = []
    with open(filename) as f:
        for line in f.readlines():
            source, dest = map(str.strip, line.split("->"))
            if source == "broadcaster":
                type = ""
                id = source
            else:
                type = source[0]
                id = source[1:]
            broadcast = list(map(str.strip, dest.split(",")))
            lines.append((type, id, broadcast))

    conjunctions = {i: [] for t, i, b in lines if t == "&"}
    for type, id, broadcast in lines:
        for b in broadcast:
            if b in conjunctions:
                conjunctions[b].append(id)

    for type, id, broadcast in lines:
        if id == "broadcaster":
            modules[id] = broadcast
        elif type == "%":
            modules[id] = FlipFlop(id, broadcast)
        elif type == "&":
            modules[id] = Conjunction(id, broadcast, conjunctions[id])
        else:
            raise ValueError(f"Oh dear... {type}")

    # Find Output node(s)
    for _, _, broadcast in lines:
        for b in broadcast:
            if b not in modules:
                modules[b] = Output(b)

    return modules


def push_button(modules):
    Module.signal_counts[Signal.LOW] += 1
    for b in modules["broadcaster"]:
        Module.q.put(("broadcaster", b, Signal.LOW))
        Module.signal_counts[Signal.LOW] += 1

    while Module.q.qsize():
        source, dest, signal = Module.q.get()
        modules[dest].receive_signal(source, signal)


if __name__ == "__main__":
    filename = "input.txt"
    modules = load_input(filename)

    for _ in range(1000):
        push_button(modules)
    print(Module.signal_counts[Signal.LOW] * Module.signal_counts[Signal.HIGH])
