# Scotty: Quantum Computing in Scala

[![Build Status](https://travis-ci.org/entangled-xyz/scotty.svg?branch=master)](https://travis-ci.org/entangled-xyz/scotty)

*"Whatever you say, sir. Thy will be done."*—Montgomery Scott

Scotty is a quantum computing platform for Scala developers. It comes with a quantum computer simulator that can be used for writing hybrid (classical and quantum) programs.

Scotty was created with three goals in mind:

- **Write once, run anywhere**: experiment with quantum code and run it with Scotty. Compile it (coming soon) to Quil or OpenQASM and run it on other simulators or real quantum computers.
- **Avoid stateful programs**: embrace type safety, immutability, and referential transparency.
- **No PhD required**: it should be easy to get started and everything should work out-of-the-box.

Here's an example of a quantum teleportation algorithm to give you an idea of how easy it is to write hybrid code with Scotty:

```scala
implicit val random = new Random

def bellPair(q1: Int, q2: Int) = Circuit(H(q1), CNOT(q1, q2))

val msg = Qubit(Complex(0.8), Complex(0.6))

val circuit = bellPair(1, 2)
  .combine(Circuit(CNOT(0, 1), H(0)))
  .combine(CNOT(1, 2), Controlled(0, Z(2)))
  .withRegister(msg, Qubit.zero, Qubit.zero)

QuantumSimulator().run(circuit) match {
  case s: Superposition => assert(
    QubitProbabilityReader(SimSuperposition(msg)).read(0).probability ==
      QubitProbabilityReader(s).read(2).probability)
}
```

Here we setup a quantum circuit with a custom register of qubits, ran it in a quantum simulator and then peeked at the initial state of the *message* qubit and one of the the *superposition* probabilities.

## Getting Started

To learn more about how to use Scotty please [check out the docs](http://www.entangled.xyz/scotty/).