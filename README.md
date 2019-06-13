# Scotty: Quantum Computing in Scala

[![Build Status](https://travis-ci.org/entangled-xyz/scotty.svg?branch=master)](https://travis-ci.org/entangled-xyz/scotty) [![Maven Central](https://maven-badges.herokuapp.com/maven-central/xyz.entangled/scotty_2.12/badge.svg)](https://maven-badges.herokuapp.com/maven-central/xyz.entangled/scotty_2.12) [![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://github.com/gitbucket/gitbucket/blob/master/LICENSE)

*"Whatever you say, sir. Thy will be done."*—Montgomery Scott

Scotty is a quantum computing framework for Scala developers. It comes with a quantum computer simulator that can be used for writing hybrid programs.

Most quantum frameworks and simulators are written either in quantum-specific languages or Python. Scotty is one of the first attempts to build a cross-platform quantum framework on top of the JVM.

It was created with three principles in mind:

- **Write once, run anywhere**: experiment with quantum code and run it with Scotty. Compile it (coming soon) to Quil or OpenQASM and run it on other simulators or real quantum computers.
- **Expandability**: provide a high-level set of abstractions that can be expanded to different architectures.
- **No PhD required**: it should be easy to get started and everything should work out-of-the-box.

Here is an example of a quantum teleportation algorithm written in Scotty to give you an idea of what a typical piece of code looks like:

```scala
implicit val random = new Random()

def bellPair(q1: Int, q2: Int) = Circuit(H(q1), CNOT(q1, q2))

val msg = Qubit(Complex(0.8), Complex(0.6))

val circuit = bellPair(1, 2)
  .combine(Circuit(CNOT(0, 1), H(0)))
  .combine(CNOT(1, 2), Controlled(0, Z(2)))
  .withRegister(msg, Qubit.zero("here"), Qubit.zero("there"))

QuantumSimulator().run(circuit) match {
  case s: Superposition => assert(QubitProbabilityReader(s).read("there")
    .forall(q => q.probability == Math.pow(msg.b.abs, 2)))
}
```

Here we setup a quantum circuit with a custom register of qubits, run it on the quantum simulator, and then peek at the *superposition* probability of the "there" qubit.

## Getting Started

To learn more about installing and using Scotty please [check out the official docs](https://www.entangled.xyz/scotty/).

## Contributing

Contributions are super welcome! Take a look at the current issues and if you'd like to help please submit a pull request with some tests covering your implementation.

## License

Scotty is available under the Apache 2 License.