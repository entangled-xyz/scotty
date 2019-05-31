# Scotty: Quantum Computing for Scala/JVM

*"Whatever you say, sir. Thy will be done."*<br>—Montgomery Scott

Scotty is a quantum computing framework designed for Scala developers. It comes with a quantum computer simulator that can be used for writing hybrid (classical and quantum) code in the same Scala application.

Here's an example of a quantum teleportation circuit to give you an idea of how easy it is to write hybrid programs with Scotty:

```scala
val sim = QuantumSimulator()

def bellState(q1: Int, q2: Int) = Circuit(
  H(q1), CNOT(q1, q2)
)

val circuit = bellState(1, 2)
  .combine(CNOT(0, 1), H(0))
  .combine(Qubit.zero, Qubit.zero, Qubit.zero)(CNOT(1, 2), Controlled(0, Z(2)))

val superposition = sim.run(circuit)

val collapsed = superposition.measure()
```
