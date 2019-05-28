package scotty.simulator

import scotty.quantum.QuantumContext
import scotty.quantum.QuantumContext._
import scotty.simulator.math.RawGate
import scotty.simulator.math.Implicits._

import scala.collection.mutable
import scala.util.Random

case class QuantumSimulator(implicit random: Random) extends QuantumContext {
  type GateGen = Seq[Double] => Matrix
  private val gateGenerators = mutable.Map[String, GateGen]()

  GateLoader.loadDefaultGens(this)

  def addGateGen(name: String, f: GateGen) = gateGenerators(name) = f

  def qubitsToSuperposition(qs: Seq[Qubit]): Superposition = {
    qs.foldLeft(SimSuperposition())((superposition, q) => superposition.par(SimSuperposition(q.state)))
  }

  def run(circuit: Circuit): Superposition = {
    val initState = qubitsToSuperposition(circuit.qs)

    circuit.ops.map(prepareOp).foldLeft(initState)((state, op) => state.applyOp(op)(this))
  }

  def prepareOp(op: Op): Op = op match {
    case g: Gate => prepareGate(g)
    case _ => op // TODO: add support for measurements
  }

  def prepareGate(gate: Gate): Op = {
    def pad(g: Gate, qs: Seq[Qubit]): Seq[Gate] = {
      def padTop(g: Gate) = (0 until qs.sortWith(_.index < _.index)(0).index).map(_ => g)
      def padBottom(g: Gate) = (qs.sortWith(_.index > _.index)(0).index until gate.qubitCount - 1).map(_ => g)

      val identity = Array(
        Array(Complex(1), Complex(0)),
        Array(Complex(0), Complex(1))
      )

      (padTop(RawGate(identity)) :+ g) ++ padBottom(RawGate(identity))
    }

    pad(gate, gate.qs).reduce((a, b) => a.par(b)(this))
  }

  def par(g1: Gate, g2: Gate): Gate = RawGate((RawGate(g1)(this) ⊗ RawGate(g2)(this).fieldMatrix).getData)

  def isUnitary(g: Gate): Boolean = RawGate(g)(this).isUnitaryMatrix

  def control(q: Qubit, gate: Gate): Matrix = gate.matrix()(this) //gate match {
//    case targetGate: Target =>
//      def toBasisState(n: Int): (Complex, Complex) = if (n == 1) (Complex(0), Complex(1)) else (Complex(1), Complex(0))
//
//      def toBinary(a: Complex, b: Complex): Int = if (a == Complex(0) && b == Complex(1)) 1 else 0
//
//      val indices = qs.map(_.index)
//      val gap = Math.abs(indices.reduceLeft(_ - _)) - 1
//      val isFlipped = indices.sliding(2).forall { case Seq(x, y) => x > y }
//
//      (0 until Math.pow(2, 2 + gap).toInt).map(index => {
//        val binary = MathUtils.toBinaryPadded(index, 2 + gap).toArray
//        val controlIndex = if (isFlipped) binary.length - 1 else 0
//        val targetIndex = if (isFlipped) 0 else binary.length - 1
//
//        if (binary(controlIndex) == 1) {
//          val data = RawGate(targetGate).product(toBasisState(binary(targetIndex))).getData
//
//          binary(targetIndex) = toBinary(data(0), data(1))
//        }
//
//        binary
//          .map(b => SimSuperposition(toBasisState(b)))
//          .reduce((s1, s2) => s1 par s2)
//          .rawVector
//      }).toArray
//    case controlGate: Controlled => control(controlGate.control, controlGate.target)
//  }
  override def matrix(targetGate: Target): Matrix = gateGenerators(targetGate.name).apply(targetGate.params)
}