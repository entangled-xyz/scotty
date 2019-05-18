package scotty.simulator

import scotty.quantum.QuantumMachine
import scotty.quantum.QuantumMachine._

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Random, Success, Try}
import org.apache.commons.math3.complex.{Complex => ApacheComplex}
import scotty.simulator.gate.I

case class QuantumSim(random: Random) extends QuantumMachine {
  private val storage = ListBuffer[Qubit]()

  private var mutableState = SimState()

  def allocate(qubitState: (Complex, Complex), n: Int): Seq[Qubit] = {
    (0 until n).foldLeft(storage)((qs, _) => {
      mutableState = mutableState ⊗ SimState(qubitState)
      qs += SimQubit()
    }).toList
  }

  def indexedQubits: Seq[(Int, Qubit)] = storage.zipWithIndex.map(_.swap).toList

  def qubits: Seq[Qubit] = storage.toList

  def state: State = mutableState

  def find(qubit: Qubit): Option[(Int, Qubit)] = indexedQubits.find(q => q._2.id == qubit.id)

  def indexOf(qubit: Qubit): Int = storage.indexOf(qubit)

  def applyOp(qubit: Qubit)(op: Op): State = {
    val stepOp = qubits.map(q => {
      if (q == qubit) op
      else SimOp(I.data)(this)
    }).reduce(_ combine _)

    mutableState = SimOp(stepOp.data)(this) * SimState(state.data)

    state
  }

  def applyOp(qs: Qubit*)(op: Op): Try[State] = {
    case class OrderIterator(index: Int = -1, ordered: Boolean = true)

    if (qs.length < op.qubitCount) {
      Failure(QuantumException("Not enough qubits to execute operation"))
    } else if (!qs.foldLeft(OrderIterator())((previous, q) => {
      if (previous.index < indexOf(q) && previous.ordered) {
        OrderIterator(indexOf(q))
      } else {
        OrderIterator(indexOf(q), ordered = false)
      }
    }).ordered) {
      Failure(QuantumException("Qubits not ordered."))
    } else if (qs.length > op.qubitCount) {
      Failure(QuantumException("Too many qubits for provided operation"))
    } else {
      Success(state) // TODO: add proper multiplication for state
    }
  }
}

object QuantumSim {
  object Implicits {
    implicit def toApacheComplexNestedArray(ca: Array[Array[Complex]]) = ca.map(c => c.map(r => new ApacheComplex(r.r, r.i)))
    implicit def toApacheComplexArray(ca: Array[Complex]) = ca.map(c => new ApacheComplex(c.r, c.i))
    implicit def toApacheComplex(c: Complex) = new ApacheComplex(c.r, c.i)

    implicit def toComplexNestedArray(ca: Array[Array[ApacheComplex]]) = ca.map(c => c.map(r => Complex(r.getReal, r.getImaginary)))
    implicit def toComplexArray(ca: Array[ApacheComplex]) = ca.map(c => Complex(c.getReal, c.getImaginary))
    implicit def toComplex(c: ApacheComplex) = Complex(c.getReal, c.getImaginary)
  }

  def apply(): QuantumSim = this(new scala.util.Random)

  def toBinary(n: Long): Seq[Long] = {
    @tailrec
    def binary(acc: Seq[Long], n: Long): Seq[Long] = n match {
      case 0 | 1 => n +: acc
      case _ => binary((n % 2) +: acc, n / 2)
    }

    binary(Seq(), n)
  }
}