package xyz.hyperreal.funl2

import xyz.hyperreal.lia.Math
import xyz.hyperreal.bvm._

object Natives {

  def toInt(vm: VM, v: String): Int = v.toInt

  def toLong(vm: VM, v: String): Long = v.toLong

  def toBigInt(vm: VM, v: String) = BigInt(v)

  def toFloat(vm: VM, v: String): Double = v.toDouble

  def eval(vm: VM, expr: String): Any = xyz.hyperreal.funl2.run(expr)

  def sqrt(vm: VM, n: Number): Number = Math.sqrtFunction(n)

  def abs(vm: VM, n: Number): Number = Math.absFunction(n)
}
