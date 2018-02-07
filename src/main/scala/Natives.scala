package xyz.hyperreal.funl2

import xyz.hyperreal.lia.Math
import xyz.hyperreal.bvm._


object Natives {

	def toInt( vm: VM, v: String ) = v.toInt

	def toLong( vm: VM, v: String ) = v.toLong

	def toBigInt( vm: VM, v: String ) = BigInt( v )

	def toFloat( vm: VM, v: String ) = v.toDouble

	def eval( vm: VM, expr: String ) = run( expr )

	def sqrt( vm: VM, n: Number ) = Math.sqrtFunction( n )

	def abs( vm: VM, n: Number ) = Math.absFunction( n )
}
