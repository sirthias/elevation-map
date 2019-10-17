/*
 * (C) 2015-2019 fine.lines.software. All rights reserved.
 */

package org.flsx

import java.nio.file.{Files, Path}

import cats.{Eval, Foldable}
import cats.data.NonEmptyList
import cats.implicits._

package object elevation {

  type Checked[+T] = Either[String, T]

  def Checked[T](expr: => T): Checked[T] = Either.catchNonFatal(expr).leftMap(_.toString)

  def listDirectory(directory: Path): Checked[NonEmptyList[Path]] =
    for {
      optPaths <- Checked(NonEmptyList.fromFoldable(Files.list(directory).toArray(Array.ofDim[Path])))
      paths    <- optPaths.toRight(s"The directory `$directory` is empty")
    } yield paths

  implicit object ArrayFoldable extends Foldable[Array] {
    def foldLeft[A, B](fa: Array[A], b: B)(f: (B, A) => B)                     = fa.foldLeft(b)(f)
    def foldRight[A, B](fa: Array[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]) = fa.foldRight(lb)(f)
  }
}
