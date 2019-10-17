/*
 * (C) 2015-2019 fine.lines.software. All rights reserved.
 */

package org.flsx.elevation

import java.io.ByteArrayOutputStream

import cats.implicits._
import io.bullet.borer.{Codec, Decoder, Encoder}
import io.bullet.borer.derivation.ArrayBasedCodecs
import io.jenetics.jpx.{Latitude, Longitude}

import scala.annotation.tailrec

object ElevationMap {

  final case class WorldXY(x: Double, y: Double) {
    if (x < 0.0 || 360.0 <= x) throw new IllegalArgumentException
    if (y < 0.0 || 160.0 <= y) throw new IllegalArgumentException

    def tileId: String = WorldXY.tileId(x.round.toInt / 10, y.round.toInt / 10)
  }

  object WorldXY {
    implicit val borerCodec: Codec[WorldXY] = ArrayBasedCodecs.deriveCodec

    def fromLatLon(lat: Latitude, lon: Longitude): Option[WorldXY] =
      Either
        .catchOnly[IllegalArgumentException] {
          WorldXY(x = lon.doubleValue + 180.0, y = 80.0 - lat.doubleValue)
        }
        .toOption

    def tileId(tileX: Int, tileY: Int): String = {
      val sb = new java.lang.StringBuilder
      if (tileY < 8) sb.append('N').append(7 - tileY)
      else sb.append('S').append(tileY - 8)
      if (tileX < 18) sb.append('W').append(f"${17 - tileX}%02d")
      else sb.append('E').append(f"${tileX - 18}%02d")
      sb.toString
    }
  }

  final case class Tile(id: String, topLeftCorner: WorldXY, width: Int, height: Int, data: Array[Short]) {
    if (width * height != data.length) throw new IllegalArgumentException
  }

  implicit val borerCodec: Codec[Tile] = {
    implicit val dataEncoder: Encoder[Array[Short]] =
      Encoder { (w, array) =>
        val output = new ByteArrayOutputStream(array.length * 5 / 4)
        @tailrec def rec(sourceIx: Int, lastValue: Int): Array[Byte] =
          if (sourceIx < array.length) {
            val v     = array(sourceIx).toInt
            val delta = v - lastValue
            if (math.abs(delta) <= Byte.MaxValue.toInt) {
              output.write(delta)
            } else {
              output.write(Byte.MinValue)
              output.write(v >> 8)
              output.write(v)
            }
            rec(sourceIx + 1, v)
          } else output.toByteArray
        w.writeArrayHeader(2).writeInt(array.length).writeBytes(rec(0, 0))
      }

    implicit val dataDecoder: Decoder[Array[Short]] =
      Decoder { r =>
        val target = new Array[Short](r.readArrayHeader(2).readInt())
        val source = r.readByteArray()
        @tailrec def rec(sourceIx: Int, targetIx: Int, acc: Int): Array[Short] =
          if (sourceIx < source.length) {
            val byte         = source(sourceIx).toInt
            var nextSourceIx = sourceIx + 1
            val v =
              if (byte == Byte.MinValue) {
                nextSourceIx = sourceIx + 3
                (source(sourceIx + 1) << 8) | (source(sourceIx + 2) & 0xFF)
              } else acc + byte
            target(targetIx) = v.toShort
            rec(nextSourceIx, targetIx + 1, v)
          } else if (targetIx != target.length) r.validationFailure("Delta Encoding Error")
          else target
        rec(0, 0, 0)
      }

    ArrayBasedCodecs.deriveCodec[Tile]
  }
}
