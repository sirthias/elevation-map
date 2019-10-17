/*
 * (C) 2015-2019 fine.lines.software. All rights reserved.
 */

package org.flsx.elevation

import java.awt.image.DataBufferShort
import java.io._
import java.nio.file.{Files, Path, Paths}
import java.util.concurrent.TimeUnit

import cats.implicits._
import io.bullet.borer.Cbor
import io.jenetics.jpx.{Latitude, Longitude}
import javax.imageio.ImageIO
import org.apache.commons.imaging.FormatCompliance
import org.apache.commons.imaging.common.bytesource.ByteSourceArray
import org.apache.commons.imaging.formats.tiff.{TiffDirectory, TiffReader}
import org.apache.commons.imaging.formats.tiff.constants.GeoTiffTagConstants
import org.flsx.elevation.ElevationMap.{Tile, WorldXY}
import org.tukaani.xz.{LZMA2Options, XZOutputStream}

import scala.annotation.tailrec
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global

object ConvertElevationData {

  //////////////////////////////// CONFIG ////////////////////////////////////////

  final private val sourceDir = Paths.get("/Users/mathias/Downloads/_no-backup/ASTGTMV003")
  final private val targetDir = Paths.get("/Users/mathias/Downloads/2")

  final private val tileTargetWidth  = 360
  final private val tileTargetHeight = 360

  final private val dirLoadTimeout = scala.concurrent.duration.Duration(60, TimeUnit.SECONDS)

  ///////////////////////////////////////////////////////////////////////////////

  final case class TiffSource(name: String, tiePointLat: Latitude, tiePointLon: Longitude, data: Array[Short]) {
    require(data.length == 3601 * 3601)

    override def toString = s"TiffSource($name, $tiePointLat, $tiePointLon,  <${data.length} pixels>)"
  }

  final private val Params           = new java.util.TreeMap[String, Any]
  final private val FormatCompliance = new FormatCompliance("")

  final private val smoothingTileWidth  = 3600 / tileTargetWidth
  final private val smoothingTileHeight = 3600 / tileTargetHeight

  final def main(args: Array[String]): Unit = {
    val result =
      for {
        dirs <- listDirectory(sourceDir)
        dirCount = dirs.size
        nestedTiles <- dirs.zipWithIndex.traverse {
          case (dir, ix) =>
            println(s"Processing $dir ... (${ix * 100.0 / dirCount}%4.1f%%)")
            for {
              tiffs   <- loadDirectory(dir)
              optTile <- resampleAll(tiffs)
            } yield optTile.traverse(tile => saveAsElevMap(tile, targetDir.resolve(s"${tile.id}.tile.xz")))
        }
        tiles <- nestedTiles.sequence
      } yield tiles.toList.flatten

    println(result.fold(x => s"ERROR: $x", x => s"SUCCESS: ${x.size} files"))
  }

  def loadDirectory(directory: Path): Checked[List[TiffSource]] =
    for {
      paths  <- listDirectory(directory)
      images <- Await.result(paths.zipWithIndex.traverse(loadImage(paths.size)), dirLoadTimeout).sequence
    } yield images.toList.flatten

  def loadImage(pathCount: Int)(filePathAndIndex: (Path, Int)): Future[Checked[Option[TiffSource]]] = Future {
    import GeoTiffTagConstants._

    def extractTiePoint(dir: TiffDirectory): Checked[(Latitude, Longitude)] =
      for {
        field   <- Option(dir.findField(EXIF_TAG_MODEL_TIEPOINT_TAG)).toRight("ModelTiepointTag not found")
        doubles <- Checked(field.getDoubleArrayValue)
        t <- doubles match {
          case Array(0.0, 0.0, 0.0, lon, lat, 0.0) =>
            (Latitude.ofDegrees((lat + 0.5).floor), Longitude.ofDegrees((lon + 0.5).floor)).asRight
          case x => s"unexpected ModelTiepointTag value array [${x.mkString(", ")}]".asLeft
        }
      } yield (t._1, t._2)

    def loadImageData(bytes: Array[Byte]): Checked[Array[Short]] =
      for {
        image <- Checked(ImageIO.read(new ByteArrayInputStream(bytes))).ensure("Could not load image")(_ ne null)
        raster = image.getRaster
        _ <- raster.getMinX.asRight.ensureOr(x => s"unexpected minX $x")(_ == 0)
        _ <- raster.getMinY.asRight.ensureOr(x => s"unexpected minY $x")(_ == 0)
        _ <- raster.getWidth.asRight.ensureOr(x => s"unexpected width $x")(_ == 3601)
        _ <- raster.getHeight.asRight.ensureOr(x => s"unexpected height $x")(_ == 3601)
        buf <- Checked(raster.getDataBuffer.asInstanceOf[DataBufferShort].getData)
          .ensureOr(x => s"unexpected buffer length $x")(_.length == 3601 * 3601)
      } yield buf

    val (path, ix) = filePathAndIndex
    println(f"Loading ${ix * 100 / pathCount}%02d%% ...");
    {
      for {
        bytes <- Checked(Files.readAllBytes(path))
        reader = new TiffReader(true)
        contents   <- Checked(reader.readFirstDirectory(new ByteSourceArray(bytes), Params, true, FormatCompliance))
        rootDir    <- Checked(contents.directories.get(0))
        tiePoint   <- extractTiePoint(rootDir)
        optImgData <- WorldXY.fromLatLon(tiePoint._1, tiePoint._2).traverse(_ => loadImageData(bytes))
      } yield optImgData.map(TiffSource(path.toFile.getName, tiePoint._1, tiePoint._2, _))
    }.leftMap(x => s"Error loading image file [$path]: $x")
  }

  def resampleAll(tiffs: List[TiffSource]): Checked[Option[Tile]] =
    tiffs.headOption.flatMap(x => WorldXY.fromLatLon(x.tiePointLat, x.tiePointLon)).traverse { someTileXY =>
      val tileId = someTileXY.tileId
      val data   = new Array[Short](tileTargetWidth * 10 * tileTargetHeight * 10)

      val tiffCount = tiffs.size
      tiffs.zipWithIndex
        .traverse {
          case (tiff, index) =>
            for {
              worldXY <- WorldXY.fromLatLon(tiff.tiePointLat, tiff.tiePointLon).toRight("alien subtile")
              _       <- worldXY.asRight.ensure(s"Raster mismatch: ${worldXY.tileId} != $tileId")(_.tileId == tileId)
              _     = println(f"Resampling ${index * 100 / tiffCount}%02d%% ...")
              tileX = worldXY.x.round.toInt % 10
              tileY = worldXY.y.round.toInt % 10
              _ <- resample(tiff.data, data, tileX, tileY)
            } yield ()
        }
        .map { _ =>
          Tile(
            id = tileId,
            topLeftCorner = WorldXY((someTileXY.x / 10.0).floor * 10.0, (someTileXY.y / 10.0).floor * 10.0),
            width = 10 * tileTargetWidth,
            height = 10 * tileTargetHeight,
            data = data
          )
        }
    }

  def resample(sourceData: Array[Short], targetData: Array[Short], tileX: Int, tileY: Int): Checked[Array[Short]] = {

    @tailrec def average(x: Int, y: Int, sourceOffset: Int, acc: Long, count: Int): Long =
      if (y < smoothingTileHeight) {
        if (x < smoothingTileWidth) {
          val value = sourceData(sourceOffset)
          if (value >= -500) average(x + 1, y, sourceOffset + 1, acc + value.toLong, count + 1)
          else average(x + 1, y, sourceOffset + 1, acc, count) // we simply ignore fill values
        } else average(0, y + 1, sourceOffset + 3601 - smoothingTileWidth, acc, count)
      } else if (count > 0) acc / count.toLong
      else 0

    @tailrec def rec(x: Int, y: Int, sourceOffset: Int, targetOffset: Int): Array[Short] =
      if (y < tileTargetHeight) {
        if (x < tileTargetWidth) {
          targetData(targetOffset) = average(0, 0, sourceOffset, 0L, 0).toShort
          rec(x + 1, y, sourceOffset + smoothingTileWidth, targetOffset + 1)
        } else rec(0, y + 1, sourceOffset - 3600 + (smoothingTileHeight * 3601), targetOffset + 9 * tileTargetWidth)
      } else targetData

    for {
      _ <- sourceData.asRight.ensure(s"bad source length: ${sourceData.length}")(_.length == 3601 * 3601)
      _ <- targetData.asRight.ensure(s"bad target length: ${targetData.length}")(
        _.length == tileTargetWidth * tileTargetHeight * 10 * 10)
    } yield rec(0, 0, 0, tileY * tileTargetWidth * 10 * tileTargetHeight + tileX * tileTargetWidth)
  }

  def saveAsElevMap(tile: ElevationMap.Tile, target: Path): Checked[Path] = {
    println(s"Saving elevation map to [$target]...")
    val outfile = new FileOutputStream(target.toFile)
    val options = new LZMA2Options
    options.setPreset(9)         // maximum
    options.setDictSize(1 << 22) // but with only 4 MB dictionary size
    val outxz = new XZOutputStream(outfile, options)
    val result = Checked {
      try Cbor.encode(tile).to[OutputStream](outxz).resultEither
      finally outxz.close()
    }
    result.flatMap(_.bimap(_.toString, _ => target))
  }
}
