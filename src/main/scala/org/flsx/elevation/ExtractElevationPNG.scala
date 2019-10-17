/*
 * (C) 2015-2019 fine.lines.software. All rights reserved.
 */

package org.flsx.elevation

import java.awt.image.{BufferedImage, DataBufferByte, IndexColorModel}
import java.io.{ByteArrayOutputStream, FileInputStream}
import java.nio.file.{Files, Path, Paths}

import cats.implicits._
import io.bullet.borer.Cbor
import javax.imageio.ImageIO
import org.flsx.elevation.ElevationMap.WorldXY
import org.tukaani.xz.XZInputStream

import scala.annotation.tailrec

object ExtractElevationPNG {

  //////////////////////////////// CONFIG ////////////////////////////////////////

  final private val sourceDir  = Paths.get("/Users/mathias/Downloads/1")
  final private val targetFile = Paths.get("/Users/mathias/Downloads/world.png")

  // the extraction rectangle in coarse world XY coordinates
  final private val targetX = 0  // W0 is 17, E0 is 18
  final private val targetY = 0  // N0 is 7, S0 is 8
  final private val targetW = 36 // the width of the rectangle in 10s of degrees
  final private val targetH = 16 // the height of the rectangle in 10s of degrees

  // WORLD (i.e. max data set)
  // final private val targetX = 0
  // final private val targetY = 0
  // final private val targetW = 36
  // final private val targetH = 16

  // EUROPE
  // final private val targetX = 16
  // final private val targetY = 1
  // final private val targetW = 6
  // final private val targetH = 4

  // GERMANY
  // final private val targetX = 18
  // final private val targetY = 2
  // final private val targetW = 2
  // final private val targetH = 2

  final private val downsamplingFactor = 20 // the edge length of the square to average into a single pixel

  final private val tileSourceWidth  = 3600 // the pixel width of one of our tiles (not the raw ASTGTMV003 tiles!)
  final private val tileSourceHeight = 3600 // the pixel width of one of our tiles (not the raw ASTGTMV003 tiles!)

  ////////////////////////////////////////////////////////////////////////////////

  final private val tileTargetWidth  = tileSourceWidth / downsamplingFactor
  final private val tileTargetHeight = tileSourceHeight / downsamplingFactor

  final private val targetWidthPixels  = targetW * tileTargetWidth
  final private val targetHeightPixels = targetH * tileTargetHeight

  final private val minElev = -500
  final private val maxElev = 9000

  final def main(args: Array[String]): Unit = {
    val tileIds = allTileIds().toSet
    val result =
      for {
        allTilePaths <- listDirectory(sourceDir)
        tilePaths = allTilePaths
          .map(x => x -> tileId(x))
          .filter(t => tileIds.contains(t._2))
          .sortBy(_._2)
          .map(_._1)
        imgData         <- resampleAll(tilePaths)
        scaledImageData <- scale(imgData)
        output          <- saveAsPng(scaledImageData, targetFile)
      } yield output

    println(result.fold(x => s"ERROR: $x", x => s"SUCCESS: $x"))
  }

  def allTileIds() =
    for {
      x <- targetX until (targetX + targetW)
      y <- targetY until (targetY + targetH)
    } yield WorldXY.tileId(x, y)

  def tileId(tilePath: Path): String = tilePath.toFile.getName.takeWhile(_ != '.')

  def resampleAll(tilePaths: List[Path]): Checked[Array[Short]] = {
    val imgData   = new Array[Short](targetWidthPixels * targetHeightPixels)
    val fileCount = tilePaths.size
    tilePaths.zipWithIndex
      .traverse {
        case (tilePath, ix) =>
          println(f"Loading and resampling tile ${tileId(tilePath)}... (${ix * 100.0 / fileCount}%4.1f%%)")
          for {
            tile <- loadTile(tilePath)
            _    <- resample(tile, imgData)
          } yield imgData
      }
      .map(_ => imgData)
  }

  def resample(tile: ElevationMap.Tile, imgData: Array[Short]): Checked[Array[Short]] = {

    @tailrec def average(x: Int, y: Int, sourceOff: Int, acc: Long): Long =
      if (y < downsamplingFactor) {
        if (x < downsamplingFactor) {
          average(x + 1, y, sourceOff + 1, acc + tile.data(sourceOff).toLong)
        } else average(0, y + 1, sourceOff + tileSourceWidth - downsamplingFactor, acc)
      } else acc / (downsamplingFactor * downsamplingFactor)

    @tailrec def rec(x: Int, y: Int, sourceOff: Int, targetOff: Int): Array[Short] =
      if (y < tileTargetHeight) {
        if (x < tileTargetWidth) {
          imgData(targetOff) = average(0, 0, sourceOff, 0L).toShort
          rec(x + 1, y, sourceOff + downsamplingFactor, targetOff + 1)
        } else
          rec(
            0,
            y + 1,
            sourceOff + (downsamplingFactor - 1) * tileSourceWidth,
            targetOff + targetWidthPixels - tileTargetWidth)
      } else imgData

    for {
      _ <- tile.asRight.ensure(s"bad tile: $tile")(x => x.width == tileSourceWidth && x.height == tileSourceHeight)
      tileX = tile.topLeftCorner.x.toInt / 10 - targetX
      tileY = tile.topLeftCorner.y.toInt / 10 - targetY
    } yield rec(0, 0, 0, tileY * tileTargetHeight * targetWidthPixels + tileX * tileTargetWidth)
  }

  def loadTile(target: Path): Checked[ElevationMap.Tile] = {
    val fis = new FileInputStream(target.toFile)
    val xis = new XZInputStream(fis, 10 * 1024)
    val result = Checked {
      try Cbor.decode(xis).to[ElevationMap.Tile].valueEither
      finally xis.close()
    }
    result.flatMap(_.leftMap(_.toString))
  }

  def scale(imgData: Array[Short]): Checked[Array[Byte]] = Checked {
    val target = new Array[Byte](imgData.length)
    for (ix <- target.indices) {
      val x = math.max(math.min(imgData(ix).toInt, maxElev), minElev)
      target(ix) = ((x - minElev) * 255 / (maxElev - minElev)).toByte
    }
    // show full gradient
    // for (x <- 0 to 2550) {
    //   for (y <- 1000 to 1200) {
    //     target(y * 3600 + 1000 + x) = (x / 10).toByte
    //   }
    // }
    target
  }

  def saveAsPng(data: Array[Byte], target: Path): Checked[Path] = {
    println(s"Saving image data to [$target]...")
    val (image, imgData) = createBufferedImage()
    require(data.length == imgData.length)
    System.arraycopy(data, 0, imgData, 0, imgData.length)

    val os = new ByteArrayOutputStream
    for {
      _ <- Checked(ImageIO.write(image, "png", os)).ensure("NOPE")(identity)
      _ <- Checked(Files.write(target, os.toByteArray))
    } yield target
  }

  def createBufferedImage(): (BufferedImage, Array[Byte]) = {
    val colorModel = {
      val (r, g, b) = createPalette()
      new IndexColorModel(8, 256, r, g, b)
    }
    val img     = new BufferedImage(targetWidthPixels, targetHeightPixels, BufferedImage.TYPE_BYTE_INDEXED, colorModel)
    val imgData = img.getRaster.getDataBuffer.asInstanceOf[DataBufferByte].getData
    img -> imgData
  }

  def createPalette(): (Array[Byte], Array[Byte], Array[Byte]) = {
    // inspired by the topo palettes here: http://soliton.vm.bytemark.co.uk/pub/cpt-city/views/topo.html

    // SVG gradient:
    //
    // <?xml version="1.0" encoding="UTF-8"?>
    // <svg xmlns="http://www.w3.org/2000/svg" version="1.1" width="1000px" height="100px" viewBox="0 0 1000 100">
    //   <g>
    //     <defs>
    //       <linearGradient id="world-topo" gradientUnits="objectBoundingBox" spreadMethod="pad" x1="0%" x2="100%" y1="0%" y2="0%">
    //         <stop offset="0%" stop-color="rgb(174,239,213)" stop-opacity="1.0000"/>
    //         <stop offset="5%" stop-color="rgb(255,255,160)" stop-opacity="1.0000"/>
    //         <stop offset="10%" stop-color="rgb(0,140,32)" stop-opacity="1.0000"/>
    //         <stop offset="20%" stop-color="rgb(255,192,0)" stop-opacity="1.0000"/>
    //         <stop offset="30%" stop-color="rgb(220,0,0)" stop-opacity="1.0000"/>
    //         <stop offset="50%" stop-color="rgb(128,64,0)" stop-opacity="1.0000"/>
    //         <stop offset="75%" stop-color="rgb(255,255,255)" stop-opacity="1.0000"/>
    //         <stop offset="100%" stop-color="rgb(192,192,255)" stop-opacity="1.0000"/>
    //       </linearGradient>
    //     </defs>
    //     <rect fill="url(#world-topo)" x="4" y="4" width="992" height="92" stroke="black" stroke-width="1"/>
    //   </g>
    // </svg>

    case class Fixpoint(scalePoint: Double, r: Int, g: Int, b: Int)
    val fixpoints = List(
      Fixpoint(0.00, 174, 239, 213),
      Fixpoint(0.05, 255, 255, 160),
      Fixpoint(0.10, 0, 140, 32),
      Fixpoint(0.25, 255, 192, 0),
      Fixpoint(0.35, 192, 0, 0),
      Fixpoint(0.50, 128, 64, 0),
      Fixpoint(0.75, 255, 255, 255),
      Fixpoint(1.00, 192, 192, 255),
    )
    val fixpointsReversed = fixpoints.reverse

    val r = new Array[Byte](256)
    val g = new Array[Byte](256)
    val b = new Array[Byte](256)
    for (ix <- 0 to 255) {
      val x = ix.toDouble / 255
      val fp = fixpoints.find(_.scalePoint == x).getOrElse {
        val a  = fixpointsReversed.find(_.scalePoint < x).get
        val b  = fixpoints.find(_.scalePoint > x).get
        val wa = b.scalePoint - x
        val wb = x - a.scalePoint
        Fixpoint(x, interpolate(a.r, wa, b.r, wb), interpolate(a.g, wa, b.g, wb), interpolate(a.b, wa, b.b, wb))
      }
      r(ix) = fp.r.toByte
      g(ix) = fp.g.toByte
      b(ix) = fp.b.toByte
    }

    // sea level: a nice blue
    r(13) = 40.toByte
    g(13) = 116.toByte
    b(13) = 166.toByte

    (r, g, b)
  }

  def interpolate(a: Int, wa: Double, b: Int, wb: Double) = ((a * wa + b * wb) / (wa + wb)).toInt
}
