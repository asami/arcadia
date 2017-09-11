package arcadia

import org.goldenport.bag.ChunkBag

/*
 * @since   Jul. 16, 2017
 * @version Aug. 30, 2017
 * @author  ASAMI, Tomoharu
 */
sealed trait Content {
  def mimetype: MimeType
}

case class StringContent(
  mimetype: MimeType,
  charset: Option[String],
  string: String
) extends Content {
}

case class BinaryContent(
  mimetype: MimeType,
  binary: ChunkBag
) extends Content {
}
