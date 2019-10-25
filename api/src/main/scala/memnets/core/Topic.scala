package memnets.core

object Topic {
  implicit def m2opt(mh: Topic): Option[Topic] = Option(mh)
  implicit def l2opt(lt: LicenseType): Option[LicenseType] = Option(lt)
  val TOPIC_SECTION_LINE = " ------------------------------TOPIC SUMMARY--------------------------------\n\n"
  val LICENSE_SECTION_LINE = "\n ---------------------------------LICENSE---------------------------------------\n\n"
  def apply(
      summary: String,
      url: Option[String] = None,
      license: Option[LicenseType] = None
  ): Topic = Topic(summary, url, license.getOrElse(if (url.isDefined) LicenseType.CC_BY_SA else LicenseType.None))
}

/**
 * a general summary of the topic can give the history,top contributors,
 * explanation of equations, uses in different branches, ...
 *
 * specific features of an instance of a MemNets model should be in ModelBuilder.desc
 */
case class Topic(
    summary: String,
    url: Option[String],
    licenseType: LicenseType
) {
  import Topic._
  def prettyPrint(description: String): String = {
    val sb = new StringBuilder
    val desc = description
    if (desc != null && !desc.isEmpty) {
      sb.append(desc)
      sb.append("\n")
    }
    sb.append(TOPIC_SECTION_LINE)
    sb.append(summary)
    url.map { url =>
      val licenseMsg = licenseType match {
        case LicenseType.None =>
          ""
        case default =>
          default
      }
//      sb.append(LICENSE_SECTION_LINE)
      sb.append("\n\n\n\n")
      sb.append(s"This topic summary was derived from ${licenseMsg} material found at: \n\n$url\n\n")
      licenseType match {
        case LicenseType.None =>
          ""
        case default =>
          sb.append(s"Any alterations are hereby licensed under the same ${default} license found at that site")
      }
    }
    sb.toString()
  }
}
