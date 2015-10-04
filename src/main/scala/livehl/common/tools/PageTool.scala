package common

/**
 * Created by admin on 14-6-23.
 */
object PageTool {
  def PageAndMax(pageth: Int, total: Int): (Int, Int, Int) = {
    val pageSize = Tool.getSettingMap()("pageNum").toInt;
    val maxPage: Int = if (total % pageSize == 0) total / pageSize else total / pageSize + 1
    val page: Int = {
      if (pageth < 1 || maxPage == 0) 1
      else if (pageth > maxPage && maxPage > 1) maxPage
      else if (pageth >= 1 && pageth <= maxPage) pageth
      else 1
    }
    (page, maxPage, pageSize)
  }
}
