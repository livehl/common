package livehl.common.db

/**
 * Created by yf on 15/9/15.
 */
trait DataStorage {
    def insert(fields: String*)
    def update(where: String, fields: String*)
    def insertUpdate(updateFields:List[String], fields: String*)
    def delete(where: String)
}
