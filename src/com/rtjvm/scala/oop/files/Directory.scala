package com.rtjvm.scala.oop.files

import com.rtjvm.scala.oop.filesystem.FilesystemException

class Directory(override val parentPath: String,
                override val name: String,
                val contents: List[DirEntry])
  extends DirEntry(parentPath, name) {

  def asDirectory: Directory = this

  def isDirectory: Boolean = true

  def asFile: File = throw new FilesystemException("A directory cannot be converted to a file")

  def isFile: Boolean = false

  def getType: String = "Directory"

  def addEntry(newEntry: DirEntry): Directory = new Directory(parentPath, name, contents :+ newEntry)

  def removeEntry(entryName: String): Option[Directory] =
    if (!hasEntry(entryName)) Some(this)
    else Some(new Directory(parentPath, name, contents.filterNot(_.name == entryName)))

  def hasEntry(name: String): Boolean = contents.exists(_.name == name)

  def findEntry(name: String): Option[DirEntry] = contents.find(_.name == name)

  def getAllFoldersInPath: List[String] =
    path.substring(1).split(Directory.SEPARATOR).toList.filterNot(_.isEmpty)

  def findDescendant(path: List[String]): Directory = {
    if (path.isEmpty) this
    else {
      findEntry(path.head) match {
        case Some(entry) => entry.asDirectory.findDescendant(path.tail)
        case None => this
      }
    }
  }

  def findDescendant(relativePath: String): Directory =
    if (relativePath.isEmpty) this
    else findDescendant(relativePath.split(Directory.SEPARATOR).toList)

  def replaceEntry(entryName: String, newEntry: DirEntry): Directory =
    new Directory(parentPath, name, contents.filter(_.name != entryName) :+ newEntry)

  def isRoot: Boolean = parentPath.isEmpty

}

object Directory {
  val SEPARATOR: String = "/"
  val ROOT_PATH: String = "/"

  def ROOT: Directory = Directory.empty("", "")

  def empty(parentPath: String, name: String): Directory = {
    new Directory(parentPath, name, List())
  }
}