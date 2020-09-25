package com.rtjvm.scala.oop.files

abstract class DirEntry(val parentPath: String, val name: String) {

  def path: String = {
    val maybeSeparator: String = if (parentPath == Directory.ROOT_PATH) "" else Directory.SEPARATOR
    parentPath + maybeSeparator + name
  }

  def asDirectory: Directory

  def isDirectory: Boolean

  def asFile: File

  def isFile: Boolean

  def getType: String

}
