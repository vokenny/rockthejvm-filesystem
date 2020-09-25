package com.rtjvm.scala.oop.commands

import com.rtjvm.scala.oop.files.{DirEntry, Directory}
import com.rtjvm.scala.oop.filesystem.State

class Rm(name: String) extends Command {

  override def apply(state: State): State = {
    // 1. Get working directory
    val wd: Directory = state.workDir

    // 2. Get absolute path
    val absPath: String =
      if (name.startsWith(Directory.SEPARATOR)) name
      else if (wd.isRoot) wd.path + name
      else wd.path + Directory.SEPARATOR + name

    // 3. Do some checks
    if (absPath == Directory.ROOT_PATH) state.setMessage("Cannot delete ROOT directory")
    else doRm(state, absPath)
  }

  def doRm(state: State, path: String): State = {
    def rmHelper(currentDirectory: Directory, path: List[String]): Option[Directory] = {
      path match {
        case Nil => Some(currentDirectory)
        case h :: Nil => currentDirectory.removeEntry(h)
        case h :: t =>
          val nextDirectory: Option[DirEntry] = currentDirectory.findEntry(path.head)
          nextDirectory.map { nd =>
            if (!nd.isDirectory) currentDirectory
            else {
              val newNextDir: Option[Directory] = rmHelper(nd.asDirectory, t)
              newNextDir.map { nnd =>
                if (nnd == nd.asDirectory) currentDirectory
                else currentDirectory.replaceEntry(h, nnd)
              }
            }.get
          }
      }
    }

    // 4. Find the entry to remove
    // 5. Update structure like we do for mkdir
    val tokens: List[String] = path.substring(1).split(Directory.SEPARATOR).toList
    val newRoot: Option[Directory] = rmHelper(state.root, tokens)

    newRoot match {
      case Some(nr) =>
        if (nr == state.root) state.setMessage(path + ": no such file or directory")
        else State(nr, nr.findDescendant(state.workDir.path.substring(1)))
      case None => state.setMessage(path + ": no such file or directory")
    }
  }
}
