package com.rtjvm.scala.oop.commands
import com.rtjvm.scala.oop.files.{DirEntry, Directory}
import com.rtjvm.scala.oop.filesystem.State

class Cat(file: String) extends Command {

  override def apply(state: State): State = {
    val wd: Directory = state.workDir
    val dirEntry: Option[DirEntry] = wd.findEntry(file)

    dirEntry match {
      case None => state.setMessage(file + ": no such file")
      case Some(d) if !d.isFile => state.setMessage(file + ": no such file")
      case Some(d) => state.setMessage(d.asFile.contents)
    }
  }

}
