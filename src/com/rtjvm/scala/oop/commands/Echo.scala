package com.rtjvm.scala.oop.commands

import com.rtjvm.scala.oop.files.{DirEntry, Directory, File}
import com.rtjvm.scala.oop.filesystem.State

class Echo(contents: List[String]) extends Command {

  override def apply(state: State): State = {
    /* If no content, return same state
       if one in content, print to console
       else
          {
             operator = next to last arg
             if > echo to file (may create a new file if it doesn't exist)
             if >> append to a file
             else print everything to console
    */
    contents.reverse match {
      case Nil => state
      case h :: Nil => state.setMessage(h)
      case file :: op :: _ if op == ">" => doEcho(state, contents.dropRight(2), file, append = false)
      case file :: op :: _ if op == ">>" => doEcho(state, contents.dropRight(2), file, append = true)
      case _ => state.setMessage(contents.mkString(" "))
    }
  }

  def doEcho(state: State, contents: List[String], filename: String, append: Boolean): State = {
    if (filename.contains(Directory.SEPARATOR)) state.setMessage("Echo: filename must not contain separators")
    else {
      val newRoot: Directory =
        getRootAfterEcho(state.root, state.workDir.getAllFoldersInPath :+ filename, contents, append)
      if (newRoot == state.root) state.setMessage(filename + ": no such file")
      else State(newRoot, newRoot.findDescendant(state.workDir.getAllFoldersInPath))
    }
  }

  def getRootAfterEcho(currentDir: Directory,
                       path: List[String],
                       contents: List[String],
                       append: Boolean): Directory = {
    path match {
      case Nil => currentDir
      case h :: Nil =>
        val dirEntry: Option[DirEntry] = currentDir.findEntry(h)
        dirEntry match {
          case None => currentDir.addEntry(new File(currentDir.path, h, contents.mkString(" ")))
          case Some(d) if d.isDirectory => currentDir
          case Some(d) if !append => currentDir.replaceEntry(h, d.asFile.setContents(contents))
          case Some(d) if append => currentDir.replaceEntry(h, d.asFile.appendContents(contents))
          case _ =>
            val nextDir: Option[DirEntry] = currentDir.findEntry(h)
            val newNextDir: Option[Directory] = nextDir.map(nd =>
              getRootAfterEcho(nd.asDirectory, path.tail, contents, append))

            nextDir.flatMap { nd =>
              newNextDir.map { nnd =>
                if (nd.asDirectory == nnd) currentDir
                else currentDir.replaceEntry(h, nnd)
              }
            }.get
        }
    }

    /*
    If path is empty, then return currentDir
    else if path.tail is empty
      find the file to create/add contents to
      if file not found, create file
      else if the entry is actually a Dir, then fail
      else
        replace or append content to file
        replace the entry with the filename with the new file
    else
      find the next directory to navigate
      call gRAE recursively on that

     if recursive call failed, fail
     else replace entry with the NEW directory after the recursive call

     */
  }

}
