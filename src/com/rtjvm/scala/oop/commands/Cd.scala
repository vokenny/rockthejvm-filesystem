package com.rtjvm.scala.oop.commands

import com.rtjvm.scala.oop.files.{DirEntry, Directory}
import com.rtjvm.scala.oop.filesystem.{FilesystemException, State}

import scala.annotation.tailrec

class Cd(dir: String) extends Command {

  override def apply(state: State): State = {
    // 1. Find root
    val root: Directory = state.root
    val wd: Directory = state.workDir

    // 2. Find absolute path of dir you want to cd to
    val absolutePath: String = {
      if (dir.startsWith(Directory.SEPARATOR)) dir
      else if (wd.isRoot) wd.path + dir
      else wd.path + Directory.SEPARATOR + dir
    }

    // 3. Find the directory given the path
    val destinationDir: Option[DirEntry] = doFindEntry(root, absolutePath)

    // 4. Change the state given the new directory
    destinationDir match {
      case None => state.setMessage(dir + ": no such directory")
      case Some(d) =>
        if (d.isDirectory) State(root, d.asDirectory)
        else state.setMessage(dir + ": no such directory")
    }
  }

  def doFindEntry(root: Directory, path: String): Option[DirEntry] = {
    @tailrec
    def findEntryHelper(currentDirectory: Directory, path: List[String]): Option[DirEntry] = {
      path match {
        case Nil => Some(currentDirectory)
        case h :: Nil => if (h.isEmpty) Some(currentDirectory) else currentDirectory.findEntry(h)
        case h :: t =>
          val nextDir: Option[DirEntry] = currentDirectory.findEntry(h)
          nextDir match {
            case Some(nd) =>
              if (nd.isDirectory) findEntryHelper(nd.asDirectory, t)
              else None
            case None => None
          }
      }
    }

    @tailrec
    def collapseRelTokens(path: List[String], result: List[String]): Option[List[String]] = {
      if (path.isEmpty) Some(result)
      else {
        path.head match {
          case "." => collapseRelTokens(path.tail, result)
          case ".." =>
            if (result.isEmpty) None
            else collapseRelTokens(path.tail, result.tail)
          case t => collapseRelTokens(path.tail, result :+ t)
        }
      }
    }

    val tokens: List[String] = path.substring(1).split(Directory.SEPARATOR).toList
    val newTokens: Option[List[String]] = collapseRelTokens(tokens, Nil)

    newTokens match {
      case Some(t) => findEntryHelper(root, t)
      case None => None
    }
  }

}
