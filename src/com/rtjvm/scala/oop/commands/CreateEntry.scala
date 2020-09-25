package com.rtjvm.scala.oop.commands

import com.rtjvm.scala.oop.files.{DirEntry, Directory}
import com.rtjvm.scala.oop.filesystem.{FilesystemException, State}

import scala.annotation.tailrec

abstract class CreateEntry(name: String) extends Command {

  override def apply(state: State): State = {
    val wd: Directory = state.workDir

    if (wd.hasEntry(name)) {
      state.setMessage("Entry " + name + " already exists!")
    } else if (name.contains(Directory.SEPARATOR)) {
      state.setMessage(name + " must not contain separators!")
    } else if (checkIllegal(name)) {
      state.setMessage(name + ": illegal entry name!")
    } else {
      doCreateEntry(state, name)
    }
  }

  def checkIllegal(name: String): Boolean = {
    name.contains(".")
  }

  def doCreateEntry(state: State, name: String): State = {
    def updateStructure(currentDirectory: Directory, path: List[String], newEntry: DirEntry): Directory = {
      if (path.isEmpty) currentDirectory.addEntry(newEntry)
      else {
        val oldEntry: Directory = currentDirectory.findEntry(path.head) match {
          case Some(entry) => entry.asDirectory
          case None => throw new FilesystemException("Old entry: " + path.head + " not found")
        }
        currentDirectory.replaceEntry(oldEntry.name, updateStructure(oldEntry, path.tail, newEntry))
      }
    }

    val wd: Directory = state.workDir

    // 1. Get all directories in the full path
    val allDirsInPath: List[String] = wd.getAllFoldersInPath

    // 2. Create a new directory entry in the workdir
    val newEntry: DirEntry = createSpecificEntry(state)

    // 3. Update the whole directory structure starting from the root
    // (directory structure is immutable)
    val newRoot = updateStructure(state.root, allDirsInPath, newEntry)

    // 4. Find new workdir INSTANCE given workdir's full path, in the NEW directory structure
    val newWd = newRoot.findDescendant(allDirsInPath)

    State(newRoot, newWd)
  }

  def createSpecificEntry(state: State): DirEntry

}