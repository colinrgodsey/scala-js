package org.scalajs.core.tools.optimizer

import org.scalajs.core.ir
import ir.{Definitions, Infos}
import org.scalajs.core.ir.Types.ClassType

final class LinkingUnit(
    val classDefs: List[LinkedClass],
    val infos: Map[String, Infos.ClassInfo],
    val isComplete: Boolean,
    val elidableModuleAccessors: Set[ClassType]) {

  import LinkingUnit._

  private lazy val pureModules: Set[ClassType] =
    /*classDefs.iterator.filter(_.isPureModule).map(x => ClassType(x.encodedName)).toSet ++ */elidableModuleAccessors

  lazy val globalInfo: GlobalInfo = {
    classDefs.find(_.encodedName == Definitions.ClassClass).fold {
      GlobalInfo(
          isParentDataAccessed = false,
          pureModules = pureModules
      )
    } { classClassDef =>
      val methodNames = classClassDef.memberMethods.map(_.info.encodedName).toSet
      GlobalInfo(
          isParentDataAccessed = methodNames.contains("getSuperclass__jl_Class"),
          pureModules = pureModules
      )
    }
  }

  def updated(classDefs: List[LinkedClass], isComplete: Boolean,
      elidableModuleAccessors: Set[ClassType] = this.elidableModuleAccessors): LinkingUnit = {
    val newInfos = infos ++ classDefs.map(cd => cd.encodedName -> cd.toInfo)
    new LinkingUnit(classDefs, newInfos, isComplete,
      this.elidableModuleAccessors ++ elidableModuleAccessors)
  }
}

object LinkingUnit {

  final class GlobalInfo private (
      /** Whether the parent data of class data is accessed.
       *  This is true if the java.lang.Class.getSuperclass() method exists,
       *  since it is the only one that can do it.
       */
      val isParentDataAccessed: Boolean,

      /** Set of module ClassTypes that do not require load-time initialization */
      val pureModules: Set[ClassType]
  )

  object GlobalInfo {
    private[LinkingUnit] def apply(
        isParentDataAccessed: Boolean,
        pureModules: Set[ClassType]
    ): GlobalInfo = {
      new GlobalInfo(isParentDataAccessed, pureModules)
    }

    val SafeApproximation: GlobalInfo =
      apply(true, Set.empty)
  }

}
