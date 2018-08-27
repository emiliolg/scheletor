package org.mule.scheletor

import org.mulesoft.common.io.{FileSystem, Fs}

class JvmJsonTestSuite extends JsonTestSuite {
  override def fs: FileSystem = Fs
}
