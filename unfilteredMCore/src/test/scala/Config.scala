package com.logikujo.www
package test

/**
 *
 * UnfilteredM / LogiDev - [Fun Functional] / Logikujo.com
 *
 * 7/06/14 :: 17:52 :: eof
 *
 */

import scalate._
import org.scalatest._
import org.scalatest.matchers._
import org.scalatest.mock.MockitoSugar
import org.scalamock.scalatest._
import org.mockito.Mockito._
import com.typesafe.config._
import com.github.kxbmap.configs._
import scalaz._
import Scalaz._
import com.logikujo.www.Config

trait ConfigSpec extends UnitSpec with MockitoSugar  {
  trait ConfigProxy {
    def resolvM[Tag, A, T](implicit ev: Config[Tag] => ErrorM[A @@ T]) = #>(ev)
  }
  implicit def configToProxy[T](c: Config[T]):ConfigProxy = new ConfigProxy {}

  def newConfig[AppName](conn:(String,List[String]),
                         db: (String, String),
                         col: (String, String)) = {
    // We use a proxy object
    //val config = Configuration[AppName]()
    val configProxy = mock[Configuration]
    when(configProxy.config).thenReturn(ConfigFactory.empty())
    when(configProxy.opt[List[String]](conn._1)).thenReturn(Some(conn._2))
    when(configProxy.opt[String](col._1)).thenReturn(Some(col._2))
    when(configProxy.opt[String](db._1)).thenReturn(Some(db._2))

    configProxy.withTag[AppName]
  }

  // We need to call explicitly the configToProxy method, because its a mock
  def getDAO[A, B, M](implicit ev: Config[A] => ErrorM[B @@ M]) = for {
    config <- configM[A]
    dao <- configToProxy(config).resolvM[A, B, M]
  } yield dao

  def resolveConfig[A](implicit ev:Configuration @@ A) = ev

}
