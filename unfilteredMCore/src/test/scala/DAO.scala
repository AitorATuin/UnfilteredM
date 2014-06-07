package com.logikujo.www
package test

/**
 *
 * UnfilteredM / LogiDev - [Fun Functional] / Logikujo.com
 *
 * 7/06/14 :: 10:23 :: eof
 *
 */

import model._
import scalate._
import org.scalatest._
import org.scalatest.matchers._
import org.scalatest.mock.MockitoSugar
import org.scalamock.scalatest._
import org.mockito.Mockito._
import com.typesafe.config._
import com.github.kxbmap.configs._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import com.logikujo.www.Config

import scala.collection.concurrent.TrieMap
import scalaz._
import Scalaz._
import reactivemongo.bson.{BSONDocument, BSONDocumentReader, BSONWriter, BSONDocumentWriter}

trait DAOSpec extends UnitSpec with MockitoSugar  {
  trait ConfigProxy {
    def resolvM[Tag, A, T](implicit ev: Config[Tag] => ErrorM[A @@ T]) = #>(ev)
  }
  implicit def configToProxy[T](c: Config[T]):ConfigProxy = new ConfigProxy {}

  def liftMError[AppName, B, T](error: String) = (c:Config[AppName]) => error.left[B @@ T]

  def newConfig[AppName] = {
    val configProxy = mock[Configuration]
    configProxy.withTag[AppName]
  }

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

  object MongoConfiguration {

    trait MongoApp

    object Posts {

      case class Post(title: String, contents: String)

      implicit object PostReader extends BSONDocumentReader[Post] {
        def read(post: BSONDocument): Post = Post("dummy", "dummy")
      }

      val mongoBackEnd = mock[MongoDBDAO]
      when(mongoBackEnd.findOne[Post]("id" -> "001")).thenReturn(Future(some(Post("post1", "contents1"))))
      when(mongoBackEnd.findOne[Post]("id" -> "002")).thenReturn(Future(some(Post("post1", "contents1"))))
      /*implicit val postDAOBuilder: Config[MongoApp] => ErrorM[MongoDBDAO @@ Post] = {
        (c: Config[MongoApp]) => mongoBackEnd.withTag[Post].right[String]
      }*/
    }
    object Config1 {
      import MongoConfiguration.Posts.{Post, mongoBackEnd}

      implicit val config1 = newConfig[MongoApp]("mongo.connection" -> List("127.0.0.1"),
        "blog.database" -> "db1",
        "blog.posts.collection" -> "posts")
      implicit val postDAOBuilder: Config[MongoApp] => ErrorM[MongoDBDAO @@ Post] = {
        (c: Config[MongoApp]) => mongoBackEnd.withTag[Post].right[String]
      }
      val postDAO = getDAO[MongoApp, MongoDBDAO, Post].run(resolveConfig[MongoApp])
    }
  }

  object TrieMapConfiguration {
    trait TrieMapApp
    object Users {
      case class User(name: String, password: String)

      implicit object UserReader extends BSONDocumentReader[User] {
        def read(user: BSONDocument): User = User("dummy", "dummy")
      }

      trait TrieMapDBDAO[A] {
        val store: TrieMap[String, A] = TrieMap[String, A]() //("user1" -> "pass1", "user2" -> "pass2")

        def findOne(query: A): Option[A]

        def insert(a: A): Option[A]
      }

      def usersStore = {
        val usersStore = new TrieMapDBDAO[User] {
          def findOne(query: User): Option[User] =
            store.get(query.name).
              map(_.password == query.password)
              .map(_.?(query.some).|(none)).flatten

          def insert(user: User): Option[User] = {
            store += (user.name -> user)
            user.some
          }
        }.withTag[User]

        usersStore insert (User("user1", "pass1"))
        usersStore insert (User("user2", "pass2"))
        usersStore insert (User("user3", "pass3"))
        usersStore
      }

      //implicit val userDAOBuilder: Config[TrieMapApp] => ErrorM[TrieMapDBDAO[User] @@ User] = _ => usersStore.right[String]
    }
    // TriedMapDBDAO which success returning a TrieMapDBDAO
    object Config1 {
      import Users.{TrieMapDBDAO, User}
      implicit val userDAOBuilder: Config[TrieMapApp] => ErrorM[TrieMapDBDAO[User] @@ User] = _ => Users.usersStore.right[String]
      implicit val config2 = newConfig[TrieMapApp]
      val usersDAO = getDAO[TrieMapApp, TrieMapDBDAO[User], User].run(resolveConfig[TrieMapApp])
    }
    // TrieMapDBDAO which fails at configuration time returning -\/(error)
    object Config2 {
      import Users.{TrieMapDBDAO, User}
      implicit val config2 = newConfig[TrieMapApp]
      implicit val userDAOBuilder = liftMError[TrieMapApp, TrieMapDBDAO[User], User]("Couldnt get DBDAO")
      val usersDAO = getDAO[TrieMapApp, TrieMapDBDAO[User], User].run(resolveConfig[TrieMapApp])
    }
  }
}

class DAOTest extends DAOSpec {
  import TrieMapConfiguration.Users.{TrieMapDBDAO, User}

  "DAO" should
    "be resolved specifying DB backend and model" in {
      import MongoConfiguration.{Config1 => MConfig1}
      import MongoConfiguration.Posts.Post
      MConfig1.postDAO should be(anInstanceOf[\/[String, MongoDBDAO @@ Post]])
      //postDAO.map(_.findOne("id" -> "001")) should equal (\/-(Future(Post("post1", "contents1"))))
      //postDAO.map(_.findOne("id" -> "002")) should equal (\/-(Future(Post("post1", "contents1"))))

      // TrieMapDBDAO resolved

      // Success TrieMapDBDAO
      import TrieMapConfiguration.Users.{TrieMapDBDAO, User}
      import TrieMapConfiguration.Config1
      Config1.usersDAO should be(anInstanceOf[\/[String, TrieMapDBDAO[User] @@ User]])

      // Failed TrieMapDBDAO at config time
      import TrieMapConfiguration.Config2
      Config2.usersDAO should be(anInstanceOf[\/[String, TrieMapDBDAO[User] @@ User]])
      Config2.usersDAO should equal(-\/("Couldnt get DBDAO"))
    }
  it should
    "be able to perform basic DAO operations on it" in {
      import TrieMapConfiguration.Users.{TrieMapDBDAO, User}
      import TrieMapConfiguration.Config1
      Config1.usersDAO.map(_.findOne(User("user1", "pass1"))) should equal (\/-(Some(User("user1", "pass1"))))
      Config1.usersDAO.map(_.findOne(User("user2", "pass2"))) should equal (\/-(Some(User("user2", "pass2"))))
      Config1.usersDAO.map(_.findOne(User("user3", "pass3"))) should equal (\/-(Some(User("user3", "pass3"))))
      info("findOne")

      Config1.usersDAO.map(_.findOne(User("user4", "pass4"))) should equal (\/-(None))
      Config1.usersDAO.map(_.insert(User("user4", "pass4"))) should equal (\/-(Some(User("user4", "pass4"))))
      Config1.usersDAO.map(_.findOne(User("user4", "pass4"))) should equal (\/-(Some(User("user4", "pass4"))))
      info("insert")
    }
}
