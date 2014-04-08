import scala.reflect.macros.Context
import language.experimental.macros

class Lock {
  def acquireLock() = println("ACQUIRE")
  def releaseLock() = println("RELEASE")
}

object Lock {
  implicit class LockExtensions(val lock: Lock) extends AnyVal {
    @throws(classOf[InterruptedException])
    def acquire[T](cb: => T) = macro Impl.acquireLock[T]
  }

  object Impl {
    def acquireLock[T : c.WeakTypeTag](c: Context)(cb: c.Expr[T]): c.Expr[T] = {
      import c.universe._

      val lock = c.Expr[Lock](Select(c.prefix.tree, newTermName("lock")))
      reify {
        lock.splice.acquireLock()
        try {
          cb.splice
        }
        finally {
          lock.splice.releaseLock()
        }
      }
    }
  }
}