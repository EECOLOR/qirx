package qirx

trait FailureHandler {
  def failure(cause: Throwable):Unit
}

object FailureHandler {
  def proxyTo(handler: Throwable => Unit):FailureHandler =
    new FailureHandler {
      def failure(cause: Throwable):Unit = handler apply cause
    }
}
