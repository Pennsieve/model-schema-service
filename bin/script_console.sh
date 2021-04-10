#!/usr/bin/env expect

spawn bin/bootstrap_console.sh
expect connected
send ":load src/test/scala/com/blackfynn/modelschema/console/package.scala\n"
send "val env = Console.setup\n"
send "implicit val system = env.system\n"
send "implicit val ec = env.executionContext\n"
send "implicit val mat = env.materializer\n"
send "implicit val timeout = env.timeout\n"
send "implicit val config = env.config\n"
send "implicit val container = env.msContainer\n"
send "implicit val client = env.client\n"
send "import scala.concurrent.{Await, Future}\n"
send "import scala.concurrent.duration._\n"
send "import scala.language.postfixOps\n"
send "import io.circe._\n"
send "import io.circe.syntax._\n"
send "import io.circe.shapes._\n"
send "import shapeless.{ :+:, CNil, Coproduct }\n"
send "import shapeless.syntax.inject._\n"
send "import com.blackfynn.modelschema.model.implicits._\n"
interact
