resolvers += Resolver.bintrayRepo("twilio", "releases")

addSbtPlugin("io.spray" % "sbt-revolver" % "0.9.1")

addSbtPlugin("se.marcuslonnberg" % "sbt-docker" % "1.5.0")

addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.14.6")

addSbtPlugin("com.geirsson" % "sbt-scalafmt" % "1.6.0-RC4")

addSbtPlugin("com.twilio" % "sbt-guardrail" % "0.41.5")
