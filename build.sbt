lazy val akkaHttpVersion = "10.1.11"
lazy val akkaVersion     = "2.6.5"
lazy val authMiddlewareVersion = "4.2.2"
lazy val catsVersion     = "1.1.0"
lazy val circeVersion    = "0.9.3"
lazy val shapelessVersion = "2.3.3"
lazy val coreModelsVersion = "43-62e24a8"
lazy val conceptsModelsVersion = "2.6.1-SNAPSHOT"
lazy val serviceUtilitiesVersion = "1.3.4-SNAPSHOT"

lazy val nexusHost = "nexus.pennsieve.cc"

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization    := "com.blackfynn.modelschema",
      scalaVersion    := "2.12.6"
    )),
    name := "model-schema-service",
    // https://tpolecat.github.io/2017/04/25/scalac-flags.html
    scalacOptions ++= Seq(
      "-encoding", "utf-8", // Specify character encoding used by source files.
      "-deprecation", // Emit warning and location for usages of deprecated APIs.
      "-explaintypes", // Explain type errors in more detail.
      "-feature", // Emit warning and location for usages of features that should be imported explicitly.
      "-language:implicitConversions", // Allow definition of implicit functions called views
      "-Ypartial-unification", // Enable partial unification in type constructor inference (needed for cats)
      "-Ywarn-infer-any" // Warn when a type argument is inferred to be `Any`.
    ),
    resolvers ++= Seq(
      "Pennsieve Releases" at s"https://${nexusHost}/repository/maven-releases",
      "Pennsieve Snapshots" at s"https://${nexusHost}/repository/maven-snapshots",
      "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
      "jitpack.io" at "https://jitpack.io"
    ),
    addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full),
    credentials += Credentials("Sonatype Nexus Repository Manager",
      nexusHost,
      sys.env.getOrElse("PENNSIEVE_NEXUS_USER", "pennsieveci"),
      sys.env.getOrElse("PENNSIEVE_NEXUS_PW", "")
    ),
    test in assembly := {},
    guardrailTasks in Compile := List(
      Server(file("swagger/model-schema-service.yml"), pkg="com.blackfynn.http.server")
    ),
    libraryDependencies ++= Seq(
      "com.blackfynn" %% "auth-middleware"   % authMiddlewareVersion,
      "com.blackfynn" %% "core-models"       % coreModelsVersion,
      "com.blackfynn" %% "service-utilities" % serviceUtilitiesVersion,

      "com.typesafe.akka" %% "akka-http"     % akkaHttpVersion,
      "com.typesafe.akka" %% "akka-http-xml" % akkaHttpVersion,
      "com.typesafe.akka" %% "akka-stream"   % akkaVersion,

      "com.chuusai" %% "shapeless" % shapelessVersion,

      "de.heikoseeberger" %% "akka-http-circe" % "1.21.0",

      "org.typelevel" %% "cats-core" % catsVersion,

      "org.apache.commons" % "commons-lang3" % "3.8.1",

      "io.circe" %% "circe-core"           % circeVersion,
      "io.circe" %% "circe-generic"        % circeVersion,
      "io.circe" %% "circe-generic-extras" % circeVersion,
      "io.circe" %% "circe-java8"          % circeVersion,
      "io.circe" %% "circe-optics"         % circeVersion,
      "io.circe" %% "circe-parser"         % circeVersion,
      "io.circe" %% "circe-shapes"         % circeVersion,

      "com.github.everit-org.json-schema" % "org.everit.json.schema" % "1.9.1",

      "com.typesafe" % "config" % "1.3.2",
      "com.iheart" % "ficus_2.12" % "1.4.3",

      "io.scalaland" %% "chimney" % "0.2.1",

      "com.typesafe.slick" %% "slick" % "3.2.1",
      "com.typesafe.slick" %% "slick-hikaricp" % "3.2.1",

      "com.github.tminglei" %% "slick-pg" % "0.15.3",
      "com.github.tminglei" %% "slick-pg_circe-json" % "0.15.3",

      "org.slf4j" % "slf4j-api" % "1.7.25",
      "org.slf4j" % "jul-to-slf4j" % "1.7.25",
      "org.slf4j" % "jcl-over-slf4j" % "1.7.25",
      "org.slf4j" % "log4j-over-slf4j" % "1.7.25",
      "ch.qos.logback" % "logback-classic" % "1.2.3",
      "ch.qos.logback" % "logback-core" % "1.2.3",
      "com.typesafe.scala-logging" %% "scala-logging" % "3.7.2",
      "io.sentry" % "sentry-logback" % "1.6.6",

      "org.scalacheck"    %% "scalacheck" % "1.14.0",

      "com.blackfynn" %% "core-models" % coreModelsVersion % Test,
      "com.blackfynn" %% "service-utilities" % serviceUtilitiesVersion % "test" classifier "tests",
      "com.typesafe.akka" %% "akka-http-testkit"    % akkaHttpVersion % Test,
      "com.typesafe.akka" %% "akka-testkit"         % akkaVersion     % Test,
      "com.typesafe.akka" %% "akka-stream-testkit"  % akkaVersion     % Test,
      "org.scalamock"     %% "scalamock"            % "4.1.0"         % Test,
      "org.scalatest"     %% "scalatest"            % "3.0.1"         % Test,
      "com.dimafeng"      %% "testcontainers-scala" % "0.27.0"        % Test,
      "org.testcontainers" % "postgresql"           % "1.15.1"         % Test
    ).map(_.exclude("commons-logging", "commons-logging")),

    scalafmtOnCompile := true,

    dockerfile in docker := {
      val artifact: File = assembly.value
      val artifactTargetPath = s"/app/${artifact.name}"

      // Where Postgres (psql/JDBC) expects to find the trusted CA certificate
      val CA_CERT_LOCATION = "/home/pennsieve/.postgresql/root.crt"

      new Dockerfile {
        from("pennsieve/java-cloudwrap:8-jre-alpine-0.5.9")
        copy(artifact, artifactTargetPath, chown="pennsieve:pennsieve")
        copy(baseDirectory.value / "bin" / "start_server.sh", "/app/start_server.sh", chown="pennsieve:pennsieve")
        addRaw(
          "https://truststore.pki.rds.amazonaws.com/global/global-bundle.pem",
          CA_CERT_LOCATION,
        )
        user("root")
        run("chmod", "+r", CA_CERT_LOCATION)
        user("pennsieve")
        env("RUST_BACKTRACE", "1")
        cmd("--service", "model-schema-service", "exec", "/app/start_server.sh", artifactTargetPath)
      }
    },
    imageNames in docker := Seq(
      ImageName("pennsieve/model-schema-service:latest")
    )
  )
  .enablePlugins(DockerPlugin)
