package com.blackfynn.modelschema.clients

import scala.concurrent.{ Await, ExecutionContext }
import scala.concurrent.duration._

import com.blackfynn.modelschema._
import com.blackfynn.modelschema.managers.SchemaManager
import com.blackfynn.modelschema.model._
import com.blackfynn.modelschema.util.MockHttpResponder

import akka.actor.ActorSystem
import akka.http.scaladsl.model._
import akka.stream.ActorMaterializer
import io.circe.generic.auto._
import io.circe.syntax._

class BioportalTestHttpResponder(
  hostName: String
)(implicit
  val system: ActorSystem,
  val executionContext: ExecutionContext,
  val materializer: ActorMaterializer
) extends MockHttpResponder {

  override def mock = {
    case (method @ HttpMethods.GET, uri)
        if uri.toString.contains("%2Fbad%2Fbioportal%2Furi") =>
      (StatusCodes.NotFound, "Bad uri!".asJson)
    case (method @ HttpMethods.GET, uri)
        if uri.toString.contains("%2Fgood%2Fbioportal%2Furi") =>
      (StatusCodes.OK, "Good uri!".asJson)

    // a hierarchy
    case (method @ HttpMethods.GET, uri)
        if uri.toString.endsWith("/example/request/uri?apikey=apikey") => {

      // this is just used to test the validity of this URI, no
      // need for response content.
      (StatusCodes.OK, None.asJson)
    }
    case (method @ HttpMethods.GET, uri)
        if uri.toString.endsWith(
          "/ontologies/EXAMPLE/classes/%3Cbioportalhost%3E%2Fexample%2Frequest%2Furi/children?apikey=apikey"
        ) => {
      (
        StatusCodes.OK,
        BioportalResponse(
          collection = List(
            BioportalResponseInner(
              prefLabel = "child1",
              links = BioportalResponseLinks(
                children = s"${hostName}/example/request/uri/children/child1"
              )
            ),
            BioportalResponseInner(
              prefLabel = "child2",
              links = BioportalResponseLinks(
                children = s"${hostName}/example/request/uri/children/child2"
              )
            )
          ),
          nextPage = Some(2),
          links = BioportalResponsePageLinks(
            nextPage = Some(s"${hostName}/example/request/uri?page=2")
          )
        ).asJson
      )
    }
    case (method @ HttpMethods.GET, uri)
        if uri.toString.endsWith("/example/request/uri?page=2&apikey=apikey") => {
      (
        StatusCodes.OK,
        BioportalResponse(
          collection = List(
            BioportalResponseInner(
              prefLabel = "child3",
              links = BioportalResponseLinks(
                children = s"${hostName}/example/request/uri/children/child3"
              )
            )
          ),
          nextPage = None,
          links = BioportalResponsePageLinks(nextPage = None)
        ).asJson
      )
    }
    case (method @ HttpMethods.GET, uri)
        if uri.toString.endsWith(
          "/example/request/uri/children/child1?apikey=apikey"
        ) => {
      (
        StatusCodes.OK,
        BioportalResponse(
          collection = List.empty,
          nextPage = None,
          links = BioportalResponsePageLinks(nextPage = None)
        ).asJson
      )
    }
    case (method @ HttpMethods.GET, uri)
        if uri.toString.endsWith(
          "/example/request/uri/children/child2?apikey=apikey"
        ) => {
      (
        StatusCodes.OK,
        BioportalResponse(
          collection = List(
            BioportalResponseInner(
              prefLabel = "grandchild1",
              links = BioportalResponseLinks(
                children =
                  s"${hostName}/example/request/uri/children/child2/grandchildren/grandchild1"
              )
            )
          ),
          nextPage = None,
          links = BioportalResponsePageLinks(nextPage = None)
        ).asJson
      )
    }
    case (method @ HttpMethods.GET, uri)
        if uri.toString.endsWith(
          "/example/request/uri/children/child3?apikey=apikey"
        ) => {
      (
        StatusCodes.OK,
        BioportalResponse(
          collection = List.empty,
          nextPage = None,
          links = BioportalResponsePageLinks(nextPage = None)
        ).asJson
      )
    }
    case (method @ HttpMethods.GET, uri)
        if uri.toString.endsWith(
          "/example/request/uri/children/child2/grandchildren/grandchild1?apikey=apikey"
        ) => {
      (
        StatusCodes.OK,
        BioportalResponse(
          collection = List.empty,
          nextPage = None,
          links = BioportalResponsePageLinks(nextPage = None)
        ).asJson
      )
    }
  }
}

class BioportalSpec extends HelperSpec {

  override val responder =
    new TemplatesHandlerMockResponder()(system, executionContext, materializer)

  implicit lazy val branchContainer: BranchContainer = msContainer.branches

  "isUriValid" should {
    "reject invalid uri values" in {
      val validation =
        bioportal.isClassIdValid(
          "EXAMPLE",
          s"${BIOPORTAL_TEST_HOST}/bad/bioportal/uri"
        )
      Await.result(validation, 5.seconds) shouldBe false
    }

    "accept valid uri values" in {
      val validation =
        bioportal.isClassIdValid(
          "EXAMPLE",
          s"${BIOPORTAL_TEST_HOST}/good/bioportal/uri"
        )
      Await.result(validation, 5.seconds) shouldBe true
    }
  }

  "removeBioportalHostFromURI" should {
    "remove bioportal host from a URI with http" in {
      bioportal.removeBioportalHostFromURI(
        s"http://${BIOPORTAL_TEST_HOST}/bioportal/uri"
      ) shouldBe ("/bioportal/uri")
    }

    "remove bioportal host from a URI with no http" in {
      bioportal.removeBioportalHostFromURI(
        s"${BIOPORTAL_TEST_HOST}/bioportal/uri"
      ) shouldBe ("/bioportal/uri")
    }

    "fail when given a non-bioportal URI" in {
      assertThrows[IllegalArgumentException] {
        bioportal.removeBioportalHostFromURI(s"www.google.com/bioportal/uri")
      }
    }
  }

  "getFlattenedHierarchyFromClass" should {
    "return a hierarchy at depth 1" in {
      val flatHierarchy = bioportal.getFlattenedHierarchyFromClass(
        "EXAMPLE",
        s"${BIOPORTAL_TEST_HOST}/example/request/uri",
        0
      )
      Await.result(flatHierarchy, 5.seconds).sorted shouldBe List(
        "child1",
        "child2",
        "child3"
      )
    }

    "return a full hierarchy" in {
      val flatHierarchy = bioportal.getFlattenedHierarchyFromClass(
        "EXAMPLE",
        s"${BIOPORTAL_TEST_HOST}/example/request/uri",
        5
      )
      Await.result(flatHierarchy, 5.seconds).sorted shouldBe List(
        "child1",
        "child2",
        "child3",
        "grandchild1"
      )
    }
  }

  "SchemaManager.validateBranchProperties" should {
    "invalidate invalid bioportal branches" in {
      val properties = Map(
        "bioportal_1" -> SchemaProperty(
          `type` = PropertyType.String,
          branches = Some(
            List(
              PropertyType.Branch(
                "INVALID",
                s"${BIOPORTAL_TEST_HOST}/bad/bioportal/uri",
                "Invalid",
                1
              )
            )
          )
        ),
        "bioportal_2" -> SchemaProperty(
          `type` = PropertyType.String,
          branches = Some(
            List(
              PropertyType
                .Branch(
                  "VALID",
                  s"${BIOPORTAL_TEST_HOST}/good/bioportal/uri",
                  "Valid",
                  1
                )
            )
          )
        ),
        "bioportal_3" -> SchemaProperty(
          `type` = PropertyType.String,
          branches = Some(
            List(
              PropertyType.Branch(
                "INVALID",
                s"${BIOPORTAL_TEST_HOST}/bad/bioportal/uri",
                "Invalid",
                1
              )
            )
          )
        )
      )

      val validated = SchemaManager.validateBranchProperties(properties)
      validated.isLeft shouldBe true
      validated.left.get.properties shouldBe List(
        (
          "bioportal_1",
          s"Invalid bioportal URI: ${BIOPORTAL_TEST_HOST}/bad/bioportal/uri"
        ),
        (
          "bioportal_3",
          s"Invalid bioportal URI: ${BIOPORTAL_TEST_HOST}/bad/bioportal/uri"
        )
      )
    }

    "show multiple errors on the same branch" in {
      val properties = Map(
        "bioportal_1" -> SchemaProperty(
          `type` = PropertyType.String,
          branches = Some(
            List(
              PropertyType.Branch(
                "INVALID",
                s"${BIOPORTAL_TEST_HOST}/bad/bioportal/uri",
                "Invalid",
                10000
              )
            )
          )
        ),
        "bioportal_2" -> SchemaProperty(
          `type` = PropertyType.String,
          branches = Some(
            List(
              PropertyType
                .Branch(
                  "VALID",
                  s"${BIOPORTAL_TEST_HOST}/good/bioportal/uri",
                  "Valid",
                  1
                )
            )
          )
        ),
        "bioportal_3" -> SchemaProperty(
          `type` = PropertyType.String,
          branches = Some(
            List(
              PropertyType.Branch(
                "VALID",
                s"${BIOPORTAL_TEST_HOST}/good/bioportal/uri",
                "Invalid depth",
                10
              )
            )
          )
        )
      )

      val validated = SchemaManager.validateBranchProperties(properties)
      validated.isLeft shouldBe true
      validated.left.get.properties shouldBe List(
        (
          "bioportal_1",
          s"Invalid bioportal URI: ${BIOPORTAL_TEST_HOST}/bad/bioportal/uri"
        ),
        ("bioportal_1", "Bioportal depth cannot exceed 2"),
        ("bioportal_3", "Bioportal depth cannot exceed 2")
      )
    }

    "Succeed when all bioportal branches are valid" in {
      val properties = Map(
        "bioportal_2" -> SchemaProperty(
          `type` = PropertyType.BranchString(
            branches = List(
              PropertyType
                .Branch(
                  "VALID",
                  s"${BIOPORTAL_TEST_HOST}/good/bioportal/uri",
                  "Valid",
                  1
                )
            )
          )
        ),
        "bioportal_3" -> SchemaProperty(
          `type` = PropertyType.BranchString(
            branches = List(
              PropertyType
                .Branch(
                  "VALID",
                  s"${BIOPORTAL_TEST_HOST}/good/bioportal/uri",
                  "Valid",
                  1
                )
            )
          )
        )
      )

      val validated = SchemaManager.validateBranchProperties(properties)
      validated.isRight shouldBe true
      validated.right.get shouldBe properties
    }
  }

  "SchemaManager.parseProperties" should {
    "Parse out bioportal hierarchy" in {
      val properties = Map(
        "bioportal" -> SchemaProperty(
          `type` = PropertyType.String,
          branches = Some(
            List(
              PropertyType.Branch(
                acronym = "EXAMPLE",
                uri = s"${BIOPORTAL_TEST_HOST}/example/request/uri",
                name = "Example",
                maxDepth = 1
              )
            )
          )
        ),
        "unrelated" -> SchemaProperty(PropertyType.String)
      )

      val parsedProps = SchemaManager.populateBranchProperties(properties)

      parsedProps.isRight shouldBe true

      parsedProps.right.get("unrelated") shouldBe SchemaProperty(
        PropertyType.String
      )
      parsedProps.right
        .get("bioportal")
        .`type`
        .asEnumOf(parsedProps.right.get("bioportal").enum.get) shouldBe Some(
        Set("child3", "child1", "child2", "grandchild1")
      )
    }
  }

  "BioportalHierarchyNode" should {

    val testNode = new BioportalHierarchyNode(
      "base",
      List(
        new BioportalHierarchyNode(
          "depth1-el1",
          List(
            new BioportalHierarchyNode("depth2-el1", List()),
            new BioportalHierarchyNode("depth2-el2", List())
          )
        ),
        new BioportalHierarchyNode(
          "depth1-el2",
          List(
            new BioportalHierarchyNode("depth2-el1", List()),
            new BioportalHierarchyNode("depth2-el2", List())
          )
        )
      )
    )

    "flatten correctly" in {
      testNode.flatten.sorted shouldEqual (
        List(
          "base",
          "depth1-el1",
          "depth1-el2",
          "depth2-el1",
          "depth2-el1",
          "depth2-el2",
          "depth2-el2"
        )
      )
    }

    "be displayed correctly" in {
      testNode.display(0) shouldEqual (
        "base\n  depth1-el1\n    depth2-el1\n    depth2-el2\n  depth1-el2\n    depth2-el1\n    depth2-el2\n"
      )
    }
  }
}
