package com.blackfynn.modelschema.db

import com.blackfynn.concepts.models.Icon
import com.blackfynn.modelschema.model.{
  DatasetId,
  DatasetTemplateId,
  ModelId,
  OrganizationId,
  SchemaLinkedPropertyId,
  SchemaRelationshipId
}
import com.blackfynn.modelschema.responses
import com.github.tminglei.slickpg.{
  ExPostgresProfile,
  PgCirceJsonSupport,
  PgRangeSupport
}
import io.circe._
import io.circe.syntax._
import java.sql.Timestamp
import java.time.{ OffsetDateTime, ZoneOffset }

trait PostgresProfile
    extends ExPostgresProfile
    with PgCirceJsonSupport
    with PgRangeSupport {

  override val pgjson = "jsonb"

  trait Implicits { self: API with CirceImplicits =>

    implicit val offsetDateTimeMapper =
      MappedColumnType.base[OffsetDateTime, Timestamp](
        z => Timestamp.from(z.toInstant),
        t => OffsetDateTime.ofInstant(t.toInstant, ZoneOffset.UTC)
      )

    implicit val listStringMapper = MappedColumnType.base[List[String], Json](
      list => list.asJson,
      json => json.as[List[String]].right.get
    )

    implicit val schemaIdMapper = MappedColumnType
      .base[ModelId, String](
        (id: ModelId) => id.value,
        (id: String) => ModelId(id)
      )

    implicit val datasetIdMapper = MappedColumnType
      .base[DatasetId, Int](
        (id: DatasetId) => id.value,
        (id: Int) => DatasetId(id)
      )

    implicit val organizationIdMapper = MappedColumnType
      .base[OrganizationId, Int](
        (id: OrganizationId) => id.value,
        (id: Int) => OrganizationId(id)
      )

    implicit val datasetTemplateIdMapper = MappedColumnType
      .base[DatasetTemplateId, Int](
        (id: DatasetTemplateId) => id.value,
        (id: Int) => DatasetTemplateId(id)
      )

    implicit val schemaRelationshipIdMapper = MappedColumnType
      .base[SchemaRelationshipId, Int](
        (id: SchemaRelationshipId) => id.value,
        (id: Int) => SchemaRelationshipId(id)
      )

    implicit val schemaLinkedPropertyIdMapper = MappedColumnType
      .base[SchemaLinkedPropertyId, Int](
        (id: SchemaLinkedPropertyId) => id.value,
        (id: Int) => SchemaLinkedPropertyId(id)
      )

    implicit val iconMapper = MappedColumnType
      .base[Icon, String](_.toString, Icon.withNameInsensitive(_))

    implicit val schemaPropertyResponsesMapper = MappedColumnType
      .base[Seq[responses.modelservice.SchemaPropertyResponse], Json](
        (properties: Seq[responses.modelservice.SchemaPropertyResponse]) =>
          properties.asJson,
        // UNSAFE:
        (json: Json) =>
          json.as[Seq[responses.modelservice.SchemaPropertyResponse]].right.get
      )
  }

  object Profile
      extends API
      with CirceImplicits
      with RangeImplicits
      with Implicits

  override val api = Profile
}

object PostgresProfile extends PostgresProfile
