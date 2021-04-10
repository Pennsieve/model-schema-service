package com.blackfynn.modelschema.util

import com.blackfynn.auth.middleware.{
  OrganizationNodeId,
  ClaimType,
  Jwt,
  DatasetId => JwtDatasetId,
  OrganizationId => JwtOrganizationId,
  UserClaim,
  UserId
}
import com.blackfynn.modelschema.model.DatasetId
import com.blackfynn.models.Role
import com.blackfynn.modelschema.model.OrganizationId

import shapeless.syntax.inject._

import scala.concurrent.duration._

object Token {

  def generate(
    organizationId: OrganizationId,
    organizationNodeId: Option[OrganizationNodeId] = None,
    organizationRole: Role = Role.Owner,
    datasetId: Option[DatasetId] = None,
    datasetRole: Role = Role.Owner
  )(implicit
    config: Jwt.Config
  ): Jwt.Token = {
    val content: ClaimType = UserClaim(
      UserId(1),
      List(
        Jwt.OrganizationRole(
          JwtOrganizationId(organizationId.value)
            .inject[Jwt.Role.RoleIdentifier[JwtOrganizationId]],
          organizationRole,
          node_id = organizationNodeId
        )
      ) ++ datasetId.map(
        id =>
          Jwt.DatasetRole(
            JwtDatasetId(id.value)
              .inject[Jwt.Role.RoleIdentifier[JwtDatasetId]],
            datasetRole
          )
      )
    )
    val claim = Jwt.generateClaim(content, 10.seconds)
    Jwt.generateToken(claim)
  }
}
