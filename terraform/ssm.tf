resource "aws_ssm_parameter" "api_service_url" {
  name  = "/${var.environment_name}/${var.service_name}/api-service-url"
  type  = "String"
  value = "https://${data.terraform_remote_state.api.outputs.internal_fqdn}"
}

resource "aws_ssm_parameter" "bioportal_api_key" {
  name      = "/${var.environment_name}/${var.service_name}/bioportal-api-key"
  overwrite = false
  type      = "SecureString"
  value     = "dummy"

  lifecycle {
    ignore_changes = [value]
  }
}

resource "aws_ssm_parameter" "bioportal_host" {
  name  = "/${var.environment_name}/${var.service_name}/bioportal-host"
  type  = "String"
  value = var.bioportal_host
}

resource "aws_ssm_parameter" "bioportal_rate_limit" {
  name  = "/${var.environment_name}/${var.service_name}/bioportal-rate-limit"
  type  = "String"
  value = var.bioportal_rate_limit
}

resource "aws_ssm_parameter" "jwt_secret_key" {
  name      = "/${var.environment_name}/${var.service_name}/jwt-secret-key"
  overwrite = false
  type      = "SecureString"
  value     = "dummy"

  lifecycle {
    ignore_changes = [value]
  }
}

resource "aws_ssm_parameter" "model_service_url" {
  name  = "/${var.environment_name}/${var.service_name}/model-service-url"
  type  = "String"
  value = "https://${data.terraform_remote_state.model_service.outputs.internal_fqdn}"
}

resource "aws_ssm_parameter" "prefix_url" {
  name  = "/${var.environment_name}/${var.service_name}/prefix-url"
  type  = "String"
  value = "https://${data.terraform_remote_state.gateway.outputs.external_fqdn}/model-schema"
}

resource "aws_ssm_parameter" "schema_postgres_host" {
  name  = "/${var.environment_name}/${var.service_name}/schema-postgres-host"
  type  = "String"
  value = data.terraform_remote_state.model_schema_postgres.outputs.master_fqdn
}

resource "aws_ssm_parameter" "schema_postgres_password" {
  name      = "/${var.environment_name}/${var.service_name}/schema-postgres-password"
  overwrite = false
  type      = "SecureString"
  value     = "dummy"

  lifecycle {
    ignore_changes = [value]
  }
}

resource "aws_ssm_parameter" "schema_postgres_user" {
  name  = "/${var.environment_name}/${var.service_name}/schema-postgres-user"
  type  = "String"
  value = "${var.environment_name}_${replace(var.service_name, "-", "_")}_user"
}
